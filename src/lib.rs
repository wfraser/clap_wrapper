use heck::{
    ToKebabCase, ToLowerCamelCase, ToPascalCase, ToShoutySnakeCase, ToSnakeCase, ToUpperCamelCase,
};
use proc_macro::TokenStream;
use quote::ToTokens;
use syn::parse::Parser;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, Data, DataStruct, DeriveInput, Error, Expr, ExprAssign, ExprCall, ExprLit,
    ExprPath, Field, Lit, LitBool, Meta, MetaNameValue, Token,
};

#[derive(Debug, Default)]
struct Options {
    prefix: Option<String>,
}

#[proc_macro_attribute]
pub fn clap_wrapper(main_attr: TokenStream, input: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(input as DeriveInput);
    if let Err(e) = apply(main_attr, &mut input) {
        return e.to_compile_error().into();
    }

    input.to_token_stream().into()
}

fn apply(main_attr: TokenStream, input: &mut DeriveInput) -> syn::Result<()> {
    let mut opts = Options::default();
    let attr_exprs =
        Punctuated::<ExprAssign, Token![,]>::parse_terminated.parse(main_attr.clone())?;
    for expr in &attr_exprs {
        let key = expr.left.to_token_stream().to_string();
        match key.as_str() {
            "prefix" => {
                opts.prefix = Some(expr_str_lit(&expr.right)?);
            }
            _ => return Err(Error::new_spanned(&expr.left, "unrecognized attribute")),
        }
    }

    // Grab the first line of doc comment for the struct and use it as the help heading for the
    // args.
    let mut heading = None;
    for attr in &input.attrs {
        if attr.path().is_ident("doc") {
            let Meta::NameValue(MetaNameValue { value, .. }) = &attr.meta else {
                return Err(Error::new_spanned(attr, "malformed #[doc] attribute"));
            };
            if let Expr::Lit(ExprLit {
                lit: Lit::Str(s), ..
            }) = value
            {
                heading = Some(s.value());
                break;
            }
        }
    }
    if let Some(heading) = heading {
        let heading = heading.trim();
        input.attrs.push(syn::parse_quote! {
            #[command(next_help_heading = #heading)]
        });
    }

    if let Ok(false) = any_clap_here(input) {
        return Err(Error::new_spanned(
            attr_exprs,
            "this attribute needs to be put before any #[derive(clap::Parser)]",
        ));
    }

    if let Some(prefix) = opts.prefix {
        add_prefix_to_everything(&prefix, input)?;
    }

    add_better_boolean_attrs(input)?;

    Ok(())
}

fn clap_to_lower(s: &str) -> String {
    ToSnakeCase::to_snake_case(s).replace('_', "")
}

fn clap_to_upper(s: &str) -> String {
    ToShoutySnakeCase::to_shouty_snake_case(s).replace('_', "")
}

fn any_clap_here(input: &DeriveInput) -> syn::Result<bool> {
    for attr in &input.attrs {
        if attr.path().is_ident("derive") {
            let derives =
                attr.parse_args_with(Punctuated::<ExprPath, Token![,]>::parse_terminated)?;
            if derives.into_iter().any(|path| path.path.is_ident("Parser")) {
                return Ok(true);
            }
        }
    }
    Ok(false)
}
fn add_better_boolean_attrs(input: &mut DeriveInput) -> syn::Result<()> {
    match &mut input.data {
        Data::Struct(DataStruct { fields, .. }) => {
            for field in fields {
                if field.ty.to_token_stream().to_string() == "bool" {
                    let default = get_bool_default(field)?;
                    let required = field_is_required(field)?;

                    // Insert at the front, so later (i.e. manually specified) clap attributes can
                    // override it.
                    field.attrs.insert(
                        0,
                        syn::parse_quote! {
                            #[arg(
                                num_args(0..=1),
                                action=clap::ArgAction::Set,
                                // This is for when the flag is specified without any value (i.e. as
                                // "--flag", not "--flag=whatever). Setting this to anything except True
                                // would be super confusing, and it's not related to the default value.
                                default_missing_value = "true",
                            )]
                        },
                    );

                    if default.is_none() && !required {
                        field.attrs.insert(
                            1,
                            syn::parse_quote! {
                                #[arg(default_value_t = false)]
                            },
                        );
                    }
                }
            }
        }
        Data::Enum(_) => {}
        Data::Union(_) => {}
    }
    Ok(())
}

fn get_bool_default(field: &Field) -> syn::Result<Option<bool>> {
    let mut default = None;
    for attr in &field.attrs {
        if !attr.path().is_ident("arg") {
            continue;
        }
        let exprs = attr.parse_args_with(Punctuated::<Expr, Token![,]>::parse_terminated)?;
        for expr in exprs {
            let value = match expr {
                Expr::Assign(expr) => match expr.left.to_token_stream().to_string().as_str() {
                    "default_value" => Some(expr_str_lit(&expr.right)? == "true"),
                    "default_value_t" => match expr.right.as_ref() {
                        Expr::Lit(ExprLit {
                            lit: Lit::Bool(LitBool { value, .. }),
                            ..
                        }) => Some(*value),
                        _ => return Err(Error::new_spanned(expr.right, "expected a bool literal")),
                    },
                    _ => None,
                },
                Expr::Call(call) if call.func.to_token_stream().to_string() == "default_value" => {
                    let arg = call
                        .args
                        .first()
                        .ok_or_else(|| Error::new_spanned(&call, "expected a single arg"))?;
                    Some(expr_str_lit(arg)? == "true")
                }
                _ => None,
            };
            if let Some(new_value) = value {
                if let Some(old_value) = default {
                    if new_value != old_value {
                        return Err(Error::new_spanned(
                            field,
                            "conflicting default_value and/or default_value_t values",
                        ));
                    }
                }
                default = Some(new_value);
            }
        }
    }
    Ok(default)
}

fn add_prefix_to_everything(prefix: &str, input: &mut DeriveInput) -> syn::Result<()> {
    let name_style = get_field_name_style(input)?;

    let Data::Struct(DataStruct { fields, .. }) = &mut input.data else {
        return Err(Error::new_spanned(
            input,
            "prefix can only be applied to a struct",
        ));
    };

    for field in fields {
        let field_name = field
            .ident
            .as_ref()
            .ok_or_else(|| Error::new(field.span(), "field must have a name"))?
            .to_string();
        for attr in &mut field.attrs {
            if let Meta::List(list) = &mut attr.meta {
                let head = list
                    .path
                    .get_ident()
                    .map(ToString::to_string)
                    .unwrap_or_default();
                if head == "clap" {
                    return Err(Error::new_spanned(field, "do not use the #[clap] attribute on fields; use #[arg] or #[command] instead"));
                } else if head == "arg" {
                    let mut exprs = Punctuated::<Expr, Token![,]>::parse_terminated
                        .parse2(list.tokens.clone())?;

                    // Add the prefix to the arg ID it to prevent collisions with other structs.
                    let prefixed_id = format!("{prefix}.{field_name}");
                    exprs.push(syn::parse_quote! {
                        id(#prefixed_id)
                    });

                    // Also add the prefix to the name unless one was specified manually.
                    let explicit_name = exprs.iter().any(|x| {
                        let head = match x {
                            Expr::Call(expr) => &expr.func,
                            Expr::Assign(expr) => &expr.left,
                            _ => return false,
                        };
                        head.to_token_stream().to_string() == "value_name"
                    });
                    if !explicit_name {
                        let value_name = if field.ty.to_token_stream().to_string() == "bool" {
                            "BOOL".to_owned()
                        } else {
                            field_name.to_shouty_snake_case()
                        };
                        exprs.push(syn::parse_quote! { value_name = #value_name });
                    }

                    // Presence of a made-up "noprefix" attribute disables any prefixing of the
                    // flag.
                    let mut noprefix = false;
                    exprs = exprs
                        .into_iter()
                        .filter(|x| {
                            if matches!(x, Expr::Path(p) if p.path.is_ident("noprefix")) {
                                noprefix = true;
                                // Remove this expression so clap doesn't choke on it.
                                false
                            } else {
                                true
                            }
                        })
                        .collect();
                    if noprefix {
                        // Done with this field.
                        list.tokens = exprs.to_token_stream();
                        continue;
                    }

                    for expr in &mut exprs {
                        match expr {
                            // long = "foo"
                            Expr::Assign(expr)
                                if expr.left.to_token_stream().to_string() == "long" =>
                            {
                                let renamed = expr_str_lit(&expr.right)?;
                                let name = format!("{prefix}.{renamed}");
                                *expr = syn::parse_quote_spanned! { expr.span() => long = #name };
                            }
                            // long("foo")
                            Expr::Call(expr)
                                if expr.func.to_token_stream().to_string() == "long" =>
                            {
                                match (expr.args.first(), expr.args.len()) {
                                    // long("foo")
                                    (Some(arg), 1) => {
                                        let renamed = expr_str_lit(arg)?;
                                        let name = format!("{prefix}.{renamed}");
                                        *expr = syn::parse_quote_spanned! { expr.span() => long(#name) };
                                    }
                                    // long() -- assuming this is actually valid
                                    (None, 0) => {
                                        let name = format!("{prefix}.{}", name_style(&field_name));
                                        *expr = syn::parse_quote_spanned! { expr.span() => long(#name) };
                                    }
                                    _ => {
                                        return Err(Error::new_spanned(
                                            expr,
                                            "expected exactly one argument",
                                        ));
                                    }
                                }
                            }
                            // long
                            Expr::Path(path) if path.path.is_ident("long") => {
                                let name = format!("{prefix}.{}", name_style(&field_name));
                                *expr = syn::parse_quote_spanned! { path.span() => long(#name) };
                            }
                            // anything else just remains unchanged
                            _ => (),
                        }
                    }

                    list.tokens = exprs.to_token_stream();
                }
            }
        }
    }
    Ok(())
}

fn get_field_name_style(input: &DeriveInput) -> syn::Result<fn(&str) -> String> {
    let mut name_style: fn(&str) -> String = <str as ToKebabCase>::to_kebab_case;

    for attr in &input.attrs {
        if let Meta::List(list) = &attr.meta {
            match list.path.get_ident() {
                Some(id) if id == "clap" || id == "command" => (),
                _ => continue,
            }
            let exprs =
                Punctuated::<Expr, Token![,]>::parse_terminated.parse2(list.tokens.clone())?;
            for expr in exprs {
                match expr {
                    Expr::Assign(expr)
                        if expr.left.to_token_stream().to_string() == "rename_all" =>
                    {
                        let style = expr_str_lit(&expr.right)?;
                        let mut norm = style.to_upper_camel_case().to_lowercase();
                        norm = norm
                            .strip_suffix("case")
                            .map(ToOwned::to_owned)
                            .unwrap_or(norm);
                        name_style = match norm.as_str() {
                            "pascal" => <str as ToPascalCase>::to_pascal_case,
                            "kebab" => <str as ToKebabCase>::to_kebab_case,
                            "camel" => <str as ToLowerCamelCase>::to_lower_camel_case,
                            "screamingsnake" => <str as ToShoutySnakeCase>::to_shouty_snake_case,
                            "snake" => <str as ToSnakeCase>::to_snake_case,
                            "lower" => clap_to_lower,
                            "upper" => clap_to_upper,
                            "verbatim" => str::to_owned,
                            _ => {
                                return Err(Error::new_spanned(
                                    expr.right,
                                    "unrecognized rename style",
                                ))
                            }
                        }
                    }
                    _ => (),
                }
            }
        }
    }

    Ok(name_style)
}

fn field_is_required(field: &Field) -> syn::Result<bool> {
    let mut required = false;
    for attr in field.attrs.iter().filter(|a| a.path().is_ident("arg")) {
        let exprs = attr.parse_args_with(Punctuated::<Expr, Token![,]>::parse_terminated)?;
        for expr in &exprs {
            let (left, right) = match expr {
                Expr::Call(ExprCall { func, args, .. }) if args.len() == 1 => {
                    (func.as_ref(), args.first().unwrap())
                }
                Expr::Assign(ExprAssign { left, right, .. }) => (left.as_ref(), right.as_ref()),
                _ => continue,
            };
            if expr_is_ident(left, "required") {
                if let Expr::Lit(ExprLit {
                    lit: Lit::Bool(litbool),
                    ..
                }) = right
                {
                    required = litbool.value;
                }
            }
        }
    }
    Ok(required)
}

fn expr_is_ident(expr: &Expr, ident: &str) -> bool {
    if let Expr::Path(ExprPath { path, .. }) = expr {
        if path.is_ident(ident) {
            return true;
        }
    }
    false
}

fn expr_str_lit(expr: &Expr) -> syn::Result<String> {
    let Expr::Lit(ExprLit {
        lit: Lit::Str(s), ..
    }) = expr
    else {
        return Err(Error::new_spanned(expr, "expected a string literal"));
    };
    Ok(s.value())
}
