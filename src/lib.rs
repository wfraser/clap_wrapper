use core::panic;

use heck::{
    ToKebabCase, ToLowerCamelCase, ToPascalCase, ToShoutySnakeCase, ToSnakeCase, ToUpperCamelCase,
};
use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse::Parser;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, DataStruct, DeriveInput, Error, Expr, ExprAssign, ExprLit, Lit, Token,
};

const ATTR_NAME: &str = "dbx_clap";

#[derive(Debug, Default)]
struct Options {
    prefix: Option<String>,
}

#[proc_macro_attribute]
pub fn dbx_clap(
    main_attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut opts = Options::default();
    let attr_exprs =
        match Punctuated::<ExprAssign, Token![,]>::parse_terminated.parse(main_attr.clone()) {
            Ok(exprs) => exprs,
            Err(e) => return e.to_compile_error().into(),
        };
    for expr in attr_exprs {
        let key = expr.left.to_token_stream().to_string();
        match key.as_str() {
            "prefix" => {
                let prefix = match expr_literal(&expr.right) {
                    Ok(s) => s,
                    Err(e) => return e.to_compile_error().into(),
                };
                opts.prefix = Some(prefix);
            }
            _ => panic!("unrecognized #[{ATTR_NAME}] attribute {key:?}"),
        }
    }

    let mut input: DeriveInput = parse_macro_input!(input);
    if let Some(prefix) = opts.prefix {
        if let Err(e) = add_prefix_to_everything(&prefix, &mut input) {
            return e.into_compile_error().into();
        }
    }

    let mut ts = TokenStream::new();
    input.to_tokens(&mut ts);
    ts.into()
}

fn clap_to_lower(s: &str) -> String {
    ToSnakeCase::to_snake_case(s).replace('_', "")
}

fn clap_to_upper(s: &str) -> String {
    ToShoutySnakeCase::to_shouty_snake_case(s).replace('_', "")
}

fn add_prefix_to_everything(prefix: &str, input: &mut DeriveInput) -> syn::Result<()> {
    let mut name_style: fn(&str) -> String = <str as ToKebabCase>::to_kebab_case;

    for attr in &input.attrs {
        if let syn::Meta::List(list) = &attr.meta {
            let head = list.path.to_token_stream().to_string();
            if head == "clap" || head == "command" {
                let exprs =
                    Punctuated::<Expr, Token![,]>::parse_terminated.parse2(list.tokens.clone())?;
                for expr in exprs {
                    match expr {
                        Expr::Assign(expr)
                            if expr.left.to_token_stream().to_string() == "rename_all" =>
                        {
                            let style = expr_literal(&expr.right)?;
                            let mut norm = style.to_upper_camel_case().to_lowercase();
                            norm = norm
                                .strip_suffix("case")
                                .map(ToOwned::to_owned)
                                .unwrap_or(norm);
                            name_style = match norm.as_str() {
                                "pascal" => <str as ToPascalCase>::to_pascal_case,
                                "kebab" => <str as ToKebabCase>::to_kebab_case,
                                "camel" => <str as ToLowerCamelCase>::to_lower_camel_case,
                                "screamingsnake" => {
                                    <str as ToShoutySnakeCase>::to_shouty_snake_case
                                }
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
    }

    match &mut input.data {
        syn::Data::Struct(DataStruct { fields, .. }) => {
            for field in fields {
                let field_name = field
                    .ident
                    .as_ref()
                    .expect("field must have a name")
                    .to_string();

                for attr in &mut field.attrs {
                    match &mut attr.meta {
                        syn::Meta::List(list)
                            if list.path.get_ident().map(ToString::to_string).as_deref()
                                == Some("arg") =>
                        {
                            let mut exprs = Punctuated::<Expr, Token![,]>::parse_terminated
                                .parse2(list.tokens.clone())?;

                            for expr in &mut exprs {
                                match expr {
                                    // long = "foo"
                                    Expr::Assign(expr)
                                        if expr.left.to_token_stream().to_string() == "long" =>
                                    {
                                        let renamed = expr_literal(&expr.right)?;
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
                                                let renamed = expr_literal(arg)?;
                                                let name = format!("{prefix}.{renamed}");
                                                *expr = syn::parse_quote_spanned! { expr.span() => long(#name) };
                                            }
                                            // long() -- assuming this is actually valid
                                            (None, 0) => {
                                                let name =
                                                    format!("{prefix}.{}", name_style(&field_name));
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
                                    Expr::Path(path)
                                        if path.to_token_stream().to_string() == "long" =>
                                    {
                                        let name = format!("{prefix}.{}", name_style(&field_name));
                                        *expr = syn::parse_quote_spanned! { path.span() => long(#name) };
                                    }
                                    // anything else just remains unchanged
                                    _ => (),
                                }
                            }

                            list.tokens = exprs.to_token_stream();
                        }
                        _ => (),
                    };
                }
            }
        }
        syn::Data::Enum(_) => todo!("enums"),
        syn::Data::Union(_) => todo!("unions"),
    }
    Ok(())
}

fn expr_literal(expr: &Expr) -> syn::Result<String> {
    let Expr::Lit(ExprLit {
        lit: Lit::Str(s), ..
    }) = expr
    else {
        return Err(Error::new_spanned(expr, "expected a string literal"));
    };
    Ok(s.value())
}
