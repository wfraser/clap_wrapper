#![allow(clippy::bool_assert_comparison)] // much easier to read when several are in sequence

use std::collections::HashMap;

use clap::{CommandFactory, Parser};
use clap_wrapper::clap_wrapper;

/// This is struct A.
#[clap_wrapper(prefix = "prefix1")]
#[derive(Parser, Debug, PartialEq)]
struct A {
    #[arg(long)]
    a: String,

    #[arg(long = "renamed1")]
    b: String,

    #[arg(long("renamed2"))]
    c: String,

    #[arg(long = "not-prefixed", noprefix)]
    d: i32,
}

/// This is struct B.
#[clap_wrapper(prefix = "prefix2")]
#[derive(Parser, Debug, PartialEq)]
#[command(rename_all = "camel")]
struct B {
    #[arg(long)]
    field_name: String,

    #[arg(long, value_name = "number")]
    a: i32,

    #[arg(long, required = true)]
    bool_required: bool,

    #[arg(long, default_value = "false")]
    bool_def1: bool,

    #[arg(long, default_value_t = false)]
    bool_def2: bool,

    #[arg(long, default_value("false"))]
    bool_def3: bool,

    #[arg(long)]
    bool_just_flag: bool,

    #[arg(long)]
    bool_separate_word: bool,

    #[arg(long)]
    bool_with_equals: bool,
}

#[clap_wrapper()]
#[derive(Parser, Debug, PartialEq)]
struct Outer {
    #[command(flatten)]
    inner1: A,

    #[command(flatten)]
    inner2: B,
}

#[test]
fn test_parse() {
    let s = Outer::parse_from([
        "foo",
        "--prefix1.a=a",
        "--prefix1.renamed1=b",
        "--prefix1.renamed2=c",
        "--not-prefixed=999",
        "--prefix2.a=8675309",
        "--prefix2.fieldName=d",
        "--prefix2.boolRequired=false",
        "--prefix2.boolDef1=false",
        "--prefix2.boolDef2=false",
        "--prefix2.boolDef3=false",
        "--prefix2.boolJustFlag",
        "--prefix2.boolSeparateWord",
        "true",
        "--prefix2.boolWithEquals=true",
    ]);

    assert_eq!(
        s,
        Outer {
            inner1: A {
                a: "a".to_owned(),
                b: "b".to_owned(),
                c: "c".to_owned(),
                d: 999,
            },
            inner2: B {
                field_name: "d".to_owned(),
                a: 8675309,
                bool_required: false,
                bool_def1: false,
                bool_def2: false,
                bool_def3: false,
                bool_just_flag: true,
                bool_separate_word: true,
                bool_with_equals: true,
            }
        }
    );
}

#[test]
fn test_args() {
    let cmd = Outer::command();
    let args = cmd
        .get_arguments()
        .map(|arg| (arg.get_id().as_str(), arg))
        .collect::<HashMap<_, _>>();
    assert_eq!(args["prefix1.a"].get_long(), Some("prefix1.a"));
    assert_eq!(args["prefix1.b"].get_long(), Some("prefix1.renamed1"));
    assert_eq!(args["prefix1.c"].get_long(), Some("prefix1.renamed2"));
    assert_eq!(args["prefix1.d"].get_long(), Some("not-prefixed"));

    assert_eq!(
        args["prefix2.field_name"].get_long(),
        Some("prefix2.fieldName")
    );

    assert_eq!(args["prefix2.a"].get_long(), Some("prefix2.a"));

    assert_eq!(args["prefix2.bool_required"].is_required_set(), true);
    assert_eq!(args["prefix2.bool_def1"].is_required_set(), false);
    assert_eq!(args["prefix2.bool_def2"].is_required_set(), false);
    assert_eq!(args["prefix2.bool_def3"].is_required_set(), false);
    assert_eq!(args["prefix2.bool_just_flag"].is_required_set(), false);
    assert_eq!(args["prefix2.bool_separate_word"].is_required_set(), false);
    assert_eq!(args["prefix2.bool_with_equals"].is_required_set(), false);

    for flag in &["prefix1.a", "prefix1.b", "prefix1.c", "prefix1.d"] {
        assert_eq!(
            args[flag].get_help_heading(),
            Some("This is struct A."),
            "flag {flag} has wrong heading"
        );
    }
    for flag in &[
        "prefix2.field_name",
        "prefix2.a",
        "prefix2.bool_required",
        "prefix2.bool_def1",
        "prefix2.bool_def2",
        "prefix2.bool_def3",
        "prefix2.bool_just_flag",
        "prefix2.bool_separate_word",
        "prefix2.bool_with_equals",
    ] {
        assert_eq!(
            args[flag].get_help_heading(),
            Some("This is struct B."),
            "flag {flag} has wrong heading"
        );
    }
}
