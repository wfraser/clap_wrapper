use clap_wrapper::clap_wrapper;
use clap::Parser;

#[test]
fn test() {
    #[clap_wrapper(prefix = "prefix1")]
    #[derive(Parser, Debug, PartialEq)]
    struct A {
        #[arg(long)]
        a: String,

        #[arg(long = "renamed1")]
        b: String,

        #[arg(long("renamed2"))]
        c: String,
    }

    #[clap_wrapper(prefix = "prefix2")]
    #[derive(Parser, Debug, PartialEq)]
    #[command(rename_all = "camel")]
    struct B {
        #[arg(long)]
        field_name: String,

        #[arg(long)]
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

    let s = Outer::try_parse_from([
        "foo",
        "--prefix1.a=a",
        "--prefix1.renamed1=b",
        "--prefix1.renamed2=c",
        "--prefix2.fieldName=d",
        "--prefix2.boolRequired=false",
        "--prefix2.boolDef1=true",
        "--prefix2.boolDef2=true",
        "--prefix2.boolDef3=true",
        "--prefix2.boolJustFlag",
        "--prefix2.boolSeparateWord",
        "true",
        "--prefix2.boolWithEquals=true",
        ]).unwrap();

    assert_eq!(s, Outer {
        inner1: A {
            a: "a".to_owned(),
            b: "b".to_owned(),
            c: "c".to_owned(),
        },
        inner2: B {
            field_name: "d".to_owned(),
            bool_required: false,
            bool_def1: true,
            bool_def2: true,
            bool_def3: true,
            bool_just_flag: true,
            bool_separate_word: true,
            bool_with_equals: true,
        }
    });
}
