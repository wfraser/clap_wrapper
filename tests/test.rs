use clap::Parser;
use clap_wrapper::clap_wrapper;

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

        #[arg(long = "jk-lol", noprefix)]
        secret: i32,
    }

    #[clap_wrapper(prefix = "prefix2")]
    #[derive(Parser, Debug, PartialEq)]
    #[command(rename_all = "camel")]
    struct B {
        #[arg(long)]
        field_name: String,

        #[arg(long)]
        a: i32,

        #[arg(long, required(true))]
        bool_required: bool,

        #[arg(long, default_value = "true")]
        bool_def1: bool,

        #[arg(long, default_value_t = true)]
        bool_def2: bool,

        #[arg(long, default_value("true"))]
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

    let s = Outer::parse_from([
        "foo",
        "--prefix1.a=a",
        "--prefix1.renamed1=b",
        "--prefix1.renamed2=c",
        "--jk-lol=999",
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
                secret: 999,
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
