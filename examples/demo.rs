use std::str::FromStr;

use clap::Parser;
use clap_wrapper::dbx_clap;

/// Some cool struct that I want to fold into a bigger options struct.
#[dbx_clap(prefix = "myprefix")]
#[derive(Parser, Debug)]
#[clap(rename_all = "camel")]
struct MyArgs {
    /// This has default name.
    #[arg(long, default_value = "false")]
    boolean_flag: bool,

    /// This has to be specified.
    #[arg(long)]
    reqd_bool: bool,

    #[arg(long("renamed"), default_value_t = 42)]
    int_field: i32,

    #[arg(long = "renamed2", action = clap::ArgAction::Set)]
    str_field: String,

    #[arg(long)]
    blah_blah: Option<String>,

    #[arg(long)]
    friend: Option<Animal>,
}

#[derive(Debug, Clone, Copy)]
enum Animal {
    Cat,
    Dog,
}

impl FromStr for Animal {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "cat" => Self::Cat,
            "dog" => Self::Dog,
            _ => return Err("unknown Animal variant"),
        })
    }
}

/// The main args struct.
#[derive(Parser, Debug)]
struct Args {
    #[arg(long)]
    outer: Option<String>,

    #[command(flatten)]
    inner: MyArgs,
}

fn main() {
    let args = Args::parse();
    println!("{args:#?}");
}
