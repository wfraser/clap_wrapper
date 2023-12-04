use clap::Parser;
use clap_wrapper::dbx_clap;

/// Some cool struct that I want to fold into a bigger options struct.
#[dbx_clap(prefix = "myprefix")]
#[derive(Parser, Debug, Default)]
#[clap(rename_all = "camel")]
struct MyArgs {
    /// This has default name.
    #[arg(long)]
    boolean_flag: bool,

    #[arg(long("renamed"), default_value_t = 42)]
    int_field: i32,

    #[arg(long = "renamed2", action = clap::ArgAction::Set)]
    str_field: String,

    #[arg(long)]
    blah_blah: Option<String>,
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
