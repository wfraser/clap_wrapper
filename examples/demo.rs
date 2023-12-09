use std::path::PathBuf;

use clap::{Parser, ValueEnum};
use clap_wrapper::clap_wrapper;

/// This is a help message for the program.
#[derive(Parser, Debug)]
struct Args {
    /// Path to config file.
    #[arg(long)]
    config: Option<PathBuf>,

    #[command(flatten)]
    http: HttpOptions,

    #[command(flatten)]
    debug: DebugOptions,
}

/// HTTP Options
#[clap_wrapper(prefix = "http")]
#[derive(Parser, Debug)]
struct HttpOptions {
    #[arg(long)]
    port: u16,

    #[arg(long = "tls_cert")]
    tls_cert_path: Option<PathBuf>,

    #[arg(long = "tls_key")]
    tls_key_path: Option<PathBuf>,
}

/// Debugging Options
#[clap_wrapper(prefix = "debug")]
#[derive(Parser, Debug)]
struct DebugOptions {
    #[arg(long, default_value = "syslog")]
    output: DebugOutput,

    #[arg(long, short, noprefix)]
    verbose: bool,
}

#[derive(Debug, ValueEnum, Clone)]
enum DebugOutput {
    Syslog,
    Stderr,
}

fn main() {
    let args = Args::parse();
    println!("{args:#?}");
}
