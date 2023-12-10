This is a helper for [Clap](https://github.com/clap-rs/clap) that extends it and changes some defaults.

# What this changes
## Extensions:
  * Ability to add some specified prefix to the long flag for all options in a struct.
    * The prefix is specified on the struct itself, which is useful for a set of options used by a library.
      All programs that incorporate this struct then get the same flags with the same names (good for consistency), and they won't collide with any flags the program defines.
    * It also means the options all get grouped together automatically in the program's `--help` text.

## Changed Defaults:
  * Boolean fields
    * Can take a value or not. Specifying `--flag=true` or `--flag true` or just `--flag` are all the same, and specifying `--flag=false` is the same as not specifying it at all.
    * The "value name" used in help text defaults to the string "BOOL".
  * The first line of doc comments on a struct is used as a help text heading and groups all the options together.

# How to use:

Instead of

```rust
use clap::Parser;

#[derive(Parser)]
#[command(next_help_heading = "My Options")]
struct MyOptions {
    #[arg(long = "my.foo", value_name = "BOOL", num_args(0..=1), action=clap::ArgAction::Set, default_missing_value = "true")]
    foo: bool,
    
    #[arg(long = "my.bar")]
    bar: Option<String>,
}
```

write:

```rust
use clap::Parser;
use clap_wrapper::clap_wrapper;

/// My Options
#[clap_wrapper(prefix = "my")]
#[derive(Parser)]
struct MyOptions {
    #[arg(long)]
    foo: bool,
    
    #[arg(long)]
    bar: Option<String>,
}
```

These are equivalent, but one is much easier to read and write :)

Help text for this group of options will be like:

```text
My Options:
      --my.foo [<BOOL>]  [default: false] [possible values: true, false]
      --my.bar <BAR>
```

This group of options would then be used within a program's main options struct like:

```rust
#[derive(Parser)]
struct Args {
    #[arg]
    whatever: String,
    // and other program options...
    
    // include all the options from MyOptions:
    #[command(flatten)]
    my: MyOptions,
}
```