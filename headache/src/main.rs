use clap::Parser;

fn main() {
    let args = libheadache::Args::parse();
    libheadache::run(args.into());
}
