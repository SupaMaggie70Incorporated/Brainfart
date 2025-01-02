use clap::Parser;
use libbrainfart::*;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    input: String,
    #[arg(short, long)]
    output: String,
    #[arg(short, long, default_value = "false")]
    pretty: bool,
    #[arg(long, default_value = "false")]
    optimize_adds: bool,
    #[arg(short, long, default_value = "true")]
    run: bool,
}

fn main() {
    let args = Args::parse();
    let file_str = std::fs::read_to_string(&args.input).expect("Input file does not exist");
    let mut out_writer = std::io::BufWriter::new(
        std::fs::File::create(&args.output).expect("Unable to create output file"),
    );
    println!("Parsing input");
    let mut parser = libbrainfart::Parser::new(&file_str);
    if let Err(e) = parser.parse() {
        e.print_ariadne(&args.input, &file_str);
        std::process::exit(1);
    }
    println!("Lowering input");
    let module = match parser.lower() {
        Ok(module) => module,
        Err(e) => {
            e.print_ariadne(&args.input, &file_str);
            std::process::exit(1);
        }
    };
    println!("Writing output");
    AsmModuleWriter::new(
        module,
        AsmModuleWriteOptions {
            pretty: args.pretty,
            optimize_constant_set: args.optimize_adds,
        },
    )
    .write(&mut out_writer)
    .expect("Error writing module");
    println!("Writing complete");
    drop(out_writer);
    if args.run {
        let mut run_args = Vec::new();
        run_args.push(std::ffi::OsString::new());
        run_args.push(args.output.as_str().into());
        {
            let mut accept = false;
            for arg in std::env::args_os() {
                if accept {
                    run_args.push(arg.clone());
                    println!("{}", arg.into_string().unwrap());
                } else if arg == "--" {
                    accept = true
                }
            }
        }
        println!("Running program");
        libheadache::run(libheadache::Args::parse_from(run_args).into());
    }
}
