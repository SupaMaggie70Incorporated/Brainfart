use std::ops::Range;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Args {
    pub file: String,
    #[arg(long, default_value = "256")]
    pub initial_memory: u32,
    #[arg(long, default_value = "30000")]
    pub max_memory: u32,
    #[arg(short, long, default_value = "false")]
    pub large_int: bool,
    #[arg(short, long, default_value = None)]
    pub input: Option<String>,
    #[arg(long, default_value = None)]
    pub input_file: Option<String>,
    #[arg(short, long, default_value = "false")]
    pub advanced_comments: bool,
    #[arg(short, long, default_value = "false")]
    pub no_wrap: bool,
    #[arg(short, long, default_value = "false")]
    pub debug: bool,
}
impl Into<RunOptions> for Args {
    fn into(self) -> RunOptions {
        let content = std::fs::read_to_string(&self.file).expect("Program file does not exist");
        let input = if let Some(i) = self.input {
            i
        } else if let Some(i) = self.input_file {
            std::fs::read_to_string(&i).expect("Program input file does not exist")
        } else {
            println!("Input:");
            let mut str = String::new();
            std::io::stdin()
                .read_line(&mut str)
                .expect("Unable to read input");
            str.truncate(str.trim_end().len());
            str
        };
        RunOptions {
            content,
            source: self.file,
            input,
            initial_memory: self.initial_memory,
            max_memory: self.max_memory,
            byte: !self.large_int,
            advanced_comments: self.advanced_comments,
            wrap: !self.no_wrap,
            debug: self.debug,
        }
    }
}
pub struct RunOptions {
    pub content: String,
    pub source: String,
    pub initial_memory: u32,
    pub max_memory: u32,
    pub byte: bool,
    pub input: String,
    pub advanced_comments: bool,
    pub wrap: bool,
    pub debug: bool,
}
impl Default for RunOptions {
    fn default() -> Self {
        Self {
            content: String::new(),
            source: String::new(),
            initial_memory: 256,
            max_memory: 30000,
            byte: true,
            input: String::new(),
            advanced_comments: false,
            wrap: true,
            debug: false,
        }
    }
}
#[derive(Clone, Copy, Debug)]
pub enum Operation {
    /// + or -, contains the actual value added, the maximum value added, and maximum value subtracted
    Add(i32, i32, i32, i32),
    /// ,
    Input,
    /// .
    Output,
    /// > or <, contains the actual value moved, maximum moved right, maximum moved left
    Move(i32, i32, i32, i32),
    /// [
    StartLoop(u32),
    /// ]
    EndLoop(u32),
    Debug,
}
fn append_operation(
    program: &mut Vec<(Operation, Range<usize>)>,
    operation: Operation,
    span: Range<usize>,
) {
    if let Some(last) = program.last_mut() {
        *last = match (last.0, operation) {
            (Operation::Add(a1, b1, c1, d1), Operation::Add(a2, b2, c2, d2)) => {
                let a = a1 + a2;
                let b = a.max(b1).max(b2);
                let c = a.min(c1).min(c2);
                (Operation::Add(a, b, c, d1 + d2), last.1.start..span.end)
            }
            (Operation::Move(a1, b1, c1, d1), Operation::Move(a2, b2, c2, d2)) => {
                let a = a1 + a2;
                let b = a.max(b1).max(b2);
                let c = a.min(c1).min(c2);
                (Operation::Move(a, b, c, d1 + d2), last.1.start..span.end)
            }
            _ => {
                program.push((operation, span));
                return;
            }
        };
    } else {
        program.push((operation, span));
    }
}
pub fn print_ariadne(
    program: &str,
    program_file: &str,
    span: Range<usize>,
    message: &str,
    note: Option<&str>,
) {
    use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};

    let mut colors = ColorGenerator::new();

    // Generate & choose some colours for each of our elements
    let a = colors.next();

    let mut report = Report::build(ReportKind::Error, (program_file, span.clone()))
        .with_message(message)
        .with_label(
            Label::new((program_file, span))
                .with_message(message)
                .with_color(a),
        );
    if let Some(note) = note {
        report = report.with_note(note);
    }
    report
        .finish()
        .print((program_file, Source::from(program)))
        .unwrap();
    std::process::exit(1);
}
pub fn run(args: RunOptions) {
    println!("Parsing");
    let mut program = Vec::new();
    let mut comment_layers = 0;
    let mut line_comment = false;
    let mut i = 0;
    let mut loop_start_indices = Vec::new();
    while i < args.content.len() {
        let c = args.content.as_bytes()[i] as char;
        let next_c = if i < args.content.len() - 1 {
            Some(args.content.as_bytes()[i + 1] as char)
        } else {
            None
        };
        if args.advanced_comments {
            match c {
                '*' => {
                    let next_char = next_c.unwrap();
                    if next_char == '/' {
                        if comment_layers == 0 {
                            print_ariadne(
                                &args.content,
                                &args.source,
                                i..i + 2,
                                "Unmatched end comment",
                                None,
                            );
                        }
                        comment_layers -= 1;
                        i += 2;
                        continue;
                    }
                }
                '/' => {
                    let next_char = next_c.unwrap();
                    if next_char == '*' {
                        comment_layers += 1;
                        i += 2;
                        continue;
                    } else if next_char == '/' {
                        line_comment = true;
                        i += 2;
                        continue;
                    }
                }
                _ => (),
            }
        }
        if line_comment || comment_layers > 0 {
            i += 1;
            continue;
        }
        match c {
            '+' => append_operation(&mut program, Operation::Add(1, 1, 0, 1), i..i + 1),
            '-' => append_operation(&mut program, Operation::Add(-1, 0, -1, 1), i..i + 1),
            ',' => append_operation(&mut program, Operation::Input, i..i + 1),
            '.' => append_operation(&mut program, Operation::Output, i..i + 1),
            '>' => append_operation(&mut program, Operation::Move(1, 1, 0, 1), i..i + 1),
            '<' => append_operation(&mut program, Operation::Move(-1, 0, -1, 1), i..i + 1),
            '[' => {
                loop_start_indices.push(program.len() as u32);
                append_operation(&mut program, Operation::StartLoop(0), i..i + 1);
            }
            ']' => {
                if let Some(index) = loop_start_indices.pop() {
                    program[index as usize].0 = Operation::StartLoop(program.len() as u32 + 1);
                    append_operation(&mut program, Operation::EndLoop(index + 1), i..i + 1);
                } else {
                    print_ariadne(
                        &args.content,
                        &args.source,
                        i..i + 1,
                        "Unmatched loop end",
                        None,
                    );
                }
            }
            '#' if args.debug => {
                append_operation(&mut program, Operation::Debug, i..i + 1);
            }
            _ => (),
        }
        i += 1;
    }
    if let Some(unmatched) = loop_start_indices.pop() {
        print_ariadne(
            &args.content,
            &args.source,
            program[unmatched as usize].1.clone(),
            "Unmatched loop start",
            None,
        );
    }
    execute(args, &mut program);
}
fn execute(args: RunOptions, program: &mut Vec<(Operation, Range<usize>)>) {
    println!("Executing:");
    let mut ops = 0;
    let mut unfiltered_ops = 0;
    let mut memory: Vec<i32> = Vec::with_capacity(args.initial_memory as usize);
    memory.push(0);
    let mut ptr = 0;
    let mut pc = 0;
    let mut read_idx = 0;
    while pc < program.len() {
        let op = program[pc].clone();
        let val = memory[ptr];
        match op.0 {
            Operation::Add(a, b, c, d) => {
                memory[ptr] = val + a;
                if args.wrap {
                    memory[ptr] = memory[ptr].rem_euclid(256);
                } else {
                    if args.byte && val + b > 255 {
                        print_ariadne(
                            &args.content,
                            &args.source,
                            op.1.clone(),
                            "Integer overflow",
                            None,
                        );
                    }
                    if val + c < 0 {
                        print_ariadne(
                            &args.content,
                            &args.source,
                            op.1.clone(),
                            "Integer underflow",
                            None,
                        );
                    }
                }
                unfiltered_ops += d;
            }
            Operation::Move(a, b, c, d) => {
                if ptr + (b as usize) >= args.max_memory as usize {
                    print_ariadne(
                        &args.content,
                        &args.source,
                        op.1.clone(),
                        "Memory overflow",
                        None,
                    );
                }
                if ptr as i32 + c < 0 {
                    print_ariadne(
                        &args.content,
                        &args.source,
                        op.1.clone(),
                        "Moved to negative memory address",
                        None,
                    );
                }
                ptr = (ptr as i32 + a) as usize;
                if ptr >= memory.len() {
                    memory.resize(ptr + 1, 0);
                }
                unfiltered_ops += d;
            }
            Operation::Input => {
                memory[ptr] = if read_idx < args.input.len() {
                    read_idx += 1;
                    args.input.as_bytes()[read_idx - 1] as u8 as i32
                } else {
                    0
                };
                unfiltered_ops += 1;
            }
            Operation::Output => {
                if val > 255 {
                    print_ariadne(
                        &args.content,
                        &args.source,
                        op.1.clone(),
                        "Character doesn't fit in a byte",
                        None,
                    );
                }
                print!("{}", val as u8 as char);
                std::io::Write::flush(&mut std::io::stdout())
                    .expect("Error writing program output");
                unfiltered_ops += 1;
            }
            Operation::StartLoop(end) => {
                if val == 0 {
                    pc = end as usize;
                    continue;
                }
                unfiltered_ops += 1;
            }
            Operation::EndLoop(start) => {
                if val != 0 {
                    pc = start as usize;
                    continue;
                }
                unfiltered_ops += 1;
            }
            Operation::Debug => {
                print!("\n[HEADACHE DEBUG: {{ {}", memory[0]);
                for value in &memory[1..] {
                    print!(", {value}");
                }
                print!(" }}]\n");
            }
        }
        pc += 1;
        ops += 1
    }
    println!();
    println!("Execution summary:\n\tProgram length: {}\n\tOperations: {unfiltered_ops}\n\tLogical operations: {ops}\n\tMemory usage: {}", args.content.len(), memory.len());
}
