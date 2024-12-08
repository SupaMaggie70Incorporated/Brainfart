use std::io::Write;

pub type BlockHandle = u32;
pub type GlobalCallHandle = u32;
pub type StaticHandle = u32;

#[derive(Debug, Clone)]
pub struct StaticVariable {
    pub size: u32,
    pub initialization: Option<Vec<u32>>,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct Register(pub u32);
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum ValueAccess {
    Register(Register),
    StaticVariable(StaticHandle, u32),
    Stack(u32),
    /// From the top of the static stack
    StaticStack(u32),
    Raw(u32),
    /// A value access based on a parameter provided to a code block
    Virtual(u32),
    Heap(u32),
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InlineCall {
    pub block: BlockHandle,
    pub parameters: Vec<ValueAccess>,
}
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum Instruction {
    #[default]
    Noop,
    // Brainfuck or context intrinsics, which will still be used to determine the following ptr indices
    IntrinsicAdd(i32),
    IntrinsicMove(i32),
    IntrinsicInput,
    IntrinsicOutput,
    IntrinsicStartLoop,
    IntrinsicEndLoop,
    IntrinsicUpdatePtrIndex(ValueAccess),
    IntrinsicUpdatePtrIndexDynamic(i32),
    // Brainfuck instructions that won't be parsed by the compiler
    IntrinsicText(String),
    // Assembly operations
    SetExit,
    SetCall(GlobalCallHandle),
    InlineBlock(InlineCall),
    GoTo(ValueAccess),
    SetConstant {
        position: ValueAccess,
        value: u32,
        tmp: Option<ValueAccess>,
    },
    Move {
        src: ValueAccess,
        dst: ValueAccess,
        tmp: Option<ValueAccess>,
    },
    Increment {
        position: ValueAccess,
        value: i32,
        tmp: Option<ValueAccess>,
    },
    Add {
        src: ValueAccess,
        dst: ValueAccess,
        tmp: Option<ValueAccess>,
    },
    Subtract {
        src: ValueAccess,
        dst: ValueAccess,
        tmp: Option<ValueAccess>,
    },
    CallIfZero {
        position: ValueAccess,
        zero: GlobalCallHandle,
        nonzero: GlobalCallHandle,
        tmp: [Option<ValueAccess>; 2],
    },
    BlockIfZero {
        position: ValueAccess,
        zero: InlineCall,
        nonzero: InlineCall,
        tmp: [Option<ValueAccess>; 2],
    },
    InlineForRange {
        times: u32,
        block: InlineCall,
        tmp: [Option<ValueAccess>; 2],
    },
    PushStack(u32),
    PopStack(u32),
    PushStaticStack(u32),
    PopStaticStack(u32),
}
#[derive(Clone, Debug)]
pub struct CodeBlock {
    pub instructions: Vec<Instruction>,
    pub num_parameters: u32,
}
#[derive(Clone, Debug)]
pub struct AsmModule {
    pub num_registers: u32,
    pub stack_size: u32,
    /// Should this always be 1?
    pub stack_align: u32,
    pub static_stack_size: u32,
    pub static_variables: Vec<StaticVariable>,
    pub code_blocks: Vec<CodeBlock>,
    pub global_calls: Vec<BlockHandle>,
    pub start: BlockHandle,
}
#[derive(Clone, Debug)]
pub struct BlockContext {
    ptr_position: u32,
    indent_level: u32,
    static_stack_index: u32,
    highest_static_stack_index: u32,
    stack_position_to_end: Option<i32>,
    parameter_stack: Vec<Vec<ValueAccess>>,
}
#[derive(Clone, Debug)]
pub struct AsmModuleWriteOptions {
    /// Whether to pretty print(use newlines and indentation)
    pub pretty: bool,
    /// Whether or not to optimize many sequential adds or subtracts using loops, which reduces file size but reduces optimizability by headache
    pub optimize_constant_set: bool,
}
fn next_multiple_minus_one(a: i32, b: i32) -> i32 {
    a + b - a % b - 1
}
pub struct AsmModuleWriter {
    module: AsmModule,
    write_options: AsmModuleWriteOptions,
    stack_range: std::ops::Range<u32>,
    #[allow(unused)]
    stack_call_width: u32,
    register_range: std::ops::Range<u32>,
    continue_position: u32,
    #[allow(unused)]
    call_stuff_range: std::ops::Range<u32>,
    static_stack_range: std::ops::Range<u32>,
    #[allow(unused)]
    static_range: std::ops::Range<u32>,
    static_ranges: Vec<std::ops::Range<u32>>,
    heap_start: u32,
}
impl AsmModuleWriter {
    fn calculate_best_sum_product(num: u32, distance: u32) -> (u32, u32, i32) {
        if num == 0 {
            return (0, 0, 0);
        }
        const fn cost(a: u32, b: u32, c: u32, distance: u32) -> u32 {
            // One + for each
            // If b > 0 then you need brackets and moves both ways for the distance
            a + b + c + if b > 0 { 2 * (1 + distance) } else { 0 }
        }
        let mut best = (cost(0, 0, num, distance), (0, 0, num as i32));
        for a in 1..num_integer::sqrt(num) + 1 {
            let b = num / a;
            let c = num - a * b;
            let price = cost(a, b, c, distance);
            if price < best.0 {
                best = (price, (a, b, c as i32));
            }
            if num % a > 0 {
                let b = num / a + 1;
                let c = num as i32 - (a * b) as i32;
                let price = cost(a, b, c.unsigned_abs(), distance);
                if price < best.0 {
                    best = (price, (a, b, c));
                }
            }
        }
        assert!((best.1 .0 * best.1 .1) as i32 + best.1 .2 == num as i32);
        best.1
    }
    pub fn resolve_position(&self, access: ValueAccess, ctx: &mut BlockContext) -> Option<u32> {
        match access {
            ValueAccess::Register(Register(r)) => Some(self.register_range.start + r),
            ValueAccess::StaticVariable(id, offset) => {
                Some(self.static_ranges[id as usize].start + offset)
            }
            ValueAccess::Raw(v) => Some(v),
            ValueAccess::StaticStack(o) => {
                Some(self.static_stack_range.start + ctx.static_stack_index - o)
            }
            ValueAccess::Stack(_) => None,
            ValueAccess::Virtual(k) => {
                let parameters = ctx.parameter_stack.pop().unwrap();
                let result = self.resolve_position(parameters[k as usize], ctx);
                ctx.parameter_stack.push(parameters);
                result
            }
            ValueAccess::Heap(a) => Some(self.heap_start + a),
        }
    }
    pub fn offset_from(
        &self,
        base: ValueAccess,
        access: ValueAccess,
        ctx: &mut BlockContext,
    ) -> Option<i32> {
        match (
            self.resolve_position(base, ctx),
            self.resolve_position(access, ctx),
        ) {
            (Some(a), Some(b)) => Some((b as i32) - (a as i32)),
            (None, None) => match (base, access) {
                (ValueAccess::Heap(a), ValueAccess::Heap(b)) => Some((b as i32) - (a as i32)),
                _ => unreachable!(),
            },
            _ => None,
        }
    }
    /// Calculates the offset relative to the final compass of the stack
    pub fn calculate_raw_stack_position(&self, offset: i32) -> i32 {
        if offset >= 0 {
            offset + offset / (self.module.stack_align as i32) + 1
        } else {
            // Every block looks like -4,-3,-2,-1 and not 0,1,2,3
            offset + (offset + 1) / (self.module.stack_align as i32)
        }
    }
    pub fn write_block(
        &self,
        writer: &mut impl Write,
        block: &CodeBlock,
        ctx: &mut BlockContext,
    ) -> std::io::Result<()> {
        ctx.indent_level += 1;
        if self.write_options.pretty {
            write!(writer, "\n")?;
            for _ in 0..ctx.indent_level {
                write!(writer, "  ")?;
            }
        }
        for instruction in &block.instructions {
            self.write_instruction(writer, instruction.clone(), ctx)?;
        }
        if self.write_options.pretty {
            writeln!(writer, "")?;
        }
        ctx.indent_level -= 1;
        for _ in 0..ctx.indent_level {
            write!(writer, "  ")?;
        }
        Ok(())
    }
    pub fn write_instruction(
        &self,
        writer: &mut impl Write,
        instruction: Instruction,
        ctx: &mut BlockContext,
    ) -> std::io::Result<()> {
        match instruction {
            Instruction::Noop => (),
            Instruction::IntrinsicAdd(a) => {
                if a > 0 {
                    for _ in 0..a {
                        write!(writer, "+")?;
                    }
                } else {
                    for _ in 0..-a {
                        write!(writer, "-")?;
                    }
                }
            }
            Instruction::IntrinsicMove(mut a) => {
                if let Some(offset) = &mut ctx.stack_position_to_end {
                    let old_position = self.calculate_raw_stack_position(*offset);
                    *offset += a;
                    let new_position = self.calculate_raw_stack_position(*offset);
                    a = new_position - old_position;
                } else {
                    ctx.ptr_position = (ctx.ptr_position as i32 + a) as u32;
                }
                if a > 0 {
                    for _ in 0..a {
                        write!(writer, ">")?;
                    }
                } else {
                    for _ in 0..-a {
                        write!(writer, "<")?;
                    }
                }
            }
            Instruction::IntrinsicInput => {
                write!(writer, ",")?;
            }
            Instruction::IntrinsicOutput => {
                write!(writer, ".")?;
            }
            Instruction::IntrinsicStartLoop => {
                write!(writer, "[")?;
            }
            Instruction::IntrinsicEndLoop => {
                write!(writer, "]")?;
            }
            Instruction::IntrinsicText(text) => {
                write!(writer, "{}", text)?;
            }
            Instruction::IntrinsicUpdatePtrIndex(access) => {
                if let Some(pos) = self.resolve_position(access, ctx) {
                    ctx.ptr_position = pos;
                    ctx.stack_position_to_end = None;
                } else if let ValueAccess::Stack(offset) = access {
                    ctx.stack_position_to_end = Some(offset as i32);
                }
            }
            Instruction::IntrinsicUpdatePtrIndexDynamic(change) => {
                if let Some(pos) = &mut ctx.stack_position_to_end {
                    *pos += change;
                } else {
                    ctx.ptr_position = (ctx.ptr_position as i32 + change) as u32;
                }
            }
            Instruction::SetExit => {
                self.write_instruction(
                    writer,
                    Instruction::SetConstant {
                        position: ValueAccess::Raw(self.continue_position),
                        value: 0,
                        tmp: None,
                    },
                    ctx,
                )?;
            }
            Instruction::SetCall(id) => {
                self.write_instruction(
                    writer,
                    Instruction::SetConstant {
                        position: ValueAccess::Raw(self.continue_position + 3),
                        value: id,
                        tmp: Some(ValueAccess::Raw(self.continue_position + 1)),
                    },
                    ctx,
                )?;
            }
            Instruction::GoTo(pos) => {
                if let Some(pos) = self.resolve_position(pos, ctx) {
                    if let Some(offset) = ctx.stack_position_to_end {
                        if offset < 0 {
                            // Put us at -1 so that moving up puts us on the compass
                            self.write_instruction(
                                writer,
                                Instruction::IntrinsicMove(-offset + 1),
                                ctx,
                            )?;
                        } else {
                            // Put us at the next multiple of stack align minus 1
                            self.write_instruction(
                                writer,
                                Instruction::IntrinsicMove(next_multiple_minus_one(
                                    offset,
                                    self.module.stack_align as i32,
                                )),
                                ctx,
                            )?;
                        }
                        write!(writer, ">")?;
                        ctx.stack_position_to_end = Some(0);
                        write!(writer, "[")?;
                        self.write_instruction(
                            writer,
                            Instruction::IntrinsicMove(self.module.stack_align as i32),
                            ctx,
                        )?;
                        write!(writer, "]")?;
                        ctx.ptr_position = self.stack_range.end - 1;
                        ctx.stack_position_to_end = None;
                    }
                    self.write_instruction(
                        writer,
                        Instruction::IntrinsicMove(pos as i32 - ctx.ptr_position as i32),
                        ctx,
                    )?;
                } else if let ValueAccess::Stack(offset) = pos {
                    if let Some(existing_offset) = ctx.stack_position_to_end {
                        self.write_instruction(
                            writer,
                            Instruction::IntrinsicMove(offset as i32 - existing_offset as i32),
                            ctx,
                        )?;
                    } else {
                        self.write_instruction(
                            writer,
                            Instruction::GoTo(ValueAccess::Raw(self.stack_range.end - 1)),
                            ctx,
                        )?;
                        // Skip past the first compass which is zero
                        for _ in 0..self.module.stack_align + 1 {
                            write!(writer, "<")?;
                        }
                        // Go until you reach the first 0 compass
                        write!(writer, "[")?;
                        for _ in 0..self.module.stack_align + 1 {
                            write!(writer, "<")?;
                        }
                        write!(writer, "]")?;
                        // We will end at the first 0 compass, so go forward to the next compass plus one
                        let current_offset = -(self.module.stack_align as i32 + 1);
                        let desired = self.calculate_raw_stack_position(offset as i32);
                        for _ in 0..desired - current_offset {
                            write!(writer, ">")?;
                        }
                        ctx.stack_position_to_end = Some(offset as i32);
                    }
                }
            }
            Instruction::SetConstant {
                position,
                value,
                tmp,
            } => {
                let (a, b, c, offset) = if let Some(va) = tmp {
                    if self.write_options.optimize_constant_set {
                        let offset = self.offset_from(position, va, ctx).unwrap();
                        let (a, b, c) =
                            Self::calculate_best_sum_product(value, offset.unsigned_abs());
                        (a, b, c, offset)
                    } else {
                        (0, 0, value as i32, 0)
                    }
                } else {
                    (0, 0, value as i32, 0)
                };
                self.write_instruction(writer, Instruction::GoTo(position), ctx)?;
                write!(writer, "[-]")?;
                if a > 0 && b > 0 {
                    self.write_instruction(writer, Instruction::GoTo(tmp.unwrap()), ctx)?;
                    self.write_instruction(writer, Instruction::IntrinsicAdd(a as i32), ctx)?;
                    write!(writer, "[-")?;
                    self.write_instruction(writer, Instruction::IntrinsicMove(-offset), ctx)?;
                    self.write_instruction(writer, Instruction::IntrinsicAdd(b as i32), ctx)?;
                    self.write_instruction(writer, Instruction::IntrinsicMove(offset), ctx)?;
                    write!(writer, "]")?;
                }
                self.write_instruction(writer, Instruction::GoTo(position), ctx)?;
                self.write_instruction(writer, Instruction::IntrinsicAdd(c as i32), ctx)?;
            }
            Instruction::InlineBlock(b) => {
                ctx.parameter_stack.push(b.parameters);
                self.write_block(writer, &self.module.code_blocks[b.block as usize], ctx)?;
                ctx.parameter_stack.pop().unwrap();
            }
            Instruction::Move { src, dst, tmp } => {
                self.write_instruction(writer, Instruction::GoTo(dst), ctx)?;
                write!(writer, "[-]")?;
                if let Some(tmp) = tmp {
                    self.write_instruction(writer, Instruction::GoTo(tmp), ctx)?;
                    write!(writer, "[-]")?;
                }
                self.write_instruction(writer, Instruction::GoTo(src), ctx)?;
                write!(writer, "[")?;
                write!(writer, "-")?;
                self.write_instruction(writer, Instruction::GoTo(dst), ctx)?;
                write!(writer, "+")?;
                if let Some(tmp) = tmp {
                    self.write_instruction(writer, Instruction::GoTo(tmp), ctx)?;
                    write!(writer, "+")?;
                }
                self.write_instruction(writer, Instruction::GoTo(src), ctx)?;
                write!(writer, "]")?;
                if let Some(tmp) = tmp {
                    self.write_instruction(
                        writer,
                        Instruction::Move {
                            src: tmp,
                            dst: src,
                            tmp: None,
                        },
                        ctx,
                    )?;
                }
            }
            Instruction::Increment {
                position,
                value,
                tmp,
            } => {
                self.write_instruction(writer, Instruction::GoTo(position), ctx)?;
                self.write_instruction(writer, Instruction::IntrinsicAdd(value), ctx)?;
                todo!()
            }
            Instruction::Add { src, dst, tmp } => {
                if let Some(tmp) = tmp {
                    self.write_instruction(
                        writer,
                        Instruction::SetConstant {
                            position: tmp,
                            value: 0,
                            tmp: None,
                        },
                        ctx,
                    )?;
                }
                self.write_instruction(writer, Instruction::GoTo(src), ctx)?;
                write!(writer, "[-")?;
                self.write_instruction(writer, Instruction::GoTo(dst), ctx)?;
                write!(writer, "+")?;
                if let Some(tmp) = tmp {
                    self.write_instruction(writer, Instruction::GoTo(tmp), ctx)?;
                    write!(writer, "+")?;
                }
                self.write_instruction(writer, Instruction::GoTo(src), ctx)?;
                write!(writer, "]")?;
                if let Some(tmp) = tmp {
                    self.write_instruction(
                        writer,
                        Instruction::Add {
                            src: tmp,
                            dst: src,
                            tmp: None,
                        },
                        ctx,
                    )?;
                }
            }
            Instruction::Subtract { src, dst, tmp } => {
                if let Some(tmp) = tmp {
                    self.write_instruction(
                        writer,
                        Instruction::SetConstant {
                            position: tmp,
                            value: 0,
                            tmp: None,
                        },
                        ctx,
                    )?;
                }
                self.write_instruction(writer, Instruction::GoTo(src), ctx)?;
                write!(writer, "[-")?;
                self.write_instruction(writer, Instruction::GoTo(dst), ctx)?;
                write!(writer, "-")?;
                if let Some(tmp) = tmp {
                    self.write_instruction(writer, Instruction::GoTo(tmp), ctx)?;
                    write!(writer, "+")?;
                }
                self.write_instruction(writer, Instruction::GoTo(src), ctx)?;
                write!(writer, "]")?;
                if let Some(tmp) = tmp {
                    self.write_instruction(
                        writer,
                        Instruction::Add {
                            src: tmp,
                            dst: src,
                            tmp: None,
                        },
                        ctx,
                    )?;
                }
            }
            Instruction::BlockIfZero {
                position,
                zero,
                nonzero,
                tmp: all_temp,
            } => {
                if let Some(tmp) = all_temp[0] {
                    self.write_instruction(
                        writer,
                        Instruction::Move {
                            src: position,
                            dst: tmp,
                            tmp: all_temp[1],
                        },
                        ctx,
                    )?;
                    self.write_instruction(
                        writer,
                        Instruction::BlockIfZero {
                            position: tmp,
                            zero,
                            nonzero,
                            tmp: [None, all_temp[1]],
                        },
                        ctx,
                    )?;
                } else {
                    self.write_instruction(writer, Instruction::PushStaticStack(1), ctx)?;
                    self.write_instruction(
                        writer,
                        Instruction::SetConstant {
                            position: ValueAccess::StaticStack(0),
                            value: 1,
                            tmp: None,
                        },
                        ctx,
                    )?;
                    self.write_instruction(writer, Instruction::GoTo(position), ctx)?;
                    write!(writer, "[[-]")?;
                    self.write_instruction(
                        writer,
                        Instruction::Increment {
                            position: ValueAccess::StaticStack(0),
                            value: -1,
                            tmp: None,
                        },
                        ctx,
                    )?;
                    self.write_instruction(writer, Instruction::InlineBlock(nonzero), ctx)?;
                    self.write_instruction(writer, Instruction::GoTo(position), ctx)?;
                    write!(writer, "]")?;
                    self.write_instruction(
                        writer,
                        Instruction::GoTo(ValueAccess::StaticStack(0)),
                        ctx,
                    )?;
                    write!(writer, "[-")?;
                    self.write_instruction(writer, Instruction::InlineBlock(zero), ctx)?;
                    self.write_instruction(
                        writer,
                        Instruction::GoTo(ValueAccess::StaticStack(0)),
                        ctx,
                    )?;
                    write!(writer, "]")?;
                    self.write_instruction(writer, Instruction::PopStaticStack(1), ctx)?;
                }
            }
            Instruction::CallIfZero {
                mut position,
                zero,
                nonzero,
                tmp: all_temp,
            } => {
                if let Some(tmp) = all_temp[0] {
                    self.write_instruction(
                        writer,
                        Instruction::Move {
                            src: position,
                            dst: tmp,
                            tmp: Some(ValueAccess::Register(Register(0))),
                        },
                        ctx,
                    )?;
                    position = tmp;
                }
                self.write_instruction(writer, Instruction::SetCall(nonzero), ctx)?;
                self.write_instruction(writer, Instruction::GoTo(position), ctx)?;
                write!(writer, "[[-]")?;
                self.write_instruction(
                    writer,
                    Instruction::Increment {
                        position: ValueAccess::Raw(self.continue_position + 3),
                        value: zero as i32 - nonzero as i32,
                        tmp: all_temp[1],
                    },
                    ctx,
                )?;
                self.write_instruction(writer, Instruction::GoTo(position), ctx)?;
                write!(writer, "]")?;
            }
            Instruction::PushStaticStack(s) => {
                ctx.static_stack_index += s;
                ctx.highest_static_stack_index =
                    ctx.highest_static_stack_index.max(ctx.static_stack_index);
            }
            Instruction::PopStaticStack(s) => {
                ctx.static_stack_index -= s;
            }
            Instruction::InlineForRange {
                times,
                block,
                tmp: all_temp,
            } => {
                if let Some(tmp) = all_temp[0] {
                    self.write_instruction(
                        writer,
                        Instruction::SetConstant {
                            position: tmp,
                            value: times,
                            tmp: all_temp[1],
                        },
                        ctx,
                    )?;
                    write!(writer, "[-")?;
                    self.write_instruction(writer, Instruction::InlineBlock(block), ctx)?;
                    self.write_instruction(writer, Instruction::GoTo(tmp), ctx)?;
                    write!(writer, "]")?;
                } else {
                    for _ in 0..times {
                        self.write_instruction(
                            writer,
                            Instruction::InlineBlock(block.clone()),
                            ctx,
                        )?;
                    }
                }
            }
            Instruction::PushStack(v) => {
                if v > 0 {
                    self.write_instruction(writer, Instruction::GoTo(ValueAccess::Stack(0)), ctx)?;
                    write!(writer, "<")?;
                    for _ in 0..v {
                        for _ in 0..self.module.stack_align + 1 {
                            write!(writer, "<")?;
                        }
                        write!(writer, "+")?;
                    }
                    write!(writer, ">")?;
                    ctx.stack_position_to_end = Some(0);
                }
            }
            Instruction::PopStack(v) => {
                if v > 0 {
                    self.write_instruction(writer, Instruction::GoTo(ValueAccess::Stack(0)), ctx)?;
                    write!(writer, "<-")?;
                    for _ in 0..v - 1 {
                        for _ in 0..self.module.stack_align + 1 {
                            write!(writer, ">")?;
                        }
                        write!(writer, "-")?;
                    }
                    for _ in 0..self.module.stack_align + 2 {
                        write!(writer, ">")?;
                    }
                    ctx.stack_position_to_end = Some(0);
                }
            }
        }
        Ok(())
    }
    pub fn write(&self, writer: &mut impl Write) -> std::io::Result<()> {
        let mut block_context = BlockContext {
            ptr_position: 0,
            indent_level: 0,
            static_stack_index: 0,
            highest_static_stack_index: 0,
            stack_position_to_end: None,
            parameter_stack: Vec::new(),
        };
        let ctx = &mut block_context;
        self.write_instruction(
            writer,
            Instruction::SetConstant {
                position: ValueAccess::Raw(self.stack_range.end - self.module.stack_align - 1),
                value: 1,
                tmp: None,
            },
            ctx,
        )?;
        self.write_instruction(
            writer,
            Instruction::SetConstant {
                position: ValueAccess::Raw(self.continue_position),
                value: 1,
                tmp: None,
            },
            ctx,
        )?;
        self.write_instruction(
            writer,
            Instruction::SetConstant {
                position: ValueAccess::Raw(self.continue_position + 2),
                value: 1,
                tmp: None,
            },
            ctx,
        )?;
        self.write_instruction(
            writer,
            Instruction::SetConstant {
                position: ValueAccess::Raw(self.continue_position + 3),
                value: self.module.start,
                tmp: Some(ValueAccess::Raw(self.continue_position + 4)),
            },
            ctx,
        )?;
        self.write_instruction(
            writer,
            Instruction::SetConstant {
                position: ValueAccess::Raw(self.continue_position + 4),
                value: 1,
                tmp: None,
            },
            ctx,
        )?;
        for (i, static_var) in self.module.static_variables.iter().enumerate() {
            if let Some(init) = &static_var.initialization {
                self.write_instruction(
                    writer,
                    Instruction::GoTo(ValueAccess::StaticVariable(i as u32, 0)),
                    ctx,
                )?;
                for (j, thing) in init.iter().enumerate() {
                    if self.write_options.pretty {
                        write!(writer, "\n\t")?;
                    }
                    self.write_instruction(
                        writer,
                        Instruction::SetConstant {
                            position: ValueAccess::StaticVariable(i as u32, j as u32),
                            value: *thing,
                            tmp: Some(ValueAccess::StaticVariable(i as u32, j as u32 + 1)),
                        },
                        ctx,
                    )?;
                }
                if self.write_options.pretty {
                    write!(writer, "\n")?;
                }
            }
        }
        self.write_instruction(
            writer,
            Instruction::GoTo(ValueAccess::Raw(self.continue_position)),
            ctx,
        )?;
        self.write_instruction(writer, Instruction::IntrinsicStartLoop, ctx)?;
        self.write_instruction(
            writer,
            Instruction::GoTo(ValueAccess::Raw(self.continue_position + 2)),
            ctx,
        )?;
        for global_call in &self.module.global_calls {
            write!(writer, "[>[->-<]>[-<<-")?;
            ctx.ptr_position = self.continue_position + 2;
            self.write_block(writer, &self.module.code_blocks[*global_call as usize], ctx)?;
            self.write_instruction(
                writer,
                Instruction::GoTo(ValueAccess::Raw(self.continue_position + 4)),
                ctx,
            )?;
            write!(writer, "]+<<<]>>>[>]<<<")?;
            ctx.ptr_position = self.continue_position + 2;
        }
        self.write_instruction(
            writer,
            Instruction::GoTo(ValueAccess::Raw(self.continue_position)),
            ctx,
        )?;
        self.write_instruction(writer, Instruction::IntrinsicEndLoop, ctx)?;
        if ctx.highest_static_stack_index > self.module.static_stack_size {
            panic!("Overflowed static stack");
        }
        // Set up static variables, stack alignment, set function call to start function, move to correct position in init_block

        Ok(())
    }
    pub fn new(module: AsmModule, write_options: AsmModuleWriteOptions) -> Self {
        let stack_range = 0..(module.stack_size + 1) * (module.stack_align + 1) + 1;
        let register_range = stack_range.end..stack_range.end + module.num_registers + 2;
        let continue_position = register_range.end;
        let call_stuff_range = continue_position + 1..continue_position + 6;
        let static_stack_range =
            call_stuff_range.end..call_stuff_range.end + module.static_stack_size;
        let mut static_range = static_stack_range.end..static_stack_range.end;
        let mut static_ranges = Vec::new();
        for var in &module.static_variables {
            static_ranges.push(static_range.end..static_range.end + var.size);
            static_range.end += var.size;
        }
        let heap_start = static_range.end;
        let stack_call_width = if module.global_calls.len() > u16::MAX as usize {
            4
        } else if module.global_calls.len() > u8::MAX as usize {
            2
        } else {
            1
        };
        Self {
            module,
            write_options,
            stack_range,
            register_range,
            continue_position,
            call_stuff_range,
            static_range,
            static_ranges,
            stack_call_width,
            static_stack_range,
            heap_start,
        }
    }
}
