use super::asm_module::*;
use logos::{Lexer, Logos, Span};
use std::{
    collections::HashMap,
    fmt::Debug,
    ops::{Deref, DerefMut},
};

pub use ast::Parser;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    RegisterOutOfRange {
        max: Spanned<u32>,
        value: Spanned<u32>,
    },
    StaticOutOfRange {
        len: Spanned<u32>,
        offset: Spanned<u32>,
    },
    Other(SpannedString),
    Unspanned(String),
    Unexpected(Span, Option<String>),
    UnexpectedEOF,
}
impl From<SpannedString> for Error {
    fn from(value: SpannedString) -> Self {
        Self::Other(value)
    }
}
impl From<Spanned<&str>> for Error {
    fn from(value: Spanned<&str>) -> Self {
        Self::Other(Spanned(value.0.to_owned(), value.1))
    }
}
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Spanned<T: Clone + Debug>(pub T, pub Span);
impl<T: Clone + Debug> Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<T: Clone + Debug> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
type SpannedString = Spanned<String>;
pub type Result<T> = std::result::Result<T, Error>;
impl Default for Error {
    fn default() -> Self {
        Self::Other(Spanned(String::new(), Span::default()))
    }
}

#[derive(Logos, Debug, PartialEq, Eq, Hash, Clone)]
#[logos(skip r"[ \t\r\n\f]+")]
#[logos(error = Error)]
enum Token {
    #[token("static")]
    Static,
    #[token("global")]
    Global,
    #[token("block")]
    Block,
    #[token(">>")]
    MoveRight,
    #[token("<<")]
    MoveLeft,
    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,
    #[regex(r"[0-9]+", |lex| lex.slice().parse::<u32>().unwrap())]
    Number(u32),
    #[regex(
        r#""([^"\\]|\\["\\nrt0]|[a-fA-F0-9]{2})*""#,
        |lex| parse_string(lex.slice(), lex.span())
    )]
    String(String),
    #[token("{")]
    BraceOpen,
    #[token("}")]
    BraceClose,
    #[token("(")]
    ParenOpen,
    #[token(")")]
    ParenClose,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token("=")]
    Move,
    #[token("bfraw")]
    BfRaw,
    #[token("goto")]
    GoTo,
    #[token("exit")]
    SetExit,
    #[token("len")]
    Length,
    #[token("meta")]
    Meta,
    #[regex(r#"[_a-zA-Z][_a-zA-Z0-9]*"#, |lex| lex.slice().to_owned(), priority = 1)]
    Ident(String),
    #[token("rtp")]
    TempRegister,
    #[regex("r([1-9][0-9]*)", |lex| lex.slice()[1..].parse::<u32>().unwrap()-1, priority = 2)]
    NumberedRegister(u32),
    #[token("stack")]
    Stack,
    #[token("staticstack")]
    StaticStack,
    #[token("const")]
    Const,
    #[token("updateptr")]
    UpdatePtr,
    #[token("repeat")]
    Repeat,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("++")]
    Add,
    #[token("--")]
    Subtract,
    #[token("inline")]
    Inline,
    #[token("push")]
    Push,
    #[token("pop")]
    Pop,
    #[token("dynamic")]
    Dynamic,
    #[token("loop")]
    Loop,
    #[regex("//[^\\n]*", logos::skip)]
    LineComment,
    #[regex("/*", parse_block_comment)]
    BlockComment,
    #[token("current")]
    Current,
    #[token("while")]
    While,
    #[token("zero")]
    Zero,
    #[token("notzero")]
    NotZero,
}
impl Error {
    pub fn ariadne_properties(&self) -> (String, Vec<(String, Span)>, Vec<String>) {
        match self {
            Self::RegisterOutOfRange { max, value } => (
                "Register out of range".to_string(),
                vec![
                    (format!("Register used here"), value.1.clone()),
                    (format!("Number of registers defined here"), max.1.clone()),
                ],
                vec![format!(
                    "Number of registers is {}, but register had index {}",
                    max.0, value.0
                )],
            ),
            Self::StaticOutOfRange { len, offset } => (
                "Static offset out of range".to_string(),
                vec![
                    (format!("Static offset declared here"), offset.1.clone()),
                    (format!("Static length declared here"), len.1.clone()),
                ],
                vec![format!(
                    "Static variable length is {}, but static access had offset {}",
                    len.0, offset.0
                )],
            ),
            Self::Other(Spanned(s, span)) => (
                s.clone(),
                vec![(format!("Error here"), span.clone())],
                vec![],
            ),
            Self::UnexpectedEOF => (format!("Unexpected end of file"), vec![], vec![]),
            Self::Unexpected(span, desc) => (
                format!("Unexpected token"),
                vec![(format!("Unexpected token here"), span.clone())],
                Vec::from_iter(desc.as_ref().map(|s| format!("Expected {s}"))),
            ),
            Self::Unspanned(s) => (s.clone(), vec![], vec![]),
        }
    }
    pub fn print_ariadne(&self, source: &str, program: &str) {
        let properties = self.ariadne_properties();
        use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};

        let mut colors = ColorGenerator::new();
        let out = Color::Fixed(81);

        let mut report =
            Report::build(ReportKind::Error, ("sample.tao", 12..12)).with_message(properties.0);
        for (label, span) in properties.1 {
            let color = colors.next();
            report.add_label(
                Label::new((source, span))
                    .with_message(label.fg(color))
                    .with_color(color),
            );
        }
        for note in properties.2 {
            report.add_note(note.fg(out));
        }
        report
            .finish()
            .print((source, Source::from(program)))
            .unwrap();
    }
}
fn parse_string(slice: &str, span: Span) -> Result<String> {
    if !slice.is_ascii() {
        return Err(Spanned(
            format!("String literals must contain only ascii characters!"),
            span,
        )
        .into());
    }
    let mut out = String::new();
    let slice = &slice[1..slice.len() - 1];
    let mut chars = slice.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            out.push(match chars.next() {
                Some('n') => '\n',
                Some('r') => '\r',
                Some('t') => '\t',
                Some('\\') => '\\',
                Some('"') => '"',
                Some('0') => '\0',
                Some(c2) => {
                    if let Some(c3) = chars.next() {
                        let a = String::from_utf8_lossy(&[c2 as u8, c3 as u8]).to_string();
                        if let Ok(r) = u8::from_str_radix(&a, 16) {
                            r as char
                        } else {
                            return Err(Spanned(
                                format!("Unrecognized string escape sequence: \\{c2}{c3}"),
                                span,
                            )
                            .into());
                        }
                    } else {
                        unreachable!()
                    }
                }
                None => unreachable!(),
            });
        } else {
            out.push(c);
        }
    }
    Ok(out)
}
fn parse_block_comment<'a>(lex: &mut Lexer<'a, Token>) -> logos::Skip {
    let mut remainder = lex.remainder();
    let mut jumped = 0;
    let mut depth = 1;
    while depth > 0 {
        let next_open = remainder.find("/*").unwrap_or(usize::MAX);
        let next_close = remainder.find("*/");
        if let Some(next_close) = next_close {
            if next_open < next_close {
                depth += 1;
                jumped += next_open + 2;
                remainder = &remainder[next_open + 2..];
            } else {
                depth -= 1;
                jumped += next_close + 2;
                remainder = &remainder[next_close + 2..];
            }
        } else {
            // There is no next close, so everything else is commented, skip to the very end
            jumped += remainder.len();
            break;
        }
    }
    lex.bump(jumped);
    logos::Skip
}
pub(crate) mod ast {
    use logos::SpannedIter;

    use super::*;
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum ValueAccessExpression {
        Raw(ValueAccess, Option<SpannedInteger>),
        StaticVariable(SpannedString, SpannedInteger),
    }
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct BlockStatement {
        global_call: Option<u32>,
        instructions: Vec<SpannedInstruction>,
        num_params: u32,
    }
    #[derive(Debug, Clone, PartialEq, Eq, Default)]
    pub struct InstructionStatement {
        instruction: Instruction,
        value_overrides: Vec<Spanned<(Integer, bool)>>,
        value_access_overrides: Vec<SpannedValueAccess>,
        block_overrides: Vec<(SpannedString, Vec<SpannedValueAccess>)>,
    }
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Integer {
        Literal(u32),
        Length(String),
        Constant(String),
    }
    impl Default for Integer {
        fn default() -> Self {
            Self::Literal(0)
        }
    }
    type SpannedValueAccess = Spanned<ValueAccessExpression>;
    type SpannedInteger = Spanned<Integer>;
    type SpannedInstruction = Spanned<InstructionStatement>;
    type SpannedBlock = Spanned<BlockStatement>;
    pub struct Parser<'source> {
        lexer: SpannedIter<'source, Token>,
        pub statics: Vec<Spanned<(StaticVariable, Option<SpannedInteger>)>>,
        pub static_names: HashMap<String, u32>,
        pub blocks: Vec<SpannedBlock>,
        pub block_names: HashMap<String, u32>,
        pub num_globals: u32,
        pub constants: HashMap<String, Spanned<u32>>,

        pub entry: Option<SpannedString>,
        pub registers: Option<SpannedInteger>,
        pub stack_align: Option<SpannedInteger>,
        pub stack_size: Option<SpannedInteger>,
        pub static_stack_size: Option<SpannedInteger>,
    }
    impl<'a> Parser<'a> {
        pub fn new(input: &'a str) -> Self {
            let lexer = Token::lexer(input).spanned();
            Self {
                lexer,
                statics: Vec::new(),
                static_names: HashMap::new(),
                blocks: Vec::new(),
                block_names: HashMap::new(),
                num_globals: 0,
                constants: HashMap::new(),

                entry: None,
                registers: None,
                stack_align: None,
                stack_size: None,
                static_stack_size: None,
            }
        }
    }
    impl<'source> Parser<'source> {
        pub fn parse(&mut self) -> Result<()> {
            let mut global: Option<Span> = None;
            while let Some(next) = self.lexer.next() {
                match next.0? {
                    Token::Static => {
                        if let Some(g) = global.clone() {
                            return Err(Error::Unexpected(g, None));
                        }
                        self.parse_static(next.1.start)?;
                    }
                    Token::Meta => {
                        if let Some(global) = global.clone() {
                            return Err(Error::Unexpected(global, None));
                        }
                        self.parse_meta()?;
                    }
                    Token::Const => {
                        if let Some(global) = global.clone() {
                            return Err(Error::Unexpected(global, None));
                        }
                        self.parse_const(next.1.start)?;
                    }
                    Token::Block => {
                        let span_start = if let Some(g) = global.clone() {
                            g.start
                        } else {
                            next.1.start
                        };
                        let next2 = self.next_token()?;
                        let id = if let Token::Ident(id) = next2.0 {
                            id
                        } else {
                            return Err(Error::Unexpected(next2.1, Some("identifier".to_string())));
                        };
                        self.expect_token(Token::ParenOpen)?;
                        let mut params = HashMap::new();
                        let mut prev_was_comma = true;
                        let mut i = 0;
                        let header_finish;
                        loop {
                            let t = self.next_token()?;
                            if let Token::ParenClose = t.0 {
                                header_finish = t.1.end;
                                break;
                            } else if let Token::Comma = t.0 {
                                if prev_was_comma {
                                    return Err(Error::Unexpected(
                                        t.1,
                                        Some("identifier or ')'".to_string()),
                                    ));
                                }
                                prev_was_comma = true;
                            } else if let Token::Ident(id) = t.0 {
                                if !prev_was_comma {
                                    return Err(Error::Unexpected(
                                        t.1,
                                        Some("',' or ')'".to_string()),
                                    ));
                                }
                                prev_was_comma = false;
                                params.insert(id, i);
                                i += 1;
                            } else {
                                return Err(Error::Unexpected(
                                    t.1,
                                    Some(
                                        if prev_was_comma {
                                            "identifier or ')'"
                                        } else {
                                            "',' or ')'"
                                        }
                                        .to_string(),
                                    ),
                                ));
                            }
                        }
                        let mut b = self.parse_block(&params, span_start)?;
                        b.1 = span_start..header_finish; // We want the span to only cover the header and not body
                        if global.is_some() {
                            b.0.global_call = Some(self.num_globals);
                            self.num_globals += 1;
                            global = None;
                        }
                        self.block_names.insert(id, self.blocks.len() as u32);
                        self.blocks.push(b);
                    }
                    Token::Global => {
                        if let Some(global) = global {
                            return Err(Error::Unexpected(global, Some("'block'".to_string())));
                        }
                        global = Some(next.1);
                    }
                    _ => return Err(Error::Unexpected(next.1, None)),
                }
            }
            if let Some(global) = global {
                return Err(Spanned("Unused final global modifier".to_owned(), global).into());
            }
            if self.entry.is_none()
                || self.registers.is_none()
                || self.stack_align.is_none()
                || self.stack_size.is_none()
                || self.static_stack_size.is_none()
            {
                return Err(Error::Unspanned("Missing meta statement(s)".to_owned()));
            }
            Ok(())
        }
        pub fn lower(&mut self) -> Result<AsmModule> {
            for i in 0..self.statics.len() {
                if let Some(a) = &self.statics[i].0 .1 {
                    self.statics[i].0 .0.size = self.lower_integer(a)?;
                }
            }
            let mut module = AsmModule {
                num_registers: self.lower_integer(self.registers.as_ref().unwrap())?,
                stack_align: self.lower_integer(self.stack_align.as_ref().unwrap())?,
                stack_size: self.lower_integer(self.stack_size.as_ref().unwrap())?,
                static_stack_size: self.lower_integer(self.static_stack_size.as_ref().unwrap())?,
                start: self.lower_global_call_id(self.entry.as_ref().unwrap())?,

                static_variables: Vec::new(),
                code_blocks: Vec::new(),
                global_calls: Vec::new(),
            };
            for (block_idx, block) in self.blocks.iter().enumerate() {
                if let Some(_) = block.global_call {
                    module.global_calls.push(block_idx as u32);
                    if block.num_params != 0 {
                        return Err(Spanned(
                            "Global block cannot have parameters".to_string(),
                            block.1.clone(),
                        )
                        .into());
                    }
                }
                let mut b = CodeBlock {
                    instructions: Vec::new(),
                    num_parameters: block.num_params,
                };
                for instruction in &block.instructions {
                    b.instructions.push(self.lower_instruction(&instruction)?);
                }
                module.code_blocks.push(b);
            }
            for Spanned((v, _), _) in &mut self.statics {
                module.static_variables.push(v.clone());
            }
            Ok(module)
        }
        pub fn get_code(&mut self) -> String {
            self.lexer.slice().to_owned()
        }
        fn next_token(&mut self) -> Result<(Token, Span)> {
            match self.lexer.next() {
                Some((Ok(token), span)) => Ok((token, span)),
                Some((Err(e), _)) => Err(e),
                None => Err(Error::UnexpectedEOF),
            }
        }
        fn expect_token(&mut self, token: Token) -> Result<Span> {
            let next = self.next_token()?;
            if token == next.0 {
                Ok(next.1)
            } else {
                Err(Error::Unexpected(next.1, Some(format!("'{token:?}'"))))
            }
        }
        fn parse_ident(&mut self) -> Result<SpannedString> {
            let ident = self.next_token()?;
            if let Token::Ident(value) = ident.0 {
                Ok(Spanned(value, ident.1))
            } else {
                Err(Error::Unexpected(ident.1, Some("identifier".to_string())))
            }
        }
        fn parse_integer(&mut self) -> Result<SpannedInteger> {
            let t = self.next_token()?;
            match t.0 {
                Token::Number(num) => Ok(Spanned(Integer::Literal(num), t.1)),
                Token::Length => {
                    let id = self.parse_ident()?;
                    Ok(Spanned(Integer::Length(id.0), t.1.start..id.1.end))
                }
                Token::Ident(id) => Ok(Spanned(Integer::Constant(id), t.1)),
                _ => Err(Error::Unexpected(t.1, Some("integer".to_string()))),
            }
        }
        pub fn parse_string(&mut self) -> Result<SpannedString> {
            let ident = self.next_token()?;
            if let Token::String(value) = ident.0 {
                Ok(Spanned(value, ident.1))
            } else {
                Err(Error::Unexpected(ident.1, Some("string".to_string())))
            }
        }
        fn parse_value_access(
            &mut self,
            params: &HashMap<String, u32>,
        ) -> Result<SpannedValueAccess> {
            let initial_token = self.next_token()?;
            match &initial_token.0 {
                Token::NumberedRegister(register) => Ok(Spanned(
                    ValueAccessExpression::Raw(ValueAccess::Register(Register(register + 2)), None),
                    initial_token.1,
                )),
                Token::TempRegister => Ok(Spanned(
                    ValueAccessExpression::Raw(ValueAccess::Register(Register(0)), None),
                    initial_token.1,
                )),
                Token::Current => Ok(Spanned(
                    ValueAccessExpression::Raw(ValueAccess::Current, None),
                    initial_token.1,
                )),
                Token::Stack | Token::StaticStack | Token::Ident(_) => {
                    self.expect_token(Token::BracketOpen)?;
                    let offset = self.parse_integer()?;
                    let span_end = self.expect_token(Token::BracketClose)?.end;
                    match initial_token.0 {
                        Token::Stack => Ok(Spanned(
                            ValueAccessExpression::Raw(ValueAccess::Stack(0), Some(offset)),
                            initial_token.1.start..span_end,
                        )),
                        Token::StaticStack => Ok(Spanned(
                            ValueAccessExpression::Raw(ValueAccess::StaticStack(0), Some(offset)),
                            initial_token.1.start..span_end,
                        )),
                        Token::Ident(id) => {
                            if let Some(i) = params.get("") {
                                Ok(Spanned(
                                    ValueAccessExpression::Raw(ValueAccess::Virtual(*i), None),
                                    initial_token.1.start..span_end,
                                ))
                            } else {
                                Ok(Spanned(
                                    ValueAccessExpression::StaticVariable(
                                        Spanned(id, initial_token.1.clone()),
                                        offset,
                                    ),
                                    initial_token.1.start..span_end,
                                ))
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                _ => Err(Error::Other(Spanned(
                    "Invalid value access expression".to_string(),
                    initial_token.1,
                ))),
            }
        }
        fn parse_block(
            &mut self,
            params: &HashMap<String, u32>,
            span_start: usize,
        ) -> Result<SpannedBlock> {
            let mut res = BlockStatement {
                global_call: None,
                instructions: Vec::new(),
                num_params: params.len() as u32,
            };
            self.expect_token(Token::BraceOpen)?;
            let mut span_end;
            loop {
                let instruction = self.parse_instruction(params)?;
                span_end = instruction.1.end;
                if let Some(i) = instruction.0 {
                    res.instructions.push(Spanned(i, instruction.1));
                } else {
                    break;
                }
            }
            Ok(Spanned(res, span_start..span_end))
        }
        fn parse_inline_call(
            &mut self,
            params: &HashMap<String, u32>,
        ) -> Result<(
            Spanned<InlineCall>,
            Option<(SpannedString, Vec<SpannedValueAccess>)>,
        )> {
            todo!()
        }
        fn parse_instruction(
            &mut self,
            params: &HashMap<String, u32>,
        ) -> Result<(Option<InstructionStatement>, Span)> {
            let (t, mut span) = self.next_token()?;
            let r = match t {
                Token::BraceClose => return Ok((None, span)),
                Token::BfRaw => {
                    let bfstr = self.parse_string()?;
                    span.end = bfstr.1.end;
                    InstructionStatement {
                        instruction: Instruction::IntrinsicText(bfstr.0),
                        ..Default::default()
                    }
                }
                Token::GoTo => {
                    let dest = self.parse_value_access(params)?;
                    span.end = dest.1.end;
                    InstructionStatement {
                        instruction: Instruction::GoTo(ValueAccess::Raw(0)),
                        value_access_overrides: vec![dest],
                        ..Default::default()
                    }
                }
                Token::MoveLeft => {
                    let amount = self.parse_integer()?;
                    span.end = amount.1.end;
                    InstructionStatement {
                        instruction: Instruction::IntrinsicMove(0),
                        value_overrides: vec![Spanned((amount.0, true), amount.1)],
                        ..Default::default()
                    }
                }
                Token::MoveRight => {
                    let amount = self.parse_integer()?;
                    span.end = amount.1.end;
                    InstructionStatement {
                        instruction: Instruction::IntrinsicMove(0),
                        value_overrides: vec![Spanned((amount.0, false), amount.1)],
                        ..Default::default()
                    }
                }
                Token::UpdatePtr => {
                    let target = self.parse_value_access(params)?;
                    span.end = target.1.end;
                    InstructionStatement {
                        instruction: Instruction::IntrinsicUpdatePtrIndex(ValueAccess::Raw(0)),
                        value_access_overrides: vec![target],
                        ..Default::default()
                    }
                }
                Token::SetExit => InstructionStatement {
                    instruction: Instruction::SetExit,
                    ..Default::default()
                },
                Token::Add => {
                    let value = self.parse_integer()?;
                    span.end = value.1.end;
                    InstructionStatement {
                        instruction: Instruction::Increment {
                            position: ValueAccess::Current,
                            value: 0,
                            tmp: None,
                        },
                        value_overrides: vec![Spanned((value.0, false), value.1)],
                        ..Default::default()
                    }
                }
                Token::Subtract => {
                    let value = self.parse_integer()?;
                    span.end = value.1.end;
                    InstructionStatement {
                        instruction: Instruction::Increment {
                            position: ValueAccess::Current,
                            value: 0,
                            tmp: None,
                        },
                        value_overrides: vec![Spanned((value.0, true), value.1)],
                        ..Default::default()
                    }
                }
                Token::Push => {
                    let typ = self.next_token()?;
                    let value = self.parse_integer()?;
                    span.end = value.1.end;
                    InstructionStatement {
                        instruction: match typ.0 {
                            Token::Stack => Instruction::PushStack(0),
                            Token::StaticStack => Instruction::PushStaticStack(0),
                            _ => {
                                return Err(Error::Unexpected(
                                    typ.1,
                                    Some("'stack' or 'staticstack'".to_string()),
                                ))
                            }
                        },
                        value_overrides: vec![Spanned((value.0, false), value.1)],
                        ..Default::default()
                    }
                }
                Token::Pop => {
                    let typ = self.next_token()?;
                    let value = self.parse_integer()?;
                    span.end = value.1.end;
                    InstructionStatement {
                        instruction: match typ.0 {
                            Token::Stack => Instruction::PopStack(0),
                            Token::StaticStack => Instruction::PopStaticStack(0),
                            _ => {
                                return Err(Error::Unexpected(
                                    typ.1,
                                    Some("'stack' or 'staticstack'".to_string()),
                                ))
                            }
                        },
                        value_overrides: vec![Spanned((value.0, false), value.1)],
                        ..Default::default()
                    }
                }
                Token::Inline => {
                    let (call, other) = self.parse_inline_call(params)?;
                    span.end = call.1.end;
                    InstructionStatement {
                        instruction: Instruction::InlineBlock(call.0),
                        block_overrides: other.into_iter().collect(),
                        ..Default::default()
                    }
                }
                a => todo!("Token parsing not yet implemented: {a:?}"),
            };
            self.expect_token(Token::Semicolon)?;
            Ok((Some(r), span))
        }
        fn parse_meta(&mut self) -> Result<()> {
            let id = self.parse_ident()?;
            match id.0.as_str() {
                "entry" => self.entry = Some(self.parse_ident()?),
                "registers" => self.registers = Some(self.parse_integer()?),
                "stackalign" => self.stack_align = Some(self.parse_integer()?),
                "stacksize" => self.stack_size = Some(self.parse_integer()?),
                "staticstacksize" => self.static_stack_size = Some(self.parse_integer()?),
                other => {
                    return Err(Error::Other(Spanned(
                        format!("Unexpected meta declaration: attempted to set {other}"),
                        id.1,
                    )))
                }
            }
            self.expect_token(Token::Semicolon)?;
            Ok(())
        }
        fn parse_const(&mut self, span_start: usize) -> Result<()> {
            let id = self.parse_ident()?;
            self.expect_token(Token::Move)?;
            let token = self.next_token()?;
            let value = if let Token::Number(i) = token.0 {
                i
            } else {
                return Err(Error::Unexpected(
                    token.1,
                    Some("integer literal".to_string()),
                ));
            };
            self.expect_token(Token::Semicolon)?;
            self.constants
                .insert(id.0, Spanned(value, span_start..token.1.end));
            Ok(())
        }
        fn parse_static(&mut self, span_start: usize) -> Result<()> {
            let id = self.parse_ident()?.0;
            let mut size = None;
            let mut token = self.next_token()?;
            if let Token::BracketOpen = token.0 {
                size = Some(self.parse_integer()?);
                self.expect_token(Token::BracketClose)?;
                token = self.next_token()?;
            }
            let mut initialization: Option<Vec<u32>> = None;
            if let Token::Move = token.0 {
                let t = self.next_token()?;
                if let Token::String(value) = t.0 {
                    initialization =
                        Some(value.as_bytes().into_iter().map(|i| *i as u32).collect());
                    token = self.next_token()?;
                } else if let Token::BracketOpen = t.0 {
                    let mut init = Vec::new();
                    let mut prev_was_comma = true;
                    loop {
                        let t = self.next_token()?;
                        if let Token::BracketClose = t.0 {
                            break;
                        } else if let Token::Comma = t.0 {
                            if prev_was_comma {
                                return Err(Error::Unexpected(
                                    t.1,
                                    Some("identifier or ']'".to_string()),
                                ));
                            }
                            prev_was_comma = true;
                        } else if let Token::Number(value) = t.0 {
                            if !prev_was_comma {
                                return Err(Error::Unexpected(t.1, Some("',' or ']'".to_string())));
                            }
                            prev_was_comma = false;
                            init.push(value);
                        } else {
                            return Err(Error::Unexpected(
                                t.1,
                                Some(
                                    if prev_was_comma {
                                        "identifier or ']'"
                                    } else {
                                        "',' or ']'"
                                    }
                                    .to_string(),
                                ),
                            ));
                        }
                    }
                    initialization = Some(init);
                    token = self.next_token()?;
                } else {
                    return Err(Error::Unexpected(
                        token.1,
                        Some("string or integer list".to_string()),
                    ));
                }
            }
            if token.0 != Token::Semicolon {
                return Err(Error::Unexpected(token.1, Some(format!("';'"))));
            }
            let self_span = span_start..token.1.end;
            match (&size, &initialization) {
                (None, None) => {
                    return Err(Error::Other(Spanned(
                        "Unsized and uninitialized static declaration".to_owned(),
                        self_span,
                    )))
                }
                (Some(_), Some(_)) => {
                    return Err(Error::Other(Spanned(
                        "Static declared with both size and initialization".to_owned(),
                        self_span,
                    )));
                }
                _ => (),
            }
            self.static_names.insert(id, self.statics.len() as u32);
            self.statics.push(Spanned(
                (
                    StaticVariable {
                        size: initialization.as_ref().map(|a| a.len()).unwrap_or(0) as u32,
                        initialization,
                    },
                    size,
                ),
                self_span,
            ));
            Ok(())
        }
        fn lower_integer(&self, integer: &SpannedInteger) -> Result<u32> {
            Ok(match &integer.0 {
                Integer::Constant(v) => {
                    if let Some(v) = self.constants.get(v) {
                        v.0
                    } else {
                        return Err(
                            Spanned(format!("Invalid constant: {}", v), integer.1.clone()).into(),
                        );
                    }
                }
                Integer::Length(v) => {
                    if let Some(&idx) = self.static_names.get(v) {
                        if let Some(a) = self.statics[idx as usize].0 .0.initialization.as_ref() {
                            a.len() as u32
                        } else {
                            self.lower_integer(self.statics[idx as usize].0 .1.as_ref().unwrap())?
                        }
                    } else {
                        return Err(
                            Spanned(format!("Invalid static: {v}"), integer.1.clone()).into()
                        );
                    }
                }
                Integer::Literal(v) => *v,
            })
        }
        fn lower_value_access(&self, access: SpannedValueAccess) -> Result<ValueAccess> {
            let span = access.1;
            match access.0 {
                ValueAccessExpression::Raw(mut access, offset) => {
                    if let Some(offset) = offset {
                        let value = self.lower_integer(&offset)?;
                        match &mut access {
                            ValueAccess::Raw(i) => *i = value,
                            ValueAccess::Register(_) => unreachable!(),
                            ValueAccess::Stack(i) => *i = value,
                            ValueAccess::StaticStack(i) => *i = value,
                            ValueAccess::Heap(i) => *i = value,
                            ValueAccess::Virtual(_) => unreachable!(),
                            ValueAccess::StaticVariable(_, _) => unreachable!(),
                            ValueAccess::Current => unreachable!(),
                        }
                    }
                    if let ValueAccess::Register(k) = &access {
                        let max = self.lower_integer(self.registers.as_ref().unwrap())?;
                        if k.0 >= max + 2 {
                            return Err(Error::RegisterOutOfRange {
                                max: Spanned(max, self.registers.as_ref().unwrap().1.clone()),
                                value: Spanned(k.0 - 2, span),
                            });
                        }
                    }
                    Ok(access)
                }
                ValueAccessExpression::StaticVariable(id, offset) => {
                    let a = self.lower_static_id(&id)?;
                    let b = self.lower_integer(&offset)?;
                    if b >= self.statics[a as usize].0 .0.size {
                        return Err(Error::StaticOutOfRange {
                            len: Spanned(
                                self.statics[a as usize].0 .0.size,
                                self.statics[a as usize].1.clone(),
                            ),
                            offset: Spanned(b, offset.1),
                        });
                    }
                    Ok(ValueAccess::StaticVariable(a, b))
                }
            }
        }
        fn lower_value_override(&self, ovrride: &Spanned<(Integer, bool)>) -> Result<i32> {
            let value =
                self.lower_integer(&Spanned(ovrride.0 .0.clone(), ovrride.1.clone()))? as i32;
            Ok(if ovrride.0 .1 { -value } else { value })
        }
        fn lower_instruction(&self, instruction: &SpannedInstruction) -> Result<Instruction> {
            let mut i = instruction.instruction.clone();
            let value_access = |index: usize| {
                self.lower_value_access(instruction.value_access_overrides[index].clone())
            };
            let value_override =
                |index: usize| self.lower_value_override(&instruction.value_overrides[index]);
            let global =
                |index: usize| self.lower_global_call_id(&instruction.block_overrides[index].0);
            let inline = |index: usize| -> Result<InlineCall> {
                let a = &instruction.block_overrides[index];
                let block = self.lower_block_id(&a.0)?;
                let mut parameters = Vec::with_capacity(a.1.len());
                for p in &a.1 {
                    parameters.push(self.lower_value_access(p.clone())?);
                }
                todo!(); // Incomplete: handle inline calls(where the block isn't declared elsewhere)
                Ok(InlineCall::Block { block, parameters })
            };
            let temp = |index: usize| {
                if instruction.value_access_overrides.len() > index {
                    Some(self.lower_value_access(instruction.value_access_overrides[index].clone()))
                } else {
                    None
                }
                .transpose()
            };
            match &mut i {
                Instruction::Noop
                | Instruction::IntrinsicText(_)
                | Instruction::SetExit
                | Instruction::IntrinsicInput
                | Instruction::IntrinsicOutput
                | Instruction::IntrinsicStartLoop
                | Instruction::IntrinsicEndLoop => (),
                Instruction::GoTo(a) | Instruction::IntrinsicUpdatePtrIndex(a) => {
                    *a = value_access(0)?;
                }
                Instruction::IntrinsicMove(a) | Instruction::IntrinsicUpdatePtrIndexDynamic(a) => {
                    *a = value_override(0)?;
                }
                Instruction::IntrinsicAdd(a) => {
                    *a = value_override(0)?;
                }
                Instruction::SetConstant {
                    position,
                    value,
                    tmp,
                } => {
                    *position = value_access(0)?;
                    *value = value_override(0)? as u32;
                    *tmp = temp(1)?;
                }
                Instruction::PushStaticStack(a)
                | Instruction::PopStaticStack(a)
                | Instruction::PushStack(a)
                | Instruction::PopStack(a) => {
                    *a = value_override(0)? as u32;
                }
                Instruction::InlineBlock(a) => {
                    *a = inline(0)?;
                }
                Instruction::SetCall(a) => {
                    *a = global(0)?;
                }
                Instruction::InlineForRange { times, block, tmp } => {
                    *times = value_override(0)? as u32;
                    *block = inline(0)?;
                    tmp[0] = temp(0)?;
                    tmp[1] = temp(1)?;
                }
                Instruction::Move { src, dst, tmp }
                | Instruction::Add { src, dst, tmp }
                | Instruction::Subtract { src, dst, tmp } => {
                    *src = value_access(0)?;
                    *dst = value_access(1)?;
                    *tmp = temp(2)?;
                }
                Instruction::Increment {
                    position,
                    value,
                    tmp,
                } => {
                    *position = value_access(0)?;
                    *value = value_override(0)?;
                    *tmp = temp(1)?;
                }
                Instruction::CallIfZero {
                    position,
                    zero,
                    nonzero,
                    tmp,
                } => {
                    *position = value_access(0)?;
                    *zero = global(0)?;
                    *nonzero = global(1)?;
                    tmp[0] = temp(1)?;
                    tmp[1] = temp(2)?;
                }
                Instruction::BlockIfZero {
                    position,
                    zero,
                    nonzero,
                    tmp,
                } => {
                    *position = value_access(0)?;
                    *zero = inline(0)?;
                    *nonzero = inline(1)?;
                    tmp[0] = temp(1)?;
                    tmp[1] = temp(2)?;
                }
            }
            Ok(i)
        }
        fn lower_global_call_id(&self, id: &SpannedString) -> Result<u32> {
            let block_id = self.lower_block_id(id)?;
            if let Some(call) = self.blocks[block_id as usize].global_call {
                Ok(call)
            } else {
                Err(Spanned(format!("Block is not global: {}", id.0), id.1.clone()).into())
            }
        }
        fn lower_block_id(&self, id: &SpannedString) -> Result<u32> {
            if let Some(i) = self.block_names.get(&id.0) {
                Ok(*i)
            } else {
                Err(Spanned(format!("Invalid block: {}", id.0), id.1.clone()).into())
            }
        }
        fn lower_static_id(&self, id: &SpannedString) -> Result<u32> {
            if let Some(i) = self.static_names.get(&id.0) {
                Ok(*i)
            } else {
                Err(Spanned(format!("Invalid static: {}", id.0), id.1.clone()).into())
            }
        }
    }
}
