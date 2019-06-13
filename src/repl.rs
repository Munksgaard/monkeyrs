use crate::env::Environment;
use crate::evaluator::Evaluator;
use crate::lexer::Lexer;
use crate::parser::parse;
use std::io::{BufRead, Write};

const PROMPT: &[u8; 3] = b">> ";

pub fn start<R, W>(reader: &mut R, writer: &mut W) -> Result<(), std::io::Error>
where
    R: BufRead,
    W: Write,
{
    let mut env = Environment::new();

    loop {
        writer.write_all(PROMPT)?;
        writer.flush()?;

        let mut line = String::new();
        reader.read_line(&mut line)?;

        let lexer = Lexer::new(&line);

        let tokens: Vec<_> = lexer.collect();

        let ast = parse(tokens);

        let mut evaluator = Evaluator::new();

        let result = ast
            .and_then(|node| evaluator.eval(node, &mut env))
            .map(|x| x.to_string())
            .unwrap_or_else(|x| x);

        writer.write_all(format!("{}\n", result).as_bytes())?;
        writer.flush()?;
    }
}
