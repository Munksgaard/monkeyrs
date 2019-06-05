use crate::lexer::Lexer;
use std::io::{BufRead, Write};

const PROMPT: &[u8; 3] = b">> ";

pub fn start<R, W>(reader: &mut R, writer: &mut W) -> Result<(), std::io::Error>
where
    R: BufRead,
    W: Write,
{
    loop {
        writer.write_all(PROMPT)?;
        writer.flush()?;

        let mut line = String::new();
        reader.read_line(&mut line)?;

        let lexer = Lexer::new(&line);

        let result: Vec<_> = lexer.collect();

        writer.write_all(format!("{:?}\n", result).as_bytes())?;
        writer.flush()?;
    }
}
