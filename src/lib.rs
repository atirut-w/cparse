use std::ops::Range;

pub mod ast;
pub mod lexer;
pub mod parser;

#[derive(Debug, Clone)]
pub struct SourcePosition {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

#[derive(Debug)]
pub struct Error {
    pub message: String,
    pub span: Range<SourcePosition>,
}

impl SourcePosition {
    pub fn until(&self, other: &SourcePosition) -> Range<SourcePosition> {
        Range {
            start: self.clone(),
            end: other.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_simple() {
        let input =
            std::fs::read_to_string("tests/simple.c").expect("Failed to read test input file");
        let mut lexer = lexer::Lexer::new(&input);

        loop {
            match lexer.next_token() {
                Ok(token) => {
                    println!("{:?}", token);
                    if token.kind == lexer::TokenKind::EOF {
                        break;
                    }
                }
                Err(e) => {
                    panic!("Lexer error: {:?}", e);
                }
            }
        }
    }
}
