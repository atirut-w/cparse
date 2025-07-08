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
    fn simple() {
        let input =
            std::fs::read_to_string("tests/simple.c").expect("Failed to read test input file");
        let lexer = lexer::Lexer::new(&input);
        let mut parser = parser::Parser::new(lexer);
        let ast = parser
            .parse_translation_unit()
            .expect("Failed to parse translation unit");
        println!("{:#?}", ast);
    }
}
