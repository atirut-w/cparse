use std::ops::Range;

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
    fn test() {
        let tests = std::fs::read_dir("tests").expect("Failed to read tests directory");
        for entry in tests {
            let path = entry.expect("Failed to read entry").path();
            if path.extension().and_then(|s| s.to_str()) == Some("c") {
                let input = std::fs::read_to_string(&path).expect("Failed to read test input file");
                let lexer = lexer::Lexer::new(&input);
                let mut parser = parser::Parser::new(lexer);
                match parser.parse_translation_unit() {
                    Ok(ast) => println!("Parsed AST for {}: {:#?}", path.display(), ast),
                    Err(e) => panic!(
                        "Error parsing {}: {} at {}:{}",
                        path.display(),
                        e.message,
                        e.span.start.line,
                        e.span.start.column
                    ),
                }
            }
        }
    }
}
