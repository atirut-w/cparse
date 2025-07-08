use crate::{Error, SourcePosition};
use std::{collections::HashMap, ops::Range};

pub struct Lexer<'a> {
    input: &'a str,
    position: SourcePosition,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            position: SourcePosition {
                line: 1,
                column: 1,
                offset: 0,
            },
        }
    }

    pub fn next_token(&mut self) -> Result<Token, Error> {
        self.skip_whitespace();
        let start_position = self.position.clone();

        let ch = match self.next_char() {
            Some(c) => c,
            None => {
                return Ok(Token {
                    kind: TokenKind::EOF,
                    span: start_position.until(&self.position),
                });
            }
        };

        let kind = match ch {
            '0' => {
                if !self.is_at_word_boundary() {
                    return Err(Error {
                        message: "Non-decimal integer constants not supported".to_string(),
                        span: start_position.until(&self.position),
                    });
                }
                TokenKind::IntConstant(0)
            }
            '1'..='9' => {
                let mut number = ch.to_string();
                while let Some(next_ch) = self.peek_char() {
                    if next_ch.is_digit(10) {
                        number.push(self.next_char().unwrap());
                    } else if !self.is_at_word_boundary() {
                        let invalid_ch = self.next_char().unwrap();
                        return Err(Error {
                            message: format!("Invalid digit '{}' in decimal constant", invalid_ch),
                            span: start_position.until(&self.position),
                        });
                    } else {
                        break;
                    }
                }

                TokenKind::IntConstant(number.parse::<i64>().unwrap())
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let keywords =
                    HashMap::from([("int", TokenKind::Int), ("return", TokenKind::Return)]);
                let mut identifier = ch.to_string();

                while let Some(_next_ch) = self.peek_char() {
                    if !self.is_at_word_boundary() {
                        identifier.push(self.next_char().unwrap());
                    } else {
                        break;
                    }
                }

                if let Some(keyword) = keywords.get(identifier.as_str()) {
                    keyword.clone()
                } else {
                    TokenKind::Identifier(identifier)
                }
            }
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,
            ';' => TokenKind::Semicolon,
            '~' => TokenKind::Tilde,
            '-' => {
                if self.peek_char() == Some('-') {
                    self.next_char(); // Consume the second '-'
                    TokenKind::MinusMinus
                } else {
                    TokenKind::Minus
                }
            }
            '+' => TokenKind::Plus,
            '*' => TokenKind::Asterisk,
            '/' => TokenKind::Slash,
            '%' => TokenKind::Percent,

            _ => {
                return Err(Error {
                    message: format!("Unexpected character: '{}'", ch),
                    span: start_position.until(&self.position),
                });
            }
        };

        Ok(Token {
            kind,
            span: start_position.until(&self.position),
        })
    }

    fn is_at_word_boundary(&self) -> bool {
        match self.peek_char() {
            Some(ch) => !(ch.is_alphanumeric() || ch == '_'),
            None => true, // End of input is a word boundary
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek_char() {
            if ch.is_whitespace() {
                self.next_char();
            } else {
                break;
            }
        }
    }

    fn next_char(&mut self) -> Option<char> {
        if self.position.offset < self.input.len() {
            let ch = self.input[self.position.offset..].chars().next()?;
            self.position.offset += ch.len_utf8();

            if ch == '\r' {
                if let Some(next_ch) = self.input[self.position.offset..].chars().next() {
                    if next_ch == '\n' {
                        self.position.offset += next_ch.len_utf8();
                    }
                }
                self.position.line += 1;
                self.position.column = 1;
            } else if ch == '\n' {
                self.position.line += 1;
                self.position.column = 1;
            } else {
                self.position.column += ch.len_utf8();
            }

            Some(ch)
        } else {
            None
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.position.offset..].chars().next()
    }

    pub fn peek_token(&mut self) -> Result<Token, Error> {
        let current_position = self.position.clone();
        let token = self.next_token();
        self.position = current_position;
        token
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Range<SourcePosition>,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum TokenKind {
    Identifier(String),
    IntConstant(i64),

    Int,

    Return,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Semicolon,

    Tilde,
    Minus,
    MinusMinus,
    Plus,
    Asterisk,
    Slash,
    Percent,

    EOF,
}
