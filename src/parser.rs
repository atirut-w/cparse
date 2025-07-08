use crate::{
    Error,
    lexer::{Lexer, TokenKind},
};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self { lexer }
    }

    pub fn parse_translation_unit(&mut self) -> Result<TranslationUnit, Error> {
        Ok(TranslationUnit {
            function_definition: self.parse_function_definition()?,
        })
    }

    fn parse_function_definition(&mut self) -> Result<FunctionDefinition, Error> {
        self.expect_token(&TokenKind::Int)?;
        let name = self.expect_identiier()?;
        self.expect_token(&TokenKind::LeftParen)?;
        self.expect_token(&TokenKind::RightParen)?;
        self.expect_token(&TokenKind::LeftBrace)?;
        let body = self.parse_statement()?;
        self.expect_token(&TokenKind::RightBrace)?;

        Ok(FunctionDefinition { name, body })
    }

    fn parse_statement(&mut self) -> Result<Statement, Error> {
        self.expect_token(&TokenKind::Return)?;
        let expr = self.parse_expression()?;
        self.expect_token(&TokenKind::Semicolon)?;

        Ok(Statement::Return(expr))
    }

    fn parse_expression(&mut self) -> Result<Expression, Error> {
        let token = self.lexer.next_token()?;
        match token.kind {
            TokenKind::IntConstant(value) => Ok(Expression::IntConstant(value)),
            TokenKind::Tilde | TokenKind::Minus => {
                let operator = match token.kind {
                    TokenKind::Tilde => UnaryOperator::Complement,
                    TokenKind::Minus => UnaryOperator::Negate,
                    _ => unreachable!(),
                };
                let inner = self.parse_expression()?;
                Ok(Expression::UnaryOp {
                    op: operator,
                    expr: Box::new(inner),
                })
            }
            TokenKind::LeftParen => {
                let inner = self.parse_expression()?;
                self.expect_token(&TokenKind::RightParen)?;
                Ok(inner)
            }
            _ => {
                Err(Error {
                    message: format!("Malformed expression"),
                    span: token.span,
                })
            }
        }
    }

    fn expect_token(&mut self, expected: &TokenKind) -> Result<(), Error> {
        let token = self.lexer.next_token()?;
        if &token.kind != expected {
            return Err(Error {
                message: format!("Expected token {:?}, found {:?}", expected, token.kind),
                span: token.span,
            });
        }
        Ok(())
    }

    fn expect_identiier(&mut self) -> Result<String, Error> {
        let token = self.lexer.next_token()?;
        match token.kind {
            TokenKind::Identifier(ref id) => Ok(id.clone()),
            _ => Err(Error {
                message: format!("Expected identifier, found {:?}", token.kind),
                span: token.span,
            }),
        }
    }
}

#[derive(Debug)]
pub struct TranslationUnit {
    pub function_definition: FunctionDefinition,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub body: Statement,
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug)]
pub enum Expression {
    IntConstant(i64),
    UnaryOp {
        op: UnaryOperator,
        expr: Box<Expression>,
    },
}

#[derive(Debug)]
pub enum UnaryOperator {
    Complement,
    Negate,
}
