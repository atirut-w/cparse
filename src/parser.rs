use std::collections::HashMap;

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
        let expr = self.parse_expression(0)?;
        self.expect_token(&TokenKind::Semicolon)?;

        Ok(Statement::Return(expr))
    }

    fn parse_expression(&mut self, min_prec: u8) -> Result<Expression, Error> {
        let mut left = self.parse_factor()?;
        let mut next = self.lexer.peek_token()?;
        let prec = HashMap::from([
            (TokenKind::Asterisk, 50),
            (TokenKind::Slash, 50),
            (TokenKind::Percent, 50),
            (TokenKind::Plus, 45),
            (TokenKind::Minus, 45),
            (TokenKind::Lt, 35),
            (TokenKind::LtEq, 35),
            (TokenKind::Gt, 35),
            (TokenKind::GtEq, 35),
            (TokenKind::EqEq, 30),
            (TokenKind::Neq, 30),
            (TokenKind::And, 10),
            (TokenKind::Or, 5),
        ]);

        while matches!(
            next.kind,
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Asterisk
                | TokenKind::Slash
                | TokenKind::Percent
                | TokenKind::Lt
                | TokenKind::LtEq
                | TokenKind::Gt
                | TokenKind::GtEq
                | TokenKind::EqEq
                | TokenKind::Neq
                | TokenKind::And
                | TokenKind::Or
        ) && prec.get(&next.kind).unwrap() >= &min_prec
        {
            let op = self.parse_binary_operator()?;
            let right = self.parse_expression(prec.get(&next.kind).unwrap() + 1)?;
            left = Expression::BinaryExpression {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
            next = self.lexer.peek_token()?;
        }
        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<Expression, Error> {
        let token = self.lexer.peek_token()?;
        match token.kind {
            TokenKind::IntConstant(value) => {
                self.lexer.next_token()?; // consume the token
                Ok(Expression::IntConstant(value))
            }
            TokenKind::Tilde | TokenKind::Minus | TokenKind::Not => {
                let op = self.parse_unary_operator()?;
                let inner = self.parse_factor()?;
                Ok(Expression::UnaryOp {
                    op,
                    expr: Box::new(inner),
                })
            }
            TokenKind::LeftParen => {
                self.lexer.next_token()?; // consume the left paren
                let inner = self.parse_expression(0)?;
                self.expect_token(&TokenKind::RightParen)?;
                Ok(Expression::UnaryOp {
                    op: UnaryOperator::Complement, // This will need proper handling
                    expr: Box::new(inner),
                })
            }
            _ => {
                let consumed_token = self.lexer.next_token()?;
                Err(Error {
                    message: "Malformed factor".to_string(),
                    span: consumed_token.span,
                })
            }
        }
    }

    fn parse_unary_operator(&mut self) -> Result<UnaryOperator, Error> {
        let token = self.lexer.next_token()?;
        match token.kind {
            TokenKind::Tilde => Ok(UnaryOperator::Complement),
            TokenKind::Minus => Ok(UnaryOperator::Negate),
            TokenKind::Not => Ok(UnaryOperator::Not),
            _ => Err(Error {
                message: format!("Expected unary operator, found {:?}", token.kind),
                span: token.span,
            }),
        }
    }

    fn parse_binary_operator(&mut self) -> Result<BinaryOperator, Error> {
        let token = self.lexer.next_token()?;
        match token.kind {
            TokenKind::Plus => Ok(BinaryOperator::Add),
            TokenKind::Minus => Ok(BinaryOperator::Subtract),
            TokenKind::Asterisk => Ok(BinaryOperator::Multiply),
            TokenKind::Slash => Ok(BinaryOperator::Divide),
            TokenKind::Percent => Ok(BinaryOperator::Modulo),
            TokenKind::Lt => Ok(BinaryOperator::Lt),
            TokenKind::LtEq => Ok(BinaryOperator::LtEq),
            TokenKind::Gt => Ok(BinaryOperator::Gt),
            TokenKind::GtEq => Ok(BinaryOperator::GtEq),
            TokenKind::EqEq => Ok(BinaryOperator::EqEq),
            TokenKind::Neq => Ok(BinaryOperator::Neq),
            TokenKind::And => Ok(BinaryOperator::And),
            TokenKind::Or => Ok(BinaryOperator::Or),
            _ => Err(Error {
                message: format!("Expected binary operator, found {:?}", token.kind),
                span: token.span,
            }),
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
    BinaryExpression {
        left: Box<Expression>,
        op: BinaryOperator,
        right: Box<Expression>,
    },
}

#[derive(Debug)]
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Lt,
    LtEq,
    Gt,
    GtEq,
    EqEq,
    Neq,
    And,
    Or,
}
