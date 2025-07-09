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
        let body = self.parse_block()?;

        Ok(FunctionDefinition {
            name,
            body,
        })
    }

    fn parse_declaration(&mut self) -> Result<Declaration, Error> {
        self.expect_token(&TokenKind::Int)?;
        let name = self.expect_identiier()?;
        let init = if self.lexer.peek_token()?.kind == TokenKind::Eq {
            self.lexer.next_token()?;
            Some(self.parse_expression(0)?)
        } else {
            None
        };
        self.expect_token(&TokenKind::Semicolon)?;

        Ok(Declaration { name, init })
    }

    fn parse_statement(&mut self) -> Result<Statement, Error> {
        let token = self.lexer.peek_token()?;
        match token.kind {
            TokenKind::Return => {
                self.lexer.next_token()?;
                let expr = self.parse_expression(0)?;
                self.expect_token(&TokenKind::Semicolon)?;
                Ok(Statement::Return(expr))
            }
            TokenKind::Semicolon => {
                self.lexer.next_token()?;
                Ok(Statement::Null)
            }
            TokenKind::If => {
                self.lexer.next_token()?;
                self.expect_token(&TokenKind::LeftParen)?;
                let condition = self.parse_expression(0)?;
                self.expect_token(&TokenKind::RightParen)?;
                let then_branch = Box::new(self.parse_statement()?);
                let else_branch = if self.lexer.peek_token()?.kind == TokenKind::Else {
                    self.lexer.next_token()?;
                    Some(Box::new(self.parse_statement()?))
                } else {
                    None
                };
                Ok(Statement::If {
                    condition,
                    then_branch,
                    else_branch,
                })
            }
            TokenKind::LeftBrace => Ok(Statement::Compound(self.parse_block()?)),
            _ => {
                let expr = self.parse_expression(0)?;
                self.expect_token(&TokenKind::Semicolon)?;
                Ok(Statement::Expression(expr))
            }
        }
    }

    fn parse_block(&mut self) -> Result<Block, Error> {
        self.expect_token(&TokenKind::LeftBrace)?;

        let mut decls = vec![];
        while let Ok(token) = self.lexer.peek_token() {
            if token.kind == TokenKind::RightBrace {
                break;
            }
            if token.kind == TokenKind::Int {
                let decl = self.parse_declaration()?;
                decls.push(decl);
            } else {
                break;
            }
        }

        let mut body = vec![];
        while let Ok(token) = self.lexer.peek_token() {
            if token.kind == TokenKind::RightBrace {
                break;
            }
            body.push(self.parse_statement()?);
        }

        self.expect_token(&TokenKind::RightBrace)?;

        Ok(Block {
            declarations: decls,
            statements: body,
        })
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
            (TokenKind::Question, 3),
            (TokenKind::Eq, 1),
        ]);

        while prec.contains_key(&next.kind) && prec.get(&next.kind).unwrap() >= &min_prec {
            if next.kind == TokenKind::Eq {
                self.lexer.next_token()?;
                let right = self.parse_expression(*prec.get(&next.kind).unwrap())?;
                left = Expression::Assignment {
                    left: Box::new(left),
                    right: Box::new(right),
                };
            } else if next.kind == TokenKind::Question {
                self.lexer.next_token()?;
                let true_expr = self.parse_expression(0)?;
                self.expect_token(&TokenKind::Colon)?;
                let false_expr = self.parse_expression(0)?;
                left = Expression::Ternary {
                    condition: Box::new(left),
                    true_expr: Box::new(true_expr),
                    false_expr: Box::new(false_expr),
                };
            } else {
                let op = self.parse_binary_operator()?;
                let right = self.parse_expression(prec.get(&next.kind).unwrap() + 1)?;
                left = Expression::BinaryExpression {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                };
            }
            next = self.lexer.peek_token()?;
        }
        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<Expression, Error> {
        let token = self.lexer.peek_token()?;
        match token.kind {
            TokenKind::IntConstant(value) => {
                self.lexer.next_token()?;
                Ok(Expression::IntConstant(value))
            }
            TokenKind::Tilde | TokenKind::Minus | TokenKind::Not | TokenKind::PlusPlus | TokenKind::MinusMinus => {
                let op = self.parse_unary_operator()?;
                let inner = self.parse_factor()?;
                Ok(Expression::UnaryOp {
                    op,
                    expr: Box::new(inner),
                })
            }
            TokenKind::LeftParen => {
                self.lexer.next_token()?;
                let inner = self.parse_expression(0)?;
                self.expect_token(&TokenKind::RightParen)?;
                Ok(inner)
            }
            TokenKind::Identifier(ref id) => {
                self.lexer.next_token()?;
                let mut expr = Expression::Identifier(id.clone());
                
                // Check for postfix increment/decrement
                let next_token = self.lexer.peek_token()?;
                match next_token.kind {
                    TokenKind::PlusPlus => {
                        self.lexer.next_token()?;
                        expr = Expression::UnaryOp {
                            op: UnaryOperator::PostIncrement,
                            expr: Box::new(expr),
                        };
                    }
                    TokenKind::MinusMinus => {
                        self.lexer.next_token()?;
                        expr = Expression::UnaryOp {
                            op: UnaryOperator::PostDecrement,
                            expr: Box::new(expr),
                        };
                    }
                    _ => {}
                }
                
                Ok(expr)
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
            TokenKind::PlusPlus => Ok(UnaryOperator::PreIncrement),
            TokenKind::MinusMinus => Ok(UnaryOperator::PreDecrement),
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
    pub body: Block,
}

#[derive(Debug)]
pub struct Declaration {
    pub name: String,
    pub init: Option<Expression>,
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    If {
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    Compound(Block),
    Null,
}

#[derive(Debug)]
pub struct Block {
    pub declarations: Vec<Declaration>,
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Expression {
    Identifier(String),
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
    Assignment {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Ternary {
        condition: Box<Expression>,
        true_expr: Box<Expression>,
        false_expr: Box<Expression>,
    },
}

#[derive(Debug)]
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
    PreIncrement,
    PreDecrement,
    PostIncrement,
    PostDecrement,
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
