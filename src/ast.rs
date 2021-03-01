use super::token::Token;
use std::fmt::{Display, Formatter, Result};

pub enum Node {
    Statement(Statement),
    Expression(Expression),
    Program(Program),
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", match self {
            Node::Statement(ls) => format!("{}", ls),
            Node::Expression(ex) => format!("{}", ex),
            Node::Program(p) => format!("{}", p),
        })
    }
}

impl Node {
    pub fn token_literal(&self) -> &str {
        match self {
            Node::Statement(s) => s.token_literal(),
            Node::Expression(e) => e.token_literal(),
            Node::Program(p) => p.token_literal(),
        }
    }
}

pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", match self {
            Statement::LetStatement(ls) => format!("{}", ls),
            Statement::ReturnStatement(rs) => format!("{}", rs),
            Statement::ExpressionStatement(es) => format!("{}", es),
        })
    }
}

impl Statement {
    pub fn token_literal(&self) -> &str {
        match self {
            Statement::LetStatement(ls) => ls.token.literal.as_str(),
            Statement::ReturnStatement(rs) => rs.token.literal.as_str(),
            Statement::ExpressionStatement(es) => es.token.literal.as_str(),
        }
    }
}

pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Expression>,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let output = format!("{} {} = {};", self.token.literal,
            self.name, match &self.value {
                Some(v) => format!("{}", v),
                None => String::new(),
            });
        write!(f, "{}", output)
    }
}

impl LetStatement {
    pub fn new(token: Token, name: Identifier, value: Option<Expression>) -> Self {
        Self { token, name, value }
    }
}

pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Expression>,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let output = format!("{} {};", self.token.literal,
            match &self.return_value {
                Some(rv) => format!("{}", rv),
                None => String::new(),
            });
        write!(f, "{}", output)
    }
}

impl ReturnStatement {
    pub fn new(token: Token, return_value: Option<Expression>) -> Self {
        Self { token, return_value }
    }
}

pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Expression>,
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let output = format!("{}", match &self.expression {
            Some(ex) => format!("{}", ex),
            None => String::new(),
        });
        write!(f, "{}", output)
    }
}

impl ExpressionStatement {
    pub fn new(token: Token, expression: Option<Expression>) -> Self {
        Self { token, expression }
    }
}

#[derive(Clone)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    Boolean(Boolean),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", match self {
            Expression::Identifier(id) => format!("{}", id),
            Expression::IntegerLiteral(il) => format!("{}", il),
            Expression::PrefixExpression(pe) => format!("{}", pe),
            Expression::InfixExpression(ie) => format!("{}", ie),
            Expression::Boolean(bl) => format!("{}", bl),
        })
    }
}

impl Expression {
    pub fn token_literal(&self) -> &str {
        match self {
            Expression::Identifier(id) => id.token.literal.as_str(),
            Expression::IntegerLiteral(il) => il.token.literal.as_str(),
            Expression::PrefixExpression(pe) => pe.token.literal.as_str(),
            Expression::InfixExpression(ie) => ie.token.literal.as_str(),
            Expression::Boolean(bl) => bl.token.literal.as_str(),
        }
    }
}

#[derive(Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.value)
    }
}

impl Identifier {
    pub fn new(token: Token, value: &str) -> Self {
        Self { token, value: String::from(value) }
    }
}

#[derive(Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.token.literal)
    }
}

impl IntegerLiteral {
    pub fn new(token: Token, value: i64) -> Self {
        Self { token, value }
    }
}

#[derive(Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

impl PrefixExpression {
    pub fn new(token: Token, operator: &str, right: Box<Expression>) -> Self {
        Self { token, operator: String::from(operator), right }
    }
}

#[derive(Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

impl InfixExpression {
    pub fn new(token: Token, left: Box<Expression>, operator: &str, right: Box<Expression>) -> Self {
        Self { token, left, operator: String::from(operator), right }
    }
}

#[derive(Clone)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Display for Boolean {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.token.literal)
    }
}

impl Boolean {
    pub fn new(token: Token, value: bool) -> Self {
        Boolean { token, value }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let mut output = String::new();
        for statement in &self.statements {
            output += format!("{}", statement).as_str();
        }

        write!(f, "{}", output)
    }
}

impl Program {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self { statements }
    }

    pub fn token_literal(&self) -> &str {
        if !self.statements.is_empty() {
            self.statements[0].token_literal()
        } else {
            ""
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use super::super::token::TokenType;

    #[test]
    fn test_string() {
        let mut statements = vec![];
        
        statements.push(Statement::LetStatement(LetStatement::new(Token::new(
                TokenType::Let, String::from("let")), 
                Identifier::new(Token::new(TokenType::Ident, String::from("myVar")), "myVar"), 
                Some(Expression::Identifier(Identifier::new(Token::new(TokenType::Ident, String::from("anotherVar")), "anotherVar"))))));
                
        let program = Program::new(statements);
        assert_eq!(format!("{}", program), String::from("let myVar = anotherVar;"));
    }
}