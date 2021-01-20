use super::token::Token;

pub struct Node {
    pub token_literal: String,
}

impl Node {
    pub fn new() -> Self {
        Self { token_literal: String::new() }
    }
}

pub trait Statement: StatementClone {
    fn statement_node(&self);
    fn token_literal(&self) -> String;
    fn get_name(&self) -> Identifier;
    fn type_of(&self) -> &'static str;
}

pub trait StatementClone {
    fn clone_box(&self) -> Box<dyn Statement>;
}

impl <T: 'static + Statement + Clone> StatementClone for T {
    fn clone_box(&self) -> Box<dyn Statement> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Statement> {
    fn clone(&self) -> Box<dyn Statement> {
        self.clone_box()
    }
}

pub trait Expression: ExpressionClone {
    fn expression_node(&self);
    fn token_literal(&self) -> String;
    fn type_of(&self) -> &'static str;
}

pub trait ExpressionClone {
    fn clone_box(&self) -> Box<dyn Expression>;
}

impl <T: 'static + Expression + Clone> ExpressionClone for T {
    fn clone_box(&self) -> Box<dyn Expression> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Expression> {
    fn clone(&self) -> Box<dyn Expression> {
        self.clone_box()
    }
}

#[derive(Clone)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Option<Box<dyn Expression>>,
}

impl LetStatement {
    pub fn new(token: Token, name: Identifier) -> Self {//}, value: Box<dyn Expression>) -> Self {
        Self { token, name, value: None }
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) { }

    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn get_name(&self) -> Identifier {
        self.name.clone()
    }

    fn type_of(&self) -> &'static str {
        "LetStatement"
    }
}

#[derive(Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Box<dyn Expression>>,
}

impl ReturnStatement {
    pub fn new(token: Token) -> Self {//}, value: Box<dyn Expression>) -> Self {
        Self { token, return_value: None }
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) { }

    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn get_name(&self) -> Identifier {
        Identifier::new(self.token.clone(), String::new())
    }

    fn type_of(&self) -> &'static str {
        "ReturnStatement"
    }
}

#[derive(Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn new(token: Token, value: String) -> Self {
        Self { token, value }
    }
}

impl Expression for Identifier {
    fn expression_node(&self) { }

    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn type_of(&self) -> &'static str {
        "Identifier"
    }
}

#[derive(Clone)]
pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Program {
    pub fn new() -> Self {
        Self { statements: vec![] }
    }

    pub fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            String::new()
        }
    }
}