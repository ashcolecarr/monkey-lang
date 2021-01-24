use super::token::Token;
use std::any::Any;

pub trait Statement: StatementClone {
    fn token_literal(&self) -> String;
    fn to_string(&self) -> String;
    fn get_name(&self) -> Identifier;
    fn type_of(&self) -> &'static str;
    fn expression(&self) -> Option<Box<dyn Expression>>;
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
    fn token_literal(&self) -> String;
    fn value(&self) -> String;
    fn to_string(&self) -> String;
    fn type_of(&self) -> &'static str;
    fn as_any(&self) -> &dyn Any;
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
    pub fn new(token: Token, name: Identifier, value: Option<Box<dyn Expression>>) -> Self {
        Self { token, name, value }
    }
}

impl Statement for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut return_string = format!("{} {}", self.token_literal(), self.name.value);
        return_string += " = ";
        // Avoiding a borrow-checker error.
        return_string += self.value.clone()
            .map(|v| v.to_string()).unwrap_or_else(String::new).as_str();
        return_string += ";";

        return_string
    }

    fn get_name(&self) -> Identifier {
        self.name.clone()
    }

    fn type_of(&self) -> &'static str {
        "LetStatement"
    }

    fn expression(&self) -> Option<Box<dyn Expression>> {
        self.value.clone()
    }
}

#[derive(Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Option<Box<dyn Expression>>,
}

impl ReturnStatement {
    pub fn new(token: Token, return_value: Option<Box<dyn Expression>>) -> Self {
        Self { token, return_value }
    }
}

impl Statement for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut return_string = format!("{} ", self.token_literal());
        // As above.
        return_string += self.return_value.clone()
            .map(|v| v.to_string()).unwrap_or_else(String::new).as_str();
        return_string += ";";

        return_string
    }

    fn get_name(&self) -> Identifier {
        Identifier::new(self.token.clone(), String::new())
    }

    fn type_of(&self) -> &'static str {
        "ReturnStatement"
    }

    fn expression(&self) -> Option<Box<dyn Expression>> {
        self.return_value.clone()
    }
}

#[derive(Clone)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Box<dyn Expression>>,
}

impl ExpressionStatement {
    pub fn new(token: Token, expression: Option<Box<dyn Expression>>) -> Self {
        Self { token, expression }
    }
}

impl Statement for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        // As above.
        self.expression.clone()
            .map(|v| v.to_string()).unwrap_or_else(String::new)
    }

    fn get_name(&self) -> Identifier {
        Identifier::new(self.token.clone(), self.token_literal())
    }

    fn type_of(&self) -> &'static str {
        "ExpressionStatement"
    }

    fn expression(&self) -> Option<Box<dyn Expression>> {
        self.expression.clone()
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
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn value(&self) -> String {
        self.value.clone()
    }

    fn to_string(&self) -> String {
        self.value.clone()
    }

    fn type_of(&self) -> &'static str {
        "Identifier"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl IntegerLiteral {
    pub fn new(token: Token, value: i64) -> Self {
        Self { token, value }
    }
}

impl Expression for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn value(&self) -> String {
        self.value.to_string()
    }

    fn to_string(&self) -> String {
        self.token.literal.clone()
    }

    fn type_of(&self) -> &'static str {
        "IntegerLiteral"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<dyn Expression>
}

impl PrefixExpression {
    pub fn new(token: Token, operator: String, right: Box<dyn Expression>) -> Self {
        Self { token, operator, right }
    }
}

impl Expression for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn value(&self) -> String {
        String::new()
    }

    fn to_string(&self) -> String {
        format!("({}{})", &self.operator, &self.right.to_string())
    }

    fn type_of(&self) -> &'static str {
        "PrefixExpression"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<dyn Expression>,
    pub operator: String,
    pub right: Box<dyn Expression>
}

impl InfixExpression {
    pub fn new(token: Token, left: Box<dyn Expression>, operator: String, right: Box<dyn Expression>) -> Self {
        Self { token, left, operator, right }
    }
}

impl Expression for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn value(&self) -> String {
        String::new()
    }

    fn to_string(&self) -> String {
        format!("({} {} {})", &self.left.to_string(),
            &self.operator, &self.right.to_string())
    }

    fn type_of(&self) -> &'static str {
        "InfixExpression"
    }

    fn as_any(&self) -> &dyn Any {
        self
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

    pub fn to_string(&self) -> String {
        let mut return_string = String::new();

        for statement in &self.statements {
            return_string.push_str(statement.to_string().as_str());
        }

        return_string
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use super::super::token::TokenType;

    #[test]
    fn verify_to_string_returns_correct_value() {
        let mut program = Program::new();

        program.statements.push(
            Box::new(
                LetStatement::new(
                    Token::new(TokenType::Let, String::from("let")), 
                    Identifier::new(Token::new(TokenType::Ident, String::from("myVar")), String::from("myVar")), 
                    Some(Box::new(
                        Identifier::new(Token::new(TokenType::Ident, String::from("anotherVar")), String::from("anotherVar")))))
            ));
        
        assert_eq!(program.to_string(), String::from("let myVar = anotherVar;"));
    }
}