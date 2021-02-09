use super::token::Token;
use std::any::Any;

pub trait Node: NodeClone + BaseTrait {
    fn token_literal(&self) -> String;
    fn to_string(&self) -> String;
    fn type_of(&self) -> &'static str;
    fn as_any(&self) -> &dyn Any;
}

pub trait NodeClone {
    fn clone_box(&self) -> Box<dyn Node>;
}

impl <T: 'static + Node + Clone> NodeClone for T {
    fn clone_box(&self) -> Box<dyn Node> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Node> {
    fn clone(&self) -> Box<dyn Node> {
        self.clone_box()
    }
}

pub trait BaseTrait {
    fn as_base(&self) -> &dyn Node;
}

impl<T: Node> BaseTrait for T {
    fn as_base(&self) -> &dyn Node {
        self
    }
}

pub trait Statement: Node + StatementClone {
    fn expression(&self) -> Option<Box<dyn Expression>>;
}

pub trait StatementClone {
    fn clone_stmt_box(&self) -> Box<dyn Statement>;
}

impl <T: 'static + Statement + Clone> StatementClone for T {
    fn clone_stmt_box(&self) -> Box<dyn Statement> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Statement> {
    fn clone(&self) -> Box<dyn Statement> {
        self.clone_stmt_box()
    }
}

pub trait Expression: Node + ExpressionClone {
    fn value(&self) -> String;
}

pub trait ExpressionClone {
    fn clone_exp_box(&self) -> Box<dyn Expression>;
}

impl <T: 'static + Expression + Clone> ExpressionClone for T {
    fn clone_exp_box(&self) -> Box<dyn Expression> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Expression> {
    fn clone(&self) -> Box<dyn Expression> {
        self.clone_exp_box()
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

impl Node for LetStatement {
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

    fn type_of(&self) -> &'static str {
        "LetStatement"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for LetStatement {
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

impl Node for ReturnStatement {
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

    fn type_of(&self) -> &'static str {
        "ReturnStatement"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for ReturnStatement {
    fn expression(&self) -> Option<Box<dyn Expression>> {
        self.return_value.clone()
    }
}

#[derive(Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Box<dyn Statement>>,
}

impl BlockStatement {
    pub fn new(token: Token, statements: Vec<Box<dyn Statement>>) -> Self {
        Self { token, statements }
    }
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let mut return_string = String::new();
        for statement in &self.statements {
            return_string += &statement.to_string();
        }

        return_string
    }

    fn type_of(&self) -> &'static str {
        "BlockStatement"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for BlockStatement {
    fn expression(&self) -> Option<Box<dyn Expression>> {
        None
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

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        // As above.
        self.expression.clone()
            .map(|v| v.to_string()).unwrap_or_else(String::new)
    }

    fn type_of(&self) -> &'static str {
        "ExpressionStatement"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Statement for ExpressionStatement {
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

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
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

impl Expression for Identifier {
    fn value(&self) -> String {
        self.value.clone()
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

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
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

impl Expression for IntegerLiteral {
    fn value(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Clone)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl BooleanLiteral {
    pub fn new(token: Token, value: bool) -> Self {
        BooleanLiteral { token, value }
    }
}

impl Node for BooleanLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        self.token.literal.clone()
    }

    fn type_of(&self) -> &'static str {
        "Boolean"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for BooleanLiteral {
    fn value(&self) -> String {
        self.value.to_string()
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

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
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

impl Expression for PrefixExpression {
    fn value(&self) -> String {
        String::new()
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

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
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

impl Expression for InfixExpression {
    fn value(&self) -> String {
        String::new()
    }
}

#[derive(Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<dyn Expression>,
    pub consequence: Box<dyn Statement>,
    pub alternative: Option<Box<dyn Statement>>,
}

impl IfExpression {
    pub fn new(token: Token, condition: Box<dyn Expression>, consequence: Box<dyn Statement>, alternative: Option<Box<dyn Statement>>) -> Self {
        Self { token, condition, consequence, alternative }
    }
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        format!("(if {} {} {})", &self.condition.to_string(),
            &self.consequence.to_string(), match &self.alternative {
                Some(alt) => String::from("else ") + &alt.to_string(),
                None => String::new(),
            })
    }

    fn type_of(&self) -> &'static str {
        "IfExpression"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for IfExpression {
    fn value(&self) -> String {
        String::new()
    }
}

#[derive(Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Box<dyn Expression>>,
    pub body: Box<dyn Statement>,
}

impl FunctionLiteral {
    pub fn new(token: Token, parameters: Vec<Box<dyn Expression>>, body: Box<dyn Statement>) -> Self {
        Self { token, parameters, body }
    }
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let params: Vec<String> = self.parameters.iter().map(|p| p.to_string()).collect();
        format!("({}) {}", params.join(", "), self.body.to_string())
    }

    fn type_of(&self) -> &'static str {
        "FunctionLiteral"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for FunctionLiteral {
    fn value(&self) -> String {
        String::new()
    }
}

#[derive(Clone)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<dyn Expression>,
    pub arguments: Vec<Box<dyn Expression>>,
}

impl CallExpression {
    pub fn new(token: Token, function: Box<dyn Expression>, arguments: Vec<Box<dyn Expression>>) -> Self {
        Self { token, function, arguments }
    }
}

impl Node for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn to_string(&self) -> String {
        let args: Vec<String> = self.arguments.iter().map(|a| a.to_string()).collect();
        format!("{}({})", self.function.to_string(), args.join(", "))
    }

    fn type_of(&self) -> &'static str {
        "CallExpression"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Expression for CallExpression {
    fn value(&self) -> String {
        String::new()
    }
}

#[derive(Clone)]
pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            String::new()
        }
    }

    fn to_string(&self) -> String {
        let mut return_string = String::new();

        for statement in &self.statements {
            return_string.push_str(statement.to_string().as_str());
        }

        return_string
    }

    fn type_of(&self) -> &'static str {
        "Program"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Program {
    pub fn new() -> Self {
        Self { statements: vec![] }
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