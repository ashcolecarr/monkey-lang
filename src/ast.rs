use super::token::Token;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result};
use std::hash::{Hash, Hasher};

pub enum Node {
    Statement(Statement),
    Expression(Expression),
    Program(Program),
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", match self {
            Self::Statement(ls) => format!("{}", ls),
            Self::Expression(ex) => format!("{}", ex),
            Self::Program(p) => format!("{}", p),
        })
    }
}

impl Node {
    pub fn token_literal(&self) -> &str {
        match self {
            Self::Statement(s) => s.token_literal(),
            Self::Expression(e) => e.token_literal(),
            Self::Program(p) => p.token_literal(),
        }
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", match self {
            Self::LetStatement(ls) => format!("{}", ls),
            Self::ReturnStatement(rs) => format!("{}", rs),
            Self::ExpressionStatement(es) => format!("{}", es),
            Self::BlockStatement(bs) => format!("{}", bs),
        })
    }
}

impl Statement {
    pub fn token_literal(&self) -> &str {
        match self {
            Self::LetStatement(ls) => ls.token.literal.as_str(),
            Self::ReturnStatement(rs) => rs.token.literal.as_str(),
            Self::ExpressionStatement(es) => es.token.literal.as_str(),
            Self::BlockStatement(bs) => bs.token.literal.as_str(),
        }
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
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

#[derive(Clone, Eq, Hash, PartialEq)]
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

#[derive(Clone, Eq, Hash, PartialEq)]
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

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let mut output = String::new();
        for statement in &self.statements {
            output += format!("{}", statement).as_str();
        }

        write!(f, "{}", output)
    }
}

impl BlockStatement {
    pub fn new(token: Token, statements: Vec<Statement>) -> Self {
        Self { token, statements }
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    BooleanLiteral(BooleanLiteral),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
    StringLiteral(StringLiteral),
    ArrayLiteral(ArrayLiteral),
    IndexExpression(IndexExpression),
    HashLiteral(HashLiteral),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", match self {
            Self::Identifier(id) => format!("{}", id),
            Self::IntegerLiteral(il) => format!("{}", il),
            Self::PrefixExpression(pe) => format!("{}", pe),
            Self::InfixExpression(ie) => format!("{}", ie),
            Self::BooleanLiteral(bl) => format!("{}", bl),
            Self::IfExpression(ie) => format!("{}", ie),
            Self::FunctionLiteral(fl) => format!("{}", fl),
            Self::CallExpression(ce) => format!("{}", ce),
            Self::StringLiteral(sl) => format!("{}", sl),
            Self::ArrayLiteral(al) => format!("{}", al),
            Self::IndexExpression(ie) => format!("{}", ie),
            Self::HashLiteral(hl) => format!("{}", hl),
        })
    }
}

impl Expression {
    pub fn token_literal(&self) -> &str {
        match self {
            Self::Identifier(id) => id.token.literal.as_str(),
            Self::IntegerLiteral(il) => il.token.literal.as_str(),
            Self::PrefixExpression(pe) => pe.token.literal.as_str(),
            Self::InfixExpression(ie) => ie.token.literal.as_str(),
            Self::BooleanLiteral(bl) => bl.token.literal.as_str(),
            Self::IfExpression(ie) => ie.token.literal.as_str(),
            Self::FunctionLiteral(fl) => fl.token.literal.as_str(),
            Self::CallExpression(ce) => ce.token.literal.as_str(),
            Self::StringLiteral(sl) => sl.token.literal.as_str(),
            Self::ArrayLiteral(al) => al.token.literal.as_str(),
            Self::IndexExpression(ie) => ie.token.literal.as_str(),
            Self::HashLiteral(hl) => hl.token.literal.as_str(),
        }
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
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

#[derive(Clone, Eq, Hash, PartialEq)]
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

#[derive(Clone, Eq, Hash, PartialEq)]
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

#[derive(Clone, Eq, Hash, PartialEq)]
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

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl Display for BooleanLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.token.literal)
    }
}

impl BooleanLiteral {
    pub fn new(token: Token, value: bool) -> Self {
        BooleanLiteral { token, value }
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "if {} {} {}", self.condition, self.consequence, match &self.alternative {
            Some(alt) => format!("else {}", alt),
            None => String::new(),
        })
    }
}

impl IfExpression {
    pub fn new(token: Token, condition: Box<Expression>, consequence: BlockStatement, alternative: Option<BlockStatement>) -> Self {
        Self { token, condition, consequence, alternative }
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub name: String,
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let params: Vec<String> = self.parameters.iter().map(|p| format!("{}", p)).collect();
        let display_name = if !self.name.is_empty() { format!("<{}>", self.name) } else { String::new() };

        write!(f, "{}{}({}) {}", self.token.literal, display_name, params.join(", "), self.body)
    }
}

impl FunctionLiteral {
    pub fn new(token: Token, parameters: Vec<Identifier>, body: BlockStatement, name: &str) -> Self {
        Self { token, parameters, body, name: String::from(name) }
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let args: Vec<String> = self.arguments.iter().map(|p| format!("{}", p)).collect();

        write!(f, "{}({})", self.function, args.join(", "))
    }
}

impl CallExpression {
    pub fn new(token: Token, function: Box<Expression>, arguments: Vec<Expression>) -> Self {
        Self { token, function, arguments }
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl Display for StringLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}", self.token.literal)
    }
}

impl StringLiteral {
    pub fn new(token: Token, value: String) -> Self {
        Self { token, value }
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<Expression>,
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let elems: Vec<String> = self.elements.iter().map(|p| format!("{}", p)).collect();

        write!(f, "[{}]", elems.join(", "))
    }
}

impl ArrayLiteral {
    pub fn new(token: Token, elements: Vec<Expression>) -> Self {
        Self { token, elements }
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct IndexExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

impl Display for IndexExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}

impl IndexExpression {
    pub fn new(token: Token, left: Box<Expression>, index: Box<Expression>) -> Self {
        Self { token, left, index }
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct HashLiteral {
    pub token: Token,
    pub pairs: HashMap<Expression, Expression>,
}

impl Display for HashLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let mut pairs = vec![];
        for (key, value) in &self.pairs {
            pairs.push(format!("{}:{}", key, value));
        }

        write!(f, "{{{}}}", pairs.join(", "))
    }
}

// Rust requires that Hash be implemented here, but it is not a valid key for hashing.
impl Hash for HashLiteral {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("HashLiteral is not valid for hashing.");
    }
}

impl HashLiteral {
    pub fn new(token: Token, pairs: HashMap<Expression, Expression>) -> Self {
        Self { token, pairs }
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