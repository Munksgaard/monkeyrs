#[derive(Debug, PartialEq)]
pub struct Program(pub Vec<Statement>);

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct Identifier(pub String);

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
    Expression(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    IntegerLiteral(usize),
    Ident(Identifier),
    Negated(Box<Expression>),
    Not(Box<Expression>),
    Plus(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Equals(Box<Expression>, Box<Expression>),
    NotEquals(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    Boolean(bool),
    If(Box<Expression>, Vec<Statement>, Option<Vec<Statement>>),
    Lambda(Vec<Identifier>, Vec<Statement>),
    Call(Box<Expression>, Vec<Expression>),
}
