#![allow(unused_imports, dead_code, unused_variables)]

use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;
use std::iter::Peekable;

#[derive(Debug, PartialEq, Clone, Copy, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

fn precedence(token: &Token) -> Precedence {
    use Precedence::*;
    match token {
        Token::EQ => Equals,
        Token::NOTEQ => Equals,
        Token::LT => LessGreater,
        Token::GT => LessGreater,
        Token::PLUS => Sum,
        Token::MINUS => Sum,
        Token::SLASH => Product,
        Token::ASTERISK => Product,
        Token::LPAREN => Call,
        _ => Lowest,
    }
}

fn peek_precedence(tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>) -> Precedence {
    tokens
        .peek()
        .map(|x: &&Token| precedence(*x))
        .unwrap_or(Precedence::Lowest)
}

fn parse_infix_expression(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    left: Expression,
) -> Option<Expression> {
    match tokens.next() {
        Some(tok @ Token::PLUS) => {
            if let Some(right) = parse_expression(tokens, precedence(tok)) {
                Some(Expression::Plus(Box::new(left), Box::new(right)))
            } else {
                panic!("Couldn't parse infix expression 1")
            }
        }
        Some(tok @ Token::MINUS) => {
            if let Some(right) = parse_expression(tokens, precedence(tok)) {
                Some(Expression::Minus(Box::new(left), Box::new(right)))
            } else {
                panic!("Couldn't parse infix expression 2")
            }
        }
        Some(tok @ Token::SLASH) => {
            if let Some(right) = parse_expression(tokens, precedence(tok)) {
                Some(Expression::Divide(Box::new(left), Box::new(right)))
            } else {
                panic!("Couldn't parse infix expression 3")
            }
        }
        Some(tok @ Token::ASTERISK) => {
            if let Some(right) = parse_expression(tokens, precedence(tok)) {
                Some(Expression::Multiply(Box::new(left), Box::new(right)))
            } else {
                panic!("Couldn't parse infix expression 4")
            }
        }
        Some(tok @ Token::EQ) => {
            if let Some(right) = parse_expression(tokens, precedence(tok)) {
                Some(Expression::Equals(Box::new(left), Box::new(right)))
            } else {
                panic!("Couldn't parse infix expression 5")
            }
        }
        Some(tok @ Token::NOTEQ) => {
            if let Some(right) = parse_expression(tokens, precedence(tok)) {
                Some(Expression::NotEquals(Box::new(left), Box::new(right)))
            } else {
                panic!("Couldn't parse infix expression 5")
            }
        }
        Some(tok @ Token::LT) => {
            if let Some(right) = parse_expression(tokens, precedence(tok)) {
                Some(Expression::LessThan(Box::new(left), Box::new(right)))
            } else {
                panic!("Couldn't parse infix expression 6")
            }
        }
        Some(tok @ Token::GT) => {
            if let Some(right) = parse_expression(tokens, precedence(tok)) {
                Some(Expression::GreaterThan(Box::new(left), Box::new(right)))
            } else {
                panic!("Couldn't parse infix expression 7")
            }
        }
        Some(tok @ Token::LPAREN) => {
            let right = parse_call_arguments(tokens);
            Some(Expression::Call(Box::new(left), right))
        }
        Some(_) => None,
        None => None,
    }
}

fn parse_call_arguments(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
) -> Vec<Expression> {
    let mut args = Vec::new();

    loop {
        match tokens.peek() {
            Some(Token::RPAREN) => {
                tokens.next();
                break;
            }
            Some(_) => args.push(
                parse_expression(tokens, Precedence::Lowest)
                    .expect("Function argument should be an expression"),
            ),
            None => panic!("Expected identifier, got end of file"),
        }

        if let Some(Token::COMMA) = tokens.peek() {
            tokens.next();
        }
    }

    args
}

fn parse_block_statements(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
) -> Vec<Statement> {
    let mut statements = Vec::new();

    loop {
        if let Some(Token::RBRACE) = tokens.peek() {
            tokens.next();
            break;
        } else if let Some(statement) = parse_statement(tokens) {
            statements.push(statement);
        } else {
            panic!("Expected statement");
        }
    }

    statements
}

fn parse_function_parameters(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
) -> Vec<Identifier> {
    let mut identifiers = Vec::new();

    loop {
        match tokens.next() {
            Some(Token::RPAREN) => break,
            Some(Token::IDENT(ident)) => identifiers.push(Identifier(ident.to_string())),
            Some(e) => panic!("Expected identifier, got {:?}", e),
            None => panic!("Expected identifier, got end of file"),
        }

        if let Some(Token::COMMA) = tokens.peek() {
            tokens.next();
        }
    }

    identifiers
}

fn parse_expression(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    precedence: Precedence,
) -> Option<Expression> {
    let mut left_exp = match tokens.next() {
        Some(Token::IDENT(ident)) => Expression::Ident(ident.to_string()),
        Some(Token::INT(n)) => Expression::IntegerLiteral(*n),
        Some(Token::BANG) => {
            if let Some(expr) = parse_expression(tokens, Precedence::Prefix) {
                Expression::Not(Box::new(expr))
            } else {
                panic!("Error parsing not expression")
            }
        }
        Some(Token::MINUS) => {
            if let Some(expr) = parse_expression(tokens, Precedence::Prefix) {
                Expression::Negated(Box::new(expr))
            } else {
                panic!("Error parsing not expression")
            }
        }
        Some(Token::LPAREN) => {
            let expr = if let Some(expr) = parse_expression(tokens, Precedence::Lowest) {
                expr
            } else {
                panic!("Error parsing grouped expression")
            };

            match tokens.next() {
                Some(Token::RPAREN) => expr,
                x => panic!("Expected RPAREN, got {:?}, parsed: {:?}", x, expr),
            }
        }
        Some(Token::TRUE) => Expression::Boolean(true),
        Some(Token::FALSE) => Expression::Boolean(false),
        Some(Token::IF) => {
            if let Some(Token::LPAREN) = tokens.next() {
                let condition = parse_expression(tokens, Precedence::Lowest).unwrap();

                if let Some(Token::RPAREN) = tokens.next() {
                } else {
                    panic!("Expected right paren")
                }

                if let Some(Token::LBRACE) = tokens.next() {
                } else {
                    panic!("Expected left brace")
                }

                let consequence = parse_block_statements(tokens);

                let alternative = if let Some(Token::ELSE) = tokens.next() {
                    if let Some(Token::LBRACE) = tokens.next() {
                    } else {
                        panic!("Expected left brace")
                    }

                    Some(parse_block_statements(tokens))
                } else {
                    None
                };

                Expression::If(Box::new(condition), consequence, alternative)
            } else {
                panic!("unimplemented")
            }
        }
        Some(Token::FUNCTION) => {
            if let Some(Token::LPAREN) = tokens.next() {
            } else {
                panic!("Expected left paren");
            }

            let parameters = parse_function_parameters(tokens);

            if let Some(Token::LBRACE) = tokens.next() {
            } else {
                panic!("Expected left brace");
            }

            let body = parse_block_statements(tokens);

            Expression::Lambda(parameters, body)
        }
        x => panic!("parse_expression: {:?}", x),
    };

    while tokens.peek() != Some(&&Token::SEMICOLON) && precedence < peek_precedence(tokens) {
        if let Some(new_exp) = parse_infix_expression(tokens, left_exp.clone()) {
            left_exp = new_exp;
        } else {
            break;
        }
    }

    Some(left_exp)
}

fn parse_let_statement(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
) -> Option<Statement> {
    match (tokens.next(), tokens.next()) {
        (Some(Token::IDENT(ident)), Some(Token::ASSIGN)) => {
            if let Some(expr) = parse_expression(tokens, Precedence::Lowest) {
                Some(Statement::Let(Identifier(ident.to_string()), expr))
            } else {
                panic!("Invalid expression in let statement")
            }
        }
        _ => panic!("Invalid let statement"),
    }
}

fn parse_return_statement(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
) -> Option<Statement> {
    if let Some(expr) = parse_expression(tokens, Precedence::Lowest) {
        Some(Statement::Return(expr))
    } else {
        panic!("Invalid expression in return statement")
    }
}

fn parse_expression_statement(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
) -> Option<Statement> {
    if let Some(expr) = parse_expression(tokens, Precedence::Lowest) {
        Some(Statement::Expression(expr))
    } else {
        panic!("Invalid expression statement")
    }
}
fn parse_statement(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
) -> Option<Statement> {
    let stmt = match tokens.peek() {
        Some(Token::LET) => {
            tokens.next();
            parse_let_statement(tokens)
        }
        Some(Token::RETURN) => {
            tokens.next();
            parse_return_statement(tokens)
        }
        Some(x) => parse_expression_statement(tokens),
        None => None,
    };

    if let Some(Token::SEMICOLON) = tokens.peek() {
        tokens.next();
    }

    stmt
}

pub fn parse(tokens: Vec<Token>) -> Program {
    let mut peekable: std::iter::Peekable<std::slice::Iter<'_, Token>> = tokens.iter().peekable();

    let mut statements = Vec::new();

    while let Some(statement) = parse_statement(&mut peekable) {
        statements.push(statement);
    }
    Program(statements)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_let_statements() {
        let input = r#"let x = 5;
let y = 10;
let foobar = 838383;
"#;
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect();
        let result = parse(tokens);

        let expected = Program(vec![
            Statement::Let(Identifier("x".to_string()), Expression::IntegerLiteral(5)),
            Statement::Let(Identifier("y".to_string()), Expression::IntegerLiteral(10)),
            Statement::Let(
                Identifier("foobar".to_string()),
                Expression::IntegerLiteral(838383),
            ),
        ]);

        assert_eq!(expected, result);

        let input = r#"let x = 5;
let y = true;
let foobar = y;
"#;
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect();
        let result = parse(tokens);

        let expected = Program(vec![
            Statement::Let(Identifier("x".to_string()), Expression::IntegerLiteral(5)),
            Statement::Let(Identifier("y".to_string()), Expression::Boolean(true)),
            Statement::Let(
                Identifier("foobar".to_string()),
                Expression::Ident("y".to_string()),
            ),
        ]);

        assert_eq!(expected, result);
    }

    #[test]
    fn test_return_statements() {
        let input = r#"return 5;
return 10;
return 993322;
"#;
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect();
        let result = parse(tokens);

        let expected = Program(vec![
            Statement::Return(Expression::IntegerLiteral(5)),
            Statement::Return(Expression::IntegerLiteral(10)),
            Statement::Return(Expression::IntegerLiteral(993322)),
        ]);

        assert_eq!(expected, result);
    }

    #[test]
    fn test_identifer_expression() {
        let input = r#"foobar;"#;
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect();
        let result = parse(tokens);

        let expected = Program(vec![Statement::Expression(Expression::Ident(
            "foobar".to_string(),
        ))]);

        assert_eq!(expected, result);
    }

    #[test]
    fn test_literal_integer_expression() {
        let input = r#"5;"#;
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect();
        let result = parse(tokens);

        let expected = Program(vec![Statement::Expression(Expression::IntegerLiteral(5))]);

        assert_eq!(expected, result);
    }

    #[test]
    fn test_precedence() {
        assert!(
            Precedence::Lowest < Precedence::Call,
            "`Lowest` must have lower precedence than function calls"
        );
    }

    #[test]
    fn test_not_expression() {
        let input = r#"!5;"#;
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect();
        let result = parse(tokens);

        let expected = Program(vec![Statement::Expression(Expression::Not(Box::new(
            Expression::IntegerLiteral(5),
        )))]);

        assert_eq!(expected, result);
    }

    #[test]
    fn test_negated_expression() {
        let input = r#"-5;"#;
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect();
        let result = parse(tokens);

        let expected = Program(vec![Statement::Expression(Expression::Negated(Box::new(
            Expression::IntegerLiteral(5),
        )))]);

        assert_eq!(expected, result);
    }

    #[test]
    fn test_infix_expression() {
        use Expression::*;

        let input = r#"5 + 5;
5 - 5;
5 * 5;
5 / 5;
5 > 5;
5 < 5;
5 == 5;
5 != 5;
true == true;
true != false;
false == false;
"#;
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect();
        let result = parse(tokens);

        let expected = Program(vec![
            Statement::Expression(Plus(
                Box::new(IntegerLiteral(5)),
                Box::new(IntegerLiteral(5)),
            )),
            Statement::Expression(Minus(
                Box::new(IntegerLiteral(5)),
                Box::new(IntegerLiteral(5)),
            )),
            Statement::Expression(Multiply(
                Box::new(IntegerLiteral(5)),
                Box::new(IntegerLiteral(5)),
            )),
            Statement::Expression(Divide(
                Box::new(IntegerLiteral(5)),
                Box::new(IntegerLiteral(5)),
            )),
            Statement::Expression(GreaterThan(
                Box::new(IntegerLiteral(5)),
                Box::new(IntegerLiteral(5)),
            )),
            Statement::Expression(LessThan(
                Box::new(IntegerLiteral(5)),
                Box::new(IntegerLiteral(5)),
            )),
            Statement::Expression(Equals(
                Box::new(IntegerLiteral(5)),
                Box::new(IntegerLiteral(5)),
            )),
            Statement::Expression(NotEquals(
                Box::new(IntegerLiteral(5)),
                Box::new(IntegerLiteral(5)),
            )),
            Statement::Expression(Equals(Box::new(Boolean(true)), Box::new(Boolean(true)))),
            Statement::Expression(NotEquals(Box::new(Boolean(true)), Box::new(Boolean(false)))),
            Statement::Expression(Equals(Box::new(Boolean(false)), Box::new(Boolean(false)))),
        ]);

        assert_eq!(expected, result);
    }

    #[test]
    fn test_operator_precedence_expressions() {
        use Expression::*;

        fn test_it(input1: &str, input2: &str) {
            let lexer = Lexer::new(input1);
            let tokens: Vec<_> = lexer.collect();
            let result1 = parse(tokens);

            let lexer = Lexer::new(input2);
            let tokens: Vec<_> = lexer.collect();
            let result2 = parse(tokens);

            assert_eq!(result1, result2);
        }

        test_it("-a * b", "((-a) * b)");
        test_it("!-a", "(!(-a))");
        test_it("a + b + c", "((a + b) + c)");
        test_it("a + b - c", "((a + b) - c)");
        test_it("a * b * c", "((a * b) * c)");
        test_it("a * b / c", "((a * b) / c)");
        test_it("a + b / c", "(a + (b / c))");
        test_it("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)");
        test_it("3 + 4; -5 * 5", "(3 + 4); ((-5) * 5)");
        test_it("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))");
        test_it("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))");
        test_it(
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        );
        test_it("true", "true");
        test_it("false", "false");
        test_it("3 > 5 == false", "((3 > 5) == false)");
        test_it("3 < 5 == true", "((3 < 5) == true)");
        test_it("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)");
        test_it("(5 + 5) * 2", "((5 + 5) * 2)");
        test_it("2 / (5 + 5)", "(2 / (5 + 5))");
        test_it("-(5 + 5)", "(-(5 + 5))");
        test_it("!(true == true)", "(!(true == true))");

        test_it("a + add(b * c) + d", "((a + add((b * c))) + d)");
        test_it(
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        );
        test_it(
            "add(a + b + c * d / f + g)",
            "add((((a + b) + ((c * d) / f)) + g))",
        );
    }

    #[test]
    fn test_multiply_plus() {
        use Expression::*;
        let lexer = Lexer::new("a + b * c");
        let tokens: Vec<_> = lexer.collect();
        let result = parse(tokens);

        let expected = Program(vec![Statement::Expression(Plus(
            Box::new(Ident("a".to_string())),
            Box::new(Multiply(
                Box::new(Ident("b".to_string())),
                Box::new(Ident("c".to_string())),
            )),
        ))]);

        assert_eq!(result, expected);
    }

    #[test]
    fn test_boolean_expression() {
        use Expression::*;

        let input = r#"true;
false;
let foobar = true;
let barfoo = false;
"#;
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect();
        let result = parse(tokens);

        let expected = Program(vec![
            Statement::Expression(Boolean(true)),
            Statement::Expression(Boolean(false)),
            Statement::Let(Identifier("foobar".to_string()), Boolean(true)),
            Statement::Let(Identifier("barfoo".to_string()), Boolean(false)),
        ]);

        assert_eq!(expected, result);
    }

    #[test]
    fn test_if_expression() {
        use Expression::*;

        let input = r#"if (x < y) { x }
"#;
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect();
        let result = parse(tokens);

        let expected = Program(vec![Statement::Expression(If(
            Box::new(LessThan(
                Box::new(Ident("x".to_string())),
                Box::new(Ident("y".to_string())),
            )),
            vec![(Box::new(Statement::Expression(Ident("x".to_string()))))],
            None,
        ))]);

        assert_eq!(expected, result);
    }

    #[test]
    fn test_if_else_expression() {
        use Expression::*;

        let input = r#"if (x < y) { x } else { y }
"#;
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect();
        let result = parse(tokens);

        let expected = Program(vec![Statement::Expression(If(
            Box::new(LessThan(
                Box::new(Ident("x".to_string())),
                Box::new(Ident("y".to_string())),
            )),
            vec![(Box::new(Statement::Expression(Ident("x".to_string()))))],
            Some(vec![
                (Box::new(Statement::Expression(Ident("y".to_string())))),
            ]),
        ))]);

        assert_eq!(expected, result);
    }

    #[test]
    fn test_function_literal_expression() {
        use Expression::*;

        let input = r#"fn(x, y) { x + y; };
fn() {};
fn(x) {};
fn(x, y, z) {};
"#;
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect();
        let result = parse(tokens);

        let expected = Program(vec![
            Statement::Expression(Lambda(
                vec![Identifier("x".to_string()), Identifier("y".to_string())],
                vec![
                    (Box::new(Statement::Expression(Plus(
                        Box::new(Ident("x".to_string())),
                        Box::new(Ident("y".to_string())),
                    )))),
                ],
            )),
            Statement::Expression(Lambda(vec![], vec![])),
            Statement::Expression(Lambda(vec![Identifier("x".to_string())], vec![])),
            Statement::Expression(Lambda(
                vec![
                    Identifier("x".to_string()),
                    Identifier("y".to_string()),
                    Identifier("z".to_string()),
                ],
                vec![],
            )),
        ]);

        assert_eq!(expected, result);
    }

    #[test]
    fn test_call_expression() {
        use Expression::*;

        let input = r#"add(1, 2 * 3, 4 + 5);
"#;
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect();
        let result = parse(tokens);

        let expected = Program(vec![Statement::Expression(Call(
            Box::new(Ident("add".to_string())),
            vec![
                Box::new(IntegerLiteral(1)),
                Box::new(Multiply(
                    Box::new(IntegerLiteral(2)),
                    Box::new(IntegerLiteral(3)),
                )),
                Box::new(Plus(
                    Box::new(IntegerLiteral(4)),
                    Box::new(IntegerLiteral(5)),
                )),
            ],
        ))]);

        assert_eq!(expected, result);
    }

}
