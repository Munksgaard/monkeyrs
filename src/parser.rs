use crate::ast::*;
use crate::token::Token;

type TokenIter<'a, 'b> = &'a mut std::iter::Peekable<std::slice::Iter<'b, Token>>;

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

fn peek_precedence(tokens: TokenIter) -> Precedence {
    tokens
        .peek()
        .map(|x: &&Token| precedence(*x))
        .unwrap_or(Precedence::Lowest)
}

fn parse_infix_expression(tokens: TokenIter, left: Expression) -> Result<Expression, String> {
    match tokens.next() {
        Some(tok @ Token::PLUS) => {
            if let Ok(right) = parse_expression(tokens, precedence(tok)) {
                Ok(Expression::Plus(Box::new(left), Box::new(right)))
            } else {
                Err("Couldn't parse infix expression 1".to_string())
            }
        }
        Some(tok @ Token::MINUS) => {
            if let Ok(right) = parse_expression(tokens, precedence(tok)) {
                Ok(Expression::Minus(Box::new(left), Box::new(right)))
            } else {
                Err("Couldn't parse infix expression 2".to_string())
            }
        }
        Some(tok @ Token::SLASH) => {
            if let Ok(right) = parse_expression(tokens, precedence(tok)) {
                Ok(Expression::Divide(Box::new(left), Box::new(right)))
            } else {
                Err("Couldn't parse infix expression 3".to_string())
            }
        }
        Some(tok @ Token::ASTERISK) => {
            if let Ok(right) = parse_expression(tokens, precedence(tok)) {
                Ok(Expression::Multiply(Box::new(left), Box::new(right)))
            } else {
                Err("Couldn't parse infix expression 4".to_string())
            }
        }
        Some(tok @ Token::EQ) => {
            if let Ok(right) = parse_expression(tokens, precedence(tok)) {
                Ok(Expression::Equals(Box::new(left), Box::new(right)))
            } else {
                Err("Couldn't parse infix expression 5".to_string())
            }
        }
        Some(tok @ Token::NOTEQ) => {
            if let Ok(right) = parse_expression(tokens, precedence(tok)) {
                Ok(Expression::NotEquals(Box::new(left), Box::new(right)))
            } else {
                Err("Couldn't parse infix expression 5".to_string())
            }
        }
        Some(tok @ Token::LT) => {
            if let Ok(right) = parse_expression(tokens, precedence(tok)) {
                Ok(Expression::LessThan(Box::new(left), Box::new(right)))
            } else {
                Err("Couldn't parse infix expression 6".to_string())
            }
        }
        Some(tok @ Token::GT) => {
            if let Ok(right) = parse_expression(tokens, precedence(tok)) {
                Ok(Expression::GreaterThan(Box::new(left), Box::new(right)))
            } else {
                Err("Couldn't parse infix expression 7".to_string())
            }
        }
        Some(Token::LPAREN) => {
            let right = parse_call_arguments(tokens)?;
            Ok(Expression::Call(Box::new(left), right))
        }
        Some(token) => Err(format!(
            "Invalid token while parsing infix expression: {:?}",
            token
        )),
        None => Err("Got EOF while parsing infix expression".to_string()),
    }
}

fn parse_call_arguments(tokens: TokenIter) -> Result<Vec<Expression>, String> {
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
            None => {
                return Err(
                    "Expected identifier while parsing call arguments, got end of file".to_string(),
                )
            }
        }

        if let Some(Token::COMMA) = tokens.peek() {
            tokens.next();
        }
    }

    Ok(args)
}

fn parse_block_statements(tokens: TokenIter) -> Result<Vec<Statement>, String> {
    let mut statements = Vec::new();

    loop {
        if let Some(Token::RBRACE) = tokens.peek() {
            tokens.next();
            break;
        } else {
            statements.push(parse_statement(tokens)?);
        }
    }

    Ok(statements)
}

fn parse_function_parameters(tokens: TokenIter) -> Result<Vec<Identifier>, String> {
    let mut identifiers = Vec::new();

    loop {
        match tokens.next() {
            Some(Token::RPAREN) => break,
            Some(Token::IDENT(ident)) => identifiers.push(Identifier(ident.to_string())),
            Some(e) => return Err(format!("Expected identifier, got {:?}", e)),
            None => return Err("Expected identifier, got end of file".to_string()),
        }

        if let Some(Token::COMMA) = tokens.peek() {
            tokens.next();
        }
    }

    Ok(identifiers)
}

fn parse_expression(tokens: TokenIter, precedence: Precedence) -> Result<Expression, String> {
    let mut left_exp = match tokens.next() {
        Some(Token::IDENT(ident)) => Expression::Ident(Identifier(ident.to_string())),
        Some(Token::INT(n)) => Expression::IntegerLiteral(*n),
        Some(Token::BANG) => {
            let expr = parse_expression(tokens, Precedence::Prefix)?;

            Expression::Not(Box::new(expr))
        }
        Some(Token::MINUS) => {
            let expr = parse_expression(tokens, Precedence::Prefix)?;

            Expression::Negated(Box::new(expr))
        }
        Some(Token::LPAREN) => {
            let expr = parse_expression(tokens, Precedence::Lowest)?;

            match tokens.next() {
                Some(Token::RPAREN) => expr,
                x => return Err(format!("Expected RPAREN, got {:?}, parsed: {:?}", x, expr)),
            }
        }
        Some(Token::TRUE) => Expression::Boolean(true),
        Some(Token::FALSE) => Expression::Boolean(false),
        Some(Token::IF) => {
            if let Some(Token::LPAREN) = tokens.next() {
            } else {
                return Err("Expected left paren after if keyword".to_string());
            }

            let condition = parse_expression(tokens, Precedence::Lowest)?;

            if let Some(Token::RPAREN) = tokens.next() {
            } else {
                return Err("Expected right paren".to_string());
            }

            if let Some(Token::LBRACE) = tokens.next() {
            } else {
                return Err("Expected left brace".to_string());
            }

            let consequence = parse_block_statements(tokens)?;

            let alternative = if let Some(Token::ELSE) = tokens.next() {
                if let Some(Token::LBRACE) = tokens.next() {
                } else {
                    return Err("Expected left brace".to_string());
                }

                Some(parse_block_statements(tokens)?)
            } else {
                None
            };

            Expression::If(Box::new(condition), consequence, alternative)
        }
        Some(Token::FUNCTION) => {
            if let Some(Token::LPAREN) = tokens.next() {
            } else {
                return Err("Expected left paren while parsing function expression".to_string());
            }

            let parameters = parse_function_parameters(tokens)?;

            if let Some(Token::LBRACE) = tokens.next() {
            } else {
                return Err("Expected left brace while parsing function expression".to_string());
            }

            let body = parse_block_statements(tokens)?;

            Expression::Lambda(parameters, body)
        }
        x => {
            return Err(format!(
                "Unexpected token while parsing expression: {:?}",
                x
            ))
        }
    };

    while tokens.peek() != Some(&&Token::SEMICOLON) && precedence < peek_precedence(tokens) {
        // left_expr = parse_infix_expression(tokens, left.exp_clone())?

        if let Ok(new_exp) = parse_infix_expression(tokens, left_exp.clone()) {
            left_exp = new_exp;
        } else {
            break;
        }
    }

    Ok(left_exp)
}

fn parse_let_statement(tokens: TokenIter) -> Result<Statement, String> {
    match (tokens.next(), tokens.next()) {
        (Some(Token::IDENT(ident)), Some(Token::ASSIGN)) => Ok(Statement::Let(
            Identifier(ident.to_string()),
            parse_expression(tokens, Precedence::Lowest)?,
        )),
        _ => Err("Invalid let statement".to_string()),
    }
}

fn parse_return_statement(tokens: TokenIter) -> Result<Statement, String> {
    parse_expression(tokens, Precedence::Lowest).map(Statement::Return)
}

fn parse_expression_statement(tokens: TokenIter) -> Result<Statement, String> {
    parse_expression(tokens, Precedence::Lowest).map(Statement::Expression)
}

fn parse_statement(tokens: TokenIter) -> Result<Statement, String> {
    let stmt = match tokens.peek() {
        Some(Token::LET) => {
            tokens.next();
            parse_let_statement(tokens)
        }
        Some(Token::RETURN) => {
            tokens.next();
            parse_return_statement(tokens)
        }
        Some(_) => parse_expression_statement(tokens),
        None => return Err("Expected statement, found EOF".to_string()),
    };

    if let Some(Token::SEMICOLON) = tokens.peek() {
        tokens.next();
    }

    stmt
}

pub fn parse(tokens: Vec<Token>) -> Result<Program, String> {
    let mut peekable: std::iter::Peekable<std::slice::Iter<'_, Token>> = tokens.iter().peekable();

    let mut statements = Vec::new();

    while let Some(_) = peekable.peek() {
        statements.push(parse_statement(&mut peekable)?);
    }

    Ok(Program(statements))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn test_let_statements() {
        let input = r#"let x = 5;
let y = 10;
let foobar = 838383;
"#;
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect();
        let result = parse(tokens).unwrap();

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
        let result = parse(tokens).unwrap();

        let expected = Program(vec![
            Statement::Let(Identifier("x".to_string()), Expression::IntegerLiteral(5)),
            Statement::Let(Identifier("y".to_string()), Expression::Boolean(true)),
            Statement::Let(
                Identifier("foobar".to_string()),
                Expression::Ident(Identifier("y".to_string())),
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
        let result = parse(tokens).unwrap();

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
        let result = parse(tokens).unwrap();

        let expected = Program(vec![Statement::Expression(Expression::Ident(Identifier(
            "foobar".to_string(),
        )))]);

        assert_eq!(expected, result);
    }

    #[test]
    fn test_literal_integer_expression() {
        let input = r#"5;"#;
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect();
        let result = parse(tokens).unwrap();

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
        let result = parse(tokens).unwrap();

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
        let result = parse(tokens).unwrap();

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
        let result = parse(tokens).unwrap();

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
        fn test_it(input1: &str, input2: &str) {
            let lexer = Lexer::new(input1);
            let tokens: Vec<_> = lexer.collect();
            let result1 = parse(tokens).unwrap();

            let lexer = Lexer::new(input2);
            let tokens: Vec<_> = lexer.collect();
            let result2 = parse(tokens).unwrap();

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
        let result = parse(tokens).unwrap();

        let expected = Program(vec![Statement::Expression(Plus(
            Box::new(Ident(Identifier("a".to_string()))),
            Box::new(Multiply(
                Box::new(Ident(Identifier("b".to_string()))),
                Box::new(Ident(Identifier("c".to_string()))),
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
        let result = parse(tokens).unwrap();

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
        let result = parse(tokens).unwrap();

        let expected = Program(vec![Statement::Expression(If(
            Box::new(LessThan(
                Box::new(Ident(Identifier("x".to_string()))),
                Box::new(Ident(Identifier("y".to_string()))),
            )),
            vec![Statement::Expression(Ident(Identifier("x".to_string())))],
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
        let result = parse(tokens).unwrap();

        let expected = Program(vec![Statement::Expression(If(
            Box::new(LessThan(
                Box::new(Ident(Identifier("x".to_string()))),
                Box::new(Ident(Identifier("y".to_string()))),
            )),
            vec![Statement::Expression(Ident(Identifier("x".to_string())))],
            Some(vec![Statement::Expression(Ident(Identifier(
                "y".to_string(),
            )))]),
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
        let result = parse(tokens).unwrap();

        let expected = Program(vec![
            Statement::Expression(Lambda(
                vec![Identifier("x".to_string()), Identifier("y".to_string())],
                vec![Statement::Expression(Plus(
                    Box::new(Ident(Identifier("x".to_string()))),
                    Box::new(Ident(Identifier("y".to_string()))),
                ))],
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
        let result = parse(tokens).unwrap();

        let expected = Program(vec![Statement::Expression(Call(
            Box::new(Ident(Identifier("add".to_string()))),
            vec![
                IntegerLiteral(1),
                Multiply(Box::new(IntegerLiteral(2)), Box::new(IntegerLiteral(3))),
                Plus(Box::new(IntegerLiteral(4)), Box::new(IntegerLiteral(5))),
            ],
        ))]);

        assert_eq!(expected, result);
    }

}
