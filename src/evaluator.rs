use crate::ast;
use crate::ast::{Expression, Program, Statement};
use crate::env::Environment;
use crate::object::Object;

pub struct Evaluator {
    pub return_value: Option<Object>,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator { return_value: None }
    }

    fn eval_expr(
        &mut self,
        env: &mut Environment,
        node: &ast::Expression,
    ) -> Result<Object, String> {
        match node {
            Expression::IntegerLiteral(n) => Ok(Object::Integer(*n as isize)),
            Expression::Boolean(b) => Ok(Object::Boolean(*b)),
            Expression::Not(expr) => match self.eval_expr(env, expr)? {
                Object::Boolean(b) => Ok(Object::Boolean(!b)),
                Object::Integer(0) => Ok(Object::Boolean(true)),
                Object::Integer(_) => Ok(Object::Boolean(false)),
                Object::Null => Ok(Object::Boolean(true)),
                Object::Function(_, _, _) => Err("Cannot not a function".to_string()),
            },
            Expression::Negated(expr) => match self.eval_expr(env, expr)? {
                Object::Integer(i) => Ok(Object::Integer(-i)),
                x => Err(format!("Type mismatch for negation: {:?}", x)),
            },
            Expression::Plus(expr1, expr2) => {
                if let Object::Integer(left) = self.eval_expr(env, expr1)? {
                    if let Object::Integer(right) = self.eval_expr(env, expr2)? {
                        return Ok(Object::Integer(left + right));
                    }
                }

                Err(format!("type mismatch for plus: {:?}, {:?}", expr1, expr2))
            }
            Expression::Minus(expr1, expr2) => {
                if let Object::Integer(left) = self.eval_expr(env, expr1)? {
                    if let Object::Integer(right) = self.eval_expr(env, expr2)? {
                        return Ok(Object::Integer(left - right));
                    }
                }

                Err(format!("type mismatch for minus: {:?}, {:?}", expr1, expr2))
            }
            Expression::Divide(expr1, expr2) => {
                if let Object::Integer(left) = self.eval_expr(env, expr1)? {
                    if let Object::Integer(right) = self.eval_expr(env, expr2)? {
                        return Ok(Object::Integer(left / right));
                    }
                }

                Err(format!(
                    "type mismatch for divide: {:?}, {:?}",
                    expr1, expr2
                ))
            }
            Expression::Multiply(expr1, expr2) => {
                if let Object::Integer(left) = self.eval_expr(env, expr1)? {
                    if let Object::Integer(right) = self.eval_expr(env, expr2)? {
                        return Ok(Object::Integer(left * right));
                    }
                }

                Err(format!("type mismatch for plus: {:?}, {:?}", expr1, expr2))
            }
            Expression::Equals(expr1, expr2) => {
                match self.eval_expr(env, expr1)? {
                    Object::Integer(left) => {
                        if let Object::Integer(right) = self.eval_expr(env, expr2)? {
                            return Ok(Object::Boolean(left == right));
                        }
                    }
                    Object::Boolean(left) => {
                        if let Object::Boolean(right) = self.eval_expr(env, expr2)? {
                            return Ok(Object::Boolean(left == right));
                        }
                    }
                    _ => {}
                }

                Err(format!("type mimatch for equals: {:?}, {:?}", expr1, expr2))
            }
            Expression::NotEquals(expr1, expr2) => {
                match self.eval_expr(env, expr1)? {
                    Object::Integer(left) => {
                        if let Object::Integer(right) = self.eval_expr(env, expr2)? {
                            return Ok(Object::Boolean(left != right));
                        }
                    }
                    Object::Boolean(left) => {
                        if let Object::Boolean(right) = self.eval_expr(env, expr2)? {
                            return Ok(Object::Boolean(left != right));
                        }
                    }
                    _ => {}
                }

                Err(format!(
                    "type mismatch for not equals: {:?}, {:?}",
                    expr1, expr2
                ))
            }
            Expression::GreaterThan(expr1, expr2) => {
                if let Object::Integer(left) = self.eval_expr(env, expr1)? {
                    if let Object::Integer(right) = self.eval_expr(env, expr2)? {
                        return Ok(Object::Boolean(left > right));
                    }
                }

                Err(format!(
                    "type mismatch for greater than: {:?}, {:?}",
                    expr1, expr2
                ))
            }
            Expression::LessThan(expr1, expr2) => {
                if let Object::Integer(left) = self.eval_expr(env, expr1)? {
                    if let Object::Integer(right) = self.eval_expr(env, expr2)? {
                        return Ok(Object::Boolean(left < right));
                    }
                }

                Err(format!(
                    "type mismatch for less than: {:?}, {:?}",
                    expr1, expr2
                ))
            }
            Expression::If(condition, consequence, alternative) => {
                if is_truthy(self.eval_expr(env, condition)?) {
                    self.eval_block_statements(env, &consequence)
                } else if let Some(stmts) = alternative {
                    self.eval_block_statements(env, &stmts)
                } else {
                    Ok(Object::Null)
                }
            }
            Expression::Ident(ident) => {
                if let Some(obj) = env.get(ident) {
                    Ok(obj.clone())
                } else {
                    Err(format!("Identifier not found: {:?}", ident))
                }
            }
            Expression::Lambda(params, stmts) => Ok(Object::Function(
                params.to_vec(),
                env.clone(),
                stmts.to_vec(),
            )),
            Expression::Call(f, params) => match self.eval_expr(env, f)? {
                Object::Function(idents, fenv, stmts) => {
                    if params.len() == idents.len() {
                        let mut extended_env = fenv.extend();
                        for (ident, param) in idents.iter().zip(params) {
                            extended_env.set(ident.clone(), self.eval_expr(env, param)?);
                        }
                        self.eval_block_statements(&mut extended_env, &stmts)
                    } else {
                        Err(format!(
                            "Incorrect number of arguments to function call: {:?}",
                            params
                        ))
                    }
                }
                _ => Err(format!("Not a function: {:?}", f)),
            },
        }
    }

    fn eval_block_statements(
        &mut self,
        env: &mut Environment,
        stmts: &[ast::Statement],
    ) -> Result<Object, String> {
        let mut result = Object::Null;

        for stmt in stmts {
            match stmt {
                Statement::Expression(expr) => result = self.eval_expr(env, &expr)?,
                Statement::Return(expr) => {
                    result = self.eval_expr(env, &expr)?;
                    self.return_value = Some(result.clone());
                    return Ok(result);
                }
                Statement::Let(ident, expr) => {
                    let obj = self.eval_expr(env, &expr)?;
                    env.set(ident.clone(), obj);
                    result = Object::Null;
                }
            }
        }

        Ok(result)
    }

    pub fn eval(
        &mut self,
        prgm: ast::Program,
        env: &mut Environment,
    ) -> Result<Object, String> {
        let Program(stmts) = prgm;

        let result = self.eval_block_statements(env, &stmts)?;

        if let Some(return_value) = &self.return_value {
            Ok(return_value.clone())
        } else {
            Ok(result)
        }
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean(b) => b,
        Object::Integer(0) => false,
        Object::Integer(_) => true,
        Object::Function(_, _, _) => false,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::parse;

    fn helper(input: &str) -> Result<Object, String> {
        let lexer = Lexer::new(input);
        let tokens: Vec<_> = lexer.collect();
        let ast = parse(tokens).unwrap();
        let mut evaluator = Evaluator::new();
        let mut env = Environment::new();
        evaluator.eval(ast, &mut env)
    }

    #[test]
    fn test_eval_integer_expression() {
        assert_eq!(Ok(Object::Integer(1)), helper("1"));
        assert_eq!(Ok(Object::Integer(5)), helper("5"));
        assert_eq!(Ok(Object::Integer(10)), helper("10"));
        assert_eq!(Ok(Object::Integer(-5)), helper("-5"));
        assert_eq!(Ok(Object::Integer(-10)), helper("-10"));
        assert_eq!(Ok(Object::Integer(10)), helper("5 + 5 + 5 + 5 - 10"));
        assert_eq!(Ok(Object::Integer(32)), helper("2 * 2 * 2 * 2 * 2"));
        assert_eq!(Ok(Object::Integer(0)), helper("-50 + 100 + -50"));
        assert_eq!(Ok(Object::Integer(20)), helper("5 * 2 + 10"));
        assert_eq!(Ok(Object::Integer(25)), helper("5 + 2 * 10"));
        assert_eq!(Ok(Object::Integer(0)), helper("20 + 2 * -10"));
        assert_eq!(Ok(Object::Integer(60)), helper("50 / 2 * 2 + 10"));
        assert_eq!(Ok(Object::Integer(30)), helper("2 * (5 + 10)"));
        assert_eq!(Ok(Object::Integer(37)), helper("3 * 3 * 3 + 10"));
        assert_eq!(Ok(Object::Integer(37)), helper("3 * (3 * 3) + 10"));
        assert_eq!(
            Ok(Object::Integer(50)),
            helper("(5 + 10 * 2 + 15 / 3) * 2 + -10")
        );
    }

    #[test]
    fn test_eval_boolean_expression() {
        assert_eq!(Ok(Object::Boolean(true)), helper("true"));
        assert_eq!(Ok(Object::Boolean(false)), helper("false"));
        assert_eq!(Ok(Object::Boolean(true)), helper("1 < 2"));
        assert_eq!(Ok(Object::Boolean(false)), helper("1 > 2"));
        assert_eq!(Ok(Object::Boolean(false)), helper("1 < 1"));
        assert_eq!(Ok(Object::Boolean(false)), helper("1 > 1"));
        assert_eq!(Ok(Object::Boolean(true)), helper("1 == 1"));
        assert_eq!(Ok(Object::Boolean(false)), helper("1 != 1"));
        assert_eq!(Ok(Object::Boolean(false)), helper("1 == 2"));
        assert_eq!(Ok(Object::Boolean(true)), helper("1 != 2"));
        assert_eq!(Ok(Object::Boolean(true)), helper("true == true"));
        assert_eq!(Ok(Object::Boolean(true)), helper("false == false"));
        assert_eq!(Ok(Object::Boolean(false)), helper("true == false"));
        assert_eq!(Ok(Object::Boolean(true)), helper("true != false"));
        assert_eq!(Ok(Object::Boolean(true)), helper("false != true"));
        assert_eq!(Ok(Object::Boolean(true)), helper("(1 < 2) == true"));
        assert_eq!(Ok(Object::Boolean(false)), helper("(1 < 2) == false"));
        assert_eq!(Ok(Object::Boolean(false)), helper("(1 > 2) == true"));
        assert_eq!(Ok(Object::Boolean(true)), helper("(1 > 2) == false"));
    }

    #[test]
    fn test_eval_not() {
        assert_eq!(Ok(Object::Boolean(false)), helper("!true"));
        assert_eq!(Ok(Object::Boolean(true)), helper("!false"));
        assert_eq!(Ok(Object::Boolean(false)), helper("!5"));
        assert_eq!(Ok(Object::Boolean(true)), helper("!!true"));
        assert_eq!(Ok(Object::Boolean(false)), helper("!!false"));
        assert_eq!(Ok(Object::Boolean(true)), helper("!!5"));
    }

    #[test]
    fn test_if_else_expression() {
        assert_eq!(Ok(Object::Integer(10)), helper("if (true) { 10 }"));
        assert_eq!(Ok(Object::Null), helper("if (false) { 10 }"));
        assert_eq!(Ok(Object::Integer(10)), helper("if (1) { 10 }"));
        assert_eq!(Ok(Object::Integer(10)), helper("if (1 < 2) { 10 }"));
        assert_eq!(Ok(Object::Null), helper("if (1 > 2) { 10 }"));
        assert_eq!(
            Ok(Object::Integer(20)),
            helper("if (1 > 2) { 10 } else { 20 }")
        );
        assert_eq!(
            Ok(Object::Integer(10)),
            helper("if (1 < 2) { 10 } else { 20 }")
        );
    }

    #[test]
    fn test_return_statement() {
        assert_eq!(Ok(Object::Integer(10)), helper("return 10;"));
        assert_eq!(Ok(Object::Integer(10)), helper("return 10; 9;"));
        assert_eq!(Ok(Object::Integer(10)), helper("return 2 * 5; 9;"));
        assert_eq!(Ok(Object::Integer(10)), helper("9; return 2 * 5; 9;"));
        assert_eq!(
            Ok(Object::Integer(10)),
            helper(
                r#"if (10 > 1) {
  if (10 > 1) {
    return 10;
  }

  return 1;
  }
"#
            )
        );
    }

    #[test]
    fn test_error_handling() {
        assert!(helper("5 + true").is_err(), "`5 + true` should be an error");
        assert!(
            helper("5 + true; 5").is_err(),
            "`5 + true; 5` should be an error"
        );
        assert!(helper("-true").is_err(), "`-true` should be an error");
        assert!(
            helper("true + false").is_err(),
            "`true + false` should be an error"
        );
        assert!(
            helper("5; true + false; 5").is_err(),
            "`5; true + false; 5` should be an error"
        );
        assert!(
            helper("if (10 > 1) { true + false; }").is_err(),
            "`if (10 > 1) { true + false; }` should be an error"
        );
        assert!(
            helper(
                r#"if (10 > 1) {
  if (10 > 1) {
    return true + false;
  }

  return 1;
}"#
            )
            .is_err(),
            "This crazyness should return an error"
        );
    }

    #[test]
    fn test_let_statements() {
        assert_eq!(Ok(Object::Integer(5)), helper("let a = 5; a;"));
        assert_eq!(Ok(Object::Integer(25)), helper("let a = 5 * 5; a;"));
        assert_eq!(Ok(Object::Integer(5)), helper("let a = 5; let b = a; b;"));
        assert_eq!(
            Ok(Object::Integer(15)),
            helper("let a = 5; let b = a; let c = a + b + 5; c;")
        );
        assert!(helper("foobar").is_err(), "Unbound identifier should err");
    }

    #[test]
    fn test_function_expression() {
        assert!(
            if let Ok(Object::Function(_, _, _)) = helper("fn(x) { x + 2; };") {
                true
            } else {
                false
            },
            "Functions should be returnable"
        );
    }

    #[test]
    fn test_function_application() {
        assert_eq!(
            Ok(Object::Integer(5)),
            helper("let identity = fn(x) { x; }; identity(5);")
        );
        assert_eq!(
            Ok(Object::Integer(5)),
            helper("let identity = fn(x) { return x; }; identity(5);")
        );
        assert_eq!(
            Ok(Object::Integer(10)),
            helper("let double = fn(x) { x * 2; }; double(5);")
        );
        assert_eq!(
            Ok(Object::Integer(10)),
            helper("let add = fn(x, y) { x + y; }; add(5, 5);")
        );
        assert_eq!(
            Ok(Object::Integer(20)),
            helper("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));")
        );
        assert_eq!(Ok(Object::Integer(5)), helper("fn(x) { x; }(5)"));
    }
}
