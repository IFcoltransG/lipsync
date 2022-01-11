use std::{default, fmt::Debug};

#[derive(Debug, Eq, PartialEq, Clone)]
enum Token {
    LeftBracket,
    RightBracket,
    IdentifierToken(String),
    NumberToken(String),
    Dot,
    Colon,
}

fn is_id(character: &char) -> bool {
    !(character.is_whitespace() || "().:".contains(*character))
}

fn lex(input: &str) -> Vec<Token> {
    use Token::*;
    let mut string_stream = input.chars().peekable();
    let mut tokens = Vec::with_capacity(input.len());
    while let Some(character) = string_stream.next() {
        match character {
            '(' => tokens.push(LeftBracket),
            ')' => tokens.push(RightBracket),
            '.' => tokens.push(Dot),
            ':' => tokens.push(Colon),
            character if character.is_whitespace() => (),
            character if character.is_numeric() => {
                let mut number = character.to_string();
                while let Some(true) = string_stream.peek().map(|character| character.is_numeric())
                {
                    number.push(string_stream.next().unwrap());
                }
                tokens.push(NumberToken(number))
            }
            character if is_id(&character) => {
                let mut id = character.to_string();
                while let Some(true) = string_stream.peek().map(is_id) {
                    id.push(string_stream.next().unwrap());
                }
                tokens.push(IdentifierToken(id))
            }
            _ => (),
        }
    }
    tokens
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum Type<V> {
    Unknown(V),
    Expr(AstNode<V>),
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct TypeContext {
    latest_id: usize,
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct TypeVar {
    id: usize,
}

type AstNode<V> = Box<Ast<V>>;

#[derive(Debug, Eq, PartialEq, Clone)]
enum AstValue<V> {
    Identifier(String),
    Number(String), // should probably do some parsing here
    Cons(AstNode<V>, AstNode<V>),
    Ascription(AstNode<V>, Type<V>),
    Nil,
}

#[derive(Debug, Clone)]
struct Ast<V>(AstValue<V>, Type<V>);

impl<V: PartialEq> PartialEq for Ast<V> {
    fn eq(&self, Ast(other_val, _): &Self) -> bool {
        let Ast(self_val, _) = self;
        self_val == other_val
    }
}

impl<V: PartialEq> Eq for Ast<V> {}

impl TypeContext {
    fn new() -> Self {
        Self { latest_id: 0 }
    }

    fn next_id(&mut self) -> usize {
        self.latest_id += 1;
        self.latest_id
    }
}

impl TypeVar {
    fn new(context: &mut TypeContext) -> Self {
        TypeVar {
            id: context.next_id(),
        }
    }
}

impl<V> AstValue<V> {
    fn into_ast(self, parameters: V) -> Ast<V> {
        Ast(self, Type::Unknown(parameters))
    }
}
impl<V> Ast<V> {
    fn cons_to(self, tail: Ast<V>, parameters: V) -> Ast<V> {
        AstValue::Cons(Box::new(self), Box::new(tail)).into_ast(parameters)
    }

    fn ascribe(self, type_of: Ast<V>, parameters: V) -> Ast<V> {
        AstValue::Ascription(Box::new(self), Type::Expr(Box::new(type_of))).into_ast(parameters)
    }
}

impl<V: Debug> Ast<V> {
    fn pretty(&self) -> String {
        use AstValue::*;
        match self {
            Ast(Identifier(name), _) => name.to_owned(),
            Ast(Number(number), _) => number.to_owned(),
            Ast(Cons(left, right), _) => format!("({}.{})", left.pretty(), right.pretty()),
            Ast(Nil, _) => "()".to_owned(),
            Ast(Ascription(left, right), _) => format!("({}:{})", left.pretty(), right.pretty()),
        }
    }
}

impl<V: Debug> Type<V> {
    fn pretty(&self) -> String {
        match self {
            Type::Unknown(_) => "?".to_string(),
            Type::Expr(ast) => format!("TyFrom[{}]", ast.pretty()),
        }
    }
}

fn parse(tokens: Vec<Token>, context: &mut TypeContext) -> Ast<TypeVar> {
    fn assemble_token_list(
        tokens: Vec<Tokenlike<TypeVar>>,
        context: &mut TypeContext,
    ) -> Ast<TypeVar> {
        fn add_next_value(
            mut value: Ast<TypeVar>,
            current_ast: &mut Ast<TypeVar>,
            ops: &mut Vec<Operator<TypeVar>>,
            context: &mut TypeContext,
        ) {
            use Type::*;
            if let Some(operator) = ops.pop() {
                match operator {
                    DotOp(cdr) => *current_ast = cdr, // current_ast should be Nil
                    ColonOp(mut type_ast) => {
                        if let Ast(Ascription(first_type, Expr(second_type)), _) = type_ast.clone()
                        {
                            value = Ascription(Box::new(value), Expr(first_type))
                                .into_ast(TypeVar::new(context));
                            type_ast = *second_type;
                        }
                        *current_ast = Ascription(Box::new(value), Expr(Box::new(type_ast)))
                            .into_ast(TypeVar::new(context))
                            .cons_to(current_ast.clone(), TypeVar::new(context));
                        return;
                    }
                }
            }
            *current_ast = value.cons_to(current_ast.clone(), TypeVar::new(context));
        }
        #[derive(Debug, Eq, PartialEq, Clone)]
        enum Operator<V> {
            DotOp(Ast<V>),
            ColonOp(Ast<V>),
        }
        use AstValue::*;
        use Operator::*;
        let mut operators = Vec::with_capacity(tokens.len());
        let mut current_ast = Nil.into_ast(TypeVar::new(context));
        let mut token_stream = tokens.iter();
        while let Some(next_token) = token_stream.next() {
            match next_token {
                LeftBracketMarker => panic!("Left bracket inside a list somehow?"),
                TokenMarker(LeftBracket | RightBracket) => panic!("Bracket inside list somehow?"),
                TokenMarker(IdentifierToken(name)) => add_next_value(
                    Identifier(name.to_owned()).into_ast(TypeVar::new(context)),
                    &mut current_ast,
                    &mut operators,
                    context,
                ),
                TokenMarker(NumberToken(number)) => add_next_value(
                    Number(number.to_owned()).into_ast(TypeVar::new(context)),
                    &mut current_ast,
                    &mut operators,
                    context,
                ),
                TokenMarker(Dot) => {
                    if let Ast(Cons(ref last_value, ref rest), _) = current_ast {
                        if **rest != Nil.into_ast(TypeVar::new(context)) {
                            panic!("Dot with more than one expression following")
                        }
                        operators.push(DotOp(*(*last_value).clone()))
                    } else {
                        panic!("Dot at end of list")
                    }
                }
                TokenMarker(Colon) => {
                    if let Ast(Cons(last_value, ref rest), _) = current_ast {
                        current_ast = *rest.clone();
                        operators.push(ColonOp(*last_value))
                        // colon op contains ascribed type
                    } else {
                        panic!("Colon at end of list")
                    }
                }
                TokenList(ast) => {
                    add_next_value(ast.clone(), &mut current_ast, &mut operators, context)
                }
            }
        }
        if !operators.is_empty() {
            panic!("Trailing operator")
        }
        current_ast
    }

    #[derive(Debug, Eq, PartialEq, Clone)]
    enum Tokenlike<V> {
        LeftBracketMarker,
        TokenMarker(Token),
        TokenList(Ast<V>),
    }

    use Token::*;
    use Tokenlike::*;
    let mut stack = Vec::with_capacity(tokens.len());
    let mut tokens = tokens.iter();
    let mut output = Vec::with_capacity(tokens.len());
    while let Some(token) = tokens.next() {
        match token {
            LeftBracket => stack.push(LeftBracketMarker),
            RightBracket => {
                let mut token_list = Vec::with_capacity(stack.len());
                while let Some(token) = stack.pop() {
                    if let LeftBracketMarker = token {
                        break;
                    }
                    token_list.push(token)
                }
                stack.push(TokenList(assemble_token_list(token_list, context)))
            }
            IdentifierToken(name) => stack.push(TokenMarker(IdentifierToken(name.to_string()))),
            NumberToken(number) => stack.push(TokenMarker(NumberToken(number.to_string()))),
            Dot => stack.push(TokenMarker(Dot)),
            Colon => stack.push(TokenMarker(Colon)),
        }
    }
    while let Some(token) = stack.pop() {
        if let LeftBracketMarker = token {
            panic!("Extraneous left bracket")
        }
        output.push(token)
    }
    if !stack.is_empty() {
        panic!("Couldn't parse")
    }
    assemble_token_list(output, context)
}

// fn typecheck(Ast<>) {

//}

#[cfg(test)]
mod tests;
