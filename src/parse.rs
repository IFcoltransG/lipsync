use std::fmt::Debug;

#[derive(Debug, Clone)]
enum Token {
    LeftBracket(Location),
    RightBracket(Location),
    IdentifierToken(String, Location),
    NumberToken(String, Location),
    Dot(Location),
    Colon(Location),
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
struct Location(usize);

fn is_id(character: &char) -> bool {
    !(character.is_whitespace() || "().:".contains(*character))
}

fn lex(input: &str) -> Vec<Token> {
    use Token::*;
    let mut string_stream = input.chars().enumerate().peekable();
    let mut tokens = Vec::with_capacity(input.len());
    while let Some((location, character)) = string_stream.next() {
        match character {
            '(' => tokens.push(LeftBracket(Location(location))),
            ')' => tokens.push(RightBracket(Location(location))),
            '.' => tokens.push(Dot(Location(location))),
            ':' => tokens.push(Colon(Location(location))),
            character if character.is_whitespace() => (),
            character if character.is_numeric() => {
                let mut number = character.to_string();
                while let Some(true) = string_stream
                    .peek()
                    .map(|(_, character)| character.is_numeric())
                {
                    number.push(
                        string_stream
                            .next()
                            .expect("Peeked character has disappeared")
                            .1,
                    );
                }
                tokens.push(NumberToken(number, Location(location)))
            }
            character if is_id(&character) => {
                let mut id = character.to_string();
                while let Some(true) = string_stream.peek().map(|(_, character)| is_id(character)) {
                    id.push(
                        string_stream
                            .next()
                            .expect("Peeked character has disappeared")
                            .1,
                    );
                }
                tokens.push(IdentifierToken(id, Location(location)))
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
struct Ast<V>(AstValue<V>, Type<V>, Location);

impl<V: PartialEq> PartialEq for Ast<V> {
    fn eq(&self, Ast(other_val, ..): &Self) -> bool {
        let Ast(self_val, ..) = self;
        self_val == other_val
    }
}

impl<V: PartialEq> Eq for Ast<V> {}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Token::LeftBracket(_) => matches!(other, Token::LeftBracket(_)),
            Token::RightBracket(_) => matches!(other, Token::RightBracket(_)),
            Token::IdentifierToken(name, _) => matches!(
                other,
                Token::IdentifierToken(other_name, _) if name == other_name
            ),
            Token::NumberToken(number, _) => {
                matches!(
                    other,
                    Token::NumberToken(other_number, _) if number == other_number
                )
            }
            Token::Dot(_) => matches!(other, Token::Dot(_)),
            Token::Colon(_) => matches!(other, Token::Colon(_)),
        }
    }
}

impl Eq for Token {}

impl Token {
    fn location(&self) -> Location {
        match self {
            Token::LeftBracket(location) => *location,
            Token::RightBracket(location) => *location,
            Token::IdentifierToken(_, location) => *location,
            Token::NumberToken(_, location) => *location,
            Token::Dot(location) => *location,
            Token::Colon(location) => *location,
        }
    }
}

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
    fn into_ast(self, type_var: V, location: Location) -> Ast<V> {
        Ast(self, Type::Unknown(type_var), location)
    }
}
impl<V> Ast<V> {
    fn cons_to(self, tail: Ast<V>, type_var: V, location: Location) -> Ast<V> {
        AstValue::Cons(Box::new(self), Box::new(tail)).into_ast(type_var, location)
    }

    fn ascribe(self, type_of: Ast<V>, type_var: V, location: Location) -> Ast<V> {
        AstValue::Ascription(Box::new(self), Type::Expr(Box::new(type_of)))
            .into_ast(type_var, location)
    }
}

impl<V: Debug> Ast<V> {
    fn pretty(&self) -> String {
        use AstValue::*;
        match self {
            Ast(Identifier(name), ..) => name.to_owned(),
            Ast(Number(number), ..) => number.to_owned(),
            Ast(Cons(left, right), ..) => format!("({}.{})", left.pretty(), right.pretty()),
            Ast(Nil, ..) => "()".to_owned(),
            Ast(Ascription(left, right), ..) => format!("({}:{})", left.pretty(), right.pretty()),
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
        location: Location
    ) -> Ast<TypeVar> {
        fn add_next_value(
            mut value: Ast<TypeVar>,
            current_ast: &mut Ast<TypeVar>,
            ops: &mut Vec<Operator<TypeVar>>,
            context: &mut TypeContext,
            location: Location,
        ) {
            use Type::*;
            if let Some(operator) = ops.pop() {
                match operator {
                    DotOp(Ast(value, type_of, location)) => {
                        *current_ast = Ast(value, type_of, location)
                    } // current_ast should be Nil
                    ColonOp(mut type_ast) => {
                        let Ast(_, _, mut first_location) = type_ast;
                        if let Ast(Ascription(first_type, Expr(second_type)), _, second_location) =
                            type_ast.clone()
                        {
                            value = Ascription(Box::new(value), Expr(first_type))
                                .into_ast(TypeVar::new(context), first_location);
                            first_location = second_location;
                            type_ast = *second_type;
                        }
                        *current_ast = Ascription(Box::new(value), Expr(Box::new(type_ast)))
                            .into_ast(TypeVar::new(context), first_location)
                            .cons_to(current_ast.clone(), TypeVar::new(context), first_location);
                        return;
                    }
                }
            }
            *current_ast = value.cons_to(current_ast.clone(), TypeVar::new(context), location);
        }
        #[derive(Debug, Eq, PartialEq, Clone)]
        enum Operator<V> {
            DotOp(Ast<V>),
            ColonOp(Ast<V>),
        }
        use AstValue::*;
        use Operator::*;
        let mut operators = Vec::with_capacity(tokens.len());
        let mut current_ast = Nil.into_ast(TypeVar::new(context), location);
        let mut token_stream = tokens.iter();
        while let Some(next_token) = token_stream.next() {
            match next_token {
                LeftBracketMarker(loc) => {
                    panic!("Left bracket inside a list somehow? at {:?}", loc)
                }
                TokenMarker(LeftBracket(loc) | RightBracket(loc)) => {
                    panic!("Bracket inside list somehow? at {:?}", loc)
                }
                TokenMarker(IdentifierToken(name, loc)) => add_next_value(
                    Identifier(name.to_owned()).into_ast(TypeVar::new(context), *loc),
                    &mut current_ast,
                    &mut operators,
                    context,
                    *loc,
                ),
                TokenMarker(NumberToken(number, loc)) => add_next_value(
                    Number(number.to_owned()).into_ast(TypeVar::new(context), *loc),
                    &mut current_ast,
                    &mut operators,
                    context,
                    *loc,
                ),
                TokenMarker(Dot(loc)) => {
                    if let Ast(Cons(ref last_value, ref rest), _, cons_loc) = current_ast {
                        if **rest != Nil.into_ast(TypeVar::new(context), cons_loc) {
                            panic!(
                                "Dot at {:?} with more than one expression following at {:?}",
                                loc, cons_loc
                            )
                        }
                        operators.push(DotOp(*(*last_value).clone()))
                    } else {
                        panic!("Dot at end of list")
                    }
                }
                TokenMarker(Colon(loc)) => {
                    if let Ast(Cons(last_value, ref rest), _, _) = current_ast {
                        current_ast = *rest.clone();
                        operators.push(ColonOp(*last_value))
                        // colon op contains ascribed type
                    } else {
                        panic!("Colon at end of list at {:?}", loc)
                    }
                }
                TokenList(ast) => {
                    let Ast(_, _, loc) = ast;
                    add_next_value(ast.clone(), &mut current_ast, &mut operators, context, *loc)
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
        LeftBracketMarker(Location),
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
            LeftBracket(loc) => stack.push(LeftBracketMarker(*loc)),
            RightBracket(loc) => {
                let mut token_list = Vec::with_capacity(stack.len());
                while let Some(token) = stack.pop() {
                    if let LeftBracketMarker(loc) = token {
                        break;
                    }
                    token_list.push(token)
                }
                stack.push(TokenList(assemble_token_list(token_list, context, *loc)))
            }
            IdentifierToken(name, loc) => {
                stack.push(TokenMarker(IdentifierToken(name.to_string(), *loc)))
            }
            NumberToken(number, loc) => {
                stack.push(TokenMarker(NumberToken(number.to_string(), *loc)))
            }
            Dot(loc) => stack.push(TokenMarker(Dot(*loc))),
            Colon(loc) => stack.push(TokenMarker(Colon(*loc))),
        }
    }
    while let Some(token) = stack.pop() {
        if let LeftBracketMarker(loc) = token {
            panic!("Extraneous left bracket at {:?}", loc)
        }
        output.push(token)
    }
    if !stack.is_empty() {
        panic!("Couldn't parse")
    }
    assemble_token_list(output, context, Location(0))
}

// fn typecheck(Ast<>) {

//}

#[cfg(test)]
mod tests;
