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
    !character.is_whitespace() && "().:".chars().all(|x| &x != character)
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
enum TypeVar<V> {
    Unknown,
    Type,
    Expr(AstNode<V>), // This is going to go infinite I think because corecursion
}

type AstNode<V> = Box<Ast<V>>;

#[derive(Debug, Eq, PartialEq, Clone)]
enum AstValue<V> {
    Identifier(String),
    Number(String), // should probably do some parsing here
    Cons(AstNode<V>, AstNode<V>),
    Nil,
    Var(V),
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct Ast<V>(AstValue<V>, TypeVar<V>);

impl<V> AstValue<V> {
    fn into_ast(self, parameters: V) -> Ast<V> {
        Ast(self, TypeVar::Unknown)
    }
}
impl<V> Ast<V> {
    fn cons_to(self, tail: Ast<V>, parameters: V) -> Ast<V> {
        AstValue::Cons(Box::new(self), Box::new(tail)).into_ast(parameters)
    }

    fn pretty(&self) -> String {
        use AstValue::*;
        match self {
            Ast(Identifier(name), _) => name.to_owned(),
            Ast(Number(number), _) => number.to_owned(),
            Ast(Cons(left, right), _) => format!("({}.{})", left.pretty(), right.pretty()),
            Ast(Nil, _) => "()".to_owned(),
            Ast(Var(..), _) => "VAR".to_owned(),
        }
    }
}

fn parse(tokens: Vec<Token>) -> Ast<()> {
    fn assemble_token_list<V: Clone + Eq + Default>(tokens: Vec<Tokenlike<V>>) -> Ast<V> {
        fn add_next_value<V: Clone + Eq + Default>(
            value: Ast<V>,
            current_ast: &mut Ast<V>,
            ops: &mut Vec<Operator<V>>,
        ) {
            if let Some(operator) = ops.pop() {
                match operator {
                    DotOp(cdr) => *current_ast = cdr,
                    ColonOp(_) => todo!(),
                }
            }
            *current_ast = value.cons_to(current_ast.clone(), Default::default());
        }
        enum Operator<V> {
            DotOp(Ast<V>),
            ColonOp(Ast<V>),
        }
        use AstValue::*;
        use Operator::*;
        let mut operators = Vec::with_capacity(tokens.len());
        let mut current_ast = Nil.into_ast(Default::default());
        let mut token_stream = tokens.iter();
        while let Some(next_token) = token_stream.next() {
            match next_token {
                LeftBracketMarker => panic!("Left bracket inside a list somehow?"),
                TokenMarker(LeftBracket | RightBracket) => panic!("Bracket inside list somehow?"),
                TokenMarker(IdentifierToken(name)) => add_next_value(
                    Identifier(name.to_owned()).into_ast(Default::default()),
                    &mut current_ast,
                    &mut operators,
                ),
                TokenMarker(NumberToken(number)) => add_next_value(
                    Number(number.to_owned()).into_ast(Default::default()),
                    &mut current_ast,
                    &mut operators,
                ),
                TokenMarker(Dot) => {
                    if let Ast(Cons(ref last_value, ref rest), _) = current_ast {
                        if **rest != Nil.into_ast(Default::default()) {
                            panic!("Dot with more than one expression following")
                        }
                        operators.push(DotOp(*(*last_value).clone()))
                    } else {
                        panic!("Dot at end of list")
                    }
                }
                TokenMarker(Colon) => todo!(),
                TokenList(ast) => add_next_value(ast.clone(), &mut current_ast, &mut operators),
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
                stack.push(TokenList(assemble_token_list(token_list)))
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
    assemble_token_list(output)
}

#[cfg(test)]
mod tests;
