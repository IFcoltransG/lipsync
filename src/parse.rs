use lex::{Location, Token};
use std::{fmt::Debug, hash::Hash};

use crate::typecheck::Type;

pub(crate) mod lex;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub(crate) struct TypeContext {
    latest_id: usize,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub(crate) struct TypeVar {
    id: usize,
}

pub(crate) type AstNode<V> = Box<Ast<V>>;

#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) enum AstValue<V> {
    Identifier(String),
    Number(String), // should probably do some parsing here
    Cons(AstNode<V>, AstNode<V>),
    Ascription(AstNode<V>, Type<V>),
    Nil,
}

#[derive(Debug, Clone)]
pub(crate) struct Ast<V>(pub AstValue<V>, pub Type<V>, pub Location);

impl<V: PartialEq> PartialEq for Ast<V> {
    fn eq(&self, Ast(other_val, ..): &Self) -> bool {
        let Ast(self_val, ..) = self;
        self_val == other_val
    }
}

impl<V: PartialEq> Eq for Ast<V> {}

impl TypeContext {
    pub fn new() -> Self {
        Self { latest_id: 0 }
    }

    fn next_id(&mut self) -> usize {
        self.latest_id += 1;
        self.latest_id
    }
}

impl TypeVar {
    pub(crate) fn new(context: &mut TypeContext) -> Self {
        TypeVar {
            id: context.next_id(),
        }
    }
}

impl<V> AstValue<V> {
    pub(crate) fn into_ast(self, type_var: V, location: Location) -> Ast<V> {
        Ast(self, Type::Unknown(type_var), location)
    }
}
impl<V> Ast<V> {
    pub(crate) fn cons_to(self, tail: Ast<V>, type_var: V, location: Location) -> Ast<V> {
        AstValue::Cons(Box::new(self), Box::new(tail)).into_ast(type_var, location)
    }

    pub(crate) fn ascribe(self, type_of: Ast<V>, type_var: V, location: Location) -> Ast<V> {
        AstValue::Ascription(Box::new(self), Type::Expr(Box::new(type_of)))
            .into_ast(type_var, location)
    }
}

impl<V: Clone> Ast<V> {
    pub(crate) fn get_type(&self) -> Type<V> {
        let Ast(_, type_of, _) = self;
        type_of.clone()
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
            Type::TypeKind => "Ty*".to_string(),
            Type::Special(id) => format!("TyOf[{}]", id),
            Type::Expr(ast) => format!("TyFrom[{}]", ast.pretty()),
        }
    }
}

pub(crate) fn parse(tokens: Vec<Token>, context: &mut TypeContext) -> Ast<TypeVar> {
    fn assemble_token_list(
        tokens: Vec<Tokenlike<TypeVar>>,
        context: &mut TypeContext,
        location: Location,
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
                    if let Ast(Cons(last_value, ref rest), ..) = current_ast {
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
                    if let LeftBracketMarker(_) = token {
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

#[cfg(test)]
mod tests;
