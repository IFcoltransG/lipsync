#[test]
fn lex_works() {
    use super::Token::*;
    let lexed = super::lex("(    asdasd asdasd)assdasd))((");
    let golden = vec![
        LeftBracket,
        IdentifierToken("asdasd".to_string()),
        IdentifierToken("asdasd".to_string()),
        RightBracket,
        IdentifierToken("assdasd".to_string()),
        RightBracket,
        RightBracket,
        LeftBracket,
        LeftBracket,
    ];

    assert_eq!(lexed, golden);
}

#[test]
fn lex_works_with_symbols() {
    use super::Token::*;
    assert_eq!(
        super::lex("a b-c 12345 ..."),
        vec![
            IdentifierToken("a".to_string()),
            IdentifierToken("b-c".to_string()),
            NumberToken("12345".to_string()),
            Dot,
            Dot,
            Dot
        ]
    );
}

#[test]
fn lex_ids_starting_with_numbers() {
    use super::Token::*;
    assert_eq!(
        super::lex("111aaa"),
        vec![
            NumberToken("111".to_string()),
            IdentifierToken("aaa".to_string()),
        ]
    );
}

#[test]
fn lex_punctuation() {
    use super::Token::*;
    assert_eq!(
        super::lex(".():"),
        vec![Dot, LeftBracket, RightBracket, Colon]
    );
}

#[test]
fn parse_identifier() {
    use super::{AstValue::*, TypeContext, TypeVar};
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("hello");
    assert_eq!(
        super::parse(tokens, parse_context),
        Identifier("hello".to_string())
            .into_ast(TypeVar::new(golden_context))
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
    );
}

#[test]
fn parse_list() {
    use super::{AstValue::*, TypeContext, TypeVar};
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("(hello)");
    assert_eq!(
        super::parse(tokens, parse_context),
        Identifier("hello".to_string())
            .into_ast(TypeVar::new(golden_context))
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
    );
}

#[test]
fn parse_simple_order_in_list() {
    use super::{AstValue::*, TypeContext, TypeVar};
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("(hello world!)");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        Identifier("hello".to_string())
            .into_ast(TypeVar::new(golden_context))
            .cons_to(
                Identifier("world!".to_string())
                    .into_ast(TypeVar::new(golden_context))
                    .cons_to(
                        Nil.into_ast(TypeVar::new(golden_context)),
                        TypeVar::new(golden_context)
                    ),
                TypeVar::new(golden_context)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
            .pretty()
    );
}

#[test]
fn parse_listed_order_in_list() {
    use super::{AstValue::*, TypeContext, TypeVar};
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("((hello) (world!))");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        Identifier("hello".to_string())
            .into_ast(TypeVar::new(golden_context))
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
            .cons_to(
                Identifier("world!".to_string())
                    .into_ast(TypeVar::new(golden_context))
                    .cons_to(
                        Nil.into_ast(TypeVar::new(golden_context)),
                        TypeVar::new(golden_context)
                    )
                    .cons_to(
                        Nil.into_ast(TypeVar::new(golden_context)),
                        TypeVar::new(golden_context)
                    ),
                TypeVar::new(golden_context)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
            .pretty()
    );
}

#[test]
fn parse_big_list() {
    use super::{AstValue::*, TypeContext, TypeVar};
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("(hello there i have words)");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        ["hello", "there", "i", "have", "words"]
            .iter()
            .rev()
            .fold(Nil.into_ast(TypeVar::new(golden_context)), |acc, &name| {
                Identifier(name.to_owned())
                    .into_ast(TypeVar::new(golden_context))
                    .cons_to(acc, TypeVar::new(golden_context))
            })
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
            .pretty()
    );
}

#[test]
fn parse_two_lists() {
    use super::{AstValue::*, TypeContext, TypeVar};
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("() ()");
    assert_eq!(
        super::parse(tokens, parse_context),
        Nil.into_ast(TypeVar::new(golden_context)).cons_to(
            Nil.into_ast(TypeVar::new(golden_context)).cons_to(
                Nil.into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            ),
            TypeVar::new(golden_context)
        )
    );
}

#[test]
fn parse_nested_list() {
    use super::{AstValue::*, TypeContext, TypeVar};
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("((hello))");
    assert_eq!(
        super::parse(tokens, parse_context),
        Identifier("hello".to_string())
            .into_ast(TypeVar::new(golden_context))
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
    )
}

#[test]
fn parse_smaller_list_structure() {
    use super::{AstValue::*, TypeContext, TypeVar};
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("() (())");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        Nil.into_ast(TypeVar::new(golden_context))
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context))
                    .cons_to(
                        Nil.into_ast(TypeVar::new(golden_context)),
                        TypeVar::new(golden_context)
                    )
                    .cons_to(
                        Nil.into_ast(TypeVar::new(golden_context)),
                        TypeVar::new(golden_context)
                    ),
                TypeVar::new(golden_context)
            )
            .pretty(),
    );
}

#[test]
fn parse_medium_list() {
    use super::{AstValue::*, TypeContext, TypeVar};
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("(id (id2))");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        Identifier("id".to_string())
            .into_ast(TypeVar::new(golden_context))
            .cons_to(
                Identifier("id2".to_string())
                    .into_ast(TypeVar::new(golden_context))
                    .cons_to(
                        Nil.into_ast(TypeVar::new(golden_context)),
                        TypeVar::new(golden_context)
                    )
                    .cons_to(
                        Nil.into_ast(TypeVar::new(golden_context)),
                        TypeVar::new(golden_context)
                    ),
                TypeVar::new(golden_context)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
            .pretty()
    );
}

#[test]
fn parse_list_structure() {
    use super::{AstValue::*, TypeContext, TypeVar};
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("() (() (()))");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        Nil.into_ast(TypeVar::new(golden_context))
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context))
                    .cons_to(
                        Nil.into_ast(TypeVar::new(golden_context))
                            .cons_to(
                                Nil.into_ast(TypeVar::new(golden_context)),
                                TypeVar::new(golden_context)
                            )
                            .cons_to(
                                Nil.into_ast(TypeVar::new(golden_context)),
                                TypeVar::new(golden_context)
                            ),
                        TypeVar::new(golden_context)
                    )
                    .cons_to(
                        Nil.into_ast(TypeVar::new(golden_context)),
                        TypeVar::new(golden_context)
                    ),
                TypeVar::new(golden_context)
            )
            .pretty()
    );
}

#[test]
fn parse_infix_dot() {
    use super::{AstValue::*, TypeContext, TypeVar};
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("a . b");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        Identifier("a".to_string())
            .into_ast(TypeVar::new(golden_context))
            .cons_to(
                Identifier("b".to_string()).into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
            .pretty()
    );
}

#[test]
fn parse_infix_dot_in_list() {
    use super::{AstValue::*, TypeContext, TypeVar};
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("(a . b)");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        Identifier("a".to_string())
            .into_ast(TypeVar::new(golden_context))
            .cons_to(
                Identifier("b".to_string()).into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
            .pretty()
    );
}

#[test]
fn parse_colon() {
    use super::{AstValue::*, TypeContext, TypeVar};
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("a : b");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        Identifier("a".to_owned())
            .into_ast(TypeVar::new(golden_context))
            .ascribe(
                Identifier("b".to_owned()).into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
            .pretty()
    );
}

#[test]
fn parse_adjacent_colons() {
    use super::{AstValue::*, TypeContext, TypeVar};
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("(a : b c : d)");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        Identifier("a".to_owned())
            .into_ast(TypeVar::new(golden_context))
            .ascribe(
                Identifier("b".to_owned()).into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
            .cons_to(
                Identifier("c".to_owned())
                    .into_ast(TypeVar::new(golden_context))
                    .ascribe(
                        Identifier("d".to_owned()).into_ast(TypeVar::new(golden_context)),
                        TypeVar::new(golden_context)
                    )
                    .cons_to(
                        Nil.into_ast(TypeVar::new(golden_context)),
                        TypeVar::new(golden_context)
                    ),
                TypeVar::new(golden_context)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
            .pretty()
    );
}

#[test]
fn parse_nested_colons() {
    use super::{AstValue::*, TypeContext, TypeVar};
    let parse_context = &mut TypeContext::new();
    let golden_context = &mut TypeContext::new();
    let tokens = super::lex("(a : b : d)");
    assert_eq!(
        super::parse(tokens, parse_context).pretty(),
        Identifier("a".to_owned())
            .into_ast(TypeVar::new(golden_context))
            .ascribe(
                Identifier("b".to_owned()).into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
            .ascribe(
                Identifier("d".to_owned()).into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
            .cons_to(
                Nil.into_ast(TypeVar::new(golden_context)),
                TypeVar::new(golden_context)
            )
            .pretty()
    );
}
