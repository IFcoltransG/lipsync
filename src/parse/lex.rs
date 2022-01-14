#[derive(Debug, Clone)]
pub(crate) enum Token {
    LeftBracket(Location),
    RightBracket(Location),
    IdentifierToken(String, Location),
    NumberToken(String, Location),
    Dot(Location),
    Colon(Location),
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub(crate) struct Location(pub usize);

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

fn is_id(character: &char) -> bool {
    !(character.is_whitespace() || "().:".contains(*character))
}

pub(crate) fn lex(input: &str) -> Vec<Token> {
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
