#[derive(Debug, Clone, PartialEq)]
pub enum KeywordType {
    Int,
    Void,
    Return,
}
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolType {
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),   // [a-zA-Z_]\w*
    Constant(i32),        // [0-9]+
    Keyword(KeywordType), // int void return
    Symbol(SymbolType),   // ; , ( ) { }
}

pub fn tokenizer(input: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut s = input;

    while !s.is_empty() {
        // Trim leading whitespace
        if let Some(i) = s.find(|c: char| !c.is_whitespace()) {
            s = &s[i..];
        } else {
            break;
        }

        // Symbols : single char
        match s.chars().next().unwrap() {
            ';' => {
                tokens.push(Token::Symbol(SymbolType::Semicolon));
                s = &s[1..];
                continue;
            }
            '(' => {
                tokens.push(Token::Symbol(SymbolType::LParen));
                s = &s[1..];
                continue;
            }
            ')' => {
                tokens.push(Token::Symbol(SymbolType::RParen));
                s = &s[1..];
                continue;
            }
            '{' => {
                tokens.push(Token::Symbol(SymbolType::LBrace));
                s = &s[1..];
                continue;
            }
            '}' => {
                tokens.push(Token::Symbol(SymbolType::RBrace));
                s = &s[1..];
                continue;
            }
            _ => {}
        }

        if let Some(head) = s.chars().next() {
            // Constants
            if head.is_ascii_digit() {
                let end = s.find(|c: char| !c.is_ascii_digit()).unwrap_or(s.len());
                let number_str = &s[..end];

                if end < s.len() {
                    let tail_head = s[end..].chars().next().unwrap();
                    if !tail_head.is_whitespace() && !";)}".contains(tail_head) {
                        Err(format!(
                            "Invalid character after constant: {}, current constant : {}",
                            tail_head, number_str
                        ))?;
                    }
                }
                let number = number_str.parse().unwrap();
                tokens.push(Token::Constant(number));
                s = &s[end..];
                continue;
            }

            // Keywords and Identifiers
            if head.is_ascii_alphanumeric() || head == '_' {
                let end = s
                    .find(|c: char| !c.is_alphanumeric() && c != '_')
                    .unwrap_or(s.len());
                let ident_str = &s[..end];

                match ident_str {
                    "int" => tokens.push(Token::Keyword(KeywordType::Int)),
                    "void" => tokens.push(Token::Keyword(KeywordType::Void)),
                    "return" => tokens.push(Token::Keyword(KeywordType::Return)),
                    _ => tokens.push(Token::Identifier(ident_str.to_string())),
                }
                s = &s[end..];
                continue;
            }
            Err(format!("Unexpected character: {}", head))?;
        }
    }

    Ok(tokens)
}
