/// macro for tokenizing symbols & keywords
#[macro_export]
macro_rules! t {
    (";") => {
        $crate::lexer_base::token::TokenType::Semicolon
    };
    ("{") => {
        $crate::lexer_base::token::TokenType::LBrace
    };
    ("}") => {
        $crate::lexer_base::token::TokenType::RBrace
    };
    ("(") => {
        $crate::lexer_base::token::TokenType::LParen
    };
    (")") => {
        $crate::lexer_base::token::TokenType::RParen
    };
    ("int") => {
        $crate::lexer_base::token::TokenType::Int
    };
    ("void") => {
        $crate::lexer_base::token::TokenType::Void
    };
    ("return") => {
        $crate::lexer_base::token::TokenType::Return
    };
}
