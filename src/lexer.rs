use std::iter::{Fuse, Peekable};
use std::str::Chars;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    Not,
    And,
    Or,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    If,
    Then,
    Else,
    True,
    False,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Comma,
    Assign,
    Arrow,
    Colon,
    Unknown(char),
    Number(String),
    Ident(String),
    Operator(Op),
    End,
}

impl Op {
    pub fn name(self) -> String {
        match self {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
            Op::Eq => "==",
            Op::Neq => "!=",
            Op::Lt => "<",
            Op::Gt => ">",
            Op::Lte => "<=",
            Op::Gte => ">=",
            Op::Not => "not",
            Op::And => "and",
            Op::Or => "or",
        }
        .into()
    }
}

impl Token {
    pub fn name(&self) -> String {
        match self {
            Token::LeftBracket => "[",
            Token::RightBracket => "]",
            Token::LeftParen => "(",
            Token::RightParen => ")",
            Token::Comma => ",",
            Token::Assign => "=",
            Token::Arrow => "=>",
            Token::Number(x) => x,
            Token::Ident(x) => x,
            Token::Unknown(c) => return c.to_string(),
            Token::Operator(x) => return x.name(),
            Token::True => "true",
            Token::False => "false",
            Token::If => "if",
            Token::Then => "then",
            Token::Else => "else",
            Token::End => "<end>",
            Token::Colon => ":",
        }
        .into()
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Span(pub usize, pub usize);

struct CharStream<'a> {
    index: usize,
    iterator: Peekable<Fuse<Chars<'a>>>,
}

impl<'a> CharStream<'a> {
    fn new(line: &'a str) -> CharStream<'a> {
        Self {
            index: 0,
            iterator: line.chars().fuse().peekable(),
        }
    }

    fn next(&mut self) -> char {
        self.index += 1;
        self.iterator.next().unwrap_or('\0')
    }

    fn peek(&mut self) -> char {
        self.iterator.peek().cloned().unwrap_or('\0')
    }
}

pub struct Lexer {
    index: usize,
    tokens: Vec<Token>,
    spans: Vec<Span>,
}

impl Lexer {
    const DIGITS: &'static str = "0123456789.";
    const IDENTS: &'static str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_";

    fn parse_token(stream: &mut CharStream) -> Token {
        let mut c = stream.peek();

        if Self::IDENTS.contains(c) {
            let mut buffer = String::new();

            while Self::IDENTS.contains(c) {
                buffer.push(c);
                stream.next();
                c = stream.peek()
            }

            return match buffer.as_ref() {
                "and" => Token::Operator(Op::And),
                "or" => Token::Operator(Op::Or),
                "not" => Token::Operator(Op::Not),
                "true" => Token::True,
                "false" => Token::False,
                "if" => Token::If,
                "then" => Token::Then,
                "else" => Token::Else,
                _ => Token::Ident(buffer),
            };
        }

        if Self::DIGITS.contains(c) {
            let mut buffer = String::new();

            while Self::DIGITS.contains(c) {
                buffer.push(c);
                stream.next();
                c = stream.peek()
            }

            return Token::Number(buffer);
        }

        stream.next();

        'a: loop {
            let tok = match (c, stream.peek()) {
                ('=', '=') => Token::Operator(Op::Eq),
                ('!', '=') => Token::Operator(Op::Neq),
                ('<', '=') => Token::Operator(Op::Lte),
                ('>', '=') => Token::Operator(Op::Gte),
                ('=', '>') => Token::Arrow,
                _ => break 'a,
            };

            stream.next();
            return tok;
        }

        match c {
            '[' => Token::LeftBracket,
            ']' => Token::RightBracket,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            ',' => Token::Comma,
            '=' => Token::Assign,
            ':' => Token::Colon,
            '+' => Token::Operator(Op::Add),
            '-' => Token::Operator(Op::Sub),
            '*' => Token::Operator(Op::Mul),
            '/' => Token::Operator(Op::Div),
            '>' => Token::Operator(Op::Gt),
            '<' => Token::Operator(Op::Lt),
            c => Token::Unknown(c),
        }
    }

    fn new(line: &str) -> Lexer {
        let mut stream = CharStream::new(line);
        let mut tokens = vec![];
        let mut spans = vec![];

        loop {
            let c = stream.peek();

            if c == '\0' {
                break;
            } else if c.is_whitespace() {
                stream.next();
                continue;
            } else {
                let begin = stream.index;
                let token = Self::parse_token(&mut stream);
                let end = stream.index;
                tokens.push(token);
                spans.push(Span(begin, end));
            }
        }

        let index = stream.index;
        spans.push(Span(index, index + 1));

        Lexer {
            index: 0,
            tokens,
            spans,
        }
    }

    pub fn peek(&self) -> Token {
        self.tokens.get(self.index).cloned().unwrap_or(Token::End)
    }

    pub fn next(&mut self) -> Token {
        let tok = self.peek();
        self.index += 1;
        tok
    }

    pub fn prev(&mut self) {
        assert!(self.index > 0);
        self.index -= 1;
    }

    pub fn span(&self) -> Span {
        if self.index < self.tokens.len() {
            self.spans[self.index]
        } else {
            *self.spans.last().unwrap()
        }
    }
}

pub fn tokenize(line: &str) -> Lexer {
    Lexer::new(line)
}

#[cfg(test)]
mod test {
    use super::{tokenize, CharStream, Op, Token};

    #[test]
    fn test_charstream() {
        let line = "abc";
        let mut stream = CharStream::new(line);

        assert_eq!(stream.peek(), 'a');
        assert_eq!(stream.next(), 'a');
        assert_eq!(stream.peek(), 'b');
        assert_eq!(stream.next(), 'b');
        assert_eq!(stream.peek(), 'c');
        assert_eq!(stream.next(), 'c');
        assert_eq!(stream.peek(), '\0');
        assert_eq!(stream.next(), '\0');
    }

    fn test_match(string: &str, tokens: impl IntoIterator<Item = Token>) {
        let mut lexer = tokenize(string);

        for tok in tokens {
            assert_eq!(lexer.next(), tok);
        }

        assert_eq!(lexer.next(), Token::End);
    }

    #[test]
    fn test_operators() {
        let string = "+ - * / not and or == != < > <= >=";
        let tokens = vec![
            Op::Add,
            Op::Sub,
            Op::Mul,
            Op::Div,
            Op::Not,
            Op::And,
            Op::Or,
            Op::Eq,
            Op::Neq,
            Op::Lt,
            Op::Gt,
            Op::Lte,
            Op::Gte,
        ]
        .into_iter()
        .map(|x| Token::Operator(x));

        test_match(string, tokens);
    }

    #[test]
    fn test_tokens() {
        let string = "( ) [ ] , = => : ?";
        let tokens = vec![
            Token::LeftParen,
            Token::RightParen,
            Token::LeftBracket,
            Token::RightBracket,
            Token::Comma,
            Token::Assign,
            Token::Arrow,
            Token::Colon,
            Token::Unknown('?'),
        ];

        test_match(string, tokens);
    }

    #[test]
    fn test_idents() {
        let string = "true false if then else or and not foo";
        let tokens = vec![
            Token::True,
            Token::False,
            Token::If,
            Token::Then,
            Token::Else,
            Token::Operator(Op::Or),
            Token::Operator(Op::And),
            Token::Operator(Op::Not),
            Token::Ident("foo".into()),
        ];

        test_match(string, tokens);
    }

    #[test]
    fn test_numbers() {
        let string = "1 .2 3. 4.5";
        let tokens = vec![
            Token::Number("1".into()),
            Token::Number(".2".into()),
            Token::Number("3.".into()),
            Token::Number("4.5".into()),
        ];

        test_match(string, tokens);
    }

    #[test]
    fn test_basic() {
        let string = "compare(a, ~)";
        let tokens = vec![
            Token::Ident("compare".into()),
            Token::LeftParen,
            Token::Ident("a".into()),
            Token::Comma,
            Token::Unknown('~'),
            Token::RightParen,
        ];

        test_match(string, tokens);
    }

    #[test]
    fn test_prev_peek_next() {
        let string = "a b c";
        let mut lexer = tokenize(string);

        let a = Token::Ident("a".into());
        let b = Token::Ident("b".into());
        let c = Token::Ident("c".into());
        let end = Token::End;

        assert_eq!(lexer.peek(), a);
        assert_eq!(lexer.next(), a);
        assert_eq!(lexer.peek(), b);
        lexer.prev();
        assert_eq!(lexer.peek(), a);
        assert_eq!(lexer.next(), a);
        assert_eq!(lexer.next(), b);
        assert_eq!(lexer.next(), c);
        lexer.prev();
        assert_eq!(lexer.next(), c);
        assert_eq!(lexer.next(), Token::End);
    }
}
