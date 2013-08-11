use parser::{Op, Add, Sub, Mul, Div, Mod};
use std::float;
use std::char::is_whitespace;

#[deriving(Eq)]
pub enum Token {
    LeftParen,
    RightParen,
    Comma,
    Number(float),
    Ident(~str),
    Operator(Op),
    End
}

struct Lexer {
    input:~str,
    pos:@mut uint,
    prev_pos:@mut uint
}

impl Lexer {
    pub fn next(&self) -> ~Token {
        let mut i = *self.pos;
        let n = self.input.char_len();

        while i < n && is_whitespace(self.input.char_at(i)) {
            i += 1;
        }

        if i >= n {
            *self.prev_pos = i;
            return ~End;
        }

        let s = i; 
        let c = self.input.char_at(i);
        let token = match c {
            '0' .. '9' | '.' => {
                while i < n {
                    match self.input.char_at(i) {
                        '0' .. '9' | '.' => i += 1,
                        _ => break
                    }
                }
                i -= 1;

                let sub = self.input.slice_chars(s, i + 1); 
                match float::from_str(sub) {
                    Some(x) => Number(x),
                    None => fail!(fmt!("'%? is not a valid number'", sub))
                }
            }
            'a' .. 'z' | 'A' .. 'Z' => {
                while i < n {
                    match self.input.char_at(i) {
                        'a' .. 'z' | 'A' .. 'Z' => i += 1,
                        _ => break
                    }
                }
                i -= 1;

                let sub = self.input.slice_chars(s, i + 1);
                Ident(sub.to_owned())
            }
            '(' => LeftParen,
            ')' => RightParen,
            ',' => Comma,
            '+' => Operator(Add),
            '-' => Operator(Sub),
            '*' => Operator(Mul),
            '/' => Operator(Div),
            '%' => Operator(Mod),
            _ => fail!(fmt!("unexpected character %?", c))
        };

        *self.prev_pos = s;
        *self.pos = i + 1;
        ~token
    }

    pub fn next_expect(&self, expect:&Token) {
        let t = self.next();
        if *t != *expect {
            fail!(fmt!("Unexpected %?, expecting %?", t, expect))
        }
    }

    pub fn back(&self) {
        *self.pos = *self.prev_pos;
    }
}

pub fn lex(input:&str) -> ~Lexer {
    ~Lexer {
        input: input.to_owned(),
        pos: @mut 0,
        prev_pos: @mut 0
    }
}
