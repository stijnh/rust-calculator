use parser::{Op, Add, Sub, Mul, Div, Mod};

#[deriving(Eq)]
pub enum Token {
    LeftParen,
    RightParen,
    Number(int),
    Ident(~str),
    Operator(Op),
    End
}

pub fn lex(input:&str) -> ~[Token] {
    let mut tokens = ~[];
    let n = input.char_len();
    let mut i = 0;

    while i < n {
        let t = match input.char_at(i) {
            '0'..'9' => {
                let mut num = 0;

                while i  < n {
                    let c = input.char_at(i);
                    match c {
                        '0'..'9' => num = 10 * num + (c - '0') as int,
                        _ => break
                    }
                    i += 1;
                }
                i -= 1;
                Number(num)
            },
            'a'..'z' => {
                let s = i;

                while i < n {
                    match input.char_at(i) {
                        'a'..'z' => i += 1,
                        _ => break
                    }
                }
                i -= 1;
                Ident(input.slice_chars(s, i + 1).to_owned())
            }
            '(' => LeftParen,
            ')' => RightParen,
            '+' => Operator(Add),
            '-' => Operator(Sub),
            '*' => Operator(Mul),
            '/' => Operator(Div),
            '%' => Operator(Mod),
            ' ' | '\t' | '\n' | '\r' => loop,
            _ => fail!(fmt!("unexpected character %?", input.char_at(i)))
        };

        tokens.push(t);
        i += 1;
    };
    tokens.push(End);
    tokens
}
