#[macro_export]
macro_rules! raise {
    ($x:expr) => {
        {
            Err($x)?;
            panic!("unreachable code")
        }
    };
    ($x:path, $msg:expr) => {
        raise!($x($msg.into()))
    };
    ($x:path, $format:expr, $( $arg:expr),* ) => {
        raise!($x, format!($format, $($arg),*))
    };
}
