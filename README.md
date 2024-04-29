# variadiz

Variadic function support for Rust.

## Install

```console
cargo add tuplez variadiz
```

## Example

```rust
use variadiz::*;

#[variadic]
fn print<T, U>(counter: usize, non_variadic: T, others: Option<U>)
where
    T: std::fmt::Display,
    U: std::fmt::Debug,
{
    #[va_expand_ref(counter: usize)]
    {
        println!("{counter}: {:?}", others);
        *counter += 1;
    }
    #[va_expand_mut]
    {
        others.take();
    }
    #[va_expand(counter: usize, non_variadic: T)]
    {
        println!("[{non_variadic}] {counter}: {:?}", others);
        *counter += 1;
    }
}

print(
    0,
    20240429,
    va_args!(Some("hello"), Some(vec![1, 2, 3]), Some('e')),
);
```

Check the [documentation page](https://docs.rs/variadiz) for details.
