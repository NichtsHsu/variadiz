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
fn print<T, U>(mut counter: usize, non_variadic: T, others: Option<U>)
where
    T: std::fmt::Display,
    U: std::fmt::Debug,
{
    #[va_expand_ref(mut counter: usize)]
    {
        println!("{counter}: {:?}", others);
        *counter += 1;
    }
    #[va_expand_mut]
    {
        others.take();
    }
    #[va_expand(mut counter: usize, non_variadic: T)]
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

Outputs:

```text
0: Some("hello")
1: Some([1, 2, 3])
2: Some('e')
[20240429] 3: None
[20240429] 4: None
[20240429] 5: None
```

As methods:

```rust
use std::fmt::Debug;
use variadiz::*;

struct Person<'a, T>
where
    T: Debug,
{
    name: &'a str,
    age: u32,
    tags: Vec<T>,
}

#[variadic_impl]
impl<'a, T> Person<'a, T>
where
    T: Debug,
{
    // Non-variadic method
    fn hello(&self) -> &'static str {
        "hello"
    }

    #[variadic]
    fn grow_up<U>(&mut self, others: U)
    where
        U: std::fmt::Debug,
    {
        #[va_expand(hello: &str, who: &str, mut age: u32, tags: Vec<T>)]
        #[va_bind(hello = self.hello())]
        #[va_bind(who = self.name)]
        #[va_bind(age = self.age)]
        #[va_bind(tags = self.tags)]
        {
            println!("{hello}, {who} is {age} years old,");
            println!("\tthey are {tags:?},");
            println!("\tand do not forget {others:?}");
            *self.age += 1;
        }
    }
}

let mut person = Person {
    name: "John",
    age: 16,
    tags: vec!["smart", "awesome"],
};
person.grow_up(va_args!("hell", Some(62), 0.96));
```

Outputs:

```text
hello, John is 16 years old,
        they are ["smart", "awesome"],
        and do not forget "hell"
hello, John is 17 years old,
        they are ["smart", "awesome"],
        and do not forget Some(62)
hello, John is 18 years old,
        they are ["smart", "awesome"],
        and do not forget 0.96
```

Check the [documentation page](https://docs.rs/variadiz) for details.
