#![no_std]
#![deny(missing_docs)]

//! Variadic function support for Rust.
//!
//! # Install
//!
//! ```console
//! cargo add variadiz
//! ```
//!
//! # Example
//!
//! ```
//! use variadiz::*;
//!
//! #[variadic]
//! fn print<T, U>(mut counter: usize, non_variadic: T, others: Option<U>)
//! where
//!     T: std::fmt::Display,
//!     U: std::fmt::Debug,
//! {
//!     #[va_expand_ref(mut counter: usize)]
//!     {
//!         println!("{counter}: {:?}", others);
//!         *counter += 1;
//!     }
//!     #[va_expand_mut]
//!     {
//!         others.take();
//!     }
//!     #[va_expand(mut counter: usize, non_variadic: T)]
//!     {
//!         println!("[{non_variadic}] {counter}: {:?}", others);
//!         *counter += 1;
//!     }
//! }
//!
//! print(
//!     0,
//!     20240429,
//!     va_args!(Some("hello"), Some(vec![1, 2, 3]), Some('e')),
//! );
//! ```
//!
//! Outputs:
//!
//! ```text
//! 0: Some("hello")
//! 1: Some([1, 2, 3])
//! 2: Some('e')
//! [20240429] 3: None
//! [20240429] 4: None
//! [20240429] 5: None
//! ```
//!
//! As methods:
//!
//! ```
//! use std::fmt::Debug;
//! use variadiz::*;
//!
//! struct Person<'a, T>
//! where
//!     T: Debug,
//! {
//!     name: &'a str,
//!     age: u32,
//!     tags: Vec<T>,
//! }
//!
//! #[variadic_impl]
//! impl<'a, T> Person<'a, T>
//! where
//!     T: Debug,
//! {
//!     // Non-variadic method
//!     fn hello(&self) -> &'static str {
//!         "hello"
//!     }
//!
//!     #[variadic]
//!     fn grow_up<U>(&mut self, others: U)
//!     where
//!         U: std::fmt::Debug,
//!     {
//!         #[va_expand(hello: &str, who: &str, mut age: u32, tags: Vec<T>)]
//!         #[va_bind(hello = self.hello())]
//!         #[va_bind(who = self.name)]
//!         #[va_bind(age = self.age)]
//!         #[va_bind(tags = self.tags)]
//!         {
//!             println!("{hello}, {who} is {age} years old,");
//!             println!("\tthey are {tags:?},");
//!             println!("\tand do not forget {others:?}");
//!             *self.age += 1;
//!         }
//!     }
//! }
//!
//! let mut person = Person {
//!     name: "John",
//!     age: 16,
//!     tags: vec!["smart", "awesome"],
//! };
//! person.grow_up(va_args!("hell", Some(62), 0.96));
//! ```
//!
//! Outputs:
//!
//! ```text
//! hello, John is 16 years old,
//!         they are ["smart", "awesome"],
//!         and do not forget "hell"
//! hello, John is 17 years old,
//!         they are ["smart", "awesome"],
//!         and do not forget Some(62)
//! hello, John is 18 years old,
//!         they are ["smart", "awesome"],
//!         and do not forget 0.96
//! ```
//!
//! # Details
//!
//! ## The [`#[variadic]`](crate::variadic) attribute
//!
//! This attribute macro always takes the last declared generic type and the last parameter for variadic, i.e.:
//!
//! ```no_run
//! # use variadiz::*;
//! // The generic type `U` and the parameter `others` are used for variadic.
//! #[variadic]
//! fn print<T, U>(counter: usize, non_variadic: T, others: Option<U>) {
//!     todo!()
//! }
//! ```
//!
//! **NOTE**: It is undefined behavior to use the variadic generic type elsewhere,
//! including be used in bounds of other generic types.
//!
//! However, conversely the variadic generic type can be bound by other generic types:
//!
//! ```no_run
//! # use variadiz::*;
//! #[variadic]
//! fn print<T, U>(counter: usize, non_variadic: T, others: Option<U>)
//! where
//!     // T: From<U>,  // Bad, the behavior is undefined
//!     U: From<T>,     // OK
//! {
//!     todo!()
//! }
//! ```
//!
//! ## Expand variadic parameter pack
//!
//! To expand the variadic parameter pack, you need to add a `#[va_expand]` attribute on a block:
//!
//! ```no_run
//! # use variadiz::*;
//! # #[variadic]
//! # fn print<T, U>(counter: usize, non_variadic: T, others: Option<U>) {
//! #[va_expand]
//! {
//!     todo!()
//! }
//! # }
//! ```
//!
//! Anyway, `#[va_expand]` always consumes all variadic parameters, even if them are bound by `Copy`.
//!
//! Instead, using `#[va_expand_ref]` and `#[va_expand_mut]`, you will get an immutable reference or
//! a mutable reference to each variadic parameter, allowing you to expand the variadic parameter pack
//! multiple times.
//!
//! It should be noted that the expansion block behaves like a function body rather than a closure body -
//! it cannot capture variables from the context automatically.
//!
//! To capture context variables, you must declare them like how you declare function parameters:
//!
//! ```no_run
//! # use variadiz::*;
//! #[variadic]
//! fn print<T>(x: i32, y: &str, others: T) {
//!     #[va_expand(x: i32, y: &str)]
//!     {
//!         todo!()
//!     }
//! }
//! ```
//!
//! **NOTE**: Since the captured variables must be usable multiple times after being expanded,
//! we can only use their references anyway.
//! So when you declare to capture `x`, the `&x` is actually captured.
//! You can add a `mut` before the captured variable to indicate capturing its mutable reference.
//!
//! For example:
//!
//! ```
//! use variadiz::*;
//!
//! #[variadic]
//! fn print<T, U>(mut counter: usize, non_variadic: T, others: Option<U>)
//! where
//!     T: std::fmt::Display,
//!     U: std::fmt::Debug,
//! {
//!     // Capture `counter` by mutable reference.
//!     #[va_expand_ref(mut counter: usize)]
//!     {
//!         println!("{counter}: {:?}", others);
//!         // `counter` here is actually `&mut usize`,
//!         //  a mutable reference to the original `counter`.
//!         *counter += 1;
//!     }
//!     #[va_expand_mut]
//!     {
//!         others.take();
//!     }
//!     // Capture `counter` by mutable reference,
//!     // then capture `non_variadic` by immutable reference.
//!     #[va_expand(mut counter: usize, non_variadic: T)]
//!     {
//!         println!("[{non_variadic}] {counter}: {:?}", others);
//!         *counter += 1;
//!     }
//! }
//!
//! print(
//!     0,
//!     20240429,
//!     va_args!(Some("hello"), Some(vec![1, 2, 3]), Some('e')),
//! );
//! ```
//!
//! Another example:
//!
//! ```rust
//! use variadiz::*;
//!
//! #[variadic]
//! fn collect<T, U>(mut collector: Vec<T>, others: Option<U>) -> Vec<T>
//! where
//!     U: Into<T>, // `U` can be bound by `T`, but not vice versa.
//! {
//!     // `collector` is actually `&mut Vec<T>`
//!     #[va_expand(mut collector: Vec<T>)]
//!     {
//!         if let Some(item) = others {
//!             // The type `U` is specific to each variadic parameter.
//!             // `U` outside an expanded block is **undefined**.
//!             collector.push(<U as Into<T>>::into(item));
//!         }
//!     }
//!     collector
//! }
//!
//! let strs = collect(
//!     vec![String::from("hello")],
//!     va_args!(Some("world"), None::<std::borrow::Cow<str>>, Some('e')),
//! );
//! println!("{:?}", strs);
//! ```
//!
//! Outputs:
//!
//! ```text
//! ["hello", "world", "e"]
//! ```
//!
//! You can use an expanded block almost anywhere expressions can be used,
//! and it always evaluates to a `()`.
//! However, there are two places where you cannot use a expanded block:
//! in another expanded block, or in a macro.
//! For examples:
//!
//! ```
//! use variadiz::*;
//!
//! #[variadic]
//! fn print<T>(mut counter: usize, others: Option<T>)
//! where
//!     T: std::fmt::Debug,
//! {
//!     let _result = (0..10)
//!         .map(|i| {
//!             if i % 2 == 0 {
//!                 #[va_expand_ref(mut counter: usize)]
//!                 {
//!                     println!("{counter}: {:?}", others);
//!                     *counter += 1;
//!                 }
//!                 counter
//!             } else {
//!                 #[va_expand_ref(mut counter: usize)]
//!                 {
//!                     println!("{counter}: {:?}", others);
//!                     *counter -= 1;
//!                 }
//!                 10 - counter
//!             }
//!         })
//!         .collect::<Vec<_>>();
//!
//!     // Expanded block in an expanded block is not allowed.
//!     // #[va_expand_ref(mut counter: usize)]
//!     // {
//!     //     #[va_expand_ref(mut counter: usize)]
//!     //     {
//!     //         println!("{counter}: {:?}", others);
//!     //         *counter += 1;
//!     //     }
//!     // }
//!
//!     // Expanded block in a macro is not allowed.
//!     // call_macro! {
//!     //     #[va_expand_ref(mut counter: usize)]
//!     //     {
//!     //         println!("{counter}: {:?}", others);
//!     //         *counter += 1;
//!     //     }
//!     //};
//! }
//!
//! print(0, va_args!(Some("hello"), Some(vec![1, 2, 3]), Some('e')));
//! ```
//!
//! Except for the captured variables, all generic types, and the identifier of the variadic parameter pack,
//! the expanded block cannot interact with the outer code block.
//!
//! This means, you cannot return a value to the outer by omitting the semicolon of the last statement,
//! nor can you operate on the outer's control flow (i.e., `for`, `loop`, labelled block) via `continue`
//! and `break`.
//!
//! A statement `return` in an expanded block will only exit the expanded block itself (similar to `continue`
//! in a `loop` block) rather than exiting the outer function.
//!
//! ## Bind captured variables
//!
//! You can use the `#[va_bind]` attribute to bind a captured variable to another value, for example:
//!
//! ```
//! use variadiz::*;
//!
//! struct Person {
//!     name: String,
//!     age: u32,
//! }
//!
//! #[variadic]
//! fn print<T>(mut person: Person, interests: T)
//! where
//!     T: std::fmt::Debug,
//! {
//!     #[va_expand_ref(who: String, mut age: u32)]
//!     #[va_bind(who = person.name, age = person.age)]
//!     {
//!         println!("{who} is {age} years old");
//!         println!("And they are interested in {interests:?}");
//!         println!("then a year passed...");
//!         *age += 1;
//!     }
//!
//!     /// Although you cannot split the `va_expand` attribute,
//!     /// but you can split the `va_bind` attribute.
//!     /// The following code is totally equivalent:
//!     #[va_expand_ref(who: String, mut age: u32)]
//!     #[va_bind(who = person.name)]
//!     #[va_bind(age = person.age)]
//!     {
//!         println!("{who} is {age} years old");
//!         println!("And they are interested in {interests:?}");
//!         println!("then a year passed...");
//!         *age += 1;
//!     }
//! }
//!
//! let person = Person {
//!     name: "John".to_string(),
//!     age: 18,
//! };
//! print(person, va_args!("math", (), 114514));
//! ```
//!
//! **NOTE**: Binding a value to a captured variable does **NOT** move it.
//!
//! However, you need to be careful about traps in the case of binding variables
//! to r-values (called value expressions in Rust):
//!
//! ```
//! use variadiz::*;
//!
//! #[variadic]
//! fn count<T>(_others: T) {
//!     let mut counter = 0;
//!
//!     // Bind to l-value (called place expression in Rust).
//!     #[va_expand_ref(mut counter: usize)]
//!     #[va_bind(counter = counter)] // Default behavior.
//!     {
//!         *counter += 1;
//!     }
//!     // `counter` is updated.
//!     assert_eq!(counter, 4);
//!
//!     counter = 0;
//!
//!     // Bind to r-value (called value expression in Rust).
//!     #[va_expand_ref(mut counter: usize)]
//!     #[va_bind(counter = counter + 1 - 1)]
//!     {
//!         *counter += 1;
//!     }
//!     // `counter` is NOT updated!
//!     assert_eq!(counter, 0);
//! }
//!
//! count(va_args!(1, "2", 3.0, [4]));
//! ```
//!
//! ## Call variadic function
//!
//! It is easy to see from the above example that you should pack all variadic arguments into [`va_args!`] macro
//! and pass them as a single argument.
//!
//! The [`va_args!`] macro accepts any number of expressions. Sometimes you may want to annotate the type
//! of each argument, you can use the [`va_types!`] macro:
//!
//! ```
//! # use variadiz::*;
//! #
//! # #[variadic]
//! # fn print<T, U>(mut counter: usize, non_variadic: T, others: Option<U>)
//! # where
//! #     T: std::fmt::Display,
//! #     U: std::fmt::Debug,
//! # {
//! #     #[va_expand_ref(mut counter: usize)]
//! #     {
//! #         println!("{counter}: {:?}", others);
//! #         *counter += 1;
//! #     }
//! #     #[va_expand_mut]
//! #     {
//! #         others.take();
//! #     }
//! #     #[va_expand(mut counter: usize, non_variadic: T)]
//! #     {
//! #         println!("[{non_variadic}] {counter}: {:?}", others);
//! #         *counter += 1;
//! #     }
//! # }
//! #
//! let args: va_types!(Option<&str>, Option<Vec<usize>>, Option<char>) =
//!     va_args!(Some("hello"), Some(vec![1, 2, 3]), Some('e'));
//! print(0, 20240429, args);
//! ```
//!
//! ## Variadic methods support
//!
//! Due to some implementation details, [`#[variadic]`](variadic) has to define some
//! `trait` item outside the variadic function. It conflicts with `impl` item --
//! we cannot define `trait` item in a `impl` item.
//!
//! To solve this problem, you are required to add the [`#[variadic_impl]`](variadic_impl)
//! attribute on the `impl` item to assist moving these `trait` items out.
//!
//! For example:
//!
//! ```
//! use std::fmt::Debug;
//! use variadiz::*;
//!
//! struct Person<'a, T>
//! where
//!     T: Debug,
//! {
//!     name: &'a str,
//!     age: u32,
//!     tags: Vec<T>,
//! }
//!
//! #[variadic_impl]
//! impl<'a, T> Person<'a, T>
//! where
//!     T: Debug,
//! {
//!     // Non-variadic method
//!     fn hello(&self) -> &'static str {
//!         "hello"
//!     }
//!
//!     #[variadic]
//!     fn grow_up<U>(&mut self, others: U)
//!     where
//!         U: std::fmt::Debug,
//!     {
//!         #[va_expand(hello: &str, who: &str, mut age: u32, tags: Vec<T>)]
//!         #[va_bind(hello = self.hello())]
//!         #[va_bind(who = self.name)]
//!         #[va_bind(age = self.age)]
//!         #[va_bind(tags = self.tags)]
//!         {
//!             println!("{hello}, {who} is {age} years old,");
//!             println!("\tthey are {tags:?},");
//!             println!("\tand do not forget {others:?}");
//!             *self.age += 1;
//!         }
//!     }
//! }
//!
//! let mut person = Person {
//!     name: "John",
//!     age: 16,
//!     tags: vec!["smart", "awesome"],
//! };
//! person.grow_up(va_args!("hell", Some(514), 0.96));
//! ```
//!
//! **NOTE**: The `#[variadic]` attribute in the [`#[variadic_impl]`](variadic_impl) item is
//! not really the attribute macro: it is handled directly and will be removed by
//! [`#[variadic_impl]`](variadic_impl).
//! Retaining its name as `#[variadic]` is an ergonomic consideration.
//! This means **it is not affected by Rust's symbol resolution**.
//!
//! ## Variadic trait methods support?
//!
//! To support variadic methods in traits, it requires sharing private bounds between the trait
//! definition and each implementations.
//! There is current no good design for this purpose.

#[doc(hidden)]
pub use tuplez as __tuplez;

/// Create a variadic argument pack.
///
/// Accept many expressions separated by commas, and the result of each expression will
/// be treated as a argument.
///
/// As an extension, it is allowed to use a semicolon plus a number literal to indicate repetition,
/// e.g.:
///
/// ```
/// use variadiz::va_args;
///
/// let repetition = va_args!(3.14, "hello";3, Some(5));
/// let full = va_args!(3.14, "hello", "hello", "hello", Some(5));
/// assert_eq!(repetition, full);
/// ```
pub use tuplez::tuple as va_args;

/// Annotate types for variadic arguments.
///
/// Accept many types separated by commas.
///
/// As an extension, it is allowed to use a semicolon plus a number literal to indicate repetition,
/// e.g.:
///
/// ```
/// use variadiz::{va_args, va_types};
///
/// let repetition: va_types!(f32, &str, &str, &str, Option<i32>) =
///     va_args!(3.14, "hello";3, Some(5));
/// let full: va_types!(f32, &str;3, Option<i32>) =
///     va_args!(3.14, "hello", "hello", "hello", Some(5));
/// assert_eq!(repetition, full);
/// ```
pub use tuplez::tuple_t as va_types;

/// Define a variadic function.
///
/// Please check the [documentation home page](crate) for details.
pub use variadiz_macros::variadic;

/// Indicates that there are variadic methods in the `impl` item.
///
/// Please check the [variadic methods support section](crate#variadic-methods-support) for details.
pub use variadiz_macros::variadic_impl;

extern crate self as variadiz;
