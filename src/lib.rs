//! Variadic function support for Rust.
//!
//! # Install
//!
//! ```console
//! cargo add tuplez variadiz
//! ```
//!
//! # Example
//!
//! ```
//! use variadiz::*;
//!
//! #[variadic]
//! fn print<T, U>(counter: usize, non_variadic: T, others: Option<U>)
//! where
//!     T: std::fmt::Display,
//!     U: std::fmt::Debug,
//! {
//!     #[va_expand_ref(counter: usize)]
//!     {
//!         println!("{counter}: {:?}", others);
//!         *counter += 1;
//!     }
//!     #[va_expand_mut]
//!     {
//!         others.take();
//!     }
//!     #[va_expand(counter: usize, non_variadic: T)]
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
//! [20240429] 0: None
//! [20240429] 1: None
//! [20240429] 2: None
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
//! it cannot capture variables from the context.
//!
//! To capture context variables, you must declare them like declare function parameters:
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
//! **NOTE**: Although capturing the variables will mote them in, to ensure that the captured variables
//! can be used multiple times after being expanded, what you actually get are mutable references to them.
//!
//! Let's go back to the original example:
//!
//! ```
//! use variadiz::*;
//!
//! #[variadic]
//! fn print<T, U>(counter: usize, non_variadic: T, others: Option<U>)
//! where
//!     T: std::fmt::Display,
//!     U: std::fmt::Debug,
//! {
//!     #[va_expand_ref(counter: usize)]
//!             // Move `counter` in, `counter` is 0.
//!             // Since `usize` implements `Copy`,
//!             // it is a *copy* of the original `counter`.
//!     {
//!         println!("{counter}: {:?}", others);
//!         *counter += 1;
//!             // `counter` here is actually `&mut usize`,
//!             //  a mutable reference to *the copied* `counter`,
//!             //  so the original `counter` will NOT be modified.
//!     }
//!     #[va_expand_mut]
//!     {
//!         others.take();
//!     }
//!     #[va_expand(counter: usize, non_variadic: T)]
//!             // Move `counter` in, again, it is still `0`.
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
//! If you do not want to consume the context variables, then you should create references to them:
//!
//! ```rust
//! use variadiz::*;
//!
//! #[variadic]
//! fn collect<T, U>(mut init: Vec<T>, others: Option<U>) -> Vec<T>
//! where
//!     U: Into<T>,
//! {
//!     let collector = &mut init;
//!     #[va_expand(collector: &mut Vec<T>)]
//!             // One thing to note is,
//!             // `collector` is actually `&mut &mut Vec<T>`.
//!     {
//!         others.map(|item| collector.push(item.into()));
//!     }
//!     init
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
//! # Call variadic function
//!
//! It is easy to see from the above example that you should pack the variadic arguments into [`va_args!`] macro
//! and pass them as a single argument.
//!
//! The [`va_args!`] macro accepts any number of expressions. Sometimes you may want to annotate the type
//! of each argument, you can use the [`va_types!`] macro:
//!
//! ```
//! # use variadiz::*;
//! #
//! # #[variadic]
//! # fn print<T, U>(counter: usize, non_variadic: T, others: Option<U>)
//! # where
//! #     T: std::fmt::Display,
//! #     U: std::fmt::Debug,
//! # {
//! #     #[va_expand_ref(counter: usize)]
//! #     {
//! #         println!("{counter}: {:?}", others);
//! #         *counter += 1;
//! #     }
//! #     #[va_expand_mut]
//! #     {
//! #         others.take();
//! #     }
//! #     #[va_expand(counter: usize, non_variadic: T)]
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

extern crate self as variadiz;
