# Typr

A superset of the legendary R !

The project is still a prototype in progress and is really buggy. The syntax and the wanted features are there so there won't be some big change but additional features.

![](images/TypR_logo.png)

## Installation

To install TypR, you will need to install `Rust` and `Prolog`:

- R's installation page: https://www.r-project.org/
- Rust's installation page: https://www.rust-lang.org/
- Prolog's installation page: https://www.swi-prolog.org/download/stable

You should be sure those tools are installed and accessible through the terminal.

### Installation

After that, you just need to install the executable:

```bash
cargo install typr
```

And you're good to go.

## Usage

Actually, the executable of TypR can only type-check the targeted file. The prefered file extension is `.ty`. For instance, if you want to execute the `app.ty` file you just need this command:

```bash
typr app.ty
```

## Documentation (minimal)

TypR add great static types and a flexible syntax with some cool tricks (metaprogramming) that make him great to work with

### Types

*Basic types:*

- int
- num
- chars
- bool

Structural types:

- records
- tuples (kinds of records)
- arrays
- functions
- unions
- interfaces

### Main functionalities

- Generics
- type checking for array's shape (dependant types)
- uniform function call
- operator overloading
- type embedding
- interface inference

### Functional programming with TypR

### Object oriented programming with TypR


