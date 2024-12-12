# Typr

A superset of the legendary R !

The project is still a prototype in progress and is really buggy. The syntax and the wanted features are there so there won't be some big change but additional features.

![](images/TypR_logo.png)

There is also a more mature project named vapour written in go who has some cool features: https://vapour.run/

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

- First presentation video in french : https://www.youtube.com/watch?v=5fWRaAPeJBs  
- The next one is coming this friday 13 december 2024 !

### Philosophy: flexibillity and type safety

#### What is TypR 

TypR is a word game with Typescript (a super set of javascript) and the common way of noming things in the R community. The initial goal is to create a better experience with building Packages for R (I want them to be easily compatible with the CRAN's requirements to be easy to ship). Indeed, TypR *is not only a type checker* but bring greater tool to build packages for datascience in genral and want to be an easy way to convert research paper into code. TypR add great static types and a flexible syntax with some cool tricks (metaprogramming) that make him great to work with

#### What TypR is not

Although R looks like the next cool kid in the area with a syntax greatly inspired by R and Rust, and many interesting feature from Go, Nim, Roc. It doesn't try to replace them at all but tend to help package builder and manager to reach their goals. 

TypR is not fundamentally OOP. Like R who is more a functional programming language, TypR follow this path for good reasons. Firstly because the realm of datascience is flooded with programming languages who are more on the Object oriented programming side who has his own strength but his own weaknesses. Especially because it has his own limits in element representation (uniquely done with OOP) and strange design pattern. Functional programming offer a bit more high level representation with less headache and the power of creating easy pipelines and parallelizable code.

### Transpilation process

To explain TypR in an other way, it's a core calculus with quite a bit of syntax sugar.

![](images/typr_transpilation_type_checking_process.png)

The transpilation process must pass by a first parsing stage then a modification of the abstract syntax tree to obtain through the process of metaprogramming. This is a fancy word for syntax sugar. It's a way to transform a langage and take some shortcuts to make it easier to work with. 

### First code

Let's build our first exemple! You can create a file named `app.ty` (or the name you want). TypR has a generalized syntax for building type. To create a numeric, you just have to write:

```scala
let a: num <- 5.0;

a
```

You can now run it with this command:

```bash
typr app.ty
```

You will get this result:

```
Type checking: 
num

Execution: 
[1] 5
```

As you can see, typR display two things. The first is the result of the type checking. TypR know that you defined a numeric so the expression `a` evaluate to the type `num` (=numeric).

The second thing displayed is the evaluation of the value of the variable `a`. Since we created it with the value `5` it take it as it is.

The `typr` binary created two files to do this task. They exist in the current directory and are respectively named `adt.pl` and `app.R`. `adt.pl` is the file (prolog file) containing the information about the code and the related types. TypR is able to reason about types with it and give use the type checking we previousely saw. The `app.R` is the file with the type annotation removed (but TypR don't only do that):

```R
# [other prebuild stuffs to work with typR]
# ...

n <- 5
n
```

You don't need to pay attention to the first lines of the document but to the last two lines. As you see, it's almost the same thing as the `app.ty` document with the exception of some element removed (the "let", and the type annotation). Of course TypR is not just a R with types but bring other great construct from metaprogramming that will also bring a simpler syntax in TypR that will build some code for you in the R's side. You can play with it and see by yourself.


### Types

Even though TypR try to be the closest possible to the type system of R, it also take it's own route for certain things and do the translation for R. For now, not all basic R data types are represented but TypR has his own representation.

*Basic types:*

Each basic type will give a vector of size 1 in R. Boleans are now the two lower case values `true` and `false` instead of the uppercase one (TRUE, FALSE or T, F).

| name       | TypR  | R       |
|------------|-------|---------|
| Integers   | int   | integer |
| Numerics   | num   | numeric |
| Characters | chars | bool    |
| Booleans   | bool  | logical |

```scala
let a: int = 5;
let b: num = 5.0; 
let c: chars = "5";
let d: bool = true;
```

Structural types:

| name                  | TypR                       | R               |
|-----------------------|----------------------------|-----------------|
| Arrays                | ([[index], [type]])        | vector + arrays |
| Records               | ({[[name]: [type]]*})      | lists           |
| Tuples (like records) | ([type]*)                  | lists           |
| Functions             | (([type]*) -> type)        | functions       |
| Unions (of Tags)      | type [\| type]+            | -               |
| Interfaces            | interface {a bit too much} | -               |
| Tags                  | [name]\(type)              | NA, NaN, lists? |

#### Arrays

One can build an array from any types. Array keep the informations about the array size and it's type. As any other types, you don't need mention the type annotation. TypR can infer it for you.

```scala
let a: [3, bool] = [true, false, true];
let b = [true, false, true];
```

Here `a` and `b` have both the type `[3, bool]` with and without the type inference. 

We can also define multidimensional arrays in that way:

```scala
[[1, 2], [3, 4]]
```

This code will give you this result:

```
Type checking: 
[2, [2, int]]

Execution: 
     [,1] [,2]
[1,]    1    3
[2,]    2    4
```

The array type from TypR are recursive by definition. This mean an array `[I, T]` is the combination of an index `I` and a type `T`, so `T` can be any type including an other array type `[J, T]` so we can end with an array `[I, [J, T]]`. Since R doesn't support this feature, TypR is smart enougth to transform it to a classical Array type in R.

You can also add as much layer as you want (but not realy sure if it's that readable). You can create a tensor of dimension 3 (an array of arrays of arrays) in this fashion:

```scala
[[[1, 2], [3, 4]], [[5, 6], [7, 8]]]
```

I will let you see the result by yourself. But be sure to respect the dimensionality or else it will not work.

*Sequences*

Like R, TypR let you define sequence of elements. For instance:

```scala
1:4
```

Will give you this result:

```
Type checking: 
[4, int]

Execution: 
[1] 1 2 3 4
```

TypR also add the feature of defining steps:

```scala
0:2:20
```

Just mean "start from zero by step of two until we reach 20". It will give this result:

```
Type checking: 
[11, int]

Execution: 
[1]  0  2  4  6  8 10 12 14 16 18 20
```

TypR offer capabilities to act on array indexing and build functions that define the shape an array should have. It's a powerful tool let typR infer and check the shape of a multidimensional array like a matrix. For instance, sequences generated are juste converted to the `seq` function in R. TypR has this definition for it:

```scala
pub let seq <- fn(a: #I, b: #J, c: #K): [#J-#I/#K+1, int] {
	...
};
```

You dont need to understand everything. This is just a function that take 3 parameters (#I for the start number, #J for the end number and #K for the step) The resulting index is a calculation (#J-#I/#K+1) that TypR can use to guess the shape of the resulting array. This type of generics are `index generics`, those are like super ints who have the power to be used in arrays' index. It give the power to define the resulting shape of complex operations like transpose or the dot product. A tutorial about this topic will come soon (since it can be a whole chapter by imself).


### Main functionalities

- Generics
- Type checking for array's shape (dependant types)
- Uniform function call
- Operator overloading
- Type embedding
- Interface inference

### Functional programming with TypR

### Object oriented programming with TypR


