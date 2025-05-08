# Typr

## What is TypR ?

It's not only R with types. It's a faster, safer, cooler R that can transpile to `Typescript`/`Javascript`/`WebAssembly`.

TypR is a superset of the legendary R. It add cool types, a beautiful syntax and powerful modern features.

The project is still new and have bugs (that will be fixed soon). All the syntax basis and the features are arleady there so there won't be some big breaking change but a refinement of the core elements (and bug fix).

![](images/TypR_logo.png)

There is also a more mature project named vapour written in go who has some cool features: https://vapour.run/

## Installation

## For R (basic)
To install TypR, you will need to install `Rust` (of course you should have R installed in your system):

- R's installation page: https://www.r-project.org/
- Rust's installation page: https://www.rust-lang.org/

You should be sure those tools are installed and accessible through the terminal.

## For Typescript/Javascript/Wasm
It's not complet yet, but you can transpile TypR code to Typescript/Javascript/Wasm using node.

- node: https://nodejs.org/en 

### Installation

After that, you just need to install the executable:

```bash
cargo install typr
```

And you're good to go.

## Usage

Actually, the executable of TypR can:
- Type check the code
- Generate the target code (mainly R actually)
- Create project folder

The prefered file extension is `.ty`. It has not it's own syntax highliter yet but I recommend you to use use the one from Scala. For instance, if you want to execute the `app.ty` file you just need this command:

```bash
typr app.ty
```

## Documentation (minimal)

- First presentation video in French (subtitles in English): https://www.youtube.com/watch?v=5fWRaAPeJBs  
- Second presentation video in French (subtitle in English): https://www.youtube.com/watch?v=vUf31KiV3J4

### Philosophy: flexibillity and type safety

#### What is TypR 

"TypR" is a word game with Typescript (a superset of JavaScript) and the common way of naming things in the R community. The initial goal is to create a better experience with building Packages for R (I want them to be easily compatible with the CRAN's requirements and to be easy to ship). Indeed, TypR *is not only a type checker* but bring greater tools to build packages for data science in general and want to be an easy way to convert research paper into code. TypR add great static types and a flexible syntax with some cool tricks (metaprogramming) that make it great to work with.

#### What TypR is not

Although TypR looks like the next cool kid in the town with a syntax greatly inspired by R and Rust, and many interesting feature from Go, Nim and Roc. It doesn't try to replace them at all but tend to help package builder and manager to reach their goals. This will help many developper, to bring value into the R community and the datascience community in a broader scope. 

Even though it follow it's principles. TypR is not fundamentally OOP. Like R who is more of a functional programming language, TypR follow this path for good reasons. Firstly because the realm of datascience is flooded with programming languages who are more on the Object oriented programming side. Don't get me wrong, it has its own strength but its own weaknesses too. Especially because it has his own limits in element representation (uniquely done with OOP) and strange design pattern. Functional programming offer a bit more high level representation with less headache and the power of creating easy pipelines and parallelizable code.

### Transpilation process

To explain TypR in an other way, it's a core calculus with quite a bit of syntax sugar.

![](images/transpiler_3_lang.png)

To build the corresponding code for each target language, TypR use a context based on the parsing of the syntax and the typechecking of it's construct to build enough knowledge to make inference and simplify the work of developpers. For R, it use the S3 object oriented system underneath. For Typescript/Javascript and Assemblyscript/Wasm it use monomorphization. TypR needs to transpile to Typescript before creating an equivalent javascript file. It also needs to transpile to Assemblyscript before creating wasm files.

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
# [some other types]
```

As you can see, typR display two things. The first is the result of the type checking. TypR know that you defined a numeric so the expression `a` evaluate to the type `num` (=numeric).

The second thing displayed is the evaluation of the value of the variable `a`. Since we created it with the value `5` it take it as it is.

The `typr` binary created two files to do this task. They exist in the current directory and are respectively named `adt.pl` and `app.R`. `adt.pl` is the file (prolog file) containing the information about the code and the related types. TypR is able to reason about types with it and gives us the type checking we previously saw. The `app.R` is the file with the type annotation removed (but TypR don't only do that):

```R
# [other prebuild stuffs to work with typR]
# ...

n <- 5
n
```

You don't need to pay attention to the first lines of the document but to the last two lines. As you see, it's almost the same thing as the `app.ty` document with the exception of some element removed (the "let", and the type annotation). Of course TypR is not just R with types but it brings other great constructs from metaprogramming that will also bring a simpler syntax in TypR. It will build some code for you on the R's side. You can play with it and see it for yourself.

### Types

Even though TypR try to be the closest possible to the type system of R, it also takes its own route for certain things and do the translation of R. For now, not all basic R data types are represented but TypR has its own representation.

*Basic types:*

Each basic type gives a vector of size 1 in R. Boleans are now the two lower-case values `true` and `false` instead of the uppercase one (TRUE, FALSE or T, F).

| name       | TypR  | R       |
|------------|-------|---------|
| Integers   | int   | integer |
| Numerics   | num   | numeric |
| Characters | char | bool    |
| Booleans   | bool  | logical |

```scala
let a: int = 5;
let b: num = 5.0; 
let c: char = "5";
let d: bool = true;

a
```

Structural types:

| name                  | TypR                       | R               |
|-----------------------|----------------------------|-----------------|
| Arrays                | ([[index], [type]])        | vector + arrays |
| Records               | ({[[name]: [type]]*})      | lists           |
| Tuples (like records) | ([type]*)                  | lists           |
| Functions             | (([type]*) -> type)        | functions       |
| Tags                  | .[name]\(type)             | NA, NaN, lists? |
| Unions (of Tags)      | type [\| type]+            | -               |
| Interfaces            | interface {a bit too much} | -               |

#### Arrays

One can build an array from any types. Array keep the informations about the array size and it's type. As any other types, you don't need mention the type annotation. TypR can infer it for you.

*Difference with R*
TypR's array can only hold one type. It mean, all the member of the array must have the same type.

*Exemples*

```scala
let a: [3, bool] = [true, false, true];
let b = [true, false, true];

a
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

At this stage, you dont need to understand everything. This is just a function that take 3 parameters (#I for the start number, #J for the end number and #K for the step) The resulting index is a calculation (#J-#I/#K+1) that TypR can use to guess the shape of the resulting array. This type of generics are `index generics`, those are like super ints who have the power to be used in arrays' index. It give the power to define the resulting shape of complex operations like transpose or the dot product. A tutorial about this topic will come soon (since it can be a whole chapter by imself).

#### Records

A record is a structure that hold different type of data. It's the equivalent of a named list in R. It has also his owns capability like the row polymorphism.

*Difference with R*  
TypR records are a subtype of R lists. You can't build a records with unlabeled values like in R. This restriction prevent unsafe and unpredictible operations to occure.

*Exemples*
To build a simple record representing a person with their name and age, you can just write:

```scala
:{name: "John", age: 19}
```

Will generate:

```
Type checking: 
{name: char, age: int}

Execution: 
$name
[1] "John"

$age
[1] 19
```

To make a distinction with a scope and avoid long names like "list" or "record" for each object creation, We thought this notation will be easier to work with.

To access a member by it's label, you just have to call it in this fashion:

```scala
let person = :{name: "John", age: 19};

person.name
```

Here, we accessed the name of the person. Records are mainly there to keep together a set of data in a logical way

#### Tuples

R tuples are a specific case of records. Indeed, they are just records who automaticaly generate numered labels. Here:

```scala
:{"John", 19}
```

Will generate:

```
Type checking: 
{char, int}

Execution: 
[[1]]
[1] "John"

[[2]]
[1] 19
```

It's a faster and easier way to generate data on the fly.

#### Functions

Since TypR is more oriented toward a functional style, functions are values and have a type by themself. They are then anonymous by default and should be set in variables.

*Difference with R*
There aren't that much difference with R except function are created with the `fn` keyword instead of the `function` one. Also `return` is not a function anymore but a keyword.

You can define a function with a type annotation but it's better to focus only on the function signature and let TypR infer the rest.

```scala
let f <- fn(a: int, b: int): bool {
	a == b
};

f(8, 9) # will give false
```

TypR functions are the most complex elements of TypR since many action (metaprogramming + type checking) must be done in the calling. But a lot of sugar has been added so everyone can use them seemlessely.

#### Tags

Tags are one of the algebraic data type (no need to know what an algebraic data type is yet) I needed the most but didn't know. I was asking myself wich kind of union type of union I should use (general union or tagged union), but those are the elements that bring at the same type the security and flexibility wanted for this language.


*Difference with R*

R don't have such a construct since, Tags, unions and even interface are an abstract concept to put some clarity and restriction to what the code should do. But compared to the others, tags are a kind of values and should exist in real R code. I have not made the implementation of the translation to R yet, but I think I will make

Tags are values that one can use on the fly. Each flag is unique and has its own type. It's useful do define some elements like:

```scala
let none: .None = None;
let nan: .NaN = NaN;
le na: .NA = NA;

none
```

If the tag is named `[tag_name]`, then it's base type is `:[tag_name]`. They are R's factors on steroïd and even Rust's enums on steroïd. You can use them to define some collections (like the Day of the week, gender, etc.).

Tags can also handle one type with them:

```scala
let results: .Val(num) = Val(7.3);
let person: .Person({name: char, age: int}) = Person(:{name: "Marc", age: 37});

results
```

Okay but what are their power ? Well they can be unified together inside a union type ! But here we will see how useful it is for return type in a if close:

```scala
if (true) {
	7
} else {
	"seven"
}
```

Here, TypR wont accept this code since `7` (`int`) and `"seven"` (`char`) aren't the same type. But if we use tags:

```scala
if (true) {
	Value(7)
} else {
	String("seven")
}
```

The return type will be `.Value(int) | .String(char)` meaning TypR will automaticaly unify the results if they are tags. Tags must be "unwrapped" to access the values within, forcing a user to handle the different cases.

#### Union

Union are an abstract concept that won't really appear in the resulting R code. In summary, you can only unify tags. You can now regroup tags to create other type. For instance, to create an option type, one can create an alias into this union of values:


*Difference with R*
There is no concept of union in R so nothing to compare.

```R
type Option<T> = .Some(T) | .None;
```

And if my function can return a `Na` and a `NaN` instead of an int value you can define your own type:

```R
type Failable<T> = .Value(T) | .NaN | .Na | .None;
```

This method is more flexible than an enum like Rust and more secure than an union from TypeScript.

#### Interfaces [still buggy]

TypR interfaces works like Go's interfaces. It's a typed way of doing duck typing: If it walks like a duck and quacks like a duck, then it's a duck. So there is no implicit implémentation of an existing interface.

*Difference with R*
There is no concept of union in R so nothing to compare.

You can define an interface in this fashion:

```scala
type Addable = interface {
	add: fn(a: Self, b: Self): Self,
};
```

You can then define a function that take any addable type in this way:

```scala
let time3 <- fn(n: Addable) {
	add(n, (add(n, n))
};
```

The type int already had the function add that respect the interface `Addable`, so it will be immediatly accepter to pass an int in this fuction in different ways:

```scala
time3(5)
(5).time3()
5 |> time3()
```

### Main functionalities

- Uniform function call
- Operator overloading
- Generics + Index Generics
- Type embedding
- Interface inference
- Row polymorphism

#### Uniform function call

UFC (uniform function call) is one of my favourite features in programming language. I love the simplicity of methods, but I am not a fan of classes anymore since they are less expressive than algebraic data types in general. But working with my functions using types and module and pipes isn't totaly complet without methods call. So UFC bring the best of both worlds to me. I have also added some other forms with the original.

For instance, we can see the definition of the add function for interger types:

```scala
let incr <- fn(a: int): int {
	a + 1
};
```

Now we can call it in different ways that gives the same results:

```scala
incr(2)
(2).incr()
2 |> incr()
```

There are the special elements `..` or `|>>` that make a function operate on an array level.

```scala
[1, 2, 3]..incr()
[1, 2, 3] |>> incr()
```

This code will apply an addition with 3 for each element.

#### Operator overloading

Operator overloading is one of my favourite element to build operations on types. It's a shortcut and a syntax sugar that let the user define operations for their custom types.

Imagine we create a type named point:

```scala
type Point = {x: int, y: int};

let p1 = :{x: 2, y: 1};

p1
```

You can define a function `add` to add two points. Let's assume you just add each coordinates.

```scala
let add <- fn(p1: Point, p2: Point): Point {
	:{x: ((p.x) + (q.x)), y: ((p.y) + (q.y))}
};
```

You can obviously use the function in differtent ways:

```scala
add(p1, p1)
p1.add(p1)
p1 |> add(p1)
```

But you can also add point in this fashion:

```scala
p1 + p1
```

Why is it possible ? Because `add` is a reserved symbol related to the binary operation `+`. So each time TypR see a `a+b`, it transform it to `add(a, b)`.

There are a group of reserved operators that change to their function's form.

| operator | function name |
|----------|---------------|
| +        | add           |
| ++       | add2          |
| -        | minus         |
| --       | minus2        |
| *        | mul           |
| **       | mul2          |
| /        | div           |
| //       | div2          |
| @        | at            |
| @@       | at2           |

You don't have to worry about the usage of if an operator is related to an other type. Each type can use each symbol once.

#### Modules

Modules are a useful tool that allow us to use the same name but in a different scope.

```scala
module Foo {
	pub let my_var <- "hey";
};

module Bar {
	pub let my_var <- true;
};

let my_var <- 7;

Foo::my_var
Bar::my_var
my_var
```

In this exemple, my_var is defined in 3 different place with a different value and even a different type each time. But if `my_var` is defined in a module like in the `Foo` or `Bar` module then we need to prefix the module name to have access to this variable.

In OOP language we think in term of class and object, in language with a functional programming paradim orientation like TypR, we think in term of types. A type can be refered at any moment in the program and function that works with this type can also be declared at any moment in the program. This is why module exists: if needed, we can group types and their related functions in the same place and give it a name for reference.

```scala
module Cat {
	pub type Cat = {name: char, age: int};
	
	pub let cry <- fn(c: Cat): char {
		"meow"
	};
};

let felix <- :{name: "Felix", age: 8};
Cat::cry(felix)
```

#### Generics + Index Generics

Generics is one of the hardest concept in this language, especially when we talk about Index Generics.

In complex terms, generics is a way to create functions that allows functions to work with more types related to specific relationship with parameters (Parametric polymorphism).

Since it allow almost any type, it's better to use them for structural and general purpose. For instance, it's great to describe data structures that can works with many types (like array, graphs, tree, etc.). It's also greate to use it to shape function composition.

Index Generics take the 


- Generics + Index Generics
- Type embedding
- Interface inference
- Row polymorphism

### Functional programming with TypR

### Object oriented programming with TypR

