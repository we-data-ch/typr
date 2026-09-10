# TypR Standard Library

> Auto-generated from `configs/std/*.ty` by `typr std doc --format md`.
> Compiled signatures (T1) are verified by the type-checker.

**386** documented entities (368 with structured metadata).

## `_typR` package  
- **`Option`** `.Some(T) | .None`
- **`unwrap`** `(value: Option<T>) -> T`
- **`expect`** `(value: Option<T>, msg: char) -> T`
- **`unwrap_or`** `(value: Option<T>, alternative: T) -> T`
- **`is_some`** `(value: Option<T>) -> bool`
- **`is_none`** `(value: Option<T>) -> bool`
- **`Plot`** `{ ann: bool, axes: bool, kind: char, log: char, main: char, sub: char, x: [#N, num], xlab: char, xlim: [2, num], y: [#N, num], ylab: char, ylim: [2, num] }`
- **`bplot`** `Plot`
- **`lvec`** `(a: [#M, T]) -> [1, [#M, T]]`
- **`cvec`** `(a: [#M, T]) -> [#M, [int, T]]`
- **`System2`** `{ args: [#N, char], command: char, stderr: char, stdin: char, stdout: char }`
- **`Factor`** `= int`
- **`factor`** `(a: char, b: [#N, char]) -> Factor<[#N, char]>`
- **`State`** `= any`
- **`state`** `(a: T) -> State<T>`
- **`Foreign`** `= any`
- **`Eq`** `= interface{ eq: fn(a: SELF, b: SELF) -> bool }`
- **`Ord`** `= interface{ compare: fn(a: SELF, b: SELF) -> int }`

## `base` package  
- `T1` **`+`** `(a: int, b: int) -> int` — element-wise sum of two vectors
  ```
  c(1,2) + c(3,4)   # -> c(4,6)
  ```
- `T1` **`+`** `(a: num, b: num) -> num` — element-wise sum of two vectors
  ```
  c(1,2) + c(3,4)   # -> c(4,6)
  ```
- `T1` **`-`** `(a: int, b: int) -> int` — numeric difference
  ```
  5.0 - 3.0   # -> 2
  ```
- `T1` **`-`** `(a: num, b: num) -> num` — numeric difference
  ```
  5.0 - 3.0   # -> 2
  ```
- `T1` **`/`** `(a: int, b: int) -> int` — numeric quotient
  ```
  10.0 / 3.0   # -> 3.333...
  ```
- `T1` **`/`** `(a: num, b: num) -> num` — numeric quotient
  ```
  10.0 / 3.0   # -> 3.333...
  ```
- `T1` **`*`** `(a: int, b: int) -> int` — numeric product
  ```
  3.0 * 4.0   # -> 12
  ```
- `T1` **`*`** `(a: num, b: num) -> num` — numeric product
  ```
  3.0 * 4.0   # -> 12
  ```
- `T1` **`%%`** `(a: int, b: int) -> int` — numeric modulo
  ```
  7.0 %% 3.0   # -> 1
  ```
- `T1` **`%%`** `(a: num, b: num) -> num` — numeric modulo
  ```
  7.0 %% 3.0   # -> 1
  ```
- `T1` **`&&`** `(a: bool, b: bool) -> bool` — logical AND
  ```
  TRUE && FALSE   # -> FALSE
  ```
- `T1` **`||`** `(a: bool, b: bool) -> bool` — logical OR
  ```
  TRUE || FALSE   # -> TRUE
  ```
- `T1` **`+`** `(a: Vec[#M, T], b: Vec[#M, T]) -> Vec[#M, T]` — element-wise sum of two vectors
  ```
  c(1,2) + c(3,4)   # -> c(4,6)
  ```
- `T1` **`reduce`** `(a: [#N, T], b: (T, T) -> T) -> T` — single value after left-fold
  ```
  reduce(c(1,2,3,4), add)   # -> 10
  ```
- `T1` **`fold`** `(a: [#N, T], b: U, c: (U, T) -> U) -> U` — final accumulator
  ```
  fold(c(1,2,3), 0, add)   # -> 6
  ```
- `T1` **`extend`** `(a: [#N, T], b: T) -> [#N, T]` — new vector with b appended
  ```
  extend(c(1,2), 3)   # -> c(1,2,3)
  ```
- `T3` **`test_that`** `(a: char, b: any) -> Empty` — invisibly NULL
- `T3` **`expect_true`** `(a: bool) -> Empty` — invisibly NULL
- `T3` **`expect_false`** `(a: T, b: T) -> Empty` — invisibly NULL
- `T3` **`expect_equal`** `(a: T, b: T) -> Empty` — invisibly NULL
- `T2` **`cat`** `(...: any) -> Empty` — invisibly NULL
  ```
  cat("hello", "world")
  ```
- `T1` **`to_native`** `(a: T) -> T` — same value as native R
- `T2` **`from_native`** `(a: any, b: char) -> any` — coerced value
- `T1` **`from_int`** `(a: any) -> int` — integer representation
  ```
  from_int(3.7)   # -> 3L
  ```
- `T1` **`from_num`** `(a: any) -> num` — numeric representation
  ```
  from_num(3L)   # -> 3.0
  ```
- `T1` **`from_char`** `(a: any) -> char` — character representation
  ```
  from_char(42)   # -> "42"
  ```
- `T1` **`from_bool`** `(a: any) -> bool` — logical representation
  ```
  from_bool(1)   # -> TRUE
  ```
- `T1` **`sum`** `(a: [#N, T]) -> T` — sum over all elements
  ```
  sum(c(1, 2, 3))   # -> 6
  ```
- `T1` **`print`** `(a: any) -> Empty` — prints to the console, returns nothing
  ```
  print(sum(c(1, 2, 3)))
  ```
- `T1` **`source`** `(a: char) -> Empty` — invisibly NULL
  ```
  source("utils.R")
  ```
- `T1` **`as__character`** `(a: A) -> char` — character representation
  ```
  as__character(42)   # -> "42"
  ```
- `T1` **`as_vec`** `(a: [#N, T]) -> [#N, T]` — same vector unchanged
  ```
  as_vec(c(1,2,3))   # -> c(1,2,3)
  ```
- `T1` **`map`** `(a: [#N, T], b: (T) -> U) -> [#N, U]` — transformed vector with same length
  ```
  map(c(1,2,3), add(1))   # -> c(2,3,4)
  ```
- `T1` **`filter`** `(a: [#N, T], b: (T) -> bool) -> [#N, T]` — elements where predicate is TRUE
  ```
  filter(c(1,2,3,4), is_even)   # -> c(2,4)
  ```
- `T1` **`set_at`** `(a: [#N, T], b: int, c: T) -> [#N, T]` — new vector with element at i replaced
  ```
  set_at(c(1,2,3), 2, 99)   # -> c(1,99,3)
  ```
- `T1` **`add`** `(a: int, b: int) -> int` — numeric sum
  ```
  1.5 + 2.5   # -> 4
  ```
- `T1` **`add`** `(a: num, b: num) -> num` — numeric sum
  ```
  1.5 + 2.5   # -> 4
  ```
- `T1` **`minus`** `(a: int, b: int) -> int` — numeric difference
  ```
  5.0 - 3.0   # -> 2
  ```
- `T1` **`minus`** `(a: num, b: num) -> num` — numeric difference
  ```
  5.0 - 3.0   # -> 2
  ```
- `T1` **`mul`** `(a: int, b: int) -> int` — numeric product
  ```
  3.0 * 4.0   # -> 12
  ```
- `T1` **`mul`** `(a: num, b: num) -> num` — numeric product
  ```
  3.0 * 4.0   # -> 12
  ```
- `T1` **`div`** `(a: int, b: int) -> int` — numeric quotient
  ```
  10.0 / 3.0   # -> 3.333...
  ```
- `T1` **`div`** `(a: num, b: num) -> num` — numeric quotient
  ```
  10.0 / 3.0   # -> 3.333...
  ```
- `T1` **`get`** `(a: any, b: char) -> T` — extracted value
  ```
  get(list(x=1), "x")   # -> 1
  ```
- `T1` **`seq`** `(a: #I, b: #J, c: #K) -> Vec[#J-#I/#K+int, int]` — integer sequence from a to b by c
  ```
  seq(1L, 5L, 1L)   # -> c(1,2,3,4,5)
  ```
- `T1` **`join`** `(a: [#N, char], b: char) -> char` — single concatenated string
  ```
  join(c("a","b","c"), ",")   # -> "a,b,c"
  ```
- `T1` **`startsWith`** `(a: char, b: char) -> bool` — TRUE if a starts with b
  ```
  startsWith("hello", "hel")   # -> TRUE
  ```
- `T1` **`endsWith`** `(a: char, b: char) -> bool` — TRUE if a ends with b
  ```
  endsWith("hello", "llo")   # -> TRUE
  ```
- `T1` **`contains`** `(a: char, b: char) -> bool` — TRUE if a is found in b
  ```
  contains("hello", "ell")   # -> TRUE
  ```
- `T1` **`getwd`** `() -> char` — current working directory path
  ```
  getwd()   # -> "/home/user"
  ```
- `T1` **`setwd`** `(a: char) -> char` — previous working directory
  ```
  setwd("/tmp")
  ```
- `T1` **`dir`** `() -> [#N, char]` — vector of files in current directory
  ```
  dir()
  ```
- `T1` **`list__files`** `() -> [#N, char]` — vector of files in current directory
  ```
  list__files()
  ```
- `T1` **`file__exists`** `(a: char) -> bool` — TRUE if file exists
  ```
  file__exists("README.md")   # -> TRUE
  ```
- `T1` **`file__create`** `(a: char) -> bool` — TRUE if successful
- `T1` **`file__remove`** `(a: char) -> bool` — TRUE if successful
- `T1` **`file__rename`** `(a: char, b: char) -> bool` — TRUE if successful
  ```
  file__rename("old.txt", "new.txt")
  ```
- `T1` **`file__copy`** `(a: char, b: char) -> bool` — TRUE if successful
  ```
  file__copy("a.txt", "b.txt")
  ```
- `T1` **`dir__create`** `(a: char, b: char) -> bool` — TRUE if successful
- `T1` **`unlink`** `(a: char) -> bool` — TRUE if successful
- `T2` **`stop`** `(a: char) -> Empty` — never returns (stops execution)
  ```
  stop("something went wrong")
  ```
- `T1` **`from_nullable`** `(a: any) -> Option<any>` — Option wrapping the value
- `T1` **`to_nullable`** `(a: Option<T>) -> any` — R value (NULL for 
- `T1` **`dot`** `(a: [#M, [#P, int]], b: [#P, [#N, int]]) -> [#M, [#N, int]]` — matrix product [#M x #N]
  ```
  dot([[1,2],[3,4]], [[5,6],[7,8]])   # -> [[19,22],[43,50]]
  ```
- `T1` **`t`** `(a: [#M, [#N, T]]) -> [#N, [#M, T]]` — transposed matrix [#N x #M]
  ```
  t([[1,2],[3,4]])   # -> [[1,3],[2,4]]
  ```
- `T2` **`system2`** `(a: char, b: [#N, char], c: char, d: char, e: char) -> char` — command output as character
  ```
  system2("echo", c("hello"))
  ```
- `T2` **`bsystem2`** `(a: char) -> System2` — System2 struct for execution
- `T2` **`exec`** `(a: System2) -> char` — command output
- `T1` **`annotate_factor`** `(a: int, b: [#N, char]) -> Factor<[#N, char]>` — Factor wrapping the integer index
- `T1` **`as__factor`** `(a: any, b: [#N, char]) -> Factor<[#N, char]>` — Factor from any value
- `T1` **`levels`** `(a: Factor<L>) -> L` — levels vector
  ```
  levels(factor("low", c("low","high")))   # -> c("low","high")
  ```
- `T1` **`nlevels`** `(a: Factor<L>) -> int` — number of levels
  ```
  nlevels(factor("a", c("a","b","c")))   # -> 3
  ```
- `T1` **`as__character`** `(a: Factor<L>) -> char` — character representation
  ```
  as__character(42)   # -> "42"
  ```
- `T1` **`get`** `(a: State<T>) -> T` — extracted value
  ```
  get(list(x=1), "x")   # -> 1
  ```
- `T1` **`set`** `(a: State<T>, b: T) -> Empty` — NULL (invisibly)
- `T1` **`update`** `(a: State<T>, b: (T) -> T) -> Empty` — NULL (invisibly)
- `T1` **`version`** `(a: State<T>) -> int` — version counter (incremented on each mutation)
- `T1` **`map`** `(a: State<T>, b: (T) -> U) -> State<U>` — transformed vector with same length
  ```
  map(c(1,2,3), add(1))   # -> c(2,3,4)
  ```
- `T1` **`derive`** `(a: State<T>, b: (T) -> T) -> State<T>` — new State with f applied to current value
- `T1` **`unique`** `(a: [#N, Eq]) -> [#N, Eq]` — vector with duplicates removed
  ```
  unique(c(1,2,2,3,3,3))   # -> c(1,2,3)
  ```
- `T1` **`sort`** `(a: [#N, Ord]) -> [#N, Ord]` — sorted vector
  ```
  sort(c(3,1,4,1,5))   # -> c(1,1,3,4,5)
  ```
- `T1` **`abs`** `(a: num) -> num` — absolute value
  ```
  abs(-5)   # -> 5
  ```
  See also: `sign`
- `T1` **`sqrt`** `(a: num) -> num` — square root
  ```
  sqrt(9)   # -> 3
  ```
  See also: `^`, `exp`
- `T1` **`log`** `(a: num, b: num) -> num` — logarithm of x
  ```
  log(100)   # -> 4.6051...
  ```
  See also: `log2`, `log10`, `log1p`, `exp`
- `T1` **`log2`** `(a: num) -> num` — base-2 logarithm
  ```
  log2(8)   # -> 3
  ```
  See also: `log`, `log10`
- `T1` **`log10`** `(a: num) -> num` — base-10 logarithm
  ```
  log10(100)   # -> 2
  ```
  See also: `log`, `log2`
- `T1` **`exp`** `(a: num) -> num` — e^x
  ```
  exp(0)   # -> 1
  ```
  See also: `log`
- `T1` **`expm1`** `(a: num) -> num` — e^x - 1
  ```
  expm1(1e-10)   # -> 1e-10 (approximately)
  ```
  See also: `exp`, `log1p`
- `T1` **`log1p`** `(a: num) -> num` — log(1 + x)
  ```
  log1p(1e-10)   # -> 1e-10 (approximately)
  ```
  See also: `log`, `expm1`
- `T1` **`ceiling`** `(a: num) -> int` — ceiling (round up to nearest integer)
  ```
  ceiling(2.3)   # -> 3
  ```
  See also: `floor`, `round`, `trunc`
- `T1` **`floor`** `(a: num) -> int` — floor (round down to nearest integer)
  ```
  floor(2.7)   # -> 2
  ```
  See also: `ceiling`, `round`, `trunc`
- `T1` **`round`** `(a: num, b: int) -> num` — rounded value
  ```
  round(2.5)   # -> 2 (banker's rounding)
  ```
  See also: `ceiling`, `floor`, `trunc`, `signif`
- `T1` **`trunc`** `(a: num) -> int` — truncated toward zero (integer part)
  ```
  trunc(2.7)   # -> 2
  ```
  See also: `ceiling`, `floor`, `round`
- `T1` **`sign`** `(a: num) -> int` — sign of x (-1, 0, or 1)
  ```
  sign(-5)   # -> -1
  ```
  See also: `abs`
- `T1` **`signif`** `(a: num, b: int) -> num` — value rounded to significant digits
  ```
  signif(12345, 3)   # -> 12300
  ```
  See also: `round`
- `T1` **`cos`** `(a: num) -> num` — cosine
  ```
  cos(0)   # -> 1
  ```
  See also: `sin`, `tan`, `acos`
- `T1` **`sin`** `(a: num) -> num` — sine
  ```
  sin(0)   # -> 0
  ```
  See also: `cos`, `tan`, `asin`
- `T1` **`tan`** `(a: num) -> num` — tangent
  ```
  tan(0)   # -> 0
  ```
  See also: `cos`, `sin`, `atan`
- `T1` **`acos`** `(a: num) -> num` — arc cosine (radians)
  ```
  acos(1)   # -> 0
  ```
  See also: `asin`, `atan`, `cos`
- `T1` **`asin`** `(a: num) -> num` — arc sine (radians)
  ```
  asin(0)   # -> 0
  ```
  See also: `acos`, `atan`, `sin`
- `T1` **`atan`** `(a: num) -> num` — arc tangent (radians)
  ```
  atan(0)   # -> 0
  ```
  See also: `acos`, `asin`, `tan`
- `T1` **`cosh`** `(a: num) -> num` — hyperbolic cosine
  ```
  cosh(0)   # -> 1
  ```
  See also: `sinh`, `tanh`, `cos`
- `T1` **`sinh`** `(a: num) -> num` — hyperbolic sine
  ```
  sinh(0)   # -> 0
  ```
  See also: `cosh`, `tanh`, `sin`
- `T1` **`tanh`** `(a: num) -> num` — hyperbolic tangent
  ```
  tanh(0)   # -> 0
  ```
  See also: `cosh`, `sinh`, `tan`
- `T1` **`acosh`** `(a: num) -> num` — hyperbolic arc cosine
  ```
  acosh(1)   # -> 0
  ```
  See also: `asinh`, `atanh`, `cosh`
- `T1` **`asinh`** `(a: num) -> num` — hyperbolic arc sine
  ```
  asinh(0)   # -> 0
  ```
  See also: `acosh`, `atanh`, `sinh`
- `T1` **`atanh`** `(a: num) -> num` — hyperbolic arc tangent
  ```
  atanh(0)   # -> 0
  ```
  See also: `acosh`, `asinh`, `tanh`
- `T1` **`cospi`** `(a: num) -> num` — cosine of angle in degrees
  ```
  cospi(0)   # -> 1
  ```
  See also: `cos`, `sinpi`, `tanpi`
- `T1` **`sinpi`** `(a: num) -> num` — sine of angle in degrees
  ```
  sinpi(0)   # -> 0
  ```
  See also: `sin`, `cospi`, `tanpi`
- `T1` **`tanpi`** `(a: num) -> num` — tangent of angle in degrees
  ```
  tanpi(0)   # -> 0
  ```
  See also: `tan`, `cospi`, `sinpi`
- `T1` **`digamma`** `(a: num) -> num` — digamma function (derivative of log(gamma(x)))
  ```
  digamma(1)   # -> -0.5772... (Euler-Mascheroni constant, negated)
  ```
  See also: `trigamma`, `lgamma`, `gamma`
- `T1` **`trigamma`** `(a: num) -> num` — trigamma function (second derivative of log(gamma(x)))
  ```
  trigamma(1)   # -> 1.6449... (pi^2 / 6)
  ```
  See also: `digamma`, `lgamma`
- `T1` **`psigamma`** `(a: num, b: int) -> num` — psigamma function (polygamma)
  See also: `digamma`, `trigamma`
- `T1` **`lgamma`** `(a: num) -> num` — log of absolute value of gamma function
  ```
  lgamma(1)   # -> 0 (log(|Gamma(1)|) = log(1) = 0)
  ```
  See also: `gamma`, `digamma`
- `T1` **`gamma`** `(a: num) -> num` — gamma function value
  ```
  gamma(5)   # -> 24 (4!)
  ```
  See also: `lgamma`, `digamma`
- `T1` **`cumsum`** `(a: [#N, num]) -> [#N, num]` — cumulative sum
  ```
  cumsum(c(1, 2, 3))   # -> c(1, 3, 6)
  ```
  See also: `sum`, `cumprod`
- `T1` **`cumprod`** `(a: [#N, num]) -> [#N, num]` — cumulative product
  ```
  cumprod(c(1, 2, 3))   # -> c(1, 2, 6)
  ```
  See also: `prod`, `cumsum`
- `T1` **`cummax`** `(a: [#N, num]) -> [#N, num]` — cumulative maximum
  ```
  cummax(c(3, 1, 4, 1, 5))   # -> c(3, 3, 4, 4, 5)
  ```
  See also: `max`, `cummin`
- `T1` **`cummin`** `(a: [#N, num]) -> [#N, num]` — cumulative minimum
  ```
  cummin(c(3, 1, 4, 1, 5))   # -> c(3, 1, 1, 1, 1)
  ```
  See also: `min`, `cummax`
- `T1` **`mean`** `(a: [#N, num]) -> num` — arithmetic mean
  ```
  mean(c(1, 2, 3, 4))   # -> 2.5
  ```
  See also: `median`, `var`, `sd`
- `T1` **`var`** `(a: [#N, num], b: bool) -> num` — sample variance (n-1 denominator)
  ```
  var(c(1, 2, 3, 4, 5))   # -> 2.5
  ```
  See also: `sd`, `mean`
- `T1` **`sd`** `(a: [#N, num]) -> num` — sample standard deviation (n-1 denominator)
  ```
  sd(c(1, 2, 3, 4, 5))   # -> 1.5811...
  ```
  See also: `var`, `mean`
- `T1` **`median`** `(a: [#N, num]) -> num` — median value
  ```
  median(c(1, 2, 3, 4))   # -> 2.5
  ```
  See also: `mean`, `quantile`
- `T1` **`sum`** `(a: [#N, num]) -> num` — sum over all elements
  ```
  sum(c(1, 2, 3))   # -> 6
  ```
- `T1` **`prod`** `(a: [#N, num]) -> num` — product of all elements
  ```
  prod(c(2, 3, 4))   # -> 24
  ```
  See also: `sum`, `cumprod`
- `T1` **`range`** `(a: [#N, num]) -> [2, num]` — c(min, max) — range of values
  ```
  range(c(3, 1, 4, 1, 5))   # -> c(1, 5)
  ```
  See also: `min`, `max`
- `T1` **`min`** `(a: [#N, num]) -> num` — minimum value
  ```
  min(c(3, 1, 4, 1, 5))   # -> 1
  ```
  See also: `max`, `range`
- `T1` **`max`** `(a: [#N, num]) -> num` — maximum value
  ```
  max(c(3, 1, 4, 1, 5))   # -> 5
  ```
  See also: `min`, `range`
- `T1` **`pmin`** `(a: [#N, num], b: [#N, num]) -> [#N, num]` — parallel minimum (element-wise)
  ```
  pmin(c(1, 5, 3), c(4, 2, 6))   # -> c(1, 2, 3)
  ```
  See also: `pmax`, `min`
- `T1` **`pmax`** `(a: [#N, num], b: [#N, num]) -> [#N, num]` — parallel maximum (element-wise)
  ```
  pmax(c(1, 5, 3), c(4, 2, 6))   # -> c(4, 5, 6)
  ```
  See also: `pmin`, `max`
- `T1` **`intersect`** `(a: [#N, T], b: [#M, T]) -> [#N, T]` — elements in both x and y (sorted, unique)
  ```
  intersect(c(1,2,3), c(2,3,4))   # -> c(2,3)
  ```
  See also: `union`, `setdiff`
- `T1` **`union`** `(a: [#N, T], b: [#M, T]) -> [#N, T]` — elements in x or y (sorted, unique)
  ```
  union(c(1,2,3), c(2,3,4))   # -> c(1,2,3,4)
  ```
  See also: `intersect`, `setdiff`
- `T1` **`setdiff`** `(a: [#N, T], b: [#M, T]) -> [#N, T]` — elements in x but not in y
  ```
  setdiff(c(1,2,3), c(2,3,4))   # -> c(1)
  ```
  See also: `intersect`, `union`
- `T1` **`setequal`** `(a: [#N, T], b: [#M, T]) -> bool` — TRUE if x and y contain the same elements (ignoring order)
  ```
  setequal(c(1,2,3), c(3,2,1))   # -> TRUE
  ```
  See also: `identical`, `intersect`
- `T1` **`is.element`** `(a: T, b: [#N, T]) -> bool` — TRUE if a is in x
  ```
  is.element(2, c(1,2,3))   # -> TRUE
  ```
  See also: `intersect`, `match`
- `T1` **`is.numeric`** `(a: any) -> bool` — TRUE if x is numeric (integer or double)
  ```
  is.numeric(42)   # -> TRUE
  ```
  See also: `is.integer`, `is.character`
- `T1` **`is.integer`** `(a: any) -> bool` — TRUE if x is integer
  ```
  is.integer(42L)   # -> TRUE
  ```
  See also: `is.numeric`
- `T1` **`is.character`** `(a: any) -> bool` — TRUE if x is character
  ```
  is.character("hello")   # -> TRUE
  ```
  See also: `is.numeric`
- `T1` **`is.logical`** `(a: any) -> bool` — TRUE if x is logical
  ```
  is.logical(TRUE)   # -> TRUE
  ```
  See also: `is.numeric`
- `T1` **`is.factor`** `(a: any) -> bool` — TRUE if x is a factor
  ```
  is.factor(factor("a"))   # -> TRUE
  ```
  See also: `is.character`
- `T1` **`is.null`** `(a: any) -> bool` — TRUE if x is NULL
  ```
  is.null(NULL)   # -> TRUE
  ```
- `T1` **`is.na`** `(a: any) -> bool` — TRUE if x is NA
  ```
  is.na(NA)   # -> TRUE
  ```
- `T1` **`is.nan`** `(a: any) -> bool` — TRUE if x is NaN (Not a Number)
  ```
  is.nan(NaN)   # -> TRUE
  ```
- `T1` **`is.finite`** `(a: num) -> bool` — TRUE if x is finite (not Inf, -Inf, or NaN)
  ```
  is.finite(42)   # -> TRUE
  ```
  See also: `is.infinite`
- `T1` **`is.infinite`** `(a: num) -> bool` — TRUE if x is Inf or -Inf
  ```
  is.infinite(Inf)   # -> TRUE
  ```
  See also: `is.finite`
- `T1` **`is.complex`** `(a: any) -> bool` — TRUE if x is complex
  ```
  is.complex(1+2i)   # -> TRUE
  ```
  See also: `is.numeric`
- `T1` **`is.raw`** `(a: any) -> bool` — TRUE if x is raw (bytes)
  ```
  is.raw(charToRaw("a"))   # -> TRUE
  ```
  See also: `is.character`
- `T1` **`is.function`** `(a: any) -> bool` — TRUE if x is a function
  ```
  is.function(mean)   # -> TRUE
  ```
- `T1` **`is.primitive`** `(a: any) -> bool` — TRUE if x is a primitive function
  ```
  is.primitive(`+`)   # -> TRUE
  ```
  See also: `is.function`
- `T1` **`as.numeric`** `(a: any) -> num` — numeric (double) representation
  ```
  as.numeric(TRUE)   # -> 1.0
  ```
  See also: `as.integer`, `as.character`
- `T1` **`as.integer`** `(a: any) -> int` — integer representation
  ```
  as.integer(3.7)   # -> 3L
  ```
  See also: `as.numeric`
- `T1` **`as.character`** `(a: any) -> char` — character representation
  ```
  as.character(42)   # -> "42"
  ```
  See also: `as.numeric`, `as.integer`
- `T1` **`as.logical`** `(a: any) -> bool` — logical representation
  ```
  as.logical(0)   # -> FALSE
  ```
  See also: `as.numeric`
- `T1` **`as.complex`** `(a: any) -> complex` — complex representation
  ```
  as.complex(42)   # -> 42+0i
  ```
  See also: `as.numeric`
- `T1` **`as.raw`** `(a: any) -> raw` — raw (byte) representation
  ```
  as.raw(65L)   # -> charToRaw("A")
  ```
  See also: `as.integer`
- `T1` **`nchar`** `(a: char) -> int` — number of characters
  ```
  nchar("hello")   # -> 5
  ```
  See also: `nzchar`
- `T1` **`nzchar`** `(a: char) -> bool` — TRUE if x has more than 0 characters
  ```
  nzchar("hello")   # -> TRUE
  ```
  See also: `nchar`
- `T1` **`substr`** `(a: char, b: int, c: int) -> char` — substring from start to stop
  ```
  substr("hello world", 1, 5)   # -> "hello"
  ```
  See also: `substring`
- `T1` **`substring`** `(a: char, b: int, c: int) -> char` — substring(s)
  ```
  substring("hello world", 1, 5)   # -> "hello"
  ```
  See also: `substr`
- `T1` **`tolower`** `(a: char) -> char` — lowercased string
  ```
  tolower("Hello World")   # -> "hello world"
  ```
  See also: `toupper`, `casefold`
- `T1` **`toupper`** `(a: char) -> char` — uppercased string
  ```
  toupper("hello world")   # -> "HELLO WORLD"
  ```
  See also: `tolower`, `casefold`
- `T1` **`casefold`** `(a: char, b: bool) -> char` — case-folded string
  ```
  casefold("Hello", upper = TRUE)   # -> "HELLO"
  ```
  See also: `tolower`, `toupper`
- `T1` **`chartr`** `(a: char, b: char, c: char) -> char` — string with matches replaced
  ```
  chartr("a", "A", "hello abc")   # -> "hello Abc"
  ```
  See also: `sub`, `gsub`
- `T1` **`sprintf`** `(a: char, b: any) -> char` — formatted character string
  ```
  sprintf("Hello %s, you are %d", "world", 42)   # -> "Hello world, you are 42"
  ```
  See also: `format`, `formatC`
- `T1` **`format`** `(a: any, b: any) -> char` — formatted character string
  ```
  format(3.14159, digits = 3)   # -> "3.14"
  ```
  See also: `sprintf`, `formatC`
- `T1` **`formatC`** `(a: any, b: int, c: int, d: char) -> char` — formatted character string
  ```
  formatC(3.14, width = 6, digits = 2, format = "f")   # -> "  3.14"
  ```
  See also: `sprintf`, `format`
- `T1` **`grepl`** `(a: char, b: char) -> bool` — TRUE if pattern found anywhere in x
  ```
  grepl("\\d+", "abc123")   # -> TRUE
  ```
  See also: `grep`, `sub`, `gsub`
- `T1` **`grep`** `(a: char, b: [#N, char]) -> [#M, int]` — indices of elements matching pattern
  ```
  grep("\\d+", c("abc", "123", "def"))   # -> c(1, 3) (1-indexed)
  ```
  See also: `grepl`, `regexpr`
- `T1` **`regexpr`** `(a: char, b: char) -> int` — starting position(s) of match, or -1 if no match
  ```
  regexpr("\\d+", "abc123def")   # -> 4 (position of first digit)
  ```
  See also: `gregexpr`, `regmatches`, `grepl`
- `T1` **`gregexpr`** `(a: char, b: char) -> [#N, int]` — list of all match positions
  ```
  gregexpr("\\d+", "a1b22c333")   # -> list of match positions
  ```
  See also: `regexpr`, `regmatches`, `grepl`
- `T1` **`regmatches`** `(a: char, b: int) -> char` — extracted substrings or logical match vector
  ```
  regmatches("abc123def", "\\d+")   # -> "123"
  ```
  See also: `regexpr`, `gregexpr`, `grepl`
- `T1` **`sub`** `(a: char, b: char, c: char) -> char` — string with first match replaced
  ```
  sub("world", "R", "hello world")   # -> "hello R"
  ```
  See also: `gsub`, `grepl`
- `T1` **`gsub`** `(a: char, b: char, c: char) -> char` — string with ALL matches replaced
  ```
  gsub("o", "0", "foo bar")   # -> "f00 bar"
  ```
  See also: `sub`, `grepl`
- `T1` **`strsplit`** `(a: char, b: char) -> [#N, char]` — character vector of parts
  ```
  strsplit("a,b,c", ",")   # -> c("a", "b", "c")
  ```
  See also: `paste`, `paste0`
- `T1` **`seq_len`** `(a: int) -> [#N, int]` — integer sequence 1, 2, 
  ```
  seq_len(5)   # -> c(1, 2, 3, 4, 5)
  ```
  See also: `seq`, `seq_along`
- `T1` **`seq_along`** `(a: [#N, T]) -> [#N, int]` — integer sequence 1, 2, 
  ```
  seq_along(c(10, 20, 30))   # -> c(1, 2, 3)
  ```
  See also: `seq`, `seq_len`
- `T1` **`rep_len`** `(a: [#N, T], b: int) -> [#M, T]` — repeated vector
  ```
  rep_len(c(1, 2), 3)   # -> c(1, 2, 1, 2, 1, 2)
  ```
  See also: `rep`
- `T1` **`rep.int`** `(a: [#N, T], b: int) -> [#M, T]` — vector with each element repeated
  ```
  rep.int(c(1, 2), 3)   # -> c(1, 1, 1, 2, 2, 2)
  ```
  See also: `rep_len`
- `T1` **`crossprod`** `(a: [#M, T], b: [#N, T]) -> [#M, #N]` — cross-product (t(x) %*% y)
  ```
  crossprod(c(1,2), c(3,4))   # -> 11 (1*3 + 2*4)
  ```
  See also: `tcrossprod`, `outer`, `%*%`
- `T1` **`tcrossprod`** `(a: [#M, T], b: [#N, T]) -> [#M, #N]` — cross-product (x %*% t(y))
  ```
  tcrossprod(c(1,2), c(3,4))   # -> matrix [[3,4],[6,8]]
  ```
  See also: `crossprod`, `outer`
- `T1` **`outer`** `(a: [#M, T], b: [#N, T]) -> [#M, [#N, T]]` — outer product matrix
  ```
  outer(c(1,2), c(3,4))   # -> matrix [[3,4],[6,8]]
  ```
  See also: `crossprod`, `tcrossprod`
- `T1` **`inner`** `(a: [#N, T], b: [#N, T]) -> T` — inner product (dot product)
  ```
  inner(c(1,2), c(3,4))   # -> 11
  ```
  See also: `crossprod`
- `T1` **`colSums`** `(a: [#M, [#N, num]]) -> [#N, num]` — column sums
  ```
  colSums(matrix(1:6, 2, 3))   # -> c(3, 7, 11)
  ```
  See also: `rowSums`, `colMeans`
- `T1` **`rowSums`** `(a: [#M, [#N, num]]) -> [#M, num]` — row sums
  ```
  rowSums(matrix(1:6, 2, 3))   # -> c(6, 15)
  ```
  See also: `colSums`, `rowMeans`
- `T1` **`colMeans`** `(a: [#M, [#N, num]]) -> [#N, num]` — column means
  ```
  colMeans(matrix(1:6, 2, 3))   # -> c(1.5, 3.5, 5.5)
  ```
  See also: `rowMeans`, `colSums`
- `T1` **`rowMeans`** `(a: [#M, [#N, num]]) -> [#M, num]` — row means
  ```
  rowMeans(matrix(1:6, 2, 3))   # -> c(2, 5)
  ```
  See also: `colMeans`, `rowSums`
- `T1` **`identical`** `(a: any, b: any) -> bool` — TRUE if x and y are exactly equal (or nearly equal with tolerance)
  ```
  identical(1, 1)   # -> TRUE
  ```
  See also: `all.equal`, `is.element`
- `T1` **`which.min`** `(a: [#N, num]) -> int` — index of minimum value (first occurrence)
  ```
  which.min(c(3, 1, 4, 1, 5))   # -> 2 (1-indexed)
  ```
  See also: `which.max`, `min`
- `T1` **`which.max`** `(a: [#N, num]) -> int` — index of maximum value (first occurrence)
  ```
  which.max(c(3, 1, 4, 1, 5))   # -> 5 (1-indexed)
  ```
  See also: `which.min`, `max`
- `T1` **`which.is.min`** `(a: [#N, num]) -> int` — index of minimum value
  ```
  which.is.min(c(3, 1, 4, 1, 5))   # -> 2
  ```
  See also: `which.min`
- `T1` **`which.is.max`** `(a: [#N, num]) -> int` — index of maximum value
  ```
  which.is.max(c(3, 1, 4, 1, 5))   # -> 5
  ```
  See also: `which.max`
- `T1` **`head`** `(a: [#N, T], b: int) -> [#M, T]` — first n elements
  ```
  head(1:10, 3)   # -> c(1, 2, 3)
  ```
  See also: `tail`
- `T1` **`tail`** `(a: [#N, T], b: int) -> [#M, T]` — last n elements
  ```
  tail(1:10, 3)   # -> c(8, 9, 10)
  ```
  See also: `head`
- `T1` **`append`** `(a: [#N, T], b: [#M, T], c: int) -> [#N, T]` — new vector with values inserted
  ```
  append(1:5, 99, after = 2)   # -> c(1, 2, 99, 3, 4, 5)
  ```
  See also: `c`
- `T1` **`rev`** `(a: [#N, T]) -> [#N, T]` — reversed vector
  ```
  rev(c(1, 2, 3))   # -> c(3, 2, 1)
  ```
  See also: `sort`
- `T1` **`duplicated`** `(a: [#N, T]) -> [#N, bool]` — TRUE for first occurrence of each unique value
  ```
  duplicated(c(1, 2, 1, 3))   # -> c(FALSE, FALSE, TRUE, FALSE)
  ```
  See also: `unique`, `anyDuplicated`
- `T1` **`anyDuplicated`** `(a: [#N, T]) -> int` — index of first duplicate, or 0 if none
  ```
  anyDuplicated(c(1, 2, 1, 3))   # -> 3 (position of first dup)
  ```
  See also: `duplicated`, `unique`
- `T1` **`table`** `(a: [#N, T]) -> [#M, int]` — frequency table (counts of each unique value)
  ```
  table(c("a","b","a","c","b","a"))   # -> a:3, b:2, c:1
  ```
  See also: `tabulate`
- `T1` **`tabulate`** `(a: [#N, int], b: int) -> [#M, int]` — frequency count for each bin
  ```
  tabulate(c(1, 2, 2, 3, 3, 3))   # -> c(1, 2, 3)
  ```
  See also: `table`
- `T1` **`match`** `(a: [#N, T], b: [#M, T], c: int) -> [#N, int]` — values corresponding to names
  ```
  lookup(c(a=1, b=2), c("a","c"))   # -> c(1, NA)
  ```
  See also: `match`, `which`
- `T1` **`rank`** `(a: [#N, num]) -> [#N, num]` — ranks of elements (1-based, average for ties)
  ```
  rank(c(3, 1, 4, 1, 5))   # -> c(3, 1.5, 4, 1.5, 5)
  ```
  See also: `sort`, `order`
- `T1` **`order`** `(a: [#N, T]) -> [#N, int]` — indices that would sort x
  ```
  order(c(3, 1, 4, 1, 5))   # -> c(2, 4, 1, 3, 5)
  ```
  See also: `sort`, `rank`
- `T1` **`names`** `(a: [#N, T]) -> [#N, char]` — name(s) matching value
  ```
  names(c(a=1, b=2, c=3))[2]   # -> "b"
  ```
  See also: `match`, `which`
- `T1` **`nrow`** `(a: any) -> int` — number of rows
  ```
  nrow(matrix(1:6, 2, 3))   # -> 2
  ```
  See also: `ncol`, `dim`
- `T1` **`ncol`** `(a: any) -> int` — number of columns
  ```
  ncol(matrix(1:6, 2, 3))   # -> 3
  ```
  See also: `nrow`, `dim`
- `T1` **`dim`** `(a: any) -> [#N, int]` — dimension vector (rows, cols, 
  ```
  dim(matrix(1:6, 2, 3))   # -> c(2, 3)
  ```
  See also: `nrow`, `ncol`
- `T1` **`anyNA`** `(a: [#N, T]) -> bool` — TRUE if x has no NA values
  ```
  anyNA(c(1, 2, 3))   # -> FALSE
  ```
  See also: `is.na`
- `T1` **`which`** `(a: [#N, bool]) -> [#M, int]` — positions of TRUE values
  ```
  which(c(FALSE, TRUE, FALSE, TRUE))   # -> c(2, 4)
  ```
  See also: `which.min`, `which.max`
- `T1` **`print`** `(a: char) -> Empty` — prints to the console, returns nothing
  ```
  print(sum(c(1, 2, 3)))
  ```
- `T2` **`system2`** `(a: char, b: [#N, char], c: any, d: any, e: [#M, char], f: num) -> any` — command output as character
  ```
  system2("echo", c("hello"))
  ```
- `T1` **`chartr`** `(a: char, b: char, c: [#N, char]) -> [#N, char]` — string with matches replaced
  ```
  chartr("a", "A", "hello abc")   # -> "hello Abc"
  ```
  See also: `sub`, `gsub`

## `graphics` package  
- `T2` **`plot`** `(a: [#N, num], b: [#N, num], c: char, d: [2, num], e: [2, num], f: char, g: char, h: char, i: char, j: char, k: bool, overflow: bool) -> .None` — NULL (invisibly) — opens graphics device
  ```
  plot(c(1,2,3), c(4,5,6), "p", main="My Plot")
  ```

## `stats` package  
- `T1` **`dnorm`** `(a: num, b: num, c: num) -> num` — normal density at x
  ```
  dnorm(0)   # -> 0.3989...
  ```
  See also: `pnorm`, `qnorm`, `rnorm`
- `T1` **`pnorm`** `(a: num, b: num, c: num, d: bool) -> num` — cumulative probability P(X <= q)
  ```
  pnorm(0)   # -> 0.5
  ```
  See also: `dnorm`, `qnorm`, `rnorm`
- `T1` **`qnorm`** `(a: num, b: num, c: num, d: bool) -> num` — quantile value q such that P(X <= q) = p
  ```
  qnorm(0.975)   # -> 1.9599...
  ```
  See also: `dnorm`, `pnorm`, `rnorm`
- `T1` **`rnorm`** `(a: int, b: num, c: num) -> [#N, num]` — vector of n random normal values
  ```
  rnorm(1, mean = 0, sd = 1)   # -> numeric(1) [random]
  ```
  See also: `dnorm`, `pnorm`, `qnorm`
- `T1` **`dunif`** `(a: num, b: num, c: num) -> num` — uniform density at x
  ```
  dunif(0.5)   # -> 1
  ```
  See also: `punif`, `qunif`, `runif`
- `T1` **`punif`** `(a: num, b: num, c: num, d: bool) -> num` — cumulative probability
  ```
  punif(0.5)   # -> 0.5
  ```
  See also: `dunif`, `qunif`, `runif`
- `T1` **`qunif`** `(a: num, b: num, c: num, d: bool) -> num` — quantile value
  ```
  qunif(0.5)   # -> 0.5
  ```
  See also: `dunif`, `punif`, `runif`
- `T1` **`runif`** `(a: int, b: num, c: num) -> [#N, num]` — vector of n random uniform values in [min, max]
  ```
  runif(1)   # -> numeric(1) [random in [0,1]]
  ```
  See also: `dunif`, `punif`, `qunif`
- `T1` **`dbinom`** `(a: int, b: int, c: num) -> num` — binomial density
  ```
  dbinom(3, size = 10, prob = 0.5)   # -> 0.1172...
  ```
  See also: `pbinom`, `qbinom`, `rbinom`
- `T1` **`pbinom`** `(a: int, b: int, c: num, d: bool) -> num` — cumulative probability P(X <= q)
  ```
  pbinom(3, size = 10, prob = 0.5)   # -> 0.1719...
  ```
  See also: `dbinom`, `qbinom`, `rbinom`
- `T1` **`qbinom`** `(a: num, b: int, c: num, d: bool) -> int` — quantile (number of successes)
  ```
  qbinom(0.5, size = 10, prob = 0.5)   # -> 5
  ```
  See also: `dbinom`, `pbinom`, `rbinom`
- `T1` **`rbinom`** `(a: int, b: int, c: num) -> [#N, int]` — vector of n random binomial counts
  ```
  rbinom(1, size = 10, prob = 0.5)   # -> integer(1) [random]
  ```
  See also: `dbinom`, `pbinom`, `qbinom`
- `T1` **`dpois`** `(a: int, b: num) -> num` — Poisson density
  ```
  dpois(3, lambda = 2)   # -> 0.1804...
  ```
  See also: `ppois`, `qpois`, `rpois`
- `T1` **`ppois`** `(a: int, b: num, c: bool) -> num` — cumulative probability P(X <= q)
  ```
  ppois(3, lambda = 2)   # -> 0.8571...
  ```
  See also: `dpois`, `qpois`, `rpois`
- `T1` **`qpois`** `(a: num, b: num, c: bool) -> int` — quantile (count)
  ```
  qpois(0.5, lambda = 2)   # -> 2
  ```
  See also: `dpois`, `ppois`, `rpois`
- `T1` **`rpois`** `(a: int, b: num) -> [#N, int]` — vector of n random Poisson counts
  ```
  rpois(1, lambda = 2)   # -> integer(1) [random]
  ```
  See also: `dpois`, `ppois`, `qpois`
- `T1` **`dexp`** `(a: num, b: num) -> num` — exponential density
  ```
  dexp(1, rate = 1)   # -> 0.3679...
  ```
  See also: `pexp`, `qexp`, `rexp`
- `T1` **`pexp`** `(a: num, b: num, c: bool) -> num` — cumulative probability
  ```
  pexp(1, rate = 1)   # -> 0.6321...
  ```
  See also: `dexp`, `qexp`, `rexp`
- `T1` **`qexp`** `(a: num, b: num, c: bool) -> num` — quantile value
  ```
  qexp(0.5, rate = 1)   # -> 0.6931...
  ```
  See also: `dexp`, `pexp`, `rexp`
- `T1` **`rexp`** `(a: int, b: num) -> [#N, num]` — vector of n random exponential values
  ```
  rexp(1)   # -> numeric(1) [random]
  ```
  See also: `dexp`, `pexp`, `qexp`
- `T1` **`dgamma`** `(a: num, b: num, c: num) -> num` — gamma density
  ```
  dgamma(1, shape = 2, rate = 1)   # -> 0.3679...
  ```
  See also: `pgamma`, `qgamma`, `rgamma`
- `T1` **`pgamma`** `(a: num, b: num, c: num, d: bool) -> num` — cumulative probability
  ```
  pgamma(1, shape = 2, rate = 1)   # -> 0.2642...
  ```
  See also: `dgamma`, `qgamma`, `rgamma`
- `T1` **`qgamma`** `(a: num, b: num, c: num, d: bool) -> num` — quantile value
  ```
  qgamma(0.5, shape = 2, rate = 1)   # -> 1.6783...
  ```
  See also: `dgamma`, `pgamma`, `rgamma`
- `T1` **`rgamma`** `(a: int, b: num, c: num) -> [#N, num]` — vector of n random gamma values
  ```
  rgamma(1, shape = 2, rate = 1)   # -> numeric(1) [random]
  ```
  See also: `dgamma`, `pgamma`, `qgamma`
- `T1` **`dbeta`** `(a: num, b: num, c: num) -> num` — beta density
  ```
  dbeta(0.5, shape1 = 2, shape2 = 2)   # -> 1.5
  ```
  See also: `pbeta`, `qbeta`, `rbeta`
- `T1` **`pbeta`** `(a: num, b: num, c: num, d: bool) -> num` — cumulative probability
  ```
  pbeta(0.5, shape1 = 2, shape2 = 2)   # -> 0.5
  ```
  See also: `dbeta`, `qbeta`, `rbeta`
- `T1` **`qbeta`** `(a: num, b: num, c: num, d: bool) -> num` — quantile value in [0, 1]
  ```
  qbeta(0.5, shape1 = 2, shape2 = 2)   # -> 0.5
  ```
  See also: `dbeta`, `pbeta`, `rbeta`
- `T1` **`rbeta`** `(a: int, b: num, c: num) -> [#N, num]` — vector of n random beta values in [0, 1]
  ```
  rbeta(1, shape1 = 2, shape2 = 2)   # -> numeric(1) [random]
  ```
  See also: `dbeta`, `pbeta`, `qbeta`
- `T1` **`dchisq`** `(a: num, b: num) -> num` — chi-squared density
  ```
  dchisq(2, df = 2)   # -> 0.2431...
  ```
  See also: `pchisq`, `qchisq`, `rchisq`
- `T1` **`pchisq`** `(a: num, b: num, c: bool) -> num` — cumulative probability
  ```
  pchisq(2, df = 2)   # -> 0.3935...
  ```
  See also: `dchisq`, `qchisq`, `rchisq`
- `T1` **`qchisq`** `(a: num, b: num, c: bool) -> num` — quantile value
  ```
  qchisq(0.95, df = 1)   # -> 3.8415...
  ```
  See also: `dchisq`, `pchisq`, `rchisq`
- `T1` **`rchisq`** `(a: int, b: num) -> [#N, num]` — vector of n random chi-squared values
  ```
  rchisq(1, df = 2)   # -> numeric(1) [random]
  ```
  See also: `dchisq`, `pchisq`, `qchisq`
- `T1` **`dt`** `(a: num, b: num) -> num` — Student's t density
  ```
  dt(0, df = 10)   # -> 0.3867...
  ```
  See also: `pt`, `qt`, `rt`
- `T1` **`pt`** `(a: num, b: num, c: bool) -> num` — cumulative probability
  ```
  pt(2, df = 10)   # -> 0.9633...
  ```
  See also: `dt`, `qt`, `rt`
- `T1` **`qt`** `(a: num, b: num, c: bool) -> num` — quantile value
  ```
  qt(0.975, df = 10)   # -> 2.2281...
  ```
  See also: `dt`, `pt`, `rt`
- `T1` **`rt`** `(a: int, b: num) -> [#N, num]` — vector of n random t values
  ```
  rt(1, df = 10)   # -> numeric(1) [random]
  ```
  See also: `dt`, `pt`, `qt`
- `T1` **`df`** `(a: num, b: num, c: num) -> num` — F density
  ```
  df(1, df1 = 5, df2 = 10)   # -> 0.3451...
  ```
  See also: `pf`, `qf`, `rf`
- `T1` **`pf`** `(a: num, b: num, c: num, d: bool) -> num` — cumulative probability
  ```
  pf(2, df1 = 5, df2 = 10)   # -> 0.8533...
  ```
  See also: `df`, `qf`, `rf`
- `T1` **`qf`** `(a: num, b: num, c: num, d: bool) -> num` — quantile value
  ```
  qf(0.95, df1 = 5, df2 = 10)   # -> 3.3258...
  ```
  See also: `df`, `pf`, `rf`
- `T1` **`rf`** `(a: int, b: num, c: num) -> [#N, num]` — vector of n random F values
  ```
  rf(1, df1 = 5, df2 = 10)   # -> numeric(1) [random]
  ```
  See also: `df`, `pf`, `qf`
- `T1` **`dlnorm`** `(a: num, b: num, c: num) -> num` — log-normal density
  ```
  dlnorm(1, meanlog = 0, sdlog = 1)   # -> 0.3989...
  ```
  See also: `plnorm`, `qlnorm`, `rlnorm`
- `T1` **`plnorm`** `(a: num, b: num, c: num, d: bool) -> num` — cumulative probability
  ```
  plnorm(1)   # -> 0.5
  ```
  See also: `dlnorm`, `qlnorm`, `rlnorm`
- `T1` **`qlnorm`** `(a: num, b: num, c: num, d: bool) -> num` — quantile value
  ```
  qlnorm(0.5)   # -> 1
  ```
  See also: `dlnorm`, `plnorm`, `rlnorm`
- `T1` **`rlnorm`** `(a: int, b: num, c: num) -> [#N, num]` — vector of n random log-normal values
  ```
  rlnorm(1)   # -> numeric(1) [random]
  ```
  See also: `dlnorm`, `plnorm`, `qlnorm`
- `T1` **`dlogis`** `(a: num, b: num, c: num) -> num` — logistic density
  ```
  dlogis(0)   # -> 0.25
  ```
  See also: `plogis`, `qlogis`, `rlogis`
- `T1` **`plogis`** `(a: num, b: num, c: num, d: bool) -> num` — cumulative probability
  ```
  plogis(0)   # -> 0.5
  ```
  See also: `dlogis`, `qlogis`, `rlogis`
- `T1` **`qlogis`** `(a: num, b: num, c: num, d: bool) -> num` — quantile value
  ```
  qlogis(0.5)   # -> 0
  ```
  See also: `dlogis`, `plogis`, `rlogis`
- `T1` **`rlogis`** `(a: int, b: num, c: num) -> [#N, num]` — vector of n random logistic values
  ```
  rlogis(1)   # -> numeric(1) [random]
  ```
  See also: `dlogis`, `plogis`, `qlogis`
- `T1` **`dweibull`** `(a: num, b: num, c: num) -> num` — Weibull density
  ```
  dweibull(1, shape = 2)   # -> 0.7358...
  ```
  See also: `pweibull`, `qweibull`, `rweibull`
- `T1` **`pweibull`** `(a: num, b: num, c: num, d: bool) -> num` — cumulative probability
  ```
  pweibull(1, shape = 2)   # -> 0.6321...
  ```
  See also: `dweibull`, `qweibull`, `rweibull`
- `T1` **`qweibull`** `(a: num, b: num, c: num, d: bool) -> num` — quantile value
  ```
  qweibull(0.5, shape = 2)   # -> 0.8326...
  ```
  See also: `dweibull`, `pweibull`, `rweibull`
- `T1` **`rweibull`** `(a: int, b: num, c: num) -> [#N, num]` — vector of n random Weibull values
  ```
  rweibull(1, shape = 2)   # -> numeric(1) [random]
  ```
  See also: `dweibull`, `pweibull`, `qweibull`
- `T1` **`dgeom`** `(a: int, b: num) -> num` — geometric density
  ```
  dgeom(2, prob = 0.5)   # -> 0.125
  ```
  See also: `pgeom`, `qgeom`, `rgeom`
- `T1` **`pgeom`** `(a: int, b: num, c: bool) -> num` — cumulative probability
  ```
  pgeom(2, prob = 0.5)   # -> 0.875
  ```
  See also: `dgeom`, `qgeom`, `rgeom`
- `T1` **`qgeom`** `(a: num, b: num, c: bool) -> int` — quantile (number of failures)
  ```
  qgeom(0.5, prob = 0.5)   # -> 1
  ```
  See also: `dgeom`, `pgeom`, `rgeom`
- `T1` **`rgeom`** `(a: int, b: num) -> [#N, int]` — vector of n random geometric counts
  ```
  rgeom(1, prob = 0.5)   # -> integer(1) [random]
  ```
  See also: `dgeom`, `pgeom`, `qgeom`
- `T1` **`dnbinom`** `(a: int, b: num, c: num) -> num` — negative binomial density
  ```
  dnbinom(2, size = 5, prob = 0.5)   # -> 0.1562...
  ```
  See also: `pnbinom`, `qnbinom`, `rnbinom`
- `T1` **`pnbinom`** `(a: int, b: num, c: num, d: bool) -> num` — cumulative probability
  ```
  pnbinom(2, size = 5, prob = 0.5)   # -> 0.1719...
  ```
  See also: `dnbinom`, `qnbinom`, `rnbinom`
- `T1` **`qnbinom`** `(a: num, b: num, c: num, d: bool) -> int` — quantile (number of failures)
  ```
  qnbinom(0.5, size = 5, prob = 0.5)   # -> 4
  ```
  See also: `dnbinom`, `pnbinom`, `rnbinom`
- `T1` **`rnbinom`** `(a: int, b: num, c: num) -> [#N, int]` — vector of n random negative binomial counts
  ```
  rnbinom(1, size = 5, prob = 0.5)   # -> integer(1) [random]
  ```
  See also: `dnbinom`, `pnbinom`, `qnbinom`
- `T1` **`dhyper`** `(a: int, b: int, c: int, d: int) -> num` — hypergeometric density
  ```
  dhyper(2, m = 5, n = 5, k = 4)   # -> 0.4762...
  ```
  See also: `phyper`, `qhyper`, `rhyper`
- `T1` **`phyper`** `(a: int, b: int, c: int, d: int, e: bool) -> num` — cumulative probability
  ```
  phyper(2, m = 5, n = 5, k = 4)   # -> 0.7619...
  ```
  See also: `dhyper`, `qhyper`, `rhyper`
- `T1` **`qhyper`** `(a: num, b: int, c: int, d: int, e: bool) -> int` — quantile (number of white balls)
  ```
  qhyper(0.5, m = 5, n = 5, k = 4)   # -> 2
  ```
  See also: `dhyper`, `phyper`, `rhyper`
- `T1` **`rhyper`** `(a: int, b: int, c: int, d: int) -> [#N, int]` — vector of n random hypergeometric counts
  ```
  rhyper(1, m = 5, n = 5, k = 4)   # -> integer(1) [random]
  ```
  See also: `dhyper`, `phyper`, `qhyper`
- `T2` **`pwilcox`** `(a: num, b: int, c: int, d: bool) -> num` — cumulative probability
  ```
  pwilcox(2, m = 3, n = 4)   # -> 0.7143...
  ```
  See also: `dwilcox`, `qwilcox`, `rwilcox`
- `T2` **`psignrank`** `(a: num, b: int, c: bool) -> num` — cumulative probability
  ```
  psignrank(2, n = 5)   # -> 0.6875
  ```
  See also: `dsignrank`, `qsignrank`, `rsignrank`
- `T1` **`IQR`** `(a: [#N, num], b: bool) -> num` — interquartile range (75th percentile - 25th percentile)
  ```
  IQR(1:10)   # -> 5
  ```
  See also: `quantile`, `mad`
- `T1` **`mad`** `(a: [#N, num], b: bool) -> num` — median absolute deviation: median(|x - median(x)|) * 1
  ```
  mad(c(1, 1, 2, 2, 4, 6, 9))   # -> 2.9652...
  ```
  See also: `IQR`, `sd`, `var`
- `T1` **`fivenum`** `(a: [#N, num], b: bool) -> [5, num]` — five-number summary: c(min, Q1, median, Q3, max)
  ```
  fivenum(c(1, 2, 3, 4, 5))   # -> c(1, 1.5, 3, 4.5, 5)
  ```
  See also: `summary`, `quantile`, `range`
- `T1` **`weighted.mean`** `(a: [#N, num], b: [#N, num], c: bool) -> num` — weighted arithmetic mean
  ```
  weighted.mean(c(1, 2, 3), c(3, 2, 1))   # -> 1.6666...
  ```
  See also: `mean`, `sum`
- `T1` **`cor`** `(a: [#N, num], b: [#N, num], c: char) -> num` — correlation coefficient or matrix
  ```
  cor(c(1, 2, 3), c(2, 4, 6))   # -> 1
  ```
  See also: `cor.test`, `cov`, `cov2cor`
- `T2` **`cor.test`** `(a: [#N, num], b: [#N, num], c: char, d: char, e: num) -> any` — list with estimate, p
  ```
  cor.test(1:10, c(2,4,6,8,10,12,14,16,18,20))   # -> r=1, p<2.2e-16
  ```
  See also: `cor`, `cov`
- `T1` **`cov`** `(a: [#N, num], b: [#N, num], c: char) -> num` — covariance or covariance matrix
  ```
  cov(c(1, 2, 3), c(2, 4, 6))   # -> 2
  ```
  See also: `cor`, `cor.test`
- `T2` **`t.test`** `(a: [#N, num], b: [#N, num], c: char, d: num, e: bool, f: num) -> any` — list with statistic, df, p
  ```
  t.test(1:10, rnorm(10))   # -> t-test result
  ```
  See also: `var.test`, `wilcox.test`
- `T2` **`chisq.test`** `(a: [#N, num], b: [#N, num], c: char, d: bool) -> any` — list with statistic, df, p
  ```
  chisq.test(c(10, 10, 10))   # -> Chi-squared test
  ```
  See also: `fisher.test`, `prop.test`
- `T2` **`fisher.test`** `(a: [#M, [#N, num]], b: char, c: num) -> any` — list with estimate, p
  ```
  fisher.test(matrix(c(1, 9, 11, 2), 2, 2))   # -> Fisher's exact test
  ```
  See also: `chisq.test`
- `T2` **`wilcox.test`** `(a: [#N, num], b: [#N, num], c: char, d: bool, e: num) -> any` — list with statistic, p
  ```
  wilcox.test(1:10, 2:11)   # -> Wilcoxon rank-sum test
  ```
  See also: `t.test`, `ks.test`
- `T2` **`binom.test`** `(a: int, b: int, c: num, d: char, e: num) -> any` — list with statistic, p
  ```
  binom.test(6, 10)   # -> exact binomial test
  ```
  See also: `prop.test`, `chisq.test`
- `T2` **`shapiro.test`** `(a: [#N, num]) -> any` — list with statistic, p
  ```
  shapiro.test(rnorm(100))   # -> W ≈ 1, p ≈ 1
  ```
  See also: `ks.test`
- `T2` **`ks.test`** `(a: [#N, num], b: [#N, num], c: char) -> any` — list with statistic, p
  ```
  ks.test(rnorm(100), "pnorm")   # -> KS test against normal
  ```
  See also: `shapiro.test`, `wilcox.test`
- `T2` **`var.test`** `(a: [#N, num], b: [#N, num], c: char) -> any` — list with statistic, df, p
  ```
  var.test(rnorm(100), rnorm(100, sd = 2))   # -> F-test
  ```
  See also: `t.test`
- `T2` **`kruskal.test`** `(a: [#N, num], b: any) -> any` — list with statistic, p
  ```
  kruskal.test(list(c(1,2,3), c(4,5,6), c(7,8,9)))   # -> H-test
  ```
  See also: `wilcox.test`, `oneway.test`
- `T1` **`approx`** `(a: [#N, num], b: [#N, num], c: [#M, num], d: char, e: int) -> [#M, num]` — interpolated y-values
  ```
  approx(c(1, 2, 3), c(10, 20, 30), xout = 1.5)   # -> 15
  ```
  See also: `spline`, `approx`
- `T1` **`spline`** `(a: [#N, num], b: [#N, num], c: int, d: char) -> any` — list with x and y of interpolated spline
  ```
  spline(1:4, c(0,1,0,1), n = 10)$y   # -> 10 interpolated values
  ```
  See also: `splinefun`, `approx`
- `T1` **`splinefun`** `(a: [#N, num], b: [#N, num], c: char) -> any` — list with coefficients (for predict)
  ```
  sp <- splinefun(1:4, c(1, 4, 9, 16)); sp(2.5)   # -> interpolated value
  ```
  See also: `spline`, `approx`
- `T2` **`optim`** `(a: [#N, num], b: any, c: char, d: [#N, num], e: [#N, num]) -> any` — list with par (minimum parameters), value (minimum value), convergence
  ```
  optim(c(0,0), function(x) x[1]^2 + x[2]^2)   # -> par = c(0,0), value = 0
  ```
  See also: `nlminb`, `optimize`
- `T2` **`nlminb`** `(a: [#N, num], b: any, c: [#N, num], d: [#N, num]) -> any` — list with par, objective, convergence
  ```
  nlminb(c(0,0), function(x) x[1]^2 + x[2]^2, lower = -10, upper = 10)
  ```
  See also: `optim`
- `T1` **`optimize`** `(a: any, b: num, c: num) -> any` — list with minimum (x) and objective value (objective)
  ```
  optimize(function(x) (x-3)^2, lower = 0, upper = 10)   # -> minimum = 3
  ```
  See also: `optim`, `nlminb`
- `T1` **`integrate`** `(a: any, b: num, c: num, d: int) -> any` — list with value (integral estimate) and abs
  ```
  integrate(function(x) x^2, 0, 1)   # -> value = 0.3333...
  ```
  See also: `optim`, `nlminb`
- `T1` **`dist`** `(a: [#N, T], b: char) -> any` — distance matrix (dist object)
  ```
  dist(matrix(1:6, 2, 3))   # -> pairwise distances
  ```
  See also: `hclust`, `cmdscale`
- `T2` **`hclust`** `(a: any, b: char, c: [#N, num]) -> any` — hclust object (list with merge, height, order, labels, method, dist
  ```
  hclust(dist(c(1, 2, 3, 10, 11, 12)))   # -> hierarchical cluster tree
  ```
  See also: `dist`, `cutree`, `rect.hclust`
- `T1` **`cutree`** `(a: any, b: int, c: num) -> [#N, int]` — integer vector of cluster assignments
  ```
  hc <- hclust(dist(c(1,2,3,10,11,12))); cutree(hc, k = 2)   # -> c(1,1,1,2,2,2)
  ```
  See also: `hclust`, `dist`
- `T2` **`kmeans`** `(a: [#N, [#M, num]], b: int, c: int, d: int) -> any` — list with cluster (assignments), centers, size, withinss, totss
  ```
  kmeans(matrix(rnorm(100), 50, 2), centers = 2)   # -> k-means clustering
  ```
  See also: `hclust`, `dist`
- `T1` **`quantile`** `(a: [#N, num], b: [#M, num], c: bool) -> [#M, num]` — quantile values at requested probabilities
  ```
  quantile(1:100)   # -> c(1, 25.75, 50.5, 75.25, 100)
  ```
  See also: `median`, `IQR`, `fivenum`
- `T2` **`predict`** `(a: any, b: any, c: char) -> any` — predicted values
  ```
  m <- lm(mpg ~ wt, data = mtcars); predict(m, newdata = data.frame(wt = 3))
  ```
  See also: `fitted`, `residuals`
- `T2` **`fitted`** `(a: any) -> any` — fitted (predicted) values
  ```
  m <- lm(mpg ~ wt, data = mtcars); fitted(m)   # -> fitted values
  ```
  See also: `predict`, `residuals`
- `T2` **`residuals`** `(a: any, b: char) -> any` — residuals
  ```
  m <- lm(mpg ~ wt, data = mtcars); residuals(m)   # -> residuals
  ```
  See also: `fitted`, `predict`
- `T1` **`is.ts`** `(a: any) -> bool` — TRUE if x has a time series attribute (tsp)
  ```
  is.ts(1:10)   # -> FALSE
  ```
  See also: `ts`, `as.ts`
- `T1` **`is.stepfun`** `(a: any) -> bool` — TRUE if x is a step function
  ```
  is.stepfun(stepfun(1:5, 1:5))   # -> TRUE
  ```
  See also: `stepfun`
- `T1` **`hasTsp`** `(a: any) -> bool` — TRUE if x has TSP (time series) attributes
  ```
  hasTsp(ts(1:10))   # -> TRUE
  ```
  See also: `tsp`, `ts`
- `T1` **`complete.cases`** `(a: [#N, T]) -> [#N, bool]` — complete cases indicator (logical vector)
  ```
  complete.cases(c(1, NA, 3))   # -> c(TRUE, FALSE, TRUE)
  ```
  See also: `na.omit`, `is.na`
- `T1` **`setNames`** `(a: [#N, T], b: [#N, char]) -> [#N, T]` — named list
  ```
  setNames(1:3, c("a", "b", "c"))   # -> c(a=1, b=2, c=3)
  ```
  See also: `names`
- `T2` **`step`** `(a: any, b: any, c: char) -> any` — updated model object
  ```
  m <- lm(mpg ~ wt + cyl, data = mtcars); step(m)
  ```
  See also: `lm`, `glm`, `update`

## `utils` package  
- `T2` **`read.csv`** `(a: char, b: bool, c: char, d: char, e: [#N, char], f: int, g: int, h: bool) -> any` — data frame
  ```
  read.csv("data.csv")   # -> data frame from CSV file
  ```
  See also: `read.table`, `write.csv`, `read.delim`
- `T2` **`read.csv2`** `(a: char, b: bool, c: char, d: char, e: [#N, char]) -> any` — data frame
  ```
  read.csv2("data.csv2")   # -> data frame
  ```
  See also: `read.csv`, `write.csv2`
- `T2` **`read.delim`** `(a: char, b: bool, c: char, d: char, e: [#N, char], f: int, g: int) -> any` — data frame
  ```
  read.delim("data.tsv")   # -> data frame from TSV file
  ```
  See also: `read.csv`, `read.table`, `write.table`
- `T2` **`read.delim2`** `(a: char, b: bool, c: char, d: char, e: [#N, char]) -> any` — data frame
  ```
  read.delim2("data.tsv2")   # -> data frame
  ```
  See also: `read.delim`, `write.table`
- `T2` **`read.table`** `(a: char, b: bool, c: char, d: char, e: char, f: any, g: [#M, char], h: [#N, char], i: [#M, char], j: int, k: int, overflow: bool) -> any` — data frame
  ```
  read.table("data.txt", header = TRUE)   # -> data frame
  ```
  See also: `read.csv`, `read.delim`, `write.table`
- `T2` **`read.fwf`** `(a: char, b: [#N, int], c: bool, d: [#M, char], e: int) -> any` — data frame with fixed-width fields
  ```
  read.fwf("data.fwf", widths = c(5, 10, 3))
  ```
  See also: `read.table`, `read.fortran`
- `T2` **`read.DIF`** `(a: char, b: bool, c: [#M, char], d: [#N, char]) -> any` — data frame
  ```
  read.DIF("data.dif")
  ```
  See also: `read.table`
- `T2` **`write.csv`** `(a: any, b: char, c: bool, d: bool, e: char, f: bool, g: char, h: char, i: char) -> Empty` — invisibly, NULL
  ```
  write.csv(mtcars, "output.csv")
  ```
  See also: `write.table`, `write.csv2`, `read.csv`
- `T2` **`write.csv2`** `(a: any, b: char) -> Empty` — invisibly, NULL
  ```
  write.csv2(mtcars, "output.csv2")
  ```
  See also: `write.csv`, `write.table`
- `T2` **`write.table`** `(a: any, b: char, c: bool, d: bool, e: char, f: bool, g: char, h: char) -> Empty` — invisibly, NULL
  ```
  write.table(mtcars, "output.txt", sep = "\t")
  ```
  See also: `write.csv`, `read.table`
- `T2` **`str`** `(a: any, b: int) -> Empty` — invisibly, NULL (prints to stdout)
  ```
  str(mtcars)   # -> prints column types and first values
  ```
  See also: `summary`, `head`
- `T2` **`head.matrix`** `(a: [#N, [#M, T]], b: int) -> [#K, [#M, T]]` — first n rows as data frame
  ```
  head(mtcars, 3)   # -> first 3 rows of mtcars
  ```
  See also: `tail`, `str`
- `T2` **`tail.matrix`** `(a: [#N, [#M, T]], b: int) -> [#K, [#M, T]]` — last n rows as data frame
  ```
  tail(mtcars, 3)   # -> last 3 rows of mtcars
  ```
  See also: `head`, `str`
- `T1` **`type.convert`** `(a: any, b: [#N, char], c: char) -> any` — converted value
  ```
  type.convert(c("1", "2.5", "TRUE"))   # -> c(1L, 2.5, TRUE)
  ```
  See also: `as.numeric`, `as.integer`, `as.logical`
- `T2` **`installed.packages`** `(a: [#N, char], b: [#M, char]) -> any` — character matrix with columns Package, LibPath, Version, 
  ```
  ip <- installed.packages(); head(ip[, "Package"])   # -> first 6 package names
  ```
  See also: `packageVersion`, `packageDescription`, `sessionInfo`
- `T1` **`packageVersion`** `(a: char, b: char) -> any` — packageVersion object (comparable numeric version)
  ```
  packageVersion("base")   # -> '4.x.x'
  ```
  See also: `packageDescription`, `installed.packages`
- `T2` **`packageDescription`** `(a: char, b: char, c: [#N, char], d: bool) -> any` — named list of package metadata (Title, Description, Author, etc
  ```
  pd <- packageDescription("base"); pd$Title   # -> "The R Base Package"
  ```
  See also: `packageVersion`, `installed.packages`
- `T2` **`sessionInfo`** `() -> any` — list with R
  ```
  si <- sessionInfo(); si$R.version$version.string
  ```
  See also: `installed.packages`, `packageVersion`
- `T1` **`combn`** `(a: int, b: int, c: any, d: bool) -> any` — matrix of all combinations (each column = one combination) or result of FUN
  ```
  combn(5, 3)   # -> 3 x 10 matrix of combinations
  ```
  See also: `expand.grid`
- `T2` **`expand.grid`** `(a: any) -> any` — data frame with all combinations (full factorial design)
  ```
  expand.grid(height = c(60, 80), weight = c(100, 150, 200))
  ```
  See also: `combn`
- `T1` **`stack`** `(a: [#N, T], b: [#M, char], c: char, d: char) -> any` — data frame in long format
  ```
  stack(list(a = 1:3, b = 4:6))   # -> data frame with values and ind
  ```
  See also: `unstack`
- `T1` **`unstack`** `(a: any, b: any, c: char) -> any` — data frame in wide format
  ```
  unstack(stack(list(a = 1:3, b = 4:6)))
  ```
  See also: `stack`
- `T1` **`hasName`** `(a: any, b: char) -> bool` — TRUE if x[[value]] exists and is not NULL
  ```
  hasName(list(a = 1, b = 2), "a")   # -> TRUE
  ```
  See also: `names`
- `T1` **`modifyList`** `(a: any, b: any) -> any` — modified list
  ```
  modifyList(list(a = 1, b = 2), list(b = 99, c = 3))
  ```
  See also: `c`, `list`
- `T1` **`adist`** `(a: [#N, char], b: [#M, char], c: any, d: char) -> [#N, [#M, int]]` — matrix of edit distances (rows = s, cols = t)
  ```
  adist("hello", "hallo")   # -> 1 (substitution cost)
  ```
  See also: `agrep`, `grep`
- `T1` **`aregexec`** `(a: char, b: [#N, char], c: num, d: bool) -> any` — matching strings or logical vector
  ```
  aregexec("temperature", "tempurature", max.distance = 2)
  ```
  See also: `agrep`, `grep`
- `T1` **`URLencode`** `(a: char, b: bool) -> char` — percent-encoded URL string
  ```
  URLencode("hello world")   # -> "hello%20world"
  ```
  See also: `URLdecode`
- `T1` **`URLdecode`** `(a: char) -> char` — decoded URL string
  ```
  URLdecode("hello%20world")   # -> "hello world"
  ```
  See also: `URLencode`
- `T2` **`system`** `(a: char, b: bool, c: bool, d: bool) -> any` — command output (if intern = TRUE) or exit status (if intern = FALSE)
  ```
  system("ls", intern = TRUE)   # -> character vector of file names
  ```
  See also: `system2`, `Sys.sleep`
- `T1` **`file.copy`** `(a: [#N, char], b: [#M, char], c: bool, d: bool, e: bool) -> [#N, bool]` — logical vector (TRUE for each successful copy)
  ```
  file.copy("source.txt", "dest.txt")
  ```
  See also: `file.remove`, `file.rename`, `file.exists`
- `T1` **`file.remove`** `(a: [#N, char], b: bool) -> [#N, bool]` — logical vector (TRUE for each successful deletion)
  ```
  file.remove("temp.txt")
  ```
  See also: `file.copy`, `file.exists`
- `T1` **`file.rename`** `(a: [#N, char], b: [#M, char], c: bool) -> [#N, bool]` — logical vector (TRUE for each successful rename)
  ```
  file.rename("old.txt", "new.txt")
  ```
  See also: `file.copy`, `file.remove`
- `T1` **`dir.create`** `(a: char, b: bool, c: bool) -> bool` — TRUE if successful, FALSE otherwise
  ```
  file.create("output/dir1/dir2")
  ```
  See also: `list.files`, `dir.create`
- `T1` **`list.files`** `(a: char, b: char, c: bool, d: bool, e: bool) -> [#N, char]` — character vector of file/directory names
  ```
  list.files(path = ".", pattern = "\\.R$")
  ```
  See also: `dir.create`, `file.exists`, `list.dirs`
- `T1` **`txtProgressBar`** `(a: num, b: num, c: num, d: char, e: char) -> any` — txtProgressBar object (use setTxtProgressBar to update)
  ```
  pb <- txtProgressBar(min = 0, max = 100, style = "txt")
  ```
  See also: `setTxtProgressBar`, `getTxtProgressBar`
- `T1` **`setTxtProgressBar`** `(a: any, b: num) -> Empty` — invisibly, NULL (updates progress bar display)
  ```
  for (i in 1:100) setTxtProgressBar(pb, i)
  ```
  See also: `txtProgressBar`, `getTxtProgressBar`
- `T1` **`getTxtProgressBar`** `(a: any) -> num` — current value of progress bar
  ```
  getTxtProgressBar(pb)
  ```
  See also: `txtProgressBar`, `setTxtProgressBar`
- `T2` **`View`** `(a: any, b: char) -> any` — invisibly, the (potentially modified) data object
  ```
  View(mtcars)   # -> opens data viewer window
  ```
  See also: `edit`, `data.entry`
- `T2` **`history`** `(a: int, b: char, c: bool) -> [#N, char]` — character vector of history entries
  ```
  history(10)   # -> last 10 history entries
  ```
  See also: `savehistory`, `loadhistory`
- `T2` **`savehistory`** `(a: char) -> Empty` — invisibly, NULL
  ```
  savehistory()
  ```
  See also: `loadhistory`, `history`
- `T2` **`loadhistory`** `(a: char) -> Empty` — invisibly, NULL
  ```
  loadhistory()
  ```
  See also: `savehistory`, `history`
- `T2` **`browseEnv`** `(a: any, b: bool, c: char) -> Empty` — invisibly, NULL (prints to stdout)
  ```
  browseEnv()
  ```
  See also: `ls`, `objects`
- `T2` **`recover`** `(a: char) -> Empty` — invisibly, NULL (opens interactive debugger)
  ```
  recover()
  ```
  See also: `debugger`, `traceback`
- `T2` **`help`** `(a: char, b: char, c: char, d: char) -> Empty` — invisibly, NULL (opens help page)
  ```
  help("mean")   # -> opens help for mean
  ```
  See also: `example`, `vignette`
- `T2` **`example`** `(a: char, b: char, c: char) -> Empty` — invisibly, NULL (runs and prints example code)
  ```
  example("mean")
  ```
  See also: `help`
- `T2` **`vignette`** `(a: char, b: char, c: char) -> any` — vignette object (or NULL if not found)
  ```
  vignette("intro", package = "utils")
  ```
  See also: `browseVignettes`
- `T2` **`Sys.info`** `(a: char) -> any` — requested system information
  ```
  Sys.info()["sysname"]   # -> "Linux" or "Darwin" or "Windows"
  ```
  See also: `R.version`, `sessionInfo`
- `T1` **`object.size`** `(a: any) -> any` — approximate size in bytes (as a "number_of_bytes" object)
  ```
  object.size(mtcars)   # -> ~6-7 KB
  ```
  See also: `str`, `ls.str`
- `T1` **`glob2rx`** `(a: char, b: [#N, char], c: bool, d: bool) -> [#M, char]` — matching strings or logical vector
  ```
  glob2rx("*.csv")   # -> "^.*\\.csv$"
  ```
  See also: `grep`, `grepl`
- `T1` **`capture.output`** `(a: any, b: any) -> [#N, char]` — list of captured values
  ```
  capture.output(print("hello"))   # -> "hello"
  ```
  See also: `system`
- `T2` **`strOptions`** `() -> any` — formatted object
  ```
  strOptions()   # -> default options for str()
  ```
  See also: `str`
- `T2` **`strcapture`** `(a: char, b: [#N, char], c: any) -> any` — list of captured values converted to appropriate types
  ```
  strcapture("([0-9]+)x([0-9]+)", "1920x1080",
  ```
  See also: `regexpr`, `type.convert`
- `T2` **`toBibtex`** `(a: any, b: any) -> any` — character representation
  ```
  toBibtex(citation("base"))   # -> BibTeX entry
  ```
  See also: `toLatex`

