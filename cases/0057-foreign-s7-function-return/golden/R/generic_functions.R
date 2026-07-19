#' @include std.R
#' @export
add <- function(x, ...) UseMethod('add', x)
#' @export
add <- function(x, ...) UseMethod('add', x)
#' @export
annotate_factor <- function(x, ...) UseMethod('annotate_factor', x)
#' @export
as__character <- function(x, ...) UseMethod('as__character', x)
#' @export
as__character <- function(x, ...) UseMethod('as__character', x)
#' @export
as__integer <- function(x, ...) UseMethod('as__integer', x)
#' @export
as__logical <- function(x, ...) UseMethod('as__logical', x)
#' @export
as__numeric <- function(x, ...) UseMethod('as__numeric', x)
#' @export
as_vec <- function(x, ...) UseMethod('as_vec', x)
#' @export
contains <- function(x, ...) UseMethod('contains', x)
#' @export
cvec <- function(x, ...) UseMethod('cvec', x)
#' @export
derive <- function(x, ...) UseMethod('derive', x)
#' @export
dir <- function(x, ...) UseMethod('dir', x)
#' @export
dir__create <- function(x, ...) UseMethod('dir__create', x)
#' @export
div <- function(x, ...) UseMethod('div', x)
#' @export
div <- function(x, ...) UseMethod('div', x)
#' @export
dot <- function(x, ...) UseMethod('dot', x)
#' @export
endsWith <- function(x, ...) UseMethod('endsWith', x)
#' @export
expect <- function(x, ...) UseMethod('expect', x)
#' @export
expect_equal <- function(x, ...) UseMethod('expect_equal', x)
#' @export
expect_false <- function(x, ...) UseMethod('expect_false', x)
#' @export
extend <- function(x, ...) UseMethod('extend', x)
#' @export
factor <- function(x, ...) UseMethod('factor', x)
#' @export
file__copy <- function(x, ...) UseMethod('file__copy', x)
#' @export
file__create <- function(x, ...) UseMethod('file__create', x)
#' @export
file__exists <- function(x, ...) UseMethod('file__exists', x)
#' @export
file__remove <- function(x, ...) UseMethod('file__remove', x)
#' @export
file__rename <- function(x, ...) UseMethod('file__rename', x)
#' @export
fold <- function(x, ...) UseMethod('fold', x)
#' @export
get <- function(x, ...) UseMethod('get', x)
#' @export
get_counter <- function(x, ...) UseMethod('get_counter', x)
#' @export
getwd <- function(x, ...) UseMethod('getwd', x)
#' @export
grepl <- function(x, ...) UseMethod('grepl', x)
#' @export
gsub <- function(x, ...) UseMethod('gsub', x)
#' @export
is_none <- function(x, ...) UseMethod('is_none', x)
#' @export
is_some <- function(x, ...) UseMethod('is_some', x)
#' @export
join <- function(x, ...) UseMethod('join', x)
#' @export
len <- function(x, ...) UseMethod('len', x)
#' @export
levels <- function(x, ...) UseMethod('levels', x)
#' @export
list__files <- function(x, ...) UseMethod('list__files', x)
#' @export
lvec <- function(x, ...) UseMethod('lvec', x)
#' @export
map <- function(x, ...) UseMethod('map', x)
#' @export
map <- function(x, ...) UseMethod('map', x)
#' @export
max <- function(x, ...) UseMethod('max', x)
#' @export
mean <- function(x, ...) UseMethod('mean', x)
#' @export
min <- function(x, ...) UseMethod('min', x)
#' @export
minus <- function(x, ...) UseMethod('minus', x)
#' @export
minus <- function(x, ...) UseMethod('minus', x)
#' @export
mul <- function(x, ...) UseMethod('mul', x)
#' @export
mul <- function(x, ...) UseMethod('mul', x)
#' @export
nchar <- function(x, ...) UseMethod('nchar', x)
#' @export
nlevels <- function(x, ...) UseMethod('nlevels', x)
#' @export
plot <- function(x, ...) UseMethod('plot', x)
#' @export
print <- function(x, ...) UseMethod('print', x)
#' @export
reduce <- function(x, ...) UseMethod('reduce', x)
#' @export
replace <- function(x, ...) UseMethod('replace', x)
#' @export
replace_all <- function(x, ...) UseMethod('replace_all', x)
#' @export
rev <- function(x, ...) UseMethod('rev', x)
#' @export
sd <- function(x, ...) UseMethod('sd', x)
#' @export
seq <- function(x, ...) UseMethod('seq', x)
#' @export
set <- function(x, ...) UseMethod('set', x)
#' @export
set_at <- function(x, ...) UseMethod('set_at', x)
#' @export
setwd <- function(x, ...) UseMethod('setwd', x)
#' @export
split <- function(x, ...) UseMethod('split', x)
#' @export
startsWith <- function(x, ...) UseMethod('startsWith', x)
#' @export
state <- function(x, ...) UseMethod('state', x)
#' @export
strsplit <- function(x, ...) UseMethod('strsplit', x)
#' @export
sub <- function(x, ...) UseMethod('sub', x)
#' @export
substr <- function(x, ...) UseMethod('substr', x)
#' @export
sum <- function(x, ...) UseMethod('sum', x)
#' @export
sys__date <- function(x, ...) UseMethod('sys__date', x)
#' @export
sys__getenv <- function(x, ...) UseMethod('sys__getenv', x)
#' @export
sys__info <- function(x, ...) UseMethod('sys__info', x)
#' @export
sys__setenv <- function(x, ...) UseMethod('sys__setenv', x)
#' @export
sys__setlocale <- function(x, ...) UseMethod('sys__setlocale', x)
#' @export
sys__sleep <- function(x, ...) UseMethod('sys__sleep', x)
#' @export
sys__time <- function(x, ...) UseMethod('sys__time', x)
#' @export
sys__timezone <- function(x, ...) UseMethod('sys__timezone', x)
#' @export
sys__which <- function(x, ...) UseMethod('sys__which', x)
#' @export
t <- function(x, ...) UseMethod('t', x)
#' @export
to_native <- function(x, ...) UseMethod('to_native', x)
#' @export
to_nullable <- function(x, ...) UseMethod('to_nullable', x)
#' @export
tolower <- function(x, ...) UseMethod('tolower', x)
#' @export
toupper <- function(x, ...) UseMethod('toupper', x)
#' @export
unlink <- function(x, ...) UseMethod('unlink', x)
#' @export
unwrap <- function(x, ...) UseMethod('unwrap', x)
#' @export
unwrap_or <- function(x, ...) UseMethod('unwrap_or', x)
#' @export
update <- function(x, ...) UseMethod('update', x)
#' @export
version <- function(x, ...) UseMethod('version', x)
