#' @include std.R
.norm <- function(x, ...) UseMethod('.norm', x)
.norm <- function(x, ...) UseMethod('.norm', x)
.twice <- function(x, ...) UseMethod('.twice', x)
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
as_vec <- function(x, ...) UseMethod('as_vec', x)
#' @export
bsystem2 <- function(x, ...) UseMethod('bsystem2', x)
#' @export
colMeans <- function(x, ...) UseMethod('colMeans', x)
#' @export
colMeans <- function(x, ...) UseMethod('colMeans', x)
#' @export
colSums <- function(x, ...) UseMethod('colSums', x)
#' @export
colSums <- function(x, ...) UseMethod('colSums', x)
#' @export
colnames <- function(x, ...) UseMethod('colnames', x)
#' @export
contains <- function(x, ...) UseMethod('contains', x)
#' @export
cvec <- function(x, ...) UseMethod('cvec', x)
#' @export
derive <- function(x, ...) UseMethod('derive', x)
#' @export
dim <- function(x, ...) UseMethod('dim', x)
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
exec <- function(x, ...) UseMethod('exec', x)
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
filter <- function(x, ...) UseMethod('filter', x)
#' @export
fold <- function(x, ...) UseMethod('fold', x)
#' @export
get <- function(x, ...) UseMethod('get', x)
#' @export
getwd <- function(x, ...) UseMethod('getwd', x)
#' @export
is_none <- function(x, ...) UseMethod('is_none', x)
#' @export
is_some <- function(x, ...) UseMethod('is_some', x)
#' @export
join <- function(x, ...) UseMethod('join', x)
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
merge <- function(x, ...) UseMethod('merge', x)
#' @export
minus <- function(x, ...) UseMethod('minus', x)
#' @export
minus <- function(x, ...) UseMethod('minus', x)
#' @export
mul <- function(x, ...) UseMethod('mul', x)
#' @export
mul <- function(x, ...) UseMethod('mul', x)
#' @export
names <- function(x, ...) UseMethod('names', x)
#' @export
ncol <- function(x, ...) UseMethod('ncol', x)
#' @export
ncol <- function(x, ...) UseMethod('ncol', x)
#' @export
nlevels <- function(x, ...) UseMethod('nlevels', x)
#' @export
nrow <- function(x, ...) UseMethod('nrow', x)
#' @export
nrow <- function(x, ...) UseMethod('nrow', x)
#' @export
plot <- function(x, ...) UseMethod('plot', x)
#' @export
reduce <- function(x, ...) UseMethod('reduce', x)
#' @export
rowMeans <- function(x, ...) UseMethod('rowMeans', x)
#' @export
rowMeans <- function(x, ...) UseMethod('rowMeans', x)
#' @export
rowSums <- function(x, ...) UseMethod('rowSums', x)
#' @export
rowSums <- function(x, ...) UseMethod('rowSums', x)
#' @export
rownames <- function(x, ...) UseMethod('rownames', x)
#' @export
seq <- function(x, ...) UseMethod('seq', x)
#' @export
set <- function(x, ...) UseMethod('set', x)
#' @export
set_at <- function(x, ...) UseMethod('set_at', x)
#' @export
setwd <- function(x, ...) UseMethod('setwd', x)
#' @export
sort <- function(x, ...) UseMethod('sort', x)
#' @export
startsWith <- function(x, ...) UseMethod('startsWith', x)
#' @export
state <- function(x, ...) UseMethod('state', x)
#' @export
sum <- function(x, ...) UseMethod('sum', x)
#' @export
system2 <- function(x, ...) UseMethod('system2', x)
#' @export
t <- function(x, ...) UseMethod('t', x)
#' @export
to_native <- function(x, ...) UseMethod('to_native', x)
#' @export
to_nullable <- function(x, ...) UseMethod('to_nullable', x)
#' @export
unique <- function(x, ...) UseMethod('unique', x)
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

# ---- Generated `.default` fallbacks (see r_name_lint.rs) ----
# `colMeans` is not S3-generic in `base`: without this, the stub above
# would strand it ("no applicable method for 'colMeans'").
colMeans.default <- function(...) base::colMeans(...)
# `colSums` is not S3-generic in `base`: without this, the stub above
# would strand it ("no applicable method for 'colSums'").
colSums.default <- function(...) base::colSums(...)
# `colnames` is not S3-generic in `base`: without this, the stub above
# would strand it ("no applicable method for 'colnames'").
colnames.default <- function(...) base::colnames(...)
# `ncol` is not S3-generic in `base`: without this, the stub above
# would strand it ("no applicable method for 'ncol'").
ncol.default <- function(...) base::ncol(...)
# `nrow` is not S3-generic in `base`: without this, the stub above
# would strand it ("no applicable method for 'nrow'").
nrow.default <- function(...) base::nrow(...)
# `rowMeans` is not S3-generic in `base`: without this, the stub above
# would strand it ("no applicable method for 'rowMeans'").
rowMeans.default <- function(...) base::rowMeans(...)
# `rowSums` is not S3-generic in `base`: without this, the stub above
# would strand it ("no applicable method for 'rowSums'").
rowSums.default <- function(...) base::rowSums(...)
# `rownames` is not S3-generic in `base`: without this, the stub above
# would strand it ("no applicable method for 'rownames'").
rownames.default <- function(...) base::rownames(...)
# `dim` is a primitive that dispatches internally: the stub above would
# strand every class without a `dim.<class>` method, so restore it.
dim <- base::dim
# `names` is a primitive that dispatches internally: the stub above would
# strand every class without a `names.<class>` method, so restore it.
names <- base::names
