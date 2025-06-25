# Put Alias aside for subtyping
# Create Error msg for the if branches
# Module name with a minuscule caps
# Operation order manager with ".", "+", etc.

# Make an interface behave more like a Generic 
if (Addable, Addable) -> Addable is an interface
then (int, int) -> int should be allowed if i implement add

# Factor implementation
By default, function like `read.csv()` and `data.frame()` automaticaly convert character vectors to factors. Automatically add `stringAsFactors = FALSE` in the parameters.

# Set by default a: options(warnPartialMatchDollar = TRUE)

# Use x[[i]] <- NULL to remove the field i of the list x

# For loops
An inside variable can rewritte a global variable

# On exit
Use on.exit([parameter], add = TRUE) to clean up the environment

# Encapsulated OOP VS functional OOP

# Check R.oo package for a mutable S3

# About S3
Simple OO System
Offer freedom and extensibility
TypR automatically add and manage constrains in a less restrictive manner
Can't explicitelly change a object's class
- Find a way to implement those elements:
	- Constructor DONE
	- Validator
	- Helper

- Safe convention for inheritance and subtyping: structural typing
