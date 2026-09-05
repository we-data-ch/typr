# typr_runner

![](img/rtypr.png)

## Info

A [TypR](https://github.com/fabriceHategekimana/typr)
 runner for RStudio that currently works on Windows and Linux (Ubuntu)
 
## Tutorial:

There is a short presentation [video](https://youtu.be/GMo20g__nOc) about this package.

## Installation

You should have TypR installed in your system. You can check [TypR](https://github.com/fabriceHategekimana/typr) for the main project written in Rust.

Juste run this on RStudio:

```r
devtools::install_github("fabriceHategekimana/typr_runner")
```

And now in your Addins you should be able to see "Run TypR file".
You should be inside a saved `.ty` file to run it.

# Usage

You can create a new project in a specific place with:

```r
typr.runner::new("/path/of/your/project/folder/project_name")
```

You just have to open your project like any normal RStudio project.

# Project's actions

## Running

You can click to the `Run TypR project` addin or use

```r
typr.runner::run()
```

## Building

You can click to the `Build TypR project` addin or use

```r
typr.runner::build()
```

## Checking

You can click to the `Check TypR project` addin or use

```r
typr.runner::check()
```

## Testing

You can click to the `Test TypR project` addin or use

```r
typr.runner::test()
```
