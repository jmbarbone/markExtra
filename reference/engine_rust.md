# Engine - Rust

Set Rust engine for knitr

## Usage

``` r
engine_rust(options)

set_rust_engine()
```

## Arguments

- options:

  Options passed to code chunk

## Details

If dependencies are needed they should be defined in a \`cargo.toml\`
file in the working directory and the code chunk option \`toml=TRUE\`
should be set.

Additional options:

- timeout:

  Time (in seconds) to wait for program to finish

- env:

  Environment to execute code

## References

Adapted from https://greenwood.dev/2019/12/22/running-rust-in-rmd/
