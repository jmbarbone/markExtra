# Test a git repository

Test a git repository

## Usage

``` r
test_git_repo(
  x,
  ...,
  branch = "master",
  test_fun = devtools::test,
  options = NULL
)
```

## Arguments

- x:

  A project directory

- ...:

  Additional arguments passed to test_fun

- branch:

  The branch to copy from the repository (default: "master")

- test_fun:

  A function to apply for testing (default: "devtools::test"); Note: the
  copied directory for testing is passed as the first argument

- options:

  A named list of \`options()\` to temporarily set
