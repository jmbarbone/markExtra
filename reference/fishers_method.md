# Fisher's method for combined probabilities

Computes Fisher's method for combined probabilities

## Usage

``` r
fishers_method(x, zeros = FALSE)
```

## Arguments

- x:

  A vector of p-values.

- zeros:

  Logical, if \`TRUE\` will use zeros in calculation, otherwise recoded
  as \`.Machine\$double.xmin\`. This still will produce a very small
  result but will be less likely to produce \`0\`

## Details

Values greater than 1 and less than 0 are removed from the calculation.
Values of 0 are red
