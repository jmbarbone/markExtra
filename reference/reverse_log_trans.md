# Reverse log transformation for ggplot

Show a reversed order for a log transformed scale from ggplot. Ripped
from:
https://stackoverflow.com/questions/11053899/how-to-get-a-reversed-log10-scale-in-ggplot2

## Usage

``` r
reverse_log_trans(base = exp(1))
```

## Arguments

- base:

  a positive or complex number: the base with respect to which
  logarithms are computed. Defaults to \\e\\=`exp(1)`.

## Author

Brian Diggs (https://stackoverflow.com/users/892313/brian-diggs)
