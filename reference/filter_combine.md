# Filter combine

Filters a data frame then binds the result with it (with names).

## Usage

``` r
filter_combine(x, ..., .id, .names)
```

## Arguments

- x:

  A data.frame

- ...:

  Arguments passed on to
  [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)

  `.by`

  :   **\[experimental\]**

      \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
      Optionally, a selection of columns to group by for just this
      operation, functioning as an alternative to
      [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
      For details and examples, see
      [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html).

- .id:

  When .id is supplied, a new column of identifiers is created to link
  each row to its original data frame. The labels are taken from
  \`.names\`

- .names:

  vector of characters for the

## See also

\[dplyr::filter()\] and \[dplyr::bind_rows()\]
