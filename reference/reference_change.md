# Calculate change from a visit

Calculate change from visit or proportion of change from a visit

## Usage

``` r
change_from_reference(x, reference, point = "Baseline")

pchange_from_reference(x, reference, point = "Baseline")

prop_from_reference(x, reference, point = "Baseline")

get_reference_value(x, reference, point = "Baseline")

add_change_from_reference(
  .data,
  values = "value",
  references = "VisitName",
  point = "Baseline"
)

add_pchange_from_reference(
  .data,
  values = "value",
  references = "VisitName",
  point = "Baseline"
)

add_change_from_reference_wide(
  .data,
  point = "Baseline",
  cols,
  percent = FALSE,
  name = c("CFB", "%CFB"),
  sep = " ",
  rearrange = c("end", "immediate", "after"),
  collate = FALSE
)
```

## Arguments

- x:

  A vector of values or character name of column

- reference:

  A vector of references (i.e., such as visit names) or character name
  of column

- point:

  A scalar character of the reference (i.e., visit) (default:
  \`"Baseline"\`)

- .data:

  A \`data.frame\`

- values:

  The name of the value column

- references:

  A vector of column names to compute differences from

- cols:

  A character vector of column names

- percent:

  Logical, if \`TRUE\` will also calculate percent change

- name:

  A character vector to append to the new column names. The second
  element will be used if \`percent = TRUE\`.

- sep:

  A character separation for the new column names

- rearrange:

  A method to change the arrangement of the \`data.frame\` columns with
  the new columns added: \`end\` will do nothing and append columns at
  very end; \`immediate\` will append columns immediate after each
  \`col\`; \`after\` will append columns after the last \`col\` entered.

- collate:

  For \`percent = TRUE\` and \`rearrange = "after"\`; will show the
  change from reference for all \`cols\` then the percent change.

## Details

These functions are vectorised so they may be used inside a (grouped)
\`data.frame\` to calculate the change values.

\`pchange_from_reference()\` calculations a proportion of change from
the reference, not a percentage, so it may need to be multiplied by
\`100\` to achieve that.

\`prop_from_reference()\` is just an alias for
\`pchange_from_reference()\`.

\`get_reference_value()\` returns the reference value but as a vector
the same length as \`x\`. This may be useful for creating a new column
based on the reference value in a \`data.frame\`.

\`add_change_from_reference()\` and \`add_pchange_from_reference()\`
will take a \`data.frame\` and append reference change values.

## Examples

``` r
visits <- c("Screening", "Baseline", "Week 2", "Week 4", "Week 6")
values <- c(3, 2, 3, 2, 4)

df <- data.frame(
  values      = values,
  visits      = visits,
  cfb         = change_from_reference(values, visits, "Baseline"),
  pcfb        = pchange_from_reference(values, visits, "Baseline"),
  p_screening = pchange_from_reference(values, visits, "Week 2"),
  bl_value    = get_reference_value(values, visits)
)

df
#>   values    visits cfb pcfb p_screening bl_value
#> 1      3 Screening   1  0.5   0.0000000        2
#> 2      2  Baseline   0  0.0  -0.3333333        2
#> 3      3    Week 2   1  0.5   0.0000000        2
#> 4      2    Week 4   0  0.0  -0.3333333        2
#> 5      4    Week 6   2  1.0   0.3333333        2
add_change_from_reference(df, "values", "visits")
#>   values    visits cfb pcfb p_screening bl_value values_change
#> 1      3 Screening   1  0.5   0.0000000        2             1
#> 2      2  Baseline   0  0.0  -0.3333333        2             0
#> 3      3    Week 2   1  0.5   0.0000000        2             1
#> 4      2    Week 4   0  0.0  -0.3333333        2             0
#> 5      4    Week 6   2  1.0   0.3333333        2             2
add_pchange_from_reference(df, "values", "visits")
#>   values    visits cfb pcfb p_screening bl_value values_pchange
#> 1      3 Screening   1  0.5   0.0000000        2            0.5
#> 2      2  Baseline   0  0.0  -0.3333333        2            0.0
#> 3      3    Week 2   1  0.5   0.0000000        2            0.5
#> 4      2    Week 4   0  0.0  -0.3333333        2            0.0
#> 5      4    Week 6   2  1.0   0.3333333        2            1.0

# Appending to a wide data.frame

df <- data.frame(
   Screening = c(1, 2, 3, 4),
   Baseline  = c(1, 2, 3, 4),
   `Week 2`  = c(1, 2, 3, 4),
   `Week 4`  = c(0, 1, 2, 1),
   `Week 6`  = c(0, -1, 0, 0),
   end_col   = letters[1:4],
   end_col2  = letters[1:4],
   check.names = FALSE
)

df
#>   Screening Baseline Week 2 Week 4 Week 6 end_col end_col2
#> 1         1        1      1      0      0       a        a
#> 2         2        2      2      1     -1       b        b
#> 3         3        3      3      2      0       c        c
#> 4         4        4      4      1      0       d        d
add_change_from_reference_wide(df, "Baseline", paste("Week", c(2, 4, 6)))
#>   Screening Baseline Week 2 Week 4 Week 6 end_col end_col2 Week 2 CFB
#> 1         1        1      1      0      0       a        a          0
#> 2         2        2      2      1     -1       b        b          0
#> 3         3        3      3      2      0       c        c          0
#> 4         4        4      4      1      0       d        d          0
#>   Week 4 CFB Week 6 CFB
#> 1         -1         -1
#> 2         -1         -3
#> 3         -1         -3
#> 4         -3         -4
add_change_from_reference_wide(df, cols = paste("Week", c(2, 4, 6)),
                               rearrange = "after")
#>   Screening Baseline Week 2 Week 4 Week 6 Week 2 CFB Week 4 CFB Week 6 CFB
#> 1         1        1      1      0      0          0         -1         -1
#> 2         2        2      2      1     -1          0         -1         -3
#> 3         3        3      3      2      0          0         -1         -3
#> 4         4        4      4      1      0          0         -3         -4
#>   end_col end_col2
#> 1       a        a
#> 2       b        b
#> 3       c        c
#> 4       d        d
add_change_from_reference_wide(df, "Baseline", paste("Week", c(2, 4, 6)),
                               percent = TRUE, rearrange = "end")
#>   Screening Baseline Week 2 Week 4 Week 6 end_col end_col2 Week 2 CFB
#> 1         1        1      1      0      0       a        a          0
#> 2         2        2      2      1     -1       b        b          0
#> 3         3        3      3      2      0       c        c          0
#> 4         4        4      4      1      0       d        d          0
#>   Week 2 %CFB Week 4 CFB Week 4 %CFB Week 6 CFB Week 6 %CFB
#> 1           0         -1  -100.00000         -1        -100
#> 2           0         -1   -50.00000         -3        -150
#> 3           0         -1   -33.33333         -3        -100
#> 4           0         -3   -75.00000         -4        -100
add_change_from_reference_wide(
  df, "Baseline", paste("Week", c(2, 4, 6)),
  percent = TRUE, rearrange = "end", collate = TRUE
)
#>   Screening Baseline Week 2 Week 4 Week 6 end_col end_col2 Week 2 CFB
#> 1         1        1      1      0      0       a        a          0
#> 2         2        2      2      1     -1       b        b          0
#> 3         3        3      3      2      0       c        c          0
#> 4         4        4      4      1      0       d        d          0
#>   Week 4 CFB Week 6 CFB Week 2 %CFB Week 4 %CFB Week 6 %CFB
#> 1         -1         -1           0  -100.00000        -100
#> 2         -1         -3           0   -50.00000        -150
#> 3         -1         -3           0   -33.33333        -100
#> 4         -3         -4           0   -75.00000        -100
```
