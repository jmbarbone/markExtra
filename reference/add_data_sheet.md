# Wrappers for openxlsx package functions

Add data to sheet

## Usage

``` r
add_data_sheet(
  wb,
  data,
  sheetname,
  tableStyle = "TableStyleLight8",
  bandedRows = TRUE,
  bandedCols = TRUE,
  ...,
  override = TRUE
)
```

## Arguments

- wb:

  A Workbook object to attach the new worksheet and table

- data:

  A dataframe.

- sheetname:

  The worksheet to write to. Can be the worksheet index or name.

- tableStyle:

  Any excel table style name or "none".

- bandedRows:

  Logical. If TRUE, rows are colour banded

- bandedCols:

  Logical. If TRUE, the columns are colour banded

- ...:

  Additional arguments passed to openxlsx::writeDataTable that are not
  already listed

- override:

  Logical. If TRUE, will delete the sheetname (if present)

## Details

A wrapper function to use within the openxlsx workbook building. This
adds additional functionality to override within the function and
provide an output that can be piped.

These should be applied to a string starting with
\[openxlsx::createWorkbook()\] then piped through with the functions
below.
