# Add image to sheet

A wrapper function to..

## Usage

``` r
add_image_sheet(wb, file, sheetname, ..., override = TRUE)
```

## Arguments

- wb:

  A Workbook object to attach the new worksheet and table

- file:

  An image file. Valid file types are: jpeg, png, bmp

- sheetname:

  The worksheet to write to. Can be the worksheet index or name.

- ...:

  Additional arguments passed to openxlsx::insertImage

- override:

  Logical. If TRUE, will delete the sheetname (if present)
