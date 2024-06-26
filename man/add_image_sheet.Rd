% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-openxlsx.R
\name{add_image_sheet}
\alias{add_image_sheet}
\title{Add image to sheet}
\usage{
add_image_sheet(wb, file, sheetname, ..., override = TRUE)
}
\arguments{
\item{wb}{A Workbook object to attach the new worksheet and table}

\item{file}{An image file. Valid file types are: jpeg, png, bmp}

\item{sheetname}{The worksheet to write to. Can be the worksheet index or
name.}

\item{...}{Additional arguments passed to openxlsx::insertImage}

\item{override}{Logical.  If TRUE, will delete the sheetname (if present)}
}
\description{
A wrapper function to..
}
