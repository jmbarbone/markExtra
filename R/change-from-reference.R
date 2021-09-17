#' Calculate change from a visit
#'
#' Calculate change from visit or proportion of change from a visit
#'
#' @details These functions are vectorised so they may be used inside a
#'   (grouped) `data.frame` to calculate the change values.
#'
#'   `pchange_from_reference()` calculations a proportion of change from the
#'   reference, not a percentage, so it may need to be multiplied by `100` to
#'   achieve that.
#'
#'   `prop_from_reference()` is just an alias for `pchange_from_reference()`.
#'
#'   `get_reference_value()` returns the reference value but as a vector the
#'   same length as `x`.  This may be useful for creating a new column based on
#'   the reference value in a `data.frame`.
#'
#'   `add_change_from_reference()` and `add_pchange_from_reference()` will take
#'   a `data.frame` and append reference change values.
#'
#' @examples
#' visits <- c("Screening", "Baseline", "Week 2", "Week 4", "Week 6")
#' values <- c(3, 2, 3, 2, 4)
#'
#' df <- data.frame(
#'   values      = values,
#'   visits      = visits,
#'   cfb         = change_from_reference(values, visits, "Baseline"),
#'   pcfb        = pchange_from_reference(values, visits, "Baseline"),
#'   p_screening = pchange_from_reference(values, visits, "Week 2"),
#'   bl_value    = get_reference_value(values, visits)
#' )
#'
#' df
#' add_change_from_reference(df, "values", "visits")
#' add_pchange_from_reference(df, "values", "visits")
#'
#' # Appending to a wide data.frame
#'
#' df <- data.frame(
#'    Screening = c(1, 2, 3, 4),
#'    Baseline  = c(1, 2, 3, 4),
#'    `Week 2`  = c(1, 2, 3, 4),
#'    `Week 4`  = c(0, 1, 2, 1),
#'    `Week 6`  = c(0, -1, 0, 0),
#'    end_col   = letters[1:4],
#'    end_col2  = letters[1:4],
#'    check.names = FALSE
#' )
#'
#' df
#' add_change_from_reference_wide(df, "Baseline", paste("Week", c(2, 4, 6)))
#' add_change_from_reference_wide(df, cols = paste("Week", c(2, 4, 6)),
#'                                rearrange = "after")
#' add_change_from_reference_wide(df, "Baseline", paste("Week", c(2, 4, 6)),
#'                                percent = TRUE, rearrange = "end")
#' add_change_from_reference_wide(
#'   df, "Baseline", paste("Week", c(2, 4, 6)),
#'   percent = TRUE, rearrange = "end", collate = TRUE
#' )
#'
#' @name reference_change
NULL

#' @export
#' @rdname reference_change
#' @param x A vector of values or character name of column
#' @param reference A vector of references (i.e., such as visit names) or
#'   character name of column
#' @param point A scalar character of the reference (i.e., visit) (default:
#'   `"Baseline"`)
change_from_reference <- function(x, reference, point = "Baseline") {
  cfv_check(x, reference)
  x - x[find_reference(reference, point)]
}

#' @export
#' @rdname reference_change
pchange_from_reference <- function(x, reference, point = "Baseline") {
  cfv_check(x, reference)
  m <- find_reference(reference, point)
  (x - x[m]) / x[m]
}

#' @export
#' @rdname reference_change
prop_from_reference <- function(x, reference, point = "Baseline") {
  pchange_from_reference(x, reference, point)
}

#' @export
#' @rdname reference_change
get_reference_value <- function(x, reference, point = "Baseline") {
  cfv_check(x, reference)
  x[] <- x[find_reference(reference, point)]
  x
}

#' @export
#' @rdname reference_change
#' @param .data A `data.frame`
#' @param value The name of the value column
#' @param references A vector of column names to compute differences from
add_change_from_reference <- function(
  .data,
  values = "value",
  references = "VisitName",
  point = "Baseline"
) {
  stopifnot(is.data.frame(.data))
  .data[[paste0(values, "_change")]] <-
    change_from_reference(.data[[values]], .data[[references]], point)
  .data
}

#' @export
#' @rdname reference_change
add_pchange_from_reference <- function(
  .data,
  values = "value",
  references = "VisitName",
  point = "Baseline"
) {
  stopifnot(is.data.frame(.data))
  .data[[paste0(values, "_pchange")]] <-
    pchange_from_reference(.data[[values]], .data[[references]], point)
  .data
}

#' @export
#' @rdname reference_change
#' @param percent Logical, if `TRUE` will also calculate percent change
#' @param name A character vector to append to the new column names.  The second
#'   element will be used if `percent = TRUE`.
#' @param sep A character separation for the new column names
#' @param rearrange A method to change the arrangement of the `data.frame`
#'   columns with the new columns added: `end` will do nothing and append
#'   columns at very end; `immediate` will append columns immediate after each
#'   `col`; `after` will append columns after the last `col` entered.
#' @param collate For `percent = TRUE` and `rearrange = "after"`; will show the
#'   change from reference for all `cols` then the percent change.
add_change_from_reference_wide <- function(
  .data,
  point = "Baseline",
  cols,
  percent = FALSE,
  name = c("CFB", "%CFB"),
  sep = " ",
  rearrange = c("end", "immediate", "after"),
  collate = FALSE
) {
  rearrange <- mark::match_param(rearrange)

  cn <- colnames(.data)

  if (point %out% cn) {
    stop("Reference value ('",
      reference,
      "') not found in column names",
      call. = FALSE)
  }

  m <- match(cols, cn, nomatch = NA_integer_)
  bad <- is.na(m)
  if (any(bad)) {
    stop("Not all values in `cols` not found in column names \n",
      cols[bad],
      call. = FALSE)
  }

  if (!identical(m, m[seq(m)])) {
    warning("Columns are not in sequential order", call. = FALSE)
  }

  new_cols <- integer()

  for (i in cols) {
    cn <- colnames(.data)
    # pos <- which(i == cn)

    ref_change <- .data[[i]] - .data[[point]]
    n <- ncol(.data)
    .data[[n + 1]] <- ref_change
    names(.data)[n + 1] <- paste(i, name[1], sep = sep)
    new_cols <- c(new_cols, n + 1)


    if (percent) {
      .data[[n + 2]] <- 100 * ref_change / .data[[point]]
      names(.data)[n + 2] <- paste(i, name[2], sep = sep)
      new_cols <- c(new_cols, n + 2)
    }

    # data <- data[, c(1:pos, adds, unique((pos + 1):n))]
    # new_cols <- c(new_cols, adds)
  }


  if (collate & rearrange != "immediate") {
    last <- new_cols[length(new_cols)]
    new_cols <- c(
      seq.int(new_cols[1], last, by = 2),
      seq.int(new_cols[2], last, by = 2)
    )
  }
  # m <- 1:3
  # new_cols <- 7:12
  switch(
    rearrange,
    end = .data[, c(1:new_cols[1], new_cols[-1])],
    immediate = {
      new <- unique(as.vector(t(cbind(
        rep(m, each = (1 + percent)),
        new_cols
      )), "integer"))

      .data[, unique(c(1:new[1],
        new[-1],
        max(m):(min(new_cols) - 1),
        (max(new_cols) + 1):ncol(.data)))]
    },
    after = {
      end <- max(m)
      .data[, unique(c(1:end, new_cols, (end + 1):(new_cols[1] - 1)))]
    }
  )
}


# helpers -----------------------------------------------------------------

cfv_check <- function(x, reference) {
  if (length(x) != length(reference)) {
    stop("`x` and `reference` must be the same length", call. = FALSE)
  }

  invisible(NULL)
}

find_reference <- function(reference, point) {
  if (length(point) != 1L) {
    stop("`point` must be scalar", .call = FALSE)
  }

  m <- match(point, reference)

  if (length(m) > 1L) {
    warning("Multiple references matched \n  ",
      collapse0(m, sep = " & "),
      "\n  Using first reference")
    m <- m[1L]
  }

  if (length(m) == 0) {
    stop("point `", point, "` is not found in `reference`", call. = FALSE)
  }

  m
}
