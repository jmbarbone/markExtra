#' Calculate change from a visit
#'
#' Calculate change from visit or proportion of change from a visit
#'
#' @details
#' These functions are vectorised so t/hey may be used inside a (grouped) data
#'   frame to calculate the change scores.
#'
#' `pchange_from_visit()` calculations a proportion of change from baseline, not
#'   a percentage, so it may need to be multiplied by `100` to achieve that.
#'
#' `prop_from_visit()` is just an alias for `pchange_from_visit()`.
#'
#' `get_reference_value()` returns the reference score but as a vector the same
#'   length as `x`.  This may be useful for creating a new column based on the
#'   reference value in a dataframe.
#'
#' `add_change_from_visit()` and `add_pchange_from_visit()` will take a
#'   data.frame and append reference change values.
#'
#' @param .data A data.frame
#' @param x A vector of values or character name of column
#' @param value The name of the value column
#' @param visits A vector of visits or character name of column
#' @param reference A scalar character of visit (default: `"Baseline"`)
#' @param cols A vector of column names to compute differences from
#' @param percent Logical, if `TRUE` will also calculate percent change
#' @param name A character vector to append to the new column names.  The second
#'   element will be used if `percent = TRUE`.
#' @param sep A character separation for the new column names
#' @param rearrange A method to change the arrangement of the data.frame columns
#'   with the new columns added: `end` will do nothing and append columns at
#'   very end; `immediate` will append columns immediate after each `col`;
#'   `after` will append columns after the last `col` entered.
#' @param collate For `percent = TRUE` and `rearrange = "after"`; will show the
#'   change from reference for all `cols` then the percent change.
#'
#' @examples
#' visits <- c("Screening", "Baseline", "Week 2", "Week 4", "Week 6")
#' values <- c(3, 2, 3, 2, 4)
#'
#' df <- data.frame(
#'   values      = values,
#'   visits      = visits,
#'   cfb         = change_from_visit(values, visits, "Baseline"),
#'   pcfb        = pchange_from_visit(values, visits, "Baseline"),
#'   p_screening = pchange_from_visit(values, visits, "Week 2"),
#'   bl_value    = get_reference_value(values, visits)
#' )
#'
#' df
#' add_change_from_visit(df, "values", "visits")
#' add_pchange_from_visit(df, "values", "visits")
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
#' @export
#' @name reference_change
change_from_visit <- function(x, visits, reference = "Baseline") {
  cfv_check(x, visits)
  x - x[find_reference(visits, reference)]
}

#' @export
#' @rdname reference_change
pchange_from_visit <- function(x, visits, reference = "Baseline") {
  cfv_check(x, visits)
  m <- find_reference(visits, reference)
  (x - x[m]) / x[m]
}

#' @export
#' @rdname reference_change
prop_from_visit <- function(x, visits, reference = "Baseline") {
  pchange_from_visit(x, visits, reference)
}

#' @export
#' @rdname reference_change
get_reference_value <- function(x, visits, reference = "Baseline") {
  cfv_check(x, visits)
  x[] <- x[find_reference(visits, reference)]
  x
}

#' @export
#' @rdname reference_change
add_change_from_visit <- function(
  .data,
  value = "value",
  visits = "VisitName",
  reference = "Baseline"
) {
  stopifnot(is.data.frame(.data))
  .data[[paste0(value, "_change")]] <-
    change_from_visit(.data[[value]], .data[[visits]], reference)
  .data
}

#' @export
#' @rdname reference_change
add_pchange_from_visit <- function(
  .data,
  value = "value",
  visits = "VisitName",
  reference = "Baseline"
) {
  stopifnot(is.data.frame(.data))
  .data[[paste0(value, "_pchange")]] <-
    pchange_from_visit(.data[[value]], .data[[visits]], reference)
  .data
}

#' @export
#' @rdname reference_change
add_change_from_reference_wide <- function(
  .data,
  reference = "Baseline",
  cols,
  percent = FALSE,
  name = c("CFB", "%CFB"),
  sep = " ",
  rearrange = c("end", "immediate", "after"),
  collate = FALSE
) {
  rearrange <- mark::match_param(rearrange)

  cn <- colnames(.data)

  if (reference %out% cn) {
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

    ref_change <- .data[[i]] - .data[[reference]]
    n <- ncol(.data)
    .data[[n + 1]] <- ref_change
    names(.data)[n + 1] <- paste(i, name[1], sep = sep)
    new_cols <- c(new_cols, n + 1)


    if (percent) {
      .data[[n + 2]] <- 100 * ref_change / .data[[reference]]
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

cfv_check <- function(x, visits) {
  if (length(x) != length(visits)) {
    stop("`x` and `visit` must be the same length", call. = FALSE)
  }

  invisible(NULL)
}

find_reference <- function(visits, reference) {
  if (length(reference) != 1L) {
    stop("`reference` must be scalar", .call = FALSE)
  }

  m <- match(reference, visits)

  if (length(m) > 1L) {
    warning("Multiple visits matched \n  ",
      collapse0(m, sep = " & "),
      "\n  Using first visit")
    m <- m[1L]
  }

  if (length(m) == 0) {
    stop("reference `", reference, "` is not found in `visits`", call. = FALSE)
  }

  m
}
