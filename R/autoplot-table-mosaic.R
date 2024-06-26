
#' Autoplot table mosaic
#'
#' Create a `ggplot2` object from a `table` with `ggmosaic`
#'
#' @param x A table
#'
#' @returns a `ggplot2` object
#' @export
autoplot_table_mosaic <- function(x) {
  require_namespace("rlang")
  require_namespace("ggmosaic")
  require_namespace("ggplot2")
  require_namespace("psych")

  d <- dim(x)
  stopifnot(
    inherits(x, "table"),
    length(d) == 2,
    d[1] == d[2]
  )

  dn <- dimnames(x)
  nm <- names(dn)

  rownames(x) <- as.numeric(factor(rownames(x)))
  colnames(x) <- as.numeric(factor(colnames(x)))

  # TODO this could probably be replaced
  df <- psych::table2df(x)
  colnames(df) <- nm
  df[] <- mapply(
    function(x, y) factor(x, labels = y),
    x = as.list(df),
    y = dn,
    SIMPLIFY = FALSE
  )

  x_arg <- rlang::sym(nm[1])
  y_arg <- rlang::sym(nm[2])

  # This has to be attached
  # https://github.com/haleyjeppson/ggmosaic/issues/58
  methods::getFunction("require")("ggmosaic", character.only = TRUE)

  ggplot2::ggplot(df) +
    ggmosaic::geom_mosaic(
      ggplot2::aes(x = ggmosaic::product(!!x_arg, !!y_arg), fill = !!x_arg)
    ) +
    ggplot2::scale_fill_discrete() +
    ggmosaic::theme_mosaic() +
    ggplot2::labs()
}
