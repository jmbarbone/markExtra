#' Weighted score
#'
#' Weight a score against a population
#'
#' @param score A vector of values
#' @param weights Weights applied to each score
#' @param reference A reference value to compare against weights; when `weights`
#'   are lower than `needed`, the final score is biased towards `population`.
#' @param comparison A comparison value; a population mean or other value
#' @examples
#' scores <- rep(seq(0, 1, .1), each = 10)
#' weights <- rep(1:10, 10)
#' weighted <- weighted_score(scores, weights)
#' plot(weighted, xlab = "inded", ylab = "Weighted score")
#' plot(
#'   weighted_score(
#'     rep(seq(0, 1, .01))
#'   )
#' )
#' @export
weighted_score <- function(
    score,
    weights = 1,
    reference = mean(weights, na.rm = TRUE),
    comparison = mean(score, na.rm = TRUE)
) {
  weights / (weights + reference) * score +
    reference / (weights + reference) * comparison
}