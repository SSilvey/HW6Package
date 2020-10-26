#' The Trimmed Mean
#'
#' This function allows you to compute the trimmed mean of a numeric vector.
#' @param X is the vector of numeric values.
#' @param s is the number of low values to cut off.
#' @param l is the number of high values to cut off.
#' @keywords Trimmed Mean
#' @export
#' @examples Trimmed_Mean(iris$Sepal.Length, 1, 1)

Trimmed_Mean <- function(X, s, l) {
  ifelse(length(X) >= s + l + 1, mean(sort(X)[s+1:length(X)-l]),
         stop("Vector is not of appropriate length."))
}
