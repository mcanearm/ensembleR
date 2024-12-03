#' @title RMSE
#' @export
#' @description
#' Helper RMSE function.
#'
rmse <- function(y, y_hat) {
    sqrt(mean((y - y_hat)^2))
}
