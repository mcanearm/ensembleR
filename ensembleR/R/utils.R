#' @title RMSE
#' @export
#' @param y A numeric vector of true y values
#' @param y_pred A numeric vector of predicted values.
#' @description
#' Helper RMSE function.
rmse <- function(y, y_pred) {
    sqrt(mean((y - y_pred)^2))
}
