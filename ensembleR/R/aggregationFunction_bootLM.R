#' @title Fit Aggregation function by bootLM fit
#' @export
#' @description Function Fit a standard linear model to the data for aggregation.
#' @inheritParams fitAggregationFunction
#' @note
#' This method is not intended to be used on its own, but rather used through
#' the \link[ensembleR]{fitAggregationFunction} function.
#' @param boot_iter The number of bootstrap iterations to perform.
fitAggregationFunction_bootLM <- function(Y, y_hat, boot_iter=1000, ...) {

    x <- as.matrix(cbind(1, y_hat))  # fit an intercept

    beta_mat <- replicate(boot_iter, expr={
        idx <- sample(1:nrow(x), replace=TRUE)
        x_mini <- x[idx, ]
        y_mini <- Y[idx]
        u <- chol(crossprod(x_mini, x_mini))
        betas <- backsolve(u, forwardsolve(t(u), t(x_mini) %*% y_mini))[, 1]
    }, simplify = TRUE)

    val_preds <- x %*% beta_mat
    val_resid <- Y - val_preds

    # loop hiding, but ok
    resid_scale <- apply(Y - val_preds, 2, sd)

    # degrees of freedom based on estimating the beta coefficients, intercept,
    # and residual standard deviation
    d_free <- nrow(y_hat) - ncol(y_hat) - 2

    # TODO: consider modifying the bias term and/or fitting the residuals directly
    structure(list("betas"=beta_mat, "scale"=resid_scale, "df"=d_free),
        class = c("ModelAggregator_bootLM", "list")
    )
}


#' @title Predict method for the bootLM aggregation function
#' @export
#' @describeIn fitAggregationFunction_bootLM S3 prediction method for the
#' bootLM aggregation function
#' @inheritParams fitAggregationFunction_EM
predict.ModelAggregator_bootLM <- function(object, y_hat, alpha=0.05, ...) {
    x <- as.matrix(cbind(1, y_hat))
    means <- crossprod(object$betas, t(x))

    lower <- means + qt(alpha/2, object$df, lower.tail = TRUE) * object$scale
    upper <- means + qt(alpha/2, object$df, lower.tail = FALSE) * object$scale

    pred_sd <- apply(means, 2, sd)

    cbind(
        'mean' = colMeans(means),
        'lower' = colMeans(lower),
        'upper' = colMeans(upper),
        'pred_sd' = pred_sd
    )
}
