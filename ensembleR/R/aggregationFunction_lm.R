#' @title Fit Aggregation function by simple LM fit
#' @export
#' @description Fit a standard linear model to the data for aggregation.
#' @inheritParams fitAggregationFunction
#' @import stats
#' @inherit fitAggregationFunction_bootLM note
fitAggregationFunction_lm <- function(Y, y_hat, ...) {
    # TODO: Instead of calibrating based on a single validation set, use cross validation
    # to estimate the calibration parameters around:
    # 1. The standard deviation estimates
    # 2. A final "stddev discount" parameter that optimizes for the coverage requested.
    agg_lm <- lm(Y ~ ., data=cbind.data.frame(Y, y_hat))

    # calibrator <- ensembleR::fitCalibrator(Y, agg_lm$fitted.values, y_hat)
    # TODO: consider modifying the bias term and/or fitting the residuals directly
    structure(
        list(model=agg_lm),
        class = c("ModelAggregator_lm", "list")
    )
}

#' @export
#' @describeIn fitAggregationFunction_lm predict method for the LM aggregation function
#' @import stats
predict.ModelAggregator_lm <- function(object, y_hat, alpha = 0.05, ...) {
    colnames(y_hat) <- names(object$model$coefficients)[2:length(object$model$coefficients)]

    pred_sd <- sd(object$model$residuals)
    interval_preds <- predict(
        object$model,
        as.data.frame(y_hat),
        se.fit = TRUE,
        interval = "prediction",
        level = 1-alpha,
    )

    df.resid <- object$model$df.residual
    out <- cbind(
        "mean"=interval_preds$fit[, "fit"],
        "lower"=interval_preds$fit[, "fit"] + qt(alpha/2, df.resid) * (pred_sd + interval_preds$se.fit),
        "upper"=interval_preds$fit[, "fit"] + qt(1-alpha/2, df.resid) * (pred_sd + interval_preds$se.fit),
        "sd" = pred_sd + interval_preds$se.fit
    )
    out
}
