#' @title Fit Aggregation function by simple LM fit
#' @export
#' @describeIn fitAggregationFunction Fit a standard linear model to the data for aggregation.
fitAggregationFunction_lm <- function(Y, y_hat, calibrate_sd=FALSE, ...) {
    # TODO: Instead of calibrating based on a single validation set, use cross validation
    # to estimate the calibration parameters around:
    # 1. The standard deviation estimates
    # 2. A final "stddev discount" parameter that optimizes for the coverage requested.
    agg_lm <- lm(Y ~ ., data=cbind.data.frame(Y, y_hat))

    # deprecated functionality
    if (FALSE) {
        sd_model <- ensembleR::fitCalibrator(Y, agg_lm$fitted.values, y_hat)
    } else {
        sd_model <- NULL
    }

    # calibrator <- ensembleR::fitCalibrator(Y, agg_lm$fitted.values, y_hat)
    # TODO: consider modifying the bias term and/or fitting the residuals directly
    structure(
        list(model=agg_lm, sd_model=sd_model),
        class = c("ModelAggregator_lm", "list")
    )
}

#' @export
#' @describeIn fitAggregationFunction predict method for the LM aggregation function
predict.ModelAggregator_lm <- function(obj, y_hat, alpha = 0.05, ...) {
    colnames(y_hat) <- names(obj$model$coefficients)[2:length(obj$model$coefficients)]

    if (!is.null(obj$sd_model)) {
        pred_sd <- predict(obj$sd_model, y_hat)
        pred_sd <- ifelse(pred_sd < 0, obj$model$residual.scale, pred_sd)
    } else {
        pred_sd <- sd(obj$model$residuals)
    }
    interval_preds <- predict(
        obj$model,
        as.data.frame(y_hat),
        se.fit = TRUE,
        interval = "prediction",
        level = 1-alpha,
    )

    df.resid <- obj$model$df.residual
    out <- cbind(
        "mean"=interval_preds$fit[, "fit"],
        "lower"=interval_preds$fit[, "fit"] + qt(alpha/2, df.resid) * (pred_sd + interval_preds$se.fit),
        "upper"=interval_preds$fit[, "fit"] + qt(1-alpha/2, df.resid) * (pred_sd + interval_preds$se.fit),
        "sd" = pred_sd + interval_preds$se.fit
    )
    out
}
