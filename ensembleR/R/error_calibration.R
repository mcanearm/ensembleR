#' @title Calibrate SD Estimates
#' @export
#' @description Take the outputs of the aggregator and using this as a mean, fit
#' the predicted standard deviation of the residuals.
#' TODO: EXPAND
fitCalibrator <- function(y_true, aggregated_preds, y_hat, sigma_est, n_smoother=25, ...) {
    # TODO: perform error calibration using nearest neighbors - get closest N to estimate stddev for
    # each observation, then fit a sigmoid curve to reduce the RMSE between our estimates of sigma
    resid <- y_true-aggregated_preds
    fit_df <- cbind.data.frame(y_true, aggregated_preds, y_hat, resid)

    ordered_df <- fit_df[order(fit_df$aggregated_preds), ]
    rolling_sd <- zoo::rollapply(ordered_df$resid, width=n_smoother, FUN=sd, by=1, by.column=FALSE, fill=NA)

    keep_cols <- setdiff(colnames(ordered_df), c("y_true", "aggregated_preds", "resid"))

    keep_col_formula <- paste(sprintf("splines::bs(%s)", keep_cols), collapse=" + ")
    formula <- as.formula(paste("sd_est ~ ", keep_col_formula))
    sd_df <- cbind.data.frame(ordered_df[, keep_cols], sd_est=rolling_sd)
    sd_model <- lm(data=sd_df, formula=formula)

    plot(sd_model$model$sd_est, sd_model$fitted.values)
    structure(list(sd_model=sd_model), class="intervalCalibrator")
}

#' @export
predict.intervalCalibrator <- function(obj, y_hat, alpha=0.05) {
    pred_sd <- predict(obj$sd_model, newdata=data.frame(y_hat))
    pred_sd
}


#' @title Fit Quantile Bounds
#' @export
fitQuantileBounds <- function(y_true, y_hat, tau=seq(0, 1, by=0.01)) {
    # Fit lower bound
    quantreg::rq(Y ~ y_hat, tau = alpha)
}
