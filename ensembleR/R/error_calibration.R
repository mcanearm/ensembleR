#' @export
#' @title SD Estimator based on Aggregator
fitCalibrator <- function(y_true, y_pred, y_hat, sigma_est, n_smoother=100, ...) {
    # TODO: perform error calibration using nearest neighbors - get closest N to estimate stddev for
    # each observation, then fit a sigmoid curve to reduce the RMSE between our estimates of sigma
    resid <- y_true-y_pred
    fit_df <- cbind.data.frame(y_true, y_pred, y_hat, resid)

    ordered_df <- fit_df[order(fit_df$y_pred), ]
    rolling_sd <- zoo::rollapply(ordered_df$resid, width=n_smoother, FUN=sd, by=1, by.column=FALSE, fill=NA)

    colnames(ordered_df)

    keep_cols <- setdiff(colnames(ordered_df), c("y_true", "y_pred", "resid"))

    keep_col_formula <- paste(sprintf("splines::bs(%s)", keep_cols), collapse=" + ")
    formula <- as.formula(paste("sd_est ~ ", keep_col_formula))
    sd_df <- cbind.data.frame(ordered_df, sd_est=rolling_sd)

    sd_model <- lm(data=sd_df, formula=formula)
    sd_model
}


#' @title Fit Quantile Bounds
#' @export
fitQuantileBounds <- function(Y, y_hat, lower_quantile = 0.025, upper_quantile = 0.975) {
    # Fit lower bound
    lower_model <- quantreg::rq(Y ~ y_hat, tau = lower_quantile)

    # Fit upper bound
    upper_model <- quantreg::rq(Y ~ y_hat, tau = upper_quantile)

    list(
        lower_bound_model = lower_model,
        upper_bound_model = upper_model
    )
}
