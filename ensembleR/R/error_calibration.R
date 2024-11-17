#' @export
#' @title SD Estimator based on Aggregator
fitCalibrator <- function(y_true, y_pred, sigma_est, n_smoother=100, ...) {
    # TODO: perform error calibration using nearest neighbors - get closest N to estimate stddev for
    # each observation, then fit a sigmoid curve to reduce the RMSE between our estimates of sigma
    fit_df <- cbind.data.frame(y_true, 'y_hat'=y_pred, 'resid'=y_true-y_pred)

    ordered_df <- fit_df[order(fit_df$y_hat), ]
    rolling_sd <- zoo::rollapply(ordered_df$resid, width=100, FUN=sd, by=1, by.column=FALSE, fill=NA)

    sd_df <- cbind.data.frame("y_hat"=ordered_df[, "y_hat"], sd_est=rolling_sd)

    sd_model <- lm(data=sd_df, rolling_sd ~ splines::bs(y_hat))
    sd_model
}
