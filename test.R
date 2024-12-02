library(ensembleR)


data(abalone, package="ensembleR")
abalone$Age <- abalone$Rings + 1.5
abalone$Rings <- NULL


train_idx <- sample(1:nrow(abalone), 0.8*nrow(abalone))
test_set_idx <- setdiff(1:nrow(abalone), train_idx)

test_df <- abalone[test_set_idx, ]
train_df <- abalone[train_idx, ]


Y <- train_df$Age
X <- train_df[, -which(names(train_df) == "Age")]


rmse <- function(y, y_hat) {
    sqrt(mean((y - y_hat)^2))
}

modelEnsemble_EM <- fit_models(X, Y, aggregation_method="EM", verbose=FALSE)
modelEnsemble_lm <- fit_models(X, Y, aggregation_method="lm", verbose=FALSE, calibrate_sd=TRUE)


test_X <- test_df[, -which(names(test_df) == "Age")]
test_Y <- test_df[, "Age"]

# pred_em <- predict(modelEnsemble_EM, test_X, alpha=0.05, return_components=TRUE)
pred_lm <- predict(modelEnsemble_lm, test_X, alpha=0.05, return_components=TRUE)

# Thankfuly, this is better!
# apply($y_hat, 2, function(y_hat) rmse(test_df$Age, y_hat))
# rmse(test_df$Age, pred_em$aggregated[, "mean"])
rmse(test_df$Age, pred_lm$aggregated[, "mean"])

apply(pred_lm$y_hat, 2, function(col) rmse(test_df$Age, col))
i <- 2

# terrible coverage with base methods
par(mfrow=c(1, 1))
alphas <- c(0.01, 0.05, 0.10)
for (i in 1:length(alphas)) {
    target_alpha <- alphas[i]
    lm_preds <- predict(modelEnsemble_lm, test_X, alpha=target_alpha)
    bootstrapped_cov_estimate <- replicate(1000, expr = {
        idx <- sample(1:nrow(lm_preds), replace=TRUE, size = nrow(lm_preds))
        pred_sample <- lm_preds[idx,]
        test_sample <- test_Y[idx]
        mean(lm_preds[, "lower"] <= test_sample & lm_preds[, "upper"] >= test_sample)
    })
    hist(bootstrapped_cov_estimate, main=sprintf("CI=%i%%", 100*(1-target_alpha)), xlab="Coverage",
    cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
    abline(v=1-target_alpha, col="red")
}

calibrator <- fitCalibrator(
    modelEnsemble_lm$val_Y,
    modelEnsemble_lm$aggregation_function$model$fitted.values,
    modelEnsemble_lm$y_hat
)
predict(calibrator, )

?fitCalibrator
fitCalibrator(pred_em$y_hat, pred_em$aggregated[, "mean"], test_Y)



# TODO: add comparison of interval length

test_preds <- predict(aggregator, y_hats, alpha=0.025)
my_predictions <- cbind.data.frame(test_preds, y_true)
my_predictions$in_interval <- my_predictions$y_true > my_predictions$lower & my_predictions$y_true < my_predictions$upper
my_predictions <- my_predictions[order(my_predictions$y_true), ]
my_predictions$index <- 1:nrow(my_predictions)

ggplot(data=my_predictions, aes(x=index, y=y_true, color=in_interval)) + geom_point() + geom_errorbar(aes(ymin=lower, ymax=upper, width=0)) + scale_color_manual(values=c("red", "black"))



