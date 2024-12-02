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
modelEnsemble_lm <- fit_models(X, Y, aggregation_method="LM", verbose=FALSE)
modelEnsemble_bootLM <- fit_models(X, Y, aggregation_method="bootLM", verbose=FALSE)


# pred_em <- predict(modelEnsemble_EM, test_df[, -which(names(test_df) == "Age")], alpha=0.05, return_components=TRUE)
test_X <- test_df[, -which(names(test_df) == "Age")]
test_Y <- test_df$Age


# get the predictions
pred_lm <- predict(modelEnsemble_lm, test_X, alpha=0.05, return_components=TRUE)
predictions <- predict(modelEnsemble_bootLM, test_X)
predEM <- predict(modelEnsemble_EM, test_X)


# Thankfuly, this is better!
# apply($y_hat, 2, function(y_hat) rmse(test_df$Age, y_hat))
rmse(test_df$Age, predictions[, "mean"])
rmse(test_df$Age, pred_lm$aggregated[, "mean"])
rmse(test_df$Age, predEM[, "mean"])


# TODO: Analyze the coverage of the prediction intervals.
# Add the calibration step.

# IGNORE PLOTS BELOW HERE FOR NOW, BUT PLEASE DON'T DELETE
alphas <- c(0.01, 0.05, 0.10)

pred_boot <- predict(modelEnsemble_bootLM, test_X, alpha=0.05)
mean(predictions[, "lower"] <= test_Y & predictions[, "upper"] >= test_Y)





for (i in 1:length(conf_int_coverage)) {
    target_alpha <- alphas[i]
    abline(v=1-2*target_alpha, col="red")
}


# TODO: add comparison of interval length

test_preds <- predict(aggregator, y_hats, alpha=0.025)
my_predictions <- cbind.data.frame(test_preds, y_true)
my_predictions$in_interval <- my_predictions$y_true > my_predictions$lower & my_predictions$y_true < my_predictions$upper
my_predictions <- my_predictions[order(my_predictions$y_true), ]
my_predictions$index <- 1:nrow(my_predictions)

ggplot(data=my_predictions, aes(x=index, y=y_true, color=in_interval)) + geom_point() + geom_errorbar(aes(ymin=lower, ymax=upper, width=0)) + scale_color_manual(values=c("red", "black"))



