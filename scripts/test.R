library(ensembleR)
library(ggplot2)


data(abalone, package="ensembleR")
abalone$Age <- abalone$Rings + 1.5
abalone$Rings <- NULL


train_idx <- sample(1:nrow(abalone), 0.8*nrow(abalone))
test_set_idx <- setdiff(1:nrow(abalone), train_idx)
val_set_idx <- sample(train_idx, 0.2*length(train_idx))
train_idx <- setdiff(train_idx, val_set_idx)

test_df <- abalone[test_set_idx, ]
train_df <- abalone[train_idx, ]
val_df <- abalone[val_set_idx, ]


train_Y <- train_df$Age
train_X <- train_df[, -which(names(train_df) == "Age")]

test_X <- test_df[, -which(names(test_df) == "Age")]
test_Y <- test_df$Age

val_X <- val_df[, -which(names(val_df) == "Age")]
val_Y <- val_df$Age

# example usage - uses predict methods
modelEnsemble <- fit_models(train_X, train_Y, validation_pct=0.01)
y_hats <- predict(modelEnsemble, val_X)
aggregator <- ensembleR::fitAggregationFunction(val_Y, y_hats, method="bootLM")

simple_mean_sd <- sd(val_Y - rowMeans(y_hats))


# slight bug right now - need to resolve issue when no validation is needed
modelEnsemble <- fit_models(train_X, train_Y, validation_pct=0.01)
y_hats <- predict(modelEnsemble, val_X)
# aggregator <- fitAggregationFunction(val_Y, y_hats, method="EM")
# predict(aggregator, new_y_hats)


timings <- microbenchmark::microbenchmark(
    "EM" = EM <- ensembleR::fitAggregationFunction(val_Y, y_hats, method="EM"),
    "LM" = LM <- ensembleR::fitAggregationFunction(val_Y, y_hats, method="LM"),
    "bootLM" = bootLM <- ensembleR::fitAggregationFunction(val_Y, y_hats, method="bootLM"),
    "quantile" = quantile <- ensembleR::fitAggregationFunction(val_Y, y_hats, method="quantile"),
    times=50
)
timing_summary <- summary(timings)
timing_summary[order(timing_summary[, "mean"]), c("expr", "lq", "mean", "uq")]
timings


test_y_hats <- predict(modelEnsemble, test_X)
apply(test_y_hats, 2, function(y_hat_col) {
    rmse(y_hat_col, test_Y)
})

evaluate <- function(agg_model, test_y_hats, test_y) {
    out <- predict(agg_model, test_y_hats, alpha=0.05)
    coverage <- mean(out[, "lower"] <= test_Y & out[, "upper"] >= test_Y)
    interval_length <- mean(out[, "upper"] - out[, "lower"])
    rmse_val <- rmse(test_Y, out[, "mean"])
    c("rmse"=rmse_val, "coverage"=coverage, "interval_length"=interval_length)
}

avg <- rowMeans(test_y_hats)
simple_mean <- c(
    "rmse" = rmse(test_Y, avg),
    "coverage" = mean(
        avg - 1.96 * simple_mean_sd <= test_Y &
            avg + 1.96 * simple_mean_sd >= test_Y
    ),
    "interval_length" = mean(2 * 1.96 * simple_mean_sd)
)
all_methods <- cbind(
    sapply(
        list(
            "EM" = EM,
            "LM" = LM,
            "bootLM" = bootLM,
            "quantile" = quantile
        ),
        evaluate,
        test_y_hats = test_y_hats,
        test_y = test_Y,
        USE.NAMES = TRUE
    ),
    'simple_mean' = simple_mean
)
round(all_methods, 3)


# Estimate the RMSE on the Abalone dataset using bootstrapping of our test set
agg_preds <- sapply(list("EM"=EM, "LM"=LM, "bootLM"=bootLM, "quantile"=quantile), function(mod) {
    predict(mod, test_y_hats)[, "mean"]
})
all_methods <- cbind(agg_preds, test_y_hats)

bootstrapped_RMSE <- replicate(1000, {
    idx <- sample(1:nrow(test_y_hats), nrow(test_y_hats), replace=TRUE)
    mean_agg <- rowMeans(agg_preds[idx, ])
    all_methods_sub <- cbind(mean_agg, all_methods[idx, ])
    apply(all_methods_sub, 2, function(col) rmse(test_Y[idx], col))
})
melted_data <- reshape2::melt(t(bootstrapped_RMSE), value.name = "rmse")
melted_data$method_type <- ifelse(melted_data$Var2 %in% c("EM", "LM", "bootLM", "quantile", "mean_agg"), "Aggregation", "Base")

ggplot(data=melted_data, aes(x=Var2, y=rmse, fill=method_type)) + geom_boxplot() + theme_bw() + labs(x="Method", y="RMSE")


# Now, let's do a bootstrapped estimation of interval coverage
boot_coverage <- replicate(250, expr={
    boot_eval <- sapply(list("EM"=EM, "LM"=LM, "bootLM"=bootLM, "quantile"=quantile), function(mod) {
        idx <- sample(1:nrow(test_y_hats), nrow(test_y_hats), replace=TRUE)
        out <- predict(mod, test_y_hats[idx, ])
        mean(out[, "lower"] <= test_Y[idx] & out[, "upper"] >= test_Y[idx])
    })
})

simple_mean_coverage <- replicate(250, expr={
    idx <- sample(1:nrow(test_y_hats), nrow(test_y_hats), replace=TRUE)
    out <- rowMeans(test_y_hats[idx, ])
    mean((out - 1.96*simple_mean_sd <= test_Y[idx] )& (out + 1.96*simple_mean_sd) >= test_Y[idx])
})

coverage_boot_samples <- rbind(boot_coverage, "simple_mean"=simple_mean_coverage)

interval_length <- replicate(250, expr={
    boot_eval <- sapply(list("EM"=EM, "LM"=LM, "bootLM"=bootLM, "quantile"=quantile), function(mod) {
        idx <- sample(1:nrow(test_y_hats), nrow(test_y_hats), replace=TRUE)
        out <- predict(mod, test_y_hats[idx, ])
        mean(out[, "upper"] - out[, "lower"])
    })
})

melted_data <- reshape2::melt(t(coverage_boot_samples), value.name = "coverage")
# melted_data$method_type <- ifelse(melted_data$Var2 %in% c("EM", "LM", "bootLM", "quantile", "mean_agg"), "Aggregation", "Base")
ggplot(data = melted_data, aes(x = Var2, y = coverage)) +
    geom_boxplot() +
    theme_bw() +
    labs(x = "Aggregation Method", y = "Prediction Interval Coverage") +
    geom_hline(yintercept = 0.95, col = "red", linetype = 2) +
    scale_y_continuous( labels = scales::percent_format(accuracy = 1), breaks = seq(0.92, 0.98, 0.01)) +
    theme(text=element_text(size=18))



bootstrapped <- replicate(1000, {
    idx <- sample(1:nrow(test_y_hats), nrow(test_y_hats), replace=TRUE)
    mean_agg <- rowMeans(agg_preds[idx, ])
})
melted_data <- reshape2::melt(t(bootstrapped_RMSE), value.name = "rmse")
melted_data$method_type <- ifelse(melted_data$Var2 %in% c("EM", "LM", "bootLM", "quantile", "mean_agg"), "Aggregation", "Base")
ggplot(data=melted_data, aes(x=Var2, y=rmse, fill=method_type)) + geom_boxplot() + theme_bw() + labs(x="Method", y="RMSE")


# Generate plots for BootLM and Quantile Regression interval
out <- predict(quantile, test_y_hats, alpha=0.05)
in_interval <- out[, "lower"] <= test_Y & out[, "upper"] >= test_Y
quantile_int <- cbind.data.frame(out, in_interval, test_Y)
quantile_int <- quantile_int[order(quantile_int[, "test_Y"]), ]


out <- predict(bootLM, test_y_hats, alpha=0.05)
in_interval <- out[, "lower"] <= test_Y & out[, "upper"] >= test_Y
boot_int <- cbind.data.frame(out, in_interval, test_Y)
boot_int <- boot_int[order(boot_int[, "test_Y"]), ]

par(mfrow=c(2, 1))

ylimits <- c(min(boot_int$lower, min(quantile_int$lower, test_Y)), max(max(quantile_int$upper, test_Y), boot_int$upper))

# ChatGPT was used to generate some of the plotting code.
# First plot (QuantileAggregation)
plot(1:nrow(quantile_int), quantile_int$test_Y, pch=16, col=ifelse(quantile_int$in_interval, "black", "red"),
     xlab="", ylab="Age", main="Prediction Intervals - Quantile", ylim=ylimits)
arrows(1:nrow(quantile_int), quantile_int$lower, 1:nrow(quantile_int), quantile_int$upper,
       angle=90, code=2, length=0, col=ifelse(quantile_int$in_interval, "black", "red"))

# Second plot (BootLM)
plot(1:nrow(boot_int), boot_int$test_Y, pch=16, col=ifelse(boot_int$in_interval, "black", "red"),
     xlab="", ylab="Age", main="Prediction Intervals - BootLM", ylim=ylimits)
arrows(1:nrow(boot_int), boot_int$lower, 1:nrow(boot_int), boot_int$upper,
       angle=90, code=2, length=0, col=ifelse(boot_int$in_interval, "black", "red"))





