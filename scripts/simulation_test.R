# ChatGPT Generated to start - modified to have just two
# Set seed for reproducibility
library(ggplot2)
library(latex2exp)
library(ensembleR)

set.seed(42)

# Number of samples
n_samples <- 1e5

# Generate a single random normal predictor
X1 <- rnorm(n_samples, mean = 0, sd = 1)
X2 <- rnorm(n_samples, mean = 0, sd = 0.5)

# Define a nonlinear function of the predictor
f_x <- function(x1, x2) {
    4*x1 - x1*sin(x2) + 2*x1^2 + 0.5*cos(x1^3)
}
output <-f_x(X1, X2)


# Add heteroscedastic noise, where the noise variance depends on the input
# variables. Also ensure that it is heavy-tailed noise
noise_variance <- abs(X1 * (X1-X2))  # heteroscedastic noise
t_noise <- rt(n_samples, 10)  # t-noise
hetero_noise <- rnorm(n_samples, sd=sqrt(noise_variance))



g1 <- ggplot(data.frame(X1, X2, Y = output, var = sqrt(noise_variance)),
       aes(
           x = X1,
           y = X2,
           color = sqrt(noise_variance)
       )) +
    geom_point() +
    scale_color_viridis_c(name = TeX("$\\sigma$")) +
    theme_bw() +
    theme(text = element_text(size = 12), legend.position.inside = c(0, 0)) +
    guides(color = guide_colorbar(title = TeX("$\\sigma$"))) +
    ggtitle(TeX("$\\sigma = \\sqrt{| X_1 (X_1 - X_2) |}$")) +
    labs(x=TeX("$X_1$") , y=TeX("$X_2$"))
g1

g2 <- ggplot(data = data.frame(Y=Y), aes(x=Y)) + geom_histogram() + theme_bw() + theme(axis.title.y=element_blank())
g2

gridExtra::grid.arrange(g1, g2, ncol=2)


# Define the target variable as the nonlinear combination plus heteroscedastic noise
Y <- output + hetero_noise

# Combine the data into a data frame
data <- data.frame(X1=X1, X2=X2)


# View the first few rows of the dataset
train_idx <- sample(1:nrow(data), 0.8*nrow(data))
test_idx <- setdiff(1:nrow(data), train_idx)
val_idx <- sample(train_idx, 0.2*length(train_idx))
train_idx <- setdiff(train_idx, val_idx)

train_Y <- Y[train_idx]
train_X <- data[train_idx, ]

val_Y <- Y[val_idx]
val_X <- data[val_idx, ]

test_Y <- Y[test_idx]
test_X <- data[test_idx, ]

ensemble <- fit_models(train_X, train_Y, validation_pct = 0.01)

y_hats <- predict(ensemble, val_X)
simple_mean_sd <- sd(val_Y - rowMeans(y_hats))

test_y_hats <- predict(ensemble, test_X)

timings <- microbenchmark::microbenchmark(
    "EM" = EM <- ensembleR::fitAggregationFunction(val_Y, y_hats, method="EM"),
    "LM" = LM <- ensembleR::fitAggregationFunction(val_Y, y_hats, method="LM"),
    "bootLM" = bootLM <- ensembleR::fitAggregationFunction(val_Y, y_hats, method="bootLM"),
    "quantile" = quantile <- ensembleR::fitAggregationFunction(val_Y, y_hats, method="quantile"),
    times=30
)

options(digits=5)
timings
timing_summary <- summary(timings)
timing_summary[order(timing_summary[, "mean"]), ]

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

simulation_results <- cbind(
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


round(simulation_results, 3)

