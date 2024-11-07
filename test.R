library(caret)
library(ensembleR)
library(ggplot2)



names <- c(
    "Sex",
    "Length",
    "Diameter",
    "Height",
    "whole_weight_g",
    "shucked_weight_g",
    "viscera_weight_g",
    "shell_weight_g",
    "Rings"
)
abalone <- read.table("abalone/abalone.data", sep=",", header=FALSE, col.names = names)

train_idx <- sample(1:nrow(abalone), 0.8*nrow(abalone))
val_set_idx <- sample(train_idx, 0.3*length(train_idx))
train_set_idx <- setdiff(train_idx, val_set_idx)
test_set_idx <- setdiff(1:nrow(abalone), train_idx)

test_df <- abalone[test_set_idx, ]
val_df <- abalone[val_set_idx, ]
train_df <- abalone[train_set_idx, ]

# predict the shucked weight vs. sex, length, diameter, height, and rings
lm_fit <- lm(data=train_df, shucked_weight_g ~ Rings + Height + Diameter + Length + Sex)

# rf model
rf_fit <- ranger::ranger(data=train_df, shucked_weight_g ~ Rings + Height + Diameter + Length + Sex, num.trees = 256)

# xgboost model
dv <- caret::dummyVars(~ Sex, data=abalone, fullRank=TRUE)
dummy_vars <- predict(dv, abalone)

xgb_train_features <- as.matrix(cbind(train_df[, c("Rings", "Height", "Diameter", "Length")], dummy_vars[train_set_idx, ]))
xgb_fit <- xgboost::xgboost(data = xgb_train_features, label=train_df$shucked_weight_g, objective="reg:squarederror", nrounds=500)

# now fit the weights using the validation set
# predict the shucked weight for the test set
lm_pred <- predict(lm_fit, val_df)
rf_pred <- predict(rf_fit, val_df)$predictions
xgb_pred <- predict(xgb_fit, as.matrix(cbind(val_df[, c("Rings", "Height", "Diameter", "Length")], dummy_vars[val_set_idx, ])))

val_y_hats <- cbind('lm'=lm_pred, 'rf'=rf_pred, 'xgb'=xgb_pred)
val_y_true <- val_df$shucked_weight_g

rmse <- function(y, y_hat) {
    sqrt(mean((y - y_hat)^2))
}

val_rmse <- apply(val_y_hats, 2, function(y_hat) rmse(val_y_true, y_hat))
val_rmse

# now fit weights using validation set
# stan_mod <- fitSTANModel(y_hats, y_true, verbose=TRUE)
create_density_fn <- function(y) {
    kde <- density(y)
    function(x) {
        approx(x = kde$x, y = kde$y, xout = x)$y
    }

}

get_aggregator <- function(y_hats, y_true) {
    den_fns <- apply(y_hats, 2, create_density_fn)
    eps <- y_true - y_hats
    bias <- colMeans(eps)
    sigma_ests <- apply(eps , 2, sd)
    weight_predictions <- function(pred_matrix, sims=1000) {
        # centered_preds <- pred_matrix - bias
        centered_preds <- pred_matrix
        pdf_ests <- sapply(1:ncol(pred_matrix), function(j) {
            den_fns[[j]](centered_preds[,j])
        })

        weight_mat <- pdf_ests/rowSums(pdf_ests)
        draws <- t(sapply(1:nrow(weight_mat), function(i) {
            j <- sample(1:ncol(weight_mat), sims, prob=weight_mat[i,], replace=TRUE)
            rnorm(sims, centered_preds[i, j], sigma_ests[j])
        }))
        draws
    }
    weight_predictions
}

agg_fn <- get_aggregator(val_y_hats, val_y_true)

lm_pred_test <- predict(lm_fit, test_df)
rf_pred_test <- predict(rf_fit, test_df)$predictions
xgb_pred_test <- predict(xgb_fit, as.matrix(cbind(test_df[, c("Rings", "Height", "Diameter", "Length")], dummy_vars[test_set_idx, ])))

y_hats <- cbind('lm'=lm_pred_test, 'rf'=rf_pred_test, 'xgb'=xgb_pred_test)
y_true <- test_df$shucked_weight_g

test_predictions <- agg_fn(y_hats)

pis <- t(apply(test_predictions, 1, quantile, probs = c(0.025, 0.975)))

weight_preds <- cbind.data.frame('y_tilde'= rowMeans(test_predictions), pis, y_true)
my_predictions <- weight_preds[sample(1:nrow(weight_preds), 100),]
my_predictions[, 'index'] = 1:nrow(my_predictions)
my_predictions$in_interval <- my_predictions$y_true > my_predictions$`2.5%` & my_predictions$y_true < my_predictions$`97.5%`


ggplot(data=my_predictions, aes(x=index, y=y_true, color=in_interval)) + geom_point() + geom_errorbar(aes(ymin=`2.5%`, ymax=`97.5%`, width=0)) + scale_color_manual(values=c("red", "black"))

mean(my_predictions$in_interval)
y_hat <- rowMeans(test_predictions)

apply(y_hats, 2, function(y_hat) rmse(y_true, y_hat))
rmse(y_hat, y_true)


