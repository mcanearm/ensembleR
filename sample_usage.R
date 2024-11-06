# sample_usage


library(ensembleR)


data("trees")

X <- trees[, c("Girth", "Height")]
Y <- trees[, "Height"]


all_models <- fit_models(X, Y)

# this needs to output values!
my_values <- predict(all_models, X)

# but let's assume it does
