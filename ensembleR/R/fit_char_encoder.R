#' @title One Hot Encoder for categorical variables
#' @export
#' @description
#' This is a thin wrapper around the \link[caret]{dummyVars} function that
#' fits a one-hot encoder to a dataset. Here, we assume that any character
#' variables are actually categorical, and then use the full-rank version
#' of those variables (drop one level).
#' @importFrom caret dummyVars
fit_char_encoder <- function(X) {
    is_char <- apply(X, 2, function(col) {
        tmp <- suppressWarnings(as.numeric(col))
        all(is.na(tmp))
    })

    if (sum(is_char) != 0) {
        char_cols <- colnames(X)[is_char]
        numeric_cols <- setdiff(colnames(X), char_cols)
        char_formula <- formula(paste0(" ~ ", paste0(char_cols, collapse = " + ")))
        dummy_mod <- caret::dummyVars(char_formula, data = X, fullRank = TRUE)
        out <- structure(list(dummy_model=dummy_mod, numeric_cols=numeric_cols, char_cols=char_cols), class = "char_encoder")
    } else {
        out <- NULL
    }
    out
}

#' @export
#' @describeIn fit_char_encoder Predict method for char_encoder S3 class
predict.char_encoder <- function(object, X, ...) {
    dummy_vars <- predict(object$dummy_model, newdata=X)
    fit_features <- as.matrix(cbind(X[, object$numeric_cols], dummy_vars))
    fit_features
}
