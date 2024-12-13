\name{ensembleR-package}
\alias{ensembleR-package}
\alias{ensembleR}
\docType{package}
\title{
\packageTitle{ensembleR}
}
\description{
\packageDescription{ensembleR}
}
\details{
Solving complex machine learning and statistical problems often involves fitting and
analyzing multiple models, as individual models may not offer clear advantages over one
another in all scenarios. Find the single best model can be a difficult task, as the ”best”
model may change depending on the criterion by which they are judged. To address
this problem, we developed a model aggregation framework to improve the reliability
and accuracy of predictions. We also use these aggregation methods to generate credible
prediction intervals, even for models that do not directly provide prediction intervals.
ensembleR is a technology that integrates the predictive effects and functions of multiple
models to reduce the bias and variance of individual common models and provide more
effective predictions.

To achieve this, the ensembleR package utilizes four methods for arbitrary prediction
problems – XGBoost, support vector machines, random forests and
simple linear models. For each of these methods, we use the default parameters of
the base machine learning models for prediction. After the base models are fit, we use
multiple different model aggregation methods, including the expectation maximization
algorithm, a simple linear models, bootstrapped linear models, and quantile regres-
sion, to optimize the prediction results and generate reliable prediction intervals based
on this.

For example usages, look at the `sample_usage.qmd` document located in the root
folder of the package.

}
\author{
\packageAuthor{ensembleR}

Maintainer: \packageMaintainer{ensembleR}
}
\references{
    \itemize{
        \item Chen, T., He, T., Benesty, M., Khotilovich, V., Tang, Y., Cho, H., Chen, K., Mitchell, R., Cano, I., Zhou, T., Li, M., Xie, J., Lin, M., Geng, Y., Li, Y., and Yuan, J. (2024), xgboost: Extreme Gradient Boosting.
        \item Koenker, R. (1999), “quantreg: Quantile Regression.” https://doi.org/10.32614/CRAN.package.quantreg.
        \item Meyer, D., Dimitriadou, E., Hornik, K., Weingessel, A., and Leisch, F. (2023), “e1071: Misc Functions of the Department of Statistics, Probability Theory Group (Formerly: E1071), TU Wien.”
        \item Nash, W., Sellers, T., Talbot, S., Cawthorn, Andrew, and Ford, W. (1994), “Abalone.” https://doi.org/10.24432/C55C7W.
    }
}
\keyword{ package }
\seealso{
    \link[xgboost]{xgboost}, \link[ranger]{ranger}, \link[stats]{lm}, \link[e1071]{svm}
}
