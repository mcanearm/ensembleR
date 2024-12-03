library(quantreg)
library(ggplot2)

# 下载并预处理数据
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"
abalone <- read.csv(url, header = FALSE, stringsAsFactors = FALSE)

# 添加列名
colnames(abalone) <- c("Sex", "Length", "Diameter", "Height", 
                       "WholeWeight", "ShuckedWeight", "VisceraWeight", 
                       "ShellWeight", "Rings")

# 预处理数据
abalone$Age <- abalone$Rings + 1.5
abalone$Rings <- NULL

# 划分训练集和测试集
set.seed(123)
train_idx <- sample(1:nrow(abalone), 0.8 * nrow(abalone))
test_set_idx <- setdiff(1:nrow(abalone), train_idx)

test_df <- abalone[test_set_idx, ]
train_df <- abalone[train_idx, ]

Y <- train_df$Age
X <- train_df[, -which(names(train_df) == "Age")]
test_Y <- test_df$Age
test_X <- test_df[, -which(names(test_df) == "Age")]

# 定义 RMSE 函数
rmse <- function(y, y_hat) {
  sqrt(mean((y - y_hat)^2))
}

# 创建分位数回归模型
quantiles <- c(0.05, 0.5, 0.95)
qr_models <- lapply(quantiles, function(q) {
  rq(Y ~ ., data = cbind(Y, X), tau = q)
})

# 查看模型系数
qr_coefficients <- lapply(qr_models, coef)
print("分位数回归模型系数:")
print(qr_coefficients)

# 对测试集进行预测
predictions <- sapply(qr_models, function(model) {
  predict(model, newdata = test_X)
})

# 转换为数据框格式
predictions_df <- as.data.frame(predictions)
colnames(predictions_df) <- paste0("Q", quantiles * 100)
predictions_df$mean <- rowMeans(predictions_df)

# 计算 RMSE
rmse_results <- sapply(quantiles, function(q) {
  rmse(test_Y, predictions_df[[paste0("Q", q * 100)]])
})
print("RMSE 结果:")
print(rmse_results)

# 计算中位数预测的 RMSE
median_rmse <- rmse(test_Y, predictions_df$Q50)
print(paste("中位数预测的 RMSE:", round(median_rmse, 4)))

# 分析预测区间覆盖率
coverage <- mean(test_Y >= predictions_df$Q5 & test_Y <= predictions_df$Q95)
print(paste("预测区间覆盖率 (5%-95%):", round(coverage * 100, 2), "%"))

# 绘制预测结果
predictions_df$y_true <- test_Y
predictions_df$index <- 1:nrow(predictions_df)
predictions_df$in_interval <- with(predictions_df, y_true >= Q5 & y_true <= Q95)

ggplot(data = predictions_df, aes(x = index, y = y_true, color = in_interval)) +
  geom_point() +
  geom_errorbar(aes(ymin = Q5, ymax = Q95, width = 0)) +
  scale_color_manual(values = c("red", "black")) +
  labs(title = "Quantile Regression Prediction Intervals",
       x = "Index",
       y = "True Age") +
  theme_minimal()
