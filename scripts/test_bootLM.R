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

# 确保训练集和测试集的列名一致
colnames(test_X) <- colnames(X)

# 定义 RMSE 函数
rmse <- function(y, y_hat) {
  sqrt(mean((y - y_hat)^2))
}

# 使用 `fitAggregationFunction_bootLM` 拟合模型
set.seed(123)  # 确保结果可复现
bootLM_aggregator <- fitAggregationFunction_bootLM(Y, X, boot_iter = 1000)

# 对测试集进行预测
bootLM_predictions <- predict.ModelAggregator_bootLM(bootLM_aggregator, test_X, alpha = 0.05)

# 计算 RMSE
bootLM_rmse <- rmse(test_Y, bootLM_predictions[, "mean"])
print(paste("测试集 RMSE (bootLM):", round(bootLM_rmse, 4)))

# 分析预测区间覆盖率
bootLM_coverage <- mean(test_Y >= bootLM_predictions[, "lower"] & test_Y <= bootLM_predictions[, "upper"])
print(paste("预测区间覆盖率 (bootLM):", round(bootLM_coverage * 100, 2), "%"))

# 绘制预测结果
bootLM_predictions_df <- as.data.frame(bootLM_predictions)
bootLM_predictions_df$y_true <- test_Y
bootLM_predictions_df$index <- 1:nrow(bootLM_predictions_df)
bootLM_predictions_df$in_interval <- with(bootLM_predictions_df, y_true >= lower & y_true <= upper)

ggplot(data = bootLM_predictions_df, aes(x = index, y = y_true, color = in_interval)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper, width = 0)) +
  scale_color_manual(values = c("red", "black")) +
  labs(title = "Bootstrapped LM Aggregation Prediction Intervals",
       x = "Index",
       y = "True Age") +
  theme_minimal()
