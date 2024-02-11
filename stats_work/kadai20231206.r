#課題1

#データの下準備
setwd('Z:/stats_work')
Data <- read.csv("week8-data-odd.csv")
x <- Data$X
y <- Data$Y

#散布図の描画
library(ggplot2)
plot <- ggplot(Data, aes(x = X, y = Y)) +
  geom_point() +
  xlim(-1.0, 1.0) +
  ylim(-1.0, 1.0)

#散布図に回帰直線を追加した後に画像化
model <- lm(Y ~ X, data = Data)
png("8-1.png", width = 400, height = 400)
plot + geom_smooth(method = "lm", se = FALSE, color = "blue")
dev.off()

#相関係数を求める
Correlation <- cor.test(x,y,method="pearson")

# 単回帰分析の実行
model <- lm(y ~ x)

# 回帰結果の表示
summary(model)

#課題2

# データの読み込み
load("effect-example.dat")

# aov関数を用いた分散分析
model <- aov(score ~ sub, data = scoreDataES)

# 結果の表示
summary(model)

# 分散分析の結果から必要な情報を取得
SS_total <- sum((scoreDataES$score - mean(scoreDataES$score))^2)
SS_residual <- sum(residuals(model)^2)

# 分散説明率の計算
variance_explained <- (SS_total - SS_residual) / SS_total * 100

# 結果の表示
cat("Variance Explained:", variance_explained, "%\n")









