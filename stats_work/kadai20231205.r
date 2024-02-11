#課題1-1
setwd('Z:/stats_work')
ReadData <- read.table('data_p.txt')
A <- ReadData$V1
B <- ReadData$V2
A <- A[2:length(A)]
B <- B[2:length(B)]
A <- as.numeric(A)
B <- as.numeric(B)
png("7-1-1.png", width = 400, height = 400)
hist(A,breaks=20,col='#ff00ff40')
dev.off()
png("7-1-2.png", width = 400, height = 400)
hist(B,breaks=20,col='#ff00ff40')
dev.off()

#課題1-2,1-3

# ライブラリの読み込み
library(boot)

# データの読み込み
setwd('Z:/stats_work')
ReadData <- read.table('data_p.txt')
A <- as.numeric(ReadData$V1[-1])
B <- as.numeric(ReadData$V2[-1])

# ブートストラップサンプリング関数
bootstrap_function <- function(data, indices) {
  sample_A <- data$A[indices]
  sample_B <- data$B[indices]
  diff_mean <- mean(sample_A) - mean(sample_B)
  return(diff_mean)
}

# オリジナルデータ
obs_diff_mean <- mean(A) - mean(B)

# ブートストラップ法
set.seed(22)  # 再現性のためにシードを設定
bootstrap_results <- boot(data.frame(A, B), statistic = bootstrap_function, R = 1000)

# ブートストラップ結果からp値の計算
p_value <- 2 * min(mean(bootstrap_results$t >= obs_diff_mean), mean(bootstrap_results$t <= obs_diff_mean))

# ブートストラップ結果のヒストグラム（理論分布）
png("7-1-3.png", width = 400, height = 400)
hist(bootstrap_results$t, main = "Bootstrap Distribution of Mean Difference", xlab = "Difference in Means")
abline(v = obs_diff_mean, col = "red", lty = 2)  # オリジナルデータの平均差
dev.off()

# p値の表示
cat("Observed Difference in Means:", obs_diff_mean, "\n")
cat("Bootstrap p-value:", p_value, "\n")

#課題1-4
t.test(A, B)

#課題2-1

estimate_pi <- function(num_samples) {
  # ランダムに点をサンプリング
  x <- runif(num_samples, -1, 1)
  y <- runif(num_samples, -1, 1)
  # 円内に入る点の数を数える
  inside_circle <- x^2 + y^2 <= 1
  num_inside <- sum(inside_circle)
  # 円周率の推定値を計算
  pi_estimate <- 4 * (num_inside / num_samples)
  
  return(pi_estimate)
}

# サンプル数を指定して円周率を求める
n_array <- seq(10,5000,10)
pi_array <- numeric(length(n_array))
for( k in n_array){
    i <- k / 10
    pi_array[i] <- estimate_pi(k)
}

install.packages("ggplot2")
# ライブラリの読み込み
library(ggplot2)
# グラフの作成
df <- data.frame(n = n_array, pi = pi_array)
# 折れ線グラフの描画
png("7-2-1.png",width=400,height=400)
ggplot(df, aes(x = n, y = pi)) +
  geom_line() +
  labs(title = "円周率の収束グラフ",
       x = "n",
       y = "pi") +
  theme_minimal()
dev.off()

#課題2-2
pi_mean_array <- numeric(length(n_array))
pi_var_array <- numeric(length(n_array))
r <- 100

for( k in n_array){
    pi_array_tmp <- numeric(100)
    i <- k / 10
    for( j in 1:r){
        pi_array_tmp[j] <- estimate_pi(k)
    }
    pi_mean_array[i] <- mean(pi_array_tmp)
    pi_var_array[i] <- var(pi_array_tmp)
}

# グラフの作成
df <- data.frame(n = n_array, mean = pi_mean_array)
# 折れ線グラフの描画
png("7-2-2.png",width=400,height=400)
ggplot(df, aes(x = n, y = mean)) +
  geom_line() +
  labs(title = "円周率の平均値",
       x = "n",
       y = "mean") +
  theme_minimal()
dev.off()

# グラフの作成
df <- data.frame(n = n_array, var = pi_var_array)
# 折れ線グラフの描画
png("7-2-3.png",width=400,height=400)
ggplot(df, aes(x = n, y = var)) +
  geom_line() +
  labs(title = "円周率の分散",
       x = "n",
       y = "var") +
  theme_minimal()
dev.off()