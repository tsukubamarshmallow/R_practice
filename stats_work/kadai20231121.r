#課題1
ReadData <- read.table('data_r.txt')
A <- ReadData$V1
B <- ReadData$V2
A <- A[2:length(A)]
B <- B[2:length(B)]
A <- as.numeric(A)
B <- as.numeric(B)
png("6-1.png", width = 400, height = 400)
hist(A,breaks=20,col='#ff00ff40')
dev.off()
png("6-2.png", width = 400, height = 400)
hist(B,breaks=20,col='#ff00ff40')
dev.off()

#課題2
Diff <- mean(A) - mean(B)
n <- 9999
Diff_r <- numeric(n+1)
Data <- c(A,B)
for(i in 1:n){
    data_r <- sample(Data,replace=F)
    A_r <- data_r[1:length(A)]
    B_r <- data_r[(length(A)+1):length(data_r)]
    Diff_r[i] <- (mean(A_r) - mean(B_r))
}
Diff_r[n+1] <- Diff
p <- sum(Diff_r >= Diff)(n+1)

png("6-3.png",width=400,height=400)
hist(Diff_r)
arrows(x0 = Diff,  y0 = 0, x1 = Diff, y1 = 1000, col = "red", code = 1, angle = 30)
text(Diff,1500,labels = paste("Diff =", round(Diff, 2)), col = "red")
dev.off()

#課題3
n <- 49
p_array <- numeric(10)
for(j in 1:10){
    for(i in 1:n){
        data_r <- sample(Data,replace=F)
        A_r <- data_r[1:length(A)]
        B_r <- data_r[(length(A)+1):length(data_r)]
        Diff_r[i] <- (mean(A_r) - mean(B_r))
    }
    Diff_r[n+1] <- Diff
    p <- sum(Diff_r >= Diff)/(n+1)
    p
    p_array[j] <- p
}
mean(p_array)
sd(p_array)

n_array <- seq(50,50000,5000)
mean_array <- numeric(length(n_array))
sd_array <- numeric(length(n_array)) 
for(k in n_array){
    n <- k
    p_array <- numeric(10)
    for(j in 1:10){
        for(i in 1:n){
            data_r <- sample(Data,replace=F)
            A_r <- data_r[1:length(A)]
            B_r <- data_r[(length(A)+1):length(data_r)]
            Diff_r[i] <- (mean(A_r) - mean(B_r))
        }
        Diff_r[n+1] <- Diff
        p <- sum(Diff_r >= Diff)/(n+1)
        p_array[j] <- p
    }
    mean_array <- append(mean_array,mean(p_array))
    sd_array <- append(sd_array,sd(p_array))
}

install.packages("ggplot2")
# ライブラリの読み込み
library(ggplot2)
# グラフの作成
df <- data.frame(n = n_array, mean = mean_array, sd = sd_array)
# 折れ線グラフの描画
png("6-4.png",width=400,height=400)
ggplot(df, aes(x = n, y = mean)) +
  geom_line() +
  labs(title = "折れ線グラフ",
       x = "n_array",
       y = "Mean") +
  theme_minimal()

dev.off()

png("6-5.png",width=400,height=400)
ggplot(df, aes(x = n, y = sd)) +
  geom_line() +
  labs(title = "折れ線グラフ",
       x = "n_array",
       y = "SD") +
  theme_minimal()
dev.off()

#課題4

# permパッケージをインストールして読み込む
install.packages("perm")
library(perm)
# permTS()関数を使用して順位和検定を実行
perm_test_result <- permTS(A, B, alternative = "greater")

# 結果の表示
print(perm_test_result)

# t検定を実行
t_test_result <- t.test(A, B, alternative = "greater")

# 結果の表示
print(t_test_result)



