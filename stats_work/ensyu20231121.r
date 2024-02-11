setwd('Z:/stats_work')
set.seed(022)#乱数を設定
Data <- c(rnorm(2000,mean=0,sd=5))#平均値0標準偏差5の正規分布から2000個データをサンプリング
r_array <- 10 * 2^(0:floor(log2(1000000/10)))#rの値を格納する配列を宣言
r_mean_array <- numeric(length(r_array))#rの値に対応した平均値を格納する配列を宣言
for( k in length(r_array):1){#
    n <- 100
    r <- r_array[k]
    bs <- numeric(r)
    for(i in r:1){
        bs[i] <- mean(sample(Data,n,replace=TRUE))
    }
    if(r == 10){
        hist(bs,col='red',add=TRUE)
    }
    if(r == 40){
        hist(bs,col='blue',add=TRUE)
    }
    if(r == 320){
        hist(bs,col='green',xlim = c(-5, 5), ylim = c(0, 100))
    }
    r_mean_array[k] <- mean(bs)
}#ブートストラップ処理を実行
legend("topright", legend = c("r=10", "r=40", "r=320"), fill = c("red", "blue", "green"))#凡例を追加

#install.packages("ggplot2")
# ライブラリの読み込み
library(ggplot2)
# グラフの作成
df <- data.frame(r = r_array, mean = r_mean_array)
# 折れ線グラフの描画
png("Q7-4.png",width=400,height=400)
ggplot(df, aes(x = r, y = mean)) +
  geom_line() +
  labs(title = "折れ線グラフ",
       x = "r_array",
       y = "mean") +
  theme_minimal()

dev.off()
