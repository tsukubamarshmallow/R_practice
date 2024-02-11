#課題0.標本抽出を10,000回行ってそれぞれの標本平均を求めるプログラム
specime_average<- numeric(length=10000)

for(i in 1:10000){
	specime <- rnorm(n=10,mean=50,sd=10)
	specime_average[i] <- mean(specime)
}
Mean = mean(specime_average)
hist(specime_average)


#課題1.テーブルデータの読み込み
ReadData <- read.table("week2-data.txt")

#課題2.平均値と不偏標準偏差の計算
Data <-ReadData$V1
Mean <- mean(Data)
SD <- sd(Data)
unbiased_estimeter <- SD*sqrt(length(Data)/(length(Data)-1))


#課題3.bin = 25としてヒストグラムを作成する
png("kadai3.png",width = 400)
hist(Data,breaks = 26)
dev.off()

#課題4.取り出したデータの一部を取り出して,それぞれの平均と標準誤差を求める

ReadData <- read.table("week2-data.txt",nrow = 5)
Data1_5 <- ReadData$V1
Mean1_5 <- mean(Data1_5)
SE1_5 <- sd (Data1_5) / sqrt (length(Data1_5))


ReadData <- read.table("week2-data.txt",nrow = 10)
Data1_10 <- ReadData$V1
Mean1_10 <- mean(Data1_10)
SE1_10 <- sd (Data1_10) / sqrt (length(Data1_10))

ReadData <- read.table("week2-data.txt",nrow = 20)
Data1_20 <- ReadData$V1
Mean1_20 <- mean(Data1_20)
SE1_20 <- sd (Data1_20) / sqrt (length(Data1_20))

#課題5. 課題3で作成したヒストグラム上に課題2で求めた平均と標準偏差を矢印で示す。
#また,正規分布で近似した曲線を追加して,標準誤差も示すこと

# ヒストグラムをプロット
hist(Data,breaks = 25,main = "ヒストグラム",freq = FALSE)

# 平均値と標準偏差を計算
mean_val <- mean(Data)
sd_val <- sd(Data)

# 矢印を描画
arrows(x0 = mean_val,  y0 = 0.03, x1 = mean_val, y1 = 0, col = "red", code = 1, angle = 30)
arrows(x0 = mean_val + sd_val , y0 = 0.001, x1 = mean_val, y1 = 0.001, col = "blue", code = 1, angle = 30)
arrows(x0 = mean_val - sd_val,  y0 = 0.001, x1 = mean_val, y1 = 0.001, col = "blue", code = 1, angle = 30)
# 正規分布で近似した曲線を追加
x <- seq(min(Data), max(Data), length = 100)
y <- dnorm(x, mean = mean_val, sd = sd_val)
lines(x, y, col = "green")

# 標準誤差を計算
se <- sd_val / sqrt(length(Data))

# 標準誤差をヒストグラムの下に数字で表示
text(80,0.05,labels = paste("SE =", round(se, 2)), col = "red")


