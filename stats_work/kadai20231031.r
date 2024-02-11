#課題1
#仮説H0:三群の母平均値は等しい
#帰無仮説H1:三群の母平均値は等しくない

#課題2
Data <- read.table("week4-example.txt",header=TRUE)
Mean_A <- mean(Data$A)
Mean_B <- mean(Data$B)
Mean_C <- mean(Data$C)
Mean_A 
Mean_B
Mean_C
SD_A <- sd(Data$A)
SD_B <- sd(Data$B)
SD_C <- sd(Data$C)
SD_A
SD_B
SD_C

#グラフ描画(A)
png("4-2-1.png", width = 400, height = 400)
hist(Data$A,freq=FALSE)
 # 矢印を描画
arrows(x0 = Mean_A,  y0 = 0.078, x1 = Mean_A, y1 = 0, col = "red", code = 1, angle = 30)
arrows(x0 = Mean_A + SD_A , y0 = 0.05, x1 = Mean_A, y1 = 0.05, col = "blue", code = 1, angle = 30)
arrows(x0 = Mean_A - SD_A,  y0 = 0.05, x1 = Mean_A, y1 = 0.05, col = "blue", code = 1, angle = 30)
# 正規分布で近似した曲線を追加
x <- seq(min(Data$A), max(Data$A), length = 100)
y <- dnorm(x, mean = Mean_A, sd = SD_A)
lines(x, y, col = "green")
text(80,0.030,labels = paste("Mean_A =", round(Mean_A, 2)), col = "red")
text(80,0.025,labels = paste("SD_A =", round(SD_A, 2)), col = "red")
dev.off()

#グラフ描画(B)
png("4-2-2.png", width = 400, height = 400)
hist(Data$B,freq=FALSE)
 # 矢印を描画
arrows(x0 = Mean_B,  y0 = 0.078, x1 = Mean_B, y1 = 0, col = "red", code = 1, angle = 30)
arrows(x0 = Mean_B + SD_B , y0 = 0.05, x1 = Mean_B, y1 = 0.05, col = "blue", code = 1, angle = 30)
arrows(x0 = Mean_B - SD_B,  y0 = 0.05, x1 = Mean_B, y1 = 0.05, col = "blue", code = 1, angle = 30)
# 正規分布で近似した曲線を追加
x <- seq(min(Data$B), max(Data$B), length = 100)
y <- dnorm(x, mean = Mean_B, sd = SD_B)
lines(x, y, col = "green")
text(80,0.030,labels = paste("Mean_B =", round(Mean_B, 2)), col = "red")
text(80,0.025,labels = paste("SD_B =", round(SD_B, 2)), col = "red")
dev.off()

#グラフ描画(C)
png("4-2-3.png", width = 400, height = 400)
hist(Data$C,freq=FALSE)
 # 矢印を描画
arrows(x0 = Mean_C,  y0 = 0.078, x1 = Mean_C, y1 = 0, col = "red", code = 1, angle = 30)
arrows(x0 = Mean_C + SD_C , y0 = 0.05, x1 = Mean_C, y1 = 0.05, col = "blue", code = 1, angle = 30)
arrows(x0 = Mean_C - SD_C,  y0 = 0.05, x1 = Mean_C, y1 = 0.05, col = "blue", code = 1, angle = 30)
# 正規分布で近似した曲線を追加
x <- seq(min(Data$C), max(Data$C), length = 100)
y <- dnorm(x, mean = Mean_C, sd = SD_C)
lines(x, y, col = "green")
text(80,0.030,labels = paste("Mean_C =", round(Mean_C, 2)), col = "red")
text(80,0.025,labels = paste("SD_C =", round(SD_C, 2)), col = "red")
dev.off()

#課題3

#各郡データに分解
Data_A <- Data$A
Data_B <- Data$B 
Data_C <- Data$C

#投与データ(Dataをすべてまとめた変数)を用意する
投与データ <- c(Data_A,Data_B,Data_C)

#薬の種類別ラベルを作成する
薬の種類 <- c(rep("Data_A",30),rep("Data_B",30),rep("Data_C",30))

#要因型ベクトルに分解
薬の種類 <- factor(薬の種類)

#aov関数の実行
summary(aov(投与データ~薬の種類))

#課題4
#p = 0.006となり優位水準を満たさないことから,4群の母平均は等しくないと結論できる

#課題5

oneway.test(投与データ~薬の種類,var.equal=FALSE)
#今回は対応がある場合の分散分析なので
#F=283.4/283.4+2303.9