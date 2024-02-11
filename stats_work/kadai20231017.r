#課題1.
#仮説検定とt検定によって執り行う
#H_0はAとBに差がない
#H_1はAとBに差がないとは言えない（A-Bの値が優位水準を満たさない）
Data <- read.table("weight.txt",header=TRUE)
t.test(Data$A,Data$B,paired=TRUE,var.equal=TRUE)

#課題2.
#平均値と標準偏差を求める
Mean_A <- mean(Data$A)
Mean_B <- mean(Data$B)
Mean_A 
Mean_B
SD_A <- sd(Data$A)
SD_B <- sd(Data$B)
SD_A
SD_B

png("3-2-1.png", width = 400, height = 400)
hist(Data$A,freq=FALSE)
 # 矢印を描画
arrows(x0 = Mean_A,  y0 = 0.025, x1 = Mean_A, y1 = 0, col = "red", code = 1, angle = 30)
arrows(x0 = Mean_A + SD_A , y0 = 0.014, x1 = Mean_A, y1 = 0.014, col = "blue", code = 1, angle = 30)
arrows(x0 = Mean_A - SD_A,  y0 = 0.014, x1 = Mean_A, y1 = 0.014, col = "blue", code = 1, angle = 30)
# 正規分布で近似した曲線を追加
x <- seq(min(Data$A), max(Data$A), length = 100)
y <- dnorm(x, mean = Mean_A, sd = SD_A)
lines(x, y, col = "green")
text(80,0.030,labels = paste("Mean_A =", round(Mean_A, 2)), col = "red")
text(80,0.025,labels = paste("SD_A =", round(SD_A, 2)), col = "red")
dev.off()



png("3-2-2.png", width = 400, height = 400)
hist(Data$B,freq=FALSE)
 # 矢印を描画
arrows(x0 = Mean_B,  y0 = 0.025, x1 = Mean_B, y1 = 0, col = "red", code = 1, angle = 30)
arrows(x0 = Mean_B + SD_B , y0 = 0.014, x1 = Mean_B, y1 = 0.014, col = "blue", code = 1, angle = 30)
arrows(x0 = Mean_B - SD_B,  y0 = 0.014, x1 = Mean_B, y1 = 0.014, col = "blue", code = 1, angle = 30)
# 正規分布で近似した曲線を追加
x <- seq(min(Data$B), max(Data$B), length = 100)
y <- dnorm(x, mean = Mean_B, sd = SD_B)
lines(x, y, col = "green")
text(80,0.025,labels = paste("Mean_B =", round(Mean_B, 2)), col = "red")
text(80,0.020,labels = paste("SD_B =", round(SD_B, 2)), col = "red")
dev.off()


#課題3.
#p = 2.8*10^(-2)より優位水準を満たさないのでH_0は棄却される
# 自由度は49
#帰

#課題4
#H_0は棄却され,AとBのデータセットには差があるということがわかる。

#課題5
Data <- read.table("weight.txt",header=TRUE)
t.test(Data$A,Data$B,paired=FALSE,var.equal=TRUE)
#関連2群t-検定では同一集団の条件比較がなされるのに対して,独立2群t-検定では独立した集団のデータに差異があるかどうか示すものであるため,違いが生まれるといって良い

#課題6
#t-検定を行う条件として分散が少ないことが必要である
#⓵与えられたそれぞれのデータが正規分布に近いということ⓶与えられたそれぞれのデータの分散がほぼ等しいこと



