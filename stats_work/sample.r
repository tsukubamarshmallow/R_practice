#平均値を求めるプログラム
ReadData <- read.table("week2-example.txt")
Data <- ReadData$V1
Mean <- mean(Data)
Mean