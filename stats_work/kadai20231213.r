Data <- read.table("week9-data.txt",header = TRUE)
# データの整形
age <- Data$age
ht <- Data$ht
wt <- Data$wt
grmax <- Data$grmax

# 単回帰分析
model_age <- lm(grmax ~ age, data = Data)
model_ht <- lm(grmax ~ ht, data = Data)
model_wt <- lm(grmax ~ wt, data = Data)

# 結果の表示
summary(model_age)
summary(model_ht)
summary(model_wt)

# データフレームの用意
data <- data.frame(age = age,
                   ht = ht,
                   wt = wt,
                   grmax = grmax)

# 重回帰モデルの構築
model <- lm(grmax ~ age + ht + wt, data = data)

# モデルのサマリーを表示
summary(model)

data <- data.frame(age = age,
                   ht = ht,
                   wt = wt)

# 相関行列の計算
cor_matrix <- cor(data)

# 相関行列の表示
print(cor_matrix)

 # 重回帰モデルに交互作用項を含める
 model_interaction <- lm(grmax ~ age * ht * wt, data = data)
 
# モデルのサマリーを表示
summary(model_interaction)

aic_value <- AIC(model_interaction)

# step関数を使用して変数の選択を行う
final_model <- step(model_interaction , direction = "both", trace = 0)

# ファイナルモデルの表示
summary(final_model)

aic_value <- AIC(final_model)
