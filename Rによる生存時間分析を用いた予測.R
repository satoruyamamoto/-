# データの読み込み 初期のデータ
train_data<-read.csv("NASA/prehackathonsup/train_data/train_data.csv")
test_data<-read.csv("NASA/prehackathonsup/test_data/test_data.csv")

head(train_data)
head(test_data)

# データの全体像をつかむ
summary(train_data)
summary(test_data)

# 列名の一覧
colnames(train_data)
colnames(test_data)
# train_dataとtest_dataの列は"RUL"を除き同じ

#行数・列数
dim(train_data)
dim(test_data)


# 以下はtrain_dataで作業する

# engine_noごとのデータを抽出
k<-0
for (i in 0:708) {
  nam <- paste("engine_",i,sep="")
  engine_k <-subset(train_data, engine_no==k)
  assign(nam,engine_k)
  k <- k+1
}


# ヒストグラム
hist(train_data$sensor_1)

# 折れ線グラフ
plot_l_0 <- plot(train_data$time_in_cycles, train_data$sensor_1,type="l")
plot_l_1 <- plot(train_data$time_in_cycles, train_data$sensor_21,type="l")


# 散布図 time_in_cyclesとRUL
plot(train_data$time_in_cycles, train_data$RUL)


# データの詳細確認はpythonで行っている。Rではここから予測モデルを作成する。

# Rでの生存時間解析 時間依存型
# eventを予測する
# データの読み込み 初期のデータ
train_data<-read.csv("NASA/prehackathonsup/train_data/train_data.csv")
test_data<-read.csv("NASA/prehackathonsup/test_data/test_data.csv")

# 必要ない列(Nan)を削除
train_data <- train_data[,c(-27,-28,-29,-30,-31,-32)]
test_data <- test_data[,c(-27,-28,-29,-30,-31,-32)]

# event列を追加　RUL<=100ならば1,100<RULならば0とする
train_data$event<-ifelse(train_data$RUL <= 100, 1,0)

# 終わりの時間stopを追加time_in_cyclesが1刻みなのでtime_in_cycles+1とする
train_data$stop <- train_data$time_in_cycles+1
test_data$stop <- test_data$time_in_cycles+1

# 予測モデルを作成する
library(survival)

Survival_event_time <- coxph(data=train_data,
                        Surv(time_in_cycles,stop,event)~op_setting_1 + op_setting_2 + op_setting_3 + sensor_1
                        + sensor_2 + sensor_3 + sensor_4 + sensor_5 + sensor_6 + sensor_7 + sensor_8
                        + sensor_9 + sensor_10 + sensor_11 + sensor_12 + sensor_13 + sensor_14 + sensor_15
                        + sensor_16 + sensor_17 + sensor_18 + sensor_19     
                        + sensor_20 + sensor_21)

summary(Survival_event_time)

# step関数で説明変数を選択する
step(Survival_event_time)

# 再度モデル定義
Survival_event_time_step <- coxph(data=train_data,
                                  Surv(time_in_cycles, stop, event) ~ op_setting_1 + 
                                    op_setting_3 + sensor_1 + sensor_2 + sensor_3 + sensor_4 + 
                                    sensor_5 + sensor_6 + sensor_9 + sensor_10 + sensor_11 + 
                                    sensor_12 + sensor_13 + sensor_14 + sensor_15 + sensor_16 + 
                                    sensor_17 + sensor_18 + sensor_20 + sensor_21)

#test_dataで予測する
pred_test_time_step = predict(Survival_event_time_step, 
                    newdata = test_data, 
                    type=c("lp"))

#上記の予測結果の列を追加する
test_data$pred_test_time_step <- pred_test_time_step

# eventが1になる確率に変換して、列を追加する
pred_test2_time_step <-1 / (1 + exp(-pred_test_time_step))
test_data$pred_test2_time_step <- pred_test2_time_step

# 上記の予測結果は閾値によって変わるので、複数の閾値を検証する。

# 閾値を0.5に設定
pred_test2_time_step_flag<-ifelse(pred_test2_time_step > 0.5, 1, 0)
test_data$pred_test2_time_step_flag<-pred_test2_time_step_flag
hist(pred_test2_time_step_flag)

# csvで出力して、pythonで混合行列を確認する。
write.csv(test_data, "NASA/prehackathonsup/test_data/test_data_R_time_step_0.5.csv")


# 閾値を0.4に設定
pred_test2_time_step_flag<-ifelse(pred_test2_time_step > 0.4, 1, 0)
test_data$pred_test2_time_step_flag<-pred_test2_time_step_flag
hist(pred_test2_time_step_flag)

# csvで出力
write.csv(test_data, "NASA/prehackathonsup/test_data/test_data_R_time_step_0.4.csv")


# 閾値を0.6に設定
pred_test2_time_step_flag<-ifelse(pred_test2_time_step > 0.6, 1, 0)
test_data$pred_test2_time_step_flag<-pred_test2_time_step_flag
hist(pred_test2_time_step_flag)

# csvで出力
write.csv(test_data, "NASA/prehackathonsup/test_data/test_data_R_time_step_0.6.csv")

#survfit：生存曲線を作成する
Survfit_event_time_step_test<-survfit(Survival_event_time_step,test_data)