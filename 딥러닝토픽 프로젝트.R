#데이터
setwd("D:/성균관대/딥러닝토픽/프로젝트")
library(dplyr)
library("lubridate")
data <- read.table("SSE2017.txt", header=TRUE, fileEncoding = "UTF-8", encoding="CP949", sep = "\t")
is.na(data)
str(data)
data <- data[,c(1:4,7)]
data <- as.data.frame(apply(data, 2, function(x) gsub(",", "", x)))

length(unique(data$DateNum))

current <- paste(data$Date, data$time)
current <- strptime(current, "%Y-%m-%d %H:%M")
data$current <- current
data=data %>%
  mutate(
    ts_year = year(data$current),
    ts_month = month(data$current),
    ts_day = day(data$current),
    ts_hour = hour(data$current),
    ts_minute = minute(data$current),
  )
#국내주식 거래 정규시간  09:00~15:30 사이의 데이터만
a <- subset(data, data$ts_hour>=9 & data$ts_hour<15)
b <- subset(data, data$ts_hour==15 & data$ts_minute<=30)
data <- rbind(a,b)
data <- data[c(order(data$DateNum)),] 

# # data <- data[,c(6,2,4,5)]
data <- data[,c(3,2,4,5)]
data[,2:4] <- apply(data[,2:4], 2, as.numeric)


# #1분 뒤의 RSI와 현재 RSI (20분 기준)
# min1 <- c()
# for (i in 1: length(data$CurrentP)) { min1[i] <- data[i+1,3] }
# min1[is.na(min1)] <- 0
# data$min1 <- min1


library(TTR)
data$rsi_cur <- RSI(data$CurrentP, n=20)
rsi_1min <- c()
for (i in 1: length(data$rsi_cur)) { rsi_1min[i] <- data[i+1,5] }
data$rsi_1min <- rsi_1min


data$signal_cur <- ifelse(data$rsi_cur>=70, 2, ifelse(data$rsi_cur<=30, 0, 1))
signal_1min <- c()
for (i in 1: length(data$rsi_1min)) { signal_1min[i] <- data[i+1,7] }
data$signal_1min <- signal_1min
# data$signal_1min <- ifelse(data$rsi_1min>=70, 2, ifelse(data$rsi_1min<=30, 0, 1))



str(data)
attach(data)
########################################################################

# lag = time lag in minutes
lag = 20 # use data from lag minutes to current time

# # time as numeric from 1 to 393
# ti <- as.numeric(time)

# future time to predict in minutes
pred = 1

# return is calculated for each day
# empty data set
dat <- matrix(0, length(data[,1]), 2*lag+8) #거래량과 현재가 각각 20분을 input으로 넣기 위함 

k=0
rep = unique(DateNum)

for (i in rep) # replace rep[1] to rep for whole dataset
{
  cat("date", i, "\n")
  tempd1 <- data[data[, "DateNum"]==i, c(1, 3, 4, 2, 5, 6, 7, 8)]
  for (j in 1:(length(tempd1[,1])-pred))
  {
    # print(k)
    k=k+1
    dat[k, 1:lag] = (tempd1[j:(j+lag-1), 3]) # current volume(현재 거래량)
    dat[k, (lag+1):(2*lag)] = (tempd1[j:(j+lag-1), 2]) # current price(현재 수)
    ret = (tempd1[(j+lag-1), 2] - tempd1[(j+lag-2), 2])/tempd1[(j+lag-2),2] # 현재 수익률  
    # ret5 = tempd1[(j+lag+4), 2] #5분 후의 주가 
    if(is.na(ret)) break
    # take log return if needed
    dat[k, 2*lag+1] = ret   #현재 수익률  
    dat[k, 2*lag+2] = tempd1[j+lag-1, 4] #현재 날짜
    dat[k, 2*lag+3] = tempd1[j+lag-1, 1] #현재 시간
    dat[k, 2*lag+4] = tempd1[j+lag-1, 2] #현재 주가
    dat[k, 2*lag+5] = tempd1[j+lag-1, 5] #현재 rsi
    dat[k, 2*lag+6] = tempd1[j+lag-1, 6] #1분뒤 rsi
    dat[k, 2*lag+7] = tempd1[j+lag-1, 7] #현재 매매 신호
    dat[k, 2*lag+8] = tempd1[j+lag-1, 8] #1분뒤 매매 신호 
  }
}

for (l in c(1:2*lag, 2*lag+2))
{
  dat = dat[dat[,l]!=0,]
}

# standardize responses
# dat[,2*lag+1] <- scale(dat[,2*lag+1])

# dat[,1:20] = apply(dat[,1:20], 2, log) # log transformation if needed
dat <- as.data.frame(dat)
dat <- dat[complete.cases(dat[,41]),]
summary(dat[,41])
dat[,-43] <- apply(dat[,-43], 2, as.numeric)

# scale
scale(dat[,-c(43,47,48)])
# V46 = attr(,"scaled:center") : 5.023230e+01 & attr(,"scaled:scale")=9.008150e+00 
dat[,-c(43,47,48)] <- apply(dat[,-c(43,47,48)], 2, scale)
final <- dat[,c(1:41,45:48)]
# final$V47 <- as.factor(final$V47)
# final$V48 <- as.factor(final$V48)

final <- final[-c(1,73576),]

attach(final)

library(plotly)
df <- tail(dat, 500)  
i <- list(line = list(color='red'))
d <- list(line = list(color='blue'))
fig <- df %>% plot_ly(x= ~V43, type='candlestick', close=~V40, increasing=i, decreasing= d)
fig <- fig %>% add_lines(x=~V43, y=~V40, line=list(color='black', width=2), ingerit=F)
fig <- fig %>% layout(titile='Basic Candlestick Chart', xais=list(rangeslider=list(visible=F)), showlegend=F)
fig


library(plotly)
df <- tail(dat, 500)  
i <- list(line = list(color='red'))
d <- list(line = list(color='blue'))
fig <- df %>% plot_ly(x= ~V43, type='candlestick', close=~V45, increasing=i, decreasing= d)
fig <- fig %>% add_lines(x=~V43, y=~V45, line=list(color='black', width=2), ingerit=F)
fig <- fig %>% layout(titile='Basic Candlestick Chart', xais=list(rangeslider=list(visible=F)), showlegend=F)
fig



library(keras)
library(tensorflow)
library(Metrics)

################################ 이산형 ####################################
table(final$V48)
table(final$V48)/sum(table(final$V48))
bar=barplot(table(final$V48), main="매매신호", ylab="count",names.arg=c('매수신호', '매매보류', '매도신호'), ylim=c(0,75000))
text(bar, y=table(final$V48), labels = table(final$V48), pos=3)

tt <- length(final[,1])
tr <- round(tt*0.8)
train <- final[1:tr,c(1:42,45)]
test <- final[(tr+1):tt,c(1:42,45)]

x_train <- as.data.frame(train[,1:42])
y_train <- train[,43]
x_test <- as.data.frame(test[,1:42])
y_test <- test[,43]

table(train$V48)
table(test$V48)

# 다항 로지스틱회귀모형
library(nnet)
mlogit <- multinom(V48 ~., data=train)
summary(mlogit)
predicted <- predict(mlogit, newdata = test)
sum(predicted==test$V48)/ NROW(predicted)
xtabs(~ predicted + test$V48)
# step(mlogit, direction = 'both')  #시간 너무 오래걸림 
# step(mlogit, direction = 'backward', steps=10) #시간 너무 오래걸림
#결과를 확인할 수 없어 유의미한 변수가 무엇인지 파악하지 못함



# NN
x_train <- as.matrix(train[,1:42])
y_train <- train[,43]
x_test <- as.matrix(test[,1:42])
y_test <- test[,43]

model <- keras_model_sequential()
model %>% 
  layer_dense(units=64, activation="relu", input_shape=c(42)) %>% 
  layer_dense(units=3, activation='softmax')
summary(model)

model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)
# y_train <- as.vector(y_train)
history <- model %>% fit(
  x_train, y_train, 
  epochs = 100, batch_size = 128, 
  validation_split = 0.2
)

plot(history)

################################ 연속형 ####################################
tt <- length(final[,1])
tr <- round(tt*0.8)
train <- final[1:tr,c(1:43)]
test <- final[(tr+1):tt,c(1:43)]

x_train <- as.data.frame(train[,1:42])
y_train <- train[,43]
x_test <- as.data.frame(test[,1:42])
y_test <- test[,43]

hist(final$V46)


#선형 회귀
model <- lm(V46~. , data=train)
summary(model)
# plot(model)
step(model, direction = 'both')
# V46 ~ V7 + V20 + V21 + V22 + V23 + V30 + V32 + V37 + V38 + V39 + V40 + V45

y_test_hat <- as.vector(predict(model, test)) * 9.008150e+00 + 5.023230e+01
summary((y_test * 9.008150e+00 + 5.023230e+01)-y_test_hat)
# V46 = attr(,"scaled:center") : 5.023230e+01 & attr(,"scaled:scale")=9.008150e+00 
plot(head(y_test_hat,100), type='l')
lines(head(y_test * 9.008150e+00 + 5.023230e+01,100), type='l', col='red')

plot(tail(y_test_hat,100), type='l')
lines(tail(y_test * 9.008150e+00 + 5.023230e+01,100), type='l', col='red')

rmse(y_test * 9.008150e+00 + 5.023230e+01, y_test_hat)



# NN
# modelling
x_train <- as.matrix(train[,1:42])
y_train <- train[,43]
x_test <- as.matrix(test[,1:42])
y_test <- test[,43]

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 1, activation = 'relu', input_shape = c(42)) %>% 
  layer_dense(units = 1)
summary(model)

model %>% compile(
  loss = "mse",
  optimizer = "rmsprop",
  metrics = c("mae")
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 100, batch_size = 128, 
  validation_split = 0.2
)
plot(history)

# mae_history <- history$metrics$val_mae
# mae_history_dat <- data.frame(epoch=seq(1:100),mae_history) # check number of epochs in history above
# 
# library(ggplot2)
# ggplot(mae_history_dat, aes(x=epoch, y=mae_history))+geom_line()
# 
# ggplot(mae_history_dat, aes(x=epoch, y=mae_history))+geom_smooth()

model %>% evaluate(x_test, y_test* 9.008150e+00 + 5.023230e+01)
y_test_hat <- model$predict(x_test)* 9.008150e+00 + 5.023230e+01
summary((y_test * 9.008150e+00 + 5.023230e+01)-y_test_hat)
# V46 = attr(,"scaled:center") : 5.023230e+01 & attr(,"scaled:scale")=9.008150e+00 
plot(head(y_test_hat,100), type='l')
lines(head(y_test * 9.008150e+00 + 5.023230e+01,100), type='l', col='red')

plot(tail(y_test_hat,100), type='l')
lines(tail(y_test * 9.008150e+00 + 5.023230e+01,100), type='l', col='red')

rmse(y_test * 9.008150e+00 + 5.023230e+01, y_test_hat)



#NN flexible 추가
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(42)) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 1)

summary(model)

model %>% compile(
  loss = "mse",
  optimizer = "rmsprop",
  metrics = c("mae")
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 100, batch_size = 128, 
  validation_split = 0.2
)
plot(history)

model %>% evaluate(x_test, y_test* 9.008150e+00 + 5.023230e+01)

y_test_hat <- model$predict(x_test)* 9.008150e+00 + 5.023230e+01
summary((y_test * 9.008150e+00 + 5.023230e+01)-y_test_hat)
# V46 = attr(,"scaled:center") : 5.023230e+01 & attr(,"scaled:scale")=9.008150e+00 
plot(head(y_test_hat,100), type='l')
lines(head(y_test * 9.008150e+00 + 5.023230e+01,100), type='l', col='red')

plot(tail(y_test_hat,100), type='l')
lines(tail(y_test * 9.008150e+00 + 5.023230e+01,100), type='l', col='red')

rmse(y_test * 9.008150e+00 + 5.023230e+01, y_test_hat)






# NN + dropout
# modelling
x_train <- as.matrix(train[,1:42])
y_train <- train[,43]
x_test <- as.matrix(test[,1:42])
y_test <- test[,43]

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(42)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 1)

summary(model)



model %>% compile(
  loss = "mse",
  optimizer = "rmsprop",
  metrics = c("mae")
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 100, batch_size = 128, 
  validation_split = 0.2
)

plot(history)
# validation set의 loss와 mae값이 더 작음. 

model %>% evaluate(x_test, y_test* 9.008150e+00 + 5.023230e+01)

y_test_hat <- model$predict(x_test)* 9.008150e+00 + 5.023230e+01
summary((y_test * 9.008150e+00 + 5.023230e+01)-y_test_hat)
# V46 = attr(,"scaled:center") : 5.023230e+01 & attr(,"scaled:scale")=9.008150e+00 
plot(head(y_test_hat,100), type='l')
lines(head(y_test * 9.008150e+00 + 5.023230e+01,100), type='l', col='red')

plot(tail(y_test_hat,100), type='l')
lines(tail(y_test * 9.008150e+00 + 5.023230e+01,100), type='l', col='red')

rmse(y_test * 9.008150e+00 + 5.023230e+01, y_test_hat)



#simple RNN

# modelling
x_train <- as.matrix(train[,1:42])
y_train <- train[,43]
x_test <- as.matrix(test[,1:42])
y_test <- test[,43]

model <- keras_model_sequential()
model %>%
  layer_simple_rnn(units=1, activation='tanh', input_shape=c(42,1)) %>%    #units수 조절
  layer_dense(units=1)

summary(model)

model %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop(), 
  metrics = c("mae"))

history <- model %>% fit(
  x_train, y_train, 
  epochs = 40, 
  batch_size = 128,
  validation_split = 0.2
)

plot(history)

model %>% evaluate(x_test, y_test* 9.008150e+00 + 5.023230e+01)

y_test_hat <- model$predict(x_test)* 9.008150e+00 + 5.023230e+01
summary((y_test * 9.008150e+00 + 5.023230e+01)-y_test_hat)
# V46 = attr(,"scaled:center") : 5.023230e+01 & attr(,"scaled:scale")=9.008150e+00 
plot(head(y_test_hat,100), type='l')
lines(head(y_test * 9.008150e+00 + 5.023230e+01,100), type='l', col='red')

plot(tail(y_test_hat,100), type='l')
lines(tail(y_test * 9.008150e+00 + 5.023230e+01,100), type='l', col='red')

rmse(y_test * 9.008150e+00 + 5.023230e+01, y_test_hat)


# model <- keras_model_sequential()
# model %>%
#   layer_gru(units=64, input_shape=c(42,1)) %>% 
#   layer_dense(units=1)


# LSTM
# modelling
x_train <- as.matrix(train[,1:42])
y_train <- train[,43]
x_test <- as.matrix(test[,1:42])
y_test <- test[,43]

model <- keras_model_sequential()
model %>%
  layer_lstm(units=64, input_shape=c(42,1), activation='tanh') %>% 
  # layer_lstm(units=21, activation='relu', return_sequences = T) %>% 
  layer_dense(units=1)

summary(model)

model %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop(), 
  metrics = c("mae"))


history <- model %>% fit(
  x_train, y_train, 
  epochs = 40, 
  batch_size = 128,
  validation_split = 0.2
)

plot(history)

model %>% evaluate(x_test, y_test* 9.008150e+00 + 5.023230e+01)

y_test_hat <- model$predict(x_test)* 9.008150e+00 + 5.023230e+01
summary((y_test * 9.008150e+00 + 5.023230e+01)-y_test_hat)
# V46 = attr(,"scaled:center") : 5.023230e+01 & attr(,"scaled:scale")=9.008150e+00 
plot(head(y_test_hat,100), type='l')
lines(head(y_test * 9.008150e+00 + 5.023230e+01,100), type='l', col='red')

plot(tail(y_test_hat,100), type='l')
lines(tail(y_test * 9.008150e+00 + 5.023230e+01,100), type='l', col='red')

rmse(y_test * 9.008150e+00 + 5.023230e+01, y_test_hat)




# LSTM + dropout
# modelling
x_train <- as.matrix(train[,1:42])
y_train <- train[,43]
x_test <- as.matrix(test[,1:42])
y_test <- test[,43]

model <- keras_model_sequential()
model %>%
  layer_lstm(units=64, input_shape=c(42,1), activation='tanh', recurrent_dropout = 0.4) %>% 
  # layer_lstm(units=21, activation='relu', return_sequences = T) %>% 
  layer_dense(units=1)

summary(model)

model %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop(), 
  metrics = c("mae"))


history <- model %>% fit(
  x_train, y_train, 
  epochs = 40, 
  batch_size = 128,
  validation_split = 0.2
)

plot(history)

model %>% evaluate(x_test, y_test* 9.008150e+00 + 5.023230e+01)

y_test_hat <- model$predict(x_test)* 9.008150e+00 + 5.023230e+01
summary((y_test * 9.008150e+00 + 5.023230e+01)-y_test_hat)
# V46 = attr(,"scaled:center") : 5.023230e+01 & attr(,"scaled:scale")=9.008150e+00 
plot(head(y_test_hat,100), type='l')
lines(head(y_test * 9.008150e+00 + 5.023230e+01,100), type='l', col='red')

plot(tail(y_test_hat,100), type='l')
lines(tail(y_test * 9.008150e+00 + 5.023230e+01,100), type='l', col='red')

rmse(y_test * 9.008150e+00 + 5.023230e+01, y_test_hat)




# LSTM autoencoder
# modelling
x_train <- as.matrix(train[,1:42])
y_train <- train[,43]
x_test <- as.matrix(test[,1:42])
y_test <- test[,43]

encoder_decoder <- keras_model_sequential() 
encoder_decoder %>% 
  layer_lstm(units=42, activation='tanh', input_shape=c(42,1), return_sequences = T) %>% 
  layer_lstm(units=21, activation='tanh', return_sequences = T) %>% 
  layer_lstm(units=1, activation = 'tanh') %>%
  layer_repeat_vector(42) %>%
  layer_lstm(units=42, activation='tanh', return_sequences = T) %>%
  layer_lstm(units=21, activation='tanh', return_sequences = T) %>% 
  time_distributed(layer_dense(units = 1))

summary(encoder_decoder)

encoder_decoder %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop(), 
  metrics = c("mae"))

history <- encoder_decoder %>% fit(
  x_train, y_train, 
  epochs = 40, 
  batch_size = 128,
  validation_split = 0.2
)

plot(history)

encoder_decoder %>% evaluate(x_test, y_test* 9.008150e+00 + 5.023230e+01)

encoder = keras_model(inputs=encoder_decoder$inputs, outputs=get_layer(encoder_decoder,  'lstm_2')$output)
encoded_x_train = encoder$predict(x_train)
# encoded_y_train = encoder$predict(as.matrix(y_train))
# # layer_name <-  'lstm_2' # repeat_vector전
# # intermediate_layer_model <- keras_model(inputs = encoder_decoder$input,
# #                                         outputs = get_layer(encoder_decoder, layer_name)$output)
# encoded_x_train <- predict(intermediate_layer_model, x_train)

mlp_model = keras_model_sequential()
mlp_model %>% 
  layer_dense(10, activation = 'relu', kernel_initializer = 'glorot_normal', input_shape = 1) %>%
  layer_dense(10, activation = 'relu', kernel_initializer = 'glorot_normal' ) %>%
  layer_dense(1)

summary(mlp_model)

mlp_model %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop(), 
  metrics = c("mae"))

history <- mlp_model %>% fit(
  encoded_x_train, y_train, 
  epochs = 100, 
  batch_size = 128,
  validation_split = 0.2
)

plot(history)

encoded_x_test <- encoder$predict(x_test)
# mlp_test_pred = mlp_model$predict(encoded_x_test)
# y_test_hat <- mlp_test_pred* 9.008150e+00 + 5.023230e+01

mlp_model %>% evaluate(encoded_x_test, y_test* 9.008150e+00 + 5.023230e+01)

y_test_hat <- mlp_model$predict(encoded_x_test)* 9.008150e+00 + 5.023230e+01
summary((y_test * 9.008150e+00 + 5.023230e+01)-y_test_hat)
# V46 = attr(,"scaled:center") : 5.023230e+01 & attr(,"scaled:scale")=9.008150e+00 
plot(head(y_test_hat,100), type='l')
lines(head(y_test * 9.008150e+00 + 5.023230e+01,100), type='l', col='red')

plot(tail(y_test_hat,100), type='l')
lines(tail(y_test * 9.008150e+00 + 5.023230e+01,100), type='l', col='red')

rmse(y_test * 9.008150e+00 + 5.023230e+01, y_test_hat)