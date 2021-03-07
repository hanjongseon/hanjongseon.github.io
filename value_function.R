library(tidyquant)
library(dplyr)
#확률 가중 함수
w_p = function(p,r=0.65){
  if(p<0){
    k = p * (-1)
    return((k**r)/((k**r)+(1-k)**r)**(1/r)*(-1) * 2.25)
  }
  if(p>0){
    return((p**r)/((p**r)+(1-p)**r)**(1/r))
  }
  if(p==0){
    return(0)
  }
}

# 가치 함수
value_f = function(x){
  if(x>=0){
    y = x**0.88
  }
  if(x<0){
    y = -2.25*(-x)**0.88
  }
  return(y)
}

library(data.table)
filelists = paste0("/Users/jongseonhan/Documents/RNN_SAMPLE_DATA/", dir("/Users/jongseonhan/Documents/RNN_SAMPLE_DATA/"))
sample_data = fread(filelists[4])
head(sample_data)
sample_data = 
  sample_data %>% 
  mutate(
    shift_close = shift(Close)
  ) %>% 
  na.omit() %>% 
  mutate(
    changed = Close/shift_close-1
  ) %>% 
  dplyr::select(-shift_close)


sample_data$W_P = unlist(Map(w_p,abs(sample_data$changed)))
sample_data$VALUE_F = unlist(Map(value_f,sample_data$changed))
sample_data$FELL_VALUE = sample_data$W_P*sample_data$VALUE_F
sample_data %>% head()

library(dplyr)

sample_data = 
  sample_data %>% 
  mutate(
    PRICE_1 = Close*1/(1+changed),
    PRICE_2 = Close*1/(1+FELL_VALUE)
  )

sample_data$FELL_VALUE_shift = sample_data$FELL_VALUE %>% shift(-1)
sample_data$FELL_VALUE_shift = sample_data$FELL_VALUE_shift + 1
sample_data$PRICE_2 = sample_data$Close*sample_data$FELL_VALUE_shift
sample_data$PRICE_2 = shift(sample_data$PRICE_2)
sample_data = sample_data %>% select(-FELL_VALUE_shift) %>% na.omit()
#sample_data[is.na(PRICE_2)]
sample_data = 
  sample_data %>% 
  mutate(
    PRICE_1_SMA = SMA(Close,5),
    PRICE_2_SMA = SMA(PRICE_2,5)
  )

sample_data = sample_data %>% tail(100)
library(ggplot2)

ggplot() + 
  geom_line(data=sample_data, aes(x=1:nrow(sample_data), y=PRICE_1_SMA), col='red')+
  geom_line(data=sample_data, aes(x=1:nrow(sample_data), y=PRICE_2_SMA), col='blue') +
  geom_line(data=sample_data, aes(x=1:nrow(sample_data), y=Close), col='green')

