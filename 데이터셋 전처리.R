## 데이터셋 변화율로 전환

## Read Data from CSV: 데이터 전처리
Date_DateFrame <- read_csv("Date_DataFrame",locale=locale('ko',encoding='cp949'))
Month_DataFrame <- read_csv("Month_DataFrame",locale=locale('ko',encoding='cp949'))
Quarter_DataFrame <- read_csv("Quarter_DataFrame",locale=locale('ko',encoding='cp949'))

## Check Data
view(Date_DateFrame)
view(Month_DataFrame)
view(Quarter_DataFrame)

## Change Data
Date_Data=as.matrix(Date_DataFrame) 
Date_Data[,14]=as.double(Date_Data[,14])
row<-as.numeric(Date_Data[, 1])
values<-as.numeric(Date_Data[, 14])
Date_Data_modified<-cbind(row, values)
view(Date_Data_modified)

## Data revise
# revise_Date
Date_DateFrame_PCA<-Date_DataFrame %>% 
  group_by(TIME,ITEM_CODE1) %>% 
  summarize(DATA_VALUE=sum(DATA_VALUE)) %>% 
  pivot_wider(names_from=ITEM_CODE1,values_from=DATA_VALUE)
Date_Data<-subset(Date_DateFrame_PCA, select=-c(64,95,96))
Date_Data<-na.omit(Date_Data)
Date_Data=as.matrix(Date_Data)
Date_Data_Time<-Date_Data[,1]
Date_Data<-Date_Data[,-1]
rownames(Date_Data)=Date_Data_Time

# 일별 거시경제 데이터 >> 사용금지
Date_Data<-Date_Data/lag(Date_Data) - 1
Date_Data[is.na(Date_Data)]<-0
mean_value<- mean(Date_Data, na.rm = TRUE)  # 무한대 값을 대체할 평균 값 계산
Date_Data[is.infinite(Date_Data)] <- 0  # 무한대 값을 평균 값으로 대체
write.csv(Date_Data,file='Date_Data_rate.csv',row.names=TRUE,fileEncoding='cp949')

# 월별 거시경제 데이터
daily_data<-read_csv("daily_data.csv",locale=locale('ko',encoding='cp949'))
daily_data<-as.matrix(daily_data)
daily_data<-daily_data/lag(daily_data) - 1
daily_data[is.na(daily_data)]<-0
mean_value<- mean(daily_data, na.rm = TRUE)  # 무한대 값을 대체할 평균 값 계산
daily_data[is.infinite(daily_data)] <- mean_value  # 무한대 값을 평균 값으로 대체
write.csv(daily_data,file='daily_data_rate.csv',row.names=TRUE,fileEncoding='cp949')
