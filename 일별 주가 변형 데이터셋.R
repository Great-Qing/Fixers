## 주가 데이터셋
# 주가 분석 기간
STARTDATE <- '20180302'
ENDDATE <- '20230330'

# 주가 분석 기업 리스트
clist <- list(
  list('현대제철', '004020', 'KR7004020004'), 
  list('동국제강', '01230', 'KR7001230002'),
  list('세아베스틸지주', '001430', 'KR7001430008'),
  list('한국철강', '104700', 'KR7104700000'),
  list('KG스틸', '016380', 'KR7016380008'),
  list('POSCO홀딩스', '005490', 'KR7005490008'),
  list('포스코인터내셔널', '047050', 'KR7047050000'),
  list('포스코스틸리온', '058430', 'KR7058430000'),
  list('현대비앤지스틸', '004560', 'KR7004560009'),
  list('하이스틸', '071090', 'KR7071090005'),
  list('휴스틸', '005010', 'KR7005010004'),
  list('TCC스틸', '002710', 'KR7002710002'),
  list('고려제강', '002240', 'KR7002240000')
)
len <- length(clist)  # 기업 개수
span <- 1233  # 개별 기업 주가 데이터의 행 개수 (주가 분석 기간)

# 주가 데이터 리스트
stockData <- list()

for(i in 1:len) {
  # POST 함수로 서버에 정보 요청 
  gen_otp_url = 'http://data.krx.co.kr/comm/fileDn/GenerateOTP/generate.cmd'
  gen_otp_data = list(
    locale = 'ko_KR',
    tboxisuCd_finder_stkisu0_1 = paste(clist[[i]][1],"/",clist[[i]][2], sep=""),  # '004020/현대제철'
    isuCd = clist[[i]][3],  # 'KR7004020004'
    isuCd2 = clist[[i]][2],  # 004020
    codeNmisuCd_finder_stkisu0_1 = clist[[i]][1],  # '현대제철'
    param1isuCd_finder_stkisu0_1 = 'ALL',
    strtDd = STARTDATE,
    endDd = ENDDATE,
    adjStkPrc_check = 'Y',
    adjStkPrc = '2',
    share = '1',
    money = '1',
    csvxls_isNo = 'false',
    name = 'fileDown',
    url = 'dbms/MDC/STAT/standard/MDCSTAT01701'
  )
  
  # POST 함수로 서버에 요청한 정보 다운받기  
  otp = POST(gen_otp_url, query = gen_otp_data) %>% read_html() %>% html_text()
  down_url = 'http://data.krx.co.kr/comm/fileDn/download_csv/download.cmd'
  down_KRX = POST(down_url, query = list(code = otp),
                  add_headers(referer = gen_otp_url)) %>%
    read_html(encoding = 'EUC-KR') %>%
    html_text() %>%
    read_csv()
  
  print(down_KRX)
  stockData <- append(stockData, list(down_KRX))
}

# stockData에 이름(개별 기업 이름) 레이블링하기
for(i in 1:len) {
  names(stockData)[i] <- clist[[i]][1]
}

# 주가 평균값 구하기
span <- nrow(stockData[[1]])  # stockData의 행 개수 (주가 분석 기간)
date <- stockData[[1]]$일자
df_avg <- data.frame(date, stringsAsFactors = FALSE)  # 일자별 단순산술/가중산술평균 저장한 데이터프레임

# 종가 단순산술평균
df_종가 <- data.frame(date, stringsAsFactors = FALSE)
for(i in 1:len) {df_종가[[i+1]] <- stockData[[i]]$종가}
df_avg[[2]] <- df_종가[[len+2]] <- rowMeans(df_종가[,c(1:len+1)])
names(df_종가) <- c('일자',names(stockData),'종가(단순산술평균)')

# 종가 가중산술평균
wdf_종가 <- data.frame(date, stringsAsFactors = FALSE)
weights <- list()
sumWeights <- c()
for(i in 1:len) {
  weights[[i]] <- stockData[[i]]$시가총액 / stockData[[i]]$상장주식수
  wdf_종가[[i+1]] <- stockData[[i]]$종가 * stockData[[i]]$시가총액 / stockData[[i]]$상장주식수
}
for(j in 1:span) {
  sumWeights[j] <- 0
  for(i in 1:len) {
    sumWeights[j] <- sumWeights[j] + stockData[[i]]$시가총액[j] / stockData[[i]]$상장주식수[j]
  }
}
df_avg[[3]] <- wdf_종가[[len+2]] <- round(rowSums(wdf_종가[,c(1:len+1)])/sumWeights)
names(wdf_종가) <- c('일자',names(stockData),'종가(가중산술평균)')

# 시가 단순산술평균
df_시가 <- data.frame(date, stringsAsFactors = FALSE)
for(i in 1:len) {df_시가[[i+1]] <- stockData[[i]]$시가}
df_avg[[4]] <- df_시가[[len+2]] <- rowMeans(df_시가[,c(1:len+1)])
names(df_시가) <- c('일자',names(stockData),'시가(단순산술평균)')

# 시가 가중산술평균
wdf_시가 <- data.frame(date, stringsAsFactors = FALSE)
weights <- list()
sumWeights <- c()
for(i in 1:len) {
  weights[[i]] <- stockData[[i]]$시가총액 / stockData[[i]]$상장주식수
  wdf_시가[[i+1]] <- stockData[[i]]$시가 * stockData[[i]]$시가총액 / stockData[[i]]$상장주식수
}
for(j in 1:span) {
  sumWeights[j] <- 0
  for(i in 1:len) {
    sumWeights[j] <- sumWeights[j] + stockData[[i]]$시가총액[j] / stockData[[i]]$상장주식수[j]
  }
}
df_avg[[5]] <- wdf_시가[[len+2]] <- round(rowSums(wdf_시가[,c(1:len+1)])/sumWeights)
names(wdf_시가) <- c('일자',names(stockData),'시가(가중산술평균)')

# 고가 단순산술평균
df_고가 <- data.frame(date, stringsAsFactors = FALSE)
for(i in 1:len) {df_고가[[i+1]] <- stockData[[i]]$고가}
df_avg[[6]] <- df_고가[[len+2]] <- rowMeans(df_고가[,c(1:len+1)])
names(df_고가) <- c('일자',names(stockData),'고가(단순산술평균)')

# 고가 가중산술평균
wdf_고가 <- data.frame(date, stringsAsFactors = FALSE)
weights <- list()
sumWeights <- c()
for(i in 1:len) {
  weights[[i]] <- stockData[[i]]$시가총액 / stockData[[i]]$상장주식수
  wdf_고가[[i+1]] <- stockData[[i]]$고가 * stockData[[i]]$시가총액 / stockData[[i]]$상장주식수
}
for(j in 1:span) {
  sumWeights[j] <- 0
  for(i in 1:len) {
    sumWeights[j] <- sumWeights[j] + stockData[[i]]$시가총액[j] / stockData[[i]]$상장주식수[j]
  }
}
df_avg[[7]] <- wdf_고가[[len+2]] <- round(rowSums(wdf_고가[,c(1:len+1)])/sumWeights)
names(wdf_고가) <- c('일자',names(stockData),'고가(가중산술평균)')

# 저가 단순산술평균
df_저가 <- data.frame(date, stringsAsFactors = FALSE)
for(i in 1:len) {df_저가[[i+1]] <- stockData[[i]]$저가}
df_avg[[8]] <- df_저가[[len+2]] <- rowMeans(df_저가[,c(1:len+1)])
names(df_저가) <- c('일자',names(stockData),'저가(단순산술평균)')

# 저가 가중산술평균
wdf_저가 <- data.frame(date, stringsAsFactors = FALSE)
weights <- list()
sumWeights <- c()
for(i in 1:len) {
  weights[[i]] <- stockData[[i]]$시가총액 / stockData[[i]]$상장주식수
  wdf_저가[[i+1]] <- stockData[[i]]$저가 * stockData[[i]]$시가총액 / stockData[[i]]$상장주식수
}
for(j in 1:span) {
  sumWeights[j] <- 0
  for(i in 1:len) {
    sumWeights[j] <- sumWeights[j] + stockData[[i]]$시가총액[j] / stockData[[i]]$상장주식수[j]
  }
}
df_avg[[9]] <- wdf_저가[[len+2]] <- round(rowSums(wdf_저가[,c(1:len+1)])/sumWeights)
names(wdf_저가) <- c('일자',names(stockData),'저가(가중산술평균)')

names(df_avg) <- c('일자','종가(단순)', '종가(가중)', 
                   '시가(단순)', '시가(가중)', 
                   '고가(단순)', '고가(가중)',
                   '저가(단순)', '저가(가중)')

df_avg<-df_avg[order(df_avg$일자), ]
df_avg_date<-df_avg[,1]
df_avg<-df_avg[,-1]
rownames(df_avg)<-df_avg_date
write.csv(df_avg,file='df_avg.csv',row.names=TRUE,fileEncoding='cp949')

# 검증용 예비 파일
stock_price<-bind_cols(df_avg, df_시가, wdf_시가, df_저가, wdf_저가)
stock_price<-stock_price[,-c(10,25,40,55)]
stock_price<-stock_price[order(stock_price$일자...1), ]
stock_date<-stock_price[,1]
stock_price<-stock_price[,-1]
rownames(stock_price)<-stock_date
write.csv(stock_price,file='stock_price.csv',row.names=TRUE,fileEncoding='cp949')
