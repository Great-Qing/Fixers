pacman::p_load("rvest","httr","zoo","tidyverse","lubridate","fUnitRoots","forecast","xml2","readxl","HSAUR","XML","data.table","readr","fda","fdapace","dplyr","reshape","psych","imputeTS","lubridate","tseries","mice")
### Scrap Data from ECOS
## Fixed setting
api_key = 'MSVLD06BZ2WKEPJC0JNI'
start_page='1'
end_page='100000'
date_b='20180301'
date_e='20230330'
month_b='201803'
month_e='202303'
Quarter_b='2018Q1'
Quarter_e='2023Q1'

## 일별 데이터 모음
Date_Code<-c('817Y002','802Y001','722Y001','731Y001','731Y002','731Y003')
Date_DataFrame<-data.frame()

for(main_index_date in Date_Code)
{url_Date=paste0('http://ecos.bok.or.kr/api/StatisticSearch/',api_key,'/xml/kr/',start_page,'/',end_page,'/',main_index_date,'/D/',date_b,'/',date_e,'/')
xmlfile_Date<-xmlParse(url_Date)
df_Date<-xmlToDataFrame(getNodeSet(xmlfile_Date,'//row'))
df_Date$DATA_VALUE<-as.numeric(as.character(df_Date$DATA_VALUE))
dplyr::bind_rows(df_Date)
Date_DataFrame<-rbind(Date_DataFrame,df_Date)}
view(Date_DataFrame)

## 월별 데이터 모음 1 
Month1_Code<-c('102Y004','102Y002','101Y018','101Y019','101Y003','101Y004','101Y006','101Y015','104Y014','104Y013','104Y008','104Y015','104Y009','104Y010','111Y008','111Y007','104Y016','111Y009','151Y002','151Y005','104Y017','901Y054','121Y002','121Y013','121Y008','121Y010','121Y011','121Y004','121Y007','901Y055','901Y056','901Y057')
Month1_DataFrame<-data.frame()

for(main_index_Month1 in Month1_Code)
{url_Month1=paste0('http://ecos.bok.or.kr/api/StatisticSearch/',api_key,'/xml/kr/',start_page,'/',end_page,'/',main_index_Month1,'/M/',month_b,'/',month_e,'/')
xmlfile_Month1<-xmlParse(url_Month1)
df_Month1<-xmlToDataFrame(getNodeSet(xmlfile_Month1,'//row'))
df_Month1$DATA_VALUE<-as.numeric(as.character(df_Month1$DATA_VALUE))
dplyr::bind_rows(df_Month1)
Month1_DataFrame<-rbind(Month1_DataFrame,df_Month1)}

## 월별 데이터 모음 2 
Month2_Code<-c('901Y058','901Y015','191Y001','301Y013','301Y017','301Y014','301Y015','301Y016','901Y011','901Y012','303Y001','303Y002','303Y003','303Y004','303Y005','303Y006','403Y005','732Y001','404Y014','404Y015','404Y016','404Y017','405Y006','405Y007','901Y009','901Y010','402Y014','402Y015','402Y016','401Y015','401Y016','401Y017')
Month2_DataFrame<-data.frame()

for(main_index_Month2 in Month2_Code)
{url_Month2=paste0('http://ecos.bok.or.kr/api/StatisticSearch/',api_key,'/xml/kr/',start_page,'/',end_page,'/',main_index_Month2,'/M/',month_b,'/',month_e,'/')
xmlfile_Month2<-xmlParse(url_Month2)
df_Month2<-xmlToDataFrame(getNodeSet(xmlfile_Month2,'//row'))
df_Month2$DATA_VALUE<-as.numeric(as.character(df_Month2$DATA_VALUE))
dplyr::bind_rows(df_Month2)
Month2_DataFrame<-rbind(Month2_DataFrame,df_Month2)}

## 월별 데이터 모음 3 
Month3_Code<-c('401Y018','512Y013','512Y014','512Y015','512Y016','513Y001','801Y002','606Y002','603Y001','603Y010','604Y003','605Y012','602Y001','602Y002','601Y003','601Y002','901Y067','901Y066','901Y033','901Y032','901Y034','901Y036','901Y039','901Y026','901Y035','901Y025','901Y018','901Y020','901Y0104','901Y0103','901Y037','901Y074','901Y105','901Y106','901Y071','901Y019','901Y072','901Y073','901Y038','901Y100','901Y098','901Y101','901Y108','901Y099','901Y075','901Y092','902Y004','902Y005','902Y006','902Y007','902Y008','902Y012','902Y013','902Y014','902Y020','902Y021','902Y022','902Y001','902Y002','902Y003')
Month3_DataFrame<-data.frame()

for(main_index_Month3 in Month3_Code)
{url_Month3=paste0('http://ecos.bok.or.kr/api/StatisticSearch/',api_key,'/xml/kr/',start_page,'/',end_page,'/',main_index_Month3,'/M/',month_b,'/',month_e,'/')
xmlfile_Month3<-xmlParse(url_Month3)
df_Month3<-xmlToDataFrame(getNodeSet(xmlfile_Month3,'//row'))
df_Month3$DATA_VALUE<-as.numeric(as.character(df_Month3$DATA_VALUE))
dplyr::bind_rows(df_Month3)
Month3_DataFrame<-rbind(Month3_DataFrame,df_Month3)}

## 월별 데이터 수합
Month_DataFrame<-data.frame
Month_DataFrame<-rbind(Month1_DataFrame,Month2_DataFrame,Month3_DataFrame)
view(Month_DataFrame)

## 분기별 데이터 모음
Quarter_Code<-c('151Y001','151Y004','131Y016','131Y017','131Y013','131Y011','131Y010','132Y001','132Y003','131Y014','131Y015','200Y002','502Y001','502Y002','502Y003','502Y004','514Y001','514Y002','514Y003','631Y001','632Y001','633Y001','633Y002','633Y003','633Y004','633Y005','633Y006','902Y009','902Y010','902Y011','902Y015','902Y016','902Y019','902Y017','902Y018')
Quarter_DataFrame<-data.frame()

for(main_index_Quarter in Quarter_Code)
{url_Quarter=paste0('http://ecos.bok.or.kr/api/StatisticSearch/',api_key,'/xml/kr/',start_page,'/',end_page,'/',main_index_Quarter,'/Q/',Quarter_b,'/',Quarter_e,'/')
xmlfile_Quarter<-xmlParse(url_Quarter)
df_Quarter<-xmlToDataFrame(getNodeSet(xmlfile_Quarter,'//row'))
df_Quarter$DATA_VALUE<-as.numeric(as.character(df_Quarter$DATA_VALUE))
dplyr::bind_rows(df_Quarter)
Quarter_DataFrame<-rbind(Quarter_DataFrame,df_Quarter)}
view(Quarter_DataFrame)

## Save Data as cSV
write.csv(Date_DataFrame,file='Date_DataFrame',row.names=TRUE,fileEncoding='cp949')
write.csv(Month_DataFrame,file='Month_DataFrame',row.names=TRUE,fileEncoding='cp949')
write.csv(Quarter_DataFrame,file='Quarter_DataFrame',row.names=TRUE,fileEncoding='cp949')
