## PCA 분석 1 : 일별 데이터만 활용
# 퓨리에 변환 후 분석
Date_Data_B<-Date_Data
Date_Data_Code<-colnames(Date_Data)
number_of_basis_function=1000
rangeval=c(0,1)
basis<-create.fourier.basis(rangeval, nbasis=number_of_basis_function, period=diff(rangeval))
functional_data_squared<-Data2fd(
  y=Date_Data_B,
  argvals=seq(from=0, to=1, length.out=nrow(Date_Data_B)), 
  basis=basis)
principal_components<-pca.fd(fdobj=functional_data_squared,nharm=ncol(Date_Data_B))
eigenfunctions<-principal_components$harmonics
eigenfunctions_mat<-as.matrix(eigenfunctions$coefs)

# 0.95 이상의 설명력을 가지는 주성분 확인
variance_prop<-c(cumsum(eigenfunctions_mat[1,] / sum(eigenfunctions_mat[1,])))
PCAnum<-sum(variance_prop<0.95)
eigenfunctions_mat<-eigenfunctions_mat[,1:PCAnum]
view(eigenfunctions_mat)

# 주성분에 대한 개별 요인 SCORE 확인
Date_score<-principal_components$scores
rownames(Date_score)<-Date_Data_Code
view(Date_score)

# 코드 정규화 0183000, 0000035, 0002000, 0090000, 0087000, 0091000, 0090000
RP1<-Date_Data[,c("0183000","0000035", "0002000", "0090000", "0087000", "0091000", "0090000")]

## 주성분 분석 2회차 : 일별 데이터만 활용, 주식시장 미포함
# 1회차 결과 주식시장 데이터의 영향력이 과다함. 주식시장 데이터 제거

# 주식시장 코드 확인
target_rows<-Date_DateFrame[grep("주식",Date_DateFrame$STAT_NAME),]
Market_Date<-target_rows[,4]
Market_Date<-as.matrix(Market_Date)
Market_Date=Market_Date[-which(duplicated(Market_Date))]
view(Market_Date)

# 주식시장 데이터 제거
Date_Data_B<-as.data.frame(Date_Data)
Market_Date<-as.vector(Market_Date)
Date_Data_B <- Date_Data_B[,!(colnames(Date_Data_B) %in% Market_Date)]

# 퓨리에 변환 후 분석
Date_Data_B<-as.matrix(Date_Data_B)
Date_Data_Code<-colnames(Date_Data_B)
number_of_basis_function=1000
rangeval=c(0,1)
basis<-create.fourier.basis(rangeval, nbasis=number_of_basis_function, period=diff(rangeval))
functional_data_squared<-Data2fd(
  y=Date_Data_B,
  argvals=seq(from=0, to=1, length.out=nrow(Date_Data_B)), 
  basis=basis)
principal_components<-pca.fd(fdobj=functional_data_squared,nharm=ncol(Date_Data_B))
eigenfunctions<-principal_components$harmonics
eigenfunctions_mat<-as.matrix(eigenfunctions$coefs)

# 0.95 이상의 설명력을 가지는 주성분 확인
variance_prop<-c(cumsum(eigenfunctions_mat[1,] / sum(eigenfunctions_mat[1,])))
PCAnum<-sum(variance_prop<0.95)
eigenfunctions_mat<-eigenfunctions_mat[,1:PCAnum]
view(eigenfunctions_mat)

# 주성분에 대한 개별 요인 SCORE 확인
Date_score<-principal_components$scores
rownames(Date_score)<-Date_Data_Code
view(Date_score)

## 조건: 일별 데이터, 주식시장 미포함
## 0000035, 0000029
RP2<-Date_Data[,c("0000035","0000029")]

## PCA 분석 3: 월별 데이터만 활용
# 퓨리에 변환 후 분석
daily_data<-as.matrix(daily_data)
Date_Data_B<-daily_data
Date_Data_Code<-colnames(Date_Data_B)
number_of_basis_function=5000
rangeval=c(0,1)
basis<-create.fourier.basis(rangeval, nbasis=number_of_basis_function, period=diff(rangeval))
functional_data_squared<-Data2fd(
  y=Date_Data_B,
  argvals=seq(from=0, to=1, length.out=nrow(Date_Data_B)), 
  basis=basis)
principal_components<-pca.fd(fdobj=functional_data_squared,nharm=ncol(Date_Data_B))
eigenfunctions<-principal_components$harmonics
eigenfunctions_mat<-as.matrix(eigenfunctions$coefs)

# 0.95 이상의 설명력을 가지는 주성분 확인
variance_prop<-c(cumsum(eigenfunctions_mat[1,] / sum(eigenfunctions_mat[1,])))
PCAnum<-sum(variance_prop<0.95)
eigenfunctions_mat<-eigenfunctions_mat[,1:PCAnum]
view(eigenfunctions_mat)

# 주성분에 대한 개별 요인 SCORE 확인
Date_score<-principal_components$scores
rownames(Date_score)<-Date_Data_Code
view(Date_score)

# 코드 정규화 1.1, 2.1, 3.1
RP3<-daily_data[,c("1.1","2.1","3.1")]