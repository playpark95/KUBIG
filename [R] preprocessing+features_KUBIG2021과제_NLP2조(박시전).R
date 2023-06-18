#파일 불러오기
setwd('G:/From 2021/2021/2021-Pre/학회_시즌2/딥러닝 분반/소대회')
A<-read.csv('train_nums.csv',header=T)
B<-read.csv('test_nums.csv',header=T)

#작가별 분류(sub dataset 형성)
author0 <- subset(A,A$author==0)
author1 <- subset(A,A$author==1)
author2 <- subset(A,A$author==2)
author3 <- subset(A,A$author==3)
author4 <- subset(A,A$author==4)

#EDA (여러 개를 해보았으나 코드 유실로 생략)
par(mfrow=c(2,3))
hist(author0$num_words,breaks=50)
hist(author1$num_words,breaks=50)
hist(author2$num_words,breaks=50)
hist(author3$num_words,breaks=50)
hist(author4$num_words,breaks=50)

boxplot(author3$num_words)

#사용자 정의함수_1: 각 column별 IQR 계산
countfunc<-function(temp1){
  b<-c()
  for (col in colnames(temp1)){
    a<-fivenum(temp1[[col]])[4]-fivenum(temp1[[col]])[2]
    b<-c(b,a)
  }
  as.numeric(sprintf("%.4f", b))
}

#사용자 정의 함수_2: 데이터 정리(labeling)
Viewfunc<-function(temp2){
  a<-t(as.matrix(countfunc(temp2)))
  colnames(a)<-colnames(temp2)
  a[,-c(1,2)]
}

Viewfunc(author0)
Viewfunc(author1)
Viewfunc(author2)
Viewfunc(author3)
Viewfunc(author4)

#사용자 정의 함수_3: 아웃라이어 수 탐색 및 제외할 구간 조절 목적용
lenchk_func<-function(data){
  b<-c()
  for (col in colnames(data)[-c(1,2,15:20)]){
    temp1<-which(data[[col]]>fivenum(data[[col]])[4]+1.5*Viewfunc(data)[[col]]*2)
    b<-c(b,temp1)
    b<-unique(b)
  }
  print(length(b))
}

lenchk_func(author0)
lenchk_func(author1)
lenchk_func(author2)
lenchk_func(author3)
lenchk_func(author4)
## 이 결과 기존의 아웃라이어 정의인 Q3 +1.5IQR로는 데이터 손실이 너무 크고,
### 따라서 Q3 + 3IQR을 아웃라이어 범위로 설정했으나 그럼에도 author3은 너무 많은 데이터가 포함됨을 확인하였음.


##아웃라이어 선정 및 제외 사용자 정의 함수(author3 제외)
outlier_func<-function(data){
  b<-c()
  for (col in colnames(data)[-c(1,2,15:20)]){
    temp1<-which(data[[col]]>fivenum(data[[col]])[4]+1.5*Viewfunc(data)[[col]]*2)
    b<-c(b,temp1)
    b<-unique(b)
  }
  print(b)
}

author0<-author0[-outlier_func(author0),]
author1<-author1[-outlier_func(author1),]
author2<-author2[-outlier_func(author2),]
author4<-author4[-outlier_func(author4),]


### author3 전용 outlier 제거 함수
outlier_func3<-function(data){
  b<-c()
  for (col in colnames(data)[-c(1,2,15:20)]){
    temp1<-which(data[[col]]>fivenum(data[[col]])[4]+1.5*Viewfunc(data)[[col]]*4)
    b<-c(b,temp1)
    b<-unique(b)
  }
  print(b)
}

author3<-author3[-outlier_func3(author3),]