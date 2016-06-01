#导入数据
Data<-read.table("Analysis.txt",header=F,dec=".",col.names=c("season","size","speed","mxPH","mn02","Cl","N03","NH4","oP04","P04","Chla","a1","a2","a3","a4","a5","a6","a7"),na.strings=c("XXXXXXX"))

#得到数据基本的统计量
summary(Data)

#画直方图
#画QQ图
hist(Data$mxPH,prob=T)
qqnorm(Data$mxPH, main="Q-Q plot:mxPH")
qqline(Data$mxPH)

hist(Data$mn02,prob=T)
qqnorm(Data$mn02, main="Q-Q plot:mn02")
qqline(Data$mn02)

hist(Data$Cl,prob=T)
qqnorm(Data$Cl, main="Q-Q plot:CI")
qqline(Data$Cl)

hist(Data$N03,prob=T)
qqnorm(Data$N03, main="Q-Q plot:NO3")
qqline(Data$N03)

hist(Data$NH4,prob=T)
qqnorm(Data$NH4, main="Q-Q plot:NH4")
qqline(Data$NH4)

hist(Data$oP04,prob=T)
qqnorm(Data$oP04, main="Q-Q plot:oP04")
qqline(Data$oP04)

hist(Data$P04,prob=T)
qqnorm(Data$P04, main="Q-Q plot:P04")
qqline(Data$P04)

hist(Data$Chla,prob=T)
qqnorm(Data$Chla, main="Q-Q plot:Chla")
qqline(Data$Chla)


#画盒图

boxplot(Data$mxPH,col=c("steelblue"))
boxplot(Data$mn02,col=c("mediumturquoise"))
boxplot(Data$Cl,col=c("sandybrown"))
boxplot(Data$N03,col=c("hotpink"))
boxplot(Data$NH4,col=c("red"))
boxplot(Data$oP04,col=c("yellow"))
boxplot(Data$P04,col=c("green"))
boxplot(Data$Chla,col=c("orange"))

#获取存在缺失值的数据元组

Data[!complete.cases(Data),]

#统计存在缺失值的属性的频数，并获取对应最大值
x = table(Data$mxPH)
x <- sort(x,decreasing = TRUE)
fre <- x[1]
fre

x = table(Data$mn02)
x <- sort(x,decreasing = TRUE)
fre <- x[1]
fre

x = table(Data$mn02)
x <- sort(x,decreasing = TRUE)
fre <- x[1]
fre
 
x = table(Data$Cl)
x <- sort(x,decreasing = TRUE)
fre <- x[1]
fre
x = table(Data$N03)
x <- sort(x,decreasing = TRUE)
fre <- x[1]
fre
 
x = table(Data$NH4)
x <- sort(x,decreasing = TRUE)
fre <- x[1]
fre

x = table(Data$oP04)
x <- sort(x,decreasing = TRUE)
fre <- x[1]
fre
 
 
x = table(Data$P04)
x <- sort(x,decreasing = TRUE)
fre <- x[1]
fre


x = table(Data$Chla)
x <- sort(x,decreasing = TRUE)
fre <- x[1]
fre

#利用相似数据填补缺失值
#Data[38,"mn02"]<-median(Data[c(as.integer(names(sort)))])
library(cluster)
dist.mtx<-as.matrix(daisy(Data,stand=T))#计算相似矩阵
#which(!complete.cases(Data))

#Data[38,"mn02"]<-median(Data[c(as.integer(names(sort(dist.mtx[38,])[2:11]))),"mn02"],na.rm=T)
#apply(Data[c(as.integer(names(sort(dist.mtx[55,])[2:11]))),which(is.na(Data[55,]))],2,median,na.rm=T)
 
central.value<-function(x){
	if(is.numeric(x))
		median(x,na.rm=T)
	else if(is.factor(x))
		levels(x)[which.max(table(x))]
	else{
		f<-as.factor(x)
		levels(f)[which.max(table(f))]
	}
} 

 for(r in which(!complete.cases(Data)))
	Data[r,which(is.na(Data[r,]))]<-
		apply(data.frame(Data[c(as.integer(names(sort(dist.mtx[r,])[2:11]))),
			which(is.na(Data[r,]))]),2,central.value)
 
 write.table(Data, file = "E:/1_DataMining2015/assignment1/cleanData.txt", row.names = F, quote = F, sep="\t") 