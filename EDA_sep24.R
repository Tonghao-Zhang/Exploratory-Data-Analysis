pdf("EDA_sep26.pdf",paper = "a4")
data1<-function(){
  names<-c("Defense","HEW","Agriculture","Treasury","Congress","Commerce","Energy Research and Dev.Admin","NASA","Transportation","HUD","White House","Veterans Admin")
  number_employee<-c(1486,388,650,202,446,164,128,208,117,69,85,47)
  cost<-c(24508000,21000000,11467300,5798235,5663174,5683609,5236800,4500000,2913509,2455000,2300000,1313300)
  return(data.frame(names,number_employee,cost))
}



##using method of 5A
par(mfrow=c(2,2))
first<-data1()
first<-first[order(first[,2]),]
fit<-function(dataframe){
  left<-dataframe[1:4,];right<-dataframe[9:12,]
  xL<-median(left[,2]);xR<-median(right[,2])
  yL<-median(left[,3]);yR<-median(right[,3])
  slope<-(yR-yL)/(xR-xL)
  intercept<-median(dataframe$cost-slope*dataframe$number_employee)
  return(data.frame(slope,intercept))
}

cof<-function(RRline,RRfit){
  RRline[,1]<-RRline[,1]+RRfit[,1]
  RRline[,2]<-RRline[,2]+RRfit[,2]
  return(RRline)
}

renew<-function(dataframe,RRline){
  dataframe[,3]<-dataframe[,3]-dataframe[,2]*RRline[,1]-RRline[,2]
  return(dataframe)
}

newline<-function(b,a){
  slope<-b;intercept<-a
  return(data.frame(slope,intercept))
}

RRline<-newline(0,0)
RRfit<-fit(first)
RRline<-cof(RRline,RRfit)
#plot(first$number_employee,first$cost)+abline(b=RRline$slope,a=RRline$intercept)
for(i in 1:30){
  RRfit<-fit(renew(first,RRline))
  RRline<-cof(RRline,RRfit)
}
plot(first$number_employee,first$cost,xlab = "number of employee",ylab = "cost",main = "RRline 5A",sub = "b = 18349, a = 753502")+abline(b=RRline$slope,a=RRline$intercept)

##using method of 5B
first<-data1()
first<-first[order(first[,2]),]
RRline<-newline(0,0)
RRfit0<-fit(first)
RRline<-cof(RRline,RRfit0)

RRfit1<-fit(renew(first,RRline))
RRline<-cof(RRline,RRfit1)
converge<-0
iteration.time<-0
while(converge==0){
  
  if(RRfit0[,1] * RRfit1[,1] > 0.001){
    RRfit2<-fit(renew(first,RRline))
    RRline<-cof(RRline,RRfit2)
    RRfit0<-RRfit1
    RRfit1<-RRfit2
  }
  if(RRfit0[,1]*RRfit1[,1]< -0.001){
    RRfit2<-newline(0,0)
    RRfit2[,1]<-0.5*(RRfit1[,1]+RRfit0[,1])
    RRfit2[,2]<-median(renew(first,RRline)[,3]-RRfit[,1]*first[,2])
    RRline<-cof(RRline,RRfit2)
    RRfit0<-RRfit1
    RRfit1<-RRfit2
  }
  if(abs(RRfit1[,1])<=0.001){
   converge<-1 
  }
  iteration.time<-iteration.time+1
}  
iteration.time
plot(first$number_employee,first$cost,xlab = "number of employee",ylab = "cost",main = "RRline 5B", sub = "b = 28213, a = -50294")+abline(b=RRline$slope,a=RRline$intercept)



data2<-function(){
  child<-c(1:18)
  age<-c(109,113,115,116,119,120,121,124,126,129,130,133,134,135,137,139,141,142)
  height<-c(137.6,147.8,136.8,140.7,132.7,145.4,135.0,133.0,148.5,148.3,147.5,148.8,133.2,148.7,152.0,150.6,165.3,149.9)
  return(data.frame(child,age,height))
}
second<-data2()
second<-second[order(second[,2]),]
##wald
slope.wald<-(sum(second$height[10:18])-sum(second$height[1:9]))/(sum(second$age[10:18])-sum(second$age[1:9]))
intecept.wald<-mean(second$height)-slope.wald*mean(second$age)
#par(mfrow=c(3,1))
plot(second$age,second$height,xlab = "age",ylab = "height",main = "Wald ex2",sub = "b = 0.553, a = 74.42")+abline(a=intecept.wald,b=slope.wald)

##Bartlett
slope.bartlett<-(mean(second$height[13:18])-mean(second$height[1:6]))/(mean(second$age[13:18])-mean(second$age[1:6]))
intecept.bartlett<-mean(second$height)-slope.bartlett*mean(second$age)
plot(second$age,second$height,xlab = "age",ylab = "height",main = "Bartlett ex2",sub = "b = 0.432, a = 89.80")+abline(a=intecept.bartlett,b=slope.bartlett)
##Brown and Mood
converge<-0
iteration.time<-0
left<-second[1:9,];right<-second[10:18,]
slope.BM=1
while(converge==0){
  medL<-median(left[,3]-slope.BM*left[,2])
  medR<-median(right[,3]-slope.BM*right[,2])
  if(medL>medR+0.001){
    slope.BM<-slope.BM*0.9
  }
  if(medL<medR-0.001){
    slope.BM<-slope.BM*1.1
  }
  if(abs(medL-medR)<=0.001){
    converge<-1}
  iteration.time<-iteration.time+1
}  
intercept.BM<-medL
plot(second$age,second$height,xlab = "age",ylab = "height",main = "Brown Mood ex2",sub = "b = 0.479, a = 84.38")+abline(a=intercept.BM,b=slope.BM)
medL<-median(left[,3]-slope.BM*left[,2])
medR<-median(right[,3]-slope.BM*right[,2])
medL
medR
slope.BM
iteration.time

data3<-function(){
  child<-c(1:18)
  age<-c(109,113,115,116,119,120,121,124,126,129,130,133,134,135,137,139,141,142)
  height<-c(13.76,147.8,136.8,140.7,132.7,145.4,135.0,133.0,148.5,148.3,147.5,148.8,133.2,148.7,152.0,150.6,165.3,149.9)
  return(data.frame(child,age,height))
}
##RRline
#par(mfrow=c(3,1))
third<-data3()
third<-third[order(third[,2]),]
fit<-function(dataframe){
  left<-dataframe[1:6,];right<-dataframe[13:18,]
  xL<-median(left[,2]);xR<-median(right[,2])
  yL<-median(left[,3]);yR<-median(right[,3])
  slope<-(yR-yL)/(xR-xL)
  intercept<-median(dataframe[,3]-slope*dataframe[,2])
  return(data.frame(slope,intercept))
}

cof<-function(RRline,RRfit){
  RRline[,1]<-RRline[,1]+RRfit[,1]
  RRline[,2]<-RRline[,2]+RRfit[,2]
  return(RRline)
}

renew<-function(dataframe,RRline){
  dataframe[,3]<-dataframe[,3]-dataframe[,2]*RRline[,1]-RRline[,2]
  return(dataframe)
}

newline<-function(b,a){
  slope<-b;intercept<-a
  return(data.frame(slope,intercept))
}

RRline<-newline(0,0)
RRfit<-fit(third)
RRline<-cof(RRline,RRfit)

for(i in 1:30){
  RRfit<-fit(renew(third,RRline))
  RRline<-cof(RRline,RRfit)
}
plot(third[,2],third[,3],xlab = "age",ylab = "height",main = "RRline ex3",sub = "b = 0.507, a = 80.8")+abline(b=RRline$slope,a=RRline$intercept)
##least square regression
LSR<-lm(height~age,data=third)
plot(third[,2],third[,3],xlab = "age",ylab = "height",main = "LSR ex3",sub = "b = 1.76, a = -85.39")+abline(b=1.759,a=-85.393)
##bartlett
slope.bartlett<-(mean(third$height[13:18])-mean(third$height[1:6]))/(mean(third$age[13:18])-mean(third$age[1:6]))
intecept.bartlett<-mean(third$height)-slope.bartlett*mean(third$age)
plot(third$age,third$height,xlab = "age",ylab = "height",main = "Bartlett ex3",sub = "b = 1.34, a = -32.57")+abline(a=intecept.bartlett,b=slope.bartlett)
dev.off()