area<-c("Berkshire","Franklin-Hampshire","Holyoke-Chicopee","Springfield","Westfield","Fitchburg","Gardner-Athol","Blackstone Valley","Southbridge","Wocrester","Cambridge-Somerville","Concord","Lowell","Metropolitan-Beaverbrook","Mystic Valley","Danvers","Haverhill-Newburyport","Lawrence","Lynn","Tri-Cities","Eastern Middlesex","Cape Ann","Westwood-Norwood","Newton-Weston-Wellesley","South Shore","Framingham","Westborough","Boston State","Boston University","Lindermann Mental Health Center","Massachusetts Mental Health Center","Tufts Mental Health Center","Cape Cod and Islands","Brockton","Fall River","Foxboro","New Bedford","Plymouth","Taunton")
PVRTY<-c(61,54,70,81,46,63,54,44,53,60,80,25,59,42,34,46,63,62,67,54,31,54,31,26,41,34,42,82,202,89,84,133,85,51,91,45,102,48,60)#,202)
SING<-c(38,47,40,40,36,37,37,35,36,42,52,34,37,42,37,37,37,39,39,41,37,38,37,43,39,34,35,44,57,46,61,50,35,37,37,37,38,33,37)#,61)
RINC<-c(127,213,134,131,116,127,128,114,123,130,177,104,113,112,91,108,126,122,121,122,96,118,88,95,101,90,101,125,231,145,214,158,148,114,144,113,151,106,120)#,88)
INFM<-c(144,152,149,219,174,171,178,125,213,191,194,96,167,118,165,135,178,243,107,176,109,146,104,70,156,157,136,257,276,141,177,140,198,154,152,119,171,151,204)#,70)
WARD<-c(50,37,40,67,48,50,85,58,78,52,59,19,45,32,31,41,57,48,64,63,46,41,31,15,38,49,49,108,153,124,138,88,54,72,70,40,57,84,42)#,15)
RESP<-c(92,163,215,210,128,239,317,195,108,158,263,101,266,290,169,168,146,145,195,254,99,146,132,105,131,198,237,202,461,350,275,467,201,169,253,202,195,140,179)#,467)
ex1<-data.frame(PVRTY,SING,RINC,INFM,WARD,RESP)
pairs(ex1)
lm.1<-lm(RESP~PVRTY+SING+RINC+INFM+WARD,data = ex1)
cor(ex1,method = "kendall",use = "pairwise")
summary(lm.1)
##(slope,intercept) RRfit(response vaiable, independent variable)
RRfit<-function(response,independent){
  ##sort the data in ascending seqence
  response<-response[order(independent)]
  independent<-sort(independent)
  ##separate the data into different groups
  xleft<-independent[1:13];xright<-independent[27:39]
  yleft<-response[1:13];yright<-response[27:39]
  ##fit the RRline,iterate 5 times
  xL<-median(xleft);xR<-median(xright)
  yL<-median(yleft);yR<-median(yright)
  slope<-(yR-yL)/(xR-xL);slopeT<-slope
  intercept<-median(response-slope*independent);interceptT<-intercept
  for(i in 1:4){
    response<-response-slope*independent-intercept
    yleft<-response[1:13];yright<-response[27:39]
    xL<-median(xleft);xR<-median(xright)
    yL<-median(yleft);yR<-median(yright)
    slope<-(yR-yL)/(xR-xL)
    intercept<-median(response-slope*independent)
    slopeT<-slopeT+slope;interceptT<-interceptT+intercept
  }
  return(data.frame(slopeT,interceptT))
}
fit1<-RRfit(RESP,PVRTY)
RESP.1<-RESP-PVRTY*fit1$slopeT-fit1$interceptT
sweep<-function(y,x){
  return(y-RRfit(y,x)$slopeT*x-RRfit(y,x)$interceptT)
}
fit2<-RRfit(INFM,PVRTY)
SING.1<-sweep(SING,PVRTY)
RINC.1<-sweep(RINC,PVRTY)
INFM.1<-sweep(INFM,PVRTY)
WARD.1<-sweep(WARD,PVRTY)
pairs(data.frame(SING.1,RINC.1,INFM.1,WARD.1,RESP.1))
m<-cbind(RESP.1,SING.1,RINC.1,INFM.1,WARD.1)
cor(m,method = "kendall",use = "pairwise")
