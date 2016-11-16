# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
A<-c(1,3,5,1,2,2,2,2,1,2,6,1,3,2,6,5,5,4,1,3)
B<-c(5,6,10,7,6,9,6,21,8,6,14,13,11,10,5,7,5,7,11,4)
C<-c(4,8,7,5,11,11,4,11,11,17,9,13,14,5,8,10,10,9,8,6)
D<-c(10,18,11,17,12,12,12,18,18,11,19,21,17,17,16,17,17,12,21,19)
E<-c(18,29,19,13,17,20,22,19,24,23,17,28,22,11,15,17,22,25,14,26)
numbers<-c(A,B,C,D,E)

library(ggplot2)
library(car)
library(grid)
library(gridExtra)
group<-c(1:100)
for(i in 1:20)
  group[i]<-"A"
for(i in 21:40)
  group[i]<-"B"
for(i in 41:60)
  group[i]<-"C"
for(i in 61:80)
  group[i]<-"D"
for(i in 81:100)
  group[i]<-"E"
#creat spread-level data for original scale
spread<-c(3.0,4.5,4.5,6.0,6.5)
median<-c(2.0,7.0,9.0,17.0,19.5)
Log_spread<-sapply(spread,log10)
Log_median<-sapply(median,log10)
groupsl<-c("A","B","C","D","E")
sl<-data.frame(Log_spread,Log_median,groupsl)
#creat spread-level data for transformed scale
spreadt<-c(2.161819,2.281236,2.241917,2.501359,2.455428)
mediant<-c(1.184126,4.332358,5.352774,8.860648,9.835387)
Log_spreadt<-sapply(spreadt,log10)
Log_mediant<-sapply(mediant,log10)
slt<-data.frame(Log_spreadt,Log_mediant,groupsl)
lr1<-lm(Log_spread~Log_median)
lr2<-lm(Log_spreadt~Log_mediant)

pdf("HW3.pdf",paper="a4")
EDA<-data.frame(numbers,group)
par(mfrow=c(2,2))
p1<-ggplot(EDA,aes(x=factor(group),y=numbers))+geom_boxplot()
#p2<-spread.level.plot(numbers~group, data=EDA)
p<-0.6758451
trans<-function(x){
  return((x**p-1)/p)
}
trans(2)
numberst<-sapply(EDA$numbers,trans)
EDAtrans<-data.frame(numberst,group)
p3<-ggplot(EDAtrans,aes(x=factor(group),y=numberst))+geom_boxplot()
#p4<-spread.level.plot(numberst~group, data=EDAtrans)
p2<-ggplot(sl,aes(x=Log_median,y=Log_spread))+geom_point()+geom_abline(intercept=0.369,slope=0.331)
p4<-ggplot(slt,aes(x=Log_mediant,y=Log_spreadt))+geom_point()+geom_abline(intercept=0.323,slope=0.064)
grid.arrange(p1,p2,p3,p4,ncol=2)
dev.off()
