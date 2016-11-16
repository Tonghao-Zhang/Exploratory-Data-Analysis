library(tcltk)
library(aplpack)
library(ggplot2)
time<-c(124,188,225,398,64,24,365,294,189,355,381,362,354,35,135,21,198,369,58,353,357,359,281,360,371,342,189,76,358,79,400,242,262,378,388)
time<-c(time,75,55,347,50,48,292,250,301,281,157,139,159,349,346,49,151,58,316,112,141,349,356,96,306,152,339,329,327,31,174,212,37,110,276,239,327,132,102,68,300,299,257,291,222)
censor<-c(0,0,0,9,0,0,9,0,0,9,9,9,9,0,0,0,0,9,0,9,9,0,0,9,9,9,0,0,0,0,9,0,0,9,9,0,0,0,0,0,0,0,0,0,0,0,0,9,9,0,0,0,9,0,0,9,9,0,9,0,9,9,9,0,0,0,0,0,0,0,9,0,0,0,9,9,0,9,0)

stem.leaf(time)
stem.leaf(time,m=5)
fulltime<-time(which(censor==0))
censoredtime<-time(which(censor>1))
stem.leaf.backback(fulltime,censoredtime,back.to.back = TRUE)

tmp<-capture.output(stem.leaf(time))
plot.new()
text(0,1,paste(tmp,collapse = '\n'),adj = c(0,1),family='mono')
d<-data.frame(time,censor,stringsAsFactors = False)
# tmp <- capture.output(stem(time))
# stemdf = data.frame(tmp, rr=1:length(tmp))
# ggplot(stemdf)+ geom_text(aes(x=rr, y=0, label=tmp), hjust=0) + 
#   coord_flip()+ theme_classic() + 
#   scale_x_discrete(breaks=NULL)+ 
#   scale_y_discrete(breaks=NULL, limits=c(0,1))+ 
#   theme(axis.text = element_blank(),
#         axis.title = element_blank(), 
#         axis.ticks=element_blank(), 
#         panel.grid=element_blank(), 
#         axis.line=element_blank())
label<-which(censor>1)
fulltime<-time(which(censor==0))
censoredtime<-time(which(censor>1))
stem.leaf.backback(fulltime,censoredtime,back.to.back = TRUE)
stem.leaf(cen)
stem.leaf(d[[i]])
cycle<-c(22.9,26.3,26.6,26.8,26.9,26.9,27.5,27.6,27.6,28.0,28.4,28.4,28.5,28.8,28.8,29.4,29.9,29.9,30.0,30.3,31.2,31.8)
fivenum(cycle)
box<-data.frame(days=cycle)
print(ggplot(box,aes(y=days))+geom_boxplot())

