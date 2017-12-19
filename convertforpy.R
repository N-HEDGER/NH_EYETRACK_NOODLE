setwd("/Users/nicholashedger/Documents/GitHub/NH_EYETRACK_NOODLE")
load('Workspace3.RData')

head(data)

library(eyetrackingR)
Sampled <- make_time_sequence_data(data, time_bin_size = 100,aois = c("NONSOCIAL","SOCIAL"),predictor_columns = c("scramb","X3"))




library(ggplot2)

Plottime(data,16,77)

data$id=rep(1:6160,each=500)


Plottime=function(dat,id)
{xboundu=1432
yboundu=822
xboundl=168
yboundl=378
ggplot(dat[dat$id==id,],aes(x=X1,y=X2))+geom_rect(xmin =168 ,xmax=712,ymin=yboundl,ymax=yboundu)+geom_rect(xmin =888 ,xmax=1432,ymin=yboundl,ymax=yboundu)+geom_point(size=3,alpha=.4,aes(color=factor(AOI)))+
  scale_x_continuous(limits=c(xboundl,xboundu))+scale_y_continuous(limits=c(yboundl,yboundu))+
  geom_path()
}



reduced=data

max(reduced$id)

reduced2=reduced[!is.na(reduced$X2),]


Plottime(data,1)