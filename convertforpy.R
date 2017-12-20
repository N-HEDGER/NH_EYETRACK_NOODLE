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




Plottime(data,425)

# Go through the data and pick out trials where there are very few unique coordinates (suggests that the eyes werent moved)
vectorx=rep(0,length(unique(data$id)))
vectory=rep(0,length(unique(data$id)))

for (i in 1:length(unique(data$id))){
  
  tempframe=data[data$id==i,]
  vectorx[i]=length(unique(tempframe$X1))
  vectory[i]=length(unique(tempframe$X2))
}
  
needle0=0
needle1=1
needle2=2
needle3=3
indicesx0 <- needle0 == vectorx
indicesx1 <- needle1 == vectorx
indicesx2 <- needle2 == vectorx
indicesx3 <- needle3 == vectorx


indices=c(which(indicesx0),which(indicesx1),which(indicesx2),which(indicesx3))



# Now go through and find cases where no coordinates are on the images

vectorL=rep(0,length(unique(data$id)))
vectorR=rep(0,length(unique(data$id)))

for (i in 1:length(unique(data$id))){
  tempframe=data[data$id==i,]
  vectorL[i]=nrow(tempframe[tempframe$isinL==TRUE,])
  vectorR[i]=nrow(tempframe[tempframe$isinR==TRUE,])
}



needle=0
indicesL <- needle == vectorL
indicesR <- needle == vectorR
which(indicesL)
which(indicesR)


Plottime(data3,1789)


#These all seem legitimate though, I dont see any real reason to get rid of them.

# Another question concerns how many fixations there are outside the boundary region





# Now lets get rid of the problematic cases

length(unique(data$id))

# Cases with an NA.
data2=data[!is.na(data$X1),]
  
length(unique(data2$id))

# Cases with too few fixations

data3=data2[!(data2$id %in% indices),]

length(unique(data3$id))


vectorO=rep(0,length(unique(data3$id)))

for (i in 1:length(unique(data3$id))){
  tempframe=data3[data3$id==i,]
  vectorO[i]=nrow(tempframe[tempframe$isinL=="NA" & tempframe$isinR=="NA",])
}


# Get rid of cases where they spend more than 300ms outside the stimulus area.
indices2=which(vectorO>300)



data4=data3[!(data3$id %in% indices2),]



length(unique(data4$id))

# Relabel new dataframe
data4$id=rep(1:5952,each=500)
data4$time=rep(1:500,5952)

# Now we need to create a new array for the labels.


scramb=rep(0,length(unique(data4$id)))
for (i in 1:length(unique(data4$id))){
  tempframe=data4[data4$id==i,]
  scramb[i]=tempframe$scramb[1]
}



TIMESERIES=data.frame(cbind(data4$id,data4$time,data4$X1,data4$X2,data4$ps))

head(TIMESERIES)

colnames(TIMESERIES)=c("id","time","X","Y","ps")

SCLABS=data.frame(cbind(scramb))
colnames(SCLABS)=c("sc")


setwd("/Users/nicholashedger/Documents")

write.csv(TIMESERIES,"TIMESERIES_CLEAN.csv")

write.csv(SCLABS,"SCLABELS_CLEAN.csv")

Plottime(data3,654)


data5=data4[data4$scramb==1,]


head(data5)
length(unique(data5$id))

data5$id=rep(1:2963,each=500)


side=rep(0,length(unique(data5$id)))
for (i in 1:length(unique(data5$id))){
  tempframe=data5[data5$id==i,]
  side[i]=tempframe$X3[1]
}





TIMESERIES2=data.frame(cbind(data5$id,data5$time,data5$X1,data5$X2,data5$ps))

head(TIMESERIES2)

colnames(TIMESERIES2)=c("id","time","X","Y","ps")

SIDELABS=data.frame(cbind(side))
colnames(SIDELABS)=c("side")


setwd("/Users/nicholashedger/Documents")

write.csv(TIMESERIES2,"TIMESERIES2_CLEAN.csv")

write.csv(SIDELABS,"SIDELABELS_CLEAN.csv")

