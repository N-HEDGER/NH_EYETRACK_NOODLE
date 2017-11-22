library(ggplot2)

const.rep_subs=77 #Reported subjects
const.rep_sample_rate=(1/500)*1000 #Reported sample rate

setwd("/Users/nickhedger/Documents/NH_EYETRACK_NOODLE")
DATA   <- read.csv("FV_data_raw.csv")

head(DATA)

# The last time subjects make a saccade is ~5000, so will assume that this is in ms.
max(DATA$CURRENT_SAC_START_TIME)


#Confirm sample rate 
sum(DATA$CURRENT_SAC_START_TIME%%const.rep_sample_rate==0)
#Sampling rate is once every 2 milliseconds.


DATA$SUB=factor(as.numeric(DATA$RECORDING_SESSION_LABEL))
DATA$TRIAL=factor(as.numeric(DATA$trial_1))

# Make index for fixation number
DATA$index=rep(0,nrow(DATA))

for (s in 1:length(levels(DATA$SUB))) {
for (t in 1:length(levels(DATA$TRIAL))) {
  if (nrow(DATA[DATA$SUB==s & DATA$TRIAL ==t,])>0) {
DATA[DATA$SUB==s & DATA$TRIAL ==t,]$index=1:nrow(DATA[DATA$SUB==s & DATA$TRIAL ==t,])}
}
}


# Would help to visualise some data.
Plotfix=function(dat,subj)
{xboundu=1432
yboundu=822
xboundl=168
yboundl=378
ggplot(dat[dat$SUB==subj,],aes(x=CURRENT_SAC_END_X,y=CURRENT_SAC_END_Y))+geom_rect(xmin =168 ,xmax=712,ymin=yboundl,ymax=yboundu)+geom_rect(xmin =888 ,xmax=1432,ymin=yboundl,ymax=yboundu)+geom_point(size=3,alpha=.4,color="red")+
  facet_wrap(~TRIAL,nrow=8)+scale_x_continuous(limits=c(xboundl,xboundu))+scale_y_continuous(limits=c(yboundl,yboundu))+
  geom_text(aes(label=index))+geom_path(color="blue")
}


# Need to 
# 1) Upsample each trial so that there are 2501 data points per trial.
# 2) Define whether or not the data was in a particular ROI (boolean).




DATA2=DATA[as.numeric(DATA$SUB)<30,]
DATA2=DATA2[DATA2$CURRENT_SAC_START_TIME<5000,]
DATA2=DATA2[DATA2$CURRENT_SAC_START_TIME>70,]

DATA2$SUB=factor(DATA2$SUB)
NEWFRAMELEN=(80*29)*500

EXPDATA_FRAME=data.frame(matrix(ncol=3,nrow=NEWFRAMELEN))
EXPDATA_FRAME$samp=rep(seq(0,5000,length=500))
EXPDATA_FRAME$ps=rep(as.numeric(levels(DATA2$SUB)),each=nrow(EXPDATA_FRAME)/29)
EXPDATA_FRAME$trial=rep(as.numeric(levels(DATA2$TRIAL)),each=nrow(EXPDATA_FRAME)/(29*80))


for (subject in 1:length(levels(DATA2$SUB))) {
  for (trial in 1:length(levels(DATA2$TRIAL))) {
    if (nrow(DATA2[DATA2$SUB==subject & DATA2$TRIAL ==trial,])>0) {
      x=sprintf(c("Doing subject %f","Trial %f"),c(subject,trial))
      print(x)
    locx=DATA2[DATA2$SUB==subject & DATA2$TRIAL==trial,]$CURRENT_SAC_END_X
    locy=DATA2[DATA2$SUB==subject & DATA2$TRIAL==trial,]$CURRENT_SAC_END_Y
    times=round((DATA2[DATA2$SUB==subject & DATA2$TRIAL==trial,]$CURRENT_SAC_START_TIME/10))
for (t in 1:length(times)){
  if (t<length(times)){
    EXPDATA_FRAME[EXPDATA_FRAME$ps==subject & EXPDATA_FRAME$trial==trial,][times[t]:times[t+1],]$X1=locx[t]
    EXPDATA_FRAME[EXPDATA_FRAME$ps==subject & EXPDATA_FRAME$trial==trial,][times[t]:times[t+1],]$X2=locy[t]
    
  }

  else{
    EXPDATA_FRAME[EXPDATA_FRAME$ps==subject & EXPDATA_FRAME$trial==trial,][times[t]:nrow(EXPDATA_FRAME[EXPDATA_FRAME$ps==subject & EXPDATA_FRAME$trial==trial,]),]$X1=locx[t]
    EXPDATA_FRAME[EXPDATA_FRAME$ps==subject & EXPDATA_FRAME$trial==trial,][times[t]:nrow(EXPDATA_FRAME[EXPDATA_FRAME$ps==subject & EXPDATA_FRAME$trial==trial,]),]$X2=locy[t]
  }
}
    }
    else{
      y=sprintf(c("Cannot do subject %f","Trial %f"),c(subject,trial))
      print(y)
    }

    
  }
}



