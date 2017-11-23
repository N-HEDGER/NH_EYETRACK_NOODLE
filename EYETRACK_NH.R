library(ggplot2)

const.rep_subs=77 #Reported subjects
const.rep_sample_rate=(1/500)*1000 #Reported sample rate

setwd("/Users/nicholashedger/Documents/Github/NH_EYETRACK_NOODLE")
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

Plotfix(DATA,1)


# Need to 
# 1) Upsample each trial so that there are 2501 data points per trial.
# 2) Define whether or not the data was in a particular ROI (boolean).


# This is subjects 1-10 for now.



# For now only do 1/2 the trials

DATA2=DATA

# Unscrambled only
DATA2=DATA2[DATA2$trial_1<41,]


DATA2=DATA2[DATA2$CURRENT_SAC_START_TIME<5000,]
DATA2=DATA2[DATA2$CURRENT_SAC_START_TIME>70,]

DATA2$SUB=factor(DATA2$SUB)
DATA2$TRIAL=factor(DATA2$TRIAL)

# 500 samples per trial (100 hz)
NEWFRAMELEN=(40*77)*500

# Add back in the time dimension (500 samples per trial)
EXPDATA_FRAME=data.frame(matrix(ncol=3,nrow=NEWFRAMELEN))
EXPDATA_FRAME$samp=rep(seq(0,5000,length=500))
EXPDATA_FRAME$ps=rep(as.numeric(levels(DATA2$SUB)),each=nrow(EXPDATA_FRAME)/77)
EXPDATA_FRAME$trial=rep(as.numeric(levels(DATA2$TRIAL)),each=nrow(EXPDATA_FRAME)/(77*40))




for (subject in 1:length(levels(DATA2$SUB))) {
  for (trial in 1:length(levels(DATA2$TRIAL))) {
    
    # If there is data for the trial
    if (nrow(DATA2[DATA2$SUB==subject & DATA2$TRIAL ==trial,])>0) {
    x=sprintf(c("Doing subject %f","Trial %f"),c(subject,trial))
    print(x)
    # Get the starting points
    locxs=DATA2[DATA2$SUB==subject & DATA2$TRIAL==trial,]$CURRENT_SAC_START_X
    locys=DATA2[DATA2$SUB==subject & DATA2$TRIAL==trial,]$CURRENT_SAC_START_Y
    
    # Get the ending points
    locx=DATA2[DATA2$SUB==subject & DATA2$TRIAL==trial,]$CURRENT_SAC_END_X
    locy=DATA2[DATA2$SUB==subject & DATA2$TRIAL==trial,]$CURRENT_SAC_END_Y
    
    # Get the side of the social stim
    side=DATA2[DATA2$SUB==subject & DATA2$TRIAL==trial,]$side[1]
    
    # Resample the times
    times=round((DATA2[DATA2$SUB==subject & DATA2$TRIAL==trial,]$CURRENT_SAC_START_TIME/10))
    
    # Starting x
    EXPDATA_FRAME[EXPDATA_FRAME$ps==subject & EXPDATA_FRAME$trial==trial,][1:times[1],]$X1=locxs[1]
    # Starting y
    EXPDATA_FRAME[EXPDATA_FRAME$ps==subject & EXPDATA_FRAME$trial==trial,][1:times[1],]$X2=locys[1]
    # Side
    EXPDATA_FRAME[EXPDATA_FRAME$ps==subject & EXPDATA_FRAME$trial==trial,]$X3=rep(side,nrow(EXPDATA_FRAME[EXPDATA_FRAME$ps==subject & EXPDATA_FRAME$trial==trial,]))
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



EXPDATA_FRAME$isinL=as.logical(ifelse(EXPDATA_FRAME$X1<712 & EXPDATA_FRAME$X1>168 & EXPDATA_FRAME$X2<822 & EXPDATA_FRAME$X2>378 ,1,0))
EXPDATA_FRAME$isinR=as.logical(ifelse(EXPDATA_FRAME$X1<1432 & EXPDATA_FRAME$X1>888 & EXPDATA_FRAME$X2<822 & EXPDATA_FRAME$X2>378 ,1,0))




EXPDATA_FRAME$AOI=rep(0,nrow(EXPDATA_FRAME))

for (i in 1:nrow(EXPDATA_FRAME)){
  if (is.na(EXPDATA_FRAME$X1[i])){
    EXPDATA_FRAME$AOI[i]==""}
  else if (EXPDATA_FRAME$X1[i]<1432 & EXPDATA_FRAME$X1[i]>888 & EXPDATA_FRAME$X2[i]<822 & EXPDATA_FRAME$X2[i]>378){
  EXPDATA_FRAME$AOI[i]=2}
  else if (EXPDATA_FRAME$X1[i]<712 & EXPDATA_FRAME$X1[i]>168 & EXPDATA_FRAME$X2[i]<822 & EXPDATA_FRAME$X2[i]>378){
    EXPDATA_FRAME$AOI[i]=1}
}


EXPDATA_FRAME$SOCIAL=as.logical(ifelse(EXPDATA_FRAME$X3==EXPDATA_FRAME$AOI,1,0))
EXPDATA_FRAME$NONSOCIAL=as.logical(ifelse(EXPDATA_FRAME$X3!=EXPDATA_FRAME$AOI & EXPDATA_FRAME$AOI!=0 ,1,0))



Plottime=function(dat,subj,trial)
{xboundu=1432
yboundu=822
xboundl=168
yboundl=378
ggplot(dat[dat$ps==subj & dat$trial==trial,],aes(x=X1,y=X2))+geom_rect(xmin =168 ,xmax=712,ymin=yboundl,ymax=yboundu)+geom_rect(xmin =888 ,xmax=1432,ymin=yboundl,ymax=yboundu)+geom_point(size=3,alpha=.4,aes(color=AOI))+
  scale_x_continuous(limits=c(xboundl,xboundu))+scale_y_continuous(limits=c(yboundl,yboundu))+
  geom_path(color="blue")
}

sdx=Plottime(EXPDATA_FRAME,1,1)

sd=qplot(EXPDATA_FRAME[EXPDATA_FRAME$ps==1 & EXPDATA_FRAME$trial==58,]$samp,EXPDATA_FRAME[EXPDATA_FRAME$ps==1 & EXPDATA_FRAME$trial==58,]$X1)

multiplot(sdx,sd)



library("Matrix")
library("lme4")
library("ggplot2")

library("eyetrackingR")


EXPDATA_FRAME$track=as.logical(rep(0,nrow(EXPDATA_FRAME)))


data <- make_eyetrackingr_data(EXPDATA_FRAME, 
                               participant_column = "ps",
                               trial_column = "trial",
                               time_column = "samp",
                               aoi_columns = c('SOCIAL','NONSOCIAL'),
                               treat_non_aoi_looks_as_missing = TRUE,trackloss_column="track"
)


data$isinL=as.logical(data$isinL)
data$isinR=as.logical(data$isinR)

data$Scramb=factor(data$Scramb,levels=c(1,2),labels=c("Unscrambled","Scrambled"))

response_time <- make_time_sequence_data(data, time_bin_size = 200,aois = c("SOCIAL","NONSOCIAL"),summarize_by = "ps")


plot(response_time)



tb_analysis <- analyze_time_bins(data = response_time, test = "t.test", alpha = .05,p_adjust_method = "bonferroni")


plot(tb_analysis, type = "estimate") + theme_light()
