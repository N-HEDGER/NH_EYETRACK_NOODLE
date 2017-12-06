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

# Now scram
DATAS=DATA[DATA$trial_1>40,]
DATAS=DATAS[DATAS$CURRENT_SAC_START_TIME<5000,]
DATAS=DATAS[DATAS$CURRENT_SAC_START_TIME>70,]

DATAS$SUB=factor(DATAS$SUB)
DATAS$TRIAL=factor(DATAS$TRIAL)

# 500 samples per trial (100 hz)
NEWFRAMELEN=(40*77)*500

# Add back in the time dimension (500 samples per trial)
EXPDATA_FRAMES=data.frame(matrix(ncol=3,nrow=NEWFRAMELEN))
EXPDATA_FRAMES$samp=rep(seq(0,5000,length=500))
EXPDATA_FRAMES$ps=rep(as.numeric(levels(DATAS$SUB)),each=nrow(EXPDATA_FRAMES)/77)
EXPDATA_FRAMES$trial=rep(as.numeric(levels(DATAS$TRIAL)),each=nrow(EXPDATA_FRAMES)/(77*40))


# Accidentally overwritten EXPDATA_FRAME
for (subject in 1:length(levels(DATAS$SUB))) {
  for (trial in 1:length(levels(DATAS$TRIAL))) {
    trial=(trial+40)
    # If there is data for the trial
    if (nrow(DATAS[DATAS$SUB==subject & DATAS$TRIAL ==trial,])>0) {
      x=sprintf(c("S_Doing subject %f","Trial %f"),c(subject,trial))
      print(x)
      # Get the starting points
      locxs=DATAS[DATAS$SUB==subject & DATAS$TRIAL==trial,]$CURRENT_SAC_START_X
      locys=DATAS[DATAS$SUB==subject & DATAS$TRIAL==trial,]$CURRENT_SAC_START_Y
      
      # Get the ending points
      locx=DATAS[DATAS$SUB==subject & DATAS$TRIAL==trial,]$CURRENT_SAC_END_X
      locy=DATAS[DATAS$SUB==subject & DATAS$TRIAL==trial,]$CURRENT_SAC_END_Y
      
      # Get the side of the social stim
      side=DATAS[DATAS$SUB==subject & DATAS$TRIAL==trial,]$side[1]
      
      # Resample the times
      times=round((DATAS[DATAS$SUB==subject & DATAS$TRIAL==trial,]$CURRENT_SAC_START_TIME/10))
      
      # Starting x
      EXPDATA_FRAMES[EXPDATA_FRAMES$ps==subject & EXPDATA_FRAMES$trial==trial,][1:times[1],]$X1=locxs[1]
      # Starting y
      EXPDATA_FRAMES[EXPDATA_FRAMES$ps==subject & EXPDATA_FRAMES$trial==trial,][1:times[1],]$X2=locys[1]
      # Side
      EXPDATA_FRAMES[EXPDATA_FRAMES$ps==subject & EXPDATA_FRAMES$trial==trial,]$X3=rep(side,nrow(EXPDATA_FRAMES[EXPDATA_FRAMES$ps==subject & EXPDATA_FRAMES$trial==trial,]))
      for (t in 1:length(times)){
        if (t<length(times)){
          EXPDATA_FRAMES[EXPDATA_FRAMES$ps==subject & EXPDATA_FRAMES$trial==trial,][times[t]:times[t+1],]$X1=locx[t]
          EXPDATA_FRAMES[EXPDATA_FRAMES$ps==subject & EXPDATA_FRAMES$trial==trial,][times[t]:times[t+1],]$X2=locy[t]
          
        }
        
        else{
          EXPDATA_FRAMES[EXPDATA_FRAMES$ps==subject & EXPDATA_FRAMES$trial==trial,][times[t]:nrow(EXPDATA_FRAMES[EXPDATA_FRAMES$ps==subject & EXPDATA_FRAMES$trial==trial,]),]$X1=locx[t]
          EXPDATA_FRAMES[EXPDATA_FRAMES$ps==subject & EXPDATA_FRAMES$trial==trial,][times[t]:nrow(EXPDATA_FRAMES[EXPDATA_FRAMES$ps==subject & EXPDATA_FRAMES$trial==trial,]),]$X2=locy[t]
        }
      }
    }
    else{
      y=sprintf(c("Cannot do subject %f","Trial %f"),c(subject,trial))
      print(y)
    }
    
  }
}


# first half is intact, second is scrambled.
EXPDATA_FRAME_BIG=rbind(EXPDATA_FRAME,EXPDATA_FRAMES)


EXPDATA_FRAME_BIG$isinL=as.logical(ifelse(EXPDATA_FRAME_BIG$X1<712 & EXPDATA_FRAME_BIG$X1>168 & EXPDATA_FRAME_BIG$X2<822 & EXPDATA_FRAME_BIG$X2>378 ,1,0))
EXPDATA_FRAME_BIG$isinR=as.logical(ifelse(EXPDATA_FRAME_BIG$X1<1432 & EXPDATA_FRAME_BIG$X1>888 & EXPDATA_FRAME_BIG$X2<822 & EXPDATA_FRAME_BIG$X2>378 ,1,0))

EXPDATA_FRAME_BIG$scramb=rep(c(1,2),each=1540000)


EXPDATA_FRAME_BIG$AOI=rep(0,nrow(EXPDATA_FRAME_BIG))

for (i in 1:nrow(EXPDATA_FRAME_BIG)){
  if (is.na(EXPDATA_FRAME_BIG$X1[i])){
    EXPDATA_FRAME_BIG$AOI[i]==""}
  else if (EXPDATA_FRAME_BIG$X1[i]<1432 & EXPDATA_FRAME_BIG$X1[i]>888 & EXPDATA_FRAME_BIG$X2[i]<822 & EXPDATA_FRAME_BIG$X2[i]>378){
  EXPDATA_FRAME_BIG$AOI[i]=2}
  else if (EXPDATA_FRAME_BIG$X1[i]<712 & EXPDATA_FRAME_BIG$X1[i]>168 & EXPDATA_FRAME_BIG$X2[i]<822 & EXPDATA_FRAME_BIG$X2[i]>378){
    EXPDATA_FRAME_BIG$AOI[i]=1}
}

EXPDATA_FRAME_BIG$SOCIAL=as.logical(ifelse(EXPDATA_FRAME_BIG$X3==EXPDATA_FRAME_BIG$AOI,1,0))
EXPDATA_FRAME_BIG$NONSOCIAL=as.logical(ifelse(EXPDATA_FRAME_BIG$X3!=EXPDATA_FRAME_BIG$AOI & EXPDATA_FRAME_BIG$AOI!=0 ,1,0))



Plottime=function(dat,subj,trial)
{xboundu=1432
yboundu=822
xboundl=168
yboundl=378
ggplot(dat[dat$ps==subj & dat$trial==trial,],aes(x=X1,y=X2))+geom_rect(xmin =168 ,xmax=712,ymin=yboundl,ymax=yboundu)+geom_rect(xmin =888 ,xmax=1432,ymin=yboundl,ymax=yboundu)+geom_point(size=3,alpha=.4,aes(color=AOI))+
  scale_x_continuous(limits=c(xboundl,xboundu))+scale_y_continuous(limits=c(yboundl,yboundu))+
  geom_path(color="blue")
}

sdx=Plottime(EXPDATA_FRAME,1,5)

sd=qplot(EXPDATA_FRAME[EXPDATA_FRAME$ps==1 & EXPDATA_FRAME$trial==1,]$samp,EXPDATA_FRAME[EXPDATA_FRAME$ps==1 & EXPDATA_FRAME$trial==1,]$X1)

multiplot(sdx,sd)



library("Matrix")
library("lme4")
library("ggplot2")

library("eyetrackingR")

COPY=EXPDATA_FRAME_BIG
EXPDATA_FRAME_BIG$track=as.logical(rep(0,nrow(EXPDATA_FRAME_BIG)))


data <- make_eyetrackingr_data(EXPDATA_FRAME_BIG, 
                               participant_column = "ps",
                               trial_column = "trial",
                               time_column = "samp",
                               aoi_columns = c('isinL','isinR',"SOCIAL","NONSOCIAL"),
                               treat_non_aoi_looks_as_missing = TRUE,trackloss_column="track"
)


data$scramb=factor(data$scramb,levels=c(1,2),labels=c("Intact","Scrambled"))
data$X3=factor(data$X3,levels=c(1,2),labels=c("LeftSoc","Rightsoc"))


response_window_agg_by_sub <- make_time_window_data(data, aois=c("SOCIAL","NONSOCIAL"),summarize_by = "ps",predictor_columns = c("scramb"))

plot(response_window_agg_by_sub, predictor_column = "scramb")+geom_point(aes(colour=ps),position = position_jitter(w=0.3))

library(lme4)
library(afex)
library(phia)
library(nlme)
library(effects)

# Set up model
model_window <- lmer(Prop ~ AOI*scramb + (1 | ps), data = response_window_agg_by_sub, REML = FALSE)

model_window_p = mixed(Prop ~ AOI*scramb+(1|ps), response_window_agg_by_sub)

# Get p values
primemodp

# Test interactions
testInteractions(model_window, fixed=c("scramb"), pairwise=c("AOI"),adjustment="holm")



# Time sequence data
response_time <- make_time_sequence_data(data, time_bin_size = 100,aois = c("NONSOCIAL","SOCIAL"),summarize_by = "ps",predictor_columns = c("scramb"))

plot(response_time, predictor_column = "scramb")


model_time_sequence_intact1 <- lmer(Prop ~ AOI+(1|ps),data = response_time[response_time$scramb==1,], REML = FALSE)
model_time_sequence_intact2 <- lmer(Prop ~ AOI*(ot1)+(1|ps),data = response_time[response_time$scramb==1,], REML = FALSE)
model_time_sequence_intact3 <- lmer(Prop ~ AOI*(ot1+ot2)+(1|ps),data = response_time[response_time$scramb==1,], REML = FALSE)
model_time_sequence_intact4 <- lmer(Prop ~ AOI*(ot1+ot2+ot3)+(1|ps),data = response_time[response_time$scramb==1,], REML = FALSE)

anova(model_time_sequence_intact1,model_time_sequence_intact2)
anova(model_time_sequence_intact2,model_time_sequence_intact3)
anova(model_time_sequence_intact3,model_time_sequence_intact4)

# Indicates that best fitting model is linear + quadratic - so chack that quadratic alone isn't better
model_time_sequence_intact5 <- lmer(Prop ~ AOI*(ot2)+(1|ps),data = response_time[response_time$scramb==1,], REML = FALSE)


anova(model_time_sequence_intact3,model_time_sequence_intact5)

# Model 3 is best fitting
plot(response_time[response_time$scramb==1,], predictor_column = c("AOI"), dv = "Prop", model = model_time_sequence_intact3) +theme_light()



model_time_sequence_scramb1 <- lmer(Prop ~ AOI+(1|ps),data = response_time[response_time$scramb==2,], REML = FALSE)
model_time_sequence_scramb2 <- lmer(Prop ~ AOI*(ot1)+(1|ps),data = response_time[response_time$scramb==2,], REML = FALSE)
model_time_sequence_scramb3 <- lmer(Prop ~ AOI*(ot1+ot2)+(1|ps),data = response_time[response_time$scramb==2,], REML = FALSE)

anova(model_time_sequence_scramb1,model_time_sequence_scramb2)
anova(model_time_sequence_scramb1,model_time_sequence_scramb3)
# Linear + quadratic improves - but linear itself did not - is quadratic better than linear/quadratic?

model_time_sequence_scramb4 <- lmer(Prop ~ AOI*(ot2)+(1|ps),data = response_time[response_time$scramb==2,], REML = FALSE)
anova(model_time_sequence_scramb4,model_time_sequence_scramb3)
# Yes it is


# Model 4 is best fitting (cubic term, but no linear term)
plot(response_time[response_time$scramb==2,], predictor_column = c("AOI"), dv = "Prop", model = model_time_sequence_scramb4) +theme_light()



# Divergence analysis
# Because of the weird eyetrackingR synntax, we need to restructure the data slightly so that we can investigate the main effect of AOI.
# So we instead look at whether the observer was looking into the
# left or right AOI as a function of where the social image was.

response_time2 <- make_time_sequence_data(data, time_bin_size = 100,aois = c("isinL","isinR"),summarize_by = "ps",predictor_columns = c("scramb","X3"))

# X3 codes where the social image was.
response_time2$side=factor(response_time2$X3)

tb_analysis_INT <- analyze_time_bins(data = response_time2[response_time2$scramb==1,],test = "t.test",aoi=c("isinL"),predictor_column =c("side"),  alpha = .05,p_adjust_method = 'holm')
plot(tb_analysis_INT)


# Intact - Significant everywhere from 100 ms onwards
summary(tb_analysis_INT)


tb_analysis_SCR <- analyze_time_bins(data = response_time2[response_time2$scramb==2,],test = "t.test",aoi=c("isinL"),predictor_column =c("side"),  alpha = .05,p_adjust_method = 'holm')
plot(tb_analysis_SCR)


# Scrambled - no significant effects when adjusting for time bins!
summary(tb_analysis_SCR)



# Where do intact images diverge from scrambled images in terms of looking at the social AOI
tb_analysis <- analyze_time_bins(data = response_time,test = "t.test",aoi=c("SOCIAL"),predictor_column =c("scramb"),  alpha = .05,p_adjust_method = 'holm')

plot(tb_analysis, type = "estimate") + theme_light()

summary(tb_analysis)



response_timeR <- make_time_sequence_data(data, time_bin_size = 100,aois = c("isinR"),predictor_columns=c("X3"),summarize_by = "ps")
tb_analysisR <- analyze_time_bins(data = response_timeL,test = "t.test",predictor_column =c("X3"),  alpha = .05,p_adjust_method = 'bonferroni')
plot(response_timeR, predictor_column = "X3") + theme_light()
plot(tb_analysisR, type = "estimate") + theme_light()


response_timeG <- make_time_sequence_data(data, time_bin_size = 100,aois = c("isinL","isinR"),predictor_columns=c("X3"),summarize_by = "ps")
plot(response_timeG, predictor_column = "X3") + theme_light()


model_time_sequence <- lmer(Prop ~ AOI*(ot)+(1|ps),data = response_time, REML = FALSE)

broom::tidy(model_time_sequence, effects = "fixed")
drop1(model_time_sequence, ~., test="Chi")

plot(response_time, predictor_column = "AOI", dv = "Prop", model = model_time_sequence) +theme_light()



response_window <- subset_by_window(data, window_start_time = 0, window_end_time = 50000, rezero = FALSE)


onsets <- make_onset_data(response_window, onset_time = 500, fixation_window_length = 50, target_aoi='SOCIAL',distractor_aoi = 'NONSOCIAL')
# participants' ability to orient to the trial target overall:
plot(onsets) + theme(legend.text=element_text(size=5))

onset_switches <- make_switch_data(onsets)

# visualize subject's switch times
plot(onset_switches)

model_switches <- lmer(FirstSwitch ~ FirstAOI + (1 | ps), data=onset_switches, REML=FALSE)

drop1(model_switches,~.,test="Chi")

tb_analysis <- analyze_time_bins(data = response_time, test = "t.test",predictor_column = "AOI", alpha = .05,p_adjust_method = "bonferroni",treatment_level = NONSOCIAL)


plot(tb_analysis,type="ne")

response_window <- subset_by_window(data,window_start_time = 0, 
                                    window_end_time = 5000, rezero = FALSE)

data_summaryL <- describe_data(response_window,describe_column='isinL', group_columns=c('X3','ps'))
xL=plot(data_summaryL)
XL2=xL+xlab("Social Image Location")+ggtitle("Proportion viewing left image")


data_summaryR <- describe_data(response_window,describe_column='isinR', group_columns=c('X3','ps'))
xR=plot(data_summaryR)
xR2=xR+xlab("Social Image Location")+ggtitle("Proportion viewing right image")


gh=data.frame()
# Plot fixed effects
plotfixed=function(model){
for (sub in 1:77){
linear=ranef(model)$ps$ot1[sub]*response_time$ot1[1:50]
quadratic=ranef(model)$ps$ot2[sub]*response_time$ot2[1:50]
timebin=response_time$TimeBin[1:50]
gh=data.frame(rbind(cbind(sub,timebin,linear,quadratic),gh))
}
  return(gh)
}

curvesIN=plotfixed(model_time_sequence_intact3)
curvesSC=plotfixed(model_time_sequence_scramb3)

curvesall=rbind(curvesIN,curvesSC)
curvesall$stim=rep(c(1,2),each=3850)
curvesall$stim=factor(curvesall$stim)

ggplot(curvesall,aes(x=timebin,y=linear))+facet_wrap(~sub,nrow=8)+geom_point(aes(colour=stim))




curves=plotfixed(model_time_sequence_intact3)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
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



multiplot(XL2,xR2,cols=2)
