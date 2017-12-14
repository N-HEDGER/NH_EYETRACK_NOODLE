
# Reduced data
# Fit a whole bunch of sensible models
# Define a function for leave one out 
# Sum of squared errors.
# Dummy data - show how EQ and GE impact on growth curves.



# First, reduce the data so that we are restricted to only subjects who have EQ and GE data.
response_timeT <- make_time_sequence_data(data, time_bin_size = 100,aois = c("NONSOCIAL","SOCIAL"),summarize_by = "ps",predictor_columns = c("scramb"))

# Adjust the participant factor to account for the fact that there was no p 22.
response_time_adjust_ps=response_timeT
response_time_adjust_ps$ps=as.numeric(response_time_adjust_ps$ps)
response_time_adjust_ps$ps=factor(ifelse(response_time_adjust_ps$ps>21,response_time_adjust_ps$ps+1,response_time_adjust_ps$ps))

response_time_adjust_ps$EQ=rep(NA,nrow(response_time_adjust_ps))

# Add the EQ and GE data
for (sub in 1:length(EQ_PS)){
  response_time_adjust_ps[response_time_adjust_ps$ps==EQ_PS[sub],]$EQ=EQ[sub]
}

response_time_adjust_ps$GE=rep(NA,nrow(response_time_adjust_ps))
for (sub in 1:length(GE_PS)){
  response_time_adjust_ps[response_time_adjust_ps$ps==GE_PS[sub],]$GE=GE[sub]
}


# Remove individuals with no EQ or GE data
EQFRAME=response_time_adjust_ps[!is.na(response_time_adjust_ps$EQ),]
EQFRAME$AOI=factor(EQFRAME$AOI)
EQGEFRAME=EQFRAME[!is.na(EQFRAME$GE),]



# Model 1 - Just AOI
model_1 <- lmer(Prop ~ AOI+(1|ps),data = EQGEFRAME[EQGEFRAME$scramb==1,], REML = FALSE)

# Model 2 - linear
model_2 <- lmer(Prop ~ AOI*(ot1)+(1 + ot1 |ps),data = EQGEFRAME[EQGEFRAME$scramb==1,], REML = FALSE)

model_3 <- lmer(Prop ~ AOI*(ot1+ot2)+(1 + ot1 + ot2 |ps),data = EQGEFRAME[EQGEFRAME$scramb==1,], REML = FALSE)

model_4 <- lmer(Prop ~ AOI*(ot1+ot2+ot3)+(1 + ot1 + ot2 + ot3 |ps),data = EQGEFRAME[EQGEFRAME$scramb==1,], REML = FALSE)


# The model with linear and quadratic is still the best.

# Now add in EQ

model_5 <- lmer(Prop ~ AOI*EQ*(ot1+ot2)+(1 + ot1 + ot2 |ps),data = EQGEFRAME[EQGEFRAME$scramb==1,], REML = FALSE)

model_6 <- lmer(Prop ~ AOI*EQ*GE*(ot1+ot2)+(1 + ot1 + ot2 |ps),data = EQGEFRAME[EQGEFRAME$scramb==1,], REML = FALSE)




library(pracma)

EQvec=linspace(range(EQGEFRAME$EQ)[1],range(EQGEFRAME$EQ)[2],10)
GEvec=linspace(range(EQGEFRAME$GE)[1],range(EQGEFRAME$GE)[2],10)



temp=EQGEFRAME[EQGEFRAME$ps==1,]
tempin=temp[temp$scramb==1,]


FRAMEBOI=data.frame()
for (EQ in 1:length(EQvec)){
    TEMPFRAME=tempin
    TEMPFRAME$EQ=rep(EQvec[EQ])
    FRAMEBOI=rbind(TEMPFRAME,FRAMEBOI)
}


FRAMEBOI=FRAMEBOI[rep(seq_len(nrow(FRAMEBOI)), 10), ]

FRAMEBOI$GE=rep(GEvec,each=1020)

  
FRAMEBOI2=cbind(FRAMEBOI,predict(model_6,FRAMEBOI))
  
  
  
colnames(FRAMEBOI2)=c(colnames(FRAMEBOI),"pred")

FRAMEBOI2$GE=factor(FRAMEBOI2$GE)
FRAMEBOI2$EQ=factor(FRAMEBOI2$EQ)

ggplot(FRAMEBOI2,aes(x=TimeBin,y=Prop))+facet_grid(GE~EQ)+geom_line(aes(x=TimeBin,y=pred,colour=AOI),size=2,linetype="solid")






EQGEFRAME=EQGEFRAME[EQGEFRAME$scramb==1,]
# We can get rid of redundant levels now, because all of the individual stuff is in the right place.
EQGEFRAME$ps=factor(EQGEFRAME$ps)



x=new.env()
createLOOstruct=function(frame){
  psvec=as.numeric(levels(frame$ps))
  envsin=strcat("In",as.character(levels(frame$ps)),"env")
  envsout=strcat("Out",as.character(levels(frame$ps)),"env")
  actual=strcat("Actual",as.character(levels(frame$ps)),"env")
  
  fit1=strcat("Fit1",as.character(levels(frame$ps)),"env")
  fitEQ=strcat("FitEQ",as.character(levels(frame$ps)),"env")
  fitEQGE=strcat("FitEQGE",as.character(levels(frame$ps)),"env")
  fitGE=strcat("FitGE",as.character(levels(frame$ps)),"env")
  fitAOI=strcat("FitAOI",as.character(levels(frame$ps)),"env")
  fitAOIlin=strcat("FitAOIlin",as.character(levels(frame$ps)),"env")
  
  predfit1=strcat("predfit1",as.character(levels(frame$ps)),"env")
  predfitEQ=strcat("predfitEQ",as.character(levels(frame$ps)),"env")
  predfitEQGE=strcat("predfitEQGE",as.character(levels(frame$ps)),"env")
  predfitGE=strcat("predfitGE",as.character(levels(frame$ps)),"env")
  predfitAOI=strcat("predfitAOI",as.character(levels(frame$ps)),"env")
  predfitAOIlin=strcat("predfitAOIlin",as.character(levels(frame$ps)),"env")
  
  error=strcat("error",as.character(levels(frame$ps)),"env")
  errorEQ=strcat("errorEQ",as.character(levels(frame$ps)),"env")
  errorEQGE=strcat("errorEQGE",as.character(levels(frame$ps)),"env")
  errorGE=strcat("errorGE",as.character(levels(frame$ps)),"env")
  errorAOI=strcat("errorAOI",as.character(levels(frame$ps)),"env")
  errorAOIlin=strcat("errorAOIlin",as.character(levels(frame$ps)),"env")
  
  k=new.env()
  
  assign("model1error",rep(0,length(psvec)),envir=k)
  
  for (i in 1:length(psvec)){
    assign(envsin[i],frame[frame$ps!=psvec[i],], envir = x)
    assign(envsout[i],frame[frame$ps==psvec[i] & !is.na(frame$Prop) ,], envir = x)
   
    assign(fit1[i],lmer(Prop ~ AOI*(ot1+ot2)+(1 + ot1 + ot2 |ps),data = get(envsin[i],envir=x), REML = FALSE), envir = x)
    assign(fitEQ[i],lmer(Prop ~ AOI*EQ*(ot1+ot2)+(1 + ot1 + ot2 |ps),data = get(envsin[i],envir=x), REML = FALSE), envir = x)
    assign(fitEQGE[i],lmer(Prop ~ AOI*EQ*GE*(ot1+ot2)+(1 + ot1 + ot2 |ps),data = get(envsin[i],envir=x), REML = FALSE), envir = x)
    assign(fitGE[i],lmer(Prop ~ AOI*GE*(ot1+ot2)+(1 + ot1 + ot2 |ps),data = get(envsin[i],envir=x), REML = FALSE), envir = x)
    assign(fitAOI[i],lmer(Prop ~ AOI+(1 |ps),data = get(envsin[i],envir=x), REML = FALSE), envir = x)
    assign(fitAOIlin[i],lmer(Prop ~ AOI*(ot1)+(1 + ot1 |ps),data = get(envsin[i],envir=x), REML = FALSE), envir = x)
    
    # Original model
    assign(predfit1[i],predict(get(fit1[i],envir = x),get(envsout[i],envir=x),allow.new.levels = TRUE), envir = x)
    assign(actual[i],get(envsout[i],envir=x)$Prop, envir = x)
    assign(error[i],sum((get(actual[i],envir=x) - get(predfit1[i],envir=x)) ^2), envir = x)
    
    # +EQ
    assign(predfitEQ[i],predict(get(fitEQ[i],envir = x),get(envsout[i],envir=x),allow.new.levels = TRUE), envir = x)
    assign(errorEQ[i],sum((get(actual[i],envir=x) - get(predfitEQ[i],envir=x)) ^2), envir = x)
    
    # +EQ + GE
    assign(predfitEQGE[i],predict(get(fitEQGE[i],envir = x),get(envsout[i],envir=x),allow.new.levels = TRUE), envir = x)
    assign(errorEQGE[i],sum((get(actual[i],envir=x) - get(predfitEQGE[i],envir=x)) ^2), envir = x)
    
    # GE
    assign(predfitGE[i],predict(get(fitGE[i],envir = x),get(envsout[i],envir=x),allow.new.levels = TRUE), envir = x)
    assign(errorGE[i],sum((get(actual[i],envir=x) - get(predfitGE[i],envir=x)) ^2), envir = x)
    
    # AOI
    assign(predfitAOI[i],predict(get(fitAOI[i],envir = x),get(envsout[i],envir=x),allow.new.levels = TRUE), envir = x)
    assign(errorAOI[i],sum((get(actual[i],envir=x) - get(predfitAOI[i],envir=x)) ^2), envir = x)
    
    # AOIlin
    assign(predfitAOIlin[i],predict(get(fitAOIlin[i],envir = x),get(envsout[i],envir=x),allow.new.levels = TRUE), envir = x)
    assign(errorAOIlin[i],sum((get(actual[i],envir=x) - get(predfitAOIlin[i],envir=x)) ^2), envir = x)
    
    print(psvec[i])
  }
  return(x)
}

hj=createLOOstruct(EQGEFRAME)

psvec=as.numeric(levels(EQGEFRAME$ps))

crossval=rep(0,length(psvec))
crossvalEQ=rep(0,length(psvec))
crossvalEQGE=rep(0,length(psvec))
crossvalGE=rep(0,length(psvec))
crossvalAOI=rep(0,length(psvec))
crossvalAOIlin=rep(0,length(psvec))


error=strcat("error",as.character(levels(EQGEFRAME$ps)),"env")
errorEQ=strcat("errorEQ",as.character(levels(EQGEFRAME$ps)),"env")
errorEQGE=strcat("errorEQGE",as.character(levels(EQGEFRAME$ps)),"env")
errorGE=strcat("errorGE",as.character(levels(EQGEFRAME$ps)),"env")
errorAOI=strcat("errorAOI",as.character(levels(EQGEFRAME$ps)),"env")
errorAOIlin=strcat("errorAOIlin",as.character(levels(EQGEFRAME$ps)),"env")

for (i in 1:length(psvec)){
crossval[i]= get(error[i],envir=x)
crossvalEQ[i]= get(errorEQ[i],envir=x)
crossvalEQGE[i]= get(errorEQGE[i],envir=x)
crossvalGE[i]= get(errorGE[i],envir=x)
crossvalAOI[i]= get(errorAOI[i],envir=x)
crossvalAOIlin[i]= get(errorAOIlin[i],envir=x)
}







FRAMEBOI=data.frame()
for (EQ in 1:length(EQvec)){
  TEMPFRAME=tempin
  TEMPFRAME$EQ=rep(EQvec[EQ])
  FRAMEBOI=rbind(TEMPFRAME,FRAMEBOI)
}


FRAMEBOI2=cbind(FRAMEBOI,predict(model_6,FRAMEBOI))



colnames(FRAMEBOI2)=c(colnames(FRAMEBOI),"pred")

FRAMEBOI2$GE=factor(FRAMEBOI2$GE)
FRAMEBOI2$EQ=factor(FRAMEBOI2$EQ)

ggplot(FRAMEBOI2,aes(x=TimeBin,y=Prop))+facet_grid(~EQ)+geom_line(aes(x=TimeBin,y=pred,colour=AOI),size=2,linetype="solid")+ylim(c(0,1))

response_time_T_mod=cbind(EQGEFRAME[EQGEFRAME$scramb==1 & !is.na(EQGEFRAME$Prop),],predict(model_5))

colnames(response_time_T_mod)=c(colnames(EQGEFRAME),"pred")

ggplot(response_time_T_mod[response_time_T_mod$scramb==1,],aes(x=TimeBin,y=Prop))+facet_wrap(~ps,nrow=8)+geom_line(aes(x=TimeBin,y=pred,colour=AOI),size=2,linetype="solid")+geom_point(aes(colour=AOI),size=0.5,shape=6)


