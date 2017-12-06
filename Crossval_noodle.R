
# Reduced data
# Fit a whole bunch of sensible models
# Define a function for leave one out 
# Sum of squared errors.
# Dummy data - show how EQ and GE impact on growth curves.






response_timeT <- make_time_sequence_data(data, time_bin_size = 100,aois = c("NONSOCIAL","SOCIAL"),summarize_by = "ps",predictor_columns = c("scramb"))

response_time_adjust_ps=response_timeT
response_time_adjust_ps$ps=as.numeric(response_time_adjust_ps$ps)
response_time_adjust_ps$ps=factor(ifelse(response_time_adjust_ps$ps>21,response_time_adjust_ps$ps+1,response_time_adjust_ps$ps))

response_time_adjust_ps$EQ=rep(NA,nrow(response_time_adjust_ps))

# Add EQ data. Here I add the overall score (no effects are detected for other sub-scales)
for (sub in 1:length(EQ_PS)){
  response_time_adjust_ps[response_time_adjust_ps$ps==EQ_PS[sub],]$EQ=EQ[sub]
}

response_time_adjust_ps$GE=rep(NA,nrow(response_time_adjust_ps))
for (sub in 1:length(GE_PS)){
  response_time_adjust_ps[response_time_adjust_ps$ps==GE_PS[sub],]$GE=GE[sub]
}


EQFRAME=response_time_adjust_ps[!is.na(response_time_adjust_ps$EQ),]
EQFRAME$AOI=factor(EQFRAME$AOI)
EQGEFRAME=EQFRAME[!is.na(EQFRAME$GE),]







model_1 <- lmer(Prop ~ AOI+(1|ps),data = EQGEFRAME[EQGEFRAME$scramb==1,], REML = FALSE)


model_2a <- lmer(Prop ~ AOI*(ot1 + ot2)+(1 + ot1 + ot2 |ps),data = EQGEFRAME[EQGEFRAME$scramb==1,], REML = FALSE)



model_2b <- lmer(Prop ~ AOI*EQ*(ot1 + ot2)+(1 + ot1 + ot2 |ps),data = EQGEFRAME[EQGEFRAME$scramb==1,], REML = FALSE)



model_2c <- lmer(Prop ~ AOI*EQ*GE*(ot1 + ot2)+(1 + ot1 + ot2 |ps),data = EQGEFRAME[EQGEFRAME$scramb==1,], REML = FALSE)

model_2d <- lmer(Prop ~ AOI*EQ*GE*(ot1 + ot2 + ot3)+(1 + ot1 + ot2 + ot3 |ps),data = EQGEFRAME[EQGEFRAME$scramb==1,], REML = FALSE)



response_time_T_mod=cbind(EQGEFRAME[EQGEFRAME$scramb==1 & !is.na(EQGEFRAME$Prop),],predict(model_2c))

colnames(response_time_T_mod)=c(colnames(EQGEFRAME),"pred")

ggplot(response_time_T_mod[response_time_T_mod$scramb==1,],aes(x=TimeBin,y=Prop))+facet_wrap(~ps,nrow=8)+geom_line(aes(x=TimeBin,y=pred,colour=AOI),size=2,linetype="solid")+geom_point(aes(colour=AOI),size=0.5,shape=6)



leaveout=EQGEFRAME[EQGEFRAME$ps==1,]
leaveoutin=leaveout[leaveout$scramb==1,]

df=predict(model_2c,leaveoutin)

newframe=cbind(leaveoutin[!is.na(leaveoutin$Prop),],df)
colnames(newframe)=c(colnames(leaveoutin),"pred")

ggplot(newframe[newframe$scramb==1,],aes(x=TimeBin,y=Prop))+geom_line(aes(x=TimeBin,y=pred,colour=AOI),size=2,linetype="solid")+geom_point(aes(colour=AOI),size=0.5,shape=6)





