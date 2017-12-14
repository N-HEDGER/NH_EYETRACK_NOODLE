AQ_DATA=read.csv("AQ_Summary.csv")
AQ_PS=as.numeric(str_extract(AQ_DATA$pps, "[0-9]+"))
AQ=AQ_DATA$AQ



num_sub = length(unique((AQ_PS)))
threshold_t = qt(p = 1 - .05/2, df = num_sub-1)

response_time_adjust_ps=response_time_new
response_time_adjust_ps$ps=as.numeric(response_time_adjust_ps$ps)
response_time_adjust_ps$ps=factor(ifelse(response_time_adjust_ps$ps>21,response_time_adjust_ps$ps+1,response_time_adjust_ps$ps))


response_time_adjust_ps$AQ=rep(NA,nrow(response_time_adjust_ps))
for (sub in 1:length(AQ_PS)){
  response_time_adjust_ps[response_time_adjust_ps$ps==AQ_PS[sub],]$AQ=AQ[sub]
}



df_timeclust_between <- make_time_cluster_data(response_time_adjust_ps[response_time_adjust_ps$scramb==1,], test= "lm",predictor_column = "AQ", threshold = threshold_t)

plot(df_timeclust_between) +  ylab("T-Statistic") + theme_light()