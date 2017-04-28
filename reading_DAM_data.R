library(rethomics)
#time_format can either be "hr"/"min"/"sec"
DAM1_read = function(file, time_format="min", ref_hour = NULL){
  #header = scan("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C01.txt", what="", nmax= 1, sep="\n")
  #infile = scan("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C01.txt", what=1, skip=1, sep="\n")
  header = scan(file, what="", nmax= 1, sep="\n")
  infile = scan(file, what=1, skip=1, sep="\n")
  sample_freq = infile[2]
  if (time_format == "hr") {
    sample_freq = sample_freq*60*60
  } else if (time_format == "min"){
    sample_freq = sample_freq*60
  } else if (time_format == "sec"){
  } else {
    warning('arguement for time_format not acceptable; time_format can either be "hr"/"min"/"sec"')
    break
  }  


  #sample_freq
  #typeof(infile)
  activity = infile[4:length(infile)]
  #activity
  t_list = vector()
  t = 0
  for (i in activity){
    #print(i)
    t_list = c(t_list, t)
    t = t + sample_freq
  }
  #t_list = c(t_list, sample_freq)
  t_round = floor(t_list/(30*60))*(30*60)
  
  dt = data.table(activity=activity, t=t_list, t_round=t_round)
  return(dt)
}

x = DAM1_read("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C01.txt", "min")
dt = x[, .(mean_activity = mean (activity)), by = t_round]

#n = 7
#dt2 = x[, .(mean_activity = mean(activity)), by = list(every_30_mins = (seq(nrow(x)) - 1) %/% n)]
#every_30 = seq(0,1195500,by = 1800)
#time_dt = data.table(every_30)
plot1 = ggplot(x, aes(x=t , y=activity)) + geom_line()
plot2 = ggplot(dt, aes(x=t_round, y=mean_activity)) + geom_line()
#plot3 = ggplot(dt2, aes(x=every_30_mins, y=mean_activity)) + geom_line()
plot1
plot2
#plot3
#data.table rolling join -> for subsampling
#rbindlist()


# t = c(1,1,1,1:50) * 60
# t
# t_round = floor(t/(5*60))*5*60
# t_round
