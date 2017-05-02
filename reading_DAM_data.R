library(rethomics)
#time_format can either be "hr"/"min"/"sec"
#time_to_avg_over in seconds
DAM1_read = function(file, time_format="min", time_to_avg_over=60*60, ref_hour = NULL){
  #header = scan("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C01.txt", what="", nmax= 1, sep="\n")
  #infile = scan("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C01.txt", what=1, skip=1, sep="\n")
  header = scan(file, what="", nmax= 1, sep="\n")
  channel = substr(strsplit(header, " ")[[1]][1], nchar(strsplit(header, " ")[[1]][1])-1, nchar(strsplit(header, " ")[[1]][1]))
  channel = as.numeric(channel)
  print(channel)
  raw_date = substr(strsplit(header, " ")[[1]][1], 1, 6)
  print(raw_date)
  # day = substr(raw_date, 1, 2)
  # month = substr(raw_date, 3, 4)
  # year = paste("20", substr(raw_date, 5, 6), sep="")
  # print(day)
  # print(month)
  # print(year)
  monitor = substr(strsplit(header, " ")[[1]][1], nchar(strsplit(header, " ")[[1]][1])-6, nchar(strsplit(header, " ")[[1]][1])-3)
  print(monitor)
  expID = paste(raw_date, monitor, sep="")
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

  activity = infile[4:length(infile)]
  t_list = vector()
  t = 0
  for (i in activity){
    #print(i)
    t_list = c(t_list, t)
    t = t + sample_freq
  }
  t_round = floor(t_list/(time_to_avg_over))*(time_to_avg_over)
  t_in_day = floor(t_round/(24*60*60))
  dt = data.table(experiment_id=expID, 
                  region_id=channel, 
                  date=raw_date, 
                  machine_name=monitor, 
                  activity=activity, 
                  t=t_list, 
                  t_round=t_round,
                  t_in_day=t_in_day)
  setkeyv(dt, c("experiment_id", "region_id", "date", "machine_name"))
  return(dt)
}
PATH="/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M"
filelist = list.files(PATH, pattern=".*\\.txt", full.names=TRUE)
filelist
x = lapply(filelist, DAM1_read, time_format="min")
DT = rbindlist(x)
setkeyv(DT, key(x[[1]]))
d = DT[region_id == 1]
d = d[machine_name == "M007"]
d = d[, .(mean_activity = mean (activity), t_in_day=t_in_day), by = t_round]
plot = ggplot(d, aes(x=t_round, y=mean_activity)) + geom_line()
plot
####PLOTS THAT Q SHOWED ME####
#overviewPlot(activity, DT, machine_name)
#overviewPlot(activity, DT[region_id==1], machine_name)
#ethogramPlot(activity, DT, machine_name, error="sem")
#ethogramPlot(activity, DT, facet_var=machine_name, error="sem")

# x = DAM1_read("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C01.txt", "min")
# x2 = DAM1_read("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C02.txt", "min")
# x3 = DAM1_read("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C03.txt", "min")
# x4 = DAM1_read("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C04.txt", "min")
# x5 = DAM1_read("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C05.txt", "min")
# dt = x[, .(mean_activity = mean (activity)), by = t_round]
# dt2 = x2[, .(mean_activity = mean (activity)), by = t_round]
# dt3 = x3[, .(mean_activity = mean (activity)), by = t_round]
# dt4 = x4[, .(mean_activity = mean (activity)), by = t_round]
# dt5 = x5[, .(mean_activity = mean (activity)), by = t_round]

#plot1 = ggplot(x, aes(x=t , y=activity)) + geom_line()
# plot1 = ggplot(dt, aes(x=t_round, y=mean_activity)) + geom_line()
# plot2 = ggplot(dt2, aes(x=t_round, y=mean_activity)) + geom_line()
# plot3 = ggplot(dt3, aes(x=t_round, y=mean_activity)) + geom_line()
# plot4 = ggplot(dt4, aes(x=t_round, y=mean_activity)) + geom_line()
# plot5 = ggplot(dt5, aes(x=t_round, y=mean_activity)) + geom_line()
# 
# plot1
# plot2
# plot3
# plot4
# plot5

#DT = rbind(x, x2, x3, x4, x5)

#data.table rolling join -> for subsampling

# t = c(1,1,1,1:50) * 60
# t
# t_round = floor(t/(5*60))*5*60
# t_round
