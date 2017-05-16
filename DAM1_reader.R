library(rethomics)
#time_format can either be "hr"/"min"/"sec"
#time_to_round_to in seconds
DAM1_single_reader = function(file, 
                   #file_format = "DAM1",
                   time_format = "min", 
                   time_to_round_to = 60*60, #aka hour in seconds
                   #num_of_dup = "double", #can be "double", "triple" or "quadruple"
                   ref_hour = NULL){ #time_zone/ref_hour not supported yet, not for future work; can force check 4th line to make sure its 0000; if not stop
  #if (file_format == "DAM1"){
  #header = scan("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C01.txt", what="", nmax= 1, sep="\n")
  #infile = scan("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C01.txt", what=1, skip=1, sep="\n")
  header = scan(file, what="", nmax= 1, sep="\n")
  channel = substr(strsplit(header, " ")[[1]][1], nchar(strsplit(header, " ")[[1]][1])-1, nchar(strsplit(header, " ")[[1]][1]))
  channel = as.numeric(channel)
  print(channel)
  condition = substr(strsplit(header, " ")[[1]][1], 7, 11)
  print(condition)
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
  t_round = floor(t_list/(time_to_round_to))#*(time_to_round_to)
  hour = t_round%%24
  day = (floor(t_round/(24)))
  dt = data.table(experiment_id=expID,
                  condition=condition,
                  machine_name=monitor,
                  region_id=channel, 
                  date=raw_date, 
                  activity=activity, 
                  t=t_list 
                  #t_round=t_round,
                  #hour=hour,
                  #day=day
                  )
  setkeyv(dt, c("experiment_id", "region_id", "date", "machine_name"))
  # actod = copy(dt)
  # if (num_of_dup == "double"){
  #   actod2 = copy(actod)
  #   actod2 = actod2[,day := day-1]
  #   actod2 = actod2[,hour := hour + 24]
  #   actod = actod[day<max(day)]
  #   actodd = rbind(actod, actod2)
  #   actodd = actodd[day>-1]
  #   actodd = actodd[, day_str := sprintf("day\n%03d",day)]
  #   p = ggplot(actodd,aes(hour,ymax=y, ymin=min(y))) +
  #     geom_ribbon() + 
  #     facet_grid(day_str ~ .) + scale_x_continuous(name="time (h)",breaks = 0:8 * 6)+
  #     scale_y_continuous(name="y")
  #   p
  # }
  
  #return(c(dt, actod, actod2))
  return(dt)
  # } else if (file_format == "DAM2"){
  #   
  # }
}

DAM1_multi_reader = function(PATH,
                             ...){ #use rethomics:::checkDirExists to also check if dir exist 
  filelist = list.files(PATH, pattern=".*\\.txt", full.names=TRUE)
  x = lapply(filelist, DAM1_single_reader, ...)
  DT = rbindlist(x)
  setkeyv(DT, key(x[[1]]))
  return(DT)
}

###MULTIFILE###
#PATH="/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115C5M"
#DT = DAM1_multi_reader(PATH, time_format = "min", time_to_round_to = 60*60)

## filelist = list.files(PATH, pattern=".*\\.txt", full.names=TRUE)
## filelist
## x = lapply(filelist, DAM1_reader, time_format="min")
## DT = rbindlist(x)
## setkeyv(DT, key(x[[1]]))
###SINGLEFILE###
# DT = DAM1_single_reader("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C01.txt")
# actod = copy(DT)
# actod = actod[,.(sum_activity = sum(activity), hour = hour, day = day), by = t_round]
# actod = unique(actod)
# actod2 = copy(actod)
# actod2 = actod2[,day := day-1]
# actod2 = actod2[,hour := hour + 24]
# actod = actod[day<max(day)]
# actodd = rbind(actod, actod2)
# actodd = actodd[day>-1]
# actodd = actodd[, day_str := sprintf("day\n%03d",day)]
# 
# 
# p = ggplot(actodd, aes(hour,ymax=sum_activity, ymin=min(sum_activity))) +
#   geom_ribbon() +
#   facet_grid(day_str ~ .) + scale_x_continuous(name="time (h)",breaks = 0:8 * 6)+
#   scale_y_continuous(name="activity")
# p = ggplot(actodd, aes(x=hour, y=sum_activity)) + 
#   geom_col() +
#   facet_grid(day_str ~ .) + scale_x_continuous(name="time (h)",breaks = 0:8 * 6)+
#   scale_y_continuous(name="activity")
# p
# 
# d = DT[region_id == 1 & machine_name == "M007"]
# e = copy(d)
# e = e[, day := day-1]
# 
# d = d[, .(mean_activity = mean (activity), hour=hour), by = t_round]

#dplot = ggplot(d, aes(x=t_round, y=mean_activity)) + geom_line()
#dplot

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