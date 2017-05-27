source("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/DAM1_reader.R")

#dam1 = DAM1_single_reader("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C01.txt")
#dt = copy(as.data.table(dam1))

PATH1 = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M"
dammulti1 = DAM1_multi_reader(PATH1, time_format = "min")
dt = copy(as.data.table(dammulti1))

t_round = floor(dt[,t]/(60*60))
hour = t_round%%24
day = (floor(t_round/(24)))
dt[, t_round := t_round]
dt[, hour := hour]
dt[, day := day]
setkeyv(dt, c("experiment_id", "region_id", "date", "machine_name"))
summary_dt = dt[,.(experiment_id = experiment_id,
           condition = condition,
           machine_name = machine_name, 
           region_id = region_id, 
           date = date, 
           activity = mean(activity), 
           hour = hour, 
           day = day), 
        by = t_round]
summary_dt = unique(summary_dt)
setkeyv(summary_dt, c("experiment_id", "date", "machine_name"))
summary_dt_all_animals = summary_dt[,list(activity=mean(activity)),
                                    by=c("t_round", #see if can utilize key(dt)
                                         key(summary_dt),
                                         "condition",
                                         "hour",
                                         "day")]
summary_dt_all_animals = summary_dt_all_animals[,list(activity=mean(activity)),
                                                by=c("t_round",
                                                     "hour",
                                                     "day")]
#x = acf(dt[,activity], ci=0.95, lag.max= 3900)
x = acf(summary_dt_all_animals[,activity], ci=0.95, plot = 1, lag.max=(length(summary_dt_all_animals[,activity])))
y = data.table(lag = c(1:(length(x[[1]])-1)),
               #period = seq(0, length(summary_dt_all_animals[,activity])),
               acf = x[[1]][2:length(x[[1]])])

period = which.max(y[10:50,acf]) + 9