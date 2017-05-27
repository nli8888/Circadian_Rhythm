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
dt = dt[,.(experiment_id = experiment_id,
           condition = condition,
           machine_name = machine_name, 
           region_id = region_id, 
           date = date, 
           activity = mean(activity), 
           hour = hour, 
           day = day), 
        by = t_round]
dt = unique(dt)
x = acf(dt[,activity], ci=0.95, lag.max= 10000)#(length(dt[,activity])/3))
x = data.table(lag = c(1:length(x[[1]])),
               acf = x[[1]])
y=x[2000:8000]
#x = acf(dt[,activity], ci=0.95, lag.max= 3900)