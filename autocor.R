source("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/DAM1_reader.R")

#dam1 = DAM1_single_reader("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C03.txt")
#dam1 = DAM1_single_reader("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/Estaban_new_data/Circadian_data_for_Nicholas/220914es5/220914es5CtM011C27.txt")
dam1 = DAM1_single_reader("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/My_webapp/www/DAM1_data/220714esM037C08.txt")

PATH1 = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/My_webapp/www/DAM1_data"
dammulti1 = DAM1_multi_reader(PATH1, time_format = "min")

# PATH1 = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M"
# PATH2 = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115C5M"

#dammulti1 = DAM1_multi_reader(PATH1, time_format = "min")
#dammulti2 = DAM1_multi_reader(PATH2, time_format = "min")
#dt = copy(as.data.table(dammulti2))

#autocor
dt = copy(as.data.table(dam1))
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
##for LD
# x = acf(summary_dt_all_animals[1:72, activity], main = "", xlim = c(12,36), xlab = "Lag / period (hours)", ylab = "Correlation Coefficient", ci = 0.95, plot=1, lag.max = 72)
##for DD
x = acf(summary_dt_all_animals[73:length(summary_dt_all_animals[,t_round]),activity], ci=0.95, plot = 1, xlim = c(12,36), main = "", xlab = "Lag / period (hours)", ylab = "Correlation Coefficient", lag.max=(length(summary_dt_all_animals[,activity])))
y = data.table(lag = c(1:(length(x[[1]])-1)),
               #period = seq(0, length(summary_dt_all_animals[,activity])),
               acf = x[[1]][2:length(x[[1]])])

subset_y = copy(as.data.table(y))
subset_y = subset_y[12:36]
setnames(subset_y, "lag", "period")
#upper=0+(1.96/sqrt(length(x[[1]])))
upper=qnorm((1 + 0.95)/2)/sqrt(length(x[[1]])) #the way acf() does it
#lower=0-(1.96/sqrt(length(x[[1]])))
lower=upper*-1  #I assume
p = ggplot(subset_y, aes(period, acf, width=.75)) +
  geom_col() +
  scale_x_continuous(name="period (hours)", breaks = seq(0, 40, 2)) +
  scale_y_continuous(name="Correlation Coefficient") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # ggtitle("Correlogram of acf over period") +
  geom_hline(yintercept = upper, linetype = 2) + geom_hline(yintercept = lower, linetype = 2)
p
which.max(subset_y[,acf]) + 9


##FOURIER ANALYSIS## NEED TO SPLIT LD AND DD DATA APART AND DO THE CALCULATIONS SEPARATELY
ff = abs(fft(x[[1]])/sqrt(length(x[[1]])))^2
p = (4/length(x[[1]]))*ff[1:((length(x[[1]])/2)+1)]
f = (0:(length(x[[1]])/2))/length(x[[1]])
level = length(x[[1]])*((1-(max(p)/sum(p[0:(length(p)/2)])))^(length(x[[1]])-1))
#level = -2*log(1-(0.05^(1/(length(x[[1]])))))
plot(f,p,type="l")
plot(f[10:25],p[10:25],type="l")
which.max(p[10:25])
f[10:25][which.max(p[10:25])]
1/f[10:25][which.max(p[10:25])]

##LOMB-SCARGLE PERIODOGRAM##
library(lomb)
lomb_periodogram = lsp(as.vector(x[[1]]),alpha=0.05,from=0,to=0.5)

# ts_object = ts(x[[1]], frequency = 1, start = 0)
ts_object = ts(summary_dt_all_animals[73:length(summary_dt_all_animals[,t_round]),activity], frequency = 1, start = 0)
library(bspec)
welch = welchPSD(x=ts_object, seglength=100, method="mean", windowfun = hammingwindow)
plot(welch$frequency, welch$power, log="y", type="l")
