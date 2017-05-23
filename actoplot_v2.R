source("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/DAM1_reader.R")

actoplot_dam1 = function(file1,
                    type_of_plot = "bar", #can be "bar", "line", "ribbon" or "tile"
                    num_of_plot = 2, #can be any integer
                    #mean = FALSE #change to actually use a function
                    operation = mean, #can be sum/median
                    condition = NULL,
                    pop_overview = NULL, #if not null, then can choose which operation like above to further summarise the population data 
                    time_to_round = hours(1) #see if can rename this to something used before
                    ){
  num_of_plot = as.numeric(num_of_plot)
  if (num_of_plot%%1!=0){
    stop("num_of_plot must be an integer")
  }
  dt = copy(as.data.table(file1))
  t_round = floor(dt[,t]/(time_to_round))
  hour = t_round%%24
  day = (floor(t_round/(24)))
  dt[, t_round := t_round]
  dt[, hour := hour]
  dt[, day := day]
  setkeyv(dt, c("experiment_id", "region_id", "date", "machine_name"))
  if (length(unique(dt[,experiment_id])) == 1){
  dt = dt[,.(experiment_id = experiment_id,
             condition = condition,
             machine_name = machine_name, 
             region_id = region_id, 
             date = date, 
             activity = operation(activity), 
             hour = hour, 
             day = day), 
          by = t_round]
  dt = unique(dt)
  if (num_of_plot>1){
    for (i in 2:num_of_plot){
      dt_temp = copy(dt)
      dt_temp = dt_temp[, day := day-1]
      dt_temp = dt_temp[, hour := hour + 24]
      dt = dt[day<(max(day))]
      dt = rbind(dt, dt_temp)
      dt = unique(dt)
    }
  }
  dt = dt[day>-1]
  dt = dt[, day_str := sprintf("day\n%03d", day)]
  x_scale = 0:(4*num_of_plot) * 6
  if (type_of_plot == "bar"){
    p = ggplot(dt, aes(x=hour, y=activity)) +
      geom_col() +
      facet_grid(day_str ~ .) + 
      scale_x_continuous(name="time (hours)",breaks = x_scale) +
      scale_y_continuous(name="activity") +
      theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
      ggtitle(sprintf("Actogram plot of individual activity over time of experiment %s", unique(dt[,experiment_id])))
  } else if (type_of_plot == "line"){
    p = ggplot(dt, aes(hour, activity)) +
      geom_line() +
      facet_grid(day_str ~ .) +
      scale_x_continuous(name="time (hours)",breaks = x_scale) +
      scale_y_continuous(name="activity") +
      theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
      ggtitle(sprintf("Actogram plot of individual activity over time of experiment %s", unique(dt[,experiment_id])))
  } else if (type_of_plot == "ribbon"){
    p = ggplot(dt, aes(hour, ymin=min(activity), ymax=activity)) +
      geom_ribbon() +
      facet_grid(day_str ~ .) +
      scale_x_continuous(name="time (hours)",breaks = x_scale) +
      scale_y_continuous(name="activity") +
      theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
      ggtitle(sprintf("Actogram plot of individual activity over time of experiment %s", unique(dt[,experiment_id])))
  } else if (type_of_plot == "tile"){
    dt[,row_name:=sprintf("%s | %02d",experiment_id,region_id)]
    p = ggplot(dt,aes(x=hour,y=row_name,fill=activity)) + 
      geom_tile(alpha=1) +
      facet_grid(day_str ~ .) +
      scale_x_continuous(name="time (hours)",breaks = x_scale) +
      theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
      labs(title="Overview of individual activity pattern over time",x="time", y="activity") +
      guides(fill=guide_legend(title="activity"))
  }
  } else if (length(unique(dt[,experiment_id])) > 1){
    summary_dt = dt[,list(activity=operation(activity), 
                          hour=hour,
                          day=day),
                     by=c("t_round", key(dt), "condition")]
    summary_dt = unique(summary_dt)
    setkeyv(summary_dt, c("experiment_id", "date", "machine_name"))
    summary_dt_all_animals = summary_dt[,list(activity=operation(activity)),
                                        by=c("t_round", #see if can utilize key(dt)
                                             key(summary_dt),
                                             "condition",
                                             "hour",
                                             "day")]
    if (num_of_plot>1){
      for (i in 2:num_of_plot){
        dt_temp = copy(summary_dt_all_animals)
        dt_temp = dt_temp[, day := day-1]
        dt_temp = dt_temp[, hour := hour + 24]
        summary_dt_all_animals = summary_dt_all_animals[day<(max(day))]
        summary_dt_all_animals = rbind(summary_dt_all_animals, dt_temp)
        summary_dt_all_animals = unique(summary_dt_all_animals)
      }
    }
    if (!is.null(pop_overview)){
      summary_dt_all_animals = summary_dt_all_animals[,list(activity=pop_overview(activity)),
                                                      by=c("t_round",
                                                           "hour",
                                                           "day")]
      summary_dt_all_animals = summary_dt_all_animals[day>-1]
      summary_dt_all_animals = summary_dt_all_animals[, day_str := sprintf("day\n%03d", day)]
      x_scale = 0:(4*num_of_plot) * 6
      if (type_of_plot == "bar"){
        p = ggplot(summary_dt_all_animals, aes(hour, activity)) +
          geom_col() +
          facet_grid(day_str ~ .) +
          scale_x_continuous(name="time (hours)",breaks = x_scale) +
          scale_y_continuous(name="activity") +
          theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
          ggtitle("Overview Actogram plot of population activity over time")
      } else if (type_of_plot == "line"){
        p = ggplot(summary_dt_all_animals, aes(hour, activity)) +
          geom_line() +
          facet_grid(day_str ~ .) +
          scale_x_continuous(name="time (hours)",breaks = x_scale) +
          scale_y_continuous(name="activity") +
          theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
          ggtitle("Overview Actogram plot of population activity over time")
      } else if (type_of_plot == "ribbon"){
        p = ggplot(summary_dt_all_animals, aes(hour, ymin=min(activity), ymax=activity)) +
          geom_ribbon() +
          facet_grid(day_str ~ .) +
          scale_x_continuous(name="time (hours)",breaks = x_scale) +
          scale_y_continuous(name="activity") +
          theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
          ggtitle("Overview Actogram plot of population activity over time")
      } else if (type_of_plot == "tile"){
        summary_dt_all_animals[,row_name:=""]
        p = ggplot(summary_dt_all_animals, aes(x=hour, y=row_name, fill=activity)) +
          geom_tile(alpha=1) +
          facet_grid(day_str ~ ., switch = "y") +
          scale_x_continuous(name="time (hours)",breaks = x_scale) +
          theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5), axis.ticks.y = element_blank()) +
          labs(title="Overview Actogram plot of population activity over time",x="time", y="") #+
          #guides(fill=guide_legend(title="activity"))
      }
    } else if (is.null(pop_overview)){
      summary_dt_all_animals = summary_dt_all_animals[day>-1]
      summary_dt_all_animals = summary_dt_all_animals[, day_str := sprintf("day\n%03d", day)]
      x_scale = 0:(4*num_of_plot) * 6
      p = ggplot(summary_dt_all_animals, aes(hour, activity, colour=machine_name)) +
        geom_line() +
        facet_grid(day_str ~ .) +
        scale_x_continuous(name="time (hours)",breaks = x_scale) +
        scale_y_continuous(name="activity") +
        theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
        ggtitle("Actogram plot of population activity over time")
      # if (is.null(condition)){
      #   p = ggplot(summary_dt_all_animals, aes(hour, activity, colour=machine_name)) +
      #     geom_line() +
      #     facet_grid(day_str ~ .) +
      #     scale_x_continuous(name="time (hours)",breaks = x_scale) +
      #     scale_y_continuous(name="activity") +
      #     theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
      #     ggtitle("Actogram plot of population activity over time")
      # } else if (!is.null(condition)){
      #   p = ggplot(summary_dt_all_animals, aes(hour, activity, colour=condition)) +
      #     geom_line() +
      #     facet_grid(day_str ~ .) +
      #     scale_x_continuous(name="time (hours)",breaks = x_scale) +
      #     scale_y_continuous(name="activity") +
      #     theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
      #     ggtitle("Actogram plot of population activity over time")
      # }
    }
  }
  p
  #return(summary_dt_all_animals)
  #return(dt)
}
##DAM1##
dam1 = DAM1_single_reader("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C01.txt")
PATH1 = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M"
PATH2 = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115C5M"
PATH3 = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/190115Aes"
PATH4 = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/190115Bes"
PATH5 = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/190115Ces"
#dammulti1 = DAM1_multi_reader(PATH1, time_format = "min")
#dammulti2 = DAM1_multi_reader(PATH2, time_format = "min")
#dammulti3 = DAM1_multi_reader(PATH3, time_format = "min")
#dammulti4 = DAM1_multi_reader(PATH4, time_format = "min")
#dammulti5 = DAM1_multi_reader(PATH5, time_format = "min")
#dammulti = rbind(dammulti1, dammulti2)
#acto = actoplot_dam1(dammulti1, num_of_plot = 4, type_of_plot = "bar", operation = mean, pop_overview = mean)
#acto

##DAM2##
file1 = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/Anne_DAM2_Data/2015-08-05_M002_merged.txt"
#file1 = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/Anne_DAM2_Data/2015-08-05_M010_merged.txt"  file = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/Anne_DAM2_Data/2016-11-20_M012_merged.txt"
#file1 = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/Anne_DAM2_Data/2016-11-20_M012_merged.txt"
#dam2 = rethomics:::loadSingleDAM2File(file1)
library(chron)
library(lubridate)
actoplot_dam2 = function(file1,
                         machine_name = NULL,
                         type_of_plot = "bar", #can be "bar", "line", "ribbon" or "tile"
                         num_of_plot = 2, #can be any integer
                         #mean = FALSE #change to actually use a function
                         operation = mean, #can be sum/median
                         condition = NULL,
                         pop_overview = NULL, #if not null, then can choose which operation like above to further summarise the population data 
                         time_to_round = hours(1) #see if can rename this to something used before
){
  num_of_plot = as.numeric(num_of_plot)
  if (num_of_plot%%1!=0){
    stop("num_of_plot must be an integer")
  }
  if (is.null(machine_name)){
    stop('"machine_name" must be specified')
  }
  dt = copy(as.data.table(file1))
  dt[,machine_name := machine_name]
  dt[, date := as.character(t)]
  x = tstrsplit(dt[,date], " ")
  dt[, date := x[1]]
  #dt[,experiment_id := paste(x[1], machine_name, sep = "")]
  dt[, t := x[2]]
  x = lapply(dt[,t], strptime, format = "%H:%M:%S")
  seconds = sapply(x, second)
  minutes = sapply(x, minute)
  hours = sapply(x, hour)
  seconds = seconds + (minutes*60) + (hours*60*60)
  dt[, t := seconds]
  return(dt)
}

acto_dam2 = actoplot_dam2(dam2, machine_name = "M012", num_of_plot = 4, type_of_plot = "bar", operation = mean, pop_overview = mean)


















##############TESTING BELOW###################
myoverviewPlot <- function(y,data,
                         condition=NULL,
                         summary_time_window=mins(30),
                         normalise_var_per_id=FALSE,
                         time_wrap=NULL,
                         time_unit_conversion=days){

  dt = copy(as.data.table(data))
  #print(y)
  y_var_name <- deparse(substitute(y))
  print(y_var_name)
  setnames(dt,y_var_name,"y_var")
  print(dt)
  dt[,t_r := floor(t/summary_time_window) * summary_time_window]
  print(dt)
  if(!is.null(time_wrap))
     dt[,t_r := t_r %% time_wrap]
  dt[,y_var:=as.numeric(y_var)]
  print(dt)
  c_var_name <- deparse(substitute(condition))
  print(c_var_name)
  if(c_var_name == "NULL")
     dt[,c_var:=TRUE]
   else
     setnames(dt, c_var_name,"c_var")

  if(normalise_var_per_id)
    dt <- na.omit(dt[,y_var:=as.vector(scale(y_var)),by=key(dt)])

  summary_dt <- dt[,list(y_var=mean(y_var)),
                   by=c("t_r","c_var",key(dt))]
  print(summary_dt)
  summary_dt[,t_d:=t_r/time_unit_conversion(1)]

  if(c_var_name != "NULL"){
    summary_dt[,row_name:=sprintf("%s | %s | %02d",c_var,experiment_id,region_id)]
    y_lab <- sprintf("Individual (%s | experiment_id | region_id)", c_var_name)
  }
  else{
    summary_dt[,row_name:=sprintf("%s | %02d",experiment_id,region_id)]
    y_lab <- "Individual (experiment_id | region_id)"
  }
  print(summary_dt)
  p <- ggplot(summary_dt,aes(x=t_d,y=row_name,fill=y_var)) + geom_tile(alpha=1) +
    labs(title= sprintf("Overview of individual '%s' pattern over time",y_var_name),x="time", y=y_lab)+
    guides(fill=guide_legend(title=y_var_name))
  #p
  return(summary_dt)
}

#data(sleep_sexual_dimorphism)
#DT = sleep_sexual_dimorphism
#over = myoverviewPlot(activity, dam1)

myethogramPlot <- function(y,data,
                         condition=NULL,
                         facet_var=NULL,
                         summary_time_window=mins(30),
                         normalise_var_per_id=FALSE,
                         error_bar=NULL,
                         time_wrap=NULL,
                         time_unit_conversion=days){
  
  dt = copy(as.data.table(data))  
  y_var_name <- deparse(substitute(y))
  setnames(dt,y_var_name,"y_var")
  dt[,t_r := floor(t/summary_time_window) * summary_time_window]
  
  if(!is.null(time_wrap))
    dt[,t_r := t_r %% time_wrap]
  
  dt[,y_var:=as.numeric(y_var)]
  c_var_name <- deparse(substitute(condition))
  f_var_name <- deparse(substitute(facet_var))
  print(c_var_name)
  if(c_var_name == "NULL")
    dt[,c_var:=TRUE]
  else
    setnames(dt, c_var_name,"c_var")
  
  if(f_var_name == "NULL")
    dt[,f_var:=TRUE]
  else
    setnames(dt, f_var_name,"f_var")
  
  print(typeof(dt[,c_var]))
  # # if(is.numeric(dt[,c_var])){
  # #   dt[,c_var = as.character(c_var)]
  # #   warning("Condition variable is a number.
  # #           Converting it to a factor")
  # # }
  # # 
  # # if(is.numeric(dt[,f_var])){
  # #   dt[,c_var = as.character(f_var)]
  # #   warning("Faceting variable is a number.
  # #           Converting it to a factor")
  # # }
  # 
  # if(normalise_var_per_id)
  #   dt <- na.omit(dt[,y_var:=as.vector(scale(y_var)),by=key(dt)])
  # 
  summary_dt <- dt[,list(y_var=mean(y_var)),
                   by=c("t_r","c_var","f_var",key(dt))]
  
  # 
  # 
  summary_dt[,t_d:=t_r/time_unit_conversion(1)]
  # 
  # # if(!is.null(error_bar)){
  # #   if(!error_bar %in% c("sd", "sem", "boot_ci", "gauss_ci"))
  # #     stop("error_bar should can be only one of NULL,'sd, 'sem', 'gauss_ci' or 'boot_ci' ")
  # #
  # #   if(error_bar == "sd")
  # #     errBarFun <- plusMinusSd
  # #
  # #   if(error_bar == "sem")
  # #     errBarFun <-plusMinusSem
  # #
  # #   if(error_bar == "gauss_ci")
  # #     errBarFun <-gaussianCi
  # #
  # #   if(error_bar == "boot_ci")
  # #     errBarFun <- bootCi
  # #
  # #
  # #   summary_dt_all_animals <- summary_dt[,c(
  # #     y_var=mean(y_var),
  # #     errBarFun(y_var)),
  # #     by=.(t_r,c_var,f_var)]
  # #
  # # }
  if(is.null(error_bar))
    summary_dt_all_animals <- summary_dt[,list(y_var=mean(y_var)),by=.(t_r,c_var,f_var)]
  # 
  summary_dt_all_animals[,t_d:=t_r/time_unit_conversion(1)]
  # 
  if(c_var_name != "NULL"){
    p <- ggplot(summary_dt_all_animals, aes(t_d,y_var,colour=c_var,fill=c_var)) + geom_line()
  }
  else{
    p <- ggplot(summary_dt_all_animals, aes(t_d,y_var)) + geom_line()
  }
  # 
  # 
  # if(!is.null(error_bar)){
  #   if(c_var_name != "NULL"){
  #     p <- p + geom_ribbon(aes(ymin=lower, ymax=higher,colour=NULL),alpha=.3)
  #   }
  #   else{
  #     p <- p + geom_ribbon(aes(ymin=lower, ymax=higher),alpha=.3)
  #   }
  # }
  # 
  # p <- p + labs(title= sprintf("Average '%s' over time",y_var_name),x="time", y=y_var_name)
  # p <- p + guides(fill=guide_legend(title=c_var_name),
  #                 colour=guide_legend(title=c_var_name))
  # 
  # if(f_var_name != "NULL"){
  #   p <- p + facet_grid(f_var ~ .)
  # }
  p
  #return(summary_dt)
  }


# plusMinusSd <- function(x){
#   m <- mean(x)
#   s <- sd(x)
#   list(lower = m - s, higher = m +s)
# }
# plusMinusSem <- function(x){
#   m <- mean(x)
#   s <- sd(x)/sqrt(length(x)) 
#   list(lower = m - s, higher = m +s)
# }
# 
# gaussianCi <- function(x){
#   m <- mean(x)
#   s <- 1.96 * sd(x)/sqrt(length(x)) 
#   list(lower = m - s, higher = m +s)
# }
# 
# 
# bootCi <- function(x,
#                    r=5000,
#                    ci=0.95){
#   v <- replicate(r, mean(sample(x,replace=T)))
#   ci <- quantile(v,c(1-ci,ci))
#   out <-list(lower = ci[1],
#              higher = ci[2])
#   out
# }

#myethogramPlot(activity, dammulti, time_wrap = NULL, summary_time_window = mins(30), condition = machine_name)