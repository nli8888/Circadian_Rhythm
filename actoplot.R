source("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/DAM1_reader.R")
library(grid)
actoplot = function(#y,
                    data,
                    type_of_plot = "bar", #can be "bar", "line", "ribbon" or "tile"
                    num_of_dup = 2, #can be any integer
                    #mean = FALSE #change to actually use a function
                    operation = mean, #can be sum/median
                    pop_overview = NULL, #if not null, then can choose which operation like above to further summarise the population data 
                    time_to_round = hours(1) #see if can rename this to something used before
                    ){
  num_of_dup = as.numeric(num_of_dup)
  if (num_of_dup%%1!=0){
    stop("num_of_dup must be an integer")
  }
  dt = copy(as.data.table(data))
  #y_var_name <- deparse(substitute(y))
  t_round = floor(dt[,t]/(time_to_round))
  hour = t_round%%24
  day = (floor(t_round/(24)))
  dt[, t_round := t_round]
  dt[, hour := hour]
  dt[, day := day]
  setkeyv(dt, c("experiment_id", "region_id", "date", "machine_name"))
  if (length(unique(dt[,experiment_id])) == 1){
  # if (mean == FALSE){
  #   dt = dt[,.(experiment_id = experiment_id, machine_name = machine_name, region_id = region_id, date = date, activity = sum(activity), hour = hour, day = day), by = t_round]
  #   dt = unique(dt)
  #   mode = "(sum)"
  # } else if (mean == TRUE){
  #   #dt = dt[, .(mean_activity = mean(activity), hour=hour), by = t_round]
  #   dt = dt[,.(experiment_id = experiment_id, machine_name = machine_name, region_id = region_id, date = date, activity = mean(activity), hour = hour, day = day), by = t_round]
  #   dt = unique(dt)
  #   mode = "(mean)"
  # }
  dt = dt[,.(experiment_id = experiment_id, 
             machine_name = machine_name, 
             region_id = region_id, 
             date = date, 
             activity = operation(activity), 
             hour = hour, 
             day = day), 
          by = t_round]
  dt = unique(dt)
  if (num_of_dup>1){
    for (i in 2:num_of_dup){
      #print(i)
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
  x_scale = 0:(4*num_of_dup) * 6
  if (type_of_plot == "bar"){
    p = ggplot(dt, aes(x=hour, y=activity)) +
      geom_col() +
      facet_grid(day_str ~ .) + 
      scale_x_continuous(name="time (hours)",breaks = x_scale) +
      scale_y_continuous(name="activity") +
      theme(panel.spacing = unit(0.1, "lines"), plot.title = element_text(hjust = 0.5)) +
      ggtitle(sprintf("Actogram plot of individual activity over time of experiment %s", unique(dt[,experiment_id])))
  } else if (type_of_plot == "line"){
    p = ggplot(dt, aes(hour, activity)) +
      geom_line() +
      facet_grid(day_str ~ .) +
      scale_x_continuous(name="time (hours)",breaks = x_scale) +
      scale_y_continuous(name="activity") +
      theme(panel.spacing = unit(0.1, "lines"), plot.title = element_text(hjust = 0.5)) +
      ggtitle(sprintf("Actogram plot of individual activity over time of experiment %s", unique(dt[,experiment_id])))
  } else if (type_of_plot == "ribbon"){
    p = ggplot(dt, aes(hour, ymin=min(activity), ymax=activity)) +
      geom_ribbon() +
      facet_grid(day_str ~ .) +
      scale_x_continuous(name="time (hours)",breaks = x_scale) +
      scale_y_continuous(name="activity") +
      theme(panel.spacing = unit(0.1, "lines"), plot.title = element_text(hjust = 0.5)) +
      ggtitle(sprintf("Actogram plot of individual activity over time of experiment %s", unique(dt[,experiment_id])))
  } else if (type_of_plot == "tile"){
    dt[,row_name:=sprintf("%s | %02d",experiment_id,region_id)]
    # p = ggplot(dt, aes(x=hour, y=row_name, fill=activity)) +
    #   geom_tile(alpha=1) +
    #   facet_grid(day_str ~ .) +
    #   scale_x_continuous(name="time (hours)",breaks = x_scale) +
    #   #scale_y_continuous(name="activity") +
    #   theme(panel.spacing = unit(0.1, "lines"), plot.title = element_text(hjust = 0.5)) #+
    #   #ggtitle(sprintf("Actogram plot of individual activity over time of experiment %s", unique(dt[,experiment_id])))
    p = ggplot(dt,aes(x=hour,y=row_name,fill=activity)) + 
      geom_tile(alpha=1) +
      facet_grid(day_str ~ .) +
      scale_x_continuous(name="time (hours)",breaks = x_scale) +
      theme(panel.spacing = unit(0.1, "lines"), plot.title = element_text(hjust = 0.5)) +
      labs(title="Overview of individual activity pattern over time",x="time", y="activity") +
      guides(fill=guide_legend(title="activity"))
  }
  # if (type_of_plot == "double"){
  #   dt2 = copy(dt)
  #   dt2 = dt2[, day := day-1]
  #   dt2 = dt2[, hour := hour + 24]
  #   dt = dt[day<max(day)]
  #   binddt = rbind(dt, dt2)
  #   binddt = binddt[day>-1]
  #   binddt = binddt[, day_str := sprintf("day\n%03d",day)]
  #   x_scale = 0:8 * 6
  #   p = ggplot(binddt, aes(x=hour, y=activity)) +
  #     geom_col() +
  #     facet_grid(day_str ~ .) + scale_x_continuous(name="time (h)",breaks = x_scale)+
  #     scale_y_continuous(name="activity") +
  #     theme(panel.spacing = unit(0.1, "lines"), plot.title = element_text(hjust = 0.5)) +
  #     ggtitle(sprintf("Double actogram plot of individual activity over time of experiment %s", unique(dt[,experiment_id])))
  #     #ggtitle(sprintf("Double actogram plot of individual activity %s over time of experiment %s", mode, unique(dt[,experiment_id])))
  # } else if (type_of_plot == "triple"){
  #   dt2 = copy(dt)
  #   dt2 = dt2[,day := day-1]
  #   dt2 = dt2[,hour := hour + 24]
  #   dt3 = copy(dt2)
  #   dt3 = dt3[,day := day-1]
  #   dt3 = dt3[,hour := hour + 24]
  #   dt = dt[day<(max(day)-1)]
  #   dt2 = dt2[day<max(day)]
  #   binddt = rbind(dt, dt2, dt3)
  #   binddt = binddt[day>-1]
  #   binddt = binddt[, day_str := sprintf("day\n%03d",day)]
  #   x_scale = 0:12 * 6
  #   p = ggplot(binddt, aes(x=hour, y=activity)) +
  #     geom_col() +
  #     facet_grid(day_str ~ .) + scale_x_continuous(name="time (h)",breaks = x_scale)+
  #     scale_y_continuous(name="activity") +
  #     theme(panel.spacing = unit(0.1, "lines"), plot.title = element_text(hjust = 0.5)) +
  #     ggtitle(sprintf("Triple actogram plot of individual activity over time of experiment %s", unique(dt[,experiment_id])))
  #     #ggtitle(sprintf("Triple actogram plot of individual activity %s over time of experiment %s", mode, unique(dt[,experiment_id])))
  # } else if (type_of_plot == "quad"){
  #   dt2 = copy(dt)
  #   dt2 = dt2[,day := day-1]
  #   dt2 = dt2[,hour := hour + 24]
  #   dt3 = copy(dt2)
  #   dt3 = dt3[,day := day-1]
  #   dt3 = dt3[,hour := hour + 24]
  #   dt4 = copy(dt3)
  #   dt4 = dt4[,day := day-1]
  #   dt4 = dt4[,hour := hour + 24]
  #   dt = dt[day<(max(day)-2)]
  #   dt2 = dt2[day<max(day)-1]
  #   dt3 = dt3[day<max(day)]
  #   binddt = rbind(dt, dt2, dt3, dt4)
  #   binddt = binddt[day>-1]
  #   binddt = binddt[, day_str := sprintf("day\n%03d",day)]
  #   x_scale = 0:16 * 6
  #   p = ggplot(binddt, aes(x=hour, y=activity)) +
  #     geom_col() +
  #     facet_grid(day_str ~ .) +
  #     scale_x_continuous(name="time (h)",breaks = x_scale) +
  #     scale_y_continuous(name="activity") +
  #     theme(panel.spacing = unit(0.1, "lines"), plot.title = element_text(hjust = 0.5)) +
  #     ggtitle(sprintf("Quadruple actogram plot of individual activity over time of experiment %s", unique(dt[,experiment_id])))
  #     #ggtitle(sprintf("Quadruple actogram plot of individual activity %s over time of experiment %s", mode, unique(dt[,experiment_id])))
  # }
  } else if (length(unique(dt[,experiment_id])) > 1){
    summary_dt = dt[,list(activity=operation(activity), 
                          hour=hour,
                          day=day),
                     by=c("t_round", key(dt))]
    summary_dt = unique(summary_dt)
    summary_dt_all_animals = summary_dt[,list(activity=operation(activity)),
                                        by=c("t_round", 
                                             "experiment_id",
                                             "date",
                                             "machine_name",
                                             "hour",
                                             "day")]
    if (num_of_dup>1){
      for (i in 2:num_of_dup){
        #print(i)
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
      x_scale = 0:(4*num_of_dup) * 6
      if (type_of_plot == "bar"){
        p = ggplot(summary_dt_all_animals, aes(hour, activity)) +
          geom_col() +
          facet_grid(day_str ~ .) +
          scale_x_continuous(name="time (hours)",breaks = x_scale) +
          scale_y_continuous(name="activity") +
          theme(panel.spacing = unit(0.1, "lines"), plot.title = element_text(hjust = 0.5)) +
          ggtitle("Overview Actogram plot of population activity over time")
      } else if (type_of_plot == "line"){
        p = ggplot(summary_dt_all_animals, aes(hour, activity)) +
          geom_line() +
          facet_grid(day_str ~ .) +
          scale_x_continuous(name="time (hours)",breaks = x_scale) +
          scale_y_continuous(name="activity") +
          theme(panel.spacing = unit(0.1, "lines"), plot.title = element_text(hjust = 0.5)) +
          ggtitle("Overview Actogram plot of population activity over time")
      } else if (type_of_plot == "ribbon"){
        p = ggplot(summary_dt_all_animals, aes(hour, ymin=min(activity), ymax=activity)) +
          geom_ribbon() +
          facet_grid(day_str ~ .) +
          scale_x_continuous(name="time (hours)",breaks = x_scale) +
          scale_y_continuous(name="activity") +
          theme(panel.spacing = unit(0.1, "lines"), plot.title = element_text(hjust = 0.5)) +
          ggtitle("Overview Actogram plot of population activity over time")
      } else if (type_of_plot == "tile"){
        summary_dt_all_animals[,row_name:=""]
        # p = ggplot(dt, aes(x=hour, y=row_name, fill=activity)) +
        #   geom_tile(alpha=1) +
        #   facet_grid(day_str ~ .) +
        #   scale_x_continuous(name="time (hours)",breaks = x_scale) +
        #   #scale_y_continuous(name="activity") +
        #   theme(panel.spacing = unit(0.1, "lines"), plot.title = element_text(hjust = 0.5)) #+
        #   #ggtitle(sprintf("Actogram plot of individual activity over time of experiment %s", unique(dt[,experiment_id])))
        p = ggplot(summary_dt_all_animals, aes(x=hour, y=row_name, fill=activity)) +
          geom_tile(alpha=1) +
          facet_grid(day_str ~ .) +
          scale_x_continuous(name="time (hours)",breaks = x_scale) +
          theme(panel.spacing = unit(0.1, "lines"), plot.title = element_text(hjust = 0.5)) +
          labs(title="Overview of individual activity pattern over time",x="time", y="activity") +
          guides(fill=guide_legend(title="activity"))
      }
    } else if (is.null(pop_overview)){
      summary_dt_all_animals = summary_dt_all_animals[day>-1]
      summary_dt_all_animals = summary_dt_all_animals[, day_str := sprintf("day\n%03d", day)]
      x_scale = 0:(4*num_of_dup) * 6
      p = ggplot(summary_dt_all_animals, aes(hour, activity, colour=machine_name)) +
        geom_line() +
        facet_grid(day_str ~ .) +
        scale_x_continuous(name="time (hours)",breaks = x_scale) +
        scale_y_continuous(name="activity") +
        theme(panel.spacing = unit(0.1, "lines"), plot.title = element_text(hjust = 0.5)) +
        ggtitle("Actogram plot of population activity over time")
    }
    # if (type_of_plot == "double"){
    #   summary_dt_all_animals2 = copy(summary_dt_all_animals)
    #   summary_dt_all_animals2 = summary_dt_all_animals2[,day := day-1]
    #   summary_dt_all_animals2 = summary_dt_all_animals2[,hour := hour + 24]
    #   summary_dt_all_animals = summary_dt_all_animals[day<max(day)]
    #   binddt = rbind(summary_dt_all_animals, summary_dt_all_animals2)
    #   binddt = binddt[day>-1]
    #   binddt = binddt[, day_str := sprintf("day\n%03d",day)]
    #   x_scale = 0:8 * 6
    #   p = ggplot(binddt, aes(hour, activity, colour=machine_name)) +
    #     geom_line() +
    #     facet_grid(day_str ~ .) +
    #     scale_x_continuous(name="time (h)",breaks = x_scale) +
    #     scale_y_continuous(name="activity") +
    #     theme(panel.spacing = unit(0.1, "lines"), plot.title = element_text(hjust = 0.5)) +
    #     ggtitle("Double actogram plot of population activity over time")
    # } else if (type_of_plot == "triple"){
    #   summary_dt_all_animals2 = copy(summary_dt_all_animals)
    #   summary_dt_all_animals2 = summary_dt_all_animals2[,day := day-1]
    #   summary_dt_all_animals2 = summary_dt_all_animals2[,hour := hour + 24]
    #   summary_dt_all_animals3 = copy(summary_dt_all_animals2)
    #   summary_dt_all_animals3 = summary_dt_all_animals3[,day := day-1]
    #   summary_dt_all_animals3 = summary_dt_all_animals3[,hour := hour + 24]
    #   summary_dt_all_animals = summary_dt_all_animals[day<(max(day)-1)]
    #   summary_dt_all_animals2 = summary_dt_all_animals2[day<max(day)]
    #   binddt = rbind(summary_dt_all_animals, summary_dt_all_animals2, summary_dt_all_animals3)
    #   binddt = binddt[day>-1]
    #   binddt = binddt[, day_str := sprintf("day\n%03d",day)]
    #   x_scale = 0:12 * 6
    #   p = ggplot(binddt, aes(hour, activity, colour=machine_name)) +
    #     geom_line() +
    #     facet_grid(day_str ~ .) +
    #     scale_x_continuous(name="time (h)",breaks = x_scale) +
    #     scale_y_continuous(name="activity") +
    #     theme(panel.spacing = unit(0.1, "lines"), plot.title = element_text(hjust = 0.5)) +
    #     ggtitle("Triple actogram plot of population activity over time of experiment")
    # } else if (type_of_plot == "quad"){
    #   summary_dt_all_animals2 = copy(summary_dt_all_animals)
    #   summary_dt_all_animals2 = summary_dt_all_animals2[,day := day-1]
    #   summary_dt_all_animals2 = summary_dt_all_animals2[,hour := hour + 24]
    #   summary_dt_all_animals3 = copy(summary_dt_all_animals2)
    #   summary_dt_all_animals3 = summary_dt_all_animals3[,day := day-1]
    #   summary_dt_all_animals3 = summary_dt_all_animals3[,hour := hour + 24]
    #   summary_dt_all_animals4 = copy(summary_dt_all_animals3)
    #   summary_dt_all_animals4 = summary_dt_all_animals4[,day := day-1]
    #   summary_dt_all_animals4 = summary_dt_all_animals4[,hour := hour + 24]
    #   summary_dt_all_animals = summary_dt_all_animals[day<(max(day)-2)]
    #   summary_dt_all_animals2 = summary_dt_all_animals2[day<max(day)-1]
    #   summary_dt_all_animals3 = summary_dt_all_animals3[day<max(day)]
    #   binddt = rbind(summary_dt_all_animals, 
    #                  summary_dt_all_animals2, 
    #                  summary_dt_all_animals3, 
    #                  summary_dt_all_animals4)
    #   binddt = binddt[day>-1]
    #   binddt = binddt[, day_str := sprintf("day\n%03d",day)]
    #   x_scale = 0:16 * 6
    #   p = ggplot(binddt, aes(hour, activity, colour=machine_name)) +
    #     geom_line() +
    #     facet_grid(day_str ~ .) +
    #     scale_x_continuous(name="time (h)",breaks = x_scale) +
    #     scale_y_continuous(name="activity") +
    #     theme(panel.spacing = unit(0.1, "lines"), plot.title = element_text(hjust = 0.5)) +
    #     ggtitle("Quadruple actogram plot of population activity over time of experiment")
    # }
  }
  # # p = ggplot(binddt,aes(hour,ymax=sum_activity, ymin=min(um_activity))) +
  # #   geom_ribbon() +
  # #   facet_grid(day_str ~ .) + scale_x_continuous(name="time (h)",breaks = x_scale)+
  # #   scale_y_continuous(name="y")
  # # p = ggplot(binddt, aes(x=hour, y=sum_activity)) + 
  # #     geom_col() +
  # #     facet_grid(day_str ~ .) + scale_x_continuous(name="time (h)",breaks = x_scale)+
  # #     scale_y_continuous(name="activity") +
  # #     theme(panel.spacing = unit(0, "lines"))
  p
  #return(summary_dt_all_animals)
  #return(dt)
}

dam1 = DAM1_single_reader("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C01.txt")
PATH="/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M"
#dammulti = DAM1_multi_reader(PATH, time_format = "min", time_to_round_to = 60*60)
acto = actoplot(dammulti, num_of_dup = 4, type_of_plot = "tile", operation = mean, pop_overview = mean)
acto

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