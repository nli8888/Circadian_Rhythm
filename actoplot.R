source("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/DAM1_reader.R")
actoplot = function(y,
                    data,
                    num_of_dup = "double", #can be "double", "triple" or "quad"
                    time_to_round = 60*60,
                    mean = FALSE
                    ){
  dt = copy(as.data.table(data))
  y_var_name <- deparse(substitute(y))
  t_round = floor(dt[,t]/(time_to_round))
  hour = t_round%%24
  day = (floor(t_round/(24)))
  dt[, t_round := t_round]
  dt[, hour := hour]
  dt[, day := day]
  dt = dt[,.(experiment_id = experiment_id, machine_name = machine_name, region_id = region_id, date = date, sum_activity = sum(activity), hour = hour, day = day), by = t_round]
  dt = unique(dt)
  setkeyv(dt, c("experiment_id", "region_id", "date", "machine_name"))
  if (mean == FALSE){
    #dt = dt[, .(mean_activity = mean(activity), hour=hour), by = t_round]
  if (num_of_dup == "double"){
    dt2 = copy(dt)
    dt2 = dt2[,day := day-1]
    dt2 = dt2[,hour := hour + 24]
    dt = dt[day<max(day)]
    binddt = rbind(dt, dt2)
    binddt = binddt[day>-1]
    binddt = binddt[, day_str := sprintf("day\n%03d",day)]
    x_scale = 0:8 * 6
  } else if (num_of_dup == "triple"){
    dt2 = copy(dt)
    dt2 = dt2[,day := day-1]
    dt2 = dt2[,hour := hour + 24]
    dt3 = copy(dt2)
    dt3 = dt3[,day := day-1]
    dt3 = dt3[,hour := hour + 24]
    dt = dt[day<(max(day)-1)]
    binddt = rbind(dt, dt2, dt3)
    binddt = binddt[day>-1]
    binddt = binddt[, day_str := sprintf("day\n%03d",day)]
    x_scale = 0:12 * 6
  } else if (num_of_dup == "quad"){
    dt2 = copy(dt)
    dt2 = dt2[,day := day-1]
    dt2 = dt2[,hour := hour + 24]
    dt3 = copy(dt2)
    dt3 = dt3[,day := day-1]
    dt3 = dt3[,hour := hour + 24]
    dt4 = copy(dt3)
    dt4 = dt4[,day := day-1]
    dt4 = dt4[,hour := hour + 24]
    dt = dt[day<(max(day)-2)]
    binddt = rbind(dt, dt2, dt3, dt4)
    binddt = binddt[day>-1]
    binddt = binddt[, day_str := sprintf("day\n%03d",day)]
    x_scale = 0:16 * 6
  }
  y_var = sum_activity
  } else if (mean == TRUE){
    
  }
  # p = ggplot(binddt,aes(hour,ymax=y_var, ymin=min(y_var))) +
  #   geom_ribbon() +
  #   facet_grid(day_str ~ .) + scale_x_continuous(name="time (h)",breaks = x_scale)+
  #   scale_y_continuous(name="y")
  p = ggplot(binddt, aes(x=hour, y=y_var)) + 
      geom_col() +
      facet_grid(day_str ~ .) + scale_x_continuous(name="time (h)",breaks = x_scale)+
      scale_y_continuous(name="activity")
  p
  #return(binddt)
}

dam1 = DAM1_reader("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C01.txt")
actoplot(activity, dam1, num_of_dup = "quad")

# myoverviewPlot <- function(y,data,
#                          condition=NULL,
#                          summary_time_window=mins(30),
#                          normalise_var_per_id=FALSE,
#                          time_wrap=NULL,
#                          time_unit_conversion=days){
#   
#   dt = copy(as.data.table(data))  
#   #print(y)
#   y_var_name <- deparse(substitute(y))
#   print(y_var_name)
#   setnames(dt,y_var_name,"y_var")
#   print(dt)
#   dt[,t_r := floor(t/summary_time_window) * summary_time_window]
#   print(dt)
#   if(!is.null(time_wrap))
#      dt[,t_r := t_r %% time_wrap]
#   dt[,y_var:=as.numeric(y_var)]
#   print(dt)
#   c_var_name <- deparse(substitute(condition))
#   print(c_var_name)
#   if(c_var_name == "NULL")
#      dt[,c_var:=TRUE]
#    else
#      setnames(dt, c_var_name,"c_var")
#    
#   if(normalise_var_per_id)
#     dt <- na.omit(dt[,y_var:=as.vector(scale(y_var)),by=key(dt)])
#   
#   summary_dt <- dt[,list(y_var=mean(y_var)),
#                    by=c("t_r","c_var",key(dt))]
#   print(summary_dt)
#   summary_dt[,t_d:=t_r/time_unit_conversion(1)]
#    
#   if(c_var_name != "NULL"){
#     summary_dt[,row_name:=sprintf("%s | %s | %02d",c_var,experiment_id,region_id)]
#     y_lab <- sprintf("Individual (%s | experiment_id | region_id)", c_var_name)
#   }
#   else{
#     summary_dt[,row_name:=sprintf("%s | %02d",experiment_id,region_id)]
#     y_lab <- "Individual (experiment_id | region_id)"
#   }
#   print(summary_dt)
#   p <- ggplot(summary_dt,aes(x=t_d,y=row_name,fill=y_var)) + geom_tile(alpha=1) +
#     labs(title= sprintf("Overview of individual '%s' pattern over time",y_var_name),x="time", y=y_lab)+
#     guides(fill=guide_legend(title=y_var_name))
#   p
#   #return(summary_dt)
# }

#data(sleep_sexual_dimorphism)
#DT = sleep_sexual_dimorphism
#myoverviewPlot(asleep, DT)