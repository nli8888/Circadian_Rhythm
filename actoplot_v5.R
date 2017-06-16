source("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/DAM1_reader.R")

actoplot_dam1 = function(file1 = file1,
                         #y = activity, #name of column in datatable you want to display on y-axis
                         type_of_plot = "bar", #can be "bar", "line", "ribbon" or "tile"
                         num_of_plot = 2, #can be any integer
                         #operation = mean, #can be sum/median ##REMINDER CHANGE THIS BACK FOR NON WEBSITE
                         operation = "mean", ##REMINDER CHANGE THIS BACK FOR NON WEBSITE
                         #DD_days = NULL, #range of days that were in DD, e.g. LD = 0:2 for days 0 to 2 ##REMINDER CHANGE THIS BACK FOR NON WEBSITE
                         DD_days_start = "none", #for sake of website, can't use NULL ##REMINDER CHANGE THIS BACK FOR NON WEBSITE
                         DD_days_end = "none", ##REMINDER CHANGE THIS BACK FOR NON WEBSITE
                         #LD_days = NULL, #range of days that were in LD, e.g. LD = 0:2 for days 0 to 2 ##REMINDER CHANGE THIS BACK FOR NON WEBSITE
                         LD_days_start = "none", ##REMINDER CHANGE THIS BACK FOR NON WEBSITE
                         LD_days_end = "none", ##REMINDER CHANGE THIS BACK FOR NON WEBSITE
                         D_start = 0,
                         D_end_L_start = 12,
                         L_end = 24,
                         LD_offset = NULL, #how much you want to shift the LD annotations by
                         condition = NULL,
                         #pop_overview = NULL, #if not null, then can choose which operation like above to further summarise the population data 
                         pop_overview = "mean", ##REMINDER CHANGE THIS BACK FOR NON WEBSITE
                         time_to_round = rethomics:::hours(1) #see if can rename this to something used before
                         ){
  num_of_plot = as.numeric(num_of_plot)
  if (num_of_plot%%1!=0){
    stop("num_of_plot must be an integer")
  }
  dt = copy(as.data.table(file1))
  t_round = floor(dt[,t]/(time_to_round))
  x_vals = (t_round%%(days(1)/time_to_round))
  day = (floor(dt[,t]/(rethomics::days(1))))
  dt[, t_round := t_round]
  dt[, x_vals := x_vals]
  dt[, day := day]
  setkeyv(dt, c("experiment_id", "region_id", "date", "machine_name"))
  if (operation == "mean"){
    operation = mean
  } else if (operation == "median"){
    operation = median
  } else if (operation == "sum"){
    operation = sum
  }
  if (pop_overview == "mean"){
    pop_overview = mean
  } else if (pop_overview == "median"){
    pop_overview = median
  } else if (pop_overview == "sum"){
    pop_overview = sum
  }
  if (DD_days_start == "none"||DD_days_end == "none"){
    DD_days = NULL
  } else {
    DD_days = DD_days_start:DD_days_end
  }
  if (LD_days_start == "none"||LD_days_end == "none"){
    LD_days = NULL
  } else {
    LD_days = LD_days_start:LD_days_end
  }
  DD = DD_days
  LD = LD_days
  offset = LD_offset
  if (length(unique(dt[,experiment_id])) == 1){
  dt = dt[,.(experiment_id = experiment_id,
             condition = condition,
             machine_name = machine_name, 
             region_id = region_id, 
             date = date, 
             t=t,
             activity = operation(activity), 
             x_vals = x_vals, 
             day = day), 
          by = t_round]
  dt = unique(dt)
  if (num_of_plot>1){
    for (i in 2:num_of_plot){
      dt_temp = copy(dt)
      dt_temp = dt_temp[, day := day-1]
      dt_temp = dt_temp[, x_vals := x_vals + (days(1)/time_to_round)]
      dt = dt[day<(max(day))]
      dt = rbind(dt, dt_temp)
      dt = unique(dt)
    }
  }
  dt = dt[day>-1]
  dt = dt[, day_str := sprintf("day\n%03d", day)]
  x_scale = 0:(12*num_of_plot) * 6
  if (type_of_plot == "bar"){
    p = ggplot(dt, aes(x=x_vals, y=activity, width=1)) 
    if (!is.null(LD)){
      ii=0
      a=1
      for (i in 0:(num_of_plot+1)){
        #print(ii)
        x_min1 = D_start + ii + offset
        x_max1 = D_end_L_start + ii + offset
        #x_min2 = L_start + ii + offset
        x_min2 = D_end_L_start + ii + offset
        x_max2 = L_end + ii + offset
        cat("MAXHOUR", max(x_vals), (max(x_vals)+1)*num_of_plot, "\n")
        if (x_min1 < 0){
          x_min1 = 0
        }
        if (x_min1 > ((max(x_vals)+1)*num_of_plot)){
          a = 0
        }
        if (x_max1 < 0){
          x_max1 = 0
        }
        if (x_max1 > ((max(x_vals)+1)*num_of_plot)){
          print("yes")
          x_max1 = (max(x_vals)+1)*num_of_plot
          print(x_max1)
        }
        cat("x_m values", x_min1, x_max1, x_min2, x_max2, "\n")
      p = p +
        geom_rect(data=subset(dt, day_str == sprintf("day\n%03d", LD)), aes(fill=day_str), fill="grey", color="grey",size=0,
                  xmin = x_min1,
                  xmax = x_max1, ymin = -Inf,ymax = Inf,alpha = a) +
        geom_rect(data=subset(dt, day_str == sprintf("day\n%03d", LD)), aes(fill=day_str), fill="grey", color="grey",size=0,
                  xmin = x_min2,
                  xmax = x_max2, ymin = -Inf,ymax = Inf,alpha = 0)
        #ii = ii + (max(x_vals)+1)
        ii = ii + L_end
      }
    }
    if (!is.null(DD)){
      p = p +
        geom_rect(data=subset(dt, day_str == sprintf("day\n%03d", DD)), aes(fill=day_str), fill="grey", color="grey",size=0,xmin = 0,xmax = (max(x_vals)+1)*num_of_plot,ymin = -Inf,ymax = Inf,alpha = 1)
    }
    p = p +
      geom_col(position = position_nudge(x = 0.5)) +
      facet_grid(day_str ~ .) + 
      scale_x_continuous(name="time (hours)", breaks = x_scale) +
      scale_y_continuous(name="activity") +
      theme(panel.spacing = unit(0, "lines"), 
            plot.title = element_text(hjust = 0.5, size = 18), 
            axis.text.x = element_text(size=16),
            axis.text.y = element_text(size=10),
            axis.title=element_text(size=14,face="bold"),
            strip.text = element_text(face="bold", size=14)) +
      ggtitle(sprintf("Actogram plot of individual activity over time of experiment %s", unique(dt[,experiment_id])))
  } else if (type_of_plot == "line"){
    p = ggplot(dt, aes(x_vals, activity)) +
      geom_line() +
      facet_grid(day_str ~ .) +
      scale_x_continuous(name="time (hours)",breaks = x_scale) +
      scale_y_continuous(name="activity") +
      theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
      ggtitle(sprintf("Actogram plot of individual activity over time of experiment %s", unique(dt[,experiment_id])))
  } else if (type_of_plot == "ribbon"){
    p = ggplot(dt, aes(x_vals, ymin=min(activity), ymax=activity)) +
      geom_ribbon() +
      facet_grid(day_str ~ .) +
      scale_x_continuous(name="time (hours)",breaks = x_scale) +
      scale_y_continuous(name="activity") +
      theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
      ggtitle(sprintf("Actogram plot of individual activity over time of experiment %s", unique(dt[,experiment_id])))
  } else if (type_of_plot == "tile"){
    dt[,row_name:=sprintf("%s | %02d",experiment_id,region_id)]
    p = ggplot(dt,aes(x=x_vals,y=row_name,fill=activity)) + 
      geom_tile(alpha=1) +
      facet_grid(day_str ~ .) +
      scale_x_continuous(name="time (hours)",breaks = x_scale) +
      theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
      labs(title="Overview of individual activity pattern over time",x="time", y="activity") +
      guides(fill=guide_legend(title="activity"))
  }
  } else if (length(unique(dt[,experiment_id])) > 1){
    summary_dt = dt[,list(activity=operation(activity), 
                          x_vals=x_vals,
                          day=day),
                     by=c("t_round", key(dt), "condition")]
    summary_dt = unique(summary_dt)
    setkeyv(summary_dt, c("experiment_id", "date", "machine_name"))
    summary_dt_all_animals = summary_dt[,list(activity=operation(activity)),
                                        by=c("t_round", #see if can utilize key(dt)
                                             key(summary_dt),
                                             "condition",
                                             "x_vals",
                                             "day")]
    if (num_of_plot>1){
      for (i in 2:num_of_plot){
        dt_temp = copy(summary_dt_all_animals)
        dt_temp = dt_temp[, day := day-1]
        dt_temp = dt_temp[, x_vals := x_vals + (days(1)/time_to_round)]
        summary_dt_all_animals = summary_dt_all_animals[day<(max(day))]
        summary_dt_all_animals = rbind(summary_dt_all_animals, dt_temp)
        summary_dt_all_animals = unique(summary_dt_all_animals)
      }
    }
    if (!is.null(pop_overview)){
      summary_dt_all_animals = summary_dt_all_animals[,list(activity=pop_overview(activity)),
                                                      by=c("t_round",
                                                           "x_vals",
                                                           "day")]
      summary_dt_all_animals = summary_dt_all_animals[day>-1]
      summary_dt_all_animals = summary_dt_all_animals[, day_str := sprintf("day\n%03d", day)]
      x_scale = 0:(8*num_of_plot) * 6
      if (type_of_plot == "bar"){
        p = ggplot(summary_dt_all_animals, aes(x_vals, activity, width=1)) 
        if (!is.null(LD)){
          ii=0
          a=1
          for (i in 0:(num_of_plot+1)){
            #print(ii)
            x_min1 = D_start + ii + offset
            x_max1 = D_end_L_start + ii + offset
            #x_min2 = L_start + ii + offset
            x_min2 = D_end_L_start + ii + offset
            x_max2 = L_end + ii + offset
            cat("MAXHOUR", max(x_vals), (max(x_vals)+1)*num_of_plot, "\n")
            if (x_min1 < 0){
              x_min1 = 0
            }
            if (x_min1 > ((max(x_vals)+1)*num_of_plot)){
              a = 0
            }
            if (x_max1 < 0){
              x_max1 = 0
            }
            if (x_max1 > ((max(x_vals)+1)*num_of_plot)){
              print("yes")
              x_max1 = (max(x_vals)+1)*num_of_plot
              print(x_max1)
            }
            cat("x_m values", x_min1, x_max1, x_min2, x_max2, "\n")
            p = p +
              geom_rect(data=subset(summary_dt_all_animals, day_str == sprintf("day\n%03d", LD)), aes(fill=day_str), fill="grey", color="grey",size=0,
                        xmin = x_min1,
                        xmax = x_max1, ymin = -Inf,ymax = Inf,alpha = a) +
              geom_rect(data=subset(summary_dt_all_animals, day_str == sprintf("day\n%03d", LD)), aes(fill=day_str), fill="grey", color="grey",size=0,
                        xmin = x_min2,
                        xmax = x_max2, ymin = -Inf,ymax = Inf,alpha = 0)
            #ii = ii + (max(x_vals)+1)
            ii = ii + L_end
          }
        }
        if (!is.null(DD)){
          p = p +
            geom_rect(data=subset(summary_dt_all_animals, day_str == sprintf("day\n%03d", DD)), aes(fill=day_str), fill="grey", color="grey",size=0,xmin = 0,xmax = (max(x_vals)+1)*num_of_plot,ymin = -Inf,ymax = Inf,alpha = 1)
        }
        p = p +
          geom_col(position = position_nudge(x = 0.5)) +
          facet_grid(day_str ~ .) +
          scale_x_continuous(name="time (hours)",breaks = x_scale) +
          scale_y_continuous(name="activity") +
          theme(panel.spacing = unit(0, "lines"), plot.title = element_text(hjust = 0.5)) +
          ggtitle("Overview Actogram plot of population activity over time")
      } else if (type_of_plot == "line"){
        p = ggplot(summary_dt_all_animals, aes(x_vals, activity)) +
          geom_line() +
          facet_grid(day_str ~ .) +
          scale_x_continuous(name="time (hours)",breaks = x_scale) +
          scale_y_continuous(name="activity") +
          theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
          ggtitle("Overview Actogram plot of population activity over time")
      } else if (type_of_plot == "ribbon"){
        p = ggplot(summary_dt_all_animals, aes(x_vals, ymin=min(activity), ymax=activity)) +
          geom_ribbon() +
          facet_grid(day_str ~ .) +
          scale_x_continuous(name="time (hours)",breaks = x_scale) +
          scale_y_continuous(name="activity") +
          theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
          ggtitle("Overview Actogram plot of population activity over time")
      } else if (type_of_plot == "tile"){
        summary_dt_all_animals[,row_name:=""]
        p = ggplot(summary_dt_all_animals, aes(x=x_vals, y=row_name, fill=activity)) +
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
      x_scale = 0:(8*num_of_plot) * 6
      p = ggplot(summary_dt_all_animals, aes(x_vals, activity, colour=machine_name)) +
        geom_line() +
        facet_grid(day_str ~ .) +
        scale_x_continuous(name="time (hours)",breaks = x_scale) +
        scale_y_continuous(name="activity") +
        theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
        ggtitle("Actogram plot of population activity over time")
      # if (is.null(condition)){
      #   p = ggplot(summary_dt_all_animals, aes(x_vals, activity, colour=machine_name)) +
      #     geom_line() +
      #     facet_grid(day_str ~ .) +
      #     scale_x_continuous(name="time (hours)",breaks = x_scale) +
      #     scale_y_continuous(name="activity") +
      #     theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
      #     ggtitle("Actogram plot of population activity over time")
      # } else if (!is.null(condition)){
      #   p = ggplot(summary_dt_all_animals, aes(x_vals, activity, colour=condition)) +
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
  # if (length(unique(dt[,experiment_id])) == 1){
  #   return(dt)
  # } else if (length(unique(dt[,experiment_id])) > 1){
  #   return(summary_dt_all_animals)
  # }
}
##DAM1##
dam1 = DAM1_single_reader("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C03.txt")
#dam1 = DAM1_single_reader("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/Estaban_new_data/Circadian_data_for_Nicholas/220914es5/220914es5CtM011C27.txt")
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

# acto = actoplot_dam1(dammulti1,
#                      num_of_plot = 4,
#                      type_of_plot = "bar", #currently only "bar" has LD and DD annotations available
#                      #DD_days = NULL, #range of days that were in DD, e.g. LD = 0:2 for days 0 to 2
#                      DD_days_start = 7, #for website functionality
#                      DD_days_end = 4, #for website functionality
#                      #LD_days = 0:2,
#                      LD_days_start = 0,
#                      LD_days_end = 2,
#                      LD_offset = 0,
#                      D_start = 0,
#                      D_end_L_start = 12,
#                      L_end = 24,
#                      operation = "mean",
#                      pop_overview = "mean",
#                      time_to_round = hours(1))
# acto

##DAM2##
#file1 = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/Anne_DAM2_Data/2015-08-05_M002_merged.txt"
#file1 = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/Anne_DAM2_Data/2015-08-05_M010_merged.txt"  
file1 = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/Anne_DAM2_Data/2016-11-20_M012_merged.txt"
query = data.table(path=file1,
                   start_date="2016-11-21_00-00-00", 
                   stop_date="2016-12-07", 
                   #region_id=c(1:32),
                   region_id=c(2:32),
                   machine_name = "M012")
dam2 = loadDAM2Data(query)
#dam2 = rethomics:::loadSingleDAM2File(file1)
# library(chron)
# library(lubridate)
actoplot_dam2 = function(file1,
                         type_of_plot = "bar", #can be "bar", "line", "ribbon" or "tile"
                         num_of_plot = 2, #can be any integer
                         #mean = FALSE #change to actually use a function
                         operation = mean, #can be sum/median
                         DD_days = NULL, #range of days that were in DD, e.g. LD = 0:2 for days 0 to 2
                         LD_days = NULL, #range of days that were in LD, e.g. LD = 0:2 for days 0 to 2
                         LD_offset = NULL, #how much you want to shift the LD annotations by
                         D_start = 0,
                         D_end_L_start = 12,
                         L_end = 24,
                         #pop_overview = NULL, #if not null, then can choose which operation like above to further summarise the population data 
                         time_to_round = rethomics:::hours(1) #see if can rename this to something used before
){
  num_of_plot = as.numeric(num_of_plot)
  if (num_of_plot%%1!=0){
    stop("num_of_plot must be an integer")
  }
  # if (is.null(machine_name)){
  #   stop('"machine_name" must be specified')
  # }
  dt = copy(as.data.table(file1))
  t_round = floor(dt[,t]/(time_to_round))
  x_vals = t_round%%24
  day = (floor(t_round/(24)))
  dt[, t_round := t_round]
  dt[, x_vals := x_vals]
  dt[, day := day]
  # setkeyv(dt, c("experiment_id", "region_id", "start_date", "machine_name"))
  LD = LD_days
  DD = DD_days
  offset = LD_offset
  if (length(unique(dt[,region_id])) == 1){
    dt = dt[,list(activity=operation(activity), 
                          x_vals=x_vals,
                          day=day),
                    by=c("t_round", key(dt))]
    dt = unique(dt)
    if (num_of_plot>1){
      for (i in 2:num_of_plot){
        dt_temp = copy(dt)
        dt_temp = dt_temp[, day := day-1]
        dt_temp = dt_temp[, x_vals := x_vals + 24]
        dt = dt[day<(max(day))]
        dt = rbind(dt, dt_temp)
        dt = unique(dt)
      }
    }
    dt = dt[day>-1]
    dt = dt[, day_str := sprintf("day\n%03d", day)]
    x_scale = 0:(4*num_of_plot) * 6
    if (type_of_plot == "bar"){
      p = ggplot(dt, aes(x=x_vals, y=activity, width=1)) 
      if (!is.null(LD)){
        ii=0
        a=1
        for (i in 0:(num_of_plot+1)){
          #print(ii)
          x_min1 = D_start + ii + offset
          x_max1 = D_end_L_start + ii + offset
          #x_min2 = L_start + ii + offset
          x_min2 = D_end_L_start + ii + offset
          x_max2 = L_end + ii + offset
          cat("MAXHOUR", max(x_vals), (max(x_vals)+1)*num_of_plot, "\n")
          if (x_min1 < 0){
            x_min1 = 0
          }
          if (x_min1 > ((max(x_vals)+1)*num_of_plot)){
            a = 0
          }
          if (x_max1 < 0){
            x_max1 = 0
          }
          if (x_max1 > ((max(x_vals)+1)*num_of_plot)){
            print("yes")
            x_max1 = (max(x_vals)+1)*num_of_plot
            print(x_max1)
          }
          cat("x_m values", x_min1, x_max1, x_min2, x_max2, "\n")
          p = p +
            geom_rect(data=subset(dt, day_str == sprintf("day\n%03d", LD)), aes(fill=day_str), fill="grey", color="grey",size=0,
                      xmin = x_min1,
                      xmax = x_max1, ymin = -Inf,ymax = Inf,alpha = a) +
            geom_rect(data=subset(dt, day_str == sprintf("day\n%03d", LD)), aes(fill=day_str), fill="grey", color="grey",size=0,
                      xmin = x_min2,
                      xmax = x_max2, ymin = -Inf,ymax = Inf,alpha = 0)
          #ii = ii + (max(x_vals)+1)
          ii = ii + L_end
        }
      }
      if (!is.null(DD)){
        p = p +
          geom_rect(data=subset(dt, day_str == sprintf("day\n%03d", DD)), aes(fill=day_str), fill="grey", color="grey",size=0,xmin = 0,xmax = (max(x_vals)+1)*num_of_plot,ymin = -Inf,ymax = Inf,alpha = 1)
      }
      p = p +
        geom_col(position = position_nudge(x = 0.5)) +
        facet_grid(day_str ~ .) + 
        scale_x_continuous(name="time (hours)",breaks = x_scale) +
        scale_y_continuous(name="activity") +
        theme(panel.spacing = unit(0, "lines"), plot.title = element_text(hjust = 0.5)) +
        ggtitle(sprintf("Actogram plot of individual activity over time of experiment %s", unique(dt[,experiment_id])))
    } else if (type_of_plot == "line"){
      p = ggplot(dt, aes(x_vals, activity)) +
        geom_line() +
        facet_grid(day_str ~ .) +
        scale_x_continuous(name="time (hours)",breaks = x_scale) +
        scale_y_continuous(name="activity") +
        theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
        ggtitle(sprintf("Actogram plot of individual activity over time of experiment %s", unique(dt[,experiment_id])))
    } else if (type_of_plot == "ribbon"){
      p = ggplot(dt, aes(x_vals, ymin=min(activity), ymax=activity)) +
        geom_ribbon() +
        facet_grid(day_str ~ .) +
        scale_x_continuous(name="time (hours)",breaks = x_scale) +
        scale_y_continuous(name="activity") +
        theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
        ggtitle(sprintf("Actogram plot of individual activity over time of experiment %s", unique(dt[,experiment_id])))
    } else if (type_of_plot == "tile"){
      dt[,row_name:=sprintf("%s | %02d",experiment_id,region_id)]
      p = ggplot(dt,aes(x=x_vals,y=row_name,fill=activity)) + 
        geom_tile(alpha=1) +
        facet_grid(day_str ~ .) +
        scale_x_continuous(name="time (hours)",breaks = x_scale) +
        theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
        labs(title="Overview of individual activity pattern over time",x="time", y="activity") +
        guides(fill=guide_legend(title="activity"))
    }
  } else if (length(unique(dt[,region_id])) > 1) {
    summary_dt = dt[,list(activity=operation(activity), #perform operation by t_round, i.e.average activity per hour 
                          x_vals=x_vals,
                          day=day),
                    by=c("t_round", key(dt))]
    summary_dt = unique(summary_dt)
    setkeyv(summary_dt, c("experiment_id", "start_date", "stop_date", "machine_name"))
    summary_dt_all_animals = summary_dt[,list(activity=operation(activity)),
                                                    by=c("t_round",
                                                         key(summary_dt),
                                                         "x_vals",
                                                         "day")]
    if (num_of_plot>1){
      for (i in 2:num_of_plot){
        dt_temp = copy(summary_dt_all_animals)
        dt_temp = dt_temp[, day := day-1]
        dt_temp = dt_temp[, x_vals := x_vals + 24]
        summary_dt_all_animals = summary_dt_all_animals[day<(max(day))]
        summary_dt_all_animals = rbind(summary_dt_all_animals, dt_temp)
        summary_dt_all_animals = unique(summary_dt_all_animals)
      }
    }
    summary_dt_all_animals = summary_dt_all_animals[day>-1]
    summary_dt_all_animals = summary_dt_all_animals[, day_str := sprintf("day\n%03d", day)]
    x_scale = 0:(4*num_of_plot) * 6
    if (type_of_plot == "bar"){
      p = ggplot(summary_dt_all_animals, aes(x_vals, activity, width=1)) 
      if (!is.null(LD)){
        ii=0
        a=1
        for (i in 0:(num_of_plot+1)){
          #print(ii)
          x_min1 = D_start + ii + offset
          x_max1 = D_end_L_start + ii + offset
          #x_min2 = L_start + ii + offset
          x_min2 = D_end_L_start + ii + offset
          x_max2 = L_end + ii + offset
          cat("MAXHOUR", max(x_vals), (max(x_vals)+1)*num_of_plot, "\n")
          if (x_min1 < 0){
            x_min1 = 0
          }
          if (x_min1 > ((max(x_vals)+1)*num_of_plot)){
            a = 0
          }
          if (x_max1 < 0){
            x_max1 = 0
          }
          if (x_max1 > ((max(x_vals)+1)*num_of_plot)){
            print("yes")
            x_max1 = (max(x_vals)+1)*num_of_plot
            print(x_max1)
          }
          cat("x_m values", x_min1, x_max1, x_min2, x_max2, "\n")
          p = p +
            geom_rect(data=subset(summary_dt_all_animals, day_str == sprintf("day\n%03d", LD)), aes(fill=day_str), fill="grey", color="grey",size=0,
                      xmin = x_min1,
                      xmax = x_max1, ymin = -Inf,ymax = Inf,alpha = a) +
            geom_rect(data=subset(summary_dt_all_animals, day_str == sprintf("day\n%03d", LD)), aes(fill=day_str), fill="grey", color="grey",size=0,
                      xmin = x_min2,
                      xmax = x_max2, ymin = -Inf,ymax = Inf,alpha = 0)
          #ii = ii + (max(x_vals)+1)
          ii = ii + L_end
        }
      }
      if (!is.null(DD)){
        p = p +
          geom_rect(data=subset(summary_dt_all_animals, day_str == sprintf("day\n%03d", DD)), aes(fill=day_str), fill="grey", color="grey",size=0,xmin = 0,xmax = (max(x_vals)+1)*num_of_plot,ymin = -Inf,ymax = Inf,alpha = 1)
      }
      p = p +
        geom_col(position = position_nudge(x = 0.5)) +
        facet_grid(day_str ~ .) +
        scale_x_continuous(name="time (hours)",breaks = x_scale) +
        scale_y_continuous(name="activity") +
        theme(panel.spacing = unit(0, "lines"), plot.title = element_text(hjust = 0.5)) +
        ggtitle("Overview Actogram plot of population activity over time")
    } else if (type_of_plot == "line"){
      p = ggplot(summary_dt_all_animals, aes(x_vals, activity)) +
        geom_line() +
        facet_grid(day_str ~ .) +
        scale_x_continuous(name="time (hours)",breaks = x_scale) +
        scale_y_continuous(name="activity") +
        theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
        ggtitle("Overview Actogram plot of population activity over time")
    } else if (type_of_plot == "ribbon"){
      p = ggplot(summary_dt_all_animals, aes(x_vals, ymin=min(activity), ymax=activity)) +
        geom_ribbon() +
        facet_grid(day_str ~ .) +
        scale_x_continuous(name="time (hours)",breaks = x_scale) +
        scale_y_continuous(name="activity") +
        theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5)) +
        ggtitle("Overview Actogram plot of population activity over time")
    } else if (type_of_plot == "tile"){
      summary_dt_all_animals[,row_name:=""]
      p = ggplot(summary_dt_all_animals, aes(x=x_vals, y=row_name, fill=activity)) +
        geom_tile(alpha=1) +
        facet_grid(day_str ~ ., switch = "y") +
        scale_x_continuous(name="time (hours)",breaks = x_scale) +
        theme(panel.spacing = unit(0.2, "lines"), plot.title = element_text(hjust = 0.5), axis.ticks.y = element_blank()) +
        labs(title="Overview Actogram plot of population activity over time",x="time", y="") #+
      #guides(fill=guide_legend(title="activity"))
    }
  } 
  print(p)
  return(dt)
}

# acto_dam2 = actoplot_dam2(dam2, 
#                           num_of_plot = 2, 
#                           type_of_plot = "bar", 
#                           LD_days = 0:3, 
#                           DD_days = 4:14, 
#                           LD_offset = -16, 
#                           D_start = 0,
#                           D_end_L_start = 14,
#                           L_end = 20,
#                           operation = mean)
# acto_dam2


actoplot_etho = function(file1 = file1,
                         num_of_plot = 2, #can be any integer
                         condition = NULL,
                         #operation = mean, #can be sum/median ##REMINDER CHANGE THIS BACK FOR NON WEBSITE
                         operation = "mean", ##REMINDER CHANGE THIS BACK FOR NON WEBSITE
                         #DD_days = NULL, #range of days that were in DD, e.g. LD = 0:2 for days 0 to 2 ##REMINDER CHANGE THIS BACK FOR NON WEBSITE
                         DD_days_start = "none", #for sake of website, can't use NULL ##REMINDER CHANGE THIS BACK FOR NON WEBSITE
                         DD_days_end = "none", ##REMINDER CHANGE THIS BACK FOR NON WEBSITE
                         #LD_days = NULL, #range of days that were in LD, e.g. LD = 0:2 for days 0 to 2 ##REMINDER CHANGE THIS BACK FOR NON WEBSITE
                         LD_days_start = "none", ##REMINDER CHANGE THIS BACK FOR NON WEBSITE
                         LD_days_end = "none", ##REMINDER CHANGE THIS BACK FOR NON WEBSITE
                         D_start = 0,
                         D_end_L_start = 12,
                         L_end = 24,
                         LD_offset = NULL, #how much you want to shift the LD annotations by
                         #pop_overview = NULL, #if not null, then can choose which operation like above to further summarise the population data 
                         pop_overview = "mean", ##REMINDER CHANGE THIS BACK FOR NON WEBSITE
                         time_to_round = rethomics:::hours(1) #see if can rename this to something used before
){
  num_of_plot = as.numeric(num_of_plot)
  if (num_of_plot%%1!=0){
    stop("num_of_plot must be an integer")
  }
  
  dt = copy(as.data.table(file1))
  to.replace <- names(which(sapply(dt, is.logical)))
  for (var in to.replace) dt[, var:= as.numeric(get(var)), with=FALSE]
  t_round = floor(dt[,t]/(time_to_round))
  x_vals = (t_round%%(days(1)/time_to_round))
  day = (floor(dt[,t]/(rethomics::days(1))))
  dt[, t_round := t_round]
  dt[, x_vals := x_vals]
  dt[, day := day]
  c_var_name <- deparse(substitute(condition))
  print(c_var_name)
  if(c_var_name == "NULL")
    dt[,c_var:=TRUE]
  else
    setnames(dt, c_var_name,"c_var")
  setkeyv(dt, c("experiment_id", "region_id", "date", "machine_name"))
  if (operation == "mean"){
    operation = mean
  } else if (operation == "median"){
    operation = median
  } else if (operation == "sum"){
    operation = sum
  }
  if (pop_overview == "mean"){
    pop_overview = mean
  } else if (pop_overview == "median"){
    pop_overview = median
  } else if (pop_overview == "sum"){
    pop_overview = sum
  }
  if (DD_days_start == "none"||DD_days_end == "none"){
    DD_days = NULL
  } else {
    DD_days = DD_days_start:DD_days_end
  }
  if (LD_days_start == "none"||LD_days_end == "none"){
    LD_days = NULL
  } else {
    LD_days = LD_days_start:LD_days_end
  }
  DD = DD_days
  LD = LD_days
  offset = LD_offset
  if (length(unique(dt[,region_id])) == 1){
    dt = dt[,.(experiment_id = experiment_id,
               machine_name = machine_name,
               region_id = region_id,
               date = date,
               t=t,
               y_val = operation(c_var),
               x_vals = x_vals,
               day = day),
            by = t_round]
    dt = unique(dt)
    if (num_of_plot>1){
      for (i in 2:num_of_plot){
        dt_temp = copy(dt)
        dt_temp = dt_temp[, day := day-1]
        dt_temp = dt_temp[, x_vals := x_vals + (days(1)/time_to_round)]
        dt = dt[day<(max(day))]
        dt = rbind(dt, dt_temp)
        dt = unique(dt)
      }
    }
    dt = dt[day>-1]
    dt = dt[, day_str := sprintf("day\n%03d", day)]
    x_scale = 0:(12*num_of_plot) * 6
    p = ggplot(dt, aes(x=x_vals, y=y_val, width=1))
    if (!is.null(LD)){
      ii=0
      a=1
      for (i in 0:(num_of_plot+1)){
        x_min1 = D_start + ii + offset
        x_max1 = D_end_L_start + ii + offset
        #x_min2 = L_start + ii + offset
        x_min2 = D_end_L_start + ii + offset
        x_max2 = L_end + ii + offset
        if (x_min1 < 0){
          x_min1 = 0
        }
        if (x_min1 > ((max(x_vals)+1)*num_of_plot)){
          a = 0
        }
        if (x_max1 < 0){
          x_max1 = 0
        }
        if (x_max1 > ((max(x_vals)+1)*num_of_plot)){
          x_max1 = (max(x_vals)+1)*num_of_plot
        }
        p = p +
          geom_rect(data=subset(dt, day_str == sprintf("day\n%03d", LD)), aes(fill=day_str), fill="grey", color="grey",size=0,
                    xmin = x_min1,
                    xmax = x_max1, ymin = -Inf,ymax = Inf,alpha = a) +
          geom_rect(data=subset(dt, day_str == sprintf("day\n%03d", LD)), aes(fill=day_str), fill="grey", color="grey",size=0,
                    xmin = x_min2,
                    xmax = x_max2, ymin = -Inf,ymax = Inf,alpha = 0)
        #ii = ii + (max(x_vals)+1)
        ii = ii + L_end
      }
    }
    if (!is.null(DD)){
      p = p +
        geom_rect(data=subset(dt, day_str == sprintf("day\n%03d", DD)), aes(fill=day_str), fill="grey", color="grey",size=0,xmin = 0,xmax = (max(x_vals)+1)*num_of_plot,ymin = -Inf,ymax = Inf,alpha = 1)
    }
    p = p +
      geom_col(position = position_nudge(x = 0.5)) +
      facet_grid(day_str ~ .) +
      scale_x_continuous(name="time (hours)", breaks = x_scale) +
      scale_y_continuous(name="activity") +
      theme(panel.spacing = unit(0, "lines"),
            plot.title = element_text(hjust = 0.5, size = 18),
            axis.text.x = element_text(size=16),
            axis.text.y = element_text(size=10),
            axis.title=element_text(size=14,face="bold"),
            strip.text = element_text(face="bold", size=14)) +
      ggtitle(sprintf("Actogram plot of individual activity over time of experiment %s", unique(dt[,experiment_id])))
  } else if (length(unique(dt[,region_id])) > 1){
    summary_dt = dt[,list(y_val=operation(c_var), 
                          x_vals=x_vals,
                          day=day),
                    by=c("t_round", key(dt))]
    summary_dt = unique(summary_dt)
    setkeyv(summary_dt, c("experiment_id", "date", "machine_name"))
    summary_dt_all_animals = summary_dt[,list(activity=operation(activity)),
                                        by=c("t_round", #see if can utilize key(dt)
                                             key(summary_dt),
                                             "x_vals",
                                             "day")]
    if (num_of_plot>1){
      for (i in 2:num_of_plot){
        dt_temp = copy(summary_dt_all_animals)
        dt_temp = dt_temp[, day := day-1]
        dt_temp = dt_temp[, x_vals := x_vals + (days(1)/time_to_round)]
        summary_dt_all_animals = summary_dt_all_animals[day<(max(day))]
        summary_dt_all_animals = rbind(summary_dt_all_animals, dt_temp)
        summary_dt_all_animals = unique(summary_dt_all_animals)
      }
    }
  }
  #return(dt)
  print(p)
}
data("sleep_sexual_dimorphism")
sleep_sexual_dimorphism = sleep_sexual_dimorphism[region_id == 1]
acto_etho = actoplot_etho(sleep_sexual_dimorphism,
                          condition = moving,
                          num_of_plot = 2)
