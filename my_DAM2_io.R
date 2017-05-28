library(rethomics)
testloadSingleDAM2File <- function(FILE, 
                               start_date=-Inf,
                               stop_date=+Inf,
                               tz = "",
                               verbose=TRUE
){
  # 1 load time stamps
  # 1 load time stamps
  if(verbose)
    print(sprintf("Reading %s.",FILE))
  dt <- fread(FILE, select=2:4, header = FALSE)
  print(dt)
  dt[,datetime:=paste(V2,V3, sep=" ")]
  print(dt) 
  dt[,t:=as.POSIXct(strptime(datetime,"%d %b %y %H:%M:%S",tz=tz))]
  print(dt)
  min_date <- rethomics:::dateStrToPosix(start_date,tz)
  max_date <- rethomics:::dateStrToPosix(stop_date,tz)
  cat(min_date, max_date, "\n")
  # if time is not in date, we add a day
  # TODO
  # ifelse(parseDateStr(stop_date)$has_time, max_date,max_date +hours(24))
  
  if(max_date < min_date)
    stop("`max_date` MUST BE greater than `min_date`")
  #Below for valid data received monitoring; google dam2 file format, 4th coloum
  valid_dt <- dt[,.( 
    valid = (t >= min_date & t < max_date & V4 ==1),
    idx = 1:.N,
    t=t
  )]
  print(valid_dt)
  valid_dt <- valid_dt[valid == T]
  print(valid_dt)
  first = min(valid_dt[,idx]) 
  last = max(valid_dt[,idx])     
  print(first)
  print(last)
  #2 check time stamps
  if(nrow(valid_dt) < 1){
    stop("There is apparently no data in this range of dates")
  }
  
  valid_dt[,diff_t := c(NA,diff(t))]
  valid_dt <- na.omit(valid_dt)
  sampling_periods <- valid_dt[,.(n=.N),by=diff_t]
  if(nrow(sampling_periods) > 1){
    warning(sprintf("The sampling period is not always regular in %s.
                    Some reads must have been skipped.",FILE))
    #fixme show a table of samplig rates
  }
  
  
  # 1 find duplicated time stamps.
  valid_dt[,t_str := as.character(t)]
  setkeyv(valid_dt,"t_str")
  n_dups <- sum (duplicated(valid_dt))
  
  if(n_dups > 0){
    warning(sprintf("Some of the dates are repeated between successive measument in %s.",FILE))
    
  }
  
  if(n_dups > 50){
    stop("More than 50 duplicated dates entries in the queries file.
         This is a likely instance of the recording computer changing time (e.g. between winter and summer time)")
  }
  
  if(any(sampling_periods[,diff_t] < 0)){
    stop("Come measument appear to have been recorded 'before' previous measuments.
         It looks as if the recording computer went back in time!")
  }
  
  
  
  # 3 actually load the file
  DAM_COL_NAMES <- c("idx", "day_month_year", "time","status", sprintf("channel_%02d", 1:32))
  dt_list <- fread(FILE, drop=5:10, header = FALSE,
                   skip = first-1, nrows = last-first+1)
  setnames(dt_list,DAM_COL_NAMES)
  dt_list <- dt_list[status ==1]
  dt_list[,datetime:=paste(day_month_year,time, sep=" ")]
  dt_list[,t:=as.POSIXct(strptime(datetime,"%d %b %y %H:%M:%S",tz=tz))]
  setkeyv(dt_list,"datetime")
  dt_list <- unique	(dt_list, by=key(dt_list))
  #clean table from unused variables (idx,time, datetime...)
  dt_list[,time:=NULL]
  dt_list[,datetime:=NULL]
  dt_list[,idx:=NULL]
  dt_list[,day_month_year:=NULL]
  dt_list[,status:=NULL]
  print(dt_list)
  out <- as.data.table(melt(dt_list,id="t"))
  
  roi_value <- function(channel_string){
    s <- strsplit(channel_string,"_")
    num <- as.integer(sapply(s,function(x) x[2]))
    return(num)
  }
  
  #get the values on activity
  
  setnames(out,"value", "activity")
  #out[,activity:=value]
  out[,region_id:=roi_value(as.character(variable))]
  out[,variable := NULL]
  #print(out)
  return(out)
}

#file = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/Anne_DAM2_Data/2015-08-05_M002_merged.txt"
#file = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/Anne_DAM2_Data/2015-08-05_M010_merged.txt"
file = "/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/Anne_DAM2_Data/2016-11-20_M012_merged.txt"
out = testloadSingleDAM2File(file)

#dt <- fread(file, header = FALSE)
#dt = read.table(file)

