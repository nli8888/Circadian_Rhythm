library(rethomics)
DAM1_read = function(file, ref_hour = NULL){
  #header = scan("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C01.txt", what="", nmax= 1, sep="\n")
  #infile = scan("/media/nick/Data/Users/N/Documents/MSc_Bioinfo/2016/Data_Analysis_Project/Circadian_Rhythm/per_rescue_v2/120115A5M/120115A5mCtM007C01.txt", what=1, skip=1, sep="\n")
  header = scan(file, what="", nmax= 1, sep="\n")
  infile = scan(file, what=1, skip=1, sep="\n")
  sample_freq = infile[2]
  #sample_freq
  #typeof(infile)
  activity = infile[4:length(infile)]
  #activity
  t_list = vector()
  t = 0
  for (i in activity){
    #print(i)
    t_list = c(t_list, t)
    t = t + sample_freq
  }
  #t_list = c(t_list, sample_freq)
  #t_list
  dt = data.table(activity=activity, t=t_list)
  return(dt)
}