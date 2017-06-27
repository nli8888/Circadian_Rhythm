# Circadian_Rhythm
Notes for Quentin. You may or may not find this useful.
Current Tempory Documentation for:

DAM1_single_reader();

DAM1_multi_reader();

acto_plot();

Main arguments for the above functions are described below and also some comments which may be of interest.
####

DAM1_single_reader()
----
3 current arguments: 

file - directory path of the DAM1 file.

time_format = "min" - converts the sampling frequency stated in the DAM1 file into seconds, if the sampling frequency is already in seconds then "sec" should be used. Otherwise, use "min" or "hr".

time_to_round_to = rethomics::hours(1) - creates an additional coloum in data.table() that keeps track of the nearest chosen interval (default 1 hour) to round down to. Needed for grouping data into days.

####

DAM1_multi_reader()
----
same arguments as above except PATH is used instead of file:

PATH - directory path of directory containing all DAM1 files of interest.

####

actoplot()
----
Any ggplot2 functions can be used to modify the plots
E.g. actoplot(EXAMPLE) + ggtitle("A better title")

file1 - data.table() of DAM1, DAM2, or ethoscope data

file_format = "dam1"/"dam2"/"ethoscope" - when choosing the data to use also indicate which format it is in 

type_of_plot = "bar" - different ggplot2 plot types are available ("line", "ribbon", and "tile") with "bar" plots as standard.

num_of_dups = 2 - the number of duplicated days for multiplotted actograms. Any interger can be used as long as there's sufficient data. 

operation - summary statistical operation to be performed on the data over chosen interval "time_to_round". This can techically be any actual function that can be used in R, such as mean(), but in some scripts the arguments may be represented as character strings. This was due to the fact of being implemented into an R Shiny webapp and complications in parsing arguments to and from the server. It shouldn't be too hard to revert the changes.

pop_overview - same idea as "operation" but basically an additional summary statistic operation to be performed on population data. If it is "NULL", then no operation will be performed and all individual data from the population will be plotted. Currently this foreces the plot to be a line graph as this is the most practical plot to visualise all the data.

conidition - solely for ethoscope data currently. As ethoscope data has multiple variables being measured at the same time ("moving, "asleep" etc.) this argument allows you to choose which variable to plot.

time_to_round - the time interval for which data is grouped by and is statistically summarised by "operation" (and "pop_overview" for population data). Default is 1 hour but can be technically changed. HOWEVER, note that if changing this will affect the x-axis labelling due to the way the code works. So for example, time_to_round = hours(0.5) is possible for 30 minute intervals but the x-axis will double as each interval is considered as an integer. This is an issue related to how the duplication of data is coded currently when plotting multi-plotted actograms. Hopefully you can figure out a work around.

LD_days_start - integer, the day when LD starts.

LD_days_end - the day when LD end. If only one day is desired then both LD_days_start and LD_days_end have to equal the same day

Same applies for DD_days_start and DD_days_end. Note that any overlapp of days between LD and DD will result in DD overwriting LD

For LD, the individual duration of light and darkness can be adjusted with D_start, D_end_L_start, and L_end. Default is 12:12 LD (D_start = 0, D_end_L_start = 12, L_end = 24). To actually shift the LD as a whole use LD_offset. LD_offset = 5 will shift the LD to the right by 5 hours and -5 to the left by 5 hours. 

Note, as of the current time of writing this I have not tested what happens when the time_to_round does not equal 1 hour on the effects of these LD and DD options. 




