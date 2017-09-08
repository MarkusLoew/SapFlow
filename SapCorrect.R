# Sap flow correction function
# 
# grabs the dT values before and after each power-on/power-off event
# this function is applied to a vector that is pre-split by date
# data has to be sorted by date!
# source("SapCorrect.R")
SapOffset <- function(data, 
                       dT, 
                       date, 
                       time,
                       hour.vector, 
                       hour.off = 21, 
                       hour.on = 4) {
   
   # only works on single days
   stopifnot(length(unique(as.Date(date))) == 1)
   date <- unique(as.Date(date))
   
   # has to be sorted
   stopifnot(is.unsorted(time) == FALSE)
   
   # in the morning: 
   # grab last dT value before power-on at 03:55 hrs
   # supposed to be faster than tail(my.dT, n = 1)
   dT.power.off.last   <- dT[hour.vector < hour.on]
   time.power.off.last <- time[hour.vector < hour.on]
   dT.power.off.last   <- dT.power.off.last[length(dT.power.off.last)] 
   time.power.off.last <- time.power.off.last[length(time.power.off.last)]
   #print(time.power.off.last)
   
   # grab the first dT after power-on at 04:10 hrs
   dT.power.on.first   <- dT[hour.vector >= hour.on]
   time.power.on.first <- time[hour.vector >= hour.on]
   dT.power.on.first   <- dT.power.on.first[3] # have to use third element to allow warming up
   time.power.on.first <- time.power.on.first[3]
   print(time.power.on.first)
   
   
   # in the evening: 
   # grab last dT value before power-off at 20:55 hrs
   dT.power.on.last   <- dT[hour.vector < hour.off]
   time.power.on.last <- time[hour.vector < hour.off]
   dT.power.on.last   <- dT.power.on.last[length(dT.power.on.last)] 
   time.power.on.last <- time.power.on.last[length(time.power.on.last)]
   #print(time.power.on.last)
   
   # grab first dT value before power-off at 21:10 hrs
   dT.power.off.first <- dT[hour.vector >= hour.off]
   dT.power.off.first <- dT.power.off.first[3]
   
   out <- data.frame(Date = date,
                     dT.power.off.last  = dT.power.off.last,
                     dT.power.on.first  = dT.power.on.first,
                     dT.power.on.last   = dT.power.on.last,
                     dT.power.off.first = dT.power.off.first
                     )
}

# grab last dT value before power-off
#   my.dT <- dT[data$hour.vector < hour.off &
#               data$date == day1]
#   last.dT <- my.dT[length(my.dT)] 
