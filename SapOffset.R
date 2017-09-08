# Sap flow offset function
# 
# grabs the dT values before and after each power-on/power-off event
# this function is applied to a vector that is pre-split per day
# data has to be sorted by date!

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
   
   # show the current sample name
   print(unique(as.character(interaction(data$Date, data$SYSTEM, data$SensorID))))
   
   # make sure that the sample actually has values for dT!
   sample.length <- length(dT)
   missing.sample.length <- sum(is.na(dT))
   
   # there must be a dT value at power-on and power-off times
   dT.at.power.on <- dT[hour.vector == hour.on]
   dT.at.power.on <- dT.at.power.on[1]
   
   dT.at.power.off <- dT[hour.vector == hour.off]
   dT.at.power.off <- dT.at.power.off[1]
   
   # set up dummy data frame for cases that can't be processed
   fail.df <- data.frame(Date = date,
           dT.power.off.last  = NA,
           dT.power.on.first  = NA,
           morning.offset     = NA,
           dT.power.on.last   = NA,
           dT.power.off.first = NA,
           evening.offset     = NA)

  fail.df$mean.power.offset <- NA
  
  # at the start I assume data are good
  data.good <- TRUE
  
  # test if data are good  
   if (sample.length - missing.sample.length == 0) {
      print("no samples, or power-on/off times")
      data.good <- FALSE
      return(fail.df)
   } 
   
   if (is.na(dT.at.power.on) == TRUE) {
      print("no power-on data")
      data.good <- FALSE
      return(fail.df)
   }
   
   if (is.na(dT.at.power.off) == TRUE) {
      print("no power-off data")
      data.good <- FALSE
      return(fail.df)
   }
   
#   if (power.on.found == FALSE & power.off.found == FALSE) {
#      print("Power-on or power-off event not present in data")
#      data.good <- FALSE
#      return(fail.df)
#   }
   print(data.good)
   
   # only do this if the test above give the go-ahead   
   if (data.good == TRUE) {
   
   # print(sum(is.na(dT)))
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
   # print(time.power.on.first)
   
   
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
       morning.offset     = dT.power.off.last - dT.power.on.first,
       dT.power.on.last   = dT.power.on.last,
       dT.power.off.first = dT.power.off.first,
     evening.offset       = dT.power.off.first - dT.power.on.last)

   out$mean.power.offset <- mean(out$morning.offset, out$evening.offset)
   return(out)}
}

