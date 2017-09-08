# Ksh zero calculation
# 
# calculate Ksh_zero based on Power and vertical flow at pre-dawn
# Ksh default set to 0.4 as, empirically, the range in the 2015 was between 0.25 and 0.55
# Ksh default recommended by Dynagage is 0.8, but probably for trees.

Kshzero <- function(data,
                    ksh = Kshapp_Avg, 
                    hour.off = 6, 
                    hour.on = 4,
                    ksh.default = 0.4) {
   
   # only works on single days
   stopifnot(length(unique(data$Date)) == 1)
   
   # cut the data down to the zero ksh period
   data <- data[data$Hour >= hour.on &
                data$Hour <= hour.off, ]
   
   # grab the correct vectors
   arguments <- as.list(match.call())
   ksh <- eval(arguments$ksh, data)
   
   Kshzero <- mean(ksh, na.rm = TRUE)
   
   if (is.na(Kshzero)) {
      Kshzero <- ksh.default
      }
   
   if (Kshzero < 0 | Kshzero > 10) {
      Kshzero <- ksh.default
      }
   
   out <- data.frame(Kshzero = Kshzero)
   return(out)
}

