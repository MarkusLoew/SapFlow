# analyse sap flow in context of calibrated humidity and air-temperature.
# based on script "Sap_flow_2015_visualisation_analysis.R"

library(MySensorCorrect)
library(ggplot2)
library(lubridate)
library(plyr)
library(nlme)
library(ggjoy)
library(predictmeans)

setwd("~/AgFace/Topics/Sap_flow")

# set theme for figures
# set the ggplot2 theme for the whole session
theme_set(theme_bw())
theme_replace(strip.background = element_rect(fill = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.key       = element_blank())

# load sap flow data
load("Sap_flow_2015_visualisation_analysis_result.RData")

# load 5 min above-canopy weather data for re-calculation of
# in-canopy sensors
load("~/AgFace/2015/Weather/5Min_weather_2015_wide.RData")

w <- Five.Min.weather.2015 # just a rename
rm(Five.Min.weather.2015)

## ++++++++++++++++++++++++++++++++++++++++++++++++++++
## merge selected microclimate parameters with sap flow
## ++++++++++++++++++++++++++++++++++++++++++++++++++++

# what to keep from above-crown weather?
to.keep <- c("DateTime", 
             "Ave.AirTemp..degC.", 
             "Ave.GSR..W.m.2.", 
             "Ave.VPD..kPa.",
             "Ave.Humidity....")

w <- w[, names(w) %in% to.keep]
names(w) <- c("DateTime", "AirTemp_above", "Hum_above", "GSR_above", "VPD_above")

# correct DateTime to be in GMT
# attr(w$DateTime, "tzone") <- "GMT"  # changes actual time
w$DateTime <- force_tz(w$DateTime, tzone = "GMT")

df.cast <- merge(df.cast, w,
                  by.x = "TIMESTAMP",
                  by.y = "DateTime",
                  all.x = TRUE,
                  all.y = FALSE)

#p <- ggplot(df.cast, aes(x = TIMESTAMP, y = VPD_above))
#  p <- p + geom_line()
#p


## ++++++++++++++++++++++++++++++++++++++++++++++++++++
## re-calculate in-canopy air-temperature
## ++++++++++++++++++++++++++++++++++++++++++++++++++++

# needs global radiation

df.cast$Temp_Avg.corr <- TempSensCorrect(df.cast$Temp_Avg,
                                         glob.rad = df.cast$GSR_above)
# [df.cast$GSR_above > 100, ]
p <- ggplot(df.cast, aes(x = TIMESTAMP))
  p <- p + geom_line(aes(y = GSR_above / 50), colour = "green")
  p <- p + geom_line(aes(y = Temp_Avg, linetype = SensorID), colour = "blue")
  p <- p + geom_line(aes(y = Temp_Avg.corr, linetype = SensorID), colour = "red")
  p <- p + facet_grid(SYSTEM ~ .)
 # p <- p + coord_cartesian(xlim = c(as.POSIXct("2015-10-25"), as.POSIXct("2015-11-10")))
p

## ++++++++++++++++++++++++++++++++++++++++++++++++++++
## re-calculate in-canopy relative humidity
## ++++++++++++++++++++++++++++++++++++++++++++++++++++

df.cast$Hum_Avg.corr <- HumSensCorrect(df.cast$Hum_Avg)

p <- ggplot(df.cast, aes(x = TIMESTAMP))
  p <- p + geom_line(aes(y = Hum_above), colour = "green")
  #p <- p + geom_line(aes(y = Hum_Avg, linetype = SensorID), colour = "blue")
  p <- p + geom_line(aes(y = Hum_Avg.corr, linetype = SensorID), colour = "red")
  p <- p + facet_grid(SYSTEM ~ .)
  p <- p + coord_cartesian(xlim = c(as.POSIXct("2015-10-25"), as.POSIXct("2015-11-10")))
p

## ++++++++++++++++++++++++++++++++++++++++++++++++++++
## re-calculate in-canopy vapour pressure deficit VPD
## ++++++++++++++++++++++++++++++++++++++++++++++++++++

# calculate VPD
# now based on re-calculated humidity and temperature
# from Abtew, Melesse (2013) Evaporation and Evapotranspiration
# chapter 5.2.1
# es = 0.611 * exp((17.27 * T) / (T + 237.3))
# VPD = es * (1 - (RH/100))

df.cast$es <- with(df.cast, 
                   0.611 * exp((17.27 * Temp_Avg.corr) / (Temp_Avg.corr + 237.3)))
df.cast$VPD_Avg.corr <- with(df.cast, 
                             es * (1 - (Hum_Avg.corr / 100)))

p <- ggplot(df.cast, aes(x = TIMESTAMP))
  p <- p + geom_line(aes(y = VPD_above), colour = "green")
  p <- p + geom_line(aes(y = VPD_Avg.corr, linetype = SensorID), colour = "red")
  p <- p + facet_grid(SYSTEM ~ .)
  p <- p + coord_cartesian(xlim = c(as.POSIXct("2015-10-25"), as.POSIXct("2015-11-10")))
p


# ++++++++++++++++++++++++++++++++++++++++++++++++
# for comparison - when is the maximum of VPD?
# ++++++++++++++++++++++++++++++++++++++++++++++++
max.VPD <- ddply(df.cast,
                  .(Date, Ring, Cultivar, CO2_treatment, SensorID),
                  function(x) FindMax(x, VPD_Avg.corr))
attr(max.VPD$V1, "tzone") <- "GMT"

fig.max.flow + geom_vline(aes(xintercept = as.numeric(V1), colour = SensorID),
                          linetype = "dashed", 
                      data = max.VPD)

# frequency of peak times
# bin data to 30min windows first?
max.VPD$hour   <- as.numeric(format(max.VPD$V1, "%H"))
max.VPD$minute <- as.numeric(format(max.VPD$V1, "%M"))

max.VPD$bin.min <- floor(max.VPD$minute / 30) * 30
max.VPD$hourmin <- paste(max.VPD$hour, max.flow$bin.min, sep = ":")
max.VPD$hourmin <- as.POSIXct(max.VPD$hourmin, format = "%H:%M")

# to estimate the probability density function of a random variable
# probability density function PDF is used to specify the probability of the random variable falling within a particular range of values, as opposed to taking on any one value

p <- ggplot(max.VPD[max.VPD$hour > 0, ], 
            aes(x = hourmin))
  #p <- p + geom_histogram()
  p <- p + geom_density(aes(fill = CO2_treatment, linetype = Cultivar), alpha = 0.3)
  #p <- p + facet_grid(CO2_treatment ~ .)
p

p <- ggplot(max.VPD[max.VPD$hour != 0, ], aes(x = hourmin, y = CO2_treatment))
  p <- p + geom_joy(aes(fill = Cultivar))
  p <- p + facet_grid(Cultivar ~ .)
p

p <- ggplot(max.VPD[max.VPD$hour != 0, ], aes(x = Cultivar, y = hourmin))
  p <- p + geom_boxplot(aes(fill = CO2_treatment))
p

my.lme <- lme(hourmin ~ CO2_treatment * Cultivar,
              random = ~ 1 | Date/Ring,
              na.action = na.omit,
              data = max.VPD[max.VPD$hour != 0, ])
              #control = lmeControl(opt = "optim"))
anova(my.lme)
# ==> no CO2 or cultivar related differences on time of max VPD.

## ++++++++++++++++++++++++++++++++++++++++++++++++++++
## is there a CO2-related difference in VPD?
## this data is limited to the time when sap flow
## was measured
## analyse VPD and microclimate in script
## Microclimate_2015.R instead!
## ++++++++++++++++++++++++++++++++++++++++++++++++++++

# using mid-day data
mid.day <- df.cast[df.cast$Hour >= 10 &
                   df.cast$Hour <= 15, ]

mid.day.mean <- ddply(mid.day,
                     .(Date, Ring, CO2_treatment, Cultivar),
                     summarise,
                     VPD.mean = mean(VPD_Avg.corr, na.rm = TRUE),
                     VPD.sd = sd(VPD_Avg.corr, na.rm = TRUE),
                     Temp.mean = mean(Temp_Avg.corr, na.rm = TRUE),
                     Temp.sd = sd(Temp_Avg.corr, na.rm = TRUE),
                     Hum.mean = mean(Hum_Avg.corr, na.rm = TRUE),
                     Hum.sd = sd(Hum_Avg.corr, na.rm = TRUE))

mid.day.mean$period <- "no test"
mid.day.mean$period[mid.day.mean$Date < as.Date("2015-11-01")] <- "test"

p <- ggplot(mid.day.mean[mid.day.mean$period == "test", ], 
            aes(x = Cultivar, y = Temp.mean))
  p <- p + geom_boxplot(aes(colour = CO2_treatment))
  #p <- p + facet_grid(Week ~ .)
p

p <- ggplot(mid.day.mean, aes(x = Date, y = VPD.mean))
  p <- p + stat_summary(aes(colour = CO2_treatment),
                        fun.data = "mean_sdl", 
                        fun.args = list(mult = 1),
                        geom = "line")
  p <- p + stat_summary(aes(colour = CO2_treatment),
                        fun.data = "mean_sdl", 
                        fun.args = list(mult = 1))
  p <- p + facet_grid(Cultivar ~ .)
p

# statistical test for mean VPD for period before Oct 29
my.lme <- lme(VPD.mean ~ CO2_treatment * Cultivar,
              random = ~ 1 | Date / Ring,
              na.action = na.omit,
              data = mid.day.mean[mid.day.mean$Date < as.Date("2015-10-29"), ])
anova(my.lme)
# ==> interaction found CO2 x Cultivar, no main effect.

# statistical test for mean VPD for period before November 1
my.lme <- lme(VPD.mean ~ CO2_treatment * Cultivar,
              random = ~ 1 | Date / Ring,
              na.action = na.omit,
              data = mid.day.mean[mid.day.mean$Date < as.Date("2015-11-01"), ])
anova(my.lme)
# main effect of Cultivar

predictmeans(my.lme, "CO2_treatment", newwd = FALSE)
predictmeans(my.lme, "Cultivar", newwd = FALSE)
# ==> lower VPD for Yitpi compared to Scout. Yitpi with higher stomatal conductance and less WUE?

#ddply(mid.day.mean[mid.day.mean$Date < as.Date("2015-11-01"), ],
#      .(Cultivar),
#      summarise,
#      my.sd = sd(VPD.mean, na.rm = TRUE))


# +++++++++++++++++++++++++++++++++++++++++++++++++
# Chen et al 2014: 
# Thus "variable of transpiration" (VT), which is an integrated index, was calculated
# from VPD and R s as follows (Du et al., 2011; :
# VT = VPD(Rs)^0.5
# Rs = solar radiation.
# parameter is rarely used (about 5 papers only) and 
# only from the same team of authors
# +++++++++++++++++++++++++++++++++++++++++++++++++
df.cast$VT <- with(df.cast, VPD_Avg.corr * (GSR_above^0.5))



# +++++++++++++++++++++++++++++++++++++++++++++++==
# sap flow response to VPD
# +++++++++++++++++++++++++++++++++++++++++++++++==

# re-doing the 30 min aggregation, as the existing data frame does not
# include the corrected VPD, humidity, temperature.
df.cast$min30 <- floor(df.cast$Minute / 30) * 30

df.cast.30min <- ddply(df.cast,
                       .(Date, Day, Hour, min30, CO2_treatment, SYSTEM, Ring, 
                         Cultivar, PlantID, SensorID),
                       colwise(mean, na.rm = TRUE))

p <- ggplot(df.cast.30min[df.cast.30min$my.Jw.rel > 0 &
                          !is.na(df.cast.30min$my.Jw.rel) &
                    df.cast.30min$TIMESTAMP < as.POSIXct("2015-11-10") &
                    !is.na(df.cast.30min$my.Jw.rel) &
                    df.cast.30min$VPD_Avg.corr > 0.6 &
                    !is.na(df.cast.30min$VPD_Avg.corr), ], # VPD rule after Chen et al 2014
            aes(x = VPD_Avg.corr, y = my.Jw.rel))
  p <- p + geom_point(aes(colour = CO2_treatment), alpha = 0.1)
  p <- p + geom_smooth(method = "lm",
                       aes(colour = CO2_treatment))
p

# sap flow versus variable of transpiration VT
# in the morning hours
p <- ggplot(df.cast.30min[df.cast.30min$my.Jw.rel > 0 &
                          !is.na(df.cast.30min$my.Jw.rel) &
                          (df.cast.30min$Hour > 9 |
                          df.cast.30min$Hour < 11) &
                          (df.cast.30min$TIMESTAMP >= zoom.start |
                           df.cast.30min$TIMESTAMP <= zoom.end), ],
            aes(y = my.Jw.rel, x = GSR_above))
  p <- p + geom_point(aes(colour = CO2_treatment))
  p <- p + facet_grid(Day ~ PlantID)
p

# response to global radiation
GSRFig <- function(x, x.para = "GSR_above", y.para = "my.Jw.rel") {
  the.day <- unique(x$Day)
  the.plant <- unique(x$PlantID)

   x <- x[(x$Hour > 6 &
           x$Hour < 14), ]
   x <- x[x$my.Jw.rel > 0 &
          !is.na(x$my.Jw.rel), ]
   x <- x[x$GSR_above > 0, ] 
  
  p <- ggplot(x, aes_string(x = x.para, y = y.para))
    p <- p + geom_point() + geom_line()
    p <- p + facet_grid(Day ~ PlantID)
    p <- p + labs(title = paste(the.plant, the.day, sep = " "))
    p <- p + theme(strip.text.y = element_text(angle = 0))
  return(p)
}

GSRfig.list <- dlply(df.cast.30min,
                     .(PlantID),
                     function(x) GSRFig(x))
#pdf(file = "Jw_GSR.pdf")
#  print(GSRfig.list)
#dev.off()

# good GSR - Jw response dates: 2015-11-02
GSRtime.list <- dlply(df.cast.30min,
                     .(Day),
                     function(x) GSRFig(x, x.para = "TIMESTAMP", y.para = "GSR_above"))

pdf(file = "Glob_rad_vs_time.pdf")
  print(GSRtime.list)
dev.off()
# good GSR - Jw response dates: 2015-11-02?

VT.list <- dlply(df.cast.30min,
                     .(Day),
                     function(x) GSRFig(x, x.para = "VT", y.para = "my.Jw.rel"))

#pdf(file = "Jw_vs_VT.pdf")
#  print(VT.list)
#dev.off()


