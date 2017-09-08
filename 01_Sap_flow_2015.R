# using the CampbellLogger library to import files
# this script loads the in-season data 2015

library(CampbellLogger)
library(nlme)
library(doMC)
library(ggplot2)
library(plyr)
library(cowplot)

# set up cluster for parallel computing
registerDoMC(4)

#cl <- makeCluster(4)
#registerDoParallel(cl)

# set theme for figures
# set the ggplot2 theme for the whole session
theme_set(theme_bw())
theme_replace(strip.background = element_rect(fill = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.key       = element_blank())

# Figure labels
CO2.label  <- expression(textstyle(CO[2]~treatment))
CO2.treats <- c(expression(textstyle(aCO[2])), 
                expression(textstyle(eCO[2])))
CO2_lab <- expression(CO[2]~treatment)

# key dates
DC65.date <- as.Date("2015-10-09") # TraitFACE wet (rainfed plots on Oct-7)
# NFACE sampling and additional measurements on TraitFACE plots on Oct-13 to Oct-15
DC90.date <- as.Date("2015-11-23") # wet plots only, rainfed was on 2015-11-18

my.days <- seq(from = DC65.date, to = DC90.date, by = "day")
my.growthstage <- seq(from = 65, to = 90, length.out = length(my.days))

# date and growth stage data frame
date.gs <- data.frame(Date = my.days,
                      Growthstage = my.growthstage)


# start of sap flow experiment
sap.flow.start <- as.POSIXct("2015-10-06", tz = "GMT")

# more sensors came online on Oct20!

# date of stem-cut
stem.cut.date    <- as.POSIXct("2015-11-20 09:00:00", tz = "GMT")
put.back.up.date <- as.POSIXct("2015-11-24 16:00:00", tz = "GMT")

setwd("~/AgFace/2015/Campbell_logger/Transmissions")

load("5min_In-season2015_sap.RData")
load("5min_In-season2015_cast_sap.RData")
#load("5Min_in_season_with_calib_PAR.RData")

setwd("~/AgFace/Topics/Sap_flow")
source("SapZeroKsh.R")


# cut the data to the time of sap-flow operation
df <- df[df$TIMESTAMP >= sap.flow.start, ]
df.cast <- df.cast[df.cast$TIMESTAMP >= sap.flow.start, ]

# df.cast$Date <- as.Date(format(df.cast$TIMESTAMP, "%F"))
df.cast <- df.cast[order(df.cast$TIMESTAMP, df.cast$SYSTEM, df.cast$SensorID), ]

#p <- ggplot(df.cast, aes(x = TIMESTAMP, y = dT_Avg))
#  p <- p + geom_line(aes(colour = SensorID))
#  p <- p + geom_vline(xintercept = as.numeric(c(stem.cut.date, put.back.up.date)), 
#                      colour ="orange")
#  p <- p + facet_grid(SYSTEM ~ .)
#p

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# import leaf area data
la <- read.csv("~/AgFace/2015/Sapflow/Leaf_areas.csv")
la$Date <- as.Date(la$Date)

# add SYSTEM information
la$SYSTEM <- paste0("SYS", la$SYSTEM)
la$SYSTEM <- as.factor(la$SYSTEM)

# calculate leaf area after
# Miralles, Daniel J., and Gustavo A. Slafer. 1991. ‘A Simple Model for Non-Destructive Estimates of Leaf Area in Wheat’. Cereal Research Communications, 439–444.
# area = length * width * 0.835
la$Area_cm2 <- with(la, Length_cm * Width_cm * 0.835)

names(la) <- gsub("Sapflow_sensor", "SensorID", names(la))

# calculate sum of flag leaf and second leaf
la.tot <- ddply(la,
              .(Year, Date, SYSTEM, SensorID, Always_same_plant),
              summarise,
              Total_leaf_area = sum(Area_cm2, na.rm = TRUE))

# merge leaf length and width with the sap flow data
la.tot.cont <- la.tot[la.tot$Always_same_plant == "yes", ]
la.tot.switch <- la.tot[la.tot$Always_same_plant == "no", ]

# first step, add leaf area to df.cast for the time before plant switching
# on 2015-11-02 some plants were switched

# part of the data before any plants were replaced
df.cast.before <- df.cast[as.Date(df.cast$Day) < max(la.tot$Date), ]

# merge with the leaf area data before Nov 2
df.cast.before <- merge(df.cast.before, la.tot.cont)

# part of the data where the plants were replaced
#df.cast.cont <- df.cast[as.Date(df.cast$Day) < max(la.tot$Date), ]
df.cast.switch <- df.cast[as.Date(df.cast$Day) >= max(la.tot$Date) &
                          (df.cast$SYSTEM %in% c("SYS6", "SYS7") &
                           df.cast$SensorID == 1), ]
df.cast.switch <- merge(df.cast.switch, la.tot.switch,
                        all.x = TRUE)

# part of the data after the replacement date, but the plants were not replaced
df.cast.no.switch <- df.cast[as.Date(df.cast$Day) >= max(la.tot$Date) &
                          !(df.cast$SYSTEM %in% c("SYS6", "SYS7") &
                            df.cast$SensorID == 1), ]
df.cast.no.switch <- merge(df.cast.no.switch, la.tot.cont,
                           all.x = TRUE)
#df.cast.cont <- merge(df.cast, la.tot.cont)


df.cast.orig <- df.cast
df.cast <- rbind(df.cast.before, df.cast.no.switch, df.cast.switch)
rm(df.cast.no.switch) # get rid of this rather large object

df.cast$Date <- as.Date(format(df.cast$TIMESTAMP, "%F"))

# calculate relative sap flow
# SFr = Sapflow/leaf area
# df.cast$Jw_Langensiepen <- df.cast$dT_Avg * 0.46
df.cast$Jw.rel <- with(df.cast, Jw_Langensiepen / Total_leaf_area)

# get rid of data before Oct 20
# there are only three sensors available before that date: only scout, only aCO2
# no stats possible here - removed
# also, mean Jw is rather low as these plants only had the flag leaf above the sensor 
# whereas after Oct 20, the leaf area increased by mounting the sensors closer to the ground
df.cast <- df.cast[df.cast$TIMESTAMP >= as.POSIXct("2015-10-20", tz = "GMT"), ]
# df.cast <- df.cast[df.cast$TIMESTAMP < stem.cut.date, ]
df.cast <- df.cast[!is.na(df.cast$Cultivar), ]

# figure
p <- ggplot(df.cast, aes(x = TIMESTAMP, y = Jw.rel))
  p <- p + geom_line(aes(colour = SensorID))
  p <- p + geom_vline(xintercept = as.numeric(c(stem.cut.date, put.back.up.date)), 
                      colour ="orange")
  p <- p + facet_grid(SYSTEM ~ .)
p
fig.sap.flow.timecourse <- p 
rm(fig.sap.flow.timecourse)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++==
#     use global radiation to re-calculate
#             air temperature
# based on calibration against MEA weather 
# +++++++++++++++++++++++++++++++++++++++++++++++++++++==






# +++++++++++++++++++++++++++++++++++++++++++++++++++++==
# re-calculate sap flow and compare with logger-calculations
# +++++++++++++++++++++++++++++++++++++++++++++++++++++==

# calculate sap flow traditionally
# Qv=Kst*A*(Bh-Ah)/dX/0.040/10
# stem diameter 3.3 or 3.4 mm, average 3.35 mm
distance <- 10 # mm
area_cm <- (pi * (3.35/2)^2) # cm
kst <- 0.28 # W m-1 K-1
Cp <- 4.186 # J g-1 K-1, specific heat of water
# myQv <- with(df.cast, kst * area_cm * (RawBh_Avg - RawAh_Avg) / distance / 0.040 / 10)

# for sap flow on wheat, Qv is set to 0 
# because the SGA2 sensors only have on pair of thermocouples!
myQv <- 0

df.cast$myQv <- myQv

# KshApp = (Pin-Qv)/Ch
myKsh <- with(df.cast, (Pin_Avg - myQv) / RawCh_Avg)
df.cast$myKsh <- myKsh 

# compare myKsh with Kshapp from logger

p <- ggplot(df.cast, aes(x = myKsh, y = Kshapp_Avg))
  p <- p + geom_point()
  p <- p + scale_y_continuous(limits = c(0, 1))
  p <- p + scale_x_continuous(limits = c(0, 1))
p

# ok, it's fairly linear, despite using 0 as Qv

p <- ggplot(df.cast[df.cast$Hour >= 4 &
                    df.cast$Hour <= 6 &
                    df.cast$Date == as.Date("2015-10-27"),], 
            aes(x = TIMESTAMP, y = myKsh))
  p <- p + geom_line(aes(colour = SensorID))
  p <- p + facet_grid(SYSTEM ~ .)
  p <- p + scale_y_continuous(limits = c(0.25, 0.55))
p

# at pre-dawn, Ksh is between 0.25 and 0.55

# calculate zero Ksh for each day, sensor and SYSTEM
myk <- ddply(df.cast, 
             .(Date, SYSTEM, SensorID),
             function(x) Kshzero(x, ksh = myKsh, hour.on = 5))

df.cast <- merge(df.cast, myk)


# check the Kshzero each day
p <- ggplot(myk, aes(x = Date, y = Kshzero))
  p <- p + geom_line(aes(colour = SensorID))
  #p <- p + coord_cartesian(ylim = c(0.1, 0.8))
  p <- p + facet_grid(SYSTEM ~ .)
p

# Qr (W) = Ksh * Ch, using the zero Ksh for each day
myQr <- with(df.cast, Kshzero * RawCh_Avg)
df.cast$myQr <- myQr

p <- ggplot(df.cast[df.cast$Date == as.Date("2015-10-27"), ], 
            aes(x = TIMESTAMP))
  p <- p + geom_line(aes(y = myQr, colour = SensorID))
  p <- p + geom_line(aes(y = Qr_Avg, colour = SensorID), linetype = "dashed")
  p <- p + facet_grid(SYSTEM ~ .)
  #p <- p + scale_y_continuous(limits = c(0.25, 0.55))
p

# Qf=Pin-Qv-Qr
myQf <- with(df.cast, Pin_Avg - myQv - myQr)
df.cast$myQf <- myQf

p <- ggplot(df.cast[df.cast$TOD == "Daytime", ],
            #[df.cast$Date == as.Date("2015-10-27"), ], 
            aes(x = TIMESTAMP, y = myQf))
  p <- p + geom_line(aes(colour = SensorID))
  p <- p + facet_grid(SYSTEM ~ .)
  p <- p + scale_y_continuous(limits = c(0, 1))
p

# dT=(Ah+Bh)/2/0.040
mydT <- with(df.cast, (RawAh_Avg + RawBh_Avg) / 2 / 0.040)
df.cast$mydT <- mydT

p <- ggplot(df.cast[df.cast$TOD == "Daytime" &
            df.cast$Date == as.Date("2015-10-27"), ], 
            aes(x = TIMESTAMP))
  p <- p + geom_line(aes(y = mydT, colour = SensorID))
  p <- p + geom_line(aes(y = dT_Avg, colour = SensorID), linetype = "dashed")
  p <- p + facet_grid(SYSTEM ~ .)
  #p <- p + scale_y_continuous(limits = c(0.25, 0.55))
p

# Flow=Qf/Cp/dT
myFlow <- with(df.cast, myQf / Cp / mydT) * 3600
df.cast$myFlow <- myFlow

p <- ggplot(df.cast[df.cast$SYSTEM == "SYS2" &
                     df.cast$Date == as.Date("2015-11-15"), ],
             aes(x = TIMESTAMP))
  p <- p + geom_point(aes(y = Sapflow_Avg, colour = SensorID))
  p <- p + geom_line(aes(y = myFlow, colour = SensorID))
  #p <- p + coord_cartesian(ylim = c(-0.1, 5))
  p <- p + scale_y_continuous(limits = c(-0.1, 5))
  p <- p + facet_grid(SYSTEM ~ .)
p

# replace all logger-calculated elements with the self-calculated equivalents
df.cast.orig   <- df.cast
df.cast$Qr_Avg <- df.cast$myQr
df.cast$Qv_Avg <- df.cast$myQv
df.cast$Qf_Avg <- df.cast$myQf
df.cast$dT_Avg <- df.cast$mydT
df.cast$Kshapp_Avg  <- df.cast$myKsh
df.cast$Sapflow_Avg <- df.cast$myFlow
df.cast$my.sapflow  <- NULL

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Temperature correction of dT
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# a look at the week before and after the stem was cut
# several days before stem was cut
seven.days.before.cut <- stem.cut.date - ((24 * 60 * 60) * 7)

p <- fig.sap.flow.timecourse
p <- p + coord_cartesian(xlim = c(seven.days.before.cut, max(df.cast$TIMESTAMP)))
p 

# the time when the cut-off stems were back in a vertical postion 
# is probably more interesting as a control

# get temperature response during that time
p <- ggplot(df.cast, aes(x = TIMESTAMP, y = dT_Avg))
  p <- p + geom_line(aes(colour = SensorID))
  p <- p + geom_vline(xintercept = as.numeric(c(stem.cut.date, put.back.up.date)), 
                      colour ="orange")
  p <- p + facet_grid(SYSTEM ~ .)
  p <- p + coord_cartesian(xlim = c(seven.days.before.cut, max(df.cast$TIMESTAMP)))
p

# is there a usable temperature correlation?
# re-do Stem.intact
df.cast$Stem.position <- "upright"
df.cast$Stem.position[df.cast$TIMESTAMP >= stem.cut.date &
                      df.cast$TIMESTAMP <= put.back.up.date] <- "lying"
df.cast$Stem.position <- as.factor(df.cast$Stem.position)

p <- ggplot(df.cast[df.cast$TIMESTAMP >= seven.days.before.cut &
                    df.cast$TOD == "Daytime", ], 
            aes(Temp_Avg, y = dT_Avg))
  p <- p + geom_point(aes(colour =  Stem.intact), alpha = 0.1)
  p <- p + geom_smooth(method = "lm", aes(colour = Stem.intact))
  p <- p + facet_grid(Stem.position ~ Stem.intact)
p

summary(lm(dT_Avg ~ Temp_Avg,
           data = df.cast[df.cast$Stem.position == "upright" &
                              df.cast$Stem.intact == "Stem cut", ]))

# fit a curve to the cut-stem data
# Y = ab^((X − c)^2) Type V exponential function
# Y = -A(X − B)^2 + C Second degree polynomial: quadratic but inverted as -A
# Y = aX^b*c^X Type I combined exponential and power function
# Y = 1 / pi * ((0.5 * a) / ((x - x0)^2 + (0.5 * a)^2))  Lorentzian function
# Y = 1 - (x^2 / a^2) Welch
# Y = y0 + A * sqrt(2 / PI) / w * exp(-2 * ((x - xc) / w)^2) Gauss
nls.fit <- nls(dT_Avg ~ y0 + a * sqrt(2 / pi) / w * exp(-2 * ((Temp_Avg - xc) / w)^2),
               data = df.cast[df.cast$Stem.position == "upright" &
                              df.cast$Stem.intact == "Stem cut", ],
               start = c(a = -33,
                         w = -3,
                         xc = 6,
                         y0 =-1))
summary(nls.fit)

xValues <- seq(min(0), max(50), length.out = 100)  # create a sequence from 0 to the desired max x-value with 100 elements

# predict values from the nls.fit. 
# Predict expects a dataframe "in which to look for variables with which to predict". 
# Name has to match the nls model for the independent variable / predictor.
my.predicted <- predict(nls.fit, data.frame(Temp_Avg = xValues))
mod.pred <- data.frame(Temp_Avg = xValues,
                       dT_Avg = my.predicted)
p <- p + geom_line(data = mod.pred, colour = "blue")
p
# ===> curve fit not very meaningful for overall data

# bell-shaped curve?
# Bell shaped fit function
# bell shape fit only works on data with at least 12 hours
MyBellPlot <- function(data) {
  require(ggplot2)
  # print(unique(data$SensorID))
  sec.since.midnight <- as.numeric(data$TIMESTAMP - min(data$TIMESTAMP))
  sec.since.midnight.sd <- sd(sec.since.midnight)

  start.amp   <- 0.2 
  start.xmean <- mean(sec.since.midnight)
  start.xsd   <- sec.since.midnight.sd
  
  the.names <- c("Amp", "xmean", "xsd")
  
  nls.fit <- try(nls(dT_Avg ~ amp * exp(-0.5*((sec.since.midnight - xmean)/xsd)^2),
               data = data,
               start = c(amp = start.amp, xmean = start.xmean, xsd = start.xsd),
               nls.control(minFactor = 1/4096, maxiter = 400)))
  if(inherits(nls.fit, "try-error")) {
	  # return("No fit")
	  out <- rep(NA, 3)
	  names(out) <- the.names
	  return(out)
  } else {
	  #return(summary(nls.fit))}
	  out <- coef(nls.fit)
	  names(out) <- the.names
	  return(out)}
}

bell <- ddply(df.cast[df.cast$Stem.position == "upright" &
                      df.cast$TOD == "Daytime" &
                      df.cast$TIMESTAMP > put.back.up.date, ],
             .(Date, SYSTEM, SensorID),
             function(x) MyBellPlot(x))

bell.mean <- sapply(bell[, 4:6], mean, na.rm = TRUE)

bell.shape <- data.frame(seconds = 1: 24 * 60 * 60)
bell.shape$dT.bell <- bell.mean[1] * exp(-0.5 * ((bell.shape$seconds - bell.mean[2]) / bell.mean[3])^2)

ggplot(bell.shape, aes(x = seconds, y = dT.bell)) + geom_line()

# different response at different times of the day?
p <- ggplot(df.cast[df.cast$TIMESTAMP >= seven.days.before.cut &
                    df.cast$TOD == "Daytime" &
                    df.cast$Stem.position == "upright", ], 
            aes(Temp_Avg, y = dT_Avg))
  p <- p + geom_point(aes(colour =  Stem.intact))
  p <- p + geom_smooth(method = "lm")
  p <- p + facet_grid(Stem.intact ~ Hour)
  p <- p + coord_cartesian(ylim = c(-2.5, 4))
  p <- p + labs(title = "Relationship temperature difference in sap vs air temperature for live and cut stems",
                subtitle = "cutting the stem changes relationship between dT and air temperature",
                y = expression(paste(Delta, T~"["*degree*C*"]")),
                x = expression("Air temperature"~"["*degree*C*"]"),
                colour = "Stem",
                caption = "Shown for individual daytime (7 - 19) hours. 30 days of data for live stems (16 plants), six days of data for cut stems on eight plants.")
  p <- p + theme(legend.position = c(0.89, 0.91),
                 legend.background = element_rect(fill = "transparent"),
                 legend.text = element_text(size = rel(0.6)),
                 panel.grid.major.y = element_line(colour = "grey92"))
p
ggsave(file = "Cutting_stem_changes_dT_vs_temp_relationship.pdf",
       width = 11, height = 7)


# dT vs airtemp figure for individual days
MydTPlot <- function(data) {
   RingSensor <- unique(interaction(data$Date))
   p <- ggplot(data, 
            aes(Temp_Avg, y = dT_Avg))
   p <- p + geom_point(aes(colour =  Stem.intact))
   p <- p + geom_smooth(method = "lm")
   p <- p + facet_grid(Stem.intact ~ .)
   p <- p + coord_cartesian(ylim = c(-2.5, 4))
   p <- p + labs(title = RingSensor,
                 y = expression(paste(Delta, T~"["*degree*C*"]")),
                 x = expression("Air temperature"~"["*degree*C*"]"),
                colour = "Stem")
   p <- p + theme(legend.position = c(0.89, 0.91),
                  legend.background = element_rect(fill = "transparent"),
                  legend.text = element_text(size = rel(0.6)),
                  panel.grid.major.y = element_line(colour = "grey92"))
return(p)
}

# what's going on with the "Date" parameter? # has now been corrected above
# df.cast$Date <- as.Date(format(df.cast$TIMESTAMP, "%F"))

dtdaily <- dlply(df.cast[df.cast$TIMESTAMP >= seven.days.before.cut &
                         df.cast$TOD == "Daytime" &
                         df.cast$Stem.position == "upright", ],
                 .(Date),
                 function(x) MydTPlot(x))
pdf(file = "dT-airtemp_per_day.pdf",
    width = 9, height = 7)
print(dtdaily)
dev.off()

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# there seems to be a difference in the linear dT-airtemp relationship depending on the status of the stem!
# therefore, establishing a correction function for dT
# first run the regression per day, then see if they are different between days with cut/alive stem
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

MyLm <- function(data, response = dT_Avg, predictor = Temp_Avg) {
    arguments <- as.list(match.call())
    response <- eval(arguments$response, data)
    predictor <- eval(arguments$predictor, data)
    
    res.missing <- sum(is.na(response))
    pre.missing <- sum(is.na(predictor))
    
    if (res.missing == length(response &
        pre.missing == length(predictor))) {
        inter <- NA
        slope <- NA
        } else {
    my.lm <- try(lm(response ~ predictor, na.action = na.omit))}
    
    if (inherits(my.lm, "try-error")) {
       inter <- NA
       slope <- NA
    } else {
    inter <- coef(my.lm)[1]
    slope <- coef(my.lm)[2]}
    
    result <- data.frame(inter = inter,
                         slope = slope)
    return(result)
}

MyLm(df.cast, response = dT_Avg, predictor = Temp_Avg)

Linear.fit <- ddply(df.cast[df.cast$TIMESTAMP >= seven.days.before.cut &
                            #df.cast$TOD == "Daytime" &
                            df.cast$Stem.position == "upright" &
                            df.cast$Date < as.Date("2015-11-30"), ],
                    .(Date, Hour, Stem.position, Stem.intact),
                    function(x) {print(unique(x$Date))
                                 MyLm(x, response = dT_Avg, predictor = Temp_Avg)})
ggplot(Linear.fit, aes(x = Stem.intact, y = slope)) + geom_boxplot()
ggplot(Linear.fit, aes(x = Stem.intact, y = inter)) + geom_boxplot()

anova(lme(slope ~ Stem.intact, random = ~ 1 | Stem.intact, data = Linear.fit))

my.lme <- lme(slope ~ 1, random = ~1 | Stem.intact/Hour, data = Linear.fit)
anova(my.lme)
my.lme <- lme(inter ~ 1, random = ~1 | Stem.intact/Hour, data = Linear.fit)
anova(my.lme)

# issues regarding un-balanced data for anova?
# set anova options to type 3
options(contrasts = c("contr.sum","contr.poly"))
my.aov <- aov(slope ~ Stem.intact, data = Linear.fit) 
summary(my.aov)

library(car)
Anova(my.aov, type = "III")

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# there is enough indication that there is a biological difference in the dT to airtemp relationship
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ddply(Linear.fit,
      .(Stem.intact),
      summarise,
      mean.slope = mean(slope), sd.slope = sd(slope),
      mean.inter = mean(inter), sd.inter = sd(inter))
      
# function and coefficients for dT correction:
# dT ~ slope * airtemp + inter
cor.slope <- mean(Linear.fit$slope[Linear.fit$Stem.intact == "Stem cut"])
cor.inter <- mean(Linear.fit$inter[Linear.fit$Stem.intact == "Stem cut"])

# correction dT as function of Temp_Avg via subtraction
# dT(Temp_Avg) = dT_Avg - (cor.slope * Temp_Avg + cor.inter)
# only implementing the correction for positive dT
df.cast$dT.temp.corfac <- NA
df.cast$dT.temp.corfac[df.cast$Stem.intact == "Stem ok" &
                       df.cast$dT_Avg > 0 &
                       df.cast$TOD == "Daytime" &
                       !is.na(df.cast$dT_Avg)] <- with(df.cast[df.cast$Stem.intact == "Stem ok" &
                                                               df.cast$dT_Avg > 0 &
                                                               df.cast$TOD == "Daytime" &
                                                               !is.na(df.cast$dT_Avg), ], 
                                                       (cor.slope * Temp_Avg + cor.inter))
plot(dT.temp.corfac ~ dT_Avg, df.cast)

# get sunrise / sunset information
riseset <- CampbellSunriseSunset(df.cast)

short.start <- as.Date("2015-11-05")
short.end   <- as.Date("2015-11-08")

attr(riseset$sunset, "tzone") <- "GMT"
attr(riseset$sunrise, "tzone") <- "GMT"

riseset$sunrise <- riseset$sunrise + 11 * 60 * 60
riseset$sunset  <- riseset$sunset + 11 * 60 * 60

my.sunset <- riseset$sunset[1:length(riseset$sunrise) - 1]
my.sunrise <- riseset$sunrise[2:length(riseset$sunrise)]

sunriseset <- data.frame(sunset = my.sunset,
                         sunrise = my.sunrise)

example.start <- as.POSIXct("2015-10-29")
example.end <- as.POSIXct("2015-11-08")
p <- ggplot(df.cast[df.cast$TIMESTAMP >  example.start &
                    df.cast$TIMESTAMP < example.end &
                    df.cast$SYSTEM == "SYS3", ], 
                    aes(x = TIMESTAMP))
     p <- p + geom_hline(yintercept = 0, colour = "grey90")
     p <- p + annotate("rect", 
                        xmin = sunriseset$sunset,
                        xmax = sunriseset$sunrise, #as.POSIXct( "2015-11-05 06:23:00 GMT"),
                        ymin = -Inf, ymax = Inf, 
                        fill = "grey", alpha = 0.1)
     p <- p + geom_line(aes(y = dT_Avg, colour = SensorID))
     p <- p + geom_line(aes(y = dT.temp.corfac, colour = SensorID), linetype = "dotted")
     #p <- p + geom_line(aes(y = dT.tempcor, colour = SensorID), linetype = "dashed")
     p <- p + coord_cartesian(ylim = c(-0.7, 4), xlim = c(example.start, example.end))
     p <- p + labs(y = expression(paste(Delta, T~"["*degree*C*"]")))
     p <- p + facet_grid(SYSTEM ~ .)
p

# correct dT by subtracting the temperature correction factor
#df.cast$dT.temp.corrected <- df.cast$dT_Avg
#df.cast$dT.temp.corrected <- df.cast$dT_Avg - df.cast$dT.temp.corfac
#df.cast$dT.temp.corrected[df.cast$dT_Avg <= 0] <- df.cast$dT_Avg[df.cast$dT_Avg <= 0]

# correction dT as function of Temp_Avg via subtraction
# dT(Temp_Avg) = dT_Avg - (cor.slope * Temp_Avg + cor.inter)
# only implementing the correction for positive dT
df.cast$dT.temp.cor.factor <- NA
df.cast$dT.temp.cor.factor[df.cast$Stem.intact == "Stem ok" &
                           #df.cast$dT_Avg > 0 &
                           df.cast$TOD == "Daytime" &
                           !is.na(df.cast$dT_Avg)] <- with(df.cast[df.cast$Stem.intact == "Stem ok" &
                                                           #df.cast$dT_Avg > 0 &
                                                           df.cast$TOD == "Daytime" &
                                                           !is.na(df.cast$dT_Avg), ], 
                                                           (cor.slope * Temp_Avg + cor.inter))
df.cast$dT.temp.corrected <- NA
df.cast$dT.temp.corrected[df.cast$Stem.intact == "Stem ok" &
                          df.cast$TOD == "Daytime" &
                          #df.cast$dT_Avg > 0 &
                          !is.na(df.cast$dT_Avg)] <- with(df.cast[df.cast$Stem.intact == "Stem ok" &
                                                                  #df.cast$dT_Avg > 0 &
                                                                  df.cast$TOD == "Daytime" &
                                                                  !is.na(df.cast$dT_Avg), ],
                                                          dT_Avg - dT.temp.cor.factor)

# for a complet data set, adding the uncorrected dT for the nighttime
df.cast$dT.temp.corrected[df.cast$Stem.intact == "Stem ok" &
                          df.cast$TOD == "Nighttime" &
                          #df.cast$dT_Avg > 0 &
                          !is.na(df.cast$dT_Avg)] <- df.cast$dT_Avg[df.cast$Stem.intact == "Stem ok" &
                                                                    df.cast$TOD == "Nighttime" &
                                                                    #df.cast$dT_Avg > 0 &
                                                                   !is.na(df.cast$dT_Avg)]

p <- ggplot(df.cast[df.cast$TIMESTAMP >  example.start &
                    df.cast$TIMESTAMP < example.end & #, ],
                    df.cast$SYSTEM == "SYS3", ],
                    aes(x = TIMESTAMP))
     p <- p + geom_hline(yintercept = 0, colour = "grey90")
     p <- p + annotate("rect", 
                        xmin = sunriseset$sunset,
                        xmax = sunriseset$sunrise, #as.POSIXct( "2015-11-05 06:23:00 GMT"),
                        ymin = -Inf, ymax = Inf, 
                        fill = "grey", alpha = 0.1)
     p <- p + geom_line(aes(y = dT_Avg, colour = SensorID), linetype = "dashed")
     p <- p + geom_line(aes(y = dT.temp.cor.factor, colour = SensorID), linetype = "dotted")
     p <- p + geom_line(aes(y = dT.temp.corrected, colour = SensorID))
     p <- p + coord_cartesian(#ylim = c(-0.7, 4), 
                              xlim = c(example.start, example.end))
     p <- p + labs(y = expression(paste(Delta, T~"["*degree*C*"]")))
     p <- p + facet_grid(SYSTEM ~ .)
p


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# dT power-on / power-off offset
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# figure of a short timecourse

short.riseset <- riseset[riseset$Date >= as.POSIXct(short.start) - 2 * 24 * 60 * 60 &
                         riseset$Date <= as.POSIXct(short.end)   + 1 * 24 * 60 * 60, ]

# shift sunset and sunrise, so that start and end of each night are in the same row
my.sunset <- short.riseset$sunset[1:length(short.riseset$sunrise) - 1]
my.sunrise <- short.riseset$sunrise[2:length(short.riseset$sunrise)]

df.cast[df.cast$SYSTEM == "SYS3" &
        df.cast$Date >= short.start, ]

p <- ggplot(df.cast[df.cast$SYSTEM == "SYS3" &
                    #df.cast$SensorID == "2" &
                    df.cast$Date >= short.start &
                    df.cast$Date <= short.end, ], 
                    aes(x = TIMESTAMP))
  p <- p + geom_hline(yintercept = 0, colour = "grey90")
  p <- p + annotate("rect", 
                    xmin = my.sunset,
                    xmax = my.sunrise, #as.POSIXct( "2015-11-05 06:23:00 GMT"),
                    ymin = -Inf, ymax = Inf, 
                    fill = "grey", alpha = 0.1)
  p <- p + geom_line(aes(y = dT.temp.corrected, colour = SensorID))
  p <- p + geom_line(aes(y = dT_Avg, colour = SensorID), linetype = "dashed")
  p <- p + coord_cartesian(xlim = as.POSIXct(c(short.start, short.end)))
  p <- p + scale_x_datetime(date_breaks = "6 hours", date_labels = "%T")
  p <- p + labs(y = expression(paste(Delta, T~"["*degree*C*"]")))
  p <- p + facet_grid(SYSTEM ~ .)
p

ggsave("dT_power-on-off_jump_example.png",
       width = 9, height = 5)


# +++++++++++++++++++++++++++++
# Offset compensation
# +++++++++++++++++++++++++++++
# compensate for offsets between power-off and power-on in some sensors
# suggestion: set the value after power on the the last value before power on, 
# therefore removing any jumps at this time.
# example: SYS3, SensorID 1
# grab last power-off value.
# if first power-on is far above the power-off, calculate the offset and remove this offset from
# dT until power-off
small <- df.cast[df.cast$SYSTEM == "SYS3" &
                 #df.cast$SensorID == "2" &
                 df.cast$Date >= short.start &
                 df.cast$Date <= short.end, ]

# order data frame
small <- small[order(small$TIMESTAMP, small$SYSTEM, small$SensorID), ]

source("SapOffset.R")
dT.offset <- ddply(small,
                   .(Date, SYSTEM, SensorID),
                   function(x) {
                         SapOffset(data = x, 
                                     dT = x$dT_Avg,
                                   time = x$TIMESTAMP,
                                   date = x$Date,
                            hour.vector = x$Hour)})

mylines <- c(dT.offset[1, 4:7])
p + geom_hline(yintercept = as.numeric(mylines))


# recalculate dT based on the known power-on / power-off offset
# merge offset with data, then recalulate dT for power-on times
dT.offset.tiny <- dT.offset[, c("Date", "SYSTEM", "SensorID", "mean.power.offset")]

small <- merge(small, dT.offset.tiny)
small$Power <- FALSE
small$Power[small$Hour >= 4 &
            small$Hour < 21 ] <- TRUE

small$dT.offcor <- small$dT_Avg
small$dT.offcor[small$Power == TRUE] <- with(small[small$Power == TRUE, ], 
                                             dT_Avg + mean.power.offset)
 
# figure after correction                                            
p <- ggplot(small, aes(x = TIMESTAMP))
  p <- p + geom_hline(yintercept = 0, colour = "grey90")
  p <- p + annotate("rect", 
                    xmin = my.sunset,
                    xmax = my.sunrise, #as.POSIXct( "2015-11-05 06:23:00 GMT"),
                    ymin = -Inf, ymax = Inf, 
                    fill = "grey", alpha = 0.1)
  p <- p + geom_line(aes(y = dT.offcor, colour = SensorID))
  # p <- p + geom_line(aes(y = dT.cor, colour = SensorID), linetype = "dashed")
  p <- p + coord_cartesian(xlim = as.POSIXct(c(short.start, short.end)),
                           ylim = c(-0.2, 2))
  p <- p + scale_x_datetime(date_breaks = "6 hours", date_labels = "%T")
  p <- p + labs(y = expression(paste(Delta, T~"["*degree*C*"]")))
  p <- p + facet_grid(SYSTEM ~ .)
p

ggsave("dT_power-on-off_jump_example_after_correction.png",
       width = 9, height = 5)

# implement the offset correction for the whole sap flow data
# can this be done even for days when the sensors were re-installed?
# order data frame
df.cast <- df.cast[order(df.cast$TIMESTAMP, df.cast$SYSTEM, df.cast$SensorID), ]

#x <- df.cast[df.cast$SYSTEM == "SYS7" &
#             df.cast$SensorID == "1" &
#             df.cast$Date == as.Date("2015-10-22"), ]
# ggplot(x, aes(x = TIMESTAMP, y = dT_Avg)) + geom_line()

dT.offset <- ddply(df.cast,
                   .(Date, SYSTEM, SensorID),
                   function(x) {
                         SapOffset(data = x, 
                                     dT = x$dT_Avg,
                                   time = x$TIMESTAMP,
                                   date = x$Date,
                            hour.vector = x$Hour)})
# histogram of the offset
ggplot(dT.offset, aes(x = mean.power.offset)) + geom_histogram()


# recalculate dT based on the known power-on / power-off offset
# merge offset with data, then recalulate dT for power-on times
dT.offset.tiny <- dT.offset[, c("Date", "SYSTEM", "SensorID", "mean.power.offset")]

df.cast <- merge(df.cast, dT.offset.tiny)
df.cast$Power <- FALSE
df.cast$Power[df.cast$Hour >= 4 &
              df.cast$Hour < 21 ] <- TRUE

df.cast$dT.offcor <- df.cast$dT_Avg
df.cast$dT.offcor[df.cast$Power == TRUE] <- with(df.cast[df.cast$Power == TRUE, ], 
                                                 dT_Avg + mean.power.offset)

PlotTimecourse <- function(data, dT = "dT.offcor") {
   sun <- sunriseset[sunriseset$sunrise >= min(data$TIMESTAMP) &
                     sunriseset$sunset <= max(data$TIMESTAMP), ]

  p <- ggplot(data, aes(x = TIMESTAMP))
     p <- p + geom_hline(yintercept = 0, colour = "grey90")
     p <- p + annotate("rect", 
                        xmin = sun$sunset,
                        xmax = sun$sunrise, #as.POSIXct( "2015-11-05 06:23:00 GMT"),
                        ymin = -Inf, ymax = Inf, 
                        fill = "grey", alpha = 0.1)
     p <- p + geom_line(aes_string(y = dT, colour = "SensorID"))
     # p <- p + geom_line(aes(y = dT.cor, colour = SensorID), linetype = "dashed")
     p <- p + coord_cartesian(ylim = c(-0.7, 4))
     p <- p + labs(y = expression(paste(Delta, T~"["*degree*C*"]")))
     p <- p + facet_grid(SYSTEM ~ Stem.intact, scale = "free_x")
return(p)
}

# create a week index
df.cast$Week <- format(df.cast$TIMESTAMP, "%V")
WeeklydT <- dlply(df.cast,
                  .(Week, SYSTEM),
                  function(x) PlotTimecourse(x))
pdf(file = "Weekly_dT_per_SYSTEM.pdf",
    width = 9, height = 6)
    print(WeeklydT)
dev.off()

# remove the power-on / power-off spikes from the dT
# spikes are at 21:00 and 4:10 hours

df.cast$Minute <- as.numeric(format(df.cast$TIMESTAMP, "%M"))
df.cast$dT.offcor.good <- df.cast$dT.offcor
df.cast$dT.offcor.good[df.cast$Hour == 21 & 
                       df.cast$Minute <= 5 ] <- NA
df.cast$dT.offcor.good[df.cast$Hour == 4 & 
                       df.cast$Minute <= 5 ] <- NA

WeeklydT.good <- dlply(df.cast,
                      .(Week, SYSTEM),
                       function(x) PlotTimecourse(x, dT = "dT.offcor.good"))
pdf(file = "Weekly_dT_per_SYSTEM_no_spikes_at_power_events.pdf",
    width = 9, height = 6)
    print(WeeklydT.good)
dev.off()
rm(WeeklydT.good)

p <- ggplot(df.cast, aes(x = TIMESTAMP))
     p <- p + geom_hline(yintercept = 0, colour = "grey90")
     p <- p + annotate("rect", 
                        xmin = sunriseset$sunset,
                        xmax = sunriseset$sunrise, #as.POSIXct( "2015-11-05 06:23:00 GMT"),
                        ymin = -Inf, ymax = Inf, 
                        fill = "grey", alpha = 0.1)
     p <- p + geom_line(aes(y = dT.offcor.good, colour = SensorID))
     # p <- p + geom_line(aes(y = dT.cor, colour = SensorID), linetype = "dashed")
     p <- p + geom_vline(xintercept = as.numeric(c(stem.cut.date, put.back.up.date)), 
                      colour ="orange")
     p <- p + coord_cartesian(ylim = c(-0.7, 4))
     p <- p + labs(y = expression(paste(Delta, T~"["*degree*C*"]")))
     p <- p + facet_grid(SYSTEM ~ .)
p

df.cast <- df.cast[order(df.cast$TIMESTAMP, df.cast$SYSTEM, df.cast$SensorID), ]




# Jw = (Qh - Qv - Qr) / (Cw * dT)
# Flow Qf/4.186/dT*3600
#Jw_trad <- with(df.cast, (Pin_Avg - 0 - Qr_Avg) / (4.18 * dT.offcor.good))
Jw_trad <- with(df.cast, Qf_Avg /  Cp / dT.offcor.good * 3600)
df.cast$Jw_trad <- Jw_trad

p <- ggplot(df.cast[df.cast$TOD == "Daytime" &
            df.cast$Day == "2015-11-01", ], aes(x= TIMESTAMP, y = Jw_trad))
  p<- p + geom_line(aes(colour = SensorID))
  p <- p + facet_grid(SYSTEM ~ .)
  p <- p + scale_y_continuous(limits = c(-0.1, 5))
p

Jwtime <- dlply(df.cast,
               .(Week),
               function(x) (PlotTimecourse(x, dT = "Jw_trad")))
rm(Jwtime)

small <- df.cast[df.cast$SYSTEM == "SYS3" &
                 df.cast$SensorID == "2" &
                 df.cast$Jw_trad < 0.08 &
                 df.cast$Jw_trad > 0 &
                 df.cast$Qf_Avg > 0.010 &
                 #df.cast$dT.offcor.good > 0.5 &
                 df.cast$Day == as.Date("2015-11-07")
                 #df.cast$Date >= short.start &
                 #df.cast$Date <= short.end
                 , ]
# save data to be used in k-analysis via flexdashboard
# save(small, file = "small.RData")
save(df.cast, file = "df_cast_flex.RData", compress = TRUE)

#iplot(small$TIMESTAMP, small$Jw_trad)
##l <- lowess(small$TIMESTAMP, small$Jw_trad, delta = 0.000001)
##ilines(l)
#iplot(small$TIMESTAMP, small$dT_Avg)
#iplot(small$TIMESTAMP, small$PPFD_from_SLD)
#iplot(small$TIMESTAMP, small$Qr_Avg)
#iplot(small$TIMESTAMP, small$Qf_Avg)
#iplot(small$Jw_trad, small$dT.offcor.good)
#mysel <- small[iset.selected(), ]

#mean(mysel$Jw_trad / mysel$dT_Avg, na.rm = TRUE)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# calculate daily K for AGFACE
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# from Langensiepen paper:
k <- 0.46 # set to a constant mean value from the range of 0.37 < k < 0.55
# from Sennock et al. 1996 paper 0.2 < k < 3.2

# overall k as Jw / dT
df.cast$K <- df.cast$Jw_trad / df.cast$dT.offcor.good
df.cast$K[df.cast$K > 4] <- NA
df.cast$K[df.cast$K < 0.2] <- NA
summary(df.cast$K[is.finite(df.cast$K) == TRUE & df.cast$TOD == "Daytime"])

p <- ggplot(df.cast[df.cast$SYSTEM == "SYS3" &
                    df.cast$Date == as.Date("2015-11-09"), ], 
                    #[df.cast$dT.offcor.good > 1, ],
            aes(x = TIMESTAMP))
  #p <- p + geom_line(aes(y = Sapflow_Avg, colour = SensorID))
  p <- p + geom_line(aes(y = myFlow, colour = SensorID), linetype = "dashed")
  p <- p + facet_grid(SYSTEM ~. )
  p <- p + scale_y_continuous(limits = c(-1, 10))
#  p <- p + scale_x_continuous(limits = c(-0.1, 4))
p


# using flexdashboard to visualise various sap-flow components 
# to determine stable conditions for k calculation
library(flexdashboard)
#rmarkdown::run("LS_dash.Rmd")

p <- ggplot(small, aes(x = TIMESTAMP, y = dT_Avg))
   p <- p + geom_line(aes(colour = SensorID))
   #p <- p + scale_y_continuous(limits = c(0, 0.08))
   #p <- p + facet_grid(SYSTEM ~ .)
p



p <- ggplot(df.cast[df.cast$Jw_trad > 0 &
                    df.cast$Jw_trad < 0.05 &
                    df.cast$dT.offcor.good > 0, ], 
            aes(y = Jw_trad, x = dT.offcor.good))
   p <- p + geom_point(aes(colour = SensorID))
   #p <- p + scale_y_continuous(limits = c(0, 0.08))
   p <- p + facet_grid(SYSTEM ~ .)
p

# challenge: find periods when Jw_trad is stable over 30 min...
# this is done via flexdashboard
# objects from flexdashboard are Kresult data frames
# filenames of the RData files start with "K-analysis"
# grad the resulting RData files for further analysis

GrabRData <- function(file) {
   load(file)
   if (exists("dailyK") == FALSE) {
     dailyK.all <- rbind(dailyK.all, dailyK) 
   } else {
     dailyK.all <- dailyK
   }
}


Kfiles <- list.files(path = "./collected_K",
                     pattern = "^K-analysis",
                     full.names = TRUE)

dailyK <- ldply(Kfiles, GrabRData)
dailyK <- unique(dailyK)

with(dailyK, table(Date, SYSTEM, SensorID))
summary(dailyK)

# quality control on dailyK values
dailyK$K[dailyK$K <= 0] <- NA
dailyK$K[dailyK$K >= 1] <- NA
K.mean <- mean(dailyK$K, na.rm = TRUE)

p <- ggplot(dailyK, aes(x = SYSTEM, y = K))
   p <- p + geom_hline(yintercept = 0.46, colour = "orange", linetype = "dashed")
   p <- p + geom_hline(yintercept = K.mean, colour = "grey")
   p <- p + geom_boxplot(aes(colour = SensorID))
p

p <- ggplot(dailyK, aes(x = Date, y = K))
  p <- p + stat_summary(aes(colour = SYSTEM, linetype = SensorID),
                        fun.data = "mean_sdl", geom = "linerange")
  p <- p + stat_summary(aes(colour = SYSTEM, linetype = SensorID),
                        fun.data = "mean_sdl", geom = "line")
  p <- p + facet_grid(SYSTEM ~ .)
p

# K data determined until harvest, on Nov 18.


# perfect data frame of potential K values
#my.date <- seq(from = min(dailyK$Date), to = max(dailyK$Date), by = "1 day")
my.date <- seq(from = min(df.cast$Date), to = max(dailyK$Date), by = "1 day")
my.sys <- df.cast[, names(df.cast) %in% c("SYSTEM", "SensorID")]
my.sys <- unique(my.sys)
sys.ID.date <- merge(my.sys, my.date)
names(sys.ID.date) <- gsub("y", "Date", names(sys.ID.date))

summary(dailyK[dailyK$K > 0 &
               dailyK$K < 3.2 & # maximum reported for soybean
               dailyK$Date < as.Date(seven.days.before.cut), ])

# average K per day and sensor
dailyK.mean <- ddply(dailyK,
                     .(Date, SYSTEM, SensorID),
                     summarise,
                     K.mean = mean(K, na.rm = TRUE))
# set a reasonable mean K to fill gaps in data. based on quality controlled data
K.mean <- mean(dailyK.mean$K.mean, na.rm = TRUE) 

summary(dailyK.mean[dailyK$K > 0 &
        dailyK$K < 3.2 & # maximum reported for soybean
        dailyK$Date < as.Date(seven.days.before.cut), ])


p <- ggplot(dailyK.mean, aes(x = Date, y = K.mean))
  p <- p + geom_line(aes(colour = SensorID))
  p <- p + scale_y_continuous(limits = c(0, 3.2))
  p <- p + facet_grid(SYSTEM ~ .)
p

# merge dailyK with empty data frame
dailyK.merge <- merge(dailyK.mean, sys.ID.date,
                      all.y = TRUE)

# for now, replace all missing K from the daily file with the mean from dailyK.mean
dailyK.merge$K.mean[is.na(dailyK.merge$K.mean)] <- K.mean
# for now, replace all very high K with the maximum from the Senock paper, i.e. 3.2
# dailyK.merge$K.mean[dailyK.merge$K.mean > 3.2] <- 3.2 # changed on July 24
dailyK.merge$K.mean[dailyK.merge$K.mean > 1] <- 0.55 # maximum from Langensiepen

# for now, replace all negative K with the mean from Langensiepen
dailyK.merge$K.mean[dailyK.merge$K.mean < 0] <- k # does nothing any more

p <- ggplot(dailyK.merge, aes(x = SYSTEM, y = K.mean))
  p <- p + geom_boxplot(aes(colour = SensorID))
  p <- p + facet_grid(SYSTEM ~ .)
p


# Last observation carried Forward
# library(zoo)
# na.locf()


# to do merge "dailyK.merge" into df.cast
# then calculate Jw used on self-defined daily K
# then do the analysis below

df.cast <- merge(df.cast, dailyK.merge,
                     all.x = TRUE)

p <- ggplot(df.cast, aes(x = SYSTEM, y = K.mean))
  p <- p + geom_boxplot(aes(colour = SensorID))
p

# calculate Jw based on my own K
df.cast$my.Jw <- with(df.cast, K.mean * dT.offcor.good)

# calculate leaf-area based Jw Jw.rel
df.cast$my.Jw.rel <- with(df.cast, my.Jw / Total_leaf_area)

p <- ggplot(df.cast, aes(x = TIMESTAMP, y = my.Jw))
  p <- p + geom_line(aes(colour = SensorID))
  p <- p + facet_grid(SYSTEM ~ .)
p


# average Jw
# average all per hour
Jw.mean <- ddply(df.cast,
                .(Date, Hour, Ring, SYSTEM, CO2_treatment, SensorID, Cultivar, TOD,
                  Stem.intact, Stem.position),
                  colwise(mean),
                  na.rm = TRUE)

p <- ggplot(Jw.mean[Jw.mean$Date >= as.Date(seven.days.before.cut) &
                    Jw.mean$TOD == "Daytime" &
                    Jw.mean$Stem.position == "upright", ], 
            aes(x = Temp_Avg, y = dT_Avg))
  p <- p + geom_hline(yintercept = 0, linetype = "dashed", colour = "grey")
  p <- p + geom_point(aes(colour = Stem.intact))
  p <- p + stat_summary(aes(colour = Stem.intact),
                        fun.data = "mean_sdl",
                        fun.args = list(mult = 1))
  p <- p + geom_smooth(aes(colour = Stem.intact))
  # p <- p + facet_grid(Stem.intact ~ .)
p

# summarise per CO2 treatment
Jw <- ddply(df.cast[df.cast$my.Jw.rel > 0, ], # was Jw.rel
            .(TIMESTAMP, CO2_treatment, Cultivar),
            .parallel = TRUE,
            colwise(mean),
            na.rm = TRUE )
Jw <- Jw[!is.na(Jw$Cultivar), ]

Jw.sd <- ddply(df.cast[df.cast$my.Jw.rel > 0, ], # was Jw.rel
            .(TIMESTAMP, CO2_treatment, Cultivar),
            .parallel = TRUE,
            colwise(sd),
            na.rm = TRUE )
Jw.sd <- Jw.sd[!is.na(Jw.sd$Cultivar), ]

names(Jw.sd) <- paste0(names(Jw.sd), "_sd")
Jw$my.Jw.rel_sd <- Jw.sd$my.Jw.rel_sd # was Jw.rel


# power-off times
power.off.start <- 22
power.off.end <- 4

Jw$Power <- "on"
Jw$Power[Jw$Hour > 18 | Jw$Hour < 7] <- "off"
Jw$Jw.rel[Jw$Power == "off"] <- NA
Jw$Day <- format(Jw$TIMESTAMP, "%F")

# what is going on wth the mean aCO2 on 2015-10-23? Barely any variance in aCO2.
Oct23 <- df.cast[df.cast$Date == as.Date("2015-10-23"), ]

# negative or unreasonable K-values are now being replaced with sensible means prior to calculating Jw.
# corrected data are fine on Oct23.

p <- ggplot(Oct23, aes(x = TIMESTAMP, y = my.Jw))
  p <- p + geom_point(aes(colour = CO2_treatment))
  p <- p + facet_grid(SYSTEM ~ .)
  #p <- p + coord_cartesian(ylim = c(0, 0.25))
  #p <- p + scale_y_continuous(limits = c(0, 10))
p


# check regarding potential issues with SYS3
# both sensors in SYS3 seem fine regarding dt, but not regarding Jw
# must be caused by bad "k"? - was due to uncorrected dT_Avg being used in calculation
# of Jw - now corrected.

my.mean.K <- mean(df.cast$K.mean, na.rm = TRUE)
p <- ggplot(df.cast, aes(x = SYSTEM, y = K.mean))
  p <- p + geom_hline(yintercept = 0.46, colour = "grey") # from paper
  p <- p + geom_hline(yintercept = my.mean.K, colour = "orange") # from own data
  p <- p + geom_boxplot(aes(colour = SensorID))
p

p <- ggplot(df.cast, aes(x = TIMESTAMP, y = my.Jw))
  p <- p + geom_line(aes(colour = SensorID))
  p <- p + facet_grid(SYSTEM ~ .)
p


z <- sapply(ls(), function(x)
            format(object.size(get(x)), units = "Mb"))
as.matrix(rev(sort(z)))
save(list = c("df.cast",
	"Jw",
	"Jw.mean",
	"MyLm",
	"dailyK",
	"SapOffset",
	"stem.cut.date",
	"seven.days.before.cut",
	"sap.flow.start",
	"put.back.up.date",
	"my.growthstage",
	"GrabRData",
	"date.gs",
	"CO2.treats",
	"CO2.label",
	"CO2_lab"), 
	file = "self-calculated_Jw.RData", compress = TRUE)


