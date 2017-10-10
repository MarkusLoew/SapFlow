# analysis if sap flow

# based on script "Sap_flow_2015.R"

# Markus Löw, June 2016

library(nlme)
library(ggplot2)
library(plyr)
library(reshape2)
library(lmediaplots)
library(effects)
library(cowplot)
library(ggjoy)
library(predictmeans)


setwd("~/AgFace/Topics/Sap_flow")
load("self-calculated_Jw.RData")

# set theme for figures
# set the ggplot2 theme for the whole session
theme_set(theme_bw())
theme_replace(strip.background = element_rect(fill = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.key       = element_blank())

#++++++++++++++++++++++++++++++++++++++++++++++++
# summarising figures
#++++++++++++++++++++++++++++++++++++++++++++++++


# more detailed view on some days
zoom.start <- as.POSIXct("2015-11-07 08:00:00", tz = "GMT")
zoom.end   <- as.POSIXct("2015-11-09 20:00:00", tz = "GMT")

#ann.df <- data.frame(x = c(as.POSIXct("2015-11-07 09:00:00", tz = "GMT"),
#                           as.POSIXct("2015-11-08 09:00:00", tz = "GMT"),
#                           as.POSIXct("2015-11-09 09:00:00", tz = "GMT"),
#                           as.POSIXct("2015-11-10 09:00:00", tz = "GMT")),
#                     y = rep(0.025, 4),
#                     label = c("B", "C", "C", "D"),
#                     stringsAsFactors = FALSE)
#ann.df$xx <- as.numeric(ann.df$x)

# what is the mean K on these three selected days?
summary(Jw$K.mean[Jw$TIMESTAMP > zoom.start &
                  Jw$TIMESTAMP < zoom.end &
                  Jw$Cultivar == "Yitpi"])

p <- ggplot(Jw[Jw$TIMESTAMP > zoom.start &
               Jw$TIMESTAMP < zoom.end &
               Jw$Cultivar == "Yitpi" &
               Jw$Hour < 19, ], 
            aes(x = TIMESTAMP, y = my.Jw.rel)) # was Jw.rel
  p <- p + geom_linerange(aes(ymin = my.Jw.rel - my.Jw.rel_sd, # was Jw.rel
                              ymax = my.Jw.rel + my.Jw.rel_sd), # was Jw.rel
                              colour = "grey", alpha = 0.15)
  p <- p + geom_line(aes(colour = CO2_treatment), size = rel(0.7))
  p <- p + scale_y_continuous(sec.axis = sec_axis(~.*1, name = element_blank(), labels = rep("", 5)))
  p <- p + scale_colour_manual(values = c("indianred", "lightblue"),
                               labels = CO2.treats)
  p <- p + facet_grid(. ~ Day, scale = "free_x")
  p <- p + coord_cartesian(ylim = c(0, 0.035))
  p <- p + labs(x = "Time",
                y = expression("Relative sap flow"~(g~h^-1~cm^-2)),
                colour = CO2_lab)
#  p <- p + annotate(geom = "text", data = ann.df, aes(x = xx, y = y, label = label))
  p <- p + theme(legend.position = c(0.08, 0.87),
                 legend.background = element_rect(fill = NA),
                 axis.line = element_line(),
                 panel.border = element_rect(color = NA, fill = NA, size = 1))
p
fig.three.day.sap.flow <- p

ggsave(file = "Three_day_sap_flow_example_Yitpi.png",
       width = 13, height = 6, dpi = 72)

save(fig.three.day.sap.flow, file = "Three_day_sap_flow.RData", compress = TRUE)

# How about another multi-day window, e.g. Oct 26 to 29?
four.day.zoom.start <- as.POSIXct("2015-10-26 08:00:00", tz = "GMT")
four.day.zoom.end   <- as.POSIXct("2015-10-29 20:00:00", tz = "GMT")

p <- ggplot(Jw[Jw$TIMESTAMP > four.day.zoom.start &
               Jw$TIMESTAMP < four.day.zoom.end &
               Jw$Cultivar == "Yitpi" &
               Jw$Hour < 19, ], 
            aes(x = TIMESTAMP, y = my.Jw.rel)) # was Jw.rel
  p <- p + geom_linerange(aes(ymin = my.Jw.rel - my.Jw.rel_sd, # was Jw.rel
                              ymax = my.Jw.rel + my.Jw.rel_sd), # was Jw.rel
                              colour = "grey", alpha = 0.15)
  p <- p + geom_line(aes(colour = CO2_treatment), size = rel(0.7))
  p <- p + scale_y_continuous(sec.axis = sec_axis(~.*1, name = element_blank(), labels = rep("", 5)))
  p <- p + scale_colour_manual(values = c("indianred", "lightblue"),
                               labels = CO2.treats)
  p <- p + facet_grid(. ~ Day, scale = "free_x")
  p <- p + coord_cartesian(ylim = c(0, 0.035))
  p <- p + labs(x = "Time",
                y = expression("Relative sap flow"~(g~h^-1~cm^-2)),
                colour = CO2_lab)
#  p <- p + annotate(geom = "text", data = ann.df, aes(x = xx, y = y, label = label))
  p <- p + theme(legend.position = c(0.08, 0.87),
                 legend.background = element_rect(fill = NA),
                 axis.line = element_line(),
                 panel.border = element_rect(color = NA, fill = NA, size = 1))
p
fig.four.day.sap.flow <- p


# After Chen et al (2014):
#Considering relatively higher TDP error under low vapor pres-
#sure deficit (VPD), only sap flow values for VPD > 0.6 kPa were used
#in the study (Ewers and Oren, 2000; O’brien et al., 2004)
# same figure, but without x-axis zoom
p <- ggplot(Jw[#Jw$Cultivar == "Yitpi" &
               #Jw$VPD_Avg > 0.6 &
               Jw$Hour < 20, ], 
            aes(x = TIMESTAMP, y = my.Jw.rel)) # was Jw.rel
  p <- p + geom_linerange(aes(ymin = my.Jw.rel - my.Jw.rel_sd, # was Jw.rel
                              ymax = my.Jw.rel + my.Jw.rel_sd), # was Jw.rel
                              colour = "grey", alpha = 0.08)
  p <- p + geom_line(aes(colour = CO2_treatment), size = rel(0.7))
  p <- p + scale_y_continuous(sec.axis = sec_axis(~.*1, name = element_blank(), labels = rep("", 4)), limits = c(0, 0.06))
  p <- p + scale_colour_manual(values = c("indianred", "lightblue"),
                               labels = CO2.treats)
  p <- p + facet_grid(Cultivar ~ Day, scale = "free_x")
  #p <- p + coord_cartesian(ylim = c(0, 0.025))
  # p <- p + scale_y_continuous(limits = c(0, 0.04))
  p <- p + labs(x = "Time",
                y = expression("Relative sap flow"~(g~h^-1~cm^-2)),
                colour = CO2_lab)
#  p <- p + annotate(geom = "text", data = ann.df, aes(x = xx, y = y, label = label))
  p <- p + theme(legend.position = c(0.08, 0.87),
                 legend.background = element_rect(fill = NA),
                 axis.line = element_line(),
                 panel.border = element_rect(color = NA, fill = NA, size = 1))
p
fig.complete.sap.flow <- p

# similar plot for VPD and PPFD
p <- ggplot(Jw[Jw$Cultivar == "Yitpi" &
               #Jw$VPD_Avg > 0.6 &
               Jw$Hour < 20, ], 
            aes(x = TIMESTAMP, y = VPD_Avg)) # was Jw.rel
  p <- p + geom_line(aes(colour = CO2_treatment), size = rel(0.7))
#  p <- p + scale_y_continuous(sec.axis = sec_axis(~.*1, name = element_blank(), labels = rep("", 6)), limits = c(0, 0.05))
  p <- p + scale_colour_manual(values = c("indianred", "lightblue"),
                               labels = CO2.treats)
  p <- p + facet_grid(. ~ Day, scale = "free_x")
  #p <- p + coord_cartesian(ylim = c(0, 0.025))
  # p <- p + scale_y_continuous(limits = c(0, 0.04))
  p <- p + labs(x = "Time",
                y = "Vapour pressure deficit VPD (kPa)",
                colour = CO2_lab)
#  p <- p + annotate(geom = "text", data = ann.df, aes(x = xx, y = y, label = label))
  p <- p + theme(legend.position = c(0.08, 0.87),
                 legend.background = element_rect(fill = NA),
                 axis.line = element_line(),
                 panel.border = element_rect(color = NA, fill = NA, size = 1))
p
fig.complete.vpd <- p

# similar plot for VPD and PPFD
p <- ggplot(Jw[Jw$Cultivar == "Yitpi" &
               #Jw$VPD_Avg > 0.6 &
               Jw$Hour < 20, ], 
            aes(x = TIMESTAMP, y = PPFD_from_SLD)) # was Jw.rel
  p <- p + stat_summary(fun.data = "mean_sdl", geom = "line", size = rel(0.7))
#  p <- p + scale_y_continuous(sec.axis = sec_axis(~.*1, name = element_blank(), labels = rep("", 6)), limits = c(0, 0.05))
#  p <- p + scale_colour_manual(values = c("indianred", "lightblue"),
#                               labels = CO2.treats)
  p <- p + facet_grid(. ~ Day, scale = "free_x")
  #p <- p + coord_cartesian(ylim = c(0, 0.025))
  # p <- p + scale_y_continuous(limits = c(0, 0.04))
  p <- p + labs(x = "Time",
                y = expression("Photosynthetic active radiation ("~mu*mol~m^-2*s^-1~")"),
                colour = CO2_lab)
#  p <- p + annotate(geom = "text", data = ann.df, aes(x = xx, y = y, label = label))
  p <- p + theme(legend.position = c(0.08, 0.87),
                 legend.background = element_rect(fill = NA),
                 axis.line = element_line(),
                 panel.border = element_rect(color = NA, fill = NA, size = 1))
p
fig.complete.PPFD <- p

plot_grid(fig.complete.sap.flow, 
          fig.complete.vpd, 
          fig.complete.PPFD,
          labels = c("A", "B", "C"),
          ncol = 1,
          align = "v")

# check linear segments in data on 2015-11-04, 2015-11-05, 2015-11-12!
# consider removing 2015-10-22 - bad VPD data?

# checking linear relationship dT.offcor.good with light
p <- ggplot(Jw, aes(y = dT.offcor.good, x = PPFD_from_SLD))
  p <- p + geom_point()
  p <- p + geom_smooth(aes(colour = CO2_treatment), method = "lm")
p

my.lm <- lm(dT.offcor.good ~ PPFD_from_SLD, data = Jw)
summary(my.lm)

# VPD response
p <- ggplot(df.cast, aes(x = VPD_Avg, y = my.Jw))
  p <- p + geom_point(aes(colour = SensorID))
  p <- p + geom_smooth(aes(colour = SensorID), colour = "red")
  p <- p + facet_grid(SYSTEM ~ .)
p


p <- ggplot(Jw.mean[Jw.mean$TIMESTAMP < as.POSIXct("2015-11-10") &
                    Jw.mean$TIMESTAMP > as.POSIXct("2015-10-20") &
                    Jw.mean$my.Jw >= 0 &
                    Jw.mean$PPFD_from_SLD > 50 &
                    Jw.mean$VPD_Avg > 0.4 &
                    Jw.mean$TOD == "Daytime" &
                    Jw.mean$Stem.intact == "Stem ok" &
                    Jw.mean$Stem.position == "upright" &
                    !is.na(Jw.mean$SYSTEM), ], 
            aes(y = my.Jw, x = VPD_Avg))
  p <- p + geom_point(alpha = 0.3)
  p <- p + geom_smooth(aes(colour = SensorID), method = "lm")
  p <- p + scale_y_continuous(limits = c(0, 5))
  p <- p + facet_grid(SYSTEM ~ .)
p


Jw.mean.selected <- Jw.mean[ #Jw.mean$TIMESTAMP < as.POSIXct("2015-11-10") &
                    #Jw.mean$TIMESTAMP > as.POSIXct("2015-10-20") &
                    Jw.mean$my.Jw >= 0 &
                    Jw.mean$PPFD_from_SLD > 50 &
                    Jw.mean$VPD_Avg > 0.4 &
                    Jw.mean$TOD == "Daytime" &
                    #Jw.mean$Stem.intact == "Stem ok" &
                    #Jw.mean$Stem.position == "upright" &
                    !is.na(Jw.mean$SYSTEM), ]
Jw.mean.selected <- Jw.mean.selected[!is.na(Jw.mean.selected$CO2_treatment), ]


p <- ggplot(Jw.mean.selected, aes(x = VPD_Avg, y = my.Jw, colour = Cultivar))
  p <- p + geom_point()
  p <- p + geom_smooth()
  p <- p + facet_grid(Stem.intact ~ CO2_treatment)
p

# VPD dependency for each plant # should this be done per day?
VPD.response <- ddply(Jw.mean[Jw.mean$TIMESTAMP < as.POSIXct("2015-11-10") &
                              Jw.mean$TIMESTAMP > as.POSIXct("2015-10-20") &
                              !is.na(Jw.mean$my.Jw) &
                              Jw.mean$my.Jw >= 0 &
                              Jw.mean$PPFD_from_SLD > 50 &
                              Jw.mean$VPD_Avg > 0, ],
                      .(CO2_treatment, SYSTEM, CO2_treatment, Cultivar, SensorID),
                      function(x) MyLm(x, response = my.Jw, predictor = VPD_Avg))

p <- ggplot(VPD.response, 
            aes(x = Cultivar, y = slope))
  p <- p + geom_boxplot(aes(colour = CO2_treatment))
  p <- p + stat_summary(aes(colour = CO2_treatment), 
                       fun.data = "mean_sdl", 
                       fun.args = list(mult = 1), position = position_dodge(0.55))
p

my.lme <- lme(slope ~ CO2_treatment * Cultivar,
              random = ~ 1 | SYSTEM/SensorID, 
              data = VPD.response,
              na.action = na.omit,
              control = lmeControl(maxIter = 100,
                                   msMaxIter = 100,
                                   tolerance = 1e-8,
                                   opt = c("nlminb")))
anova(my.lme)

diaplots <- lmediaplots(my.lme, plottype = "resvfit")
diaplots <- lmediaplots(my.lme, plottype = "within")
diaplots <- lmediaplots(my.lme, plottype = "qq")
diaplots[1]

#my.lme2 <- update(my.lme, weights = varComb(varIdent(form = ~1 | SYSTEM),
#                                            varIdent(form = ~1 | Date)))
my.lme2 <- update(my.lme, weights = varIdent(form = ~1 | SYSTEM))
anova(my.lme, my.lme2) # updated model is statistically not different, but has lower AIC
anova(my.lme2)

eff.all <- allEffects(my.lme2) # shows fitted data
plot(eff.all)

# mean values from data
ddply(VPD.response, 
      .(CO2_treatment, Cultivar), 
      summarise, 
      mean.slope = mean(slope))


my.lme <- lme(slope ~ CO2_treatment * Cultivar,
              random = ~ 1 | SYSTEM/SensorID,
              data = VPD.response)
anova(my.lme)


# figure per ring/sensor
p <- ggplot(df.cast[df.cast$TIMESTAMP > zoom.start &
               df.cast$TIMESTAMP < zoom.end &
               (df.cast$Hour < 19 & 
                df.cast$Hour > 6 ),], 
                aes(x = TIMESTAMP, y = my.Jw))
  p <- p + geom_hline(yintercept = 0, linetype = "dashed", colour = "grey")
  p <- p + geom_line(aes(colour = SensorID))
  p <- p + facet_grid(Cultivar ~ Ring)
  #p <- p + coord_cartesian(ylim = c(0, 0.05))
p

# light response of sapflow
#df.cast$to.exclude <- FALSE
#df.cast$to.exclude[df.cast$SYSTEM == "SYS3" & df.cast$SensorID == "1"] <- TRUE

p <- ggplot(df.cast, #[df.cast$to.exclude == FALSE, ], 
            aes(x = PPFD_from_SLD, y = my.Jw, shape = SensorID))
  p <- p + geom_point(aes(colour = SYSTEM))
  p <- p + facet_grid(SYSTEM ~ .)
  p <- p + scale_y_continuous(limits = c(0, 2))
p

pos.Jw <- df.cast[df.cast$my.Jw >= 0, ]
pos.Jw <- pos.Jw[!is.na(pos.Jw$SYSTEM), ]

p <- ggplot(pos.Jw[pos.Jw$PPFD_from_SLD > 20 &
                   pos.Jw$VPD_Avg >= 0, ],
            aes(x = VPD_Avg, y = my.Jw))
  p <- p + geom_point(aes(colour = CO2_treatment), alpha = 0.2)
  p <- p + geom_smooth()
  p <- p + facet_grid(CO2_treatment ~ .)
p

# stats on the afternoon difference a/eCO2 in Yitpi
# between noon and 15:00


# create nesting plant ID
df.cast$PlantID <- with(df.cast, interaction(SYSTEM, Ring, SensorID))

# statistical test on full data - probably overfitted, as each 5 min-value is used
# but not nested per 5 min! See half-hourly analysis instead!
my.lme <- lme(my.Jw.rel ~ CO2_treatment * Cultivar,
              random = ~ 1 | TIMESTAMP/Ring/Cultivar/PlantID,
              data = df.cast[df.cast$Hour >= 12 &
                             df.cast$Hour <= 15 &
                             df.cast$TIMESTAMP > zoom.start &
                             df.cast$TIMESTAMP < zoom.end, ],
                             #df.cast$Cultivar == "Yitpi", ],
              na.action = na.omit)
anova(my.lme)

# summarise data per half-hourly blocks
# create half-hourly index based on minute of measurement
df.cast$min30 <- ceiling(df.cast$Minute / 30) * 30

df.cast.30min <- ddply(df.cast,
                       .(Date, Day, Hour, min30, CO2_treatment, SYSTEM, Ring, 
                         Cultivar, PlantID, SensorID),
                       colwise(mean, na.rm = TRUE))

for.test <- df.cast.30min[df.cast.30min$Hour >= 10 &
                          df.cast.30min$Hour <= 15 , ]#&
                          # df.cast.30min$TIMESTAMP > zoom.start &
                          # df.cast.30min$TIMESTAMP < zoom.end, ]

# get rid of the first day - not all sensors were online
for.test <- for.test[for.test$Date > as.Date("2015-10-20") &
                     for.test$Date < as.Date("2015-11-19"), ]


my.lme <- lme(my.Jw.rel ~ CO2_treatment * Cultivar,
              random = ~ 1 | Day/Ring/Cultivar/PlantID,
              data = for.test,
              na.action = na.omit)
anova(my.lme)
# ==> at mid-day (10:00 to 15:00), CO2 effect, a Cultivar effect, but interaction found.
# library(predictmeans)
predictmeans(my.lme, "CO2_treatment", newwd = FALSE)
predictmeans(my.lme, "Cultivar", newwd = FALSE)
predictmeans(my.lme, "CO2_treatment:Cultivar", newwd = FALSE)

p <- ggplot(for.test, aes(x = Cultivar, y = my.Jw.rel))
  p <- p + geom_boxplot(aes(fill = CO2_treatment))
  p <- p + facet_grid(. ~ Date)
p

p <- ggplot(for.test, aes(x= Jw.rel, y = my.Jw.rel))
  p <- p + geom_point(aes(colour = CO2_treatment))
p

p <- ggplot(for.test, aes(x = TIMESTAMP))
  p <- p + geom_line(aes(y = K.mean, linetype = SensorID))
  p <- p + facet_grid(SYSTEM ~ .)
p

# confirmation that SYS3, SensorID 2 in most ov November did not have reasonable
# Jw! most Jw data of this sensor is based on mean K 
# from other sensors....
p <- ggplot(df.cast[df.cast$SYSTEM == "SYS3" &
                    df.cast$SensorID == "2" &
                    df.cast$Day == "2015-11-09", ], 
            aes(x = TIMESTAMP))
  p <- p + geom_line(aes(y = Jw_trad, colour = SensorID))
  p <- p + geom_line(aes(y = my.Jw.rel, colour = SensorID), linetype = "dashed")
  p <- p + facet_grid(SYSTEM ~ .)
  p <- p + coord_cartesian(ylim = c(0, 1))
p

p <- ggplot(for.test, aes(x = TIMESTAMP))
  p <- p + geom_line(aes(y = my.Jw.rel, linetype = SensorID), colour = "red")
  p <- p + geom_line(aes(y = Jw.rel, linetype = SensorID), colour = "blue")
  p <- p + facet_grid(SYSTEM ~ .)
p


# library(effects)
plot(effect(term = "CO2_treatment:Cultivar",
             mod = my.lme), 
     multiline = TRUE)

# eCO2 affects Scout more than Yitpi during mid-day. Higher Jw.rel under eCO2.
# interaction is present across the whole data-set, not jsut the three days

# is there a change in the difference between aCO2 and ecO2 over time?
for.test.melt <- melt(for.test[, names(for.test) %in% 
                              c("Date", "Cultivar", "CO2_treatment",
                                "Hour", "min30", "my.Jw.rel")],
                      id.vars = c("Date", "Cultivar", "CO2_treatment",
                                  "Hour", "min30"))

for.test.cast <- dcast(for.test.melt[for.test.melt$variable == "my.Jw.rel", ],
                       Date + Hour + min30 + Cultivar ~ CO2_treatment,
                       fun.aggregate = mean, na.rm = TRUE)

for.test.cast$CO2diff <- for.test.cast$eCO2 - for.test.cast$aCO2

p <- ggplot(for.test.cast, aes(x = Date, y = CO2diff))
  p <- p + geom_hline(yintercept = 0, colour = "grey90")
  p <- p + stat_summary(aes(colour = Cultivar), 
                        fun.data = "mean_sdl", 
                        fun.args = list(mult = 1), geom = "line")
  p <- p + stat_summary(aes(colour = Cultivar), 
                        fun.data = "mean_sdl", 
                        fun.args = list(mult = 1))
  p <- p + labs(y = "Difference in rel sap flow between a and eCO2")
p
# the differences between the CO2 treatments does not vary much over time


# ++++++++++++++++++++++++++++++++++++++++++++++++
# analyse time of daily maximum sap flow
# ++++++++++++++++++++++++++++++++++++++++++++++++
FindMax <- function(x, para) {
  args <- as.list(match.call())
  resp <- eval(args$para, x)
  if (sum(is.na(resp)) == length(resp)) {
     my.max <- NA
  } else {
     my.max <- x$TIMESTAMP[which.max(resp)]
  }
  return(my.max)
}

max.flow <- ddply(df.cast,
                  .(Date, Ring, Cultivar, CO2_treatment, SensorID),
                  function(x) FindMax(x, my.Jw.rel))
attr(max.flow$V1, "tzone") <- "GMT" # not sure why it changes - has to do with ddply assembly!

p <- ggplot(df.cast[df.cast$TIMESTAMP >= as.POSIXct("2015-10-28", tz = "GMT") &
                    df.cast$TIMESTAMP <= as.POSIXct("2015-10-30", tz = "GMT"), ], 
            aes(x = TIMESTAMP, y = my.Jw.rel))
  p <- p + geom_line(aes(colour = SensorID))
  p <- p + geom_vline(aes(xintercept = as.numeric(V1), colour = SensorID), 
                      data = max.flow)
  p <- p + facet_grid(Ring ~ .)
p
fig.max.flow <- p

# get rid of day information - the current date is used to fill the void
max.flow$Hour <- as.POSIXct(
                     format(max.flow$V1, "%T"), 
                   format = "%T", origin = "1970-01-01")

# frequency of peak times
# bin data to 30min windows first?
max.flow$hour   <- as.numeric(format(max.flow$V1, "%H"))
max.flow$minute <- as.numeric(format(max.flow$V1, "%M"))

max.flow$bin.min <- floor(max.flow$minute / 30) * 30
max.flow$hourmin <- paste(max.flow$hour, max.flow$bin.min, sep = ":")
max.flow$hourmin <- as.POSIXct(max.flow$hourmin, format = "%H:%M")

# density and histogram of max.flow peak times
# density of a continuous random variable, is a function, whose value at any given sample (or point) in the sample space (the set of possible values taken by the random variable) can be interpreted as providing a relative likelihood that the value of the random variable would equal that sample. In other words, while the absolute likelihood for a continuous random variable to take on any particular value is 0 (since there are an infinite set of possible values to begin with), the value of the PDF at two different samples can be used to infer that, in any particular draw of the random variable, how much more likely it is that the random variable would equal one sample compared to the other sample

p <- ggplot(max.flow[max.flow$hour >= 6 &
                     max.flow$hour <= 20 &
                     !(is.na(max.flow$hour)), ], 
            aes(x = hourmin))
  #p <- p + geom_histogram(aes(fill = CO2_treatment, linetype = Cultivar), alpha = 0.3)
  p <- p + geom_density(aes(fill = CO2_treatment, linetype = Cultivar), alpha = 0.55)
  p <- p + scale_fill_manual(values = c("indianred", "lightblue"),
                               labels = c(expression(textstyle(aCO[2])), 
                                          expression(textstyle(eCO[2]))))
  #p <- p + facet_grid(CO2_treatment ~ .)
  p <- p + theme(legend.position = c(0.15, 0.8))
  p <- p + labs(y = "Probability of sap flow peak, kernel density (ratio)",
                x = "Time of day",
                fill = expression(CO[2]~treatment))
p
fig.sapflow.peak.density <- p

ggsave(fig.sapflow.peak.density,
       file = "Peak_sapflow_time.png",
       width = 9, height = 6, dpi = 96)

# trying a joyplot
p <- ggplot(max.flow[max.flow$hour >= 6 &
                     max.flow$hour <= 20 &
                     !(is.na(max.flow$hour)), ], 
            aes(y = CO2_treatment, x = hourmin))
  p <- p + geom_joy(aes(fill = Cultivar))
  p <- p + facet_grid(Cultivar ~ .)
p

p <- ggplot(max.flow[max.flow$hour >= 6 &
                     max.flow$hour <= 20 &
                     !(is.na(max.flow$hour)), ], 
            aes(x = Cultivar, y = hourmin))
  p <- p + geom_boxplot(aes(fill = CO2_treatment))
p

ddply(max.flow[max.flow$hour >= 6 &
                     max.flow$hour <= 20 &
                     !(is.na(max.flow$hour)), ], 
      .(CO2_treatment, Cultivar),
      summarise,
      peak = mean(hourmin, na.rm = TRUE))

# is there a difference in the time of day when maximum sap flow occurs
my.lme <- lme(hourmin ~ CO2_treatment * Cultivar,
              random = ~ 1 | Date/Ring,
              na.action = na.omit,
              data = max.flow[max.flow$hour >= 6 &
                              max.flow$hour <= 20 &
                              !is.na(max.flow$hour), ])
anova(my.lme)

# ++++++++++++++++++++++++++++++++++++++++++++++++
# for comparison - when is the maximum of air temperature?
# ++++++++++++++++++++++++++++++++++++++++++++++++
max.temp <- ddply(df.cast,
                  .(Date, Ring, Cultivar, CO2_treatment, SensorID),
                  function(x) FindMax(x, Temp_Avg))
attr(max.temp$V1, "tzone") <- "GMT"

fig.max.flow + geom_vline(aes(xintercept = as.numeric(V1), colour = SensorID),
                          linetype = "dashed", 
                      data = max.temp)

# frequency of peak times
# bin data to 30min windows first?
max.temp$hour   <- as.numeric(format(max.temp$V1, "%H"))
max.temp$minute <- as.numeric(format(max.temp$V1, "%M"))

max.temp$bin.min <- floor(max.temp$minute / 30) * 30
max.temp$hourmin <- paste(max.temp$hour, max.flow$bin.min, sep = ":")
max.temp$hourmin <- as.POSIXct(max.temp$hourmin, format = "%H:%M")

p <- ggplot(max.temp, aes(x = hourmin))
  #p <- p + geom_histogram()
  p <- p + geom_density(aes(fill = CO2_treatment, linetype = Cultivar), alpha = 0.3)
  #p <- p + facet_grid(CO2_treatment ~ .)
p

p <- ggplot(max.temp, aes(y = CO2_treatment, x = hourmin))
  p <- p + geom_joy(aes(fill = Cultivar, height = ..density..), stat = "density")
  p <- p + facet_grid(Cultivar ~ .)
p

p <- ggplot(max.temp, aes(x = Cultivar, y = hourmin))
  p <- p + geom_boxplot(aes(fill = CO2_treatment))
p

my.lme <- lme(hourmin ~ CO2_treatment * Cultivar,
              random = ~ 1 | Date/Ring,
              na.action = na.omit,
              data = max.temp)
              #control = lmeControl(opt = "optim"))
anova(my.lme)


# ++++++++++++++++++++++++++++++++++++++++++++++++
# for comparison - when is the maximum of VPD?
# ++++++++++++++++++++++++++++++++++++++++++++++++
max.VPD <- ddply(df.cast,
                  .(Date, Ring, Cultivar, CO2_treatment, SensorID),
                  function(x) FindMax(x, VPD_Avg))
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

p <- ggplot(max.VPD, aes(x = hourmin))
  #p <- p + geom_histogram()
  p <- p + geom_density(aes(fill = CO2_treatment, linetype = Cultivar), alpha = 0.3)
  #p <- p + facet_grid(CO2_treatment ~ .)
p

p <- ggplot(max.VPD, aes(x = hourmin, y = CO2_treatment))
  p <- p + geom_joy(aes(fill = Cultivar))
  p <- p + facet_grid(Cultivar ~ .)
p

p <- ggplot(max.VPD, aes(x = Cultivar, y = hourmin))
  p <- p + geom_boxplot(aes(fill = CO2_treatment))
p

my.lme <- lme(hourmin ~ CO2_treatment * Cultivar,
              random = ~ 1 | Date/Ring,
              na.action = na.omit,
              data = max.VPD)
              #control = lmeControl(opt = "optim"))
anova(my.lme)
# ==> no noteworthy effects on VPD maximum time

# +++++++++++++++++++++++++++++++++++++++++++
# save workspace
# +++++++++++++++++++++++++++++++++++++++++++

save.image(file = "Sap_flow_2015_visualisation_analysis_result.RData",
           compress = TRUE)
