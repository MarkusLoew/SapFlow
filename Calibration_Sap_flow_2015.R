# using the CampbellLogger library to import files
# this script loads the in-season data 2015

library(CampbellLogger)
library(doMC)
library(ggplot2)
library(plyr)

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

# date of stem-cut
stem.cut.date    <- as.POSIXct("2015-11-20 09:00:00", tz = "GMT")
put.back.up.date <- as.POSIXct("2015-11-24 16:00:00", tz = "GMT")

setwd("~/AgFace/2015/Campbell_logger/Transmissions")

load("5min_In-season2015_sap.RData")
load("5min_In-season2015_cast_sap.RData")

setwd("~/AgFace/Topics/Sap_flow")

df.cast <- df.cast[!is.na(df.cast$Cultivar), ]
calib <- df.cast[df.cast$TIMESTAMP > stem.cut.date &
                 !is.na(df.cast$Cultivar), ]

p <- ggplot(df.cast, aes(x = TIMESTAMP, y = Jw_Langensiepen))
  p <- p + geom_line()
  p <- p + geom_vline(xintercept = as.numeric(stem.cut.date),    colour = "red")
  p <- p + geom_vline(xintercept = as.numeric(put.back.up.date), colour = "green")
  p <- p + facet_grid(Cultivar ~ Ring)
p


