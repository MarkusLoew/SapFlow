# Photosynthesis model

library(plantecophys)
library(ggplot2)
library(cowplot)

# set theme for figures
# set the ggplot2 theme for the whole session
theme_set(theme_bw())
theme_replace(strip.background  = element_rect(fill = "white"),
              panel.grid        = element_blank(),
              legend.key        = element_blank(),
              legend.background = element_blank())

setwd("~/AgFace/Topics/Sap_flow")
# create a model data frame
# needs VPD VPD
# needs Atmospheric CO2 Ca
# needs PPFD PPFD
# needs TLeaf 

# find Vcmax na Jmax from wheat papers Vcmax ~100 ?Jmax?

d <- Photosyn(VPD = rep(seq(5.2, 0.6, by = -0.2), 2), 
  Vcmax = 100, 
  Jmax = 66, 
  Rd = 0.6, 
  Tleaf = 28,
  Ca = c(rep(400, 24), rep(550, 24)),
  PPFD = 1800)


plot(ALEAF ~ VPD, data = d)
d$diffA <- c(diff(d$ALEAF), NA)
d$diffA[d$diffA > 0.15] <- NA
# calculate difference in percent of ALEAF
d$perc_changeA <- with(d, diffA / ALEAF * 100)

# differences in gs
d$diffgs <- c(diff(d$GS), NA)
d$diffgs[d$diffgs < -0.1] <- NA
# calculate difference in percent of ALEAF
d$perc_changegs <- with(d, diffgs / GS * 100)


plot(diffA  ~ VPD, data = d)
plot(diffgs ~ VPD, data = d)

# VPD conditions:
# Gobin (2012)
# VPD values below 0.45 kPa reflect a cool and humid environment, values above 1.25 kPa a hot and dry environment with values above 2 kPa resulting in zero growth due to decreasing transpiration rates (Fletcher et al., 2007; Rodriguez and Sadras, 2007). Optimal growth is considered around 0.9 – 1.0 kPa.

# what was the average VPD during the time when there was a CO2-response of VPD?
# in-canopy midday VPD during Oct - November was 2 kPa +- 0.38sd 
# from 03_Sap_vs_microclimate.R, mid.day.mean

VPD.axis <- "Vapour pressure deficit, VPD [kPa]"
CO2.label <- expression(CO[2]~treatment~"[ppm]")
CO2.colours <- c("indianred", "lightblue")

p <- ggplot(d, aes(x = VPD, y = ALEAF))
  p <- p + geom_point(aes(color = as.factor(Ca)))
  p <- p + theme(legend.position = c(0.85, 0.85))
  p <- p + scale_colour_manual(values = CO2.colours)
  p <- p + labs(x = VPD.axis,
                y = expression(Modelled~net~assimilation~rate~A~"["*mu*mol~CO[2]~m^-2*s^-1*"]"),
                colour = CO2.label)
p
fig.photo <- p


# percent change
# percent change
p <- ggplot(d, aes(x = VPD, y = perc_changeA))
  p <- p + geom_point(aes(color = as.factor(Ca)))
  p <- p + scale_colour_manual(values = CO2.colours)
  #p <- p + theme(legend.position = c(0.8, 0.75))
  p <- p + theme(legend.position = "none")
  p <- p + annotate("text", x = 2.4, y = 0.9, label = "Mean mid day VPD during grainfilling")
  p <- p + geom_segment(aes(x = 2, y = 0.85, xend = 2, yend = 0.69), 
                        arrow = arrow(length = unit(0.2, "cm")))
  p <- p + labs(y = "Percent change of assimilation rate due to lowering VPD by 0.2 kPa [%]",
                x = VPD.axis,
                colour = CO2.label)
p
fig.change <- p


p <- ggplot(d, aes(x = VPD, y = GS))
  p <- p + geom_point(aes(color = as.factor(Ca)))
  p <- p + theme(legend.position = c(0.85, 0.85))
  p <- p + scale_colour_manual(values = CO2.colours)
  p <- p + labs(x = VPD.axis,
                y = expression(Stomatal~conductance~g[s]~"["~mmol~H[2]*O~m^-2*s^-1*"]"),
                colour = CO2.label)
p
fig.gs <- p

p <- ggplot(d, aes(x = VPD, y = diffgs))
  p <- p + geom_point(aes(color = as.factor(Ca)))
  p <- p + scale_colour_manual(values = CO2.colours)
p

# percent change
p <- ggplot(d, aes(x = VPD, y = perc_changegs))
  p <- p + geom_point(aes(color = as.factor(Ca)))
  p <- p + scale_colour_manual(values = CO2.colours)
  #p <- p + theme(legend.position = c(0.8, 0.75))
  p <- p + theme(legend.position = "none")
#  p <- p + geom_hline(yintercept = 5, colour = "grey", linetype = "dashed")
#  p <- p + geom_vline(xintercept = 2, colour = "grey", linetype = "dashed")
  p <- p + annotate("text", x = 3, y = 5.9, label = "Mean mid day VPD during grainfilling")
  p <- p + geom_segment(aes(x = 2.2, y = 5.7, xend = 2.05, yend = 4.8), 
                        arrow = arrow(length = unit(0.2, "cm")))
  p <- p + labs(y = "Percent change of stomatal conductance due to lowering VPD by 0.2 kPa [%]",
                x = VPD.axis,
                colour = CO2.label)
p
fig.change.gs <- p

p <- ggplot(d, aes(x = perc_changegs, y = perc_changeA))
  p <- p + geom_hline(yintercept = c(0.4, 0.6), colour = "grey", linetype = "dashed")
  p <- p + geom_vline(xintercept = c(4, 5), colour = "grey", linetype = "dashed")
  p <- p + geom_point(aes(colour = as.factor(Ca)))
  p <- p + scale_colour_manual(values = CO2.colours)
  p <- p + theme(legend.position = c(0.85, 0.12))
  p <- p + labs(x = "Percent change of gs due to changing VPD by 0.2 kPa [%]",
                y = "Percent change of gs due to changing VPD by 0.2 kPa [%]",
                colour = CO2.label)
p

# Photosynthesis at 2 KPa?
d[d$VPD == 2, ]

# Franzaring (2010): VPD response between Ambient ant FACE fluctuating between about 
# +0.3 and -0.5 KPa.

# assemble figures
plot_grid(fig.photo, fig.change,
          align = "h",
          labels = "auto", nrow = 1)
ggsave(file = "Modelled_photsynthesis_and_change.pdf",
       width = 12.39, height = 7)

plot_grid(fig.gs, fig.change.gs,
          align = "h",
          labels = "auto", nrow = 1)
ggsave(file = "Modelled_stomatal_conductance_and_change.pdf",
       width = 12.39, height = 7)
