getwd()
work_dir <- "D:/Documents (D Drive)/PhD/Research projects/Collective responses to mass violence/Zimbabwe _ Gukurahundi"
setwd(work_dir)

packages <- c("reshape", "plyr", "dplyr", "car", "stargazer", "gridExtra", "olsrr", 
              "foreign", "ggplot2", "ggmap", "mapsapi", "sf", "sp", "data.table", 
              "mapdata", "maps", "raster", "rworldmap", "GADMTools", "rgdal", "nngeo", 
              "mapview", "plm", "gplots", "haven", "lfe", "plm", 
              "haven", "knitr", "AER", "DataCombine", "lubridate", "plotrix",
              "jtools") # combines packages
lapply(packages, library, character.only = TRUE) # loads all packages in "packages" list

zimbabwe <- read.csv("2016-08-01-2019-03-09-Zimbabwe_acled.csv")

zimbabwe$civilians <- ifelse(zimbabwe$event_type == "Violence against civilians", 1, 0)
zimbabwe$riots <- ifelse(zimbabwe$event_type == "Riots", 1, 0)
zimbabwe$protests <- ifelse(zimbabwe$event_type == "Protests", 1, 0)

zimbabwe$event_date <- as.Date(zimbabwe$event_date, format = "%m/%d/%Y")

# add week categories
zimbabwe <- group_by(zimbabwe, week = floor_date(event_date, "week"))

# create week-aggregated dataset
zimbabwe_vars <- c("civilians",
                   "protests",
                   "riots")
zimbabwe_week <- aggregate(zimbabwe[, zimbabwe_vars],
                            by = list(zimbabwe$week), 
                            FUN = sum)

# create dummy for before and after mnangagwa's ascent to power
zimbabwe_week$mnangagwa <- ifelse(zimbabwe_week$Group.1 >=
                                     as.Date("11/19/2017", format = "%m/%d/%Y"), 1, 0)

# create dummy for assignment variable (weeks since coup)
zimbabwe_week$coup_time <- difftime(zimbabwe_week$Group.1, 
                                    as.Date("11/19/2017", format = "%m/%d/%Y"),
                                    units = "weeks")

# bivariate model for rate of violence change over time, without coup effect
zimbabwe_bivariate <- lm(civilians ~ coup_time, zimbabwe_week)
summary(zimbabwe_bivariate)

# varying slopes model for effect of time on violence against civilians, before and after mnangagwa
zimbabwe_coup <- lm(civilians ~ mnangagwa +
                      coup_time +
                      mnangagwa*coup_time, zimbabwe_week)
summary(zimbabwe_coup)

# varying slopes model controlling for protests and riots, before and after mnangagwa
zimbabwe_controls <- lm(civilians ~ mnangagwa +
                          coup_time +
                          mnangagwa*coup_time +
                          protests +
                          riots, zimbabwe_week)
summary(zimbabwe_controls)

# plot violence event data by week, using discontinuity plots
zimbabwe_before <- subset(zimbabwe_week, mnangagwa == 0)
zimbabwe_after <- subset(zimbabwe_week, mnangagwa == 1)

zimbabwe_before_bivariate <- lm(civilians ~ coup_time, zimbabwe_before)
summary(zimbabwe_before_bivariate)

zimbabwe_after_bivariate <- lm(civilians ~ coup_time, zimbabwe_after)
summary(zimbabwe_after_bivariate)

pdf("zimbabwe_diff.pdf")
plot(zimbabwe_week$coup_time, zimbabwe_week$civilians, 
     xlab = "", ylab = "",
     ylim = c(0, 20))
abline(v = 0, lty = 2)

# plot fitted regression lines before and after mnangagwa coup
ablineclip(zimbabwe_before_bivariate, x2 = 0, col = "blue")
ablineclip(zimbabwe_before_bivariate, x1 = 0, col = "blue",
           lty = 2)
ablineclip(zimbabwe_after_bivariate, x1 = 0, col = "red")
legend("topright", legend = c("November 2017 coup", "Mugabe", "Mugabe (counterfactual)", "Mnangagwa"),
       col = c("black", "blue", "blue", "red"), lty = c(2, 1, 2, 1), cex = 0.75, title = "Legend")
title(main = "Effect of November 2017 coup on violence in Zimbabwe",
      xlab = "Weeks since November 2017 coup",
      ylab = "Instances of violence against civilians")

# add footnote
scriptName <- "Data source: Armed Conflict Location and Event Data Project"
author <- ""
footnote <- paste(scriptName, format(Sys.time(), "%d %b %Y"), sep=" / ")
makeFootnote <- function(footnoteText=
                           format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(.5))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y= unit(2, "mm"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}

makeFootnote(footnote)
dev.off()

# plot violence event data controlling for the effect of protests and riots
zimbabwe_before_controls <- lm(civilians - protests - riots ~ coup_time, zimbabwe_before)
summary(zimbabwe_before_controls)

zimbabwe_after_controls <- lm(civilians - protests - riots ~ coup_time, zimbabwe_after)
summary(zimbabwe_after_controls)

# plot original data
pdf("zimbabwe_diff_controls.pdf")
plot(zimbabwe_week$coup_time, zimbabwe_week$civilians, 
     xlab = "", ylab = "",
     ylim = c(-20, 20),
     col = "lightgrey",
     pch = 1)
abline(v = 0, lty = 2)
ablineclip(zimbabwe_before_bivariate, x2 = 0, 
           col = "lightgrey")
ablineclip(zimbabwe_before_bivariate, x1 = 0, col = "lightgrey",
           lty = 2)
ablineclip(zimbabwe_after_bivariate, x1 = 0, col = "lightgrey")

# plot points and fitted lines with protest and riot controls
points(zimbabwe_week$coup_time, zimbabwe_week$civilians - 
         zimbabwe_week$protests - 
         zimbabwe_week$riots,
       pch = 19, cex = 0.75)
ablineclip(zimbabwe_before_controls, x2 = 0, col = "blue")
ablineclip(zimbabwe_before_controls, x1 = 0, col = "blue",
           lty = 2)
ablineclip(zimbabwe_after_controls, x1 = 0, col = "red")
legend("topright", legend = c("November 2017 coup", "Mugabe", "Mugabe (counterfactual)", "Mnangagwa"),
       col = c("black", "blue", "blue", "red"), lty = c(2, 1, 2, 1), cex = 0.75, title = "Legend")
title(main = "Effect of November 2017 coup with protest and riot controls",
      xlab = "Weeks since November 2017 coup",
      ylab = "Violence against civilians - (Protests + Riots)")

# add footnote
scriptName <- "Data source: Armed Conflict Location and Event Data Project"
author <- ""
footnote <- paste(scriptName, format(Sys.time(), "%d %b %Y"), sep=" / ")
makeFootnote <- function(footnoteText=
                           format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(.5))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y= unit(2, "mm"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}

makeFootnote(footnote)
dev.off()

# regression table for controls model
stargazer(zimbabwe_controls, 
          report = "vct*")

# coefficient dot-and-whisker plot
pdf("zimbabwe_regression_plot.pdf")
plot_summs(zimbabwe_controls, 
           coefs = c("mnangagwa", "protests", "riots"),
           scale = TRUE)
dev.off()
