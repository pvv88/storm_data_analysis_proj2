
setwd("C:/Users/Dell/Downloads")
sourceURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(sourceURL,'rawdata_sda.csv.bz2')
stormData <- read.csv("rawdata_sda.csv.bz2", na.strings = "NA")
library(plyr)
library(ggplot2)
library(grid)

stormData$CASUALTIES <- stormData$INJURIES +stormData$FATALITIES
casualties <- with(stormData,aggregate(CASUALTIES, by= list(EVTYPE),sum))
casualties <- head(arrange(casualties, desc(x)),10)
casualties <- arrange(casualties,x)

stormData$PROPDMGEXP <- toupper(stormData$PROPDMGEXP)
stormData$CROPDMGEXP <- toupper(stormData$CROPDMGEXP)
stormData$PROPDMGEXP[stormData$PROPDMGEXP %in% c("", "+", "0","-","?")] <- 0
stormData$CROPDMGEXP[stormData$CROPDMGEXP %in% c("", "+", "0","-","?")] <- 0
stormData$PROPDMGEXP[stormData$PROPDMGEXP == "B"] <- 9
stormData$CROPDMGEXP[stormData$CROPDMGEXP == "B"] <- 9
stormData$PROPDMGEXP[stormData$PROPDMGEXP == "M"] <- 6
stormData$CROPDMGEXP[stormData$CROPDMGEXP == "M"] <- 6
stormData$PROPDMGEXP[stormData$PROPDMGEXP == "K"] <- 3
stormData$CROPDMGEXP[stormData$CROPDMGEXP == "K"] <- 3
stormData$PROPDMGEXP[stormData$PROPDMGEXP == "H"] <- 2
stormData$CROPDMGEXP[stormData$CROPDMGEXP == "H"] <- 2

stormData$PROPDMGEXP <- 10^(as.numeric(stormData$PROPDMGEXP))
stormData$CROPDMGEXP <- 10^(as.numeric(stormData$CROPDMGEXP))
stormData$DAMAGES <- stormData$PROPDMG * stormData$PROPDMGEXP + stormData$CROPDMG * stormData$CROPDMGEXP
stormData$PDAMAGES <- stormData$PROPDMG * stormData$PROPDMGEXP
stormData$CDAMAGES <- stormData$CROPDMG * stormData$CROPDMGEXP

pdamages <- with(stormData,aggregate(PDAMAGES/10^7, by= list(EVTYPE),sum))
pdamages <- head(arrange(pdamages, desc(x)),10)
pdamages <- arrange(pdamages,x)
cdamages <- with(stormData,aggregate(CDAMAGES/10^7, by= list(EVTYPE),sum))
cdamages <- head(arrange(cdamages, desc(x)),10)
cdamages <- arrange(cdamages,x)
damages <- with(stormData,aggregate(DAMAGES/10^7, by= list(EVTYPE),sum))
damages <- head(arrange(damages, desc(x)),10)
damages <- arrange(damages,x)

colnames(casualties) <- c("Type", "Count")
options(scipen = 1)
par(mar= c(5,5,5,5), mgp = c(3.1,0.3,0))
barplot(casualties$Count,
        names.arg = casualties$Type,
        xlab = "Casualties",
        ylab = "Event Types",
        horiz = TRUE,
        cex.names = 0.5,
        cex.axis = 0.8, legend.text = TRUE,
        las = 1,
        col = "darkred",
        main = "Top 10 Casualties from Weather Events(1950-2011)",
        xlim = c(0,100000))
mtext(" Figure 1. Tornado has the most casualties at 9,6979",
      side=3,
      outer=T,
      adj=0.5,
      line=-5, 
      font = 3,
      cex = 0.8) 

colnames(damages) <- c("Type", "Count")
par(mar= c(5,5,5,5), mgp = c(3.5,0.3,0))
barplot(damages$Count,
        names.arg = damages$Type,
        xlab = "Damages",
        ylab = "Event Types",
        horiz = TRUE,
        cex.names = 0.5,
        cex.axis = 0.8,
        las = 1,
        col = "darkred",
        main = "Top 10 Events with greatest economic consequences (1950-2011)",
        xlim = c(0,16000))  
mtext(" Figure 2. Flood inflicts the most damage to economic problem",
      side=3,
      outer=T,
      adj=0.5,
      line=-5, 
      font = 3,
      cex = 0.8) 

colnames(pdamages) <- c("Type", "Count")
par(mfrow = c(1, 2), mar = c(5, 4, 2, 2), oma = c(1.5, 2, 1, 1))
barplot(pdamages$Count,
        names.arg = pdamages$Type,
        xlab = "Prop",
        horiz = TRUE,
        cex.names = 0.5,
        cex.axis = 0.8,
        las = 1,
        col = "darkred",
        xlim = c(0,16000))  
colnames(cdamages) <- c("Type", "Count")
barplot(cdamages$Count,
        names.arg = cdamages$Type,
        xlab = "Crop",
        horiz = TRUE,
        cex.names = 0.5,
        cex.axis = 0.8,
        las = 1,
        col = "darkred",
        xlim = c(0,2000))  
mtext(" Figure 3. Flood is highest in property damage while Drought in Crop damage",
      side=3,
      outer=T,
      adj=0.5,
      line=-2, 
      font = 3,
      cex = 0.8) 
title(main = "Top 10 Events Breakdown by Property and Crop", outer = TRUE)
