data <- read.csv("repdata-data-StormData.csv")
#Make a copy of dataframe for analysis
dataMod <- data
#Use SQLDF package prepare dataframes for analysis 
library(sqldf)
#group records by EVTYPE and order by FATALITIE
dfFatalalies = sqldf("select EVTYPE, FATALITIES from `dataMod` group by EVTYPE order by FATALITIES desc")
#select top 10 rows for analysis
dfFatalalies=dfFatalalies[1:10,]
#group records by EVTYPE and order by INJURIES
dfInjuries = sqldf("select EVTYPE, INJURIES from `dataMod` group by EVTYPE order by INJURIES desc")
#select top 10 rows for analysis
dfInjuries=dfInjuries[1:10,]

#group records by EVTYPE and order by PROPDMGVAL - Property Damage
dfPropDmg = sqldf("select EVTYPE, PROPDMGVAL from `dataMod` group by EVTYPE order by PROPDMGVAL desc")
#select top 10 rows for analysis
dfPropDmg=dfPropDmg[1:10,]
#group records by EVTYPE and order by CROPDMGVAL - Crop Damage
dfCropDmg = sqldf("select EVTYPE, CROPDMGVAL from `dataMod` group by EVTYPE order by CROPDMGVAL desc")
#select top 10 rows for analysis
dfCropDmg=dfCropDmg[1:10,]

#Plot Fatalities and Injuries plot together
par(mfrow = c(1, 2), mar = c(12, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
#Display the event with the highest fatalities in red and others in blue
cols <- c("blue", "red")[(dfFatalalies$FATALITIES == max(dfFatalalies$FATALITIES)) + 1]
barplot(dfFatalalies$FATALITIES, las = 3, names.arg = dfFatalalies$EVTYPE, main = "Events with Highest Fatalities", 
        ylab = "Number of fatalities", col = cols,  col.lab="black", cex.lab=0.75)

#Display the event with the highest Injuries in red and others in blue
cols <- c("blue", "red")[(dfPropDmg$INJURIES == max(dfInjuries$INJURIES)) + 1]
barplot(dfInjuries$INJURIES, las = 3, names.arg = dfInjuries$EVTYPE, main = "Events with Highest Injuries", 
        ylab = "Number of Injuries", col = cols,  col.lab="black", cex.lab=0.75)


#Plot Property damage and Crop damage plots together
par(mfrow = c(1, 2), mar = c(12, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
cols <- c("blue", "red")[(dfPropDmg$PROPDMGVAL == max(dfPropDmg$PROPDMGVAL)) + 1]

barplot(dfPropDmg$PROPDMGVAL, las = 3, names.arg = dfPropDmg$EVTYPE, main = "Events with Highest Fatalities", 
        ylab = "Number of fatalities", col = cols,  col.lab="black", cex.lab=0.75)

cols <- c("blue", "red")[(dfInjuries$INJURIES == max(dfInjuries$INJURIES)) + 1]
barplot(dfInjuries$INJURIES, las = 3, names.arg = dfInjuries$EVTYPE, main = "Events with Highest Injuries", 
        ylab = "Number of Injuries", col = cols,  col.lab="black", cex.lab=0.75)


plot(x = dfFatalalies$EVTYPE, y = dfFatalalies$FATALITIES, type="h", main="Top 10 Fatalities per disaster" , xlab="Disaster", ylab="Fatalities", col="blue", lwd=5, par(las=3))

