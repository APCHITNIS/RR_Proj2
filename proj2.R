data <- read.csv("repdata-data-StormData.csv")
#Make a copy of dataframe for analysis and select the relevant columns 
dataMod <- data[, c("EVTYPE","FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]

#Find exponential indicators for Property damange  
unique(dataMod$PROPDMGEXP)

# Convert exponential indicators to actual values and store them in new PROPEXP column
# invalid indicators - ? + 
dataMod$PROPEXP[dataMod$PROPDMGEXP == "+"] <- 0
dataMod$PROPEXP[dataMod$PROPDMGEXP == "-"] <- 0
dataMod$PROPEXP[dataMod$PROPDMGEXP == "?"] <- 0
# valid numeral indicators 0 1 2 3 4 5 6 7 8 
dataMod$PROPEXP[dataMod$PROPDMGEXP == "0"] <- 1
dataMod$PROPEXP[dataMod$PROPDMGEXP == "1"] <- 10
dataMod$PROPEXP[dataMod$PROPDMGEXP == "2"] <- 100
dataMod$PROPEXP[dataMod$PROPDMGEXP == "3"] <- 1000
dataMod$PROPEXP[dataMod$PROPDMGEXP == "4"] <- 10000
dataMod$PROPEXP[dataMod$PROPDMGEXP == "5"] <- 100000
dataMod$PROPEXP[dataMod$PROPDMGEXP == "6"] <- 1000000
dataMod$PROPEXP[dataMod$PROPDMGEXP == "7"] <- 10000000
dataMod$PROPEXP[dataMod$PROPDMGEXP == "8"] <- 100000000
#valid alpha indicators B h H K m M
dataMod$PROPEXP[dataMod$PROPDMGEXP == "B"] <- 1e+09
dataMod$PROPEXP[dataMod$PROPDMGEXP == "h"] <- 100
dataMod$PROPEXP[dataMod$PROPDMGEXP == "H"] <- 100
dataMod$PROPEXP[dataMod$PROPDMGEXP == "K"] <- 1000
dataMod$PROPEXP[dataMod$PROPDMGEXP == "m"] <- 1e+06
dataMod$PROPEXP[dataMod$PROPDMGEXP == "M"] <- 1e+06

# Updating  PROPDMG column with actual values
dataMod$PROPDMG <- dataMod$PROPDMG * dataMod$PROPEXP
# Convert exponential indicators to actual values and store them in new CROPEXP column

#Find exponential indicators for Property damange  
unique(dataMod$CROPDMGEXP)

# invalid indicators ? 
dataMod$CROPEXP[dataMod$CROPDMGEXP == "?"] <- 0
# valid numeral indicators 0 2
dataMod$CROPEXP[dataMod$CROPDMGEXP == "0"] <- 1
dataMod$CROPEXP[dataMod$CROPDMGEXP == "2"] <- 100
#valid alpha indicators k K m M
dataMod$CROPEXP[dataMod$CROPDMGEXP == "k"] <- 1000
dataMod$CROPEXP[dataMod$CROPDMGEXP == "K"] <- 1000
dataMod$CROPEXP[dataMod$CROPDMGEXP == "m"] <- 1e+06
dataMod$CROPEXP[dataMod$CROPDMGEXP == "M"] <- 1e+06

# Updating  CROPDMG column with actual values
dataMod$CROPDMG <- dataMod$CROPDMG * dataMod$CROPEXP

#Use SQLDF package prepare dataframes for analysis 
library(sqldf)
#group records by EVTYPE and order by FATALITIE
dfFatalalies = sqldf("select EVTYPE, SUM(FATALITIES) as FATALITIES from `dataMod` group by EVTYPE order by FATALITIES desc")
#select top 10 rows for analysis
dfFatalalies=dfFatalalies[1:10,]
#group records by EVTYPE and order by INJURIES
dfInjuries = sqldf("select EVTYPE, SUM(INJURIES) as  INJURIES from `dataMod` group by EVTYPE order by INJURIES desc")
#select top 10 rows for analysis
dfInjuries=dfInjuries[1:10,] 

#group records by EVTYPE and order by PROPDMG - Property Damage
dfPropDmg = sqldf("select EVTYPE, SUM(PROPDMG) as PROPDMG from `dataMod` group by EVTYPE order by PROPDMG desc")
#select top 10 rows for analysis
dfPropDmg=dfPropDmg[1:10,]
dfPropDmg = transform(dfPropDmg, PROPDMG = PROPDMG / 1000000000)
#group records by EVTYPE and order by CROPDMG - Crop Damage
dfCropDmg = sqldf("select EVTYPE, SUM(CROPDMG) as CROPDMG from `dataMod` group by EVTYPE order by CROPDMG desc")
#select top 10 rows for analysis
dfCropDmg=dfCropDmg[1:10,]
dfCropDmg = transform(dfCropDmg, CROPDMG = CROPDMG / 1000000000)

#Plot Fatalities and Injuries plot together
par(mfrow = c(1, 2), mar = c(12, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
#Display the event with the highest fatalities in red and others in blue
cols <- c("blue", "red")[(dfFatalalies$FATALITIES == max(dfFatalalies$FATALITIES)) + 1]
barplot(dfFatalalies$FATALITIES, las = 3, names.arg = dfFatalalies$EVTYPE, main = "Events with Highest Fatalities", 
        ylab = "Number of fatalities", col = cols,  col.lab="black", cex.lab=0.75)

#Display the event with the highest Injuries in red and others in blue
cols <- c("blue", "red")[(dfInjuries$INJURIES == max(dfInjuries$INJURIES)) + 1]
barplot(dfInjuries$INJURIES, las = 3, names.arg = dfInjuries$EVTYPE, main = "Events with Highest Injuries", 
        ylab = "Number of Injuries", col = cols,  col.lab="black", cex.lab=0.75)


#Plot Property damage and Crop damage plots together
par(mfrow = c(1, 2), mar = c(12, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
cols <- c("blue", "red")[(dfPropDmg$PROPDMG == max(dfPropDmg$PROPDMG)) + 1]

barplot(dfPropDmg$PROPDMG, las = 3, names.arg = dfPropDmg$EVTYPE, main = "Events with Highest Property Damage", 
        ylab = "Damage in billions $", col = cols,  col.lab="black", cex.lab=0.75)

cols <- c("blue", "red")[(dfCropDmg$CROPDMG == max(dfCropDmg$CROPDMG)) + 1]
barplot(dfCropDmg$CROPDMG, las = 3, names.arg = dfCropDmg$EVTYPE, main = "Events with Highest Crom Damage", 
        ylab = "Damage in billions $", col = cols,  col.lab="black", cex.lab=0.75)


