# load required packages
libs <- c("lubridate","knitr","ggplot2","scales","gridExtra","tidyr","dplyr")
# check to see if packages are already installed
x <- sapply(libs,function(x) if(!require(x,character.only = T)) 
        install.packages(x))
rm(x,libs)

# check to see if file is already in working directory,
# download from url if needed
if(!file.exists("./stormData.csv")) {
        fileData <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        download.file(fileData, destfile = "./stormData.csv", method = "curl")
}
# read data into R
checkRawData <- read.csv("stormData.csv", sep = ",", header = TRUE, 
                         stringsAsFactors = FALSE)

# create a column of year values
checkRawData$year <- year(mdy_hms(checkRawData$BGN_DATE))
# create a temporary data frame to capture the subset of rows
# with year values after 1995
firstCut <- filter(checkRawData, year > 1995)
# show the changes to the data frame resulting from subsetting
structure <- dim(firstCut)
paste("Data frame is", structure[1], "rows and", structure[2], "columns")
names(firstCut)

# create dat frame with only essential columns,
# and rename to columns
stormData <- select(firstCut, year, evtype = EVTYPE, fatalities = FATALITIES,
                    injuries = INJURIES, propdmg = PROPDMG, propdmgexp =
                            PROPDMGEXP, cropdmg = CROPDMG, cropdmgexp = CROPDMGEXP, 
                    state = STATE, county = COUNTYNAME)
# remove the two unneeded data frame to free up memory
rm(checkRawData)
rm(firstCut)
# show a summary of the new data frame
str(stormData)
# show the number of unique entries in evtype, propdmgexp, 
# and cropdmgexp
uniqueEvtype <- length(unique(stormData$evtype))
uniquePropdmgexp <- length(unique(stormData$propdmgexp))
uniqueCropdmgexp <- length(unique(stormData$cropdmgexp))
paste("evtype has", uniqueEvtype[1], "unique values")
paste("propdmgexp has", uniquePropdmgexp[1], "unique values")
paste("cropdmgexp has", uniqueCropdmgexp[1], "unique values")
# show the sorted list of unique values for propdmgexp and cropdmgexp
sort(unique(stormData$propdmgexp))
sort(unique(stormData$cropdmgexp))

# convert all characters in evtype to upper case
stormData$evtype <- toupper(stormData$evtype)
# remove leading or trailing whitespace from evtype
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
stormData$evtypeT <- trim(stormData$evtype)
# show the sorted list of unique values for evtype
paste("Dataframe contains", length(sort(unique(stormData$evtypeT))), "unique events.")

# create the column casualties
stormData <- mutate(stormData, casualties = injuries + fatalities)
# filter the data to keep only rows where casualties 
# is greater than zero
stormPopHealth <- filter(stormData, casualties > 0)
# show the structure of the new data frame
structure2 <- dim(stormPopHealth)
paste("Data frame is", structure2[1], "rows and", structure2[2], "columns")
sort(unique(stormPopHealth$evtype))

# create the column dollars, and filter the data frame to keep only
# rows with values of dollar greater than zero
stormData <- mutate(stormData, dollars = cropdmg + propdmg)
stormDamage <- filter(stormData, dollars > 0)
# show the list of unique values in propdmgexp and cropdmgexp
sort(unique(stormDamage$propdmgexp))
sort(unique(stormDamage$cropdmgexp))
# transform the character strings for damage exponents to numbers
# expressed in billions = number/10^9
stormDamage$propExp[stormDamage$propdmgexp == "0"] <- 0
stormDamage$propExp[stormDamage$propdmgexp == ""] <- 0
stormDamage$propExp[stormDamage$propdmgexp == "K"] <- 10^3/10^9
stormDamage$propExp[stormDamage$propdmgexp == "M"] <- 10^6/10^9
stormDamage$propExp[stormDamage$propdmgexp == "B"] <- 10^9/10^9
stormDamage$cropExp[stormDamage$cropdmgexp == ""] <- 0
stormDamage$cropExp[stormDamage$cropdmgexp == "K"] <- 10^3/10^9
stormDamage$cropExp[stormDamage$cropdmgexp == "M"] <- 10^6/10^9
stormDamage$cropExp[stormDamage$cropdmgexp == "B"] <- 10^9/10^9
# create columns cropDmgD and propDmgD
stormDamage <- mutate(stormDamage, cropDmgD = cropdmg * cropExp, propDmgD = propdmg * propExp)

# subset stormDamage to capture only rows with propdmfexp = "B"
outlierCheck <- stormDamage[stormDamage$propdmgexp %in% c("B"),]
# sort into descending order based on total property damages
outlierCheck <- arrange(stormDamage, desc(propDmgD))
# select columns to display
outlierCheck <- select(outlierCheck, year, evtypeT, propdmg, propdmgexp, state, county)
head(outlierCheck, 5)

# select row with proper criteria
fix <- which(stormDamage$propdmg == 115 & stormDamage$propdmgexp == "B")
# substitute corrected values in data
stormDamage$propdmgexp[fix] <- "M"
stormDamage$propExp[fix] <- 10^6/10^9
stormDamage$propDmgD[fix] <- stormDamage$propdmg[fix] * stormDamage$propExp[fix]
# review new property damage rankings
outlierCheck <- stormDamage[stormDamage$propdmgexp %in% c("B"),]
outlierCheck <- select(outlierCheck, year, evtypeT, propdmg, propdmgexp, state, county)
head(outlierCheck, 5)

outlierCheck2 <- stormDamage[stormDamage$cropdmgexp %in% c("B"),]
outlierCheck2 <- arrange(outlierCheck2, desc(cropDmgD))
outlierCheck2 <- select(outlierCheck2, year, evtypeT, cropdmg, cropdmgexp, state, county)
head(outlierCheck2, 5)

# create a data frame of unique evtypes for the stormDamage data
stormDmgEV <- data.frame(sort(unique(stormDamage$evtypeT)))
names(stormDmgEV) <- "evType"
# create a data frame of unique evtypes for the population health data
popHealthEV<- data.frame(sort(unique(stormPopHealth$evtypeT)))
names(popHealthEV) <- "evType"
# combine into one data frame of unique evtypes
combinedEV <- bind_rows(stormDmgEV, popHealthEV)
evMatched <- data.frame(sort(unique(combinedEV$evType)))
names(evMatched) <- "evtype"
paste("There are", nrow(evMatched), "unique observations in the relevant data.")

### create data frame for fixed evtypes
evMatched2 <- data.frame(evtypeD = evMatched,
                         fixedEvent = NA, stringsAsFactors = FALSE)
evMatched2$fixedEvent[agrep("ASTRONOMICAL LOW", evMatched2$evtype)] <- "ASTRONOMICAL LOW TIDE"
evMatched2$fixedEvent[grep("AVALANCHE", evMatched2$evtype)] <- "AVALANCHE"
evMatched2$fixedEvent[agrep("BLIZZARD", evMatched2$evtype, max = 1)] <- "BLIZZARD"
evMatched2$fixedEvent[grep("FLOOD", evMatched2$evtype)] <- "FLOOD"
evMatched2$fixedEvent[agrep("COASTAL FLOOD", evMatched2$evtype, max = 3)] <- "COASTAL FLOOD"
evMatched2$fixedEvent[agrep("COASTAL STORM", evMatched2$evtype)] <- "HEAVY RAIN"
evMatched2$fixedEvent[agrep("DEBRIS FLOW", evMatched2$evtype, max = 1)] <- "DEBRIS FLOW"
evMatched2$fixedEvent[agrep("DENSE FOG", evMatched2$evtype, max = 1)] <- "DENSE FOG"
evMatched2$fixedEvent[agrep("DROUGHT", evMatched2$evtype, max = 1)] <- "DROUGHT"
evMatched2$fixedEvent[grep("DUST DEVIL", evMatched2$evtype)] <- "DUST DEVIL"
evMatched2$fixedEvent[grep("DUST STORM", evMatched2$evtype)] <- "DUST STORM"
evMatched2$fixedEvent[grep("E HEAT", evMatched2$evtype)] <- "EXCESSIVE HEAT"
evMatched2$fixedEvent[grep("EXTREME COLD/WIND CHILL", evMatched2$evtype)] <- "EXTREME COLD/WIND CHILL"
evMatched2$fixedEvent[agrep("FLASH/", evMatched2$evtype)] <- "FLASH FLOOD"
evMatched2$fixedEvent[agrep("FLASH ", evMatched2$evtype)] <- "FLASH FLOOD"
evMatched2$fixedEvent[grep("FOG", evMatched2$evtype)] <- "DENSE FOG"
evMatched2$fixedEvent[grep("FREEZE", evMatched2$evtype)] <- "FROST/FREEZE"
evMatched2$fixedEvent[agrep("FUNNEL CLOUD", evMatched2$evtype, max = 1)] <- "FUNNEL CLOUD"
evMatched2$fixedEvent[agrep("FREEZING FOG", evMatched2$evtype, max = 1)] <- "FREEZING FOG"
evMatched2$fixedEvent[agrep("HEAT", evMatched2$evtype, max = 0)] <- "HEAT"
evMatched2$fixedEvent[agrep("HEAVY RAIN", evMatched2$evtype, max = 1)] <- "HEAVY RAIN"
evMatched2$fixedEvent[agrep("SURF", evMatched2$evtype, max = 1)] <- "HIGH SURF"
evMatched2$fixedEvent[agrep("HIGH WIND", evMatched2$evtype, max = 1)] <- "HIGH WIND"
evMatched2$fixedEvent[agrep("HURRICANE", evMatched2$evtype, max = 3)] <- "HURRICANE (TYPHOON)"
evMatched2$fixedEvent[agrep("ICE STORM", evMatched2$evtype, max = 1)] <- "ICE STORM"
evMatched2$fixedEvent[agrep("LAKESHORE FLOOD", evMatched2$evtype, max = 1)] <- "LAKESHORE FLOOD"
evMatched2$fixedEvent[agrep("LIGHTNING", evMatched2$evtype, max = 0)] <- "LIGHTNING"
evMatched2$fixedEvent[agrep("MARINE HAIL", evMatched2$evtype, max = 1)] <- "MARINE HAIL"
evMatched2$fixedEvent[agrep("MARINE HIGH WIND", evMatched2$evtype, max = 0)] <- "MARINE HIGH WIND"
evMatched2$fixedEvent[agrep("MARINE STRONG WIND", evMatched2$evtype, max = 1)] <- "MARINE STRONG WIND"
evMatched2$fixedEvent[agrep("MARINE THUNDERSTORM WIND", evMatched2$evtype, max = 1)] <- "MARINE THUNDERSTORM WIND"
evMatched2$fixedEvent[agrep("RIP CURRENT", evMatched2$evtype, max = 1)] <- "RIP CURRENT"
evMatched2$fixedEvent[agrep("SEICHE", evMatched2$evtype, max = 1)] <- "SEICHE"
evMatched2$fixedEvent[agrep("SLEET", evMatched2$evtype, max = 1)] <- "SLEET"
evMatched2$fixedEvent[agrep("STORM SURGE/TIDE", evMatched2$evtype, max = 1)] <- "STORM SURGE/TIDE"
evMatched2$fixedEvent[agrep("TORNADO", evMatched2$evtype, max = 0)] <- "TORNADO"
evMatched2$fixedEvent[agrep("TROPICAL DEPRESSION", evMatched2$evtype, max = 1)] <- "TROPICAL DEPRESSION"
evMatched2$fixedEvent[agrep("TROPICAL STORM", evMatched2$evtype, max = 1)] <- "TROPICAL STORM"
evMatched2$fixedEvent[agrep("VOLCANIC ASH", evMatched2$evtype, max = 1)] <- "VOLCANIC ASH"

evMatched2$fixedEvent[agrep("FIRE", evMatched2$evtype, max = 0)] <- "WILDFIRE"
evMatched2$fixedEvent[agrep("WINTER STORM", evMatched2$evtype, max = 1)] <- "WINTER STORM"
evMatched2$fixedEvent[agrep("WINTER WEATHER", evMatched2$evtype, max = 1)] <- "WINTER WEATHER"
evMatched2$fixedEvent[agrep("EROSION", evMatched2$evtype, max = 1)] <- "COASTAL FLOOD"
evMatched2$fixedEvent[agrep("BLOWING SNOW", evMatched2$evtype)] <- "WINTER STORM"
evMatched2$fixedEvent[agrep("EXTREME COLD", evMatched2$evtype, max = 0)] <- "EXTREME COLD/WIND CHILL"
evMatched2$fixedEvent[agrep("WINTRY MIX", evMatched2$evtype)] <- "WINTER WEATHER"
evMatched2$fixedEvent[grep("DAM BREAK", evMatched2$evtype)] <- "FLOOD"
evMatched2$fixedEvent[agrep("ASTRONOMICAL HIGH", evMatched2$evtype)] <- "STORM SURGE/TIDE"
evMatched2$fixedEvent[agrep(" DUST", evMatched2$evtype, max = 0)] <- "DUST STORM"
evMatched2$fixedEvent[grep("DOWNBURST", evMatched2$evtype)] <- "THUNDERSTORM WIND"
evMatched2$fixedEvent[agrep("FROST", evMatched2$evtype, max = 1)] <- "FROST/FREEZE"
evMatched2$fixedEvent[agrep("MICROBURST", evMatched2$evtype, max = 1)] <- "HIGH WIND"
evMatched2$fixedEvent[grep("EXCESSIVE S", evMatched2$evtype)] <- "HEAVY SNOW"
evMatched2$fixedEvent[grep("DRIZZLE", evMatched2$evtype)] <- "ICE STORM"
evMatched2$fixedEvent[grep("GLAZE", evMatched2$evtype)] <- "ICE STORM"
evMatched2$fixedEvent[grep("REEZING RAI", evMatched2$evtype)] <- "ICE STORM"
evMatched2$fixedEvent[grep("HVY RAIN", evMatched2$evtype)] <- "HEAVY RAIN"
evMatched2$fixedEvent[agrep("/RAIN", evMatched2$evtype, max = 1)] <- "HEAVY RAIN"
evMatched2$fixedEvent[agrep("GRADIENT", evMatched2$evtype, max = 0)] <- "GUSTY WIND"
evMatched2$fixedEvent[grep("EXTENDED", evMatched2$evtype)] <- "EXTREME COLD/WIND CHILL"
evMatched2$fixedEvent[agrep("SEAS", evMatched2$evtype, max = 1)] <- "HIGH SURF"
evMatched2$fixedEvent[agrep("SWELLS", evMatched2$evtype, max = 1)] <- "HIGH SURF"
evMatched2$fixedEvent[agrep("TYPHOON", evMatched2$evtype, max = 3)] <- "HURRICANE (TYPHOON)"
evMatched2$fixedEvent[agrep("JAM", evMatched2$evtype, max = 1)] <- "FLOOD"
evMatched2$fixedEvent[agrep("ROADS", evMatched2$evtype, max = 1)] <- "ICE STORM"
evMatched2$fixedEvent[agrep("SLIDE", evMatched2$evtype, max = 1)] <- "DEBRIS FLOW"
evMatched2$fixedEvent[agrep("SLUMP", evMatched2$evtype, max = 1)] <- "DEBRIS FLOW"
evMatched2$fixedEvent[agrep("TSTM", evMatched2$evtype, max = 1)] <- "THUNDERSTORM WIND"
evMatched2$fixedEvent[grep("LANDSPOUT", evMatched2$evtype)] <- "TORNADO"
evMatched2$fixedEvent[grep("LATE", evMatched2$evtype)] <- "WINTER STORM"
evMatched2$fixedEvent[grep("DAMAGE", evMatched2$evtype)] <- "HIGH WIND"
evMatched2$fixedEvent[grep("MARINE", evMatched2$evtype)] <- "MARINE HIGH WIND"
evMatched2$fixedEvent[agrep("UNSEASONABLE COLD", evMatched2$evtype, max = 1)] <- "COLD/WIND CHILL"
evMatched2$fixedEvent[agrep("UNSEASONAL", evMatched2$evtype, max = 0)] <- "HEAVY RAIN"
evMatched2$fixedEvent[grep("WARM", evMatched2$evtype)] <- "HEAT"
evMatched2$fixedEvent[grep("MIXED", evMatched2$evtype)] <- "SLEET"
evMatched2$fixedEvent[grep("TIDAL", evMatched2$evtype)] <- "STORM SURGE/TIDE"
evMatched2$fixedEvent[grep("RIVER", evMatched2$evtype)] <- "FLOOD"
evMatched2$fixedEvent[grep("SQUALL", evMatched2$evtype)] <- "WINTER STORM"
evMatched2$fixedEvent[grep("LIGHT ", evMatched2$evtype)] <- "WINTER STORM"
evMatched2$fixedEvent[agrep("ACCIDENT ", evMatched2$evtype)] <- "MARINE HIGH WIND"
evMatched2$fixedEvent[grep("ICE", evMatched2$evtype)] <- "ICE STORM"
evMatched2$fixedEvent[grep("DROWNING", evMatched2$evtype)] <- "RIP CURRENT"
evMatched2$fixedEvent[grep("TORRENTIAL", evMatched2$evtype)] <- "HEAVY RAIN"
evMatched2$fixedEvent[grep("TSUNAMI", evMatched2$evtype)] <- "TSUNAMI"
evMatched2$fixedEvent[agrep("SMOKE", evMatched2$evtype, max = 1)] <- "DENSE SMOKE"
evMatched2$fixedEvent[agrep("WATER", evMatched2$evtype, max = 1)] <- "FLOOD"
evMatched2$fixedEvent[agrep("/SNOW", evMatched2$evtype, max = 0)] <- "SLEET"
evMatched2$fixedEvent[agrep("RAIN", evMatched2$evtype, max = 0)] <- "HEAVY RAIN"
evMatched2$fixedEvent[grep("WIND", evMatched2$evtype)] <- "STRONG WIND"
evMatched2$fixedEvent[agrep("WINDS", evMatched2$evtype, max = 0)] <- "HIGH WIND"
evMatched2$fixedEvent[agrep("COLD ", evMatched2$evtype, max = 1)] <- "COLD"
evMatched2$fixedEvent[grep("COLD/WIND CHILL", evMatched2$evtype)] <- "COLD/WIND CHILL"
evMatched2$fixedEvent[grep("WHIRL", evMatched2$evtype)] <- "DUST DEVIL"
evMatched2$fixedEvent[agrep("STRONG WIND", evMatched2$evtype, max = 1)] <- "STRONG WIND"
evMatched2$fixedEvent[agrep("THUNDERSTORM WIND", evMatched2$evtype, max = 0.5)] <- "THUNDERSTORM WIND"
evMatched2$fixedEvent[agrep("GUSTY", evMatched2$evtype, max = 1)] <- "GUSTY WIND"
evMatched2$fixedEvent[grep("WINDCHILL", evMatched2$evtype)] <- "EXTREME COLD/WIND CHILL"
evMatched2$fixedEvent[grep("SPRAY", evMatched2$evtype)] <- "FREEZING RAIN"
evMatched2$fixedEvent[grep("DAMAGING", evMatched2$evtype)] <- "FROST/FREEZE"
evMatched2$fixedEvent[grep("HYPER", evMatched2$evtype)] <- "EXTREME COLD/WIND CHILL"
evMatched2$fixedEvent[grep("HYPO", evMatched2$evtype)] <- "EXCESSIVE HEAT"
evMatched2$fixedEvent[grep("WATERSPOUT", evMatched2$evtype)] <- "WATERSPOUT"
evMatched2$fixedEvent[agrep("HAIL", evMatched2$evtype, max = 1)] <- "HAIL"
evMatched2$fixedEvent[grep("NON", evMatched2$evtype)] <- "HIGH WIND"
evMatched2$fixedEvent[agrep("ROGUE", evMatched2$evtype, max = 1)] <- "HIGH SURF"
evMatched2$fixedEvent[agrep("SNOW", evMatched2$evtype, max = 1)] <- "WINTER STORM"
evMatched2$fixedEvent[grep("HEAVY SNOW", evMatched2$evtype)] <- "HEAVY SNOW"
evMatched2$fixedEvent[agrep("LAKE-EFFECT", evMatched2$evtype, max = 1)] <- "LAKE-EFFECT SNOW"
        # rename column and coerce to character type
evMatched2 <- rename(evMatched2, evtypeT = evtype)
evMatched2$evtypeT <- as.character(evMatched2$evtypeT)
stormDamage <- merge(stormDamage, evMatched2, by.x = "evtypeT", by.y = "evtypeT")
stormPopHealth <- merge(stormPopHealth, evMatched2, by.x = "evtypeT", by.y = "evtypeT")
paste(length(sort(unique(stormDamage$fixedEvent))), "unique events in stormDamage dataframe")
paste(length(sort(unique(stormPopHealth$fixedEvent))), "unique events in stormPopHealth dataframe")
paste("The total numbers of observations selected for analysis:", nrow(stormDamage), "storm damage events, and", nrow(stormPopHealth), "storm population health events.")

# summarize the damages and casualties by fixedEvent
damageSum <- summarize(group_by(stormDamage, fixedEvent), 
                       allCrop = sum(cropDmgD),
                       allProperty = sum(propDmgD))
casualitySum <- summarize(group_by(stormPopHealth, fixedEvent),
                          allFatality = sum(fatalities),
                          allInjury = sum(injuries),
                          allCasualities = sum(casualties))
# create ordered dataframes
cropDamageSummary <- slice(select(arrange(damageSum, desc(allCrop)),
                        event = fixedEvent, cropDamage = allCrop), 1:10)
propertyDamageSummary <- slice(select(arrange(damageSum, desc(allProperty)),
                        event = fixedEvent, propertyDamage = allProperty), 1:10)
damageSummary <- slice(select(arrange(damageSum, desc(allCrop + allProperty)), 
                        event = fixedEvent, crop = allCrop,
                        property = allProperty), 1:10)
injurySummary <- slice(select(arrange(casualitySum, desc(allInjury)),
                        event = fixedEvent, injuries = allInjury), 1:10)
fatalitySummary <- slice(select(arrange(casualitySum, desc(allFatality)), 
                        event = fixedEvent, fatalities = allFatality), 1:10)
casualitySummary <- slice(select(arrange(casualitySum, desc(allCasualities)), 
                        event = fixedEvent, casualities = allCasualities), 1:10)

cropDamageSummary
propertyDamageSummary
damageSummary
injurySummary
fatalitySummary
casualitySummary

injury <- ggplot(injurySummary[1:10,], aes(x=reorder(event, desc(injuries)), y=injuries)) +
        geom_bar(stat = "identity", fill = "dark green", color = "blue") +
        labs(x = "weather event", y = "injuries", 
             title = "Weather Events Causing Most Injuries") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))

fatality <- ggplot(fatalitySummary[1:10,], aes(x = reorder(event, desc(fatalities)),
                        y = fatalities)) +
        geom_bar(stat = "identity", fill = "orange", color = "blue") +
        labs(x = "weather event", y = "fatalities", 
             title = "Weather Events Causing Most Fatalities") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))

grid.arrange(injury, fatality, ncol = 2)

gatherDamages <- gather(damageSummary, "damage_type", "USD_in_billions",
                        crop:property)

damages <- ggplot(gatherDamages, 
                  aes(x = reorder(event, desc(USD_in_billions)),
                      y = USD_in_billions, fill = damage_type)) +
                geom_bar(stat = "identity", color = "blue") +
                labs(x = "weather event", y = "USD, billions", 
                 title = "Weather Events Causing Most Monetary Damages") +
                theme(axis.text.x = element_text(angle = 60, hjust = 1))
print(damages)

