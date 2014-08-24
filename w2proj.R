## download and extract the data 
if (!file.exists("./data")) {
  dir.create("./data")
  fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(fileURL, destfile = "./data/dataset.bz2", method = "curl", mode = "wb")
}
# note: this next line will take a *very* long time to execute (5 or more minutes)
# ... go make a cup of coffee ...
storm <- read.csv(bzfile("./data/dataset.bz2", "r"))

dim(storm)
na.test <-c(anyNA(storm$EVTYPE),anyNA(storm$FATALITIES), anyNA(storm$INJURIES),
anyNA(storm$PROPDMG),anyNA(storm$PROPDMGEXP),
anyNA(storm$CROPDMG), anyNA(storm$CROPDMGEXP))
na.test

levels(storm$PROPDMGEXP)
levels(storm$CROPDMGEXP)

nrow(storm[!(storm$PROPDMGEXP %in% c("K", "M", "B", "k", "m", "b", "")),])
nrow(storm[!(storm$CROPDMGEXP %in% c("K", "M", "B", "k", "m", "b", "")),])

# get rid of bad EXP values
storm <- storm[(storm$PROPDMGEXP %in% c("K", "M", "B", "k", "m", "b", "")),]
storm <- storm[(storm$CROPDMGEXP %in% c("K", "M", "B", "k", "m", "b", "")),]
nrow(storm)
# multiply the PROPDMG and CROPDMG fields by their EXP values
storm$PROPMULT <- 1
storm$PROPMULT[storm$PROPDMGEXP %in% c("K", "k")] <- 1000
storm$PROPMULT[storm$PROPDMGEXP %in% c("M", "m")] <- 1000000
storm$PROPMULT[storm$PROPDMGEXP %in% c("B", "b")] <- 1000000000
storm$PROPDMG <- storm$PROPDMG * storm$PROPMULT
storm$CROPMULT <- 1
storm$CROPMULT[storm$CROPDMGEXP %in% c("K", "k")] <- 1000
storm$CROPMULT[storm$CROPDMGEXP %in% c("M", "m")] <- 1000000
storm$CROPMULT[storm$CROPDMGEXP %in% c("B", "b")] <- 1000000000
storm$CROPDMG <- storm$CROPDMG * storm$CROPMULT
storm$TOTALDMG <- storm$PROPDMG + storm$CROPDMG

# load required libraries for analysis
stopifnot(require(dplyr))
stopifnot(require(ggplot2))

# find weather events with fatalities
f <- summarise(group_by(storm, EVTYPE), sum(FATALITIES))
names(f)[2] <- "Fatalities"      # clean up the name
f <- f[f$Fatalities != 0, ]      # keep only events with nonzero fatalities

#find weather events with injuries
i <- summarise(group_by(storm, EVTYPE), sum(INJURIES))
names(i)[2] <- "Injuries"        # clean up the name
i <- i[i$Injuries != 0, ]        # keep only events with nonzero injuries

f <- f[order(-f$Fatalities), ]   # sort in descending order by fatalities
head(f, 10)

i <- i[order(-i$Injuries), ]     # sort in descending order by injuries
head(i, 10)

# merge the two together
harm <- merge(f, i, by.x = "EVTYPE", by.y = "EVTYPE", all=TRUE)
harm[is.na(harm$Fatalities),][, 2] <- 0
harm[is.na(harm$Injuries),][, 3] <- 0
harm$Total <- harm$Fatalities + harm$Injuries

# display the top 
harm <- harm[order(-harm$Total, harm$EVTYPE), ]
head(harm, 10)

# display harm as a barchart
harm$Total <- harm$Total/1000  # scale it down to make it easier to understand in the chart
ggplot(data=head(harm, 10), aes(x=reorder(EVTYPE, -Total), y=Total)) + 
geom_bar(stat="identity", fill="red") +
theme(axis.text.x = element_text(angle=20, hjust=1)) +
xlab("Weather Event Type") +
ylab("Total Casualties (Fatalities + Injuries) in Thousands") +
ggtitle("Total Number of Casualties per Weather Event Type")

# find weather events that caused property damage
p <- summarise(group_by(storm, EVTYPE), sum(PROPDMG))
names(p)[2] <- "PropertyDamageBillions"     # clean up the name
p <- p[p$PropertyDamageBillions != 0, ]     # keep only events with nonzero damage events
p$PropertyDamageBillions <- p$PropertyDamageBillions/1000000000  # scale down to billions for readability

#find weather events that caused crop damage
c <- summarise(group_by(storm, EVTYPE), sum(CROPDMG))
names(c)[2] <- "CropDamageBillions"         # clean up the name
c <- c[c$CropDamageBillions != 0, ]         # keep only events with nonzero damage events
c$CropDamageBillions <- c$CropDamageBillions/1000000000  # scale down to billions for readability

p <- p[order(-p$PropertyDamageBillions), ]   # sort in descending order by fatalities
head(p, 10)

c <- c[order(-c$CropDamageBillions), ]   # sort in descending order by fatalities
head(c, 10)

# merge the two together
cost <- merge(p, c, by.x = "EVTYPE", by.y = "EVTYPE", all=TRUE)
cost[is.na(cost$PropertyDamageBillions),][, 2] <- 0
cost[is.na(cost$CropDamageBillions),][, 3] <- 0
cost$Total <- (cost$PropertyDamageBillions + cost$CropDamageBillions)
#cost$TotalBillions <- format(cost$Total, digits=4, justify="right", scientific=FALSE)

# display the top 
cost <- cost[order(-cost$Total, cost$EVTYPE), ]
head(cost, 10)

# display cost as a barchart
ggplot(data=head(cost, 10), aes(x=reorder(EVTYPE, -Total), y=Total)) + 
  geom_bar(stat="identity", fill="blue") +
  theme(axis.text.x = element_text(angle=20, hjust=1)) +
  xlab("Weather Event Type") +
  ylab("Total Damages (Billions of US Dollars)") +
  ggtitle("Total Amount of Damages per Weather Event Type")

