## download and extract the data 
if (!file.exists("./data")) {
  dir.create("./data")
  fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(fileURL, destfile = "./data/dataset.bz2", method = "curl", mode = "wb")
}
# note: this next line will take a *very* long time to execute (5 or more minutes)
# ... go make a cup of coffee ...
if(exists(storm) == FALSE) {
  storm <- read.csv(bzfile("./data/dataset.bz2", "r"))
} 

dim(storm)
na.test <-c(anyNA(storm$EVTYPE),anyNA(storm$FATALITIES), anyNA(storm$INJURIES),
            anyNA(storm$PROPDMG),anyNA(storm$PROPDMGEXP),
            anyNA(storm$CROPDMG), anyNA(storm$CROPDMGEXP))
na.test

levels(storm$PROPDMGEXP)
levels(storm$CROPDMGEXP)

nrow(storm[!(storm$PROPDMGEXP %in% c("K", "M", "B", "k", "m", "b", "")),])
nrow(storm[!(storm$CROPDMGEXP %in% c("K", "M", "B", "k", "m", "b", "")),])

# view the data before.  Note the multiplier and total columns don't exist yet
head(storm[, c(24:27)])
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
# view the data after
head(storm[, c(24:27,38:40)])

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
harm <- harm[order(-harm$Total, harm$EVTYPE), ]

# display the top 
head(harm, 10)

# display harm as a barchart
h <- head(harm, 10)
ggplot(data=head(harm, 10), aes(x=reorder(EVTYPE, -Total), y=Total)) + 
  geom_bar(stat="identity", fill="red") +
  theme(axis.text.x = element_text(angle=20, hjust=1)) +
  xlab("Weather Event Type") +
  ylab("Total Casualties (Fatalities + Injuries)") +
  ggtitle("Total Number of Casualties per Weather Event Type")

# find weather events that caused property damage
p <- summarise(group_by(storm, EVTYPE), sum(PROPDMG))
names(p)[2] <- "PDamage"      # clean up the name
p <- p[p$PDamage != 0, ]      # keep only events with nonzero damage events

#find weather events that caused crop damage
c <- summarise(group_by(storm, EVTYPE), sum(CROPDMG))
names(c)[2] <- "CDamage"          # clean up the name
c <- c[c$CDamage != 0, ]          # keep only events with nonzero damage events

p <- p[order(-p$PDamage), ]   # sort in descending order by fatalities
p$PropertyDamage <- as.character(p$PDamage)
head(p[, c(1, 3)], 10)

c <- c[order(-c$CDamage), ]   # sort in descending order by fatalities
c$CropDamage <- as.character(c$CDamage)
head(c[, c(1, 3)], 10)
