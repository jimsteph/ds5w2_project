## download and extract the data 
if (!file.exists("./data")) {
  dir.create("./data")
  fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(fileURL, destfile = "./data/dataset.bz2", method = "curl", mode = "wb")
  # note: this next line will take a *very* long time to execute ... go make a cup of coffee ...
  storm <- read.table(bzfile("./data/dataset.bz2", "r"))
}

