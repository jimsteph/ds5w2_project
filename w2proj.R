if (!file.exists("./data")) {
  dir.create("./data")
  fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(fileURL, destfile = "./data/dataset.bz2", method = "curl", mode = "wb")
  unzip("./data/dataset.bz2", exdir = "./data")
}

