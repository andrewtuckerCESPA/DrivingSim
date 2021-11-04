library(reshape2)

setwd("P:/Driving Sim - Tucker Marsh AV Conversation Study/Data/Tucker Marsh AV Study Exports/Tucker Marsh AV Study Exports")

FileName <- "ReactionTimesClean.csv"
RTData <- read.csv(FileName)

FileName <- "SimDriverOccur.csv"
OData <- read.csv(FileName)


RTwide <- reshape(RTData, idvar = "ID", timevar = "Event", direction = "wide")


Owide <- reshape(OData, idvar = "ID", timevar = "Trial", direction = "wide")

write.csv(RTwide, paste(getwd(), "/RTwide.csv", sep = ""), row.names = TRUE)
write.csv(Owide, paste(getwd(), "/OCCURwide.csv", sep = ""), row.names = TRUE)