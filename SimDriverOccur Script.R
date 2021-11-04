---
  title: "Analysis of Sim Driver usage in Driving Sim Studies"
  author:"Andy Tucker"

  description: "Used to analyze the amount of time (given as %) per trial SimDriver was engaged."  
  requires: "SimTime column, User1 column (SimDriver status)"
  
  touse: 
    "Change value in setwd (line 16) to directory containing Data Distillery outputs
    Data distillery otuputs must be .csv format.
    .csvs found in directory that don't contain User1 column are not analyzed." 
  
---
  
  ## In setwd, enter the path for the folder containing the data. Specify file type in line 18.
  setwd("P:/Driving Sim - Tucker Marsh AV Conversation Study/Data/Tucker Marsh AV Study Exports/Tucker Marsh AV Study Exports")
  FilesList <- list.files(pattern = ".dat")
  
  
  for (i in FilesList){
    ## First we grab each data file.
    print(paste("Reading", toString(i)))
    RawData <- read.table(i,header=TRUE)
    Names <- names(RawData)
    
    if(is.element("User1", Names)){
      
      ## Then we find out where the data in it actually starts
      FirstRow = 0
      for (row in 1:nrow(RawData)){
        if (is.na(RawData$User1[row]) == FALSE){
          FirstRow = row
          break()
        }
      }
      
      ## Create a subset which only contains the usable data
      Data <- RawData[FirstRow:nrow(RawData),]
      
      ## Get count of samples in which SimDriver is engaged.
      SimDriverCount = 0
      for (row in 1:nrow(Data)){
        if (Data$User1[row] == 1)
          SimDriverCount = SimDriverCount + 1
      }
      
      TD = Data$SimTime[nrow(Data)] - Data$SimTime[1]
      SDOccur = (SimDriverCount / nrow(Data)) * 100
      MCOccur = 100 - SDOccur
      
      print(paste("In", toString(i), "participant spent", SDOccur, "of the trial with SimDriver engaged, and", MCOccur, "of the trial in manual control of the vehicle. Total trial duration was", TD))
        
      if(exists("SDFrame") == FALSE){
        SDFrame <- data.frame(FileName = toString(i), TrialDuration = TD, SimDriverOccur = SDOccur, ManualControlOccur = MCOccur)
      }
      else{
        tempdf <- data.frame(FileName = toString(i), TrialDuration = TD, SimDriverOccur = SDOccur, ManualControlOccur = MCOccur)
        SDFrame <- rbind(SDFrame, tempdf)
        rm("tempdf")
      }
    }
  }
  
  if(exists("SDFrame") == TRUE){
    if (is.element("SimDriverOccur.csv", list.files(getwd())) == FALSE){
      write.csv(SDFrame, paste(getwd(), "/SimDriverOccur.csv", sep = ""), row.names = TRUE)
    }
    else{
      print("SimDriverOccur.csv already exists. Sim Driver usage percentagess were not written to csv to avoid overwriting existing data.")
    }
  }
      
      
      

