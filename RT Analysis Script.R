---
  title: "Analysis of Reaction Time in Driving Sim Studies"
  author:"Andy Tucker"
  
  description: "Used to analyze SimDriver disengagements in response to events in driving sim."
  Requires: "simtime column, User1 column (SimDriver status) and User2 column (ManeuverFlag)."
  
  touse: 
    "Change value in setwd (line 23) to directory containing Data Distillery outputs
    Data distillery otuputs must be .csv format.
    .csvs found in directory that don't contain User1 and User2 columns are not analyzed." 
  
  process:
    "Looks for ManeuverFlags, finds next SimDriver status change, and gets difference in time between them.
    Only reports FIRST deactivation following each ManeuverFlag (User2 column), and only if that deactivation
    did not occur after the next ManeuverFlag. Reports NA for ManeuverFlags not followed by SimDriver 
    deactivations. File names, Event Times (SimTime of ManeuverFlag), and Reaction Times (elapsed time between 
    ManeuverFlag and SimDriver deactivation) are written to ReactionTimes.csv. If ReactionTimes.csv already 
    exists, it will not be overwritten."
---
    
  ## In setwd, enter the path for the folder containing the data. Data must be in .csv format!  
  setwd("P:/Driving Sim - Tucker Marsh AV Conversation Study/Data/Tucker Marsh AV Study Exports/Tucker Marsh AV Study Exports")
  FilesList <- list.files(pattern = ".dat")
  

  
  ## This loop iterates through all the files grabbed from the folder specified above
  for (i in FilesList){
    ## First we grab each data file.
    print(paste("Reading", toString(i)))
    RawData <- read.table(i,header=TRUE)
    Names <- names(RawData)
    
    if(is.element("User1", Names) && is.element("User2", Names)){
    
      ## Then we find out where the data in it actually starts

      FirstRow = 0
      for (row in 1:nrow(RawData)){
        if (RawData$User1[row] != "."){
          FirstRow = row
          break()
        }
      }
        
      ## Create a subset which only contains the usable data
      Data <- RawData[FirstRow:nrow(RawData),]
      
      Data$User1 <- as.numeric(Data$User1)
      Data$User2 <- as.numeric(Data$User2)
     
      ## Find each event in the file, and put their SimTimes in EventFrame. Events are marked with "3" after 
      ## performing the as.numeric function above, for some reason. 
      row = FirstRow
      while (row < nrow(Data)){
        if(Data$User2[row] == 3 || Data$User2[row] == 1){
          print(paste("EVENT FOUND AT ROW", toString(row), "WHERE SIMTIME = ", toString(Data$SimTime[row])))
          
          if(exists("EventFrame") == FALSE){
            EventFrame <- data.frame(SimTime = Data$SimTime[row])
          }
          else{
            EventFrame <- rbind(EventFrame, c(Data$SimTime[row]))
          }
          
          row = row + 120
        }
        else{
          row = row + 1
        }
      }
      
      
      ## Find each deactivation of SimDriver in the file, and put their SimTimes in DeactivationFrame
      ## SimDriver ON = 3, SimDriver OFF = 2
      row = FirstRow+1
      while (row < nrow(Data)){
        CurrentSimDriverStatus = Data$User1[row]
        PrevSimDriverStatus = Data$User1[row-1]
        
        if (CurrentSimDriverStatus == 2 && PrevSimDriverStatus == 3){
          print(paste("DEACTIVATION FOUND AT ROW", toString(row), "WHERE SIMTIME = ", toString(Data$SimTime[row])))
          if(exists("DeactivationFrame") == FALSE){
            DeactivationFrame <- data.frame(SimTime = Data$SimTime[row])
          }
          else{
          DeactivationFrame <- rbind(DeactivationFrame, c(Data$SimTime[row]))
          }
        }
        else {
          if (CurrentSimDriverStatus == 0 && PrevSimDriverStatus == 1){
            print(paste("DEACTIVATION FOUND AT ROW", toString(row), "WHERE SIMTIME = ", toString(Data$SimTime[row])))
            if(exists("DeactivationFrame") == FALSE){
              DeactivationFrame <- data.frame(SimTime = Data$SimTime[row])
            }
            else{
              DeactivationFrame <- rbind(DeactivationFrame, c(Data$SimTime[row]))
            }
          }
        }
        row = row + 1
      }
      
      ## Find out if there are any events, and if so, find any deactivations following each event
      erow = 1
      if (exists("EventFrame") == TRUE){
        if (exists("DeactivationFrame") == TRUE){
          while (erow <= nrow(EventFrame)){
            
            ## Gets SimTime for Event currently being evaluated
            CurrentEventTime = EventFrame$SimTime[erow]
            
            ## IF current Event is not the last event in the file, it finds the SimTime of the next Event.
            ## ELSE current Event is the last evenet in the file, it sets the time of the next event as NA.
            if (erow < nrow(EventFrame)){
              NextEventTime = EventFrame$SimTime[erow+1]
            }
            else{
              NextEventTime = NA
            }
            
            ## Iterates through Deactivations, finding those that occured AFTER the current Event,
            ## but BEFORE the next Event. Only grabs the first Deactivation following each Event.
            drow = 1
            EventHit = FALSE
            while (drow <= nrow(DeactivationFrame) && EventHit == FALSE){
              DeactivationTime = DeactivationFrame$SimTime[drow]
              print(paste("Evaluating deactivation at", DeactivationTime, "wrt Event at", CurrentEventTime))
              
              if (DeactivationTime > CurrentEventTime){
                if(is.na(NextEventTime) == FALSE){
                  if (DeactivationTime < NextEventTime){
                    ReactionTime = DeactivationTime - CurrentEventTime
                    print(paste("Event at", CurrentEventTime, "was responded to at", DeactivationTime, "with an RT of", ReactionTime))
                    EventHit = TRUE
                    if(exists("RTFrame") == FALSE){
                      RTFrame <- data.frame(FileName = toString(i), EventTime = CurrentEventTime, ReactionTime = ReactionTime)
                    }
                    else{
                      tempdf <- data.frame(FileName = toString(i), EventTime = CurrentEventTime, ReactionTime = ReactionTime)
                      RTFrame <- rbind(RTFrame, tempdf)
                      rm("tempdf")
                    }
                  }
                  else{
                    print(paste("No Deactivation in response to Event at", CurrentEventTime))
                    EventHit = TRUE
                    if(exists("RTFrame") == FALSE){
                      RTFrame <- data.frame(FileName = toString(i), EventTime = CurrentEventTime, ReactionTime = NA)
                    }
                    else{
                      tempdf <- data.frame(FileName = toString(i), EventTime = CurrentEventTime, ReactionTime = NA)
                      RTFrame <- rbind(RTFrame, tempdf)
                      rm("tempdf")
                    }
                  }
                }
                else{
                  print("Looking at last event in file!")
                  ReactionTime = DeactivationTime - CurrentEventTime
                  print(paste("Event at", CurrentEventTime, "was responded to at", DeactivationTime, "with an RT of", ReactionTime))
                  EventHit = TRUE
                  if(exists("RTFrame") == FALSE){
                    RTFrame <- data.frame(FileName = toString(i), EventTime = CurrentEventTime, ReactionTime = ReactionTime)
                  }
                  else{
                    tempdf <- data.frame(FileName = toString(i), EventTime = CurrentEventTime, ReactionTime = ReactionTime)
                    RTFrame <- rbind(RTFrame, tempdf)
                    rm("tempdf")
                  }
                }
              }
              drow = drow + 1
            }
            erow = erow + 1
          }
        }
        else{
          print("Event(s) found, but no SimDriver deactivations found")
          ## Fill an NA dataframe to bind to RTframe, so we know that these events happened but there was no response
          while (erow <= nrow(EventFrame)){
            tempdf = data.frame(Filename = toString(i), EventTime = EventFrame$SimTime[erow], ReactionTIme = NA)
            if (exists("RTFrame") == FALSE){
              RTFrame <- tempDF
            }
            else{
              RTFrame <- rbind(RTFrame, tempdf)
              
            }
            rm("tempdf")
            erow = erow + 1
          }
        }
        rm("EventFrame")
      }
      else{
        print(paste("No Events found in file", toString(i)))
      }
      rm("DeactivationFrame")
    }
  }
  
  if(exists("RTFrame") == TRUE){
    if (is.element("ReactionTimes.csv", list.files(getwd())) == FALSE){
     write.csv(RTFrame, paste(getwd(), "/ReactionTimes.csv", sep = ""), row.names = TRUE)
    }
    else{
      print("ReactionTimes.csv already exists. RTs were not written to csv to avoid overwriting existing data.")
    }
  }

  