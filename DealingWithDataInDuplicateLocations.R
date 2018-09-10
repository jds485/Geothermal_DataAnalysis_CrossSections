#These functions are used to sort data into a set in which all points have a unique spatial location.
#The SameSpot function takes the data and returns an indexed dataframe of the records in the same spatial location
#The SortingWells function sorts this dataframe and returns a dataframe in which all points have a unique spatial location.
#Note that only the deepest records are retained for analysis, for quality reasons. 
#Other methods, such as computation of surface heat flow using intervals where the BHTs are known is also possible, 
#but that is not the intent of these functions. These functions simply provide unique spatial locations. 

#There is a possibility that some wells will have the same spatial location and depth, but have different BHTs. 
#In this case, the BHTs that are within some censor temperature (say CensorTemp = 2 for BHTs that are 2 degrees C different) 
#at the same depth are averaged, and a dataset called RerunWells is saved. It is up to the user how to handle this data. 
#The option to completely censor this data is available by setting CensorTemp = 0
#To avoid these complications as much as possible, it is recommended that the well database is quality controled prior to using these functions.

SameSpot = function(Data    #The projected spatial data to be checked for wells with duplicate spatial locations.
){
  #Find records in the same spatial location.
  SameSpot = zerodist(Data)
  #Create a matrix with rows equal to the columns. This is to indicate all of the unique records in the same spatial location.
  StoreData = matrix(0, nrow=length(unique(c(SameSpot[,1], SameSpot[,2]))), ncol=length(unique(c(SameSpot[,1], SameSpot[,2]))))
  #Sort the unique records in the same spot by index number, as determined by zerodist().
  Sorted = sort(unique(c(SameSpot[,1], SameSpot[,2])))
  #Make a data frame, with the column names equal to the row number of the record.
  StoreData_Frame = as.data.frame(StoreData)
  #The columns are the unique indexes. 
  colnames(StoreData_Frame) = Sorted
  
  #Empty vector for storing the index of other wells in the sorted data that have the same spot as the well being checked.
  IndexOtherWells = vector("numeric")
  
  #Run through all unique records to find wells in the same spatial location.
  for (i in 1:length(Sorted)){
    #Store the index of the wells in the same spot for this number
    IndexSpots = which(SameSpot == Sorted[i])
    #Find the corresponding index of the other wells in that spot
    for (j in 1:length(IndexSpots)){
      #The indices are in 2 columns. Need to treat the columns differently to extract the "other" record, which is in the same row, but the other column.
      #Each index of "other" wells is half the length of SameSpot away
      if (IndexSpots[j] > length(SameSpot)/2){
        IndexOtherWells[j] = IndexSpots[j] - length(SameSpot)/2
      }
      else{
        IndexOtherWells[j] = IndexSpots[j] + length(SameSpot)/2
      }
    }
    #Store all records with the same spot in the data frame.
    StoreData_Frame[as.character(SameSpot[IndexOtherWells])][i,] = 1
    #Also store the current record
    StoreData_Frame[i,i] = 1
    
    #Delete the indices before the next iteration.
    IndexOtherWells = vector("numeric")
    IndexSpots = vector("numeric")
  }
  ReturnData = list(SameSpot=SameSpot, Sorted=Sorted, StoreData_Frame=StoreData_Frame)
  return(ReturnData)
}

SortingWells = function(SameSpot,            #Output SameSpot from the SameSpot function or output from zerodist()
                        StoreData_Frame,     #Output StoreData_Frame from the SameSpot function.
                        DataAll,             #The data to be sorted.
                        BHT,                 #Column name of BHT in DataAll (string).
                        TrueVertDepth,       #Column name of True Vertical Depth in DataAll (string). Set to 0 if not known and no column exists. If column exists and True Vertical Depth is known for some records, then the records without a True Vertical Depth should have a value of 0.
                        DrillerDepth,        #Column name of Driller Depth in DataAll (string). Set to 0 if not known and no column exists. If column exists and Driller Depth is known for some records, then the records without a Driller Depth should have a value of 0.
                        DepthOfMeasurement,  #Column name of Depth of Measurement in DataAll (string). This must be known.
                        WellDepth,           #Column name of Well Depth in DataAll (string).
                        CensorTemp)          #The temperature cutoff for wells at the same spot, and depth, but different BHTs. Set to 0 for complete censoring
{
  if (TrueVertDepth == 0){
    DataAll$TrueVertDepth = 0
    print("Making column for TrueVertDepth with values equal to 0")
  }
  if (DrillerDepth == 0){
    DataAll$DrillerDepth = 0
    print("Making column for DrillerDepth with values equal to 0")
  }
  #Vector for BHTs are the same depth that are different 
  IndsDifferent = vector("numeric")
  #Vector for BHTs that were averaged and have to be re-run
  IndsBHTDiff = vector("numeric")
  #Vector for storing wells that are greater than CensorTemp apart at the same depth
  IndsCensTemp = vector("numeric")
  #Vector for indices for which the deeper record has a smaller temperature than the shallower record
  IndsDeepSmallerBHT = vector("numeric")
  #Counter for BHTs that are the same at depth
  countSameBHTs = 0
  #Storing Averaged Wells
  AvgRecord = DataAll[1,]
  AvgRecord@data[1,1] = NA
  #Loop through the unique records and find wells in the same spot.
  for (i in 1:length(unique(c(SameSpot[,1], SameSpot[,2])))){
    #Gather all wells in the same spatial location as the current well.
    Indxs = as.numeric(colnames(StoreData_Frame[which(StoreData_Frame[i,] == 1)]))
    Wells = DataAll[Indxs,]
    
    #Find the index of the max well depth.
    index_maxDepth = which(max(Wells@data[WellDepth][,1]) == Wells@data[WellDepth][,1])
    
    #Check if the deeper wells have a greater temperature than the shallower wells
    #Check that there is a deep and shallow well, rather than only wells at the same depth
    if (nrow(Wells[-index_maxDepth,]) >= 1){
      for (temp in 1:length(index_maxDepth)){
        if (any(Wells$BHT[index_maxDepth[temp]] < Wells$BHT[-index_maxDepth])){
          IndsDeepSmallerBHT = c(IndsDeepSmallerBHT, Indxs[index_maxDepth[temp]])
        }
      }
    }
    
    #Take only the deepest well.
    if (length(index_maxDepth) == 1) {
      #There's only one entry with a maximum depth. Retain only this well, and drop the others.
      #Add the removed wells to the Removed list.
      if (i == 1) {
        #Start the lists
        Removed = Wells[-index_maxDepth,]
        IndsRemoved = Indxs[-index_maxDepth]
        IndsRetained = Indxs[index_maxDepth]
      }
      else {
        #Add to existing lists
        Removed = rbind(Removed, Wells[-index_maxDepth,])
        IndsRemoved = c(IndsRemoved, Indxs[-index_maxDepth])
        IndsRetained = c(IndsRetained,Indxs[index_maxDepth])
      }  
    }
    else {
      #There are multiple wells with the maximum depth. Check to see if they have the same BHT. This would provide the same calculated values.
      Wells_MaxDepth = Wells[index_maxDepth,]
      BHTs = Wells_MaxDepth@data[BHT][,1]
      if (length(unique(BHTs)) == 1){
        #These wells provide the same information retain only one record and remove the others.
        if (i == 1) {
          Removed = Wells[-index_maxDepth[1],]
          IndsRemoved = Indxs[-index_maxDepth[1]]
          IndsRetained = Indxs[index_maxDepth[1]]
        }
        else {
          Removed = rbind(Removed, Wells[-index_maxDepth[1],])
          IndsRemoved = c(IndsRemoved, Indxs[-index_maxDepth[1]])
          IndsRetained = c(IndsRetained,Indxs[index_maxDepth[1]])
        }
      }
      else{
        #These records are different. Record all wells in the same location and figure out why there are different temps at the same depth.
        if (length(IndsDifferent)==0){
          #Record all wells in the same location
          IndsDifferent = Indxs
        }
        else{
          IndsDifferent = c(IndsDifferent,Indxs)
        }
        Wells_MaxDepth = Wells[index_maxDepth,]
        BHTs = Wells_MaxDepth@data[BHT][,1]
        DoM = Wells_MaxDepth@data[DepthOfMeasurement][,1]
        Drillz = Wells_MaxDepth@data[DrillerDepth][,1]
        Truez = Wells_MaxDepth@data[TrueVertDepth][,1]
        #Of the wells that have temperatures within CensorTemp of each other, retain the record that matches the depth of measurement for the maximum depth the closest. 
        if (max(BHTs) - min(BHTs) <= CensorTemp){
          if (((DoM[1] > Truez) && (Truez > 0)) || ((DoM[1] > Drillz) && (Drillz > 0))){
            #The max BHT should correspond to the deeper measurement (DoM), so retain the well with the greater BHT.
            #There could be more than one record with the max BHT, so take only the first one because it would have the same depth.
            if (i == 1) {
              Removed = Wells[-index_maxDepth[-which(BHTs == max(BHTs))][1],]
              IndsRemoved = Indxs[-index_maxDepth[-which(BHTs == max(BHTs))][1]]
              IndsRetained = Indxs[index_maxDepth[which(BHTs == max(BHTs))][1]]
            }
            else {
              Removed = rbind(Removed, Wells[-index_maxDepth[-which(BHTs == max(BHTs))][1],])
              IndsRemoved = c(IndsRemoved, Indxs[-index_maxDepth[-which(BHTs == max(BHTs))][1]])
              IndsRetained = c(IndsRetained,Indxs[index_maxDepth[which(BHTs == max(BHTs))][1]])
            }
          }
          else if (((DoM[1] < Truez) && (Truez > 0)) || ((DoM[1] < Drillz) && (Drillz > 0))){
            #The well with the smaller BHT should be retained to match the DoM.
            #Again, take only the first one because it would have the same depth.
            if (i == 1) {
              Removed = Wells[-index_maxDepth[-which(BHTs == min(BHTs))][1],]
              IndsRemoved = Indxs[-index_maxDepth[-which(BHTs == min(BHTs))][1]]
              IndsRetained = Indxs[index_maxDepth[which(BHTs == min(BHTs))][1]]
            }
            else {
              Removed = rbind(Removed, Wells[-index_maxDepth[-which(BHTs == min(BHTs))][1],])
              IndsRemoved = c(IndsRemoved, Indxs[-index_maxDepth[-which(BHTs == min(BHTs))][1]])
              IndsRetained = c(IndsRetained,Indxs[index_maxDepth[which(BHTs == min(BHTs))][1]])
            }
          }
          else{
            print("BHTs within CensorTemp at the same depth for a spatial location are different. Taking average BHT for these wells. Thermal model results should be rerun for these wells.")
            countSameBHTs = countSameBHTs + 1
            #Take only every other record because these are identified as 2 records. Only 1 record should be retained.
            if (countSameBHTs %% 2 == 1){
              #First iteration, start lists.
              if (i==1){
                IndsBHTDiff = Indxs[index_maxDepth]
                Removed = Wells
                IndsRemoved = Indxs
                AvgRecord = Wells[1,]
                AvgRecord@data[length(AvgRecord),][BHT] = mean(Wells@data[BHT][,1])
              }
              else{
                #Add to existing lists
                if (length(IndsBHTDiff)==0){
                  #First set of records with different BHT, same depth. Start new lists.
                  IndsBHTDiff = Indxs[index_maxDepth]
                  Removed = rbind(Removed, Wells)
                  IndsRemoved = c(IndsRemoved, Indxs)
                  AvgRecord = Wells[1,]
                  AvgRecord@data[length(AvgRecord),][BHT] = mean(Wells@data[BHT][,1])
                }
                else{
                  #Add to existing lists.
                  IndsBHTDiff = c(IndsBHTDiff, Indxs[index_maxDepth])
                  Removed = rbind(Removed, Wells)
                  IndsRemoved = c(IndsRemoved, Indxs)
                  AvgRecord = rbind(AvgRecord, Wells[1,])
                  AvgRecord@data[length(AvgRecord),][BHT] = mean(Wells@data[BHT][,1])
                }
              }
            }
          }
        }
        else{
          #The BHTs in this well are more than CensorTemp apart. They are likely bad data, so they will be discarded. Add to the Removed list
          if (i == 1) {
            Removed = Wells
            IndsRemoved = Indxs
            IndsCensTemp = Indxs
          }
          else {
            Removed = rbind(Removed, Wells)
            IndsRemoved = c(IndsRemoved, Indxs)
            IndsCensTemp = c(IndsCensTemp, Indxs)
          }
        }
      }
    }
  }
  #New dataset of sorted wells. Also add the averaged records to this dataset.
  if (!is.na(AvgRecord@data[1,1])){
    Sorted = rbind(DataAll[-unique(IndsRemoved),], AvgRecord)
    #Wells that need to be rerun. These are the last countSameBHTs/2 wells in Sorted.
    RerunWells = Sorted[(length(Sorted) - countSameBHTs/2 + 1):length(Sorted),]
  }else{
    Sorted = DataAll[-unique(IndsRemoved),]
    RerunWells = NA
  }
  #List of data to return.
  ReturnData = list(IndsRemoved=IndsRemoved, IndsRetained=IndsRetained, IndsDifferent=IndsDifferent, Removed=Removed, Sorted=Sorted, RerunWells=RerunWells, IndsBHTDiff=IndsBHTDiff, IndsDeepSmallerBHT=IndsDeepSmallerBHT, IndsCensTemp = IndsCensTemp)
  return(ReturnData)
}

require(rgdal)


# Example use of the functions
#Data = readOGR(dsn=getwd(), layer = "AASG_BHTsForNY", stringsAsFactors=FALSE)

# Simple quality control:
# Remove the negative BHT wells before the sorting of wells in the same spatial locations:
#DataAll = Data[-which(Data$BHT < 0),]

# Determine unique spatial locations
#Same = SameSpot(DataAll)
#SortData = SortingWells(SameSpot = Same$SameSpot, StoreData_Frame = Same$StoreData_Frame, DataAll = DataAll, BHT = 'BHT', TrueVertDepth = 'TruVrtc', DrillerDepth = 'DrllrTt', DepthOfMeasurement = 'DpthOfM', WellDepth = 'WellDepth', CensorTemp = 2)

# Save data
#write.csv(SortData$Sorted, "ExampleSortedUniqueSpots_AASG_NY.csv")
#write.csv(SortData$RerunWells, "ExampleRerunWells_AASG_NY.csv")
#write.csv(SortData$Removed, "ExampleRemovedWells_AASG_NY.csv")

#The wells that are listed in RerunWells are the last rows in ExampleSortedUniqueSopts_AASG_NY.csv.