#This file makes a plot of geotherms for wells of interest, and colors them according to the latitude, or a trend of interest.
#Assumes that the geotherm information is contained within the last columns of the dataset.

Wells = read.csv("GeothermPlotWells_TompkinsCounty.csv", stringsAsFactors=FALSE)

MakeGeothermPlot = function(Wells,              #The well database
                            BHT,                #Column name for the BHT
                            WellDepth,          #Column name for the well depth
                            StartTempField,     #Column name for the first temperature at depth (e.g. T0)
                            BasementDepth = NA, #Column name for the basement depth
                            MinDepth,           #The minimum depth of the calculated temperatures
                            MaxDepth,           #The maximum depth of the calculated temperatures
                            MinTemp,            #The minimum temperature for the x-axis
                            MaxTemp,            #The maximum temperature for the x-axis
                            DepthIncrement,     #The depth increment of the calculated temperatures
                            Trend = NA,         #Field name for trend.
                            ColRampDivs = NA,   #Number of color ramp divisions. 1 is least (one color only). Using length(Wells[,1]) will use the most colors.
                            ColRampCols,        #Vector or scalar of colors to use in the color ramp. e.g. c("red", "orange", "yellow", "green", "blue")
                            ChartTitle = ''     #Title for the chart. Default is no title.
){
  if (is.na(Trend) == FALSE){
    #Sort by latitude (column 7). One could have any trend represented here, instead of latitude, but for this county the major trend is sount-north.
    Wells = Wells[order(as.numeric(Wells[Trend][,1])), ]
  }
  
  #Set the depth range and increment for the data.
  Depths = seq(MinDepth,MaxDepth,DepthIncrement)
  
  if (is.na(ColRampDivs)==FALSE){
    #Set the color ramp for coloring the geotherms
    ramp = seq(0,length(Wells[,1]),by=((length(Wells[,1]))/ColRampDivs))
    colramp = colorRampPalette(ColRampCols)
    color = colramp(length(ramp))
  }
  else{
    color = rep(ColRampCols, length(Wells[,1]))
  }
  
  for (i in 1:length(Wells[,1])){
    if (i == 1){
      plot(y = Depths, x = as.numeric(Wells[i,(which(colnames(Wells)==StartTempField):length(Wells[1,]))]), ylim=c(MaxDepth,MinDepth), xlim=c(MinTemp, MaxTemp), type='l', lwd=1.5, col=color[i], xlab=expression(paste("Temperature (",degree,"C)")), ylab="Depth (m)", main=ChartTitle, cex.lab=1.5, cex.main=1.5, cex.axis=1.5)
      par(new=TRUE)
    }
    else{
      plot(y = Depths, x = as.numeric(Wells[i,(which(colnames(Wells)==StartTempField):length(Wells[1,]))]), ylim=c(MaxDepth,MinDepth), xlim=c(MinTemp, MaxTemp), type='l', lwd=1.5, col=color[i], xlab="", ylab="", axes=FALSE)
      par(new=TRUE)
    }
    if (i == length(Wells[,1])){
      #Plot the basement depth on top of the geotherms
      if (is.na(BasementDepth) == FALSE){
        for (j in 1:length(Wells[,1])){
          plot(y = Wells[BasementDepth][j,1], x = as.numeric(Wells[j,(which(colnames(Wells)==StartTempField)-1+round(Wells[BasementDepth][j,1]/10))]), ylim=c(MaxDepth,MinDepth), xlim=c(MinTemp, MaxTemp), pch='-', cex=1.5, xlab='', ylab='', axes=FALSE)
          par(new=TRUE)
        }
      }
      #Plot the BHTs on top of the geotherms
      plot(y = Wells[WellDepth][,1], x = Wells[BHT][,1], ylim=c(MaxDepth,MinDepth), xlim=c(MinTemp, MaxTemp), pch=16, xlab='', ylab='', axes=FALSE)
      par(new=FALSE)
    }
  }
}


#Example plot:
png("ExampleGeothermPlotTompkinsNY2.png", width=8, height=8, units="in", res=150)
MakeGeothermPlot(Wells = Wells, BHT = 'BHT', WellDepth = 'WellDepth', StartTempField = 'T0', BasementDepth = 'BasementDepth', MinDepth = 10, MaxDepth = 5000, MinTemp = 0, MaxTemp = 150, DepthIncrement = 10, Trend = 'LatDegr', ColRampDivs=length(Wells[,1]), ColRampCols = c("green", "yellow", "orange", "red"), ChartTitle='Geotherms for wells in Tompkins County, NY')
legend('topright', legend=c("BHTs", "Basement Depth","Geotherms", "Southern Tomkins County", "Northern Tomkins County"), pch=c(16,95,NA,NA,NA), lty=c(NA,NA,1,1,1), col=c("black","black","black","green","red"))
dev.off()
