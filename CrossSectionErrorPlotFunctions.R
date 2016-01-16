#This file makes cross section plots based on a SpatialPoints cross section.
#The cross section points must be ordered along the cross section for this function to work. 
#Typically, sorting the cross section points by latitude or longitude will properly sort the data for cross sections that are not too complex. 

library(rgdal)
library(graphics)

#This function deals with jaggedness along a cross section that results from extraction of raster values to points along a line feature.
#The points may jump from one boundary to another and back, so this function switches the order of points to make a smooth cross section.
#This function can handle up to 4 switchbacks along the same interpolation region. If more are needed, you will need to add additional logic statements, or redefine the cross section.
SortMixedIntBounds = function(CrossSec,      #Cross section data
                              IntBoundNames, #Field name for the interpolation boundaries in CrossSec
                              PositionName   #Field name indicating the position of points (e.g. Length)
                              ){
  for (i in 1:(length(CrossSec@data[, IntBoundNames])-1)){
    if (CrossSec@data[i, IntBoundNames] != CrossSec@data[i+1, IntBoundNames]){
      if (CrossSec@data[i, IntBoundNames] == CrossSec@data[i+2, IntBoundNames]){
        if (CrossSec@data[i, IntBoundNames] == CrossSec@data[i+4, IntBoundNames]){
          #Switch The Points i+1 and i+2
          placehold1 = CrossSec@data[i+1,]
          placehold2 = CrossSec@data[i+2,]
          placehold3 = CrossSec@data[i+3,]
          placehold4 = CrossSec@data[i+4,]
          CrossSec@data[i+1,] = placehold2
          CrossSec@data[i+1,PositionName] = placehold1[, PositionName]
          CrossSec@data[i+2,] = placehold4
          CrossSec@data[i+2,PositionName] = placehold2[, PositionName]
          CrossSec@data[i+3,] = placehold1
          CrossSec@data[i+3,PositionName] = placehold3[, PositionName]
          CrossSec@data[i+4,] = placehold3
          CrossSec@data[i+4,PositionName] = placehold4[, PositionName]
        }
        else{
          #Switch The Points i+1 and i+2
          placehold1 = CrossSec@data[i+1,]
          placehold2 = CrossSec@data[i+2,]
          CrossSec@data[i+2,] = placehold1
          CrossSec@data[i+2,PositionName] = placehold2[, PositionName]
          CrossSec@data[i+1,] = placehold2
          CrossSec@data[i+1,PositionName] = placehold1[, PositionName]
        }
      }
    }
  }
  return(CrossSec)
}


#This function plots the mean and the error bars about the mean. If there are interpolation boundaries, these are plotted.
#If the SortMixedIntBounds function is not run before this function, interpolation boundaries may appear as both red dots and red dashes.
ErrorBarXC = function(CrossSec,          #Data file containing the cross section information. Should be sorted in order of the cross section
                       PositionName,     #Field name in CrossSec indicating the position of points along the cross section (e.g. Length)
                       PredVar,          #Field name of the prediction variable in CrossSec (e.g. predicted mean)
                       ErrVar,           #Field name of the error in the prediction in CrossSec (standard error is assumed)
                       xLimMin=0,        #Minimum value of the x-axis
                       xLimMax,          #Maximum value of the x-axis
                       yLimBot,          #y-axis value for the bottom of the y-axis
                       yLimTop,          #y-axis value for the top of the y-axis
                       IntBoundLen=0.25, #Distance vertically beyond error bars that the interpolation bounds should extend. This is useful for extending only those error bars that are small. Set to 0 if no extension is desired.
                       IntBoundNames,    #Field name of the interpolation boundaries. Set to 0 if no interpolation boundaries exist.
                       LinWidBound=1,    #Line width of the interpolation boundaries
                       BoundExtend=0.05  #Additional distance beyond error bars that the interpolation bounds should extend. Set to 0 if no extension is desired.
                       ){
  if (IntBoundNames == 0){
    print("Making a new field for Interpolation Boundary Names. All values will be None")
    CrossSec$IntBoundNames = "None"
  }
  if (CrossSec@data[PositionName][1,1] != 0){
    print("CrossSec@data[PositionName][,1] must start at 0. You can change to other numbers after the cross section has been computed.")
    break
  }
  if ((length(CrossSec@data[PositionName][,1])-1) != (CrossSec@data[PositionName][,1])[length(CrossSec@data[PositionName][,1])]){
    print("CrossSec@data[PositionName][,1] must have increments of 1. You can change to other increments after the cross section has been computed.")
    break
  }
  if (yLimBot > yLimTop){
    #Case for inverted y-axis
    BoundExtend = -BoundExtend
  }
  SortIVec = vector('numeric',length=length(CrossSec@data[,IntBoundNames]))
  for (i in 1:length(CrossSec@data[,IntBoundNames])){
    if (i == 1){
      if (CrossSec@data[i,IntBoundNames] != CrossSec@data[i+1,IntBoundNames]){
        #These are different. Give first location points.
        plot(CrossSec@data[PositionName][i,1], (CrossSec@data[i,PredVar] + 2*CrossSec@data[i,ErrVar]), type = 'p', pch=16, col='red', xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), lty=2, xlab="", ylab="", lwd=2, axes=FALSE)
        par(new=T)
        plot(CrossSec@data[PositionName][i,1], (CrossSec@data[i,PredVar] - 2*CrossSec@data[i,ErrVar]), type = 'p', pch=16, col='red', xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), lty=2, xlab="", ylab="", lwd=2, axes=FALSE)
        if (IntBoundLen*(yLimBot-yLimTop) < ((CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,ErrVar]) - (CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,ErrVar]))){
          lines(x=c(CrossSec@data[PositionName][i-1+1,1],CrossSec@data[PositionName][i-1+1,1]), y=c(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,ErrVar] + BoundExtend*(yLimBot-yLimTop),CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,ErrVar] - BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
        }
        else{
          lines(x=c(CrossSec@data[PositionName][i-1+1,1],CrossSec@data[PositionName][i-1+1,1]), y=c(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,PredVar] + IntBoundLen/2*(yLimBot-yLimTop) - BoundExtend*(yLimBot-yLimTop),CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,PredVar] - IntBoundLen/2*(yLimBot-yLimTop) + BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
        }
        par(new=T)
        SortIVec[i] = 1      
      }
    }
    else if (i > 2 && i != length(SortIVec)){
      #If the location at i doesn't equal the same as the location before i, then they are different.
      if (CrossSec@data[i,IntBoundNames] != CrossSec@data[i-1,IntBoundNames]){
        #Indicate that this location is a new boundary
        SortIVec[i] = 1
        #Determine if lines or points are needed for the segment before i.
        if (length(which(SortIVec>0)) > 1){
          #There's been a boundary identified already.
          if ((CrossSec@data[PositionName][i-1,1] - CrossSec@data[PositionName][,1][which(SortIVec > 0)[length(which(SortIVec > 0))-1]]) >= 1){
            #Use Lines
            plot(seq(CrossSec@data[PositionName][,1][which(SortIVec > 0)[length(which(SortIVec > 0))-1]],CrossSec@data[PositionName][i-1,1],1), (CrossSec@data[seq(CrossSec@data[PositionName][,1][which(SortIVec > 0)[length(which(SortIVec > 0))-1]]+1,CrossSec@data[PositionName][i-1,1]+1,1),PredVar] + 2*CrossSec@data[seq(CrossSec@data[PositionName][,1][which(SortIVec > 0)[length(which(SortIVec > 0))-1]]+1,CrossSec@data[PositionName][i-1,1]+1,1),ErrVar]), type = 'l', col='red', xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), lty=2, xlab="", ylab="", lwd=2, axes=FALSE)
            par(new=T)
            plot(seq(CrossSec@data[PositionName][,1][which(SortIVec > 0)[length(which(SortIVec > 0))-1]],CrossSec@data[PositionName][i-1,1],1), (CrossSec@data[seq(CrossSec@data[PositionName][,1][which(SortIVec > 0)[length(which(SortIVec > 0))-1]]+1,CrossSec@data[PositionName][i-1,1]+1,1),PredVar] - 2*CrossSec@data[seq(CrossSec@data[PositionName][,1][which(SortIVec > 0)[length(which(SortIVec > 0))-1]]+1,CrossSec@data[PositionName][i-1,1]+1,1),ErrVar]), type = 'l', col='red', xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), lty=2, xlab="", ylab="", lwd=2, axes=FALSE)
            if (IntBoundLen*(yLimBot-yLimTop) < (max(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar], CrossSec@data[CrossSec@data[PositionName][i-1,1]+2,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+2,ErrVar]))
                - min(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar], CrossSec@data[CrossSec@data[PositionName][i-1,1]+2,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+2,ErrVar])){
              lines(x=c(CrossSec@data[PositionName][i-1,1],CrossSec@data[PositionName][i-1,1]), y=c(max(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar], CrossSec@data[CrossSec@data[PositionName][i-1,1]+2,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+2,ErrVar]) - BoundExtend*(yLimBot-yLimTop), min(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar], CrossSec@data[CrossSec@data[PositionName][i-1,1]+2,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+2,ErrVar]) + BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
            }
            else{
              lines(x=c(CrossSec@data[PositionName][i-1,1],CrossSec@data[PositionName][i-1,1]), y=c(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + IntBoundLen/2*(yLimBot-yLimTop) - BoundExtend*(yLimBot-yLimTop),CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - IntBoundLen/2*(yLimBot-yLimTop) + BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
            }
            par(new=T)
          }
          else{
            #Use Points
            plot(CrossSec@data[PositionName][i-1,1], (CrossSec@data[i-1,PredVar] + 2*CrossSec@data[i-1,ErrVar]), type = 'p', pch=16, col='red', xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), lty=2, xlab="", ylab="", lwd=2, axes=FALSE)
            par(new=T)
            plot(CrossSec@data[PositionName][i-1,1], (CrossSec@data[i-1,PredVar] - 2*CrossSec@data[i-1,ErrVar]), type = 'p', pch=16, col='red', xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), lty=2, xlab="", ylab="", lwd=2, axes=FALSE)
            if (length(which(SortIVec>0)) > 2){
              if ((which(SortIVec>0)[length(which(SortIVec>0))-1] - which(SortIVec>0)[length(which(SortIVec>0))-2]) == 1){
                #These are two points right next to each other. Plot a line for the first and second.
                if (IntBoundLen*(yLimBot-yLimTop) < ((CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,ErrVar]) - (CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,ErrVar]))){
                  lines(x=c(CrossSec@data[PositionName][i-1+1,1],CrossSec@data[PositionName][i-1+1,1]), y=c(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,ErrVar] + BoundExtend*(yLimBot-yLimTop),CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,ErrVar] - BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
                }
                else{
                  lines(x=c(CrossSec@data[PositionName][i-1+1,1],CrossSec@data[PositionName][i-1+1,1]), y=c(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,PredVar] + IntBoundLen/2*(yLimBot-yLimTop) - BoundExtend*(yLimBot-yLimTop),CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,PredVar] - IntBoundLen/2*(yLimBot-yLimTop) + BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
                }
                if (IntBoundLen*(yLimBot-yLimTop) < ((CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar]) - (CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar]))){
                  lines(x=c(CrossSec@data[PositionName][i-1,1],CrossSec@data[PositionName][i-1,1]), y=c(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar] + BoundExtend*(yLimBot-yLimTop),CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar] - BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
                }
                else{
                  lines(x=c(CrossSec@data[PositionName][i-1,1],CrossSec@data[PositionName][i-1,1]), y=c(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + IntBoundLen/2*(yLimBot-yLimTop) - BoundExtend*(yLimBot-yLimTop),CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - IntBoundLen/2*(yLimBot-yLimTop) + BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
                }
              }
              else{
                if (IntBoundLen*(yLimBot-yLimTop) < ((CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar]) - (CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar]))){
                  lines(x=c(CrossSec@data[PositionName][i-1,1],CrossSec@data[PositionName][i-1,1]), y=c(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar] + BoundExtend*(yLimBot-yLimTop),CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar] - BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
                }
                else{
                  lines(x=c(CrossSec@data[PositionName][i-1,1],CrossSec@data[PositionName][i-1,1]), y=c(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + IntBoundLen/2*(yLimBot-yLimTop) - BoundExtend*(yLimBot-yLimTop),CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - IntBoundLen/2*(yLimBot-yLimTop) + BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
                }
              }
            }
            else {
              if (IntBoundLen*(yLimBot-yLimTop) < ((CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar]) - (CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar]))){
                lines(x=c(CrossSec@data[PositionName][i-1,1],CrossSec@data[PositionName][i-1,1]), y=c(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar] + BoundExtend*(yLimBot-yLimTop),CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar] - BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
              }
              else{
                lines(x=c(CrossSec@data[PositionName][i-1,1],CrossSec@data[PositionName][i-1,1]), y=c(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + IntBoundLen/2*(yLimBot-yLimTop) - BoundExtend*(yLimBot-yLimTop),CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - IntBoundLen/2*(yLimBot-yLimTop) + BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
              }
            }
            par(new=T)
          }
        }
        else{
          #There's only one boundary so far, so plot up until that boundary.
          #Check if there's at least one other point before the location you're plotting up until at (i-1)
          if (((i-1) - 1) > 1){
            #Use Lines
            plot(CrossSec@data[PositionName][,1][1:i-1], (CrossSec@data[1:i-1,PredVar] + 2*CrossSec@data[1:i-1,ErrVar]), type = 'l', col='red', xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), lty=2, xlab="", ylab="", lwd=2, axes=FALSE)
            par(new=T)
            plot(CrossSec@data[PositionName][,1][1:i-1], (CrossSec@data[1:i-1,PredVar] - 2*CrossSec@data[1:i-1,ErrVar]), type = 'l', col='red', xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), lty=2, xlab="", ylab="", lwd=2, axes=FALSE)
            if (IntBoundLen*(yLimBot-yLimTop) < (max(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar], CrossSec@data[CrossSec@data[PositionName][i-1,1]+2,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+2,ErrVar]))
                - min(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar], CrossSec@data[CrossSec@data[PositionName][i-1,1]+2,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+2,ErrVar])){
              lines(x=c(CrossSec@data[PositionName][i-1,1],CrossSec@data[PositionName][i-1,1]), y=c(max(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar], CrossSec@data[CrossSec@data[PositionName][i-1,1]+2,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+2,ErrVar]) - BoundExtend*(yLimBot-yLimTop), min(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar], CrossSec@data[CrossSec@data[PositionName][i-1,1]+2,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+2,ErrVar]) + BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
            }
            else{
              lines(x=c(CrossSec@data[PositionName][i-1,1],CrossSec@data[PositionName][i-1,1]), y=c(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + IntBoundLen/2*(yLimBot-yLimTop) - BoundExtend*(yLimBot-yLimTop),CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - IntBoundLen/2*(yLimBot-yLimTop) + BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
            }
            par(new=T)
          }
          else{
            #Use Points
            plot(CrossSec@data[PositionName][i-1,1], (CrossSec@data[i-1,PredVar] + 2*CrossSec@data[i-1,ErrVar]), type = 'p', pch=16, col='red', xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), lty=2, xlab="", ylab="", lwd=2, axes=FALSE)
            par(new=T)
            plot(CrossSec@data[PositionName][i-1,1], (CrossSec@data[i-1,PredVar] - 2*CrossSec@data[i-1,ErrVar]), type = 'p', pch=16, col='red', xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), lty=2, xlab="", ylab="", lwd=2, axes=FALSE)
            if (length(which(SortIVec>0)) > 2){
              if ((which(SortIVec>0)[length(which(SortIVec>0))-1] - which(SortIVec>0)[length(which(SortIVec>0))-2]) == 1){
                if (IntBoundLen*(yLimBot-yLimTop) < ((CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,ErrVar]) - (CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,ErrVar]))){
                  lines(x=c(CrossSec@data[PositionName][i-1+1,1],CrossSec@data[PositionName][i-1+1,1]), y=c(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,ErrVar] + BoundExtend*(yLimBot-yLimTop),CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,ErrVar] - BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
                }
                else{
                  lines(x=c(CrossSec@data[PositionName][i-1+1,1],CrossSec@data[PositionName][i-1+1,1]), y=c(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,PredVar] + IntBoundLen/2*(yLimBot-yLimTop) - BoundExtend*(yLimBot-yLimTop),CrossSec@data[CrossSec@data[PositionName][i-1,1]+1+1,PredVar] - IntBoundLen/2*(yLimBot-yLimTop) + BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
                }
                if (IntBoundLen*(yLimBot-yLimTop) < ((CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar]) - (CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar]))){
                  lines(x=c(CrossSec@data[PositionName][i-1,1],CrossSec@data[PositionName][i-1,1]), y=c(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar] + BoundExtend*(yLimBot-yLimTop),CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar] - BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
                }
                else{
                  lines(x=c(CrossSec@data[PositionName][i-1,1],CrossSec@data[PositionName][i-1,1]), y=c(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + IntBoundLen/2*(yLimBot-yLimTop) - BoundExtend*(yLimBot-yLimTop),CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - IntBoundLen/2*(yLimBot-yLimTop) + BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
                }
              }
              else{
                if (IntBoundLen*(yLimBot-yLimTop) < ((CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar]) - (CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar]))){
                  lines(x=c(CrossSec@data[PositionName][i-1,1],CrossSec@data[PositionName][i-1,1]), y=c(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar] + BoundExtend*(yLimBot-yLimTop),CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar] - BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
                }
                else{
                  lines(x=c(CrossSec@data[PositionName][i-1,1],CrossSec@data[PositionName][i-1,1]), y=c(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + IntBoundLen/2*(yLimBot-yLimTop) - BoundExtend*(yLimBot-yLimTop),CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - IntBoundLen/2*(yLimBot-yLimTop) + BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
                }
              }
            }
            else {
              if (IntBoundLen*(yLimBot-yLimTop) < ((CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar]) - (CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar]))){
                lines(x=c(CrossSec@data[PositionName][i-1,1],CrossSec@data[PositionName][i-1,1]), y=c(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar] + BoundExtend*(yLimBot-yLimTop),CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + 2*CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,ErrVar] - BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
              }
              else{
                lines(x=c(CrossSec@data[PositionName][i-1,1],CrossSec@data[PositionName][i-1,1]), y=c(CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] + IntBoundLen/2*(yLimBot-yLimTop) - BoundExtend*(yLimBot-yLimTop),CrossSec@data[CrossSec@data[PositionName][i-1,1]+1,PredVar] - IntBoundLen/2*(yLimBot-yLimTop) + BoundExtend*(yLimBot-yLimTop)),lty=2, lwd=LinWidBound)
              }
            }
            par(new=T)
          }
        }
      }
    }
    else if (i == length(SortIVec)){
      #This is for the last segment
      if (length(which(SortIVec > 0)) > 0){
        if ((i - which(SortIVec > 0)[length(which(SortIVec > 0))]) > 1){
          #Use Lines
          plot(CrossSec@data[PositionName][,1][which(SortIVec > 0)[length(which(SortIVec > 0))]:i], (CrossSec@data[which(SortIVec > 0)[length(which(SortIVec > 0))]:i,PredVar] + 2*CrossSec@data[which(SortIVec > 0)[length(which(SortIVec > 0))]:i,ErrVar]), type = 'l', col='red', xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), lty=2, xlab="", ylab="", lwd=2, axes=FALSE)
          par(new=T)
          plot(CrossSec@data[PositionName][,1][which(SortIVec > 0)[length(which(SortIVec > 0))]:i], (CrossSec@data[which(SortIVec > 0)[length(which(SortIVec > 0))]:i,PredVar] - 2*CrossSec@data[which(SortIVec > 0)[length(which(SortIVec > 0))]:i,ErrVar]), type = 'l', col='red', xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), lty=2, xlab="", ylab="", lwd=2, axes=FALSE)
          par(new=F)
        }
        else{
          #Use Points
          plot(CrossSec@data[PositionName][i,1], (CrossSec@data[i,PredVar] + 2*CrossSec@data[i,ErrVar]), type = 'p', pch=16, col='red', xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), lty=2, xlab="", ylab="", lwd=2, axes=FALSE)
          par(new=T)
          plot(CrossSec@data[PositionName][i,1], (CrossSec@data[i,PredVar] - 2*CrossSec@data[i,ErrVar]), type = 'p', pch=16, col='red', xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), lty=2, xlab="", ylab="", lwd=2, axes=FALSE)
          par(new=F)
        }
      }
      else{
        #This is the only interpolation boundary. Simply plot the error bars!
        plot(CrossSec@data[PositionName][,1], (CrossSec@data[,PredVar] + 2*CrossSec@data[,ErrVar]), type = 'l', col='red', xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), lty=2, xlab="", ylab="", lwd=2, axes=FALSE)
        par(new=T)
        plot(CrossSec@data[PositionName][,1], (CrossSec@data[,PredVar] - 2*CrossSec@data[,ErrVar]), type = 'l', col='red', xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), lty=2, xlab="", ylab="", lwd=2, axes=FALSE)
        par(new=F)
      }
    }
  }
}


#A horizontal line is placed from 0 to length(CrossSec), with y-axis position as the average of yMinLinLoc and yMaxLinLoc. 
#The names of the counties are placed along this line. The cross section name is placed at either end, with y-axis location determined by yXCNameLoc.
#Assumes that the name of the cross section is Letter-Letter' (e.g. A-A'), so XCName should be just a letter.
#The county names are separated by vertical lines with y-axis placement according to yMinLinLoc and yMaxLinLoc.
NameCounties = function(CrossSec,     #Cross Section data
                        PositionName, #Field name in CrossSec indicating the position of points along the cross section (e.g. Length)
                        CountyField,  #Field name indicating the county for a cross section point (e.g. COUNTYFP). 
                        CountyName,   #Names of the counties as a string vector
                        yTexLoc,      #Text location for the county names
                        yMaxLinLoc,   #Maximum y-axis value for the extension of the county divisor bars along the cross section
                        yMinLinLoc,   #Minimum y-axis value for the extension of the county divisor bars along the cross section
                        yXCNameLoc,   #y-axis location of the cross section letter 
                        XCname        #Name/Letter of the cross section
                        ){
  SortIVec = vector('numeric',length=length(CrossSec@data[PositionName][,1]))
  text(x=0, y=yXCNameLoc, XCname, cex=1.5)
  text(x=length(CrossSec@data[PositionName][,1]), y=yXCNameLoc, paste(XCname,"'",sep=""), cex=1.5)
  lines(x=c(0,length(CrossSec@data[PositionName][,1])),y=c((yMaxLinLoc + yMinLinLoc)/2,(yMaxLinLoc + yMinLinLoc)/2), lwd=2)
  lines(x=c(0,0),y=c(yMaxLinLoc, yMinLinLoc), lwd=2)
  lines(x=c(length(CrossSec@data[PositionName][,1]),length(CrossSec@data[PositionName][,1])),y=c(yMaxLinLoc, yMinLinLoc), lwd=2)
  for (i in 1:length(CrossSec@data[CountyField][,1])){
    if (i == 1){
      if (CrossSec@data[CountyField][i,1] != CrossSec@data[CountyField][i+1,1]){
        #This is a different county.
        SortIVec[i] = 1
        lines(x=c(CrossSec@data[PositionName][i,1], CrossSec@data[PositionName][i,1]), y=c(yMaxLinLoc, yMinLinLoc), lty=1, lwd=2)
        text(x=CrossSec@data[PositionName][i,1]/2, y=yTexLoc, CountyName[length(which(SortIVec>0))], cex=1.2)
      }
    }
    else if (i > 2 && i != length(SortIVec)){
      #Check if there have been any boundaries yet
      if (length(which(SortIVec>0)) > 0){
        #If the location at i doesn't equal the same as the location before i, then they are different.
        if (CrossSec@data[CountyField][i,1] != CrossSec@data[CountyField][i-1,1]){
          SortIVec[i] = 1
          lines(x=c(CrossSec@data[PositionName][i,1], CrossSec@data[PositionName][i,1]), y=c(yMaxLinLoc, yMinLinLoc), lty=1, lwd=2)
          text(x=(CrossSec@data[PositionName][,1][which(SortIVec>0)[length(which(SortIVec>0))-1]] + CrossSec@data[PositionName][i,1])/2, y=yTexLoc, CountyName[length(which(SortIVec>0))], cex=1.2)
        }
      }
      else{
        #This is the first boundary
        #If the location at i doesn't equal the same as the location before i, then they are different.
        if (CrossSec@data[CountyField][i,1] != CrossSec@data[CountyField][i-1,1]){
          SortIVec[i] = 1
          lines(x=c(CrossSec@data[PositionName][i,1], CrossSec@data[PositionName][i,1]), y=c(yMaxLinLoc, yMinLinLoc), lty=1, lwd=2)
          text(x=CrossSec@data[PositionName][i,1]/2, y=yTexLoc, CountyName[length(which(SortIVec>0))], cex=1.2)
        }
      }
    }
    else if (i == length(SortIVec)){
      if (length(which(SortIVec>0)) != 0){
        #Give a name to the last county
        text(x=(CrossSec@data[PositionName][,1][which(SortIVec>0)[length(which(SortIVec>0))]] + CrossSec@data[PositionName][i,1])/2, y=yTexLoc, CountyName[length(which(SortIVec>0))+1], cex=1.2)
      }
      else{
        #This is the only county in the cross section. Assign it a name.
        text(x=(CrossSec@data[PositionName][i,1])/2, y=yTexLoc, CountyName, cex=1.2)
      }
    }    
  }
}


#A horizontal line is placed from 0 to length(CrossSec), with y-axis position as the average of yMinLinLoc and yMaxLinLoc.
#The names of the states are placed along this line, and the cross section name is placed at either end, with y-axis location determined by yXCNameLoc.
#Assumes that the name of the cross section is Letter-Letter' (e.g. A-A'), so XCName should be just a letter.
#The state names are separated by vertical lines with y-axis placement according to yMinLinLoc and yMaxLinLoc.
NameStates = function(CrossSec,        #Cross Section data
                      PositionName,    #Field name in CrossSec indicating the position of points along the cross section (e.g. Length)
                      StateField,      #Field name indicating the state for a cross section point (e.g. STATEFP).
                      StateName,       #Names of the states as a string vector
                      yTexLoc,         #Text location for the state names
                      yMaxLinLoc,      #Maximum y-axis value for the extension of the state divisor bars along the cross section
                      yMinLinLoc,      #Minimum y-axis value for the extension of the state divisor bars along the cross section
                      yXCNameLoc,      #y-axis location of the cross section letter 
                      XCname           #Name/Letter of the cross section
                      ){
  SortIVec = vector('numeric',length=length(CrossSec@data[PositionName][,1]))
  text(x=0, y=yXCNameLoc, XCname, cex=1.5)
  text(x=length(CrossSec@data[PositionName][,1]), y=yXCNameLoc, paste(XCname,"'",sep=""), cex=1.5)
  lines(x=c(0,length(CrossSec@data[PositionName][,1])),y=c((yMaxLinLoc + yMinLinLoc)/2,(yMaxLinLoc + yMinLinLoc)/2), lwd=2)
  lines(x=c(0,0),y=c(yMaxLinLoc, yMinLinLoc), lwd=2)
  lines(x=c(length(CrossSec@data[PositionName][,1]),length(CrossSec@data[PositionName][,1])),y=c(yMaxLinLoc, yMinLinLoc), lwd=2)
  for (i in 1:length(CrossSec@data[StateField][,1])){
    if (i == 1){
      if (CrossSec@data[StateField][i,1] != CrossSec@data[StateField][i+1,1]){
        #This is a different state.
        SortIVec[i] = 1
        lines(x=c(CrossSec@data[PositionName][i,1], CrossSec@data[PositionName][i,1]), y=c(yMaxLinLoc, yMinLinLoc), lty=1, lwd=2)
        text(x=CrossSec@data[PositionName][i,1]/2, y=yTexLoc, StateName[length(which(SortIVec>0))], cex=1.2)
      }
    }
    else if (i > 2 && i != length(SortIVec)){
      #Check if there have been any boundaries yet
      if (length(which(SortIVec>0)) > 0){
        #If the location at i doesn't equal the same as the location before i, then they are different.
        if (CrossSec@data[StateField][i,1] != CrossSec@data[StateField][i-1,1]){
          SortIVec[i] = 1
          lines(x=c(CrossSec@data[PositionName][i,1], CrossSec@data[PositionName][i,1]), y=c(yMaxLinLoc, yMinLinLoc), lty=1, lwd=2)
          text(x=(CrossSec@data[PositionName][,1][which(SortIVec>0)[length(which(SortIVec>0))-1]] + CrossSec@data[PositionName][i,1])/2, y=yTexLoc, StateName[length(which(SortIVec>0))], cex=1.2)
        }
      }
      else{
        #This is the first boundary
        #If the location at i doesn't equal the same as the location before i, then they are different.
        if (CrossSec@data[StateField][i,1] != CrossSec@data[StateField][i-1,1]){
          SortIVec[i] = 1
          lines(x=c(CrossSec@data[PositionName][i,1], CrossSec@data[PositionName][i,1]), y=c(yMaxLinLoc, yMinLinLoc), lty=1, lwd=2)
          text(x=CrossSec@data[PositionName][i,1]/2, y=yTexLoc, StateName[length(which(SortIVec>0))], cex=1.2)
        }
      }
    }
    else if (i == length(SortIVec)){
      if (length(which(SortIVec>0)) != 0){
        #Give a name to the last state
        text(x=(CrossSec@data[PositionName][,1][which(SortIVec>0)[length(which(SortIVec>0))]] + CrossSec@data[PositionName][i,1])/2, y=yTexLoc, StateName[length(which(SortIVec>0))+1], cex=1.2)
      }
      else{
        #This is the only state in the cross section. Assign it a name.
        text(x=(CrossSec@data[PositionName][i,1])/2, y=yTexLoc, StateName, cex=1.2)
      }
    }    
  }
}



#Example Cross Section Plot and Processing
setwd()
XSG = readOGR(dsn=getwd(), layer="CrossSectionG")
XSG_sort = XSG[do.call(order, XSG@data['POINT_X']), ]
#Add a PositionName field called Length
XSG_sort$Length = 0
for (i in 1:length(XSG_sort$POINT_X)){
  XSG_sort$Length[i] = i-1  
}
rm(i)


png(filename="ExampleCrossSection_BeforeRunningSortMixedIntBounds.png", width = 1500, height = 1200, res=150)
plot(XSG_sort$Length, XSG_sort$D80Cp, type = 'l', xlim=c(0,275), ylim = c(6000, -1000), main=expression(paste("Depth to 80 ",degree,"C Along Cross Section G-G'")), xlab="Distance (km)", ylab="Depth (m)", lwd=2, cex.lab=1.5, cex.axis=1.5,cex.main=1.5)
par(new=T)
ErrorBarXC(CrossSec=XSG_sort, PositionName='Length', PredVar="D80Cp", ErrVar="D80Ce", xLimMin=0, xLimMax=275, yLimBot=6000, yLimTop=-1000, IntBoundLen=0.25, IntBoundNames="WormSect", LinWidBound=1, BoundExtend=0.05)
CountyName = c('Potter, PA','Tioga, PA','','Chemung, NY','Tompkins, NY')
text(152,-1000,'Steuben, NY',cex=1.1)
NameCounties(CrossSec=XSG_sort, PositionName='Length', CountyField = 'COUNTYFP', CountyName = CountyName, yTexLoc = -1000, yMaxLinLoc = -700, yMinLinLoc = -1100, yXCNameLoc = -600, XCname = "G")
legend(130,4000,legend=c(expression(paste('Predicted Mean (',hat(mu),")")), expression(paste(hat(mu) %+-% 2, "SE")), "Interpolation Boundary"), lty=c(1,2,2), lwd=c(2,2,1), col=c('black','red','black'), bty="n", cex=1.5, seg.len=1.5, x.intersp = 0.5, y.intersp = 1.1)
dev.off()


#Check if any of the interpolation boundaries switchback
XSG_sort = SortMixedIntBounds(XSG_sort, "WormSect", "Length")

png(filename="ExampleCrossSection_AfterRunningSortMixedIntBounds.png", width = 1500, height = 1200, res=150)
plot(XSG_sort$Length, XSG_sort$D80Cp, type = 'l', xlim=c(0,275), ylim = c(6000, -1000), main=expression(paste("Depth to 80 ",degree,"C Along Cross Section G-G'")), xlab="Distance (km)", ylab="Depth (m)", lwd=2, cex.lab=1.5, cex.axis=1.5,cex.main=1.5)
par(new=T)
ErrorBarXC(CrossSec=XSG_sort, PositionName='Length', PredVar="D80Cp", ErrVar="D80Ce", xLimMin=0, xLimMax=275, yLimBot=6000, yLimTop=-1000, IntBoundLen=0.25, IntBoundNames="WormSect", LinWidBound=1, BoundExtend=0.05)
CountyName = c('Potter, PA','Tioga, PA','','Chemung, NY','Tompkins, NY')
text(152,-1000,'Steuben, NY',cex=1.1)
NameCounties(CrossSec=XSG_sort, PositionName='Length', CountyField = 'COUNTYFP', CountyName = CountyName, yTexLoc = -1000, yMaxLinLoc = -700, yMinLinLoc = -1100, yXCNameLoc = -600, XCname = "G")
legend(130,4000,legend=c(expression(paste('Predicted Mean (',hat(mu),")")), expression(paste(hat(mu) %+-% 2, "SE")), "Interpolation Boundary"), lty=c(1,2,2), lwd=c(2,2,1), col=c('black','red','black'), bty="n", cex=1.5, seg.len=1.5, x.intersp = 0.5, y.intersp = 1.1)
dev.off()

png(filename="ExampleCrossSection_NoBoundaryExtension.png", width = 1500, height = 1200, res=150)
plot(XSG_sort$Length, XSG_sort$D80Cp, type = 'l', xlim=c(0,275), ylim = c(6000, -1000), main=expression(paste("Depth to 80 ",degree,"C Along Cross Section G-G'")), xlab="Distance (km)", ylab="Depth (m)", lwd=2, cex.lab=1.5, cex.axis=1.5,cex.main=1.5)
par(new=T)
ErrorBarXC(CrossSec=XSG_sort, PositionName='Length', PredVar="D80Cp", ErrVar="D80Ce", xLimMin=0, xLimMax=275, yLimBot=6000, yLimTop=-1000, IntBoundLen=0, IntBoundNames="WormSect", LinWidBound=1, BoundExtend=0)
CountyName = c('Potter, PA','Tioga, PA','','Chemung, NY','Tompkins, NY')
text(152,-1000,'Steuben, NY',cex=1.1)
NameCounties(CrossSec=XSG_sort, PositionName='Length', CountyField = 'COUNTYFP', CountyName = CountyName, yTexLoc = -1000, yMaxLinLoc = -700, yMinLinLoc = -1100, yXCNameLoc = -600, XCname = "G")
legend(130,4000,legend=c(expression(paste('Predicted Mean (',hat(mu),")")), expression(paste(hat(mu) %+-% 2, "SE")), "Interpolation Boundary"), lty=c(1,2,2), lwd=c(2,2,1), col=c('black','red','black'), bty="n", cex=1.5, seg.len=1.5, x.intersp = 0.5, y.intersp = 1.1)
dev.off()

png(filename="ExampleCrossSection_SelectiveBoundaryExtension.png", width = 1500, height = 1200, res=150)
plot(XSG_sort$Length, XSG_sort$D80Cp, type = 'l', xlim=c(0,275), ylim = c(6000, -1000), main=expression(paste("Depth to 80 ",degree,"C Along Cross Section G-G'")), xlab="Distance (km)", ylab="Depth (m)", lwd=2, cex.lab=1.5, cex.axis=1.5,cex.main=1.5)
par(new=T)
ErrorBarXC(CrossSec=XSG_sort, PositionName='Length', PredVar="D80Cp", ErrVar="D80Ce", xLimMin=0, xLimMax=275, yLimBot=6000, yLimTop=-1000, IntBoundLen=0.25, IntBoundNames="WormSect", LinWidBound=1, BoundExtend=0)
CountyName = c('Potter, PA','Tioga, PA','','Chemung, NY','Tompkins, NY')
text(152,-1000,'Steuben, NY',cex=1.1)
NameCounties(CrossSec=XSG_sort, PositionName='Length', CountyField = 'COUNTYFP', CountyName = CountyName, yTexLoc = -1000, yMaxLinLoc = -700, yMinLinLoc = -1100, yXCNameLoc = -600, XCname = "G")
legend(130,4000,legend=c(expression(paste('Predicted Mean (',hat(mu),")")), expression(paste(hat(mu) %+-% 2, "SE")), "Interpolation Boundary"), lty=c(1,2,2), lwd=c(2,2,1), col=c('black','red','black'), bty="n", cex=1.5, seg.len=1.5, x.intersp = 0.5, y.intersp = 1.1)
dev.off()



#### Reservoirs ####

#Function for placing reservoirs onto the cross section plots.
#The rectangle option can likely be improved by recording the joints of the lines as points, and then when the reservoir terminates, use lines() to connect them all.
#It's important to note that if this function is used along with the SortMixedIntegerBounds, the points that are switched will need to have their data switched back for the reservoirs to plot properly. Example below for important parameters.
#Currently does not handle if there are two reservoirs at the same depth with different formation thicknesses 
PlotReservoirs = function(CrossSec,                #Cross section name
                          ResData,                 #Database containing the reservoir information
                          Res,                     #Field used to indicate presence of a reservoir
                          Thermal,                 #Thermal field for plotting.
                          Depth = NA,              #Field giving depth of reservoir (optional)
                          FormThick = NA,          #Field for formation thickness (optional)
                          NewAxis = NA,            #Indicates if a new depth axis should be made on side 4. (optional)
                          Rects = 0,               #Indicates if reservoirs should be plotted as rectangles (1) or as blue/green points (0, default)
                          yResBot = NA,            #Second y-axis value at bottom (optional)
                          yResTop = 0,             #Second y-axis value at top (optional)
                          colResTop = 'blue',      #Color of the top of the reservoir for points option (Rects = 0)
                          colResBot = 'green',     #Color of the bottom of the reservoir for points option (Rects = 0)
                          colMean = 'blue',        #Color indicating presence of a reservoir on the mean line
                          colResRectsField = NA,   #Field name to color rectangles by
                          colResRects = NA,        #vector of colors for the rectangles in rgb().
                          colSeps = NA,            #Vector of color separation values
                          transpar = 0.3,          #Transparency parameter for the rectangles
                          xLimMin,                 #x-axis start
                          xLimMax,                 #x-axis stop
                          yLimBot,                 #y-axis value at bottom
                          yLimTop                  #y-axis Value at top
){
  if (is.na(colResRectsField) == FALSE){
    #Make the colors transparent
    colors = adjustcolor(colResRects, alpha=transpar)
  }
  RemResInds = NA
  if (is.na(Depth)){
    #This is for plotting the blue sections along the mean line.
    PlotBack = -1
    #Loop through all points along the cross section
    for (i in 1:length(CrossSec@data$Length)){
      #If there are reservoirs at this location, plot an indicator along the mean.
      if (PlotBack != -1){
        if (CrossSec@data[Res][i,] == 0){
          #This means that the reservoir section has ended. Plot the data back to PlotBack
          par(new=T)
          plot(CrossSec@data$Length[PlotBack:(i-1)], CrossSec@data[Thermal][PlotBack:(i-1),], col=colMean, type='l', xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), xlab="", ylab="", lwd=2, axes=FALSE)
          
          #Set PlotBack to -1
          PlotBack = -1
        }
      }
      else if (CrossSec@data[Res][i,] > 0){
        #Used to check for how far the reservoirs are present
        PlotBack = i
      }
    }
  }
  else {
    #Depth field is specified. Check to see if thermal variable is a depth to temp or temp at depth
    if (is.na(NewAxis)){
      #No new axis is needed (depth to temp variable). Plot the reservoirs on the existing y-axis
      PlotBack = -1
      #Loop through all points along the cross section
      for (i in 1:length(CrossSec@data$Length)){
        #If there are reservoirs at this location, plot them at their depth.
        if (PlotBack != -1){
          ResSpotCheck = ResData@data[which(ResData@data$Length == CrossSec@data$Length[i]),]
          ResSpotCheck$Start = -1
          if (length(ResSpotCheck)>0){
            Inds2 = which(ResSpotCheck[Depth][,1]==0)
            if (length(Inds2) > 0){
              #Remove reservoirs with 0 depth
              ResSpotCheck = ResSpotCheck[-Inds2,]
            }
          }
          if (CrossSec@data[Res][i,] != 0){
            if (Rects == 0){
              par(new=T)
              plot(rep(CrossSec@data$Length[i], length(ResSpotCheck[Depth][,1])), ResSpotCheck[Depth][,1], xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), axes=FALSE, xlab='', ylab='', col=colResTop, pch=16)
              par(new=T)
              plot(rep(CrossSec@data$Length[i], length(ResSpotCheck[Depth][,1])), ResSpotCheck[Depth][,1]+ResSpotCheck[FormThick][,1], axes=FALSE, xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), xlab='', ylab='', col=colResBot, pch=16)
            }
            else if (Rects == 1){
              #The reservoirs could go back to ResSpot$Start, but not all reservoirs have to continue. Check to see if all reservoirs in ResSpot exist here.
              if (length(ResSpot$Start) > 0){
                #The reservoir could be continued from the previous set of reservoirs
                for (j in 1:length(ResSpot[Depth][,1])){
                  if (any((ResSpotCheck[Depth][,1] == ResSpot[Depth][j,1])) == FALSE){
                    #This reservoir has terminated. Plot it back to the ResSpot$Start location.
                    if (is.na(colResRectsField) == FALSE){
                      #Get the color for the reservoir field specified
                      color = NA
                      for (k in 1:length(colSeps)){
                        if (ResSpot[colResRectsField][j,1] < colSeps[k]){
                          color = colors[k]
                          break
                        }
                        if (k == length(colSeps)){
                          color = colors[k+1]
                        }
                      }
                      par(new=T)
                      rect(xleft = ResSpot$Start[j], xright = (i-2), ytop = ResSpot[Depth][j,1], ybottom = ResSpot[Depth][j,1]+ResSpot[FormThick][j,1], xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), xlab='', ylab='', col=color)
                    }
                    else{
                      par(new=T)
                      rect(xleft = ResSpot$Start[j], xright = (i-2), ytop = ResSpot[Depth][j,1], ybottom = ResSpot[Depth][j,1]+ResSpot[FormThick][j,1], xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), xlab='', ylab='', col=rgb(red=0.2, green=0.2, blue=1.0, alpha=transpar))
                    }
                    #Remove this reservoir from ResSpot after loop concludes
                    if (length(RemResInds) > 1){
                      RemResInds = c(RemResInds, j)
                    }
                    else if (is.na(RemResInds)){
                      RemResInds = j
                    }
                    else{
                      RemResInds = c(RemResInds, j)
                    }
                  }
                }
              }
              #Now check to see if there are any new reservoirs that begin and add them to the ResSpot.
              for (k in 1:length(ResSpotCheck[Depth][,1])){
                if (any(ResSpot[Depth][,1] == ResSpotCheck[Depth][k,1]) == FALSE){
                  #Add this reservoir to ResSpot with the new start location.
                  ResSpotCheck$Start[k] = (i-1)
                  ResSpot = rbind(ResSpot, ResSpotCheck[k,])                  
                }
              }
              #Remove reservoirs that terminated.
              if (length(RemResInds) > 1){
                ResSpot = ResSpot[-RemResInds,]
                #Set RemResInds back to NA
                RemResInds = NA
              }
              else if (is.na(RemResInds) == FALSE){
                ResSpot = ResSpot[-RemResInds,]
                #Set RemResInds back to NA
                RemResInds = NA
              }
              
            }
          }
          if (CrossSec@data[Res][i,] == 0){
            if (length(ResSpot$Start) > 0){
              #The reservoir could be continued from the previous set of reservoirs
              for (j in 1:length(ResSpot[Depth][,1])){
                #This reservoir has terminated. Plot it back to the ResSpot$Start location.
                if (is.na(colResRectsField) == FALSE){
                  #Get the color for the reservoir field specified
                  color = NA
                  for (k in 1:length(colSeps)){
                    if (ResSpot[colResRectsField][j,1] < colSeps[k]){
                      color = colors[k]
                      break
                    }
                    if (k == length(colSeps)){
                      color = colors[k+1]
                    }
                  }
                  par(new=T)
                  rect(xleft = ResSpot$Start[j], xright = (i-2), ytop = ResSpot[Depth][j,1], ybottom = ResSpot[Depth][j,1]+ResSpot[FormThick][j,1], xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), xlab='', ylab='', col=color)
                }
                else{
                  par(new=T)
                  rect(xleft = ResSpot$Start[j], xright = (i-2), ytop = ResSpot[Depth][j,1], ybottom = ResSpot[Depth][j,1]+ResSpot[FormThick][j,1], xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), xlab='', ylab='', col=rgb(red=0.2, green=0.2, blue=1.0, alpha=transpar))
                }
              }
            }
            #Set PlotBack to 0 and clear contents of ResSpot
            PlotBack = -1
            ResSpot = NA
          }
        }
        else if (CrossSec@data[Res][i,] > 0){
          #Used to check for how far the reservoirs are present
          PlotBack = i
          #Get all the reservoirs at this spot from the database
          ResSpot = ResData@data[which(ResData$Length == CrossSec@data$Length[i]),]
          if (all(ResSpot[Depth][,1] == 0)){
            PlotBack = -1
            ResSpot = NA
          }
          else if (any(ResSpot[Depth][,1] == 0)){
            Inds = which(ResSpot[Depth][,1]==0)
            ResSpot = ResSpot[-Inds,]
            if (Rects == 0){
              par(new=T)
              plot(rep(CrossSec@data$Length[i], length(ResSpot[Depth][,1])), ResSpot[Depth][,1], axes=FALSE, xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), xlab='', ylab='', col=colResTop, pch=16)
              par(new=T)
              plot(rep(CrossSec@data$Length[i], length(ResSpot[Depth][,1])), ResSpot[Depth][,1]+ResSpot[FormThick][,1], axes=FALSE, xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), xlab='', ylab='', col=colResBot, pch=16)
            }
            else if (Rects == 1){
              #Do not plot anything. Record the position of the start of the reservoir for each reservoir.
              ResSpot$Start = (i-1)
            }
          }
          else{
            if (Rects == 0){
              par(new=T)
              plot(rep(CrossSec@data$Length[i], length(ResSpot[Depth][,1])), ResSpot[Depth][,1], axes=FALSE, xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), xlab='', ylab='', col=colResTop, pch=16)
              par(new=T)
              plot(rep(CrossSec@data$Length[i], length(ResSpot[Depth][,1])), ResSpot[Depth][,1]+ResSpot[FormThick][,1], axes=FALSE, xlim=c(xLimMin, xLimMax), ylim = c(yLimBot,yLimTop), xlab='', ylab='', col=colResBot, pch=16)
            }
            else if (Rects == 1){
              #Do not plot anything. Record the position of the start of the reservoir for each reservoir.
              ResSpot$Start = (i-1)
            }
          }
        }
      }
    }
    else{
      #A new depth axis is needed for the reservoirs. Add it on the right.
      par(new=T)
      plot(-200, -200, xlim=c(xLimMin,xLimMax), ylim = c(yResBot,yResTop), xlab="", ylab="", lwd=2, axes=FALSE)
      mtext("Reservoir Depth (m)",side=4,line=3, cex=1.8) 
      axis(4, ylim=c(yResBot,yResTop), cex.axis=1.8)
      
      PlotBack = -1
      #Loop through all points along the cross section
      for (i in 1:length(CrossSec@data$Length)){
        #If there are reservoirs at this location, plot them at their depth.
        if (PlotBack != -1){
          #Gather reservoirs at the new location.
          ResSpotCheck = ResData@data[which(ResData@data$Length == CrossSec@data$Length[i]),]
          ResSpotCheck$Start = -1
          if (length(ResSpotCheck[,1])>0){
            Inds2 = which(ResSpotCheck[Depth][,1]==0)
            if (length(Inds2) > 0){
              #Remove reservoirs with 0 depth
              ResSpotCheck = ResSpotCheck[-Inds2,]
              #May need to check if all rows were deleted.
            }
          }
          if (CrossSec@data[Res][,1][i] != 0){
            #There are reservoirs in this location.
            if (Rects == 0){
              par(new=T)
              plot(rep(CrossSec@data$Length[i], length(ResSpotCheck[Depth][,1])), ResSpotCheck[Depth][,1], xlim=c(xLimMin, xLimMax), ylim = c(yResBot,yResTop), axes=FALSE, xlab='', ylab='', col=colResTop, pch=16)
              par(new=T)
              plot(rep(CrossSec@data$Length[i], length(ResSpotCheck[Depth][,1])), ResSpotCheck[Depth][,1]+ResSpotCheck[FormThick][,1], axes=FALSE, xlim=c(xLimMin, xLimMax), ylim = c(yResBot,yResTop), xlab='', ylab='', col=colResBot, pch=16)
            }
            else if (Rects == 1){
              #The reservoirs could go back to ResSpot$Start, but not all reservoirs have to continue. Check to see if all reservoirs in ResSpot exist here.
              if (length(ResSpot$Start) > 0){
                #The reservoir could be continued from the previous set of reservoirs
                for (j in 1:length(ResSpot[Depth][,1])){
                  if (any((ResSpotCheck[Depth][,1] == ResSpot[Depth][j,1])) == FALSE){
                    #This reservoir has terminated. Plot it back to the ResSpot$Start location.
                    if (is.na(colResRectsField) == FALSE){
                      #Get the color for the reservoir field specified
                      color = NA
                      for (k in 1:length(colSeps)){
                        if (ResSpot[colResRectsField][j,1] < colSeps[k]){
                          color = colors[k]
                          break
                        }
                        if (k == length(colSeps)){
                          color = colors[k+1]
                        }
                      }
                      par(new=T)
                      rect(xleft = ResSpot$Start[j], xright = (i-2), ytop = ResSpot[Depth][j,1], ybottom = ResSpot[Depth][j,1]+ResSpot[FormThick][j,1], xlim=c(xLimMin, xLimMax), ylim = c(yResBot,yResTop), xlab='', ylab='', col=color)
                    }
                    else{
                      par(new=T)
                      rect(xleft = ResSpot$Start[j], xright = (i-2), ytop = ResSpot[Depth][j,1], ybottom = ResSpot[Depth][j,1]+ResSpot[FormThick][j,1], xlim=c(xLimMin, xLimMax), ylim = c(yResBot,yResTop), xlab='', ylab='', col=rgb(red=0.2, green=0.2, blue=1.0, alpha=transpar))
                    }
                    #Remove this reservoir from ResSpot after loop concludes
                    if (length(RemResInds) > 1){
                      RemResInds = c(RemResInds, j)
                    }
                    else if (is.na(RemResInds)){
                      RemResInds = j
                    }
                    else{
                      RemResInds = c(RemResInds, j)
                    }
                  }
                }
              }
              #Now check to see if there are any new reservoirs that begin and add them to the ResSpot.
              for (k in 1:length(ResSpotCheck[Depth][,1])){
                if (any(ResSpot[Depth][,1] == ResSpotCheck[Depth][k,1]) == FALSE){
                  #Add this reservoir to ResSpot with the new start location.
                  ResSpotCheck$Start[k] = (i-1)
                  ResSpot = rbind(ResSpot, ResSpotCheck[k,])                  
                }
              }
              #Remove reservoirs that terminated.
              if (length(RemResInds) > 1){
                ResSpot = ResSpot[-RemResInds,]
                #Set RemResInds back to NA
                RemResInds = NA
              }
              else if (is.na(RemResInds) == FALSE){
                ResSpot = ResSpot[-RemResInds,]
                #Set RemResInds back to NA
                RemResInds = NA
              }
            }
          }
          if (CrossSec@data[Res][i,] == 0){
            if (length(ResSpot$Start) > 0){
              #The reservoir could be continued from the previous set of reservoirs
              for (j in 1:length(ResSpot[Depth][,1])){
                #This reservoir has terminated. Plot it back to the ResSpot$Start location.
                if (is.na(colResRectsField) == FALSE){
                  #Get the color for the reservoir field specified
                  color = NA
                  for (k in 1:length(colSeps)){
                    if (ResSpot[colResRectsField][j,1] < colSeps[k]){
                      color = colors[k]
                      break
                    }
                    if (k == length(colSeps)){
                      color = colors[k+1]
                    }
                  }
                  par(new=T)
                  rect(xleft = ResSpot$Start[j], xright = (i-2), ytop = ResSpot[Depth][j,1], ybottom = ResSpot[Depth][j,1]+ResSpot[FormThick][j,1], xlim=c(xLimMin, xLimMax), ylim = c(yResBot,yResTop), xlab='', ylab='', col=color)
                }
                else{
                  par(new=T)
                  rect(xleft = ResSpot$Start[j], xright = (i-2), ytop = ResSpot[Depth][j,1], ybottom = ResSpot[Depth][j,1]+ResSpot[FormThick][j,1], xlim=c(xLimMin, xLimMax), ylim = c(yResBot,yResTop), xlab='', ylab='', col=rgb(red=0.2, green=0.2, blue=1.0, alpha=transpar))
                }
              }
            }
            #Clearly all reservoirs have terminated. Set PlotBack to 0 and clear contents of ResSpot.
            PlotBack = -1
            ResSpot = NA
          }
        }
        else if (CrossSec@data[Res][i,] != 0){
          #There are reservoirs here. Set PltBack to i. Used to check for how far the reservoirs are present
          PlotBack = i
          #Get all the reservoirs at this spot from the database
          ResSpot = ResData@data[which(ResData@data$Length == CrossSec@data$Length[i]),]
          if (all(ResSpot[Depth][,1] == 0)){
            #No reservoirs exist here. Error in data input file.
            PlotBack = -1
            ResSpot = NA
          }
          else if (any(ResSpot[Depth][,1] == 0)){
            #Some reservoirs have 0 depth. Remove them.
            Inds = which(ResSpot[Depth][,1]==0)
            ResSpot = ResSpot[-Inds,]
            if (Rects == 0){
              #Plot the reservoirs that do not have 0 depth.
              par(new=T)
              plot(rep(CrossSec@data$Length[i], length(ResSpot[Depth][,1])), ResSpot[Depth][,1], axes=FALSE, xlim=c(xLimMin, xLimMax), ylim = c(yResBot,yResTop), xlab='', ylab='', col=colResTop, pch=16)
              par(new=T)
              plot(rep(CrossSec@data$Length[i], length(ResSpot[Depth][,1])), ResSpot[Depth][,1]+ResSpot[FormThick][,1], axes=FALSE, xlim=c(xLimMin, xLimMax), ylim = c(yResBot,yResTop), xlab='', ylab='', col=colResBot, pch=16)
            }
            else if (Rects == 1){
              #Do not plot anything. Record the position of the start of the reservoir for each reservoir.
              ResSpot$Start = (i-1)
            }
          }
          else{
            #All depths are fine.
            if (Rects == 0){
              #Plot the reservoirs
              par(new=T)
              plot(rep(CrossSec@data$Length[i], length(ResSpot[Depth][,1])), ResSpot[Depth][,1], axes=FALSE, xlim=c(xLimMin, xLimMax), ylim = c(yResBot,yResTop), xlab='', ylab='', col=colResTop, pch=16)
              par(new=T)
              plot(rep(CrossSec@data$Length[i], length(ResSpot[Depth][,1])), ResSpot[Depth][,1]+ResSpot[FormThick][,1], axes=FALSE, xlim=c(xLimMin, xLimMax), ylim = c(yResBot,yResTop), xlab='', ylab='', col=colResBot, pch=16)
            }
            else if (Rects == 1){
              #Do not plot anything. Record the position of the start of the reservoir for each reservoir.
              ResSpot$Start = (i-1)
            }
          }
        }
      }
    }
  }
}


#Example Reservoir cross section:

#Load cross section
XSA = readOGR(dsn=getwd(), layer="XSA_pts_ResInd", stringsAsFactors=FALSE)
XSA_sort = XSA[do.call(order, XSA@data['POINT_Y']), ]
XSA_sort$Length = 0
for (i in 1:length(XSA_sort$POINT_Y)){
  XSA_sort$Length[i] = i-1  
}
rm(i)
XSA_sort = SortMixedIntBounds(XSA_sort, "WormSect", "Length")
XSA_sort$FmnThickm = XSA$FmnThickm
XSA_sort$ThickUncer = XSA$ThickUncer
XSA_sort$ResDepthm = XSA$ResDepthm

#Load reservoir data along the cross section. This file must have a Length field that corresponds to the cross section.
ResData = readOGR(dsn=getwd(), layer="XSA_pts_Res1p5", stringsAsFactors=FALSE)
ResData = ResData[do.call(order, ResData@data['POINT_Y']), ]
ResData$Length = 0
Len = 1
for (i in 1:length(ResData$POINT_Y)){
  if (i != 1){
    if (ResData$POINT_Y[i] == ResData$POINT_Y[i-1]){
      if (ResData$POINT_X[i] == ResData$POINT_X[i-1]){
        #These are the same point, so should have the same length ID.
        ResData$Length[i] = ResData$Length[i-1]        
      }
      else{
        #These are not the same point, give them a different ID.
        if (Len == 1){
          Len = Len + 1
        }
        ResData$Length[i] = XSA_sort$Length[Len]
        Len = Len + 1
      }
    }
    else{
      #These are not the same point, give them a different ID.
      if (Len == 1){
        Len = Len + 1
      }
      ResData$Length[i] = XSA_sort$Length[Len]
      Len = Len + 1
    }
  }
  else{
    #The first point must get the first length number
    ResData$Length[i] = XSA_sort$Length[Len]
  }
}
rm(i)


#Plot of the reservoirs
#Use mar if a reservoir depth axis is to be added.
png(filename="ExampleCrossSection_ReservoirsAdded_SingleAxis_Rectangles_Colors.png", width = 14, height = 6, units="in", res=300)
par(mgp=c(2.5,0.8,0), mar = c(5,4,4,5))
plot(XSA_sort$Length, XSA_sort$D100C_Pred, type = 'l', xlim=c(0,1100), ylim = c(8000, -500), main=expression(paste("Depth to 100 ",degree,"C Along Cross Section A-A'")), xlab="Distance (km)", ylab="Depth (m)", lwd=2, cex.lab=1.8, cex.axis=1.8,cex.main=1.5)
axis(1, at=seq(100,1100,200), labels=seq(100,1100,200), cex.axis=1.8)
minor.tick(nx=4, ny=2, tick.ratio=0.5)
par(new=T)
ErrorBarXC2(CrossSec=XSA_sort, PredVar="D100C_Pred", ErrVar="D100C_Err", xLimMin=0, xLimMax=1100, yLimBot=8000, yLimTop=-500, IntBoundLen=0.25, IntBoundNames="WormSect", LinWidBound=1, BoundExtend=0.1)
StateName = c('West Virginia', '', 'Pennsylvania', 'New York')
NameStates(XSA_sort, StateName, -450, -400, 50, 300, "A", 1.5)
text(x=(XSA_sort$Length[which(XSA_sort$STATEFP == 24)][1]+XSA_sort$Length[which(XSA_sort$STATEFP == 42)][1])/2, y=-600, "MD", cex=0.85)
legend(130,5800,legend=c(expression(paste('Predicted Mean (',hat(mu),")")), expression(paste(hat(mu) %+-% 2, "SE")), "Interpolation Boundary"), lty=c(1,2,2), lwd=c(2,2,1), col=c('black','red','black'), bty="n", cex=1.5, seg.len=1.5, x.intersp = 0.5, y.intersp = 1.1)
legend(860,4300,legend=c("Reservoir RPI <0.01",  "0.01 - 0.1", "0.1 - 1.0", "1.0 - 10", expression(phantom(1)>="10")), pch=15, col=adjustcolor(c('red','orange','yellow','springgreen','green'), alpha=0.3), bty="n", cex=1.5, seg.len=1.5, x.intersp = 0.5, y.intersp = 1.1)
legend(860,4300,legend=c("Reservoir RPI <0.01",  "0.01 - 0.1", "0.1 - 1.0", "1.0 - 10", expression(phantom(1)>="10")), pch=22, col='black', bty="n", cex=1.5, seg.len=1.5, x.intersp = 0.5, y.intersp = 1.1)
PlotReservoirs(CrossSec = XSA_sort, ResData = ResData, Res = 'ThickUncer', Thermal = 'D100C_Pred', xLimMin=0, xLimMax=1100, yLimBot=8000, yLimTop=-500)
PlotReservoirs(CrossSec = XSA_sort, ResData = ResData, Res = 'ThickUncer', Thermal = 'T1p5_Pred', Depth = 'ResDepthm', FormThick = 'FmnThickm', colResRectsField = 'RPI', colResRects=c('red','orange','yellow','springgreen','green'), colSeps = c(0.01,0.1,1,10),NewAxis = 1, Rects=1, yResBot=2200, yResTop=800, xLimMin=0, xLimMax=1100, yLimBot=8000, yLimTop=0)
dev.off()

png(filename="ExampleCrossSection_ReservoirsAdded_SingleAxis_Rectangles.png", width = 14, height = 6, units="in", res=300)
par(mgp=c(2.5,0.8,0), mar = c(5,4,4,5))
plot(XSA_sort$Length, XSA_sort$D100C_Pred, type = 'l', xlim=c(0,1100), ylim = c(8000, -500), main=expression(paste("Depth to 100 ",degree,"C Along Cross Section A-A'")), xlab="Distance (km)", ylab="Depth (m)", lwd=2, cex.lab=1.8, cex.axis=1.8,cex.main=1.5)
axis(1, at=seq(100,1100,200), labels=seq(100,1100,200), cex.axis=1.8)
minor.tick(nx=4, ny=2, tick.ratio=0.5)
par(new=T)
ErrorBarXC2(CrossSec=XSA_sort, PredVar="D100C_Pred", ErrVar="D100C_Err", xLimMin=0, xLimMax=1100, yLimBot=8000, yLimTop=-500, IntBoundLen=0.25, IntBoundNames="WormSect", LinWidBound=1, BoundExtend=0.1)
StateName = c('West Virginia', '', 'Pennsylvania', 'New York')
NameStates(XSA_sort, StateName, -450, -400, 50, 300, "A", 1.5)
text(x=(XSA_sort$Length[which(XSA_sort$STATEFP == 24)][1]+XSA_sort$Length[which(XSA_sort$STATEFP == 42)][1])/2, y=-600, "MD", cex=0.85)
legend(-70,5500,legend=c(expression(paste('Predicted Mean (',hat(mu),")")), expression(paste(hat(mu) %+-% 2, "SE")), "Interpolation Boundary"), lty=c(1,2,2), lwd=c(2,2,1), col=c('black','red','black'), bty="n", cex=1.5, seg.len=1.5, x.intersp = 0.5, y.intersp = 1.1)
PlotReservoirs(CrossSec = XSA_sort, ResData = ResData, Res = 'ThickUncer', Thermal = 'D100C_Pred', xLimMin=0, xLimMax=1100, yLimBot=8000, yLimTop=-500)
PlotReservoirs(CrossSec = XSA_sort, ResData = ResData, Res = 'ThickUncer', Thermal = 'D100C_Pred', Depth = 'ResDepthm', FormThick = 'FmnThickm', NewAxis = NA, Rects=1, yResBot=4000, yResTop=0, xLimMin=0, xLimMax=1100, yLimBot=8000, yLimTop=-500)
dev.off()

png(filename="ExampleCrossSection_ReservoirsAdded_SingleAxis_Points.png", width = 14, height = 6, units="in", res=300)
par(mgp=c(2.5,0.8,0), mar = c(5,4,4,5))
plot(XSA_sort$Length, XSA_sort$D100C_Pred, type = 'l', xlim=c(0,1100), ylim = c(8000, -500), main=expression(paste("Depth to 100 ",degree,"C Along Cross Section A-A'")), xlab="Distance (km)", ylab="Depth (m)", lwd=2, cex.lab=1.8, cex.axis=1.8,cex.main=1.5)
axis(1, at=seq(100,1100,200), labels=seq(100,1100,200), cex.axis=1.8)
minor.tick(nx=4, ny=2, tick.ratio=0.5)
par(new=T)
ErrorBarXC2(CrossSec=XSA_sort, PredVar="D100C_Pred", ErrVar="D100C_Err", xLimMin=0, xLimMax=1100, yLimBot=8000, yLimTop=-500, IntBoundLen=0.25, IntBoundNames="WormSect", LinWidBound=1, BoundExtend=0.1)
StateName = c('West Virginia', '', 'Pennsylvania', 'New York')
NameStates(XSA_sort, StateName, -450, -400, 50, 300, "A", 1.5)
text(x=(XSA_sort$Length[which(XSA_sort$STATEFP == 24)][1]+XSA_sort$Length[which(XSA_sort$STATEFP == 42)][1])/2, y=-600, "MD", cex=0.85)
legend(-70,5500,legend=c(expression(paste('Predicted Mean (',hat(mu),")")), expression(paste(hat(mu) %+-% 2, "SE")), "Interpolation Boundary"), lty=c(1,2,2), lwd=c(2,2,1), col=c('black','red','black'), bty="n", cex=1.5, seg.len=1.5, x.intersp = 0.5, y.intersp = 1.1)
PlotReservoirs(CrossSec = XSA_sort, ResData = ResData, Res = 'ThickUncer', Thermal = 'D100C_Pred', xLimMin=0, xLimMax=1100, yLimBot=8000, yLimTop=-500)
PlotReservoirs(CrossSec = XSA_sort, ResData = ResData, Res = 'ThickUncer', Thermal = 'D100C_Pred', Depth = 'ResDepthm', FormThick = 'FmnThickm', NewAxis = NA, Rects=0, yResBot=4000, yResTop=0, xLimMin=0, xLimMax=1100, yLimBot=8000, yLimTop=-500)
dev.off()


png(filename="ExampleCrossSection_ReservoirsAdded_SeparateAxis_Rectangles.png", width = 14, height = 6, units="in", res=300)
par(mgp=c(2.5,0.8,0), mar = c(5,4,4,5))
plot(XSA_sort$Length, XSA_sort$D100C_Pred, type = 'l', xlim=c(0,1100), ylim = c(8000, -500), main=expression(paste("Depth to 100 ",degree,"C Along Cross Section A-A'")), xlab="Distance (km)", ylab="Depth (m)", lwd=2, cex.lab=1.8, cex.axis=1.8,cex.main=1.5)
axis(1, at=seq(100,1100,200), labels=seq(100,1100,200), cex.axis=1.8)
minor.tick(nx=4, ny=2, tick.ratio=0.5)
par(new=T)
ErrorBarXC2(CrossSec=XSA_sort, PredVar="D100C_Pred", ErrVar="D100C_Err", xLimMin=0, xLimMax=1100, yLimBot=8000, yLimTop=-500, IntBoundLen=0.25, IntBoundNames="WormSect", LinWidBound=1, BoundExtend=0.1)
StateName = c('West Virginia', '', 'Pennsylvania', 'New York')
NameStates(XSA_sort, StateName, -450, -400, 50, 300, "A", 1.5)
text(x=(XSA_sort$Length[which(XSA_sort$STATEFP == 24)][1]+XSA_sort$Length[which(XSA_sort$STATEFP == 42)][1])/2, y=-600, "MD", cex=0.85)
legend(-70,5500,legend=c(expression(paste('Predicted Mean (',hat(mu),")")), expression(paste(hat(mu) %+-% 2, "SE")), "Interpolation Boundary"), lty=c(1,2,2), lwd=c(2,2,1), col=c('black','red','black'), bty="n", cex=1.5, seg.len=1.5, x.intersp = 0.5, y.intersp = 1.1)
PlotReservoirs(CrossSec = XSA_sort, ResData = ResData, Res = 'ThickUncer', Thermal = 'D100C_Pred', xLimMin=0, xLimMax=1100, yLimBot=8000, yLimTop=-500)
PlotReservoirs(CrossSec = XSA_sort, ResData = ResData, Res = 'ThickUncer', Thermal = 'D100C_Pred', Depth = 'ResDepthm', FormThick = 'FmnThickm', NewAxis = 1, Rects=1, yResBot=4000, yResTop=0, xLimMin=0, xLimMax=1100, yLimBot=8000, yLimTop=-500)
dev.off()


png(filename="ExampleCrossSection_ReservoirsAdded_SeparateAxis_Points.png", width = 14, height = 6, units="in", res=300)
par(mgp=c(2.5,0.8,0), mar = c(5,4,4,5))
plot(XSA_sort$Length, XSA_sort$D100C_Pred, type = 'l', xlim=c(0,1100), ylim = c(8000, -500), main=expression(paste("Depth to 100 ",degree,"C Along Cross Section A-A'")), xlab="Distance (km)", ylab="Depth (m)", lwd=2, cex.lab=1.8, cex.axis=1.8,cex.main=1.5)
axis(1, at=seq(100,1100,200), labels=seq(100,1100,200), cex.axis=1.8)
minor.tick(nx=4, ny=2, tick.ratio=0.5)
par(new=T)
ErrorBarXC2(CrossSec=XSA_sort, PredVar="D100C_Pred", ErrVar="D100C_Err", xLimMin=0, xLimMax=1100, yLimBot=8000, yLimTop=-500, IntBoundLen=0.25, IntBoundNames="WormSect", LinWidBound=1, BoundExtend=0.1)
StateName = c('West Virginia', '', 'Pennsylvania', 'New York')
NameStates(XSA_sort, StateName, -450, -400, 50, 300, "A", 1.5)
text(x=(XSA_sort$Length[which(XSA_sort$STATEFP == 24)][1]+XSA_sort$Length[which(XSA_sort$STATEFP == 42)][1])/2, y=-600, "MD", cex=0.85)
legend(-70,5500,legend=c(expression(paste('Predicted Mean (',hat(mu),")")), expression(paste(hat(mu) %+-% 2, "SE")), "Interpolation Boundary"), lty=c(1,2,2), lwd=c(2,2,1), col=c('black','red','black'), bty="n", cex=1.5, seg.len=1.5, x.intersp = 0.5, y.intersp = 1.1)
PlotReservoirs(CrossSec = XSA_sort, ResData = ResData, Res = 'ThickUncer', Thermal = 'D100C_Pred', xLimMin=0, xLimMax=1100, yLimBot=8000, yLimTop=-500)
PlotReservoirs(CrossSec = XSA_sort, ResData = ResData, Res = 'ThickUncer', Thermal = 'D100C_Pred', Depth = 'ResDepthm', FormThick = 'FmnThickm', NewAxis = 1, Rects=0, yResBot=4000, yResTop=0, xLimMin=0, xLimMax=1100, yLimBot=8000, yLimTop=-500)
dev.off()