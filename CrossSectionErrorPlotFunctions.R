#This file makes cross section plots based on a SpatialPoints cross section.
#The cross section points must be ordered along the cross section for this function to work. 
#Typically, sorting the cross section points by latitude or longitude will properly sort the data for cross sections that are not too complex. 

library(rgdal)

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
