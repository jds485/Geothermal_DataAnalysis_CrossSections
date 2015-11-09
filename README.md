# Geothermal_DataAnalysis_CrossSections
This repository contains code that may be useful for processing geothermal bottomhole temperature (BHT) data into a useful format for spatial outlier analyses and spatial regressions.
The R script DealingWithDataInDuplicateLocations.R contains functions that sort a well database and determine which wells are in the same spatial location. For those wells, there are options regarding how to deal with the points sharing the same location.
Because many points sharing the exact location are a result of BHT measurements at different depths, the default is to take the deepest measurement.

This repository also contains an R script CrossSectionErrorPlotFunctions.R for making cross section plots based on SpatialPoints data. Cross sections may plot the mean, error bars about the mean, names of counties or states, and any boundaries along the cross section (e.g. interpolation boundaries).
