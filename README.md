# Geothermal_DataAnalysis_CrossSections
This repository contains code that processes geothermal bottomhole temperature (BHT) data into a useful format for spatial outlier analyses and spatial regressions. The repository also has R scripts with plotting functions for geotherm temperature at depth plots, and cross sections.

The R script DealingWithDataInDuplicateLocations.R contains functions that sort a well database and determine which wells are in the same spatial location. For those wells, there are options regarding how to deal with the points sharing the same location.
Because many points sharing the exact location are a result of BHT measurements at different depths, the default is to take the deepest measurement.

The R script CrossSectionErrorPlotFunctions.R has functions for making cross section plots based on SpatialPoints data. Cross sections may plot the mean, error bars about the mean, names of counties or states, and any boundaries along the cross section (e.g. interpolation boundaries). There is an option to plot reservoirs (or other features in your database) at depth along the cross sections.

The R script GeothermPlot.R provides a variety of options for plotting geotherm temperatures at depth.
