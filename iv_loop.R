#Analysis Loop

#Put all the analytically steps into one loop once you are happy with how everything is running.  

max.yr <- 2022 #needs changes with each analysis
ID<- "dethier"
anal.param <- read.csv("Data/RPI_Analysis_Parameters.csv") 

source("00_setup.R")

for(t in 1:nrow(anal.param)){
  
  source("i_outputs.R")
  source("ii_analysis.R")
  source("iii_plot.R")
  
} #end nrow(anal.param)