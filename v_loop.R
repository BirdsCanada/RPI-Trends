#Analysis Loop

#Put all the analytically steps into one loop once you are happy with how everything is running.  


#"HawkCount-432" = 63 "HawkCount-563" =64 "HawkCount-494" =65
#anal.param<-anal.param[63:65, ] # to test on different stations

anal.param <- read.csv("Data/RPI_Analysis_Parameters.csv") 

for(t in 1:nrow(anal.param)){
  
  
  rm(list = ls())#clear the environment
  
  max.yr <- 2023 #needs changed with each analysis
  ID<- "dethier" #change to your user ID
  source("00_setup.R")
  

  source("i_outputs.R")
  source("ii_analysis.R")
  source("iii_plottrend.R")
  source("iV_plotcoverage.R")
 
  
} #end nrow(anal.param)

