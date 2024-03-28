#Analysis Loop

#Put all the analytically steps into one loop once you are happy with how everything is running.  

#max.yr <- 2023 #needs changed with each analysis
#ID<- "dethier" #change to your user ID
anal.param <- read.csv("Data/RPI_Analysis_Parameters.csv") 

source("00_setup.R")

anal.param<-anal.param[1:82, ] # to test on different stations

for(t in 1:nrow(anal.param)){
  
  source("i_outputs.R")
  source("ii_analysis.R")
  source("iii_plottrend.R")
  source("iV_plotcoverage.R")
 
  
} #end nrow(anal.param)

