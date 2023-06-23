#Setup scripts for CMMN Analysis
#Replaced the 03-SetWorkingEnvironment.Rmd
#May not need sql outputs. If that is the case, then can be delected. 
options(dplyr.summarise.inform = FALSE)

#Load Packages

#First time download of INLA needs to be done from the website
#install.packages("INLA", repos=c(getOption("repos"), #INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

#First time download of naturecounts needs to be done from GitHub using the remotes package. 
#install.packages("remotes")
#remotes::install_github("BirdStudiesCanada/naturecounts")

require(naturecounts)
require(INLA)
require(tidyverse)
require(lubridate)
require(reshape)
require(ggpubr)
library(mgcv)

# Create folders as necessary
if(!dir.exists("Data")) dir.create("Data")
if(!dir.exists("Output")) dir.create("Output")

## Source Scripts

source("./Functions/filterBadDates.r")
source("./Functions/sqlSave.r")
source("./Functions/sqlString.r")

## Assign parameters that have common values across CMMN sites {#Set3.3}

# set output directory for analysis files; create if not already there
out.dir <- paste("./Output/", max.year, "/", sep = "")
dir.create(out.dir, showWarnings=FALSE, recursive=TRUE)

# set data directory for analysis files; create if not already there
data.dir <- paste("./Data/", max.year, "/", sep = "")
dir.create(data.dir, showWarnings=FALSE, recursive=TRUE)

# set data directory for analysis files; create if not already there
plot.dir <- paste("./Plots/", max.year, "/", sep = "")
dir.create(data.dir, showWarnings=FALSE, recursive=TRUE)

# get list of species common names, to be used later

sp.names <- meta_species_codes() %>% filter(authority=="CMMN")
tax<-meta_species_taxonomy()
sp.names<-left_join(sp.names, tax, by=c("species_id"))
sp.names<-sp.names %>% select("species_code", "species_id", "sort_order", "english_name", "scientific_name", "french_name")
sp.names<-sp.names %>% distinct(species_code, .keep_all= TRUE)

project <- 1005 # same for all
results.code <- "CMMN" # same for all

# create sql files of indices and trends? If TRUE, trends will be written to sql files and those posted online. 
write.indices.sql = TRUE
write.trends.sql = TRUE
write.migwindowsumm.sql = TRUE  # should always be true - this can be posted for all species - shows when species are observed at a site, and isn't 'sensitive' or open to interpretation as are the trends, for the different classifications of species.

# limits for seasonal windows
season.pctile1 = 5
season.pctile2 = 95

# limits for station coverage windows
station.pctile1 = 5
station.pctile2 = 95

# set year increment for trends: every 10-year period (10-, 20-, 30-, etc)
yr.incr <- 10

## Import site-specific analysis parameters 
anal.param <- read.csv("Data/CMMN_Analysis_ParameterValues.csv") #This will need checked and updated before each analysis

## Read in Superfile

#In 2017, Ricky developed a 'superfile' which classifies species as migrant or other, determines whether they should be included in trend analysis, and gives the migration windows for each species at each station.  Only species classified as migrant ('M') should have trends displayed on the web, however, trends for all species are calculated and output for internal/station use.
df.superfile <- read.csv("Data/CMMNSuperfile.csv") #This will need checked and updated periodically. New sites will need to have superfile information generated. 

#Load bad dates into the Data folder

bad_dates<-try(read.csv("Data/bad_dates.csv"))

if(class(bad_dates) == 'try-error'){

bad_dates<-nc_query_table("bmde_filter_bad_dates")
write.csv(bad_dates, "Data/bad_dates.csv", row.names = FALSE)

}#end try catch

#In 2018 added an error output table to record when INLA crashes for a specific species. 

error <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 4, byrow = FALSE, dimnames = NULL))
names(error) <- c("Site", "Season", "SpeciesCode", "time.period")

#only need to create the table once per analysis. Error file for recording which species were not analysed.   
write.table(error, file = paste(out.dir,  "ErrorFile", ".csv", sep = ""), row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")

#Orthogonal legendre polynomial transformation to centralize day and day^2 covariates
# Some examples here: https://online.stat.psu.edu/stat502_fa21/lesson/10/10.2   

poly.legendre <- function(x, degree, minx, maxx){
  
  if (min(x) < minx) stop("min(x) < minx")
  if (max(x) > maxx) stop("max(x) > maxx")
  
  if (degree > 4) stop("Degree must be < 5")
  
  lg <- length(x)
  
  z <- data.frame(x)
  
  z$xP1 <- 2 * (x - minx) / (maxx - minx) - 1
  z$xP2 <- (3 * z$xP1 * z$xP1 - 1) / 2
  if(degree > 2) z$xP3 <- (5 * z$xP2 * z$xP1 - 2 * z$xP1) / 3
  if(degree > 3) z$xP4 <- (7 * z$xP3 * z$xP1 - 3 * z$xP2) / 4
  
  z  <- z[, -1]
}


##Load generation length table

gen<-nc_query_table(username=ID, "vwResultsSocbSpecies")
gen<-gen %>% select(speciesID, generation)
write.csv(gen, "Data/generation.csv")

##LOESS function

loess_func <- function(i,y){
  tmp <- loess(i~y, 
               span=0.55, na.action = na.exclude)
  preds <- predict(tmp)
  return(preds)
}

##BBS slope trend function. Currently create my own and am not using this. Could be deleted.   
bsl = function(i){
  n = length(wy)
  sy = sum(i)
  sx = sum(wy)
  ssx = sum(wy^2)
  sxy = sum(i*wy)
  b = (n*sxy - sx*sy)/(n*ssx - sx^2)
  return(b)
}

