#Setup scripts for PRI Analysis

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
library(doBy)
require(lattice)


# Create folders as necessary
if(!dir.exists("Data")) dir.create("Data")
if(!dir.exists("Output")) dir.create("Output")
if(!dir.exists("Plots")) dir.create("Plots")

## Source scripts

source("./Functions/filterBadDates.r")
source("./Functions/LOESS.R")

# RPI specific source scripts

## Assign parameters that have common values across CMMN sites {#Set3.3}

# set output directory for analysis files; create if not already there
out.dir <- paste("./Output/", max.yr, "/", sep = "")
dir.create(out.dir, showWarnings=FALSE, recursive=TRUE)

# set data directory for analysis files; create if not already there
data.dir <- paste("./Data/", max.yr, "/", sep = "")
dir.create(data.dir, showWarnings=FALSE, recursive=TRUE)

# set data directory for analysis files; create if not already there
plot.dir <- paste("./Plots/", max.yr, "/", sep = "")
dir.create(plot.dir, showWarnings=FALSE, recursive=TRUE)

# get list of species common names, to be used later

sp.names <- meta_species_codes() %>% filter(authority=="RPI")
tax<-meta_species_taxonomy()
sp.names<-left_join(sp.names, tax, by=c("species_id"))
sp.names<-sp.names %>% select("species_code", "species_id", "sort_order", "english_name", "scientific_name", "french_name")
sp.names<-sp.names %>% distinct(species_code, .keep_all= TRUE)

project <- 1013 # same for all
results.code <- "RPI" # same for all

## Import site-specific analysis parameters 
#anal.param <- read.csv("Data/RPI_Analysis_Parameters.csv") #This will need checked and updated before each analysis

## Read in Superfile

#Load bad dates into the Data folder

bad_dates<-try(read.csv("Data/bad_dates.csv"))

if(class(bad_dates) == 'try-error'){

bad_dates<-nc_query_table("bmde_filter_bad_dates")
write.csv(bad_dates, "Data/bad_dates.csv", row.names = FALSE)

}#end try catch

##Load generation length table

gen<-nc_query_table(username=ID, "SocbSpecies")
gen<-gen %>% select(speciesID, generation)
write.csv(gen, "Data/generation.csv")

#Set old function values. Some may no longer be needed. 
write.manip.data= FALSE # write manipulated data to file?
filter.seas = TRUE		# filter by seasonal windows?
filter.years = TRUE 		# filter data by min and max years
seas.pctile1 = 2.5		# if filter.seas = TRUE, lower percentile
seas.pctile2 = 97.5		# if filter.seas = TRUE, upper percentile
station.pctile1 = 2.5
station.pctile2 = 97.5
write.windows = TRUE		# if filter.seas = TRUE, write seasonal windows to file?
center.var = TRUE		# center date variables prior to analysis?
results.code = "RPI"






