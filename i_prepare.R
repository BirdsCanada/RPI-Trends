source("00_setup.R")

#sets parameters for your site
collection <- as.character(anal.param[t, "collection"])
station <- as.character(anal.param[t, "station"])
site <- as.character(anal.param[t, "site"])
site.specific <- anal.param[t, "site.specific"]
use.trfl <- anal.param[t, "use.trfl"]
responseM<-anal.param[t , "obs.var.M"]
responseO<-anal.param[t , "obs.var.O"]

## Import Data

#Import data for the specified station (all species, sites, seasons) using the naturecounts R package. 

#first look in file. If it does not exist, download from database.
in.data <-try(read.csv(paste(data.dir, site, "_Raw_Data.csv", sep="")))

if(class(in.data) == 'try-error'){

in.data <- nc_data_dl(collections = collection, fields_set = "extended", username = ID, info="Trend analysis")
write.csv(in.data, paste(data.dir, site, "_Raw_Data.csv", sep=""), row.names = FALSE)

} #end of try catch, which looks for proceed data on the data.dir first

in.data <- in.data %>% select(SurveyAreaIdentifier, project_id, ObservationCount, ObservationCount2, ObservationCount3, ObservationCount4, SiteCode, YearCollected, MonthCollected, DayCollected, species_id)

# We only want sites LPBO1, LPBO2, and LPBO3. 
if(station == "LPBO") {
  in.data <- in.data %>%
    distinct() %>%
    filter(SurveyAreaIdentifier != "LPBO-03") %>% 
    droplevels()
}

# This is a fix while Catherine is away - there are two names for this site VLMMS 2001-2015, and VLBO 2016-2017 which I believe should be same site, so rename
if((station == "VLBO")) {
  in.data <- in.data %>%
    mutate(SurveyAreaIdentifier = "VLBO") %>%
    droplevels()
}

# McGill is in the database as both MGBO and MBO
if((station == "MGBO")) {
  in.data <- in.data %>%
    mutate(SurveyAreaIdentifier = "MGBO") %>%
    droplevels()
}


#If RPBO only want site RPBO (not PEBA or RBPO2)
if((station == "RPBO")) {
  in.data <- in.data %>%
    filter(SurveyAreaIdentifier == "RPBO") %>%
    droplevels()
}

## Generate site list - this will have a length of 1 for most sites; at LPBO will include all three sites
site.list <- as.character(unique(in.data$SurveyAreaIdentifier))

# DROP BAD DATES:drop days that should be excluded from a site for one reason or another.  We do this again later to drop species-specific bad dates.
# Note: I made some manual tweaks to the function. Need to check that it works for all sites. 
in.data <- bscdata.filterBadDates(in.data, sitecode = site.list)

## Assign date and season variables
in.data <- in.data %>%
  mutate(date = ymd(paste(YearCollected, MonthCollected, DayCollected, sep = "/")),
         doy = yday(date),
         season = if_else(doy < 180, "Spring", "Fall"))

#ACBO fall coverage does not meet minimum requirements. Remove all fall data. 

if((station == "ACBO")) {
  in.data <- in.data %>%
    filter(season == "Spring") %>%
    droplevels()
}

# note that this assumes that if stations that are no longer collecting are to be analyzed, that the last year is specified in the anal.param table:
min.yr.filt <- ifelse(is.na(anal.param[t,"min.year"]),
                      min(in.data$YearCollected), anal.param[t,"min.year"])

max.yr.filt <- ifelse(is.na(anal.param[t,"max.year"]),
                      max.year, anal.param[t,"max.year"])

# Subset data to specified year range
in.data <- in.data %>%
  filter(YearCollected >= min.yr.filt & YearCollected <= max.yr.filt)

#print("year range:"); print(range(in.data$YearCollected))

# get the minimum number of years a species must be detected to be included. Could be MUCH more conservative... currently using 1/2 of years surveyed, but those species might also get kicked out by abundance filters below.
min.yrs.detect <- trunc(length(unique(in.data$YearCollected))/2) 

#total number of years each doy surveyed at each site (include 0-obs counts)
df.totYears<-NULL
df.totYears <- in.data %>%
  select(SurveyAreaIdentifier, YearCollected, doy) %>%
  distinct() %>%
  group_by(SurveyAreaIdentifier, doy) %>%
  summarize(totYears = n()) %>%
  as.data.frame()%>% 
  mutate(prop_year= totYears/((max.yr.filt-min.yr.filt)+1)) %>% 
  mutate(season = if_else(doy < 180, "Spring", "Fall"))

df.totYears$season_f<-factor(df.totYears$season, levels=c("Spring", "Fall"))
df.totYears<- df.totYears[order(df.totYears$doy),]

#determine station regular operating window
doy<-df.totYears %>% group_by(season_f) %>% filter(prop_year>0.68) %>% summarise(min=min(doy), max=max(doy))

min.spring<-as.numeric(doy %>% dplyr::filter(season_f=="Spring") %>% select(min))
max.spring<-as.numeric(doy %>% dplyr::filter(season_f=="Spring") %>% select(max))
min.fall<-as.numeric(doy %>% dplyr::filter(season_f=="Fall") %>% select(min))
max.fall<-as.numeric(doy %>% dplyr::filter(season_f=="Fall") %>% select(max))

#filter in.data by operating window
fall<- try(in.data %>% filter(season=="Fall", doy>=min.fall, doy<=max.fall), silent=TRUE)
spring<-try(in.data %>% filter(season=="Spring", doy>=min.spring, doy<=max.spring), silent = TRUE)

#combine fall and spring data
in.data<-rbind(spring, fall)

#create events data for zero filling
event.data <- in.data %>%
  filter(ObservationCount > 0) %>%
  group_by(SurveyAreaIdentifier, YearCollected, MonthCollected, DayCollected, date, doy, season) %>%
  mutate(nspecies = n()) %>%
  filter(nspecies > 1) %>% # assuming at least one individual detected each day. This could be modified, for example, to include only dates when at least 10 species were detected.
  select(SurveyAreaIdentifier, YearCollected, MonthCollected, DayCollected, date, doy, season) %>% 
  distinct() %>%
  ungroup() %>%
  as.data.frame()

## Assign updated species codes base on species_id since some have changed. 

sp.codes<-meta_species_codes()
sp.codes<-sp.codes %>% filter(authority=="CMMN" & rank==1) %>% select(species_id, species_code)
in.data<-left_join(in.data, sp.codes, by="species_id")
in.data<-in.data %>% dplyr::rename(SpeciesCode=species_code)

## get total count by species, date, station
in.data$ObservationCount<-as.numeric(in.data$ObservationCount)
in.data$ObservationCount2<-as.numeric(in.data$ObservationCount2)
in.data$ObservationCount3<-as.numeric(in.data$ObservationCount3)
in.data$ObservationCount4<-as.numeric(in.data$ObservationCount4)

in.data <- in.data %>%
  group_by(SurveyAreaIdentifier, YearCollected, MonthCollected, DayCollected, SpeciesCode, species_id, date, doy, season) %>%
  dplyr::summarize(ObservationCount = sum(ObservationCount, na.rm = TRUE),
                   ObservationCount2 = sum(ObservationCount2, na.rm = TRUE),
                   ObservationCount3 = sum(ObservationCount3, na.rm = TRUE),
                   ObservationCount4 = sum(ObservationCount4, na.rm = TRUE)) %>%
as.data.frame()

## create new response variable for census + band
in.data$ObservationCount7=in.data$ObservationCount3+in.data$ObservationCount4

df.superfile <-df.superfile %>%  mutate(SurveyAreaIdentifier = SiteCode,
         SpeciesCode = species_code,
         season = period) %>%
  select(-SiteCode, -species_code, -period) %>%
  filter(SurveyAreaIdentifier %in% site.list) %>%
  droplevels()

sp.analyze <- df.superfile %>%
  select(SurveyAreaIdentifier, SpeciesCode, season, start_date, end_date, analysis_code, lpbo_combine) %>%
  distinct() %>%
  droplevels()

in.data <- left_join(sp.analyze, in.data, by = c("SurveyAreaIdentifier", "SpeciesCode", "season"), multiple = "all") %>%
  droplevels()

## Run 'Assign UNEM.R' For LPBO data only!!!  
#This section modifies empidonax values by assigning unknown empidonax to  LEFL, YBFL, TRFL by the proportional representation of each species on each doy (across years) in ET data. Might use banding data (ObservationCount4), but not available digitally pre-1984. NOTE that by running this, banding data and census data are erased from in.data for empid species. Do not include ACFL because there as so few it would make little impact.

if(site == "LPBO") {
  
## 1. summarize the total of known empidonax species by doy/site, across years
  
  df.empidSum <- in.data %>%
    filter(SpeciesCode %in% c("LEFL", "YBFL", "TRFL")) %>%
    group_by(SurveyAreaIdentifier, doy) %>%
    summarize(KnownEmpidET = sum(ObservationCount, na.rm = TRUE),
              KnownEmpidCensus = sum(ObservationCount4, na.rm = TRUE))
  
## 2. summarize total of each empid species by doy/site, across years
  
  df.empidSum2 <- in.data %>%
    filter(SpeciesCode %in% c("LEFL", "YBFL", "TRFL")) %>%
    group_by(SurveyAreaIdentifier, doy, SpeciesCode) %>%
    summarize(totET = sum(ObservationCount, na.rm = TRUE),
              totCensus = sum(ObservationCount4, na.rm = TRUE))
  
##3. merge to get proportion of total empid for each species by site, doy
  
  df.empidSum <- left_join(df.empidSum, df.empidSum2, by = c("SurveyAreaIdentifier", "doy")) %>%
    mutate(p.totET = totET/KnownEmpidET,
           p.totCensus = totCensus/KnownEmpidCensus) %>%
    select(SurveyAreaIdentifier, doy, SpeciesCode, p.totET, p.totCensus)
  
  #ggplot(df.empidSum, aes(y = p.tot, x = doy, colour = SpeciesCode)) + geom_point()
  
##4. merge this with total number of unknown empidonax
  
  df.unem <- in.data %>%
    filter(SpeciesCode %in% c("UNEM")) %>%
    group_by(SurveyAreaIdentifier, YearCollected, MonthCollected, DayCollected, date, doy) %>%
    summarize(totUNEMET = sum(ObservationCount, na.rm = TRUE),
              totUNEMCensus = sum(ObservationCount4, na.rm = TRUE)) %>%
    left_join(df.empidSum, by = c("SurveyAreaIdentifier","doy")) %>%
    mutate(addObsET = round(totUNEMET*p.totET, digits = 0),
           addObsCensus = round(totUNEMCensus*p.totCensus, digits = 0)) %>%
    as.data.frame() %>%
    select(-totUNEMET, -totUNEMCensus,-p.totET, -p.totCensus)
  
##5. merge this with the raw data, and sum addObs and ObservationCount
  
  in.data <- left_join(in.data, df.unem, by = c("SurveyAreaIdentifier", "YearCollected", "MonthCollected", "DayCollected", "date", "doy", "SpeciesCode")) %>%
    mutate(addObsET = if_else(is.na(addObsET), 0, addObsET),
           addObsCensus = if_else(is.na(addObsCensus), 0, addObsCensus),
           ObservationCount = ObservationCount + addObsET,
           ObservationCount4 = ObservationCount4 + addObsCensus) %>%
    select(-addObsET, -addObsCensus) %>%
    filter(SpeciesCode != "UNEM")
  
} #end if LPBO

#Remove NA years
in.data<-in.data %>% drop_na(YearCollected) %>% drop_na(species_id)

#write clean data to file
write.csv(in.data, paste(data.dir, site, "_Clean_Data.csv", sep=""), row.names = FALSE)
write.csv(event.data, paste(data.dir, site, "_Event.csv", sep=""), row.names = FALSE)


