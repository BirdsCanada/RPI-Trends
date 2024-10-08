source("00_setup.R")

#sets parameters for your site
collection <- "HawkCount"
site <- as.character(anal.param[t, "SiteCode"])
seas <- as.character(anal.param[t,"seas"])
min.yr.filt <- anal.param[t,"min.yr.filt"]
max.yr.filt <- anal.param[t,"max.yr.filt"]
data.type <- as.character(anal.param[t, "data.type"])


#Create output tables. This is done for each station separately. 

## Set up output tables
#This is in a separate file because if loop through species fails, don't want to re-write these files and delete everything that has already been done. Can then just re-start the loop at the next species, and keep going.

site <- as.character(anal.param[t, "SiteCode"])

## Create text file for indices

indices.csv <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 15, byrow = FALSE,
                                    dimnames = NULL))
names(indices.csv) <- c("results_code", "version", "area_code", "season", "period", "species_code", "species_id", "year", "index", "stderr", "stdev", "upper_ci", "lower_ci", "LOESS_index", "trend_index")


write.table(indices.csv, file = paste(out.dir, 
                                      site, "_", seas, "_AnnualIndices",".csv", sep = ""), 
            row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")


## Create text file for trends (appending year periods into one file)
trends.csv <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 39, 
                                   byrow = FALSE, dimnames = NULL))
names(trends.csv) <- c("results_code",	"version",	"area_code",	"season",	"period", "species_code",	"species_id",	"years", "year_start",	"year_end",	"trnd",	"lower_ci", "upper_ci", "index_type", "stderr",	"model_type",	"model_fit",	"percent_change",	"percent_change_low",	"percent_change_high",	"prob_decrease_0",	"prob_decrease_25",	"prob_decrease_30",	"prob_decrease_50",	"prob_increase_0",	"prob_increase_33",	"prob_increase_100", "suitability", "precision_num",	"precision_cat",	"coverage_num",	"coverage_cat",	"sample_size", "sample_size_units", "prob_LD", "prob_MD", "prob_LC", "prob_MI", "prob_LI")

#Endpoint Trends
write.table(trends.csv, file = paste(out.dir, 
                                     site, "_", seas, "_TrendsEndpoint", ".csv", sep = ""), 
            row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")

#Slope Trends
write.table(trends.csv, file = paste(out.dir, 
                                     site, "_", seas, "_TrendsSlope", ".csv", sep = ""), 
            row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")



#In 2018 added an error output table to record when INLA crashes for a specific species. 

error <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 3, byrow = FALSE, dimnames = NULL))
names(error) <- c("Site", "Season", "SpeciesCode")

#Error file for recording which species were not analysed.   
write.table(error, file = paste(out.dir,  site, "_", seas, "_ErrorFile", ".csv", sep = ""), row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")


#seasonal migration window
window.csv<-as.data.frame(matrix(data = NA, nrow = 1, ncol = 4, 
                                   byrow = FALSE, dimnames = NULL))

names(window.csv) <- c("st.doy","end.doy","SpeciesCode", "Site")

write.table(window.csv, file = paste(out.dir, site, "_", seas, "_SeasonalWindows.csv", sep = ""), row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")
