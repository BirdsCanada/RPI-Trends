#Create output tables. This is done for each station separately. 

## Set up output tables
#This is in a separate file because if loop through species fails, don't want to re-write these files and delete everything that has already been done. Can then just re-start the loop at the next species, and keep going.

## Create text file for indices

indices.csv <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 24, byrow = FALSE,
                                    dimnames = NULL))
names(indices.csv) <- c("upload_id",	"results_code",	"version",	"area_code",	"species_code",	"species_name",	"species_sci_name",	"year",	"season",	"period",	"species_id",	"index",	"stderr",	"stdev",	"upper_ci",	"lower_ci",	"trend_id",	"smooth_index",	"smooth_upper_ci",	"smooth_lower_ci",	"upload_dt",	"error", "meanObs", "analysis_code") 


write.table(indices.csv, file = paste(out.dir, 
                                      site, "_AnnualIndices",".csv", sep = ""), 
            row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")


## Create text file for trends (appending year periods into one file)
trends.csv <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 61, 
                                   byrow = FALSE, dimnames = NULL))

names(trends.csv) <- c("results_code",	"version",	"area_code",	"species_code",	"species_name",	"species_sci_name",	"species_id",	"season",	"period",	"years",	"year_start",	"year_end",	"trnd",	"index_type",	"upper_ci",	"lower_ci",	"stderr",	"model_type",	"model_fit",	"percent_change",	"percent_change_low",	"percent_change_high",	"prob_decrease_0",	"prob_decrease_25",	"prob_decrease_30",	"prob_decrease_50",	"prob_increase_0",	"prob_increase_33",	"prob_increase_100",	"confidence",	"precision_num",	"precision_cat",	"coverage_num",	"coverage_cat",	"goal",	"goal_lower",	"sample_size",	"sample_total",	"subtitle",	"pval",	"pval_str",	"post_prob",	"trnd_order",	"dq",	"slope_trend",	"prob_LD",	"prob_MD",	"prob_LC",	"prob_MI",	"prob_LI",	"quantile_050",	"quantile_165",	"quantile_835",	"quantile_950",	"trend_id",	"upload_dt",	"error", "stdev", "analysis_code")

write.table(trends.csv, file = paste(out.dir, 
                                     site, "_Trends", ".csv", sep = ""), 
            row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")

