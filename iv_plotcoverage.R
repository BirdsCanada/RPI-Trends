## Station coverage plots
  
# directory to post plots
in.dir <- paste("./Data/", max.yr, "/", sep = "")
plot.dir <- paste("./Plots/", max.yr, "/", sep = "")

## Including Plots
#Plot using raw data the station coverage plots


for(t in 1:nrow(anal.param)){  # loop through sites and seasons
  
  #for testing
  #t<-48
  
  site <- as.character(anal.param[t, "SiteCode"])
  seas <- as.character(anal.param[t,"seas"])
  min.yr.filt <- anal.param[t,"min.yr.filt"]
  max.yr.filt <- max.yr
  
  ############################################################################
  # READ IN DATA AND MANIPULATE - SITE AND SEASON SPECIFIC
  
  in.data <- read.csv(paste(in.dir, site, ".", seas, ".RawData.csv", sep = ""))
  in.data <- subset(in.data, !is.na(datetime))
  #in.data$datetime <- as.POSIXct(in.data$datetime)
  
  tmp.data<-NULL
  
  tmp.data<-in.data %>% format_dates()
  tmp.data <- tmp.data %>% select(SiteCode, project_id, YearCollected, MonthCollected, DayCollected, doy, TimeCollected, DurationInHours, species_id, ObservationCount)
  tmp.data<-tmp.data %>% left_join(sp.names, by="species_id")
  
  # if no reason to drop any years at beginning (in anal.param), use min year in dataframe
  min.yr.filt <- ifelse(is.na(min.yr.filt), min(tmp.data$YearCollected), min.yr.filt)
  max.yr.filt <- ifelse(is.na(max.yr.filt), max.yr, max.yr.filt)
  
  ## Filter to min and max year
  
  tmp.data <- filter(tmp.data, YearCollected >= min.yr.filt & YearCollected <= max.yr.filt)
  
  #print("year range:"); print(range(tmp.data$YearCollected))
  
  ## Assign date and season variables
  tmp.data <- tmp.data %>%
    mutate(date = ymd(paste(YearCollected, MonthCollected, DayCollected, sep = "/")),
           doy = yday(date),
           season = if_else(doy < 180, "spring", "fall"))
  
  tmp.data<-tmp.data %>% drop_na(species_code)
  
  if(site == "484"){
    tmp.data <- tmp.data %>%  filter (!(species_code == "BLVU"),
                                      !(species_code == "TUVU")) %>% 
      droplevels() 
  }
  
  obsDays <- unique(subset(tmp.data, select = c("YearCollected", "doy")))
  obsDays <- summaryBy(doy ~ YearCollected, data = obsDays, FUN = c(length, min, max))
  
  
  # calculate number of observation hours/day
  
  obsHours <- unique(subset(tmp.data, select = c("YearCollected", "doy", "date", "DurationInHours")))
  
  tmp <- summaryBy(date ~ YearCollected + doy, data = obsHours, FUN = length)
  tmp <- summaryBy(date.length ~ YearCollected, data = tmp, FUN = c(mean, sd, var))
  
  pdf(paste(plot.dir, site, ".", seas, ".SamplingCoverPlot.pdf", sep=""),
      height = 10, width = 8, paper = "letter")
  par(mfrow = c(3, 1))
  plot(doy.length ~ YearCollected, data = obsDays, 
       ylab = "Number Days Sampled", xlab = "Year",
       col = "black", pch = 20, cex = 1)
  plot(doy.min ~ YearCollected, data = obsDays,
       ylab = "Range of Dates Sampled",
       xlab = "Year", ylim = c(min(obsDays$doy.min), max(obsDays$doy.max)),
       col = c("black"), pch = c(20), cex = 1)
  points(doy.max ~ YearCollected, data = obsDays,
         col = "grey50", pch = 1, cex = 1)
  plot(date.length.mean ~ YearCollected, data = tmp, 
       ylab = "Mean # hours sampled/day", xlab = "Year",
       col = "black", pch = 20, cex = 1)
  dev.off()
  
} # end of i loop


