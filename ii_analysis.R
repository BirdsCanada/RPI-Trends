source("00_setup.R")

#sets parameters for your site
collection <- "HawkCount"
site <- as.character(anal.param[t, "SiteCode"])
seas <- as.character(anal.param[t,"seas"])
min.yr.filt <- anal.param[t,"min.yr.filt"]
max.yr.filt <- anal.param[t,"max.yr.filt"]
data.type <- as.character(anal.param[t, "data.type"])

## Import Data

#Import data for the specified station (all species, sites, seasons) using the naturecounts R package. 

#first look in file. If it does not exist, download from database.
data <-try(read.csv(paste(data.dir, site, ".", seas, ".RawData.csv", sep = "")), silent = T)

if(class(data) == 'try-error'){

data <- nc_data_dl(collections = site, fields_set = "extended", username = ID, info="Trend analysis")
write.csv(data, paste(data.dir, site, ".", seas, ".RawData.csv", sep = ""), row.names = FALSE)

} #end of try catch, which looks for proceed data on the data.dir first

tmp.data<-

tmp.data<-data %>% format_dates()
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


tmp.data$DurationInHours<-as.numeric(tmp.data$DurationInHours)

#create events data for zero filling
event.data <- tmp.data %>%
  filter(ObservationCount > 0) %>%
  group_by(SiteCode, YearCollected, MonthCollected, DayCollected, doy, TimeCollected) %>%
  slice_max(DurationInHours) %>% 
  select(SiteCode, YearCollected, MonthCollected, DayCollected, doy, TimeCollected, DurationInHours) %>% 
  distinct() %>%
  ungroup() %>%
  group_by(SiteCode, YearCollected, MonthCollected, DayCollected, doy) %>% summarize(DurationInHours=sum(DurationInHours)) %>% 
  ungroup() %>%
  as.data.frame()

#Turn on once ready to run the analysis
if(max(tmp.data$YearCollected) == max.yr) { #continue only if the max year in database is what it should be

# drop unknown species from analysis
# note that you must wait until here to do this, because some unknowns 
# may mark observation slots that would otherwise be deleted
# use droplevels so unknowns are not listed in levels of the factor "species_code"
tmp.data <- droplevels(subset(tmp.data, !(species_code %in% c("URAP", "UBUT", "UACC", "UVUL", "UEAG", "UFAL"))))

# remove negative counts of birds flying back the other way
tmp.data <- subset(tmp.data, ObservationCount > -1)

#remove duration in hours since this will be added back in during the zero-fill
tmp.data<-tmp.data %>% select(-DurationInHours)

# set up an event dataframe, and filter this to keep only inner
# 95%ile of days - drops those days of the YearCollected that weren't sampled
# consistently over time.
# raw data will be filtered by this later
event.dates <- with(event.data,
                    round(quantile(doy, probs = c(station.pctile1, station.pctile2)/100, 
                                   na.rm = TRUE), digits = 0))

## and filter data by the station coverage
tmp.data <- subset(tmp.data, doy >= event.dates[[1]] & doy <= event.dates[[2]]) 

#create species list for analysis
sp.list <- unique(tmp.data$species_code) 

for(j in 1:length(sp.list)) { 
  
    sp.dat<-NULL  
    sp.dat <- subset(tmp.data, species_code == sp.list[j])
    
    sp.id <- unique(sp.dat$species_id)
    species <- sp.list[j]
    sp.nam<-filter(sp.names, species_id == sp.id)
    
    sp.dat<-left_join(event.data, sp.dat, by=c("SiteCode", "YearCollected", "MonthCollected", "DayCollected", "doy"), multiple="all") %>% 
      mutate(ObservationCount = replace(ObservationCount, is.na(ObservationCount), 0))

   sp.dat$species_code<-species
   sp.dat$ObservationCount<-as.numeric(sp.dat$ObservationCount)
   sp.dat$DurationInHours<-as.numeric(sp.dat$DurationInHours)
   sp.dat$YearCollected<-as.numeric(sp.dat$YearCollected)
    
    #amalgamate hourly counts into daily totals
  
  date.tot <- recast(sp.dat, species_code + YearCollected + doy + DurationInHours ~ variable, 
                     fun.aggregate = sum, na.rm = TRUE,
                     id.var = c("species_code", "YearCollected", "doy", "DurationInHours"), 
                     measure.var = c("ObservationCount"))
  date.tot <- data.frame(date.tot)
  date.tot <- date.tot %>% drop_na(DurationInHours)
  
  
  # determine the 95%ile of days of year with observations for
  # each species, to define migration windows
  # taking inner 95% of days with count >= 1
  
  windows <- with(date.tot, round(quantile(doy, probs = c(seas.pctile1, seas.pctile2)/100, na.rm = T), digits = 0))
                    
  
  # filter daily data frame by seasonal windows
  
  date.tot <- subset(date.tot, doy >= windows[1] & doy <= windows[2])
   
  ## function to output seasonal migration window table
  windows <- as.data.frame(t(windows))
  windows$species_code <- species
  windows$site <- site

if(nrow(date.tot)>0){   #only continue if data remains after migration window filter 
  
  #print window to file
  
 # write.table(windows, file = paste(out.dir, site, "_", seas, "_SeasonalWindows.csv", sep = ""), row.names = FALSE, append = TRUE, quote = FALSE, sep = ",", col.names = FALSE)
    
  # drop species not detect in 1/2 of all years
  
  min.yrs.detect = (max(date.tot$YearCollected) - min(date.tot$YearCollected))/ 2  
  min.yrs <- date.tot %>% group_by(species_code) %>% summarise(nyrs=n_distinct(YearCollected)) %>% filter(nyrs>=min.yrs.detect)
  
  # and subset dataframe by species list
  
if(nrow(min.yrs)>0){  #only continue if min.yrs is great then 0 
  
  # drop species that are not detected with a mean of 10 individuals per year.
  
  tmp <- date.tot %>% group_by(species_code, YearCollected) %>%
    summarise( 
      mean.count = mean(ObservationCount), 
      max.count = max(ObservationCount),
      tot.count = sum(ObservationCount)) %>%
    summarise( 
      mean.mncount = mean(mean.count), 
      max.mxcount = mean(max.count),
      mean.totcount = mean(tot.count))
  
  #sp.list <- subset(tmp$species_code, tmp$mean.totcount >= 10)
  tmp <- tmp %>% filter(mean.totcount >= 10) 

if(nrow(tmp)>0){  #only continue if tmp is great then 10
  
    # CALCULATE ANNUAL INDICES FOR ALL YAERS
  
  # turn species code into factor
  date.tot <- date.tot %>%
    mutate(species_code = as.factor(species_code)) %>%
    droplevels()
  
  #   ####################################################################
  ####################################################################
  
  #standardize variables
  #standardize year to current year  
  
  date.tot$YearCollected<-as.numeric(date.tot$YearCollected)
  date.tot <- date.tot %>% mutate(cyear = scale(YearCollected), 
                              fyear = as.factor(YearCollected),
                              doyfac = as.factor(doy)) %>% 
  arrange(YearCollected)
  
  #index for year ordered. 
  #date.tot$oyear <- as.integer(factor(date.tot$YearCollected)) #index for the random site effect
  
  ######### MODEL FORMULAS
  
  #Note: Tried adding oyear as a replicate in the f(doy, model="ar1", replicate=oyear) but this caused issues with inla. Remove and the model runs smooth. 
  
  # MODEL FORMULA with GAM for doy and year effects
  
  #determine the number of knots for year
  #add one knot for every 4 years of the time series follow Smith and Edwards
  nyears <- length(unique(date.tot$fyear))
  kyears<- ceiling(nyears/4) #round up to nearest whole number
  
  #ndays <- length(unique(date.tot$doyfac))
  #kdays<- ceiling(ndays/8)
  
  kdays<-3 #polynomial like previous
  
  #smooth years
  smy<-smoothCon(s(YearCollected, bs="cr", k=kyears), data=date.tot)[[1]]
  Xsmy<-smy$X #basis function
  colnames(Xsmy)<-paste("BasisYR", 1:ncol(Xsmy), sep="")#need unique names
  
  lcs.y<-inla.make.lincombs(data.frame(Xsmy))
  names(lcs.y)<-paste(names(lcs.y), "YR", sep="")#need unique names
  
  date.tot<-cbind(date.tot, Xsmy)
  
  #smooth day of year
  sdy<-smoothCon(s(doy, bs="cr", k=kdays), data=date.tot)[[1]]
  Xsdy<-sdy$X #basis function
  colnames(Xsdy)<-paste("BasisDay", 1:ncol(Xsdy), sep="")#need unique names
  
  lcs.d<-inla.make.lincombs(data.frame(Xsdy))
  names(lcs.d)<-paste(names(lcs.d), "DAY", sep="")#need unique names
  
  date.tot<-cbind(date.tot, Xsdy)
  lcs<-c(lcs.y, lcs.d)
  
  #Use penalised complex prior for random year effect
  hyper.iid<-list(prec=list(prior="pc.prec", param=c(2,0.05)))
  inla.setOption(scale.model.default=TRUE)
  
    index.gam<- ObservationCount ~ -1 + Xsmy + Xsdy + DurationInHours + f(fyear, model="iid", hyper=hyper.iid) 
    index.gamS<- ObservationCount ~ - 1 + Xsmy + Xsdy + DurationInHours

   ###### RUN ANALYSIS
  
  #Run nbinomial, poisson, zip model. Select the model with the lowest DIC.                
  
#  index.nb <-index.pois <-index.zip<- NULL
  
#  index.nb <- try(inla(index.gam, family = "nbinomial", data = date.tot, #E = DurationInHours, 
#                    control.predictor = list(compute = TRUE), control.compute = list(dic=TRUE, config = TRUE), lincomb=lcs, verbose =TRUE), silent = T)
  
#  index.pois <- try(inla(index.gam, family = "poisson", data = date.tot, #E = DurationInHours, 
#                    control.predictor = list(compute = TRUE), control.compute = list(dic=TRUE, config = TRUE), lincomb=lcs, verbose =TRUE), silent = T)
  
#  index.zip <- try(inla(index.gam, family = "zeroinflatednbinomial1", data = date.tot, #E = DurationInHours, 
#                         control.predictor = list(compute = TRUE), control.compute = list(dic=TRUE, config = TRUE), lincomb=lcs, verbose =TRUE), silent = T)
  
  
#  model<-c("nbinomial", "poisson", "zeroinflatednbinomial1")
#  index.nb.dic<-ifelse(class(index.nb) == 'try-error', NA, index.nb[["dic"]][["dic"]])
#  index.pois.dic<-ifelse(class(index.pois) == 'try-error', NA, index.pois[["dic"]][["dic"]])
#  index.zip.dic<-ifelse(class(index.zip) == 'try-error', NA, index.zip[["dic"]][["dic"]])
#  dic<-c(index.nb.dic, index.pois.dic, index.zip.dic)
#  index<-c("index.nb", "index.pois", "index.zip")
#  t.model<-data.frame(model, index, dic)
  
#  t.model<-t.model %>% slice_min(dic, na_rm = TRUE)
#  family<-t.model[,1]
#  index<-t.model[,2]

  family<-"nbinomial"
  index<- "index.nb" 
 
  #rerun the top model and save output
  top.model<-try(inla(index.gam, family = family, data = date.tot, #E = DurationInHours, 
                      control.predictor = list(compute = TRUE), control.compute = list(dic=TRUE, config = TRUE), lincomb=lcs, verbose =TRUE), silent = T)
  
  top.modelS<-try(inla(index.gamS, family = family, data = date.tot, #E = DurationInHours, 
                       control.predictor = list(compute = TRUE), control.compute = list(dic=TRUE, config = TRUE), lincomb=lcs, verbose =TRUE), silent = T)
  


  #What to do if there is an error in the top.model		
  if(class(top.model) == 'try-error'| is.null(top.model)){
    
    error$Site<-site
    error$Season<-seas
    error$SpeciesCode<-species
    
    #Print final error table to file
    
    write.table(error, file = paste(out.dir, site, "_", seas, "_ErrorFile.csv", sep = ""), row.names = FALSE, append = TRUE, 
                quote = FALSE, sep = ",", col.names = FALSE)
  }	#end try-error statement
  
  
  #What to do if there is no error in the try error above for either INLA function  	
  
  #first define the is.not.null function
  is.not.null <- function(x) !is.null(x)
  
  if(class(top.model) != 'try-error'& is.not.null(top.model)){
 #   if(class(top.modelS) != 'try-error'& is.not.null(top.modelS)){
      
# ANNUAL INDICES: GENERATED FOR FULL TIME PERIOD ONLY
    
      # Summary of the GAM smooth on year
      # I have checked that this is reflected in the latent posterior samples. Looks good. 
      
      #CR<-NULL
      #nr<-nrow(date.tot) #Year is stored first in the lc output
 
      #f.yr<-smooth[1:nr+0*nr, "mean"]
      #selo.yr<-smooth[1:nr+0*nr, "0.025quant"]
      #seup.yr<-smooth[1:nr+0*nr, "0.975quant"]
      
      #f.doy<-smooth[1:nr+1*nr, "mean"]
      #selo.doy<-smooth[1:nr+1*nr, "0.025quant"]
      #seup.doy<-smooth[1:nr+1*nr, "0.975quant"]
      
      #oyr<-order(date.tot$fyear)
      #odoy<-order(date.tot$doyfac)
     
      #MyData <- data.frame(
      #  mu   = c(   f.yr[oyr],    f.doy[odoy]), 
      #  SeUp = c(seup.yr[oyr], seup.doy[odoy]), 
      #  SeLo = c(selo.yr[oyr], selo.doy[odoy]), 
      #  Xaxis = c(sort(date.tot$fyear), sort(date.tot$doyfac)),
      #  ID    = factor(rep(c("Velocity smoother", "Depth smoother"), each = nrow(date.tot))))
      
      
     ##OLD 
     #smooth<-top.modelS$summary.lincomb.derived
     #intercept<-top.modelS[["summary.fixed"]][["mean"]][[1]]
   
      #f<-exp(smooth[1:nr, "mean"])
      #SeLo<-exp(smooth[1:nr, "0.025quant"])
      #SeUp<-exp(smooth[1:nr, "0.975quant"]) 
      #CR<-data.frame(mu=f, SeUp=SeUp, SeLo=SeLo, ObservationCount=date.tot$ObservationCount, Year=date.tot$YearCollected)
      #CR<-CR %>% group_by(Year) %>% summarise(meanCR=mean(mu), meanSeUp = mean(SeUp), meanSeLo=mean(SeLo), raw=mean(ObservationCount)) 
      
      # Summary of the GAM smooth on year
      # I have checked that this is reflected in the latent posterior samples. Looks good. 
      
      #nr<-nrow(date.tot) #DOY is stored second in the lc output
      #smooth<-top.model$summary.lincomb.derived
      #f<-exp(smooth[3028:6054, "mean"])
      #SeLo<-exp(smooth[3028:6054, "0.025quant"])
      #SeUp<-exp(smooth[3028:6054, "0.975quant"]) 
      #CR<-data.frame(mu=f, SeUp=SeUp, SeLo=SeLo, ObservationCount=date.tot$ObservationCount, doy=date.tot$doy)
      #CR<-CR %>% group_by(doy) %>% summarise(meanCR=mean(mu), meanOB=mean(ObservationCount), meanSeUp = mean(SeUp), meanSeLo=mean(SeLo)) 
      
      
      #if(!is.null(top.model)) {        
      
      # Calculate abundance indices using the posterior sample of the model
      
      #Posterior GAM estimate with Year effect (iid)
      nsamples<- 1000  
      post.sample1 <-NULL #clear previous
      post.sample1<-inla.posterior.sample(nsamples, top.model)

      tmp1<-NULL
      tmp1 <- select(date.tot, YearCollected)
      
      #for each sample in the posterior we want to join the predicted to tmp so that the predictions line up with doy/year and we can get the mean count by year
      for (h in 1:nsamples){
        pred<-exp(post.sample1[[h]]$latent[1:nrow(date.tot)])
        tmp1[ncol(tmp1)+1]<-pred
      }
      
      tmp1<-tmp1 %>% group_by(YearCollected) %>% summarise_all(mean, na.rm=TRUE)
      tmp1<-tmp1 %>% rowwise() %>% mutate(index = median(c_across(V2:V1001)), lower_ci=quantile(c_across(V2:V1001), 0.025), upper_ci=quantile(c_across(V2:V1001), 0.975), stdev=sd(c_across(V2:V1001))) 
      mn.yr1<-NULL
      mn.yr1<-tmp1 %>% select(YearCollected, index, lower_ci, upper_ci, stdev)
      mn.yr1$species_code <- species
      mn.yr1$years <- paste(min(date.tot$YearCollected), "-", max(date.tot$YearCollected), sep = "")
      mn.yr1$year <- sort(unique(date.tot$YearCollected))
      mn.yr1$period <-"all years"
      mn.yr1$season <- seas
      mn.yr1$results_code <- results.code
      mn.yr1$area_code <- site
      mn.yr1$model_type <- "GAM"
      mn.yr1$species_id<-sp.id
      mn.yr1$version<-max.yr
      mn.yr1$species_name<-sp.nam$english_name
      mn.yr1$species_sci_name<-sp.nam$scientific_name
      mn.yr1$error<-""
      #Assing missing data fields 
      mn.yr1$upload_id<-""
      mn.yr1$stderr<-""
      mn.yr1$stdev<-mn.yr1$stdev
      mn.yr1$trend_id<-""
      mn.yr1$smooth_upper_ci<-""
      mn.yr1$smooth_lower_ci<-""
      mn.yr1$smooth_upper_ci<-""
      mn.yr1$upload_dt<-""
      mn.yr1$family<-family
      
      # Run LOESS function
      
      mn.yr1$LOESS_index = loess_func(mn.yr1$index, mn.yr1$year)
      
      mn.yr1$trend_index<-"" #look at CMMN code to generate in next round of analysis
     
      # Order output before printing to table
      
      #mn.yr2<-mn.yr2 %>% select (upload_id,	results_code,	version,	area_code,	species_code,	species_name,	species_sci_name,	year,	season,	period,	species_id,	index,	stderr,	SD,	upper_ci,	lower_ci,	trend_id,	indexloess,	smooth_upper_ci,	smooth_lower_ci,	upload_dt,	error, meanObs, family)
      mn.yr1<-mn.yr1 %>% select(results_code, version, area_code, season, period, species_code, species_id, year, index, stderr, stdev, upper_ci, lower_ci, LOESS_index, trend_index)
      
       # Write data to table
      write.table(mn.yr1, 
                  file = paste(out.dir,	site, "_", seas, "_AnnualIndices.csv", sep = ""),
                  row.names = FALSE, 
                  append = TRUE, 
                  quote = FALSE, 
                  sep = ",", 
                  col.names = FALSE)
   

# TRENDS: SPECIFIC TO SITE, SEASON, SPECIES, and TIME PERIOD
     
    #determine the smoothed index of abundance using full model at the max and min year of interest
        
      #AUTO-GENERATE TIME PERIODS TO ANALYZE TRENDS
      time.period = NA
      nyears=length(unique(date.tot$YearCollected))
      list.years<-unique(date.tot$YearCollected)
      rev.years<-rev(list.years)
      
      #Generate all-years, 10 years and 3 generation length.
      
      if(is.na(time.period)) {
        endyr <- max(date.tot$YearCollected)
        startyr <- min(date.tot$YearCollected)
        
        #Generation length 
        gen<-gen %>% distinct()
        gen.length<-as.numeric(gen %>% filter(speciesID==sp.id) %>% select(generation))
        gen.length<-floor(gen.length)*3
        
        #if gen.length is missing assign 10
        if(is.na(gen.length)){
          gen.length<-10
        }
        
        #if 3 gen < 10 years, keep 10 years
        if(gen.length<10){
          gen.length<-10
          #threegen<-endyr-gen.length+1  
          threegen<-rev.years[gen.length]
          yrthreegen<-nyears-gen.length+1
        }else{
          threegen<-rev.years[gen.length]  
          yrthreegen<-nyears-gen.length+1
        }
        
        #if 3 gen is longer than the available dataset, keep all years  
        if(gen.length>nyears){
          threegen<-startyr
          yrthreegen<-1  
        }
        
        tenyr<-rev.years[10]
        yrten<-nyears-9
        
        time.period = c("all years", "10-years", "3Gen-Recent")
        Y1.trend <- c(startyr, tenyr, threegen)
        Y2.trend <- c(endyr, endyr, endyr)
        
        y1.trend <- c(1, yrten, yrthreegen)
        y2.trend <- c(nyears, nyears, nyears)
        
      } # end is.na(time.period) 
      
      for(p in 1:length(time.period)) {
        
        period <- time.period[p]
        Y1 <- Y1.trend[p]
        Y2 <- Y2.trend[p]
        y1 <- y1.trend[p]
        y2 <- y2.trend[p]
        
        
        #Prediction of the smooth model
        nsamples<- 1000  
        post.sample2 <-NULL #clear previous
        post.sample2<-inla.posterior.sample(nsamples, top.modelS)
        
        tmp2 <- select(date.tot, YearCollected)
        
        #for each sample in the posterior we want to join the predicted to tmp so that the predictions line up with doy/year and we can get the mean count by year
        for (h in 1:nsamples){
          pred<-exp(post.sample2[[h]]$latent[1:nrow(date.tot)])
          tmp2[ncol(tmp2)+1]<-pred
        }
        
        tmp2<-tmp2 %>% group_by(YearCollected) %>% summarise_all(mean, na.rm=TRUE)
        
        pred.ch<-tmp2 %>% filter(YearCollected %in% c(Y1, Y2)) %>% select(-YearCollected)
        pred.ch<-t(pred.ch)
        pred.ch<-as.data.frame(pred.ch)
        
        pred.ch<-pred.ch %>% mutate(ch=(V2/V1), max_year=Y2, min_year=Y1, tr=(100*((ch^(1/(max_year-min_year)))-1)))
        pred.ch<-pred.ch %>% reframe(trnd=median(tr), percent_change=100*(median(ch)-1), lower_ci=quantile(tr, probs=0.025), upper_ci=quantile(tr, probs=0.95), sd=sd(tr), Width_of_Credible_Interval=upper_ci-lower_ci) %>% distinct()
        
        #write output to table   
        trend.out<-NULL
        trend.out <- pred.ch %>%
          mutate(model_type="GAM", 
                 model_family = family,
                 years = paste(Y1, "-", Y2, sep = ""),
                 year_start=Y1, 
                 year_end=Y2,
                 period =period,
                 season = seas,
                 results_code = results.code,
                 area_code = site,
                 version=max.yr, 
                 species_code = species,
                 species_id=sp.id, 
                 index_type="endpoint", 
                 species_name=sp.nam$english_name,
                 species_sci_name=sp.nam$scientific_name,
                 stderr = "", 
                 model_fit = "", 	
                 percent_change_low ="", 
                 percent_change_high = "",
                 prob_decrease_0 = "",
                 prob_decrease_25 = "",
                 prob_decrease_30 = "",
                 prob_decrease_50 = "",
                 prob_increase_0 = "",
                 prob_increase_33 = "",	
                 prob_increase_100 = "",
                 confidence = "",
                 precision_num = "",
                 suitability="",
                 precision_cat = ifelse(pred.ch$Width_of_Credible_Interval<3.5, "High", ifelse(pred.ch$Width_of_Credible_Interval>=3.5 & pred.ch$Width_of_Credible_Interval<=6.7, "Medium", "Low")),
                 coverage_num = "",
                 coverage_cat = "",
                 goal = "",
                 goal_lower = "",
                 sample_size = "",
                 sample_size_units="",
                 sample_total = "",
                 subtitle = "",
                 pval = "",
                 pval_str = "",
                 post_prob = "",
                 trnd_order = "",
                 dq = "",
                 prob_LD = "",
                 prob_MD = "",
                 prob_LC = "",
                 prob_MI = "",
                 prob_LI = "",
                 quantile_050 = "",
                 quantile_165 = "",
                 quantile_835 = "",
                 quantile_950 = "",
                 trend_id = "",
                 upload_dt = "")
        
       #write.trend<-trend.out %>% select(results_code,	version,	area_code,	species_code,	species_name,	species_sci_name,	species_id,	season,	period,	years,	min_year, max_year, Trend, index_type,	Trend_Q_0.025, Trend_Q_0.95, stderr,	model_type,	model_fit,	percent_change,	percent_change_low,	percent_change_high,	prob_decrease_0,	prob_decrease_25,	prob_decrease_30,	prob_decrease_50,	prob_increase_0,	prob_increase_33,	prob_increase_100,	confidence,	Width_of_Credible_Interval,	precision_cat,	coverage_num,	coverage_cat,	goal,	goal_lower,	sample_size,	sample_total,	subtitle,	pval,	pval_str,	post_prob,	trnd_order,	dq,	slope_trend,	prob_LD,	prob_MD,	prob_LC,	prob_MI,	prob_LI,	quantile_050,	quantile_165,	quantile_835,	quantile_950,	trend_id,	upload_dt,	error, sd)
        write.trend<-trend.out %>% select(results_code,	version,	area_code,	season,	period, species_code,	species_id,	years,year_start,	year_end,	trnd,	lower_ci, upper_ci, stderr,	model_type,	model_fit,	percent_change,	percent_change_low,	percent_change_high,	prob_decrease_0,	prob_decrease_25,	prob_decrease_30,	prob_decrease_50,	prob_increase_0,	prob_increase_33,	prob_increase_100, suitability, precision_num,	precision_cat,	coverage_num,	coverage_cat,	sample_size, sample_size_units, prob_LD, prob_MD, prob_LC, prob_MI, prob_LI)
        
        
        write.table(write.trend, 
                    file = paste(out.dir, site, "_", seas, "_TrendsEndpoint.csv", sep = ""), 
                    row.names = FALSE, 
                    append = TRUE, 
                    quote = FALSE, 
                    sep = ",", 
                    col.names = FALSE)  
        
        
        #Estimate the slope trend base 
        
        # Summary of the GAM smooth on year
        wy=c(y1:y2)
        pred.yr<-tmp2 %>% select(-YearCollected)
        pred.yr<-t(pred.yr)
        ne = log(pred.yr[,wy]) #these are the smoothed indices
        
        #This is the slope function. 
        #It calculates the coefficient of the lm slope for each row in the smoothed output. 
        
        #slope function 1 
        slope  <-  function(x){
          return(coef(lm(x~I(y1:y2)))[2])
        }
        
        m =  apply(ne,1,slope)
        m = as.vector((exp(m)-1)*100)
        
        #slope function 2 (gives same result as above)
        
        # bsl = function(i){
        #   n = length(wy)
        #   sy = sum(i)
        #   sx = sum(wy)
        #   ssx = sum(wy^2)
        #   sxy = sum(i*wy)
        #   b = (n*sxy - sx*sy)/(n*ssx - sx^2)
        #   return(b)
        # } 
        #  m2 =  t(apply(ne,1,FUN = bsl))
        #  m2 = as.vector((exp(m2)-1)*100)
        
        
        #include slop output in new table
        trend.out$index_type="slope"
        trend.out$trnd<-median(m, na.rm=TRUE)
        trend.out$lower_ci<-quantile(m, prob=0.025)
        trend.out$upper_ci<-quantile(m, prob=0.950)
        trend.out$sd<-sd(m, na.rm=TRUE)
        
        per_trend=trend.out$trnd/100
        period_num=Y2-Y1
        trend.out$percent_change<-((1+per_trend)^period_num-1)*100
        trend.out$Width_of_Credible_Interval_slope<-trend.out$upper_ci-trend.out$lower_ci
        trend.out$precision_cat = ifelse(pred.ch$Width_of_Credible_Interval<3.5, "High", ifelse(pred.ch$Width_of_Credible_Interval>=3.5 & pred.ch$Width_of_Credible_Interval<=6.7, "Medium", "Low"))
       
        #write.trend2<-trend.out %>% select(results_code,	version,	area_code,	species_code,	species_name,	species_sci_name,	species_id,	season,	period,	years,	min_year, max_year, Trend, index_type,	Trend_Q_0.025, Trend_Q_0.95, stderr,	model_type,	model_fit,	percent_change,	percent_change_low,	percent_change_high,	prob_decrease_0,	prob_decrease_25,	prob_decrease_30,	prob_decrease_50,	prob_increase_0,	prob_increase_33,	prob_increase_100,	confidence,	Width_of_Credible_Interval,	precision_cat,	coverage_num,	coverage_cat,	goal,	goal_lower,	sample_size,	sample_total,	subtitle,	pval,	pval_str,	post_prob,	trnd_order,	dq,	slope_trend,	prob_LD,	prob_MD,	prob_LC,	prob_MI,	prob_LI,	quantile_050,	quantile_165,	quantile_835,	quantile_950,	trend_id,	upload_dt,	error, sd)
        write.trend<-trend.out %>% select(results_code,	version,	area_code,	season,	period, species_code,	species_id,	years,year_start,	year_end,	trnd,	lower_ci, upper_ci, stderr,	model_type,	model_fit,	percent_change,	percent_change_low,	percent_change_high,	prob_decrease_0,	prob_decrease_25,	prob_decrease_30,	prob_decrease_50,	prob_increase_0,	prob_increase_33,	prob_increase_100, suitability, precision_num,	precision_cat,	coverage_num,	coverage_cat,	sample_size, sample_size_units, prob_LD, prob_MD, prob_LC, prob_MI, prob_LI)
        
        
        write.table(write.trend, 
                    file = paste(out.dir, site, "_", seas, "_TrendsSlope.csv", sep = ""), 
                    row.names = FALSE, 
                    append = TRUE, 
                    quote = FALSE, 
                    sep = ",", 
                    col.names = FALSE)  
        
      
      
      ######################################################################################  
      
  } # end length time period       
 # } # end try error trend top.modelS 
  } # end try error trend top.model 
  } # nrow (tmp)>10
  } # nrow(min.yrs)>0
  } # end if data.tot nrow>0
  } # end sp.list

  } #end max year statement





