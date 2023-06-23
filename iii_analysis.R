#source("00_setup.R")

#sets parameters for your site

collection <- as.character(anal.param[t, "collection"])
station <- as.character(anal.param[t, "station"])
site <- as.character(anal.param[t, "site"])
site.specific <- anal.param[t, "site.specific"]
use.trfl <- anal.param[t, "use.trfl"]
responseM<-anal.param[t , "obs.var.M"]
responseO<-anal.param[t , "obs.var.O"]


in.data<-read.csv(paste(data.dir, site, "_Clean_Data.csv", sep=""))
event.data<-read.csv(paste(data.dir, site, "_Event.csv", sep=""))

df.superfile<-read.csv("Data/CMMNSuperfile.csv")
sp.tax<-meta_species_taxonomy()
sp.tax<-sp.tax %>% select(species_id, english_name, scientific_name)

species.list<-unique(in.data$SpeciesCode)

# Species Specific loop
  
for(k in 1:length(species.list)) {
  sp.data<-NULL #remove old data
  sp.data <- filter(in.data, SpeciesCode == species.list[k]) %>%
    select(-start_date, -end_date, -lpbo_combine, -analysis_code,) %>%
    droplevels()

  sp.id <- unique(sp.data$species_id)
  species <- species.list[k]
  sp.names<-filter(sp.tax, species_id == sp.id)
  
  ## ZERO-FILL DATA by merging event and real data. This is based on the DET column.  
  
  sp.data <- left_join(event.data, sp.data, by = c("SurveyAreaIdentifier",  "YearCollected", "MonthCollected", "DayCollected", "date", "doy", "season"), multiple="all") %>%
    mutate(
      ObservationCount = replace(ObservationCount, is.na(ObservationCount), 0),
      ObservationCount2 = replace(ObservationCount2, is.na(ObservationCount2), 0),
      ObservationCount3 = replace(ObservationCount3, is.na(ObservationCount3), 0),
      ObservationCount4 = replace(ObservationCount4, is.na(ObservationCount4), 0),
      ObservationCount7 = replace(ObservationCount7, is.na(ObservationCount7), 0),
      species_id = sp.id,
      SpeciesCode = species)
  
  ## FILTER DATA FOR SPECIES-SPECIFIC "BAD DATES"
  
  site.list <- as.character(unique(sp.data$SurveyAreaIdentifier))
  
  sp.data <- bscdata.filterBadDates(sp.data, sitecode = site.list, species = sp.id)
  
  ## Migration windows are included in the superfile.
  
  df.superfile <- df.superfile %>%
    mutate(SurveyAreaIdentifier = SiteCode,
                SpeciesCode = species_code,
                season = period) %>% 
    filter(SurveyAreaIdentifier %in% site.list) %>%
    droplevels()
  
  # some species-season have multiple code. Limit to one (random) selection. 
  df.superfile <- df.superfile %>%  group_by(SiteCode, species_code, period, SurveyAreaIdentifier) %>% slice_head(n=1) %>% ungroup()
  
  if(site != "LPBO") {
    df.migWindows <- df.superfile %>%
      filter(SpeciesCode == species.list[k]) %>%
      mutate(SpeciesCode = as.character(SpeciesCode),
             season = as.character(season)) %>%
      select(SurveyAreaIdentifier, SpeciesCode, season, analysis_code) %>%
      droplevels()
  }
  
  
  #THIS WILL NEED CHECKED WHEN RUNNING LPBO
  if((site == "LPBO")) {
    df.migWindows <- df.superfile %>%
      mutate(SurveyAreaIdentifier = SiteCode,
             SpeciesCode = species_code,
             season = period) %>% 
      filter(SpeciesCode == species.list[k], 
             lpbo_combine == 1) %>%
      mutate(SpeciesCode = as.character(SpeciesCode),
             season = as.character(season)) %>%
      select(SurveyAreaIdentifier, SpeciesCode, season, analysis_code) %>%
      droplevels()
  }
  
  
  sp.data <- left_join(sp.data, df.migWindows, by = c("SurveyAreaIdentifier", "SpeciesCode", "season"), multiple="all")
  sp.data$YearCollected<-as.character(sp.data$YearCollected)
  sp.data$MonthCollected<-as.numeric(sp.data$MonthCollected)
  sp.data$DayCollected<-as.numeric(sp.data$DayCollected)
  
  ## FILTER SPECIES THAT DON'T MEET MIN NUMBER OF YEARS
  ## This is done based on the analysis code of the species 
  ## Because analysis code can vary on season, we need to do this within the season loop
  ## M are generally Band + Census and others are DET, detailed in the analysis parameters file. 
  ## species that don't meet # years detected (by site) requirement.
  ## Filter out species that were not observed on a minimum number of years surveyed. Currently needs to be detected on at least half of all years surveyed.  
  ## This may cause problems if only detected in last half of years surveyed.
  
  ## number of years a species must be detected on to be analyzed, currently defined as half of the years surveyed.
  
  seas.list <- as.character(unique(sp.data$season))
  
for (j in 1:length(seas.list)) {
    
    df.tmp <- droplevels(subset(sp.data, season == seas.list[j]))
    analysis_code <- unique(df.tmp$analysis_code)
    
    if(is.na(analysis_code)){
      analysis_code<-"R"
    }
    
    if(nrow(df.tmp) > 0) { 
    
  if(analysis_code=="M"){
  
    df.tmp <- df.tmp %>% dplyr::select(SurveyAreaIdentifier, YearCollected, MonthCollected, DayCollected, date, doy, season, SpeciesCode, species_id, anal.param[t , "obs.var.M"], analysis_code) 
    # rename count variable, so consistent for following steps
    names(df.tmp)[10] <- c("ObservationCount") 
    
  df.nyears <- df.tmp %>%
    filter(ObservationCount > 0) %>%
    select(SurveyAreaIdentifier, YearCollected, season, SpeciesCode) %>%
    distinct() %>%
    group_by(SurveyAreaIdentifier, season, SpeciesCode) %>%
    dplyr::summarize(nyears = dplyr::n()) %>%
    filter(nyears >= min.yrs.detect)%>%
    as.data.frame()
  
  df.tmp <- left_join(df.nyears, df.tmp, by = c("SurveyAreaIdentifier", "season", "SpeciesCode"), multiple="all") %>%
    select(-nyears)
  
  ## FILTER SPECIES THAT DON'T MEET ABUNDANCE AND OCCURRENCE REQUIREMENTS
  
  ## Want to use 0-observation counts to get mean count across years, but drop 0-observation counts to get number of non-0 observation days, so do this in separate steps:
  
  ## 1. Drop seasons where mean number of individuals/year is < 10
  
  df.abund <- NULL
  df.abund <- df.tmp %>%
    group_by(SurveyAreaIdentifier, season, YearCollected) %>%
    summarize(count = sum(ObservationCount)) %>% 
    group_by(SurveyAreaIdentifier, season) %>% #now get mean across years
    summarize(meanCount = mean(count, na.rm = TRUE)) %>%
    filter(meanCount >= 10)%>%
    as.data.frame()
  
  df.tmp <- left_join(df.abund, df.tmp, by = c("SurveyAreaIdentifier", "season"), multiple = "all") %>%
    select(-meanCount)%>%
    as.data.frame()
  
  ## 2. Drop seasons where mean number of observation days/year is < 5
  
  df.abund <- NULL
  df.abund <- df.tmp %>%
    filter(ObservationCount > 0) %>%
    group_by(SurveyAreaIdentifier, season, YearCollected) %>%
    summarize(nobs = n()) %>%
    group_by(SurveyAreaIdentifier, season) %>% #now get mean across years
    summarize(meanObsDays = mean(nobs, na.rm = TRUE)) %>%
    filter(meanObsDays >= 5) %>%
    as.data.frame()
  
  df.tmp <- left_join(df.abund, df.tmp, by = c("SurveyAreaIdentifier", "season"), multiple = "all") %>%
    select(-meanObsDays) %>%
    as.data.frame()
  
  } #end if analysis code = M
  
#select the other response variable if not M species or if M species does not meet minimum data requirement. 
  if(analysis_code!="M"|nrow(df.tmp) == 0){

    df.tmp <- df.tmp %>% dplyr::select(SurveyAreaIdentifier, YearCollected, MonthCollected, DayCollected, date, doy, season, SpeciesCode, species_id, anal.param[t , "obs.var.O"], analysis_code) 
    # rename count variable, so consistent for following steps
    names(df.tmp)[10] <- c("ObservationCount") 
    
    df.nyears <- NULL
    df.nyears <- df.tmp %>%
      filter(ObservationCount > 0) %>%
      select(SurveyAreaIdentifier, YearCollected, season, SpeciesCode) %>%
      distinct() %>%
      group_by(SurveyAreaIdentifier, season, SpeciesCode) %>%
      dplyr::summarize(nyears = dplyr::n()) %>%
      filter(nyears >= min.yrs.detect)%>%
      as.data.frame()
    
    df.tmp <- left_join(df.nyears, df.tmp, by = c("SurveyAreaIdentifier", "season", "SpeciesCode"), multiple="all") %>%
      select(-nyears)
    
    ## FILTER SPECIES THAT DON'T MEET ABUNDANCE AND OCCURRENCE REQUIREMENTS
    
    ## Want to use 0-observation counts to get mean count across years, but drop 0-observation counts to get number of non-0 observation days, so do this in separate steps:
    
    ## 1. Drop seasons where mean number of individuals/year is < 10
    
    df.abund <- NULL
    df.abund <- df.tmp%>%
      group_by(SurveyAreaIdentifier, season, YearCollected) %>%
      summarize(count = sum(ObservationCount , na.rm = TRUE)) %>% 
      group_by(SurveyAreaIdentifier, season) %>% #now get mean across years
      summarize(meanCount = mean(count, na.rm = TRUE)) %>%
      filter(meanCount >= 10)%>%
      as.data.frame()
    
    df.tmp <- left_join(df.abund, df.tmp, by = c("SurveyAreaIdentifier", "season"), multiple="all") %>%
      select(-meanCount)%>%
      as.data.frame()
    
    ## 2. Drop seasons where mean number of observation days/year is < 5
    
    df.abund <- NULL
    df.abund <- df.tmp %>%
      filter(ObservationCount > 0) %>%
      group_by(SurveyAreaIdentifier, season, YearCollected) %>%
      summarize(nobs = n()) %>%
      group_by(SurveyAreaIdentifier, season) %>% #now get mean across years
      summarize(meanObsDays = mean(nobs, na.rm = TRUE)) %>%
      filter(meanObsDays >= 5) %>%
      as.data.frame()
    
    df.tmp <- left_join(df.abund, df.tmp, by = c("SurveyAreaIdentifier", "season"), multiple="all") %>%
      select(-meanObsDays) %>%
      as.data.frame()
    
  } #end if analysis code != M
    } #end if nrow df.tmp
  
#continue if there is data
  if(nrow(df.tmp) > 0) {
    
# CALCULATE ANNUAL INDICES AND TRENDS
    
    # turn species code into factor
    df.tmp <- df.tmp %>%
      mutate(SpeciesCode = as.factor(SpeciesCode)) %>%
      droplevels()

#   ####################################################################
    ####################################################################
   
    #standardize variables
    #standardize year to current year  
    
    df.tmp$YearCollected<-as.numeric(df.tmp$YearCollected)
    df.tmp <- df.tmp %>% mutate(cyear = scale(YearCollected), 
                 fyear = as.factor(YearCollected),
                 doyfac = as.factor(doy)) %>% 
                 arrange(YearCollected)
    
    #index for year ordered. 
    df.tmp$oyear <- as.integer(factor(df.tmp$YearCollected)) #index for the random site effect
   
   ######### MODEL FORMULAS
        
        #Note: Tried adding oyear as a replicate in the f(doy, model="ar1", replicate=oyear) but this caused issues with inla. Remove and the model runs smooth. 
        
        # MODEL FORMULA with GAM for doy and year effects
        
        #determine the number of knots for year
        #add one knot for every 4 years of the time series follow Smith and Edwards
        nyears <- length(unique(df.tmp$fyear))
        kyears<- ceiling(nyears/4) #round up to nearest whole number
        
        ndays <- length(unique(df.tmp$doyfac))
        kdays<- ceiling(ndays/8)
        
        #kdays<-2 #polynomial like previous
        
       #smooth years
        smy<-smoothCon(s(YearCollected, bs="cr", k=kyears), data=df.tmp)[[1]]
        Xsmy<-smy$X #basis function
        colnames(Xsmy)<-paste("BasisYR", 1:ncol(Xsmy), sep="")#need unique names
        
        lcs.y<-inla.make.lincombs(data.frame(Xsmy))
        names(lcs.y)<-paste(names(lcs.y), "YR", sep="")#need unique names
        
        df.tmp<-cbind(df.tmp, Xsmy)
        
        #smooth day of year
        sdy<-smoothCon(s(doy, bs="cr", k=kdays), data=df.tmp)[[1]]
        Xsdy<-sdy$X #basis function
        colnames(Xsdy)<-paste("BasisDay", 1:ncol(Xsdy), sep="")#need unique names
        
        lcs.d<-inla.make.lincombs(data.frame(Xsdy))
        names(lcs.d)<-paste(names(lcs.d), "DAY", sep="")#need unique names
        
        df.tmp<-cbind(df.tmp, Xsdy)
        lcs<-c(lcs.y, lcs.d)
        
        #Use penalised complex prior for random year effect
        hyper.iid<-list(prec=list(prior="pc.prec", param=c(2,0.05)))
                
        if(length(unique(df.tmp$SurveyAreaIdentifier)) == 1) {                                         
         
           index.gam<- ObservationCount ~ -1 + Xsmy + Xsdy + f(fyear, model="iid", hyper=hyper.iid)
           index.gamS<- ObservationCount ~ -1 + Xsmy + Xsdy 
           }
   
        #NEEDS UPDATED
        if(length(unique(df.tmp$SurveyAreaIdentifier)) > 1) {                                         
         
          index.gam<- ObservationCount ~ -1 + Xsmy + Xsdy +
            + f(SurveyAreaIdentifier, model = "iid") + f(yearfac, model="iid")
          
          index.gamS<- ObservationCount ~ -1 + Xsmy + Xsdy +
            + f(SurveyAreaIdentifier, model = "iid") 
            }
        
###### RUN ANALYSIS
        
        #Run nbinomial, poisson, zip model. Select the model with the lowest DIC.                
            
            #index.nb <-index.pois <-index.zip<- NULL
      
            #index.nb <- try(inla(index.gam, family = "nbinomial", data = df.tmp,
            #                  control.predictor = list(compute = TRUE), control.compute = list(dic=TRUE, config = TRUE), lincomb=lcs, verbose =TRUE), silent = T)
            
            #index.pois <- try(inla(index.gam, family = "poisson", data = df.tmp,
            #                  control.predictor = list(compute = TRUE), control.compute = list(dic=TRUE, config = TRUE), lincomb=lcs, verbose =TRUE), silent = T)
            
            #index.zip<-try(inla(index.gam, family = "zeroinflatedpoisson1", data = df.tmp,
            #                    control.predictor = list(compute = TRUE), control.compute = list(dic=TRUE, config = TRUE), lincomb=lcs, verbose =TRUE), silent = T)
          
            #model<-c("nbinomial", "poisson", "zeroinflatedpoisson1")
            #index.nb.dic<-ifelse(class(index.nb) == 'try-error', NA, index.nb[["dic"]][["dic"]])
            #index.pois.dic<-ifelse(class(index.pois) == 'try-error', NA, index.pois[["dic"]][["dic"]])
            #index.zip.dic<-ifelse(class(index.zip) == 'try-error', NA, index.zip[["dic"]][["dic"]])
            #dic<-c(index.nb.dic, index.pois.dic, index.zip.dic)
            #index<-c("index.nb", "index.pois", "index.zip")
            #t.model<-data.frame(model, index, dic)
            
            #t.model<-t.model %>% slice_min(dic, na_rm = TRUE)
            #family<-t.model[,1]
            #index<-t.model[,2]
            
            #rerun the top model and save output
            top.model<-try(inla(index.gam, family = "nbinomial", data = df.tmp,
                                control.predictor = list(compute = TRUE), control.compute = list(dic=TRUE, config = TRUE), lincomb=lcs, verbose =TRUE), silent = T)
            
            top.modelS<-try(inla(index.gamS, family = "nbinomial", data = df.tmp,
                                control.predictor = list(compute = TRUE), control.compute = list(dic=TRUE, config = TRUE), lincomb=lcs, verbose =TRUE), silent = T)
            
            
#What to do if there is an error in the top.model		
            if(class(top.model) == 'try-error'| is.null(top.model)){
              
              error$Site<-station
              error$Season<-seas.list[j]
              error$SpeciesCode<-species
              error$time.period<-period
              
              #Print final error table to file
              
              write.table(error, file = paste(out.dir, "ErrorFile.csv", sep = ""), row.names = FALSE, append = TRUE, 
                          quote = FALSE, sep = ",", col.names = FALSE)
    }	#end try-error statement
    
          
  #What to do if there is no error in the try error above for either INLA function  	
  
          #first define the is.not.null function
          is.not.null <- function(x) !is.null(x)
         
          if(class(top.model) != 'try-error'& is.not.null(top.model)){
            if(class(top.modelS) != 'try-error'& is.not.null(top.modelS)){
            
# ANNUAL INDICES: GENERATED FOR FULL TIME PERIOD ONLY
    # SPECIFIC TO SITE, SEASON, SPECIES 
    # use same data distribution as for trends
    # use the index model do the trend line does not influence the indices generated  
              
  # Summary of the GAM smooth on year
  # I have checked that this is reflected in the latent posterior samples. Looks good. 
           
           #nr<-nrow(df.tmp) #Year is stored first in the lc output
           #smooth<-top.modelS$summary.lincomb.derived
           #f<-exp(smooth[1:nr, "mean"])
           #SeLo<-exp(smooth[1:nr, "0.025quant"])
           #SeUp<-exp(smooth[1:nr, "0.975quant"]) 
           #CR<-data.frame(mu=f, SeUp=SeUp, SeLo=SeLo, ObservationCount=df.tmp$ObservationCount, Year=df.tmp$YearCollected)
           #CR<-CR %>% group_by(Year) %>% summarise(meanCR=mean(mu), meanSeUp = mean(SeUp), meanSeLo=mean(SeLo)) 
          
  # Summary of the GAM smooth on year
  # I have checked that this is reflected in the latent posterior samples. Looks good. 
      
           #nr<-nrow(df.tmp) #DOY is stored second in the lc output
           #smooth<-top.model$summary.lincomb.derived
           #f<-exp(smooth[824:1646, "mean"])
           #SeLo<-exp(smooth[824:1646, "0.025quant"])
           #SeUp<-exp(smooth[824:1646, "0.975quant"]) 
           #CR<-data.frame(mu=f, SeUp=SeUp, SeLo=SeLo, ObservationCount=df.tmp$ObservationCount, doy=df.tmp$doy)
           #CR<-CR %>% group_by(doy) %>% summarise(meanCR=mean(mu), meanOB=mean(ObservationCount), meanSeUp = mean(SeUp), meanSeLo=mean(SeLo)) 
  

  #if(!is.null(top.model)) {        
             
  # Calculate abundance indices using the posterior sample of the model

            #Posterior GAM estimate with Year effect (iid)
                nsamples<- 1000  
                post.sample1 <-NULL #clear previous
                post.sample1<-inla.posterior.sample(nsamples, top.model)
                tmp1 <- select(df.tmp, SpeciesCode, YearCollected, doy, ObservationCount)
                
                #for each sample in the posterior we want to join the predicted to tmp so that the predictions line up with doy/year and we can get the mean count by year
                pred.yr<-matrix(nrow=nsamples, ncol=nyears)
                
                for (k in 1:nsamples){
                  tmp1$pred<-exp(post.sample1[[k]]$latent[1:nrow(df.tmp)])
                  pred.yr[k,]<-t(with(tmp1, aggregate (pred, list(YearCollected=YearCollected), mean, na.action=na.omit))$x)
               
                  }
                
              #Posterior GAM estimate smoothed (not year effect)     
                
                  post.sample2<-NULL #clear previous
                  post.sample2<-inla.posterior.sample(nsamples, top.modelS)
                  tmp2 <- select(df.tmp, SpeciesCode, YearCollected, doy, ObservationCount)
                  
                  #for each sample in the posterior we want to join the predicted to tmp so that the predictions line up with doy/year and we can get the mean count by year
                  pred.yr2<-matrix(nrow=nsamples, ncol=nyears)
                
                  for (k in 1:nsamples){
                    tmp2$pred2<-exp(post.sample2[[k]]$latent[1:nrow(df.tmp)])
                    pred.yr2[k,]<-t(with(tmp2, aggregate (pred2, list(YearCollected=YearCollected), mean, na.action=na.omit))$x)
                      }
                  
                  mn.yr1<-NULL
                  mn.yr1<-matrix(nrow=nyears, ncol=4)
                  
                  for(g in 1:nyears){
                    mn.yr1[g,1]<-median(pred.yr[,g], na.rm=TRUE)
                    mn.yr1[g,2]<-quantile(pred.yr[,g], 0.025, na.rm=TRUE)
                    mn.yr1[g,3]<-quantile(pred.yr[,g], 0.975, na.rm=TRUE)
                    mn.yr1[g,4]<-sd(pred.yr[,g], na.rm=TRUE)
                  }
               
                  mn.yr1 <- as.data.frame(mn.yr1)
                  names(mn.yr1) <- c("index", "lower_ci", "upper_ci", "SD")
                  
               #   mn.yrS<-NULL
               #   mn.yrS<-matrix(nrow=nyears, ncol=4)
                    
                #    for(g in 1:nyears){
                #     mn.yrS[g,1]<-mean(pred.yr2[,g], na.rm=TRUE)
                #      mn.yrS[g,2]<-quantile(pred.yr2[,g], 0.025, na.rm=TRUE)
                #      mn.yrS[g,3]<-quantile(pred.yr2[,g], 0.975, na.rm=TRUE)
                #      mn.yrS[g,4]<-sd(pred.yr2[,g], na.rm=TRUE)
                #    }
                 
                #  mn.yrS <- as.data.frame(mn.yrS)
                #  names(mn.yrS) <- c("index_smooth", "lower_ci_smooth", "upper_ci_smooth", "SD_smooth")
                #  mn.yr1$index_smooth<-mn.yrS$index_smooth
                #  mn.yr1$lower_ci_smooth<-mn.yrS$lower_ci_smooth
                #  mn.yr1$upper_ci_smooth<-mn.yrS$upper_ci_smooth
                #  mn.yr1$SD_smooth<-mn.yrS$SD_smooth
                  mn.yr1$species_code <- species
                  mn.yr1$years <- paste(min(df.tmp$YearCollected), "-", max(df.tmp$YearCollected), sep = "")
                  mn.yr1$year <- sort(unique(df.tmp$YearCollected))
                  mn.yr1$period <-"all years"
                  mn.yr1$season <- seas.list[j]
                  mn.yr1$results_code <- results.code
                  mn.yr1$area_code <- station
                  mn.yr1$analysis_code <- analysis_code
                  mn.yr1$model_type <- "GAM"
                  mn.yr1$species_id<-sp.id
                  mn.yr1$version<-max.year
                  mn.yr1$species_name<-sp.names$english_name
                  mn.yr1$species_sci_name<-sp.names$scientific_name
                  mn.yr1$error<-"NULL"
                #Assing missing data fields 
                  mn.yr1$upload_id<-"NULL"
                  mn.yr1$stderr<-"NULL"
                  mn.yr1$trend_id<-"NULL"
                  mn.yr1$smooth_upper_ci<-"NULL"
                  mn.yr1$smooth_lower_ci<-"NULL"
                  mn.yr1$smooth_upper_ci<-"NULL"
                  mn.yr1$upload_dt<-"NULL"
                     
  # Run LOESS function
                
                  mn.yr2<-NULL
                  mn.yr2 <- mn.yr1 %>% mutate(indexloess = loess_func(index,year))
                  
                  raw.obs<-df.tmp %>% select(YearCollected, ObservationCount) %>% group_by(YearCollected) %>% summarise(meanObs=mean(ObservationCount)) %>% dplyr::rename(year=YearCollected)
                  
                  mn.yr2<-left_join(mn.yr2, raw.obs, by="year")
                  
   
 # Order output before printing to table
                
  
     mn.yr2<-mn.yr2 %>% select (upload_id,	results_code,	version,	area_code,	species_code,	species_name,	species_sci_name,	year,	season,	period,	species_id,	index,	stderr,	SD,	upper_ci,	lower_ci,	trend_id,	indexloess,	smooth_upper_ci,	smooth_lower_ci,	upload_dt,	error, meanObs, analysis_code)
    
     mean.index<-mean(mn.yr2$index)
   
#Because some models produce extreme results (likely because of poor model fit) we will not write this to the output file nor will we run trends for these speceis/seasons.   
     

     if(mean.index>=1000 &  mean.index<=.0001){ 
       
       mn.yr2$error<-"index estimation error"
       
     }
                       
 # Write data to table
                  write.table(mn.yr2, 
                              file = paste(out.dir,	site, "_AnnualIndices.csv", sep = ""),
                              row.names = FALSE, 
                              append = TRUE, 
                              quote = FALSE, 
                              sep = ",", 
                              col.names = FALSE)
                
                 
                  
#### Trends
##End-point trends for all years, 10-years, and 3-generations (min 10 years)
                  
                  #AUTO-GENERATE TIME PERIODS TO ANALYZE TRENDS
                  time.period = NA
                  
                  #Generate all-years, 10 years and 3 generation length.
                  
                  if(is.na(time.period)) {
                    endyr <- max(df.tmp$YearCollected)
                    startyr <- min(df.tmp$YearCollected)
                  
                  #Generation length 
                    gen<-gen %>% distinct()
                    gen.length<-as.numeric(gen %>% filter(speciesID==sp.id) %>% select(generation))
                    gen.length<-floor(gen.length)
                    
                  #if gen.length is missing assign 10
                    if(is.na(gen.length)){
                      gen.length<-10
                    }
                    
                  #if 3 gen < 10 years, keep 10 years
                    if(gen.length<10){
                      gen.length<-10
                      threegen<-endyr-gen.length
                      yrthreegen<-nyears-gen.length
                    }else{
                      threegen<-endyr-gen.length  
                      yrthreegen<-nyears-gen.length
                    }
                    
                  #if 3 gen is longer than the available dataset, keep all years  
                    if(gen.length>nyears){
                      threegen<-startyr
                      yrthreegen<-1  
                    }
                    
                    tenyr<-endyr-10
                    yrten<-nyears-10
                    
                    time.period = c("all years", "10-years", "3-generation")
                    Y1.trend <- c(startyr, tenyr, threegen)
                    Y2.trend <- c(endyr, endyr, endyr)
                    
                    y1.trend <- c(1, yrten, yrthreegen)
                    y2.trend <- c(nyears, nyears, nyears)
                    
                  } # end is.na(time.period) 
                  
                  # TRENDS: SPECIFIC TO SITE, SEASON, SPECIES, and TIME PERIOD
                  # LOOP THROUGH TIME PERIODS
                  
                         for(p in 1:length(time.period)) {
                  
                            period <- time.period[p]
                            Y1 <- Y1.trend[p]
                            Y2 <- Y2.trend[p]
                            y1 <- y1.trend[p]
                            y2 <- y2.trend[p]
                       
                  
                  #determine the smoothed index of abundance using full model at the max and min year of interest
                  
                  pred.ch<-pred.yr2[,c(y1, y2)] 
                  pred.ch<-as.data.frame(pred.ch) 
                  pred.ch<-pred.ch %>% mutate(ch=V2/V1, max_year=Y2, min_year=Y1, tr=(100*((ch^(1/(max_year-min_year)))-1)))
                  pred.ch<-pred.ch %>% reframe(Trend=median(tr), percent_change=100*(median(ch)-1), Trend_Q_0.025=quantile(tr, probs=0.025), Trend_Q_0.95=quantile(tr, probs=0.95), sd=sd(tr), Width_of_Credible_Interval=Trend_Q_0.95-Trend_Q_0.025) %>% distinct()
                    
                  #write output to table   
                  trend.out<-NULL
                  trend.out <- pred.ch %>%
                   mutate(model_type="GAM", 
                           model_family = "nbinomial",
                           years = paste(Y1, "-", Y2, sep = ""),
                           min_year=Y1, 
                           max_year=Y2,
                           period =period,
                           season = seas.list[j],
                           results_code = results.code,
                           area_code = station,
                           analysis_code = analysis_code, 
                           version=max.year, 
                           species_code = species,
                           species_id=sp.id, 
                           index_type="endpoint", 
                           species_name=sp.names$english_name,
                           species_sci_name=sp.names$scientific_name,
                           error=unique(mn.yr2$error), 
                           stderr = "NULL", 
                           model_fit = "NULL", 	
                          percent_change_low ="NULL", 
                          percent_change_high = "NULL",
                          prob_decrease_0 = "NULL",
                          prob_decrease_25 = "NULL",
                          prob_decrease_30 = "NULL",
                          prob_decrease_50 = "NULL",
                          prob_increase_0 = "NULL",
                          prob_increase_33 = "NULL",	
                          prob_increase_100 = "NULL",
                          confidence = "NULL",
                          precision_num = "NULL",
                          precision_cat = ifelse(pred.ch$Width_of_Credible_Interval<3.5, "High", ifelse(pred.ch$Width_of_Credible_Interval>=3.5 & pred.ch$Width_of_Credible_Interval<=6.7, "Medium", "Low")),
                          coverage_num = "NULL",
                          coverage_cat = "NULL",
                          goal = "NULL",
                          goal_lower = "NULL",
                          sample_size = "NULL",
                          sample_total = "NULL",
                          subtitle = "NULL",
                          pval = "NULL",
                          pval_str = "NULL",
                          post_prob = "NULL",
                          trnd_order = "NULL",
                          dq = "NULL",
                          slope_trend = "NULL",
                          prob_LD = "NULL",
                          prob_MD = "NULL",
                          prob_LC = "NULL",
                          prob_MI = "NULL",
                          prob_LI = "NULL",
                          quantile_050 = "NULL",
                          quantile_165 = "NULL",
                          quantile_835 = "NULL",
                          quantile_950 = "NULL",
                          trend_id = "NULL",
                          upload_dt = "NULL")
           
                 
                  write.trend<-trend.out %>% select(results_code,	version,	area_code,	species_code,	species_name,	species_sci_name,	species_id,	season,	period,	years,	min_year, max_year, Trend,	index_type,	Trend_Q_0.025, Trend_Q_0.95, stderr,	model_type,	model_fit,	percent_change,	percent_change_low,	percent_change_high,	prob_decrease_0,	prob_decrease_25,	prob_decrease_30,	prob_decrease_50,	prob_increase_0,	prob_increase_33,	prob_increase_100,	confidence,	Width_of_Credible_Interval,	precision_cat,	coverage_num,	coverage_cat,	goal,	goal_lower,	sample_size,	sample_total,	subtitle,	pval,	pval_str,	post_prob,	trnd_order,	dq,	slope_trend,	prob_LD,	prob_MD,	prob_LC,	prob_MI,	prob_LI,	quantile_050,	quantile_165,	quantile_835,	quantile_950,	trend_id,	upload_dt,	error, sd, analysis_code)
                  
                
                   
                   write.table(write.trend, 
                               file = paste(out.dir, site, "_Trends.csv", sep = ""), 
                               row.names = FALSE, 
                               append = TRUE, 
                               quote = FALSE, 
                               sep = ",", 
                               col.names = FALSE)  
        
#Estimate the slope trend base 
               
                # Summary of the GAM smooth on year
                # wy=c(y1:y2)
                # ne = (pred.yr2[,wy]) #these are the smoothed indices
                 
                 #This is the slope function. 
                 #It calculates the coefficient of the lm slope for each row in the smoothed output. 
                
                # **Not convinced this is correct ** 
                # slope  <-  function(x){
                #   return(coef(lm(I(y1:y2)~x))[2])
                # }
                 
                # m =  apply(ne,1,slope)

                 #include output in the table
                # trend.out$Trend_slope<-median(m, na.rm=TRUE)
                # trend.out$Trend_slope_Q_0.025<-quantile(m, prob=0.025)
                # trend.out$Trend_slope_Q_0.95<-quantile(m, prob=0.950)
                # trend.out$sd_slope<-sd(m, na.rm=TRUE)
                # per_trend=trend.out$Trend_slope/100
                # period_num=Y2-Y1
                # trend.out$percent_change_slope<-((1+per_trend)^period_num-1)*100
                # trend.out$percent_change_slope= ((1+(trend.out$per_trend))^(1/(Y2-Y1))-1)*100
                #trend.out$percent_change_slope=100*(median(m)-1)
                # trend.out$Width_of_Credible_Interval_slope<-trend.out$Trend_slope_Q_0.95-trend.out$Trend_slope_Q_0.025
                # trend.out$index_type="slope"
                 
                # write.trend<-NULL
                # write.trend<-trend.out %>% select(results_code, version, area_code, species_code, species_id, season, period, years, min_year, max_year, Trend_slope, Trend_slope_Q_0.025, Trend_slope_Q_0.95, sd_slope, percent_change_slope, Width_of_Credible_Interval_slope, model_type, index_type, analysis_code)
                 
                # write.table(write.trend, 
                #             file = paste(out.dir, site, "_Trends.csv", sep = ""), 
                #             row.names = FALSE, 
                #             append = TRUE, 
                #             quote = FALSE, 
                #             sep = ",", 
                #             col.names = FALSE)                  
                 
                          
          #Order output before printing to table
                } #end time period loop
 
                   ######################################################################################  
                  
                
        } # end try error trend top.modelS 
          } # end try error trend top.model 
             } # end if nrow>0
       
       } #end of for (i in 1:length(seas.list)) 
      } # end of   for (k in 1:length(sp.list))
    

#test plot

#ggplot()+
#geom_point(data=mn.yr2, aes(x=year, y=meanObs), colour="red")+
#geom_point(data=mn.yr2, aes(x=year, y=index))+
#geom_line(data=mn.yr2, aes(x=year, y=index))+
#geom_ribbon(data=mn.yr2, aes(x=year, y=index, ymin=lower_ci, ymax=upper_ci), alpha=0.3)+
#ylab("index")+
#scale_y_continuous(limits = c(0, 7))+
#theme_classic()

  
#ggplot()+
#geom_point(data=mn.yr2, aes(x=year, y=meanObs), colour="red")+
#geom_point(data=mn.yr2, aes(x=year, y=index_smooth))+
#geom_line(data=mn.yr2, aes(x=year, y=index_smooth))+
#geom_ribbon(data=mn.yr2, aes(x=year, y=index_smooth, ymin=lower_ci_smooth, ymax=upper_ci_smooth), alpha=0.3)+
#ylab("index smooth")+
#scale_y_continuous(limits = c(0, 6))+
#theme_classic()






