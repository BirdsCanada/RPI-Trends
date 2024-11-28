myfiles<-list.files(path=out.dir, pattern = "*TrendsSlope.csv", full.names = TRUE)
myfiles

dat.csv<-plyr::ldply(myfiles, read.csv)

#detach(package:plyr) #this will screw up the dplyr pacakge. 

in.data<-filter(dat.csv, period=="10-years")

#remove any duplicated output
in.data<-in.data[!duplicated(in.data),]

in.data<- merge(in.data, anal.param, by.x = "area_code", by.y = "SiteCode")

in.data<-in.data %>% dplyr::select(species_code, trnd, lower_ci, upper_ci, period, years, season, area_code, region) 
  
#create a categorical trend based on the CI's covering zero or not

in.data<- in.data %>% mutate(trnd_direction = ifelse (lower_ci >=0 & upper_ci >=0, "increase", ifelse (lower_ci<=0 & upper_ci<=0, "decrease", "stable")))
in.data<-in.data %>%mutate(region = trimws(region))


#Subset by season
in.data.f<-subset(in.data, season=="fall")

in.data.s<-subset(in.data, season=="spring")

#Group by region and summarize by species

fall.sum<-in.data.f %>% group_by(species_code, region) %>% 
  dplyr::summarize(total_stations=n_distinct(area_code), .groups = "keep") %>%
  ungroup() 

fall.sp.sum<-in.data.f %>% group_by(region, species_code, trnd_direction) %>% 
  dplyr::summarise(count = length(trnd), .groups = "keep") 

fall.sum<-left_join(fall.sp.sum, fall.sum, by=c("species_code", "region"), relationship = "many-to-many")
  
spring.sum<-in.data.s %>% group_by(species_code, region) %>% 
  dplyr::summarize(total_stations=n_distinct(area_code)) %>%
  ungroup() 

spring.sp.sum<-in.data.s %>% group_by(region, species_code, trnd_direction) %>% 
  dplyr::summarise(count = length(trnd)) 

spring.sum<-left_join(spring.sp.sum, spring.sum, by=c("species_code", "region"), relationship = "many-to-many")
  

write.csv(fall.sum, "2023 10-year Fall Regional Summary.csv")
write.csv(spring.sum, "2023 10-year Spring Regional Summary.csv")


###Do it also for 20 year

in.data<-dat.csv %>% dplyr::filter(period=="20-years")

#remove any duplicated output
in.data<-in.data[!duplicated(in.data),]

in.data<- merge(in.data, anal.param, by.x = "area_code", by.y = "SiteCode")

in.data<-in.data %>% dplyr::select(species_code, trnd, lower_ci, upper_ci, period, years, season, area_code, region) 

#create a categorical trend based on the CI's covering zero or not

in.data<- in.data %>% mutate(trnd_direction = ifelse (lower_ci >=0 & upper_ci >=0, "increase", ifelse (lower_ci<=0 & upper_ci<=0, "decrease", "stable")))
in.data<-in.data %>%mutate(region = trimws(region))

#Subset by season
in.data.f<-subset(in.data, season=="fall")

in.data.s<-subset(in.data, season=="spring")

#Group by region and summarize by species

fall.sum<-in.data.f %>% group_by(species_code, region) %>% 
  dplyr::summarize(total_stations=n_distinct(area_code)) %>%
  ungroup() 

fall.sp.sum<-in.data.f %>% group_by(region, species_code, trnd_direction) %>% 
  dplyr::summarise(count = length(trnd)) 

fall.sum<-left_join(fall.sp.sum, fall.sum, by=c("species_code", "region"), relationship = "many-to-many")

spring.sum<-in.data.s %>% group_by(species_code, region) %>% 
  dplyr::summarize(total_stations=n_distinct(area_code)) %>%
  ungroup() 

spring.sp.sum<-in.data.s %>% group_by(region, species_code, trnd_direction) %>% 
  dplyr::summarise(count = length(trnd)) 

spring.sum<-left_join(spring.sp.sum, spring.sum, by=c("species_code", "region"), relationship = "many-to-many")


write.csv(fall.sum, "2023 20-year Fall Regional Summary.csv")
write.csv(spring.sum, "2023 20-year Spring Regional Summary.csv")
