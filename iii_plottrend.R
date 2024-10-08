# PLOT ANNUAL INDICES AND TRENDS
# Loop through sites and seasons and produce separate PDF plot
# function to call in various analysis functions
  
require(tidyverse)
require(ggplot2)
require(naturecounts)

out.dir <- paste("Output/", max.yr, "/", sep = "")
plot.dir <- paste("Plots/", max.yr, "/", sep = "")

site <- as.character(anal.param[t, "SiteCode"])
seas <- as.character(anal.param[t,"seas"])
name<-as.character(anal.param[t,"site"])

sp.tax<-meta_species_taxonomy()
sp.tax<-sp.tax %>% select(species_id, sort_order, english_name)

# read in trend output
trnd<-NULL
trnds<-NULL

#trnd <- read.csv(paste(out.dir, site, "_", seas, "_TrendsEndpoint.csv", sep = ""))
trnds <- read.csv(paste(out.dir, site, "_", seas, "_TrendsSlope.csv", sep = ""))

#trnd <- trnd %>%
#  filter(!(is.na(species_code)) & period == "all years") %>%
#  select(trnd, upper_ci, lower_ci, species_code, species_id, area_code, season)

#trnd<-left_join(trnd, sp.tax, by="species_id")

trnds<-left_join(trnds, sp.tax, by="species_id")

options(digits = 2)

#sp.trnd <- trnd %>%
#  mutate(sp.trend = paste(english_name, " \n", "Endpoint: ",
#                      round( trnd, digits = 2),  " (", round( lower_ci, digits = 2), ", ",
#                      round( upper_ci, digits = 2), ")", sep = "")) %>% 
#  select(-trnd, -upper_ci, -lower_ci)

trnd2 <- trnds %>%
  filter(!(is.na(species_code)) & period == "all years") %>%
  select(trnd, upper_ci, lower_ci, english_name, species_id, area_code, season)


options(digits = 2)

sp.trnd <- trnd2 %>%
  mutate(sp.trend = paste(english_name, " \n", "Slope: ", round( trnd, digits = 2),  " (", round( lower_ci, digits = 2), ", ",
                          round( upper_ci, digits = 2), ")", sep = "")) %>% 
  select(-trnd, -upper_ci, -lower_ci)

#sp.trnd <- full_join(sp.trnd, sp.trnd2, by = c("area_code", "season", "english_name", "species_id"), relationship = "many-to-many")
#sp.trnd<-sp.trnd %>% mutate(sp.trend = paste(sp.trend, " \n", sp.trend2, sep="")) %>% select(-sp.trend2)

# read in annual index output
index<-NULL
index <- read.csv(paste(out.dir, site, "_", seas, "_AnnualIndices.csv", sep = ""))
index<-left_join(index, sp.tax, by="species_id")

index <- index %>%
  filter(!is.na(species_code) & period == "all years") %>%
  select(index, upper_ci, lower_ci, LOESS_index, english_name, species_id, year, season, area_code) 

# merge the two
plot.dat <- NULL
plot.dat <- full_join(index, sp.trnd, by = c("area_code", "season", "english_name", "species_id"), relationship = "many-to-many")


# following is so that order of plots is correct
plot.dat$sp.trnd <- factor(plot.dat$sp.trend, levels=unique(plot.dat$sp.trend))
plot.dat <- subset(plot.dat, !is.na(year))
plot.dat<-subset (plot.dat, !is.na(sp.trnd))

plot.dat<-left_join(plot.dat, sp.tax, by=c("species_id", "english_name"))
plot.dat <- plot.dat[order(plot.dat$sort_order),]

sp.list <- as.character(unique(plot.dat$english_name))

#smooth<-plot.dat %>% select(smooth_index, year, sp.trend) %>% rename(index = smooth_index)

out.plot <- NULL
i <- 1
j <- 6

  for(k in 1:(ceiling(length(sp.list)/6))) {
    
    out.plot[[k]] <- ggplot(data = subset(plot.dat, english_name %in% sp.list[i:j]), aes(x = as.numeric(year), y = index)) +
      facet_wrap(~ sp.trend, ncol = 2, scales = "free", as.table = TRUE) +
      geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci, group = season, shape = season)) +
      geom_smooth(aes(ymin = lower_ci, ymax = upper_ci, group = season, colour = season, fill = season, linetype = season), method = "loess") + 
      xlab("Year") +
      ylab("Annual Index") +
      theme_bw() +
      scale_y_continuous(trans="log10") +
    # scale_x_continuous(breaks = seq(from = min.yr.filt, to = max.yr.filt, by = 4)) +
    # scale_shape_manual(values = c(1,2)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(legend.position = "none") 
      
  
  i <- i + 6
  j <- j + 6
}

length(out.plot)
# Plot to PDF file
pdf(paste(plot.dir, site, "_", seas, "_", name, "_IndexPlot.pdf", sep=""),
    height = 10, width = 8, paper = "letter")
try(print(out.plot[[1]], silent=T))
try(print(out.plot[[2]], silent=T))
try(print(out.plot[[3]], silent=T))
try(print(out.plot[[4]], silent=T))
try(print(out.plot[[5]], silent=T))
try(print(out.plot[[6]], silent=T))
try(print(out.plot[[7]], silent=T))
try(print(out.plot[[8]], silent=T))
try(print(out.plot[[9]], silent=T))
try(print(out.plot[[10]], silent=T))
try(print(out.plot[[11]], silent=T))
try(print(out.plot[[12]], silent=T))
try(print(out.plot[[13]], silent=T))
try(print(out.plot[[14]], silent=T))
try(print(out.plot[[15]], silent=T))
try(print(out.plot[[16]], silent=T))
try(print(out.plot[[17]], silent=T))
try(print(out.plot[[18]], silent=T))
try(print(out.plot[[19]], silent=T))
try(print(out.plot[[20]], silent=T))
try(print(out.plot[[21]], silent=T))
try(print(out.plot[[22]], silent=T))
try(print(out.plot[[23]], silent=T))
try(print(out.plot[[24]], silent=T))
try(print(out.plot[[25]], silent=T))
try(print(out.plot[[26]], silent=T))
try(print(out.plot[[27]], silent=T))
try(print(out.plot[[28]], silent=T))
try(print(out.plot[[29]], silent=T))


while(!is.null(dev.list())) dev.off()

