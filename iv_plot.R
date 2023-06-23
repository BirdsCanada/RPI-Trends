# PLOT ANNUAL INDICES AND TRENDS
# Loop through sites and seasons and produce separate PDF plot
# function to call in various analysis functions
  
require(tidyverse)
require(ggplot2)
require(naturecounts)


in.dir <- paste("Output/", max.year, "/", sep = "")
out.dir <- paste("Plots/", max.year, "/", sep = "")
site <- as.character(anal.param[t, "site"])

#add species English and French name
sp.name<-meta_species_taxonomy()
sp.name<-sp.name %>% select(species_id, english_name, french_name)

# read in trend output

trnd <- read.csv(paste(in.dir, site, "_Trends.csv", sep = ""))
trnd<- left_join(trnd, sp.name, by="species_id")

trnd <- trnd %>%
  filter(!(is.na(species_code)) & period == "all years" & index_type =="endpoint")

trnd.spr <- trnd %>%
  filter(season == "Spring") %>%
  mutate(trnd.spr = trnd,
         lower_ci_trend.spr = lower_ci,
         upper_ci_trend.spr = upper_ci,
         anal_code.spr = "X") %>%
  select(species_code, english_name, french_name, area_code, trnd.spr, lower_ci_trend.spr, upper_ci_trend.spr, index_type, anal_code.spr)


trnd.fall <- trnd %>%
  filter(season == "Fall") %>%
  mutate(trnd.fall = trnd,
         lower_ci_trend.fall = lower_ci,
         upper_ci_trend.fall = upper_ci, 
         anal_code.fall = "Y") %>%
  select(species_code, english_name, french_name, area_code, trnd.fall, lower_ci_trend.fall, upper_ci_trend.fall, index_type, anal_code.fall)

options(digits = 2)

sp.trnd <- full_join(trnd.spr, trnd.fall, by = c("species_code", "area_code", "english_name", "french_name", "index_type"), multiple="all") 


sp.trnd <- sp.trnd %>%
  mutate(sp.trnd = if_else((!is.na(trnd.spr) & !is.na(trnd.fall)), 
                           paste(english_name, "/", " \n", french_name, "\n Spring (", anal_code.fall, "): ", round(trnd.spr, digits = 2),  
                                 " (", round(lower_ci_trend.spr, digits = 2), ", ",
                                 round(upper_ci_trend.spr, digits = 2), ")\n Fall (", anal_code.fall, ") : ", 
                                 round( trnd.fall, digits = 2),  " (",
                                 round( lower_ci_trend.fall, digits = 2), ", ",
                                 round( upper_ci_trend.fall, digits = 2),  ")", sep = ""), 
                           if_else(
                             is.na( trnd.spr), 
                             paste( english_name, "/", "\n",  french_name, "\n Fall (", anal_code.fall, ") : ", 
                                    round( trnd.fall, digits = 2),  " (", round( lower_ci_trend.fall, digits = 2), ", ",
                                    round( upper_ci_trend.fall, digits = 2), ")", sep = ""), 
                             if_else(
                               is.na( trnd.fall),
                               paste( english_name, "/", "\n",  french_name, "\n Spring (", anal_code.spr, ") : ", 
                                      round( trnd.spr, digits = 2),  " (", round( lower_ci_trend.spr, digits = 2), ", ", 
                                      round( upper_ci_trend.spr, digits = 2), ")", sep = ""), "NA"))))

# read in annual index output

index <- read.csv(paste(in.dir, site, "_AnnualIndices.csv", sep = ""))

sp.name<-meta_species_taxonomy()
sp.name<-sp.name %>% select(species_id, english_name, french_name, order_taxon)
index<- left_join(index, sp.name, by="species_id")

index <- index %>%
  filter(!is.na(species_code) & period == "all years") %>%
  dplyr::select(index, lower_ci, upper_ci,
                species_code, year, season, area_code, species_id, meanObs, 
                english_name, french_name, order_taxon)

plot.dat <- full_join(index, sp.trnd, by = c("area_code", "species_code", "english_name", "french_name"), multiple="all")
plot.dat <- plot.dat[order(plot.dat$order_taxon),]
plot.dat$species_code<-as.factor(plot.dat$species_code)

plot.dat<-plot.dat %>% subset(!is.na(index))

# following is so that order of plots is correct
plot.dat$sp.trnd <- factor(plot.dat$sp.trnd, levels=unique(plot.dat$sp.trnd))
plot.dat <- subset(plot.dat, !is.na(year))
plot.dat<-subset (plot.dat, !is.na(sp.trnd))

sp.list <- as.character(unique(plot.dat$species_code))

out.plot <- NULL
i <- 1
j <- 6

for(m in 1:(ceiling(length(sp.list)/6))) {
  
  out.plot[[m]] <- ggplot(data = subset(plot.dat, species_code %in% sp.list[i:j]), aes(x = as.numeric(year), y = index, colour = season, shape = season)) +
    facet_wrap(~ sp.trnd, ncol = 2, scales = "free", as.table = TRUE) +
    geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci, group = season, shape = season, width =1), size = 0.4) +
    geom_smooth(aes(ymin = lower_ci, ymax = upper_ci, group = season, colour = season, fill = season, linetype = season), method = "loess",	size = 0.5, alpha = 0.1) + 
    xlab("Year") +
    ylab("Annual Index") +
  #  scale_x_continuous(breaks = seq(from = min.yr.filt, to = max.yr.filt, by = 4)) +
    scale_shape_manual(values = c(1,2)) +
    scale_y_continuous(trans='log10') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(legend.position = "none")+
    theme_classic()
  
  #To plot bt indiex values
  #out.plot[[t]] <- ggplot(data = subset(plot.dat, species_code %in% sp.list[i:j]), aes(x = as.numeric(year), y = index.bt, colour = season, shape = season)) +
  #facet_wrap(~ sp.trnd, ncol = 2, scales = "free", as.table = TRUE) +
  #geom_pointrange(aes(ymin = lower_ci.bt, ymax = upper_ci.bt, group = season, shape = season, width =1), size = 0.4) +
  #geom_smooth(aes(ymin = lower_ci.bt, ymax = upper_ci.bt, group = season, colour = season, fill = season, linetype = season), method = "loess",
  #size = 0.5, alpha = 0.1) + #linetype = "blank", 
  #theme_bw() +
  #xlab("Year") +
  #ylab("Annual Index") +
  #scale_y_continuous(trans="log10") +
  #scale_x_continuous(breaks = seq(from = min.yr.filt, to = max.yr.filt, by = 4)) +
  #scale_shape_manual(values = c(1,2)) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #theme(legend.position = "none")
  
  i <- i + 6
  j <- j + 6
}

length(out.plot)
# Plot to PDF file
pdf(paste(out.dir, site, ".IndexPlot.pdf", sep=""),
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

