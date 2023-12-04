##Combine data outputs into one table for Catherine's uploads


Trends <- list.files(path = "C:/Users/dethier/Documents/ethier-scripts/RPI-Trends/Output/2022/",  # Identify all CSV files
                     pattern = "*TrendsSlope.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data_all 


#remove row with only NA
n<-nrow(Trends)
Trends<-Trends[2:n,]


write.csv(Trends, "Output/2022/AllTrendsRPI.csv")



Indices <- list.files(path = "C:/Users/dethier/Documents/ethier-scripts/RPI-Trends/Output/2022/",  # Identify all CSV files
                      pattern = "*AnnualIndices.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data_all

m<-nrow(Indices)
Indices<-Indices[2:m,]

write.csv(Indices, "Output/2022/AllIndicesRPI.csv")
