##Combine data outputs into one table for Catherine's uploads

Trends <- list.files(path = "C:/Users/ethie/Documents/ethier-scripts/RPI-Trends/Output/2023/",  # Identify all CSV files
                     pattern = "*TrendsSlope.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 

#remove row with only NA
n<-nrow(Trends)
Trends<-Trends[2:n,]
#Trends<-Trends %>% drop_na(results_code)

write.csv(Trends, "Output/2023/AllTrendsRPI.csv", row.names = FALSE)



Indices <- list.files(path = "C:/Users/ethie/Documents/ethier-scripts/RPI-Trends/Output/2023/",  # Identify all CSV files
                      pattern = "*AnnualIndices.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 


m<-nrow(Indices)
Indices<-Indices[2:m,]
#Indices<-Indices %>% drop_na(results_code)

write.csv(Indices, "Output/2023/AllIndicesRPI.csv", row.names = FALSE)
