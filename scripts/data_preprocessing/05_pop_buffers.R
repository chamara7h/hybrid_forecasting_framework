## calculate distance between sites

library(tidyverse)
library(geosphere)


# read data

site_df <- read.csv('data/service_delivery_site_data.csv')


# Calculate pairwise distances using haversine formula

dist_matrix <- distm(
  cbind(site_df$site_latitude, site_df$site_longitude),
  fun = distHaversine
)


# Set row and column names for clarity

rownames(dist_matrix) <- site_df$site_code
colnames(dist_matrix) <- site_df$site_code


# Initialize an empty data frame to store results

result_df <- data.frame(
  Row_Site = as.character(0),
  Paired_Site = as.character(0),
  Minimum_Distance = as.numeric(0)
)


# Loop through each row

for (i in 1:nrow(dist_matrix)) {
  # Exclude diagonal element (self-comparison)
  min_dist <- min(dist_matrix[i, -i])
  # Find the column index of the minimum distance
  min_col <- which(dist_matrix[i, -i] == min_dist)
  # Get the paired site name
  paired_site <- colnames(dist_matrix)[min_col]
  # Append to the result data frame
  result_df <- rbind(result_df, c(rownames(dist_matrix)[i], paired_site, min_dist))
}


result_df <- result_df %>% slice(-1) %>% mutate(Minimum_Distance = as.numeric(Minimum_Distance))

# calculate the buffer size for each site

buffer_df <- result_df %>% 
  
  mutate(buffer_size = as.numeric(Minimum_Distance/2)/1000, 
         
         buffer_size = case_when(
           buffer_size <= 1 ~ 1,
           buffer_size > 15 ~ 20,
           buffer_size > 1 & buffer_size <= 2 ~ 2,
           buffer_size > 2 & buffer_size <= 3 ~ 3,
           buffer_size > 3 & buffer_size <= 4 ~ 4,
           buffer_size > 4 & buffer_size <= 5 ~ 5,
           buffer_size > 5 & buffer_size <= 10 ~ 10,
           buffer_size > 10 & buffer_size <= 15 ~ 15
         )
         
         )

write.csv(buffer_df, 'data/tidy_data/buffer_df.csv')
