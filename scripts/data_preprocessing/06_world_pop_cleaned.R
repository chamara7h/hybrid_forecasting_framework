## population data extracting from world pop raster images

# install packages

libs <- c(
  "tidyverse", "terra",
  "giscoR", "sf", "ggmap",
  "rayshader", "geosphere"
)

installed_libs <- libs %in% rownames(
  installed.packages()
)

if(any(installed_libs == F)){
  install.packages(
    libs[!installed_libs]
  )
}

invisible(lapply(
  libs, library, character.only =T
))


# set folder path

folder_path <- "data/world_pop/temp"


# read all the raster files names as a list

file_list <- list.files(path = folder_path, full.names = TRUE)


# select the relevant age groups

filtered_file_list <- file_list[grepl("civ_f_20|civ_f_25|civ_f_30|civ_f_35|civ_f_40|civ_f_45|civ_f_50", file_list)]


# read data

pop_raster <- raster::raster(filtered_file_list[1])

site_df <- read.csv('data/service_delivery_site_data.csv')

buffer_df <- read.csv('data/tidy_data/buffer_df.csv')

site_buffer <- site_df %>% 
    left_join(buffer_df %>% select(Row_Site, buffer_size) %>% rename(site_code = Row_Site), by = 'site_code')


# create geo codes for sites

points <- site_df %>% 
    select(site_latitude, site_longitude)


# Convert points to an sf object

points_sf <- st_as_sf(points, coords = c("site_longitude", "site_latitude"), crs = 4326)


# Create 1 km, 2 km, 3 km, 4 km, 5 km and 10 km, 15 km and 20 km buffers around points

buffers_1 <- st_buffer(points_sf, dist = 1000)

buffers_2 <- st_buffer(points_sf, dist = 2000)

buffers_3 <- st_buffer(points_sf, dist = 3000)

buffers_4 <- st_buffer(points_sf, dist = 4000)

buffers_5 <- st_buffer(points_sf, dist = 5000)

buffers_10 <- st_buffer(points_sf, dist = 10000)

buffers_15 <- st_buffer(points_sf, dist = 15000)

buffers_20 <- st_buffer(points_sf, dist = 20000)


# Extract population values within buffers

pop_within_1_buffers <- extract(pop_raster, buffers_1)

pop_within_2_buffers <- extract(pop_raster, buffers_2)

pop_within_3_buffers <- extract(pop_raster, buffers_3)

pop_within_4_buffers <- extract(pop_raster, buffers_4)

pop_within_5_buffers <- extract(pop_raster, buffers_5)

pop_within_10_buffers <- extract(pop_raster, buffers_10)

pop_within_15_buffers <- extract(pop_raster, buffers_15)

pop_within_20_buffers <- extract(pop_raster, buffers_20)


# Calculate total population within each buffer

total_pop_within_1_buffers <- sapply(pop_within_1_buffers, sum, na.rm = TRUE)

total_pop_within_2_buffers <- sapply(pop_within_2_buffers, sum, na.rm = TRUE)

total_pop_within_3_buffers <- sapply(pop_within_3_buffers, sum, na.rm = TRUE)

total_pop_within_4_buffers <- sapply(pop_within_4_buffers, sum, na.rm = TRUE)

total_pop_within_5_buffers <- sapply(pop_within_5_buffers, sum, na.rm = TRUE)

total_pop_within_10_buffers <- sapply(pop_within_10_buffers, sum, na.rm = TRUE)

total_pop_within_15_buffers <- sapply(pop_within_15_buffers, sum, na.rm = TRUE)

total_pop_within_20_buffers <- sapply(pop_within_20_buffers, sum, na.rm = TRUE)


# Create the final data frame

# new <- c(paste0(names(pop_raster), "_5km"), paste0(names(pop_raster), "_10km"))
# old <- c('km_5', 'km_10')

df_pop_1 <- data.frame(
  site_longitude = points$site_longitude,
  site_latitude = points$site_latitude,
  age = names(pop_raster),
  population = total_pop_within_1_buffers,
  buffer_size = 1
)

df_pop_2 <- data.frame(
  site_longitude = points$site_longitude,
  site_latitude = points$site_latitude,
  age = names(pop_raster),
  population = total_pop_within_2_buffers,
  buffer_size = 2
)

df_pop_3 <- data.frame(
  site_longitude = points$site_longitude,
  site_latitude = points$site_latitude,
  age = names(pop_raster),
  population = total_pop_within_3_buffers,
  buffer_size = 3
)

df_pop_4 <- data.frame(
  site_longitude = points$site_longitude,
  site_latitude = points$site_latitude,
  age = names(pop_raster),
  population = total_pop_within_4_buffers,
  buffer_size = 4
)

df_pop_5 <- data.frame(
  site_longitude = points$site_longitude,
  site_latitude = points$site_latitude,
  age = names(pop_raster),
  population = total_pop_within_5_buffers,
  buffer_size = 5
)

df_pop_10 <- data.frame(
  site_longitude = points$site_longitude,
  site_latitude = points$site_latitude,
  age = names(pop_raster),
  population = total_pop_within_10_buffers,
  buffer_size = 10
)

df_pop_15 <- data.frame(
  site_longitude = points$site_longitude,
  site_latitude = points$site_latitude,
  age = names(pop_raster),
  population = total_pop_within_15_buffers,
  buffer_size = 15
)

df_pop_20 <- data.frame(
  site_longitude = points$site_longitude,
  site_latitude = points$site_latitude,
  age = names(pop_raster),
  population = total_pop_within_20_buffers,
  buffer_size = 20
)


df_pop <- df_pop_1 %>% 
    bind_rows(df_pop_2, df_pop_3, df_pop_4, df_pop_5, df_pop_10, df_pop_15, df_pop_20)

site_pop <- site_buffer %>% left_join(df_pop, by = c('site_latitude', 'site_longitude', 'buffer_size'))


# df_pop <- df_pop %>% 
#   
#   rename_with(~ new, all_of(old))


## creating full pop data frame

for (n in 2:length(filtered_file_list)) {
  
  
  pop_raster <- raster::raster(filtered_file_list[n])
  
  # Extract population values within buffers
  
  pop_within_1_buffers <- extract(pop_raster, buffers_1)
  
  pop_within_2_buffers <- extract(pop_raster, buffers_2)
  
  pop_within_3_buffers <- extract(pop_raster, buffers_3)
  
  pop_within_4_buffers <- extract(pop_raster, buffers_4)
  
  pop_within_5_buffers <- extract(pop_raster, buffers_5)
  
  pop_within_10_buffers <- extract(pop_raster, buffers_10)
  
  pop_within_15_buffers <- extract(pop_raster, buffers_15)
  
  pop_within_20_buffers <- extract(pop_raster, buffers_20)
  
  
  # Calculate total population within each buffer
  
  total_pop_within_1_buffers <- sapply(pop_within_1_buffers, sum, na.rm = TRUE)
  
  total_pop_within_2_buffers <- sapply(pop_within_2_buffers, sum, na.rm = TRUE)
  
  total_pop_within_3_buffers <- sapply(pop_within_3_buffers, sum, na.rm = TRUE)
  
  total_pop_within_4_buffers <- sapply(pop_within_4_buffers, sum, na.rm = TRUE)
  
  total_pop_within_5_buffers <- sapply(pop_within_5_buffers, sum, na.rm = TRUE)
  
  total_pop_within_10_buffers <- sapply(pop_within_10_buffers, sum, na.rm = TRUE)
  
  total_pop_within_15_buffers <- sapply(pop_within_15_buffers, sum, na.rm = TRUE)
  
  total_pop_within_20_buffers <- sapply(pop_within_20_buffers, sum, na.rm = TRUE)
  
  
  # Create the final data frame
  
  df_pop_1 <- data.frame(
    site_longitude = points$site_longitude,
    site_latitude = points$site_latitude,
    age = names(pop_raster),
    population = total_pop_within_1_buffers,
    buffer_size = 1
  )
  
  df_pop_2 <- data.frame(
    site_longitude = points$site_longitude,
    site_latitude = points$site_latitude,
    age = names(pop_raster),
    population = total_pop_within_2_buffers,
    buffer_size = 2
  )
  
  df_pop_3 <- data.frame(
    site_longitude = points$site_longitude,
    site_latitude = points$site_latitude,
    age = names(pop_raster),
    population = total_pop_within_3_buffers,
    buffer_size = 3
  )
  
  df_pop_4 <- data.frame(
    site_longitude = points$site_longitude,
    site_latitude = points$site_latitude,
    age = names(pop_raster),
    population = total_pop_within_4_buffers,
    buffer_size = 4
  )
  
  df_pop_5 <- data.frame(
    site_longitude = points$site_longitude,
    site_latitude = points$site_latitude,
    age = names(pop_raster),
    population = total_pop_within_5_buffers,
    buffer_size = 5
  )
  
  df_pop_10 <- data.frame(
    site_longitude = points$site_longitude,
    site_latitude = points$site_latitude,
    age = names(pop_raster),
    population = total_pop_within_10_buffers,
    buffer_size = 10
  )
  
  df_pop_15 <- data.frame(
    site_longitude = points$site_longitude,
    site_latitude = points$site_latitude,
    age = names(pop_raster),
    population = total_pop_within_15_buffers,
    buffer_size = 15
  )
  
  df_pop_20 <- data.frame(
    site_longitude = points$site_longitude,
    site_latitude = points$site_latitude,
    age = names(pop_raster),
    population = total_pop_within_20_buffers,
    buffer_size = 20
  )
  
  
  df_pop <- df_pop_1 %>% 
    
    bind_rows(df_pop_2, df_pop_3, df_pop_4, df_pop_5, df_pop_10, df_pop_15, df_pop_20)
  
  site_pop_temp <- site_buffer %>% left_join(df_pop, by = c('site_latitude', 'site_longitude', 'buffer_size'))
  
  site_pop <- site_pop %>% 
    
    bind_rows(site_pop_temp)
  
  print(names(pop_raster))
  
}



# make columns for each age group for both 5km and 10km buffer

site_pop_wide <- site_pop %>% 
  
  mutate(
    year = as.numeric(str_extract(age, "\\d{4}")),  # Extract the year
    age_group = as.numeric(str_extract(age, "\\d{2}"))  # Extract the age group
  )  %>% 
  
  pivot_wider(id_cols = c("year", "site_code", "site_type", "site_region", "site_district", "site_latitude", "site_longitude"),
              names_from = age_group, values_from = population) %>% 
  
  rename("age_20" = "20", "age_25" = "25",
         "age_30" = "30", "age_35" = "35",
         "age_40" = "40", "age_45" = "45",
         "age_50" = "50")


write_rds(site_pop_wide, "data/tidy_data/pop_tidy.rds")






# 
# # raster to df
# 
# pop_temp <- pop_raster %>% 
#   as.data.frame(xy = T)
# 
# head(pop_temp)
# 
# 
# spdf <- st_as_sf(pop_temp, coords = c("x", "y"), crs = 4326)
# 
# buffer_5km <- st_buffer(spdf, dist = 5000)  # 5 km buffer
# 
# 
# 
# for (n in 2:length(filtered_file_list)) {
#   
#   pop_raster <- terra::rast(filtered_file_list[n])
#   
#   pop_temp_1 <- pop_raster %>% 
#     as.data.frame(xy = F)
#   
#   pop_temp <- pop_temp %>% 
#     
#     bind_cols(pop_temp_1)
#   
#   print(names(pop_raster))
# }
# 
# 
# write_rds(pop_temp, 'data/pop_temp.rds')









