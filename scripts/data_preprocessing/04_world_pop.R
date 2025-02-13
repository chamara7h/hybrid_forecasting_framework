suppressPackageStartupMessages({
  library(httr)
  library(rvest)
  library(dplyr)
  library(stringr)
  library(raster)
})

# big files need greater timeout values,

old_timeout <- options(timeout = 300)
getOption("timeout")


# creating web scrapping loop

link <- "https://hub.worldpop.org/geodata/summary?id=15575"

page <- read_html(link)

files_urls <- page %>%
  html_elements("a") %>%
  html_attr("href") %>%
  data.frame(filename = .)

link <- c("https://hub.worldpop.org/geodata/summary?id=15824", "https://hub.worldpop.org/geodata/summary?id=16073",
          "https://hub.worldpop.org/geodata/summary?id=16322", "https://hub.worldpop.org/geodata/summary?id=16571",
          "https://hub.worldpop.org/geodata/summary?id=16820")

for (l in link) {
    page <- read_html(l)
    files_urls <- files_urls %>% bind_rows(page %>% 
                                             html_elements("a") %>%
                                             html_attr("href") %>%
                                             data.frame(filename = .))
}


# selecting the relavant urls

url_section <- c("https://data.worldpop.org/GIS/AgeSex_structures/Global_2000_2020/")

wanted_urls <- files_urls %>%
  filter(grepl(url_section, filename))


# download files

Map(download.file, wanted_urls$filename, file.path("data/world_pop/temp", destfile = basename(wanted_urls$filename)), mode="wb")


# read raster files

pop <- raster("data/world_pop/temp/civ_f_0_2015.tif")
x <- rasterToPoints(pop)

