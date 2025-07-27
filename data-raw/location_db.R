# create a location database
# source: https://www.geonames.org/
# database documentation: https://download.geonames.org/export/dump/readme.txt
# TW cities here: https://api.opencube.tw/twzipcode
# Populations on wiki: https://en.wikipedia.org/wiki/List_of_cities_in_Taiwan
library(dplyr)
dt <- readr::read_csv("./data-raw/tw_zipcode.csv")[,-1] %>%
  filter(lat != 0) %>%
  select(city, lat,lng) %>%
  group_by(city) %>%
  mutate(pos=seq_along(city),
         mean_lat=mean(lat),
         mean_lng=mean(lng),
         diff_lat=lat-mean(lat),
         diff_lng=lng-mean(lng),
         total_diff=diff_lat+diff_lng,
         min_diff=min(total_diff)) %>%
  filter(total_diff==min_diff) %>%
  ungroup() %>%
  mutate(tz="Asia/Taipei",
         country="Taiwan") %>%
  select(country, city, lat, lng, tz) %>%
  mutate(city=paste(city, paste("lng:",round(lng,1)), paste("lat:", round(lat,1)), sep=", ")) %>%
  select(country, city, tz)

dt$population <- c(2509897, 
                   362354, 
                   14701, 
                   
                   4039946, 
                   449963, 
                   456116, 
                   588906,
                  
                   2315727,
                   534677,
                   2844250,
                   1239614,
                   477326,
                   263596,
                   484892, 
                   659841,
                   1859776,
                   2737608,
                   107761,
                   144080,
                   795417,
                   211550,
                   317643)

col_names <- c("geonameid",
               "name",
               "asciiname",
               "alternatenames",
               "latitude",
               "longitude",
               "feature_class",
               "feature_code",
               "country_code",
               "cc2",
               "admin1_code",
               "admin2_code",
               "admin3_code",
               "admin4_code",
               "population",
               "elevation",
               "dem",
               "timezone",
               "modification_date")

cities1000 <- readr::read_delim("data-raw/cities1000.txt", 
                         delim = "\t", escape_double = FALSE, 
                         col_names = col_names, trim_ws = TRUE)
cities1000 <- 
  cities1000 %>% 
  dplyr::select(name, alternatenames, admin1_code, admin2_code, latitude, longitude, country_code, population, timezone)

countryInfo <- readr::read_delim("data-raw/countryInfo.txt", 
                               delim = "\t", escape_double = FALSE, 
                               trim_ws = TRUE, skip = 49)

# admin1CodesASCII <- readr::read_delim("deploy/admin1CodesASCII.txt", 
#                                delim = "\t", escape_double = FALSE, 
#                                col_names = c("admin1_code", "admin1_name", "admin1_name_ascii", "geonameid"), 
#                                trim_ws = TRUE)
# 
# admin1CodesASCII %>% dplyr::select(-geonameid, -admin1_name_ascii) %>% 
#   dplyr::right_join(cities1000, by="admin1_code")

country <- countryInfo %>% dplyr::rename(country_code=`#ISO`,
                              pop_country=Population) %>% 
  filter(!is.na(country_code)) %>%
  dplyr::select(country_code, Country, pop_country) %>%
  arrange(desc(pop_country)) %>%
  mutate(pop_ranking=seq_along(country_code))

cities1000 <- cities1000 %>% left_join(country, by="country_code")
names(cities1000)
cities1000 <- cities1000 %>%
  filter(country_code != "TW")

cities1000 <- 
  cities1000 %>%
  mutate(country=Country,
         city=paste(name, paste("lng:", round(longitude, 1)), paste("lat:", round(latitude,1)), sep=", ")) %>%
  select(country, city, tz=timezone, population)

cities <- rbind(dt, cities1000)
cities$big_city <- cities$population > 300000

#country_en <- unique(cities$country)
#country_en [1] <- "Taiwan"
#country_en_trans <- paste(country_en, "|", country_en, sep="")
#country_cn <- paste(country_en, unique(cities$country), sep="|")

#cat(country_en_trans, file="./inst/csv/translation_en.csv", sep="\n", append = TRUE)
#cat(country_en_trans, file="./inst/csv/translation_de.csv", sep="\n", append = TRUE)
#cat(country_cn, file="./inst/csv/translation_zh.csv", sep="\n", append = TRUE)

usethis::use_data(cities, overwrite = TRUE)
