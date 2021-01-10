## test a map-based shiny app using leaflet
## ETL data for the app
## August-September 2019
## Dave

rm(list = ls())

library(geojsonio)
library(readxl)
library(tidyverse)
library(htmltools)
library(ggmap)
library(rgeos)
library(scales)

setwd("C:/Users/davem/analysis/shiny-test")


## districts ####
districts <- geojson_read("raw/or_districts_simple.json", what = "sp",stringsAsFactors = F)
districts.df <- as.data.frame(districts) %>%
  select(district_name = NAME,
         nces_id = GEOID)
crosswalk <- read_excel("raw/ncesdata_7A5CAA65.xls") %>%
  select(nces_id = `NCES District ID`,
         state_id = `State District ID`) %>%
  mutate(state_id = as.integer(gsub("OR-0000000000","",state_id)))
info <- read.csv("raw/RCmediaDistrictsAggregate.csv", stringsAsFactors = F) %>%
  select(state_id = District.ID,
         el = Ever.English.Learners,
         sped = Students.with.Disabilities,
         frl = Free.Reduced.Priced.Lunch)

info <- data.frame(sapply(info, function(x) as.numeric(gsub("%", "", x)))) %>%
  mutate(el_pctile = percent_rank(el),
         sped_pctile = percent_rank(sped),
         frl_pctile = percent_rank(frl))
info <- left_join(info,crosswalk, by = "state_id") %>%
  left_join(., districts.df, by = "nces_id")

districts_info <- sp::merge(districts, info, by.x = "GEOID", by.y = "nces_id")

saveRDS(districts_info,"data/districts_info.rds")

## schools ####
# raw_schools <- read.csv("raw/open_institutions.csv", stringsAsFactors = F)
# schools <- raw_schools %>%
#   filter(Inst.Type == "Oregon Public School") %>%
#   select(school_id = Institution_ID,
#          street = Street_StrAddr1,
#          city = Street_City,
#          state = Street_State,
#          zip = Street_Zip) %>%
#   mutate(address = paste0(street,", ",city,", ",state, " ", zip))

# register_google(key = <add key here>, account_type = "premium", day_limit = 100000)
# schools_geocode <- schools %>%
#   mutate_geocode(address)  %>%
    # mutate(lat = ifelse(school_id == 304, 42.954380,lat),
    #    lon = ifelse(school_id == 304, -123.361973, lon),
    #    lat = ifelse(school_id == 201, 43.360771,lat),
    #    lon = ifelse(school_id == 201, -124.221274, lon),
    #    lat = ifelse(school_id == 1079, 45.462260,lat),
    #    lon = ifelse(school_id == 1079, -117.965223, lon))

# saveRDS(schools_geocode, "raw/schools_geocoded.rds")

schools_geocode <- read_rds("raw/schools_geocoded.rds")

raw_rc <- read.csv("raw/RCmediaSchoolsAggregate.csv", stringsAsFactors = F)
rc <- raw_rc %>%
  select(school_id = School.ID,
         school_name = School.Name,
         enroll = Student.Enrollment,
         reg_attend = Regular.Attenders,
         ela = English.Language.Arts,
         math = Mathematics) %>%
  mutate_all(funs(gsub("%", "", .))) %>%
  mutate_at(vars(school_id, enroll, reg_attend, ela, math), as.numeric)

schools <- rc %>%
  left_join(., schools_geocode, by = "school_id") %>%
  filter(!is.na(lat)) %>%
  distinct() %>%
  mutate(reg_attend_pctile = percent_rank(reg_attend),
         ela_pctile = percent_rank(ela),
         math_pctile = percent_rank(math),
         enroll_scaled = rescale(enroll, to = c(2,12))) #scale enroll btwn 0-10

coordinates(schools) = ~lon + lat
saveRDS(schools, "data/schools.rds")
write.csv(schools, "raw/school.csv", row.names = F)
