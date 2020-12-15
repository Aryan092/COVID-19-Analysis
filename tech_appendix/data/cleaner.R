library(dplyr)
library(rio)

# raw data import
setwd(".")
tc <- readRDS("./data/raw/tidycovid19-20-11-2020.RDS")
Countries <- import("./data/raw/country-continent.xlsx", setclass = "tibble")
OxCGRT <- read.csv("./data/raw/OxCGRT-24-11-2020.csv")

# OxCGRT 
attach(OxCGRT)
# country standardisation
library(countrycode)
OxCGRT$CountryName <- countrycode(OxCGRT$CountryCode, 
                                  origin = "iso3c", 
                                  destination = "country.name") 
# gave warning about "RKS" not being converted
OxCGRT$CountryName[CountryCode == "RKS"] <- "Kosovo"
# date standardisation
library(lubridate)
OxCGRT$Date <- ymd(OxCGRT$Date)
detach(OxCGRT)

# tidycovid19 
# country standardisation
tc$country <- countrycode(tc$iso3c, 
                                   origin = "iso3c", 
                                   destination = "country.name")
Countries$country <- countrycode(Countries$country, 
                                 origin = "country.name", 
                                 destination = "country.name") 
# gave warning about "Micronesia" not being converted
Countries$country[Countries$country == "Micronesia"] <- "Federated States of Micronesia"

# date standardisation
tc$date <- as.Date(parse_date_time(tc$date,orders=c("y","ym","ymd")))

tc <- left_join(tc, Countries, by = "country")

# adding missing continent/region info
tc$continent[tc$country == "Anguilla"] <- "North America"
tc$region[tc$country == "Anguilla"] <- "Latin America & Caribbean"
#---#
tc$continent[tc$country == "Caribbean Netherlands"] <- "North America"
tc$region[tc$country == "Caribbean Netherlands"] <- "Latin America & Caribbean"
#---#
tc$continent[tc$country == "Falkland Islands"] <- "South America"
tc$region[tc$country == "Falkland Islands"] <- "Latin America & Caribbean"
#---#
tc$continent[tc$country == "Guernsey"] <- "Europe"
tc$region[tc$country == "Guernsey"] <- "Europe & Central Asia"
#---#
tc$continent[tc$country == "Jersey"] <- "Europe"
tc$region[tc$country == "Jersey"] <- "Europe & Central Asia"

# export
tidy_oxford <- merge(tc, OxCGRT, by.x = c("country", "date"), by.y = c("CountryName", "Date"))

# europe & america subset
# by continent
library(stringr)
EuropeAmericaData <- {tidy_oxford %>% 
    filter(((is.na(continent) | continent != "Asia") & 
              (str_detect(continent, "America") | 
                 str_detect(continent, "Europe") | 
                 str_detect(region, "America") | 
                 str_detect(region, "Europe"))))
}

Countries <- filter(Countries, 
                    country %in% EuropeAmericaData$country)
EuropeAmericaData <- mutate(EuropeAmericaData, 
                            country = factor(country), 
                            continent = factor(continent))

# handle duplicate days due to same-day reporting of updated indexes
EuropeAmericaData <- EuropeAmericaData %>% mutate(indexsum = StringencyIndex + GovernmentResponseIndex + ContainmentHealthIndex + EconomicSupportIndex)

EuropeAmerica_nodupes <- EuropeAmericaData %>% group_by(country, date) %>% slice(which.max(indexsum))

#export
write.csv(EuropeAmerica_nodupes, "./data/processed/europe_americas.csv")
