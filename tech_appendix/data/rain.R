library(data.table)
# raw files too large ( > 1GB )
raw <- fread("../../../../Downloads/2020.csv", header = FALSE, sep = ",")
countries <- read.csv("../../../../Downloads/ghcnd-countries.csv", header = FALSE, sep = ",")

names(raw) <- c("id", "date", "attribute", "value", "valueflag", "qualityflag", "sourceflag", "obs")
names(countries) <- c("id", "country")

library(dplyr)
library(stringr)
library(lubridate)
library(countrycode)

raw <- raw %>% 
  filter(attribute == "PRCP", valueflag == "") %>%
  mutate(
    id = str_sub(id, 1, 2),
    date = ymd(date),
    
  ) %>%
  select(id, date, value)

raw <- left_join(raw,countries)

raw <- raw %>%
  mutate(
    country = countrycode(
      country,
      "country.name",
      "country.name"
    )
  ) %>%
  group_by(country, date) %>%
  mutate(
    avgrain = mean(value, na.rm = TRUE)
  ) %>%
  distinct(country, date, avgrain)

fwrite(raw, "./processed/rain.csv")