
###################################################################################
#Preprocess mortality data into clean time series
###################################################################################


#Load libraries and functions
source("~/Library/CloudStorage/OneDrive-UniversitaetBern/my projects/team_project/team_project_june_26/01_exposure_response_functions/mortality/00.pkg.R")

#Define directories
deathdir <- "/Volumes/FS/_ISPM/CCH/01Data/Mortality_CH/"
savedir <- '/Users/jv24t611/Library/CloudStorage/OneDrive-UniversitaetBern/my projects/causaltemp/data/processed/'
shpdir <- '/Users/jv24t611/Library/CloudStorage/OneDrive-UniversitaetBern/my projects/causaltemp/data/orig/shapefiles/'


#Load mortality data
death1 <- fread(paste0(deathdir,"/mortality_CH_1969-2018/mort_6918.csv"))
death2 <- fread(paste0(deathdir,"/mortality_CH_2017_2024/Datenlieferung_UNIBE_Vicedo_TU 2024_20260105.csv"))
death3 <- fread(paste0(deathdir,"/mortality_CH_2017_2024/Datenlieferung_UNIBE_Vicedo_TU ab 2017_20251118.csv"))

#Select municipalities fields
#shpswiss <- shpswiss[shpswiss$OBJEKTART =="Gemeindegebiet", c("NAME","BFS_NUMMER") ]

#There are no significant  differences in the MUNICIPALITY codes between the mortality data and the shapefile.
#The only differences are that in the shape file there are some municipalities that do not belong to Switzerland anymore.
# unique(swdeath[swdeath$comm_resi %in% setdiff(swdeath$comm_resi, shpswiss$BFS_NUMMER)]$comm_resi)
# unique(shpswiss[shpswiss$BFS_NUMMER%in% setdiff(shpswiss$BFS_NUMMER, swdeath$comm_resi)]$BFS_NUMMER)
# shpswiss[!(shpswiss$BFS_NUMMER %in% swdeath$comm_resi),]$NAME

#Subset relevant columns
death1 <- death1[,c("comm_resi","dod")]

#Rename columns
setnames(death, new = c("muncode","date"), old = c("comm_resi","dod"))

#Define municipalities names for switzerland
shpswiss_df <- as.data.frame(shpswiss)[,c("BFS_NUMMER","NAME")]
setnames(shpswiss_df, new = c("muncode","muname"), old = c("BFS_NUMMER","NAME"))
death <- merge(death, shpswiss_df, by = "muncode", all.x = TRUE)

#Reformat dates
death[, date := as.IDate(date, format = "%d%b%Y")][, date := as.Date(date, format = "%Y-%m-%d")]

#Aggregate counts
death <- death[, .(dcount = .N), by = .(muncode, muname, date)]

#Reorder by municipality code and date
setorder(death, muncode, date)



### total daily YLL timeseries per municipality
# -------------------------------------------------------------------------

rm(list = ls())
# packages
library(tidyverse); library(lubridate); library(stringr); library(zoo); library(slider)

### RAW DATA
#----

# load mortality series of Switzerland by municipalities
mort_data <- readRDS("/Volumes/FS/_ISPM/CCH/01Data/Mortality_CH/mortality_CH_1969-2018/deathrecordsmuncipality.rds")


# subselect data
mort_data_clean <- mort_data |>
  dplyr::select(
    Kantonname, # for cantonal aggregation
    # NAME, # for municipality aggregation
    date, yy, sex, age_death, byy
  ) |>
  rename(death_year = "yy",
         birth_year = "byy") |>
  arrange(Kantonname, date)

# link cantons to numbers
cant_numb <- mort_data |>
  group_by(KANTONSNUM) |>
  summarise(Kantonname = first(Kantonname))

# load new data 17-2023
d_new <- read_csv("/Volumes/FS/_ISPM/CCH/01Data/Mortality_CH/mortality_CH_2017_2024/Datenlieferung_UNIBE_Vicedo_TU ab 2017_20251118.csv")
d_new_clean <- d_new |>
  filter(EREIGNIS_JJJJ_GES_N > 2016) |>
  mutate(date = as.Date(paste(EREIGNIS_JJJJ_GES_N, EREIGNIS_MM_GES_N, EREIGNIS_TT_GES_N, sep = "-")),
         birth_year = EREIGNIS_JJJJ_GES_N - P_ALTER_ERFUELLT_N) |>
  left_join(cant_numb, by = c("WOHNKANTON_AKT_N" = "KANTONSNUM")) |>
  rename(death_year = "EREIGNIS_JJJJ_GES_N", age_death = "P_ALTER_ERFUELLT_N", sex = "GESCHLECHT_N") |>
  dplyr::select(Kantonname, date, death_year, sex, age_death, birth_year) |>
  arrange(Kantonname, date)

# rbind old and new data
mort_data_clean <- rbind(mort_data_clean |> filter(death_year < 2017), d_new_clean)

# load new data 2024
d_new <- read_csv("/Volumes/FS/_ISPM/CCH/01Data/Mortality_CH/mortality_CH_2017_2024/Datenlieferung_UNIBE_Vicedo_TU 2024_20260105.csv")
d_new_clean <- d_new |>
  filter(EREIGNIS_JJJJ_GES_N > 2016) |>
  mutate(date = as.Date(paste(EREIGNIS_JJJJ_GES_N, EREIGNIS_MM_GES_N, EREIGNIS_TT_GES_N, sep = "-")),
         birth_year = EREIGNIS_JJJJ_GES_N - P_ALTER_ERFUELLT_N) |>
  left_join(cant_numb, by = c("WOHNKANTON_AKT_N" = "KANTONSNUM")) |>
  rename(death_year = "EREIGNIS_JJJJ_GES_N", age_death = "P_ALTER_ERFUELLT_N", sex = "GESCHLECHT_N") |>
  dplyr::select(Kantonname, date, death_year, sex, age_death, birth_year) |>
  arrange(Kantonname, date)

# rbind old and new data
mort_data_clean <- rbind(mort_data_clean, d_new_clean)




# load remaining life expectancies
lt_data <- read.csv("data/pop_characteristics/Switzerland/life_table_1871_2030.csv")
lt_data <- lt_data |>
  rename(remaining_LE = "Verbleibende.Lebensdauer..ex.",
         birth_year = "Geburtsjahrgang",
         sex = "Geschlecht",
         age_death = "Alter") |>
  dplyr::select(birth_year, sex, age_death, remaining_LE) |>
  mutate(sex = ifelse(sex == "Frau", 2, 1),
         age_death = as.numeric(gsub("([0-9]+).*$", "\\1", age_death)))

#----


### Aggregate by municipalities and summarize
#----

YLL_data <- mort_data_clean |>
  left_join(lt_data, by = c("birth_year", "sex", "age_death")) |>
  group_by(Kantonname, date) |> # for cantonal aggregation
  # group_by(NAME, date) |> # for municipality aggregation
  summarise(YLL = sum(remaining_LE, na.rm = T),
            YLL_male = sum(remaining_LE[sex == 1], na.rm = TRUE),
            YLL_female = sum(remaining_LE[sex == 2], na.rm = TRUE),
            YLL_5plus = sum(remaining_LE[remaining_LE >= 5], na.rm = TRUE),
            YLL_0_5 = sum(remaining_LE[remaining_LE < 5], na.rm = TRUE),
            YLL_10plus = sum(remaining_LE[remaining_LE >= 10], na.rm = TRUE),
            YLL_0_10 = sum(remaining_LE[remaining_LE < 10], na.rm = TRUE),
            YLL_20plus = sum(remaining_LE[remaining_LE >= 20], na.rm = TRUE),
            YLL_0_20 = sum(remaining_LE[remaining_LE < 20], na.rm = TRUE),
            deaths_YLL = n(),
            deaths_YLL_male = sum(sex == 1, na.rm = T),
            deaths_YLL_female = sum(sex == 2, na.rm = T),
            deaths_YLL_0_5 = sum(remaining_LE < 5, na.rm = TRUE),
            deaths_YLL_5plus = sum(remaining_LE >= 5, na.rm = TRUE),
            deaths_YLL_0_10 = sum(remaining_LE < 10, na.rm = TRUE),
            deaths_YLL_10plus = sum(remaining_LE >= 10, na.rm = TRUE),
            deaths_YLL_0_20 = sum(remaining_LE < 20, na.rm = TRUE),
            deaths_YLL_20plus = sum(remaining_LE >= 20, na.rm = TRUE)
  )

# create all municipalities–date combinations
all_combos <- expand_grid(
  Kantonname = unique(mort_data_clean$Kantonname), # cantons
  # NAME = unique(mort_data_clean$NAME), # municipalities
  date = seq(min(mort_data_clean$date), max(mort_data_clean$date), by = "day")
)

# merge and fill missing with 0
YLL_data <- all_combos |>
  left_join(YLL_data, by = c("Kantonname", "date")) |> # cantons
  # left_join(YLL_data, by = c("NAME", "date")) |> # municipalities
  mutate(across(c(starts_with("YLL"), starts_with("deaths")), ~ replace_na(., 0))) |>
  rename(state = "Kantonname")

# create time indicators
YLL_data <- transform(YLL_data,
                      year = year(YLL_data$date),
                      month = month(YLL_data$date),
                      day = day(YLL_data$date),
                      mday = mday(YLL_data$date),
                      yday = yday(YLL_data$date),
                      dow = wday(YLL_data$date))


# # assign canton names to municipalities
# cant_names <- mort_data |>
#   group_by(NAME) |>
#   summarise(Kantonname = first(Kantonname)) |>
#   rename(municipality = "NAME")
# # and match to municipalities dataset
# YLL_data <- YLL_data |>
#   rename(municipality = "NAME") |>
#   left_join(cant_names, by = "municipality")
# # table(YLL_data$Kantonname)
# # length(unique(YLL_data$municipality))

# write data
saveRDS(YLL_data, "/Volumes/FS/_ISPM/CCH/Tino/YLL/Switzerland/YLL.rds", compress = "xz")

#----