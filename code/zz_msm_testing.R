


# read in demographics, example
library(haven)
library(expss)
library(tidyverse)
dn <- read_dta(unz("data/sharew9_rel9-0-0_ALL_datasets_stata.zip","sharew9_rel9-0-0_dn.dta"))
dn |> colnames()
dn |> select(mergeid, mergeidp9) |> head()
dn |> 
  separate_wider_delim(mergeid,delim="-", names = c("CC","id","wave")) |> str()
dir("data")

test <- local(get(load("data/GH_SHARE_g/GH_SHARE_g/GH_SHARE_g.rdata"))) |> 
  filter(isoa3 == "ESP")
test
vars <- c("mergeid",
         "radyear",
         "radmonth",
         "rabyear",
         "rabmonth",
         paste0("r", c(1,2,4:9), "iwstat"),
         paste0("r", c(1,2,4:9), "iwy"),
         paste0("r", c(1,2,4:9), "iwm"),
         paste0("r",c(1,2,4:9), "agem"),
         paste0("r",c(1,2,4:9), "adlfive"),
         paste0("r",c(1,2,4:9), "adltot6"))

test |> 
  select(all_of(vars)) |> 
  pivot_longer(
    cols = matches("^r\\d{1,2}(iwstat|iwy|iwm|agem|adlfive|adltot6)$"),
    names_to = c("wave", ".value"),
    names_pattern = "r(\\d{1,2})([a-z]+)"
  ) |> 
  mutate(obs_date = iwy + (iwm - .5)/12,
         age = (agem+.5)/12,
         birth_date = rabyear + (rabmonth - .5) / 12,
         death_date = radyear + (radmonth - .5) / 12,
         age_death = death_date - birth_date,
         age = if_else(is.na(age), age_death, age)) |> 
  filter(!iwstat %in% c(0,4),
         !is.na(age)) |> 
  arrange(mergeid, wave) |> 
  group_by(mergeid) |> 
  filter(age > lag(age, default = 0)) |> 
  mutate(obs_date = if_else(is.na(obs_date), death_date, obs_date)) |> 
  select(mergeid,wave, iwstat,adlfive,adltot,obs_date,age) |> 
  mutate(state = case_when(adlfive > 0 ~ 2,
                           adlfive == 0 ~ 1,
                           is.na(adlfive) ~ 3))
  


