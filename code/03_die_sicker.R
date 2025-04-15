source("code/00_setup.R")

dat <- read_csv("data/share_2_year_age_adj.csv.gz")|> 
  select(-1)

tr <- dat |> 
  filter(health_var == "self_report",
         time == 2013,
         adjusted == "yes",
         sex == "female")
tr




