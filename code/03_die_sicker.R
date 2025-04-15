source("code/00_setup.R")

dat <- read_csv("data/share_2_year_age_adj.csv.gz")|> 
  select(-1)

tr <- dat |> 
  filter(health_var == "self_report",
         time == 2013,
         adjusted == "yes",
         sex == "female")

init <- c(H = 1, U = 0)
lh   <- rep(0,length(age))
lu   <- rep(0,length(age))



