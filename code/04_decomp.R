# calculate sensitivity at average parameters
source("code/01_functions.R")

# dat <- local(get(load("data/final_result.RData")))
dat <- read_csv("data/share_2_year_age_adj.csv.gz")|> 
  select(-1)

dec <-
  dat |> 
  unite(transition, from, to, sep="") |> 
  filter(adjusted == "yes") |> 
  group_by(time, health_var) |> 
  group_modify(~do_dec(data=.x,
                       init = c(H=1,U=0), 
                       expectancy = "h", 
                       interval = 2)) 







