# calculate sensitivity at average parameters
source("code/01_functions.R")

# dat <- local(get(load("data/final_result.RData")))
dat <- read_csv("data/share_2_year_age_adj.csv.gz")|> 
  select(-1)

dech <-
  dat |> 
  unite(transition, from, to, sep="") |> 
  filter(adjusted == "yes") |> 
  group_by(time, health_var) |> 
  group_modify(~do_dec(data=.x, 
                       expectancy = "h", 
                       interval = 2)) 

decu <-
  dat |> 
  unite(transition, from, to, sep="") |> 
  filter(adjusted == "yes") |> 
  group_by(time, health_var) |> 
  group_modify(~do_dec(data=.x, 
                       expectancy = "u", 
                       interval = 2)) 


dech |> 
  group_by(health_var,time,transition) |> 
  summarize(dec = sum(dec)) |> 
  filter(health_var == "adl", time == 2013) |> 
  ggplot(aes(y = dec, x=transition,fill = transition)) +
  geom_col()

decu |> 
  group_by(health_var,time) |> 
  summarize(dec = sum(dec)) |> 
  ggplot(aes(x=time, y=dec, color = health_var)) +
  geom_line()

