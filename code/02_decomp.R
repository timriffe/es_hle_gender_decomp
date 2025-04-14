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

expectancies <-
  dat |> 
  unite(transition, from, to, sep="") |> 
  filter(adjusted == "yes") |> 
  rename(p = prob) |> 
  group_by(time, health_var, sex) |> 
  summarize(le = f1t(data = pick(everything()), 
                      expectancy = "t",
                      init = c(H=1,U=0),
                      interval = 2),
            hle = f1t(data = pick(everything()), 
                     expectancy = "h",
                     init = c(H=1,U=0),
                     interval = 2),
            .groups = "drop") 

expectancies |> 
  ggplot(aes(x = time, y = le, color = sex)) +
  geom_point() +
  geom_line() +
  facet_wrap(~health_var) +
  theme_minimal()

expectancies |> 
  ggplot(aes(x = time, y = hle, color = sex)) +
  geom_point() +
  geom_line() +
  facet_wrap(~health_var) +
  theme_minimal()



