# calculate sensitivity at average parameters
source("code/00_setup.R")
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


# example 2011 adl
dech |> 
  filter(health_var == "adl", time == 2011) |> 
  group_by(transition) |> 
  summarize(dec = sum(dec), .groups= "drop") |> 
  ggplot(aes(y = dec, x=transition, fill = transition)) +
  geom_col() +
  theme_minimal()+
  labs(y = "contribution to ADL gap") +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14)) +
  guides(color = "none") 



dech |> 
  group_by(health_var,time,transition) |> 
  summarize(dec = sum(dec), .groups= "drop") |> 
  ggplot(aes(y = dec, x=time,fill = transition)) +
  geom_bar(position="stack", stat="identity") +
  theme_minimal() +
  facet_wrap(~health_var) +
  labs(y = "contribution to HLE gap") +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14),
        panel.spacing.x = unit(2, "lines")) +
  guides(color = "none") 

  


decu |> 
  group_by(health_var,time,transition) |> 
  summarize(dec = sum(dec), .groups= "drop") |> 
  ggplot(aes(y = dec, x=time,fill = transition)) +
  geom_bar(position="stack", stat="identity") +
  theme_minimal() +
  facet_wrap(~health_var) +
  labs(y = "contribution to ADL gap") +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14),
        panel.spacing.x = unit(2, "lines")) +
  guides(color = "none") 

