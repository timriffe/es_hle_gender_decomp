library(tidyverse)
dat <- read_csv("data/share_2_year_age_adj.csv.gz")|> 
  select(-1)


dat |> 
  mutate(transition = paste0(from, to)) |> 
  filter(time  == 2011,
         between(age, 50,85),
         to == "D") |> 
  ggplot(aes(x = age, y = prob, color = transition, linetype = adjusted)) +
  geom_line() +
  facet_wrap(sex~health_var) +
  scale_y_log10() +
  labs(title = "very low mortality bias, impressive")

# all transitions for a given year
dat |> 
  filter(from != to,
         adjusted == "yes") |> 
  unite(transition, from, to, sep = "") |> 
  filter(time == 2013) |> 
  ggplot(aes(x = age, 
             y = prob, 
             color = transition, 
             linetype = sex)) +
  geom_line() +
  theme_minimal() +
  labs(y = "transition probability") +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14)) +
  facet_wrap(~health_var)

# look at mortality rate ratios
# TR: clearly, time is in the model: time trends are just very consistent.
dat |> 
  filter(from != to,
         adjusted == "yes",
         to == "D") |> 
  unite(transition, from, to, sep = "") |> 
  pivot_wider(names_from = transition, values_from = prob) |> 
  ggplot(aes(x = age, y = UD/HD, linetype = sex, color = time, groups = interaction(sex,time))) +
  geom_line() +
  facet_wrap(~health_var) +
  theme_minimal() 
  


dat |> 
  filter(from != to,
         adjusted == "yes") |> 
  unite(transition, from, to, sep = "") |> 
  filter(time == 2013, health_var=="adl",sex=="female") |> 
  ggplot(aes(x = age, 
             y = prob, 
             color = transition)) +
  geom_line() +
  theme_minimal() +
  labs(y = "transition probability") +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14)) 


dat |> 
  filter(from != to,
         adjusted == "yes") |> 
  unite(transition, from, to, sep = "") |> 
  filter(time == 2013, health_var=="adl") |> 
  ggplot(aes(x = age, 
             y = prob, 
             color = transition,
             linetype = sex)) +
  geom_line() +
  theme_minimal() +
  labs(y = "transition probability") +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14)) 
