# calculate sensitivity at average parameters
source("code/00_setup.R")

# dat <- local(get(load("data/final_result.RData")))
dat <- read_csv("data/share_2_year_age_adj.csv.gz")|> 
  select(-1)

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


# LE (just one)
expectancies |> 
  group_by(sex,time) |> 
  summarize(le = mean(le)) |> 
  ggplot(aes(x = time, y = le, color = sex)) +
  geom_point() +
  geom_line() +
  #facet_wrap(~health_var) +
  theme_minimal() +
  labs(y = "life expectancy") +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14)) +
  guides(color = "none")

# HLE
expectancies |> 
  ggplot(aes(x = time, y = hle, color = sex)) +
  geom_point() +
  geom_line() +
  facet_wrap(~health_var) +
  theme_minimal() +
  labs(y = "healthy life expectancy") +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14)) +
  guides(color = "none")

# DLE
expectancies |> 
  ggplot(aes(x = time, y = le-hle, color = sex)) +
  geom_point() +
  geom_line() +
  facet_wrap(~health_var) +
  theme_minimal() +
  labs(y = "unhealthy life expectancy") +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14)) +
  guides(color = "none")

# proportion
expectancies |> 
  ggplot(aes(x = time, y = hle/le, color = sex)) +
  geom_point() +
  geom_line() +
  facet_wrap(~health_var) +
  theme_minimal() +
  labs(y = "proportion healthy") +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14)) +
  guides(color = "none")

# difference HLE
expectancies |> 
  select(health_var,time,sex,hle) |> 
  pivot_wider(names_from = sex, values_from = hle) |> 
  mutate(`HLE(women-men)` = female - male) |> 
  ggplot(aes(x = time, y = `HLE(women-men)`)) +
  geom_hline(yintercept = 0, 
             color = "red", 
             linewidth=.5,
             alpha=.5)+
  geom_point() +
  geom_line() +
  facet_wrap(~health_var) +
  theme_minimal() +
  labs(y = "difference in HLE (women-men)") +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14)) +
  guides(color = "none") 

expectancies |> 
  mutate(ule = le-hle) |> 
  select(health_var,time,sex,ule) |>
  pivot_wider(names_from = sex, values_from = ule) |> 
  mutate(`ULE(women-men)` = female - male) |> 
  ggplot(aes(x = time, y = `ULE(women-men)`)) +
  geom_hline(yintercept = 0, 
             color = "red", 
             linewidth=.5,
             alpha=.5)+
  geom_point() +
  geom_line() +
  facet_wrap(~health_var) +
  theme_minimal() +
  labs(y = "difference in ULE (women-men)") +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14)) +
  guides(color = "none") 




