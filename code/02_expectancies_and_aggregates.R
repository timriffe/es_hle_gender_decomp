# calculate sensitivity at average parameters
source("code/00_setup.R")

# dat <- local(get(load("data/final_result.RData")))
dat <- read_csv("data/share_2_year_age_adj.csv.gz") |> 
  select(-1)
dat <- read_csv("data/final_results_prelim.csv.gz")|> 
  select(-1)

# plot reminds me we need to switch SHARE estimation to msm procedure
dat |> 
  filter(adjusted == "yes", prob_type == "1_year", from != to, time == 2015, health_var == 
      "adl") |> 
  ggplot(aes(x = age, y = prob, color = to, linetype = time_multinom)) +
  geom_line() +
  facet_wrap(sex~from) +
  scale_y_log10()

expectancies <-
  dat |> 
  unite(transition, from, to, sep="") |> 
  filter(adjusted == "yes", prob_type = "1_year") |> 
  rename(p = prob) |> 
  group_by(time, health_var, sex, prob_type, time_multinom) |> 
  summarize(le = f1t(data = pick(everything()), 
                     expectancy = "t",
                     init = c(H=1,U=0),
                     interval = if_else(prob_type[1] == "1_year",1,2)),
            hle = f1t(data = pick(everything()), 
                      expectancy = "h",
                      init = c(H=1,U=0),
                      interval = if_else(prob_type[1] == "1_year",1,2)),
            .groups = "drop") 

expectancies |> 
  filter(health_var !="chronic") |> 
  ggplot(aes(x=time, y = hle, color = prob_type, linetype = time_multinom)) +
  geom_line() +
  facet_grid(vars(health_var),vars(sex)) + 
  theme_minimal() +
  theme(axis.title = element_text(size=14),
                axis.text = element_text(size=12),
                strip.text = element_text(size=14),
        panel.spacing.x = unit(.5,"cm"))



options(device = Cairo::CairoWin)

# LE (just one)
expectancies |> 
  group_by(sex,time) |> 
  summarize(le = mean(le)) |> 
  ggplot(aes(x = time, y = le, color = sex)) +
  geom_point() +
  geom_line(linewidth=1) +
  #facet_wrap(~health_var) +
  theme_minimal() +
  labs(y = "life expectancy age 50") +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14),
        panel.spacing.x = unit(2, "lines")) +
  guides(color = "none") 

# HLE
expectancies |> 
  ggplot(aes(x = time, y = hle, color = sex)) +
  geom_point() +
  geom_line() +
  facet_wrap(~health_var) +
  theme_minimal() +
  labs(y = "healthy life expectancy age 50") +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14),
        panel.spacing.x = unit(2, "lines")) +
  guides(color = "none") 


# DLE
expectancies |> 
  ggplot(aes(x = time, y = le-hle, color = sex)) +
  geom_point() +
  geom_line() +
  facet_wrap(~health_var) +
  theme_minimal() +
  labs(y = "unhealthy life expectancy age 50") +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14),
        panel.spacing.x = unit(2, "lines")) +
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
        strip.text = element_text(size=14),
        panel.spacing.x = unit(2, "lines")) +
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
        strip.text = element_text(size=14),
        panel.spacing.x = unit(2, "lines")) +
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
        strip.text = element_text(size=14),
        panel.spacing.x = unit(2, "lines")) +
  guides(color = "none") 



# difference HLE, example



expectancies |> 
  select(health_var,time,sex,hle) |> 
  pivot_wider(names_from = sex, values_from = hle) |> 
  mutate(`HLE(women-men)` = female - male) |> 
  filter(health_var == "adl",
         time == 2011)

expectancies |> 
  filter(health_var == "adl",
         time == 2011)
