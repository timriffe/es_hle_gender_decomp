# calculate sensitivity at average parameters
source("code/00_setup.R")

# dat <- local(get(load("data/final_result.RData")))
dat <- read_csv("data/share_2_year_age_adj.csv.gz") |> 
  select(-1)
dat <- read_csv("data/final_results_prelim.csv.gz")|> 
  select(-1)
dat <- read_csv("data/crude_transition_SHARE.csv.gz")
head(dat)
dat <- read_csv("data/adjusted_transition_SHARE.csv.gz")

# plot reminds me we need to switch SHARE estimation to msm procedure
dat |> 
  filter(from != to, 
         health_var == "adl",
  
         age >= 40) |> 
  ggplot(aes(x = age, y = prob, color = to, linetype=type)) +
  geom_line() +
  facet_wrap(sex~from) +
  scale_y_log10()

expectancies <-
  dat |> 
  filter(type == "Adjusted",
         age >= 40) |> 
  unite(transition, from, to, sep="") |> 
  rename(p = prob) |> 
  group_by(time, health_var, sex) |> 
  summarize(le = f1t(data = pick(everything()), 
                     expectancy = "t",
                     init = c(H=1,U=0),
                     interval = 1),
            hle = f1t(data = pick(everything()), 
                      expectancy = "h",
                      init = c(H=1,U=0),
                      interval = 1),
            .groups = "drop") 

expectancies |> 
  filter(health_var !="chronic") |> 
  ggplot(aes(x=health_var, y = hle, fill =  sex)) +
  theme_minimal() +
  geom_col(position = "dodge") +
  theme(axis.title = element_text(size=14),
                axis.text = element_text(size=12),
                strip.text = element_text(size=14),
        panel.spacing.x = unit(.5,"cm"))



# options(device = Cairo::CairoWin)

# # LE (just one)
# expectancies |> 
#   ggplot(aes(x = time, y = le, color = sex)) +
#   geom_point() +
#   geom_line(linewidth=1) +
#   facet_wrap(~health_var) +
#   theme_minimal() +
#   labs(y = "life expectancy age 50") +
#   theme(axis.title = element_text(size=14),
#         axis.text = element_text(size=12),
#         strip.text = element_text(size=14),
#         panel.spacing.x = unit(2, "lines")) +
#   guides(color = "none") 

# HLE
# expectancies |> 
#   ggplot(aes(x = time, y = hle, color = sex)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(~health_var) +
#   theme_minimal() +
#   labs(y = "healthy life expectancy age 50") +
#   theme(axis.title = element_text(size=14),
#         axis.text = element_text(size=12),
#         strip.text = element_text(size=14),
#         panel.spacing.x = unit(2, "lines")) +
#   guides(color = "none") 


# DLE
expectancies |> 
  ggplot(aes(x = health_var, y = le-hle, fill = sex)) +
  geom_col(position = "dodge")+
  theme_minimal() +
  labs(y = "unhealthy life expectancy age 50") +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14),
        panel.spacing.x = unit(2, "lines")) +
  guides(color = "none") 


# proportion
expectancies |> 
  ggplot(aes(x = health_var, y = hle/le, fill = sex)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(y = "proportion healthy") +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14),
        panel.spacing.x = unit(2, "lines")) +
  guides(color = "none") 

expectancies |>
  filter(health_var == "adl") |> 
  mutate(hle/le)

# difference LE
expectancies |> 
  select(health_var,time,sex,le) |> 
  pivot_wider(names_from = sex, values_from = le)|> 
  mutate(`LE(women-men)` = female - male)
# difference HLE
expectancies |> 
  select(health_var,time,sex,hle) |> 
  pivot_wider(names_from = sex, values_from = hle)|> 
  mutate(`HLE(women-men)` = female - male)

expectancies |> 
  select(health_var,time,sex,hle) |> 
  pivot_wider(names_from = sex, values_from = hle) |> 
  mutate(`HLE(women-men)` = female - male) |> 
  ggplot(aes(x = health_var, y = `HLE(women-men)`)) +
  geom_hline(yintercept = 0, 
             color = "red", 
             linewidth=.5,
             alpha=.5) +
  geom_col(position = "dodge") +
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
  ggplot(aes(x = health_var, y = `ULE(women-men)`)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(y = "difference in ULE (women-men)") +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14),
        panel.spacing.x = unit(2, "lines")) +
  guides(color = "none") 


