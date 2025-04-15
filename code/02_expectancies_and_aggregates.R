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
