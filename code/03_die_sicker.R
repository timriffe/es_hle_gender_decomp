source("code/00_setup.R")

dat <- read_csv("data/share_2_year_age_adj.csv.gz")|> 
  select(-1)
dxs <- 
dat |> 
  filter(adjusted == "yes") |> 
  unite(transition,from,to,sep="") |> 
  group_by(health_var, time, sex) |> 
  group_modify(~get_dxs(tr = .x)) |> 
  ungroup() |> 
  pivot_longer(c(dxh,dxu), names_to ="state", values_to = "dx") |> 
  mutate(state = substr(state,3,3) |> toupper()) 

dxs |> 
  filter(time == 2015,
         health_var == "adl") |> 
  ggplot(aes(x = age, y = dx, color = state, linetype = sex)) +
  geom_line() +
  theme_minimal() +
  labs(y = "lifetable deaths (proportion)") +
  scale_color_manual(values = c(H = "#23a65e",U = "#81278f")) +
  theme_minimal() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14))

dxs |> 
  filter(time == 2015,
         health_var == "adl") |> 
  group_by(sex) |> 
  summarize(pu = sum(dx[state=="U"])/sum(dx))

dxs |> 
  group_by(sex, health_var, time) |> 
  summarize(pu = sum(dx[state=="U"])/sum(dx)) |> 
  ggplot(aes(x= time, y = pu, color = sex)) +
  geom_line() +
  facet_wrap(~health_var)+
  theme_minimal() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14)) +
  labs(y = "deaths unhealthy (proportion)")
