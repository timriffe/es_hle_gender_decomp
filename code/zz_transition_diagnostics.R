library(tidyverse)


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






