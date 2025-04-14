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











