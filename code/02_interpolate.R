

# RTz reports that these 2-year probabilities sometimes result
# in negative probabilities after interpolating. Let me have a try

library(compositions)
library(tidyverse)
library(expm)
source("code/functions_classic.R")
dat <- read_csv("data/share_2_year_age_adj.csv.gz") |> 
  select(-1)
head(dat)
chunk <- dat |> 
  filter(adjusted == "yes",
         sex == "female",
         time == 2011,
         health_var == "self_report")

graduate_chunk <- function(chunk, interval_current = 2, interval_desired = 1){
  trns <-
    chunk |> 
    unite(transition,from,to,sep="")  |> 
    pivot_wider(names_from = transition, values_from = prob) |> 
    select(age,HH,HU,HD,UH,UU,UD) 
  # 1) make U closed
  U <- Ptibble2U_closed(trns, interval = interval_current, start_age= 50)
  # 2) convert U to Q (rates) using U2Q()
  # U <- U[-nrow(U),-ncol(U)]
  # Q <- U2Q_hackish(U, interval_current = interval_current)
  Q <- U2Q(U, interval_current = interval_current)
  # 3) turn Q into a handy tibble using Q2Rtibble()
  Rtibble <- Q2Rtibble(Q) 
  # 4) now graduate the attrition
  Rtibble1 <- graduate_Rtibble(Rtibble, 
                               interval_current = interval_current, 
                               interval_desired =   interval_desired)
  # 5) take these single-age rates and convert back to Q using Rtibble2Q()
  Q1 <- Rtibble2Q(Rtibble1, interval = 1,start_age = 50)
  # 6) convert this back to U using Q2U()
  U1 <- Q2U(t(Q1), interval_desired = 1)
  # 7) convert U to a handy tibble using U2Ptibble()
  Ptibble1 <- U2Ptibble(U1, interval_current = 1)
  Ptibble1 |> 
    pivot_longer(-age, names_to = "transition", values_to = "p")
}
graduate_chunk(chunk,2)
dat1 <-
  dat |> 
  filter(adjusted == "yes") |> 
  group_by(sex,time,health_var) |> 
  group_modify(~graduate_chunk(chunk = .x,interval_current = 2, interval_desired = 1))

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

dat1 |> 
  filter(time == 2013,
         !transition %in% c("HH","UU")) |> 
  ggplot(aes(x = age, 
             y = p, 
             color = transition, 
             linetype = sex)) +
  geom_line() +
  theme_minimal() +
  labs(y = "transition probability") +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14)) +
  facet_wrap(~health_var)


dat1 |> 
  separate_wider_position(transition,widths=c(from=1,to=1)) |> 
  group_by(sex,time,health_var,age,from) |> 
  summarize(test = sum(p))
