# calculate sensitivity at average parameters
source("code/00_setup.R")
source("code/01_functions.R")

# dat <- local(get(load("data/final_result.RData")))
# dat <- read_csv("data/share_2_year_age_adj.csv.gz")|> 
#   select(-1)
dat <- read_csv("data/crude_transition_SHARE.csv.gz")
head(dat)
dat <- read_csv("data/adjusted_transition_SHARE.csv.gz")
head(dat)
dech <-
  dat |> 
  unite(transition, from, to, sep="") |> 
  filter(type == "Adjusted",
         age >= 40,health_var == "adl") |> 
  group_by(health_var) |> 
  group_modify(~do_dec(data=.x, 
                       expectancy = "h", 
                       interval = 1,
                       age_from = 40)) |> 
  mutate(le_type = "DFLE")

dech |> 
  group_by(transition) |> 
  summarize(dec = sum(dec)) |> 
  group_by(sign(dec)) |> 
  summarize(dec = sum(dec))

decu <-
  dat |> 
  unite(transition, from, to, sep="") |> 
  filter(type == "Adjusted",
         age >= 40,
         health_var == "adl") |> 
  group_by(health_var) |> 
  group_modify(~do_dec(data=.x, 
                       expectancy = "u", 
                       interval = 1,
                       age_from = 40)) |> 
  mutate(le_type = "DLE")
dect <-
  dat |> 
  unite(transition, from, to, sep="") |> 
  filter(type == "Adjusted",
         age >= 40,
         health_var == "adl") |> 
  group_by(health_var) |> 
  group_modify(~do_dec(data=.x, 
                       expectancy = "t", 
                       interval = 1,
                       age_from = 40)) |> 
  mutate(le_type = "LE")

dec_all <- bind_rows(dech,decu, dect)


 

dec_all |> 
  group_by(le_type,
           transition) |> 
  summarize(dec = sum(dec)) |> 
  pivot_wider(names_from = le_type, values_from = dec) 

dec_all |> 
  filter(le_type == "DFLE") |> 
  group_by(transition) |> 
  summarize(dec = sum(dec)) |> 
  group_by(sign(dec)) |> 
  summarize(dec = sum(dec))

# fig sketch for Caixa report...
dec_all |> 
  filter(le_type == "DFLE") |> 
  mutate(transition = case_match(
    transition,
    "UH" ~ "Recuperación",
    "HU" ~ "Descapacitación",
    "UD" ~ "Mortalidad\n(discapacitado)",
    "HD" ~ "Mortalidad\n(sin descapacidad)",
  )) |> 
  group_by(transition) |> 
  summarize(dec = sum(dec), .groups = "drop") |> 
  ggplot(aes(y = dec * 12, x = fct_reorder(transition,dec), fill = transition)) +
  geom_col() +
  theme_minimal() +
  coord_flip(clip = "off") +
  labs(x = "",
       y = "contribución a la brecha\n(meses)") +
  guides(fill = "none") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        plot.margin = margin(1.5,0,0,0, "cm")) +
  scale_fill_manual(values=c("#e8b620","#160552","#87bfe0","#614773")) +
  annotate("segment", x = 4.6, y = 0, xend = 4.6, yend = 10,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")), color = gray(.3)) +
  annotate("text",x = 4.7,y=4, label = "ventaja de mujeres",size = 6, color = gray(.3))

# example 2017 adl
dech |> 
  filter(health_var == "adl") |> 
  group_by(transition) |> 
  summarize(dec = sum(dec), .groups= "drop") |> 
  ggplot(aes(y = dec, x=transition, fill = transition)) +
  geom_col() +
  theme_minimal()+
  labs(y = "contribution to ADL gap") +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14)) +
  guides(color = "none") +
  scale_fill_manual(values=c("#e8b620","#614773","#160552","#87bfe0"))

dech |> 
  filter(health_var == "adl") |> 
  group_by(transition) |> 
  summarize(dec = sum(dec), .groups= "drop") 

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
  guides(color = "none") +
  scale_fill_manual(values=c("#e8b620","#614773","#160552","#87bfe0"))
  


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

