library(usethis) ### for connecting with github
library(gitcreds) ### for entering git token
library(readxl)
library(tidyverse)
library(pander)

### connect to github
create_github_token()
gitcreds_set()

setwd("~/Documents/Data Science/Reef Renewal Bonaire")
carl3 <- read_excel("Carls Hill Monitoring Database.xlsx", 
                    sheet = 2, 
                    range = 'A2:Y86',
                    col_names = c("date", "duration", "spieces", "geno", "cluster", 
                                  "n_outplanted","n_present", "n_lost", "health_0",
                                  "health_q1", "health_q2", "health_q3", "health_q4", "health_100",
                                  "perc_dead_tissue", "disease", "ciliate", "bottom_contact", "fireworm",
                                  "snail", "cliona", "competition", "unknown", "none", "n_stressors"))

### Calculating net coral loss by geno
carl_losses <- carl3 %>%
    group_by(geno) %>%
    summarize(net_loss = sum(n_outplanted - n_present)) %>%
    arrange(desc(net_loss))

### Calculating weighted coral health
carl_health <- carl3 %>%
  select(geno, n_present, health_0, health_q1, health_q2, health_q3, health_q4, health_100) %>%
  group_by(geno) %>% 
  summarize(n_present = sum(n_present, na.rm = TRUE),
            health_0 = sum(health_0, na.rm = TRUE), 
            health_U25 = sum(health_q1, na.rm = TRUE), 
            health_U50 = sum(health_q2, na.rm = TRUE),
            health_U75 = sum(health_q3, na.rm = TRUE), 
            health_U99 = sum(health_q4, na.rm = TRUE),
            health_100 = sum(health_100, na.rm = TRUE)) %>%
  mutate(weighted_health = ((health_0*0) +
                              (health_U25*0.125) +
                              (health_U50*.375) +
                              (health_U75*.625) +
                              (health_U99*.875) +
                              (health_100*1)/n_present))  %>%
  arrange(desc(weighted_health)) 

### Table of net losses and weighted health
carl_losses %>%
  inner_join(carl_health, by = "geno") %>%
  select(geno, net_loss, weighted_health) %>%
  arrange(desc(weighted_health)) %>%
  print(n=22)



