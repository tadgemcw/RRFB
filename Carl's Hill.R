### connect to github
library(usethis)
library(gitcreds)
create_github_token()
gitcreds_set()

### Main Project
library(readxl)
library(tidyverse)
library(pander)


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
    summarize(mortality_rate = sum((n_outplanted - n_present) / n_outplanted)) %>%
    arrange(desc(mortality_rate))
carl_losses

### Calculating weighted coral health by geno
carl_health <- carl3 %>%
  select(geno, n_present, health_0, health_q1, health_q2, health_q3, health_q4, health_100) %>%
  rowwise() %>%
  mutate(Health0 = sum(health_0, na.rm = TRUE),
         Health25 = sum(health_q1 * 0.125, na.rm = TRUE),
         Health50 = sum(health_q2 * 0.375, na.rm = TRUE),
         Health75 = sum(health_q3 * 0.625, na.rm = TRUE),
         Health99 = sum(health_q4 * 0.875, na.rm = TRUE),
         Health100 = sum(health_100, na.rm = TRUE),
         weighted_health = sum(Health0 + Health25 + Health50 + Health75 + 
                           Health99 + Health100, na.rm = TRUE) / n_present) 

carl_health_agg <- carl_health %>%
  group_by(geno) %>%
  summarize(avg_health = mean(weighted_health)) %>%
  arrange(desc(avg_health))


### Table of net losses and weighted health by geno
loss_health_table <- carl_losses %>%
  inner_join(carl_health_agg, by = "geno") %>%
  select(geno, avg_health, mortality_rate) %>%
  arrange(desc(avg_health)) 
print(loss_health_table, n = 22)

    
### boxplot of tissue health by geno
ggplot(carl_health, aes(x = factor(geno), y = weighted_health)) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(0.5, 1.0)) +
  labs(x = "Geno", y = "Tissue Health")

### boxplot of tissue decay by geno
ggplot(carl3, aes(x = factor(geno), y = perc_dead_tissue)) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(-0.05, 1.1)) +
  labs(x = "Geno", y = "Tissue Decay")
