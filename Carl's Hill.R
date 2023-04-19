library(readxl)
library(tidyverse)
library(pander)

setwd("~/Documents/Data Science/Reef Renewal Bonaire")
carl3 <- read_excel("Carls Hill Monitoring Database.xlsx", 
                    sheet = 2, 
                    range = 'A2:Y86',
                    col_names = c("date", "duration", "species", "geno", "cluster", 
                                  "n_outplanted","n_present", "n_lost", "health_0",
                                  "health_q1", "health_q2", "health_q3", "health_q4", "health_100",
                                  "perc_dead_tissue", "disease", "ciliate", "bottom_contact", "fireworm",
                                  "snail", "cliona", "competition", "unknown", "none", "n_stressors"))

### Calculating coral losses by geno
carl_losses <- carl3 %>%
    group_by(species, geno) %>%
    summarize(num_outplanted = sum(n_outplanted),
              num_survived = sum(n_present),
              mortality_rate = ((num_outplanted - num_survived) / num_outplanted) * 100) %>%
    arrange(desc(mortality_rate))
print(carl_losses, n = 22)

carl_losses_pretty <- carl_losses
carl_losses_pretty$mortality_rate <- paste0(round(carl_losses_pretty$mortality_rate, 2), "%")
carl_losses_pretty %>% pander()

### Calculating weighted coral health by geno
carl_health <- carl3 %>%
  select(geno, n_present, health_0, health_q1, health_q2, health_q3, health_q4, health_100) %>%
  rowwise() %>%
  mutate(Health0 = sum(health_0, na.rm = TRUE),
         Health25 = sum(health_q1 * 0.125, na.rm = TRUE),
         Health50 = sum(health_q2 * 0.4, na.rm = TRUE),
         Health75 = sum(health_q3 * 0.65, na.rm = TRUE),
         Health99 = sum(health_q4 * 0.87, na.rm = TRUE),
         Health100 = sum(health_100, na.rm = TRUE),
         weighted_health = (sum(Health25 + Health50 + Health75 + 
                                  Health99 + Health100, na.rm = TRUE) / (n_present - Health0)) * 100) %>%
  drop_na(weighted_health)


carl_health_agg <- carl_health %>%
  group_by(geno) %>%
  summarize(avg_health = mean(weighted_health)) %>%
  arrange(desc(avg_health))


### Table of net losses and weighted health by geno
carl3_loss_health_table <- carl_losses %>%
  inner_join(carl_health_agg, by = "geno") %>%
  select(species, geno, avg_health, mortality_rate) %>%
  arrange(desc(avg_health)) 

carl3_loss_health_table_pretty <- carl3_loss_health_table
carl3_loss_health_table_pretty$avg_health <- paste0(round(carl3_loss_health_table_pretty$avg_health, 2), "%")
pander(carl3_loss_health_table_pretty)

    
### boxplot of tissue health by geno
ggplot(carl_health, aes(x = factor(geno), y = weighted_health)) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5)) +
  labs(x = "Apal Genotype", 
       y = "Percent of Healthy Tissue",
       title = "Carl's Hill",
       subtitle = "3-Month Survey",
       caption = "Survey Date: 2022-04-27")



#### Bottom stressors

bottom_perc <- carl3 %>%
  summarize(N_Outplanted = n(),
            Total_Stressors = sum(n_stressors, na.rm = TRUE),
            Bottom_Contact = sum(bottom_contact, na.rm = TRUE),
            Percent_of_Total = (sum(carl3$bottom_contact, na.rm = TRUE) / 
                sum(carl3$n_stressors, na.rm = TRUE) * 100))

bottom_perc$Percent_of_Total <-paste0(round(bottom_perc$Percent_of_Total, 2), "%")
bottom_perc %>% rename("Coral Observations" = N_Outplanted,
                       "Total Stressors" = Total_Stressors,
                        "Bottom Contact" = Bottom_Contact,
                       "Precent of Total Stressors" = Percent_of_Total) %>%
  pander()


################### CARL 6 MONTH ###################

setwd("~/Documents/Data Science/Reef Renewal Bonaire")
carl6 <- read_excel("Carls Hill Monitoring Database.xlsx", 
                    sheet = 3, 
                    range = 'A2:Z51',
                    col_names = c("date", "duration", "species", "geno", "cluster", 
                                  "n_outplanted","n_present", "n_lost", "health_0",
                                  "health_q1", "health_q2", "health_q3", "health_q4", "health_100",
                                  "rm_rank", "perc_dead_tissue", "perc_rm_clust", 
                                  "disease", "ciliate", "bottom_contact", "fireworm",
                                  "snail", "cyanob", "sedimentation", "unknown", "n_stressors"))


carl6_losses <- carl6 %>%
  group_by(species, geno) %>%
  summarize(num_outplanted = sum(n_outplanted),
            num_survived = sum(n_present),
            mortality_rate = ((num_outplanted - num_survived) / num_outplanted) * 100) %>%
  arrange(desc(mortality_rate))
print(carl6_losses, n = 100)

carl6_losses_pretty <- carl6_losses
carl6_losses_pretty$mortality_rate <- paste0(round(carl6_losses_pretty$mortality_rate, 2), "%")
carl6_losses_pretty %>% pander()

### Calculating weighted coral health by geno
carl6_health <- carl6 %>%
  select(geno, n_present, health_0, health_q1, health_q2, health_q3, health_q4, health_100) %>%
  rowwise() %>%
  mutate(Health0 = sum(health_0, na.rm = TRUE),
         Health25 = sum(health_q1 * 0.125, na.rm = TRUE),
         Health50 = sum(health_q2 * 0.4, na.rm = TRUE),
         Health75 = sum(health_q3 * 0.65, na.rm = TRUE),
         Health99 = sum(health_q4 * 0.87, na.rm = TRUE),
         Health100 = sum(health_100, na.rm = TRUE),
         weighted_health = (sum(Health25 + Health50 + Health75 + 
                                  Health99 + Health100, na.rm = TRUE) / (n_present - Health0)) * 100) %>%
  drop_na(weighted_health)


carl6_health_agg <- carl6_health %>%
  group_by(geno) %>%
  summarize(avg_health = mean(weighted_health)) %>%
  arrange(desc(avg_health))


### Table of net losses and weighted health by geno
carl6_loss_health_table <- carl6_losses %>%
  inner_join(carl6_health_agg, by = "geno") %>%
  select(species, geno, avg_health, mortality_rate) %>%
  arrange(desc(avg_health)) 

carl6_loss_health_table_pretty <- carl6_loss_health_table
carl6_loss_health_table_pretty$avg_health <- paste0(round(carl6_loss_health_table_pretty$avg_health, 2), "%")
carl6_loss_health_table_pretty$mortality_rate <- paste0(round(carl6_loss_health_table_pretty$mortality_rate, 2), "%")
pander(carl6_loss_health_table_pretty)


### boxplot of tissue health by geno
ggplot(carl6_health, aes(x = factor(geno), y = weighted_health)) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5)) +
  labs(x = "Apal Genotype", 
       y = "Percent of Healthy Tissue",
       title = "Carl's Hill",
       subtitle = "6-Month Survey",
       caption = "Survey Date: 2022-07-04")

##### comparing 3 month with 6 month #####


carl_spread <- carl3_loss_health_table %>%
  left_join(carl6_loss_health_table, 
            by = c("species", "geno"), suffix = c("3", "6")) %>%
  mutate(perc_change = avg_health6 - avg_health3) %>%
  arrange(desc(perc_change))
print(carl_spread, n = 100)

carl_long <- carl_spread %>% 
  select(species, geno, avg_health3, avg_health6, perc_change) %>%
  pivot_longer("avg_health3":"avg_health6", 
               names_to = "survey_time",
               values_to = "values")

ggplot(carl_long, aes(x = factor(geno), y = values)) +
  geom_point(aes(color = survey_time)) +
  geom_line(aes(group = geno)) +
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, by = 5)) +
  scale_color_manual(labels = c("April 2022", "July 2022"), 
                     values = c("dodgerblue1", "magenta3")) +
  labs(x = "Apal Genotypes",
       y = "Percent of Healthy Tissue",
       color = "Survey",
       title = "Carl's Hill: Average Change in Tissue Health") +
  theme_light()



#### Bottom stressors 3 month

bottom_perc <- carl3 %>%
  summarize(N_Outplanted = n(),
            Total_Stressors = sum(n_stressors, na.rm = TRUE),
            Bottom_Contact = sum(bottom_contact, na.rm = TRUE),
            Percent_of_Total = (sum(carl3$bottom_contact, na.rm = TRUE) / 
                                  sum(carl3$n_stressors, na.rm = TRUE) * 100))

bottom_perc$Percent_of_Total <-paste0(round(bottom_perc$Percent_of_Total, 2), "%")
bottom_perc %>% rename("Coral Observations" = N_Outplanted,
                       "Total Stressors" = Total_Stressors,
                       "Bottom Contact" = Bottom_Contact,
                       "Precent of Total Stressors" = Percent_of_Total) %>%
  pander()

