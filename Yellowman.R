yellowman3A <- read_excel("Yellowman Acer Monitoring Database.xlsx", 
                          sheet = 2, 
                          range = 'C2:Q136',
                          col_names = c("date", "duration", "spieces", "geno", "cluster", 
                                        "n_outplanted","n_present", "n_lost", "health_0",
                                        "health_q1", "health_q2", "health_q3", "health_q4", "health_100",
                                        "pred_stressor"))

### Calculating coral losses by geno
yellowman_losses3A <- yellowman3A %>%
  group_by(spieces, geno) %>%
  summarize(num_outplanted = sum(n_outplanted),
            num_survived = sum(n_present),
            mortality_rate = (num_outplanted - num_survived) / num_outplanted) %>%
  arrange(desc(mortality_rate))
print(yellowman_losses3A, n = 100)

### Calculating weighted coral health by geno
yellowman_health3A <- yellowman3A %>%
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

yellowman_health_agg3A <- yellowman_health3A %>%
  group_by(geno) %>%
  summarize(avg_health = mean(weighted_health)) %>%
  arrange(desc(avg_health))


### Table of net losses and weighted health by geno
yellowman_loss_health_table3A <- yellowman_losses3A %>%
  inner_join(yellowman_health_agg3A, by = "geno") %>%
  select(spieces, geno, avg_health, mortality_rate) %>%
  arrange(desc(avg_health)) 
print(yellowman_loss_health_table3A, n = 100)


### boxplot of tissue health by geno
ggplot(yellowman_health3A, aes(x = factor(geno), y = weighted_health)) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(0.35, 1.0), breaks = seq(0.35, 1.0, by = 0.05)) +
  labs(x = "Acer Geno", 
       y = "Percent of Healthy Tissue",
       title = "Yellowman A",
       subtitle = "3-Month Survey",
       caption =  "Survey Date: 2021-06-23")