yellowman3A <- read_excel("Yellowman Acer Monitoring Database.xlsx", 
                          sheet = 2, 
                          range = 'C2:Q136',
                          col_names = c("date", "duration", "species", "geno", "cluster", 
                                        "n_outplanted","n_present", "n_lost", "health_0",
                                        "health_q1", "health_q2", "health_q3", "health_q4", "health_100",
                                        "pred_stressor"))

yellowman6A <- read_excel("Yellowman Acer Monitoring Database.xlsx", 
                          sheet = 3, 
                          range = 'A2:P120',
                          col_names = c("date", "duration", "species", "geno", "cluster", 
                                        "n_outplanted","n_present", "n_lost", "health_0",
                                        "health_q1", "health_q2", "health_q3", "health_q4", "health_100",
                                        "avg_health", "pred_stressor"))

### Calculating 3A coral losses by geno
yellowman_losses3A <- yellowman3A %>%
  group_by(species, geno) %>%
  summarize(num_outplanted = sum(n_outplanted),
            num_survived = sum(n_present),
            mortality_rate = ((num_outplanted - num_survived) / num_outplanted) * 100) %>%
  arrange(desc(mortality_rate))
print(yellowman_losses3A, n = 100)

### Calculating 3A weighted coral health by geno
yellowman_health3A <- yellowman3A %>%
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

yellowman_health_agg3A <- yellowman_health3A %>%
  group_by(geno) %>%
  summarize(avg_health = mean(weighted_health)) %>%
  arrange(desc(avg_health))


### Table of 3A net losses and weighted health by geno
yellowman_loss_health_table3A <- yellowman_losses3A %>%
  inner_join(yellowman_health_agg3A, by = "geno") %>%
  select(species, geno, avg_health, mortality_rate) %>%
  arrange(desc(avg_health)) 
print(yellowman_loss_health_table3A, n = 100)


### boxplot of 3A tissue health by geno
ggplot(yellowman_health3A, aes(x = factor(geno), y = weighted_health)) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(35, 100), breaks = seq(35, 100, by = 5)) +
  labs(x = "Acer Geno", 
       y = "Percent of Healthy Tissue",
       title = "Yellowman A",
       subtitle = "3-Month Survey",
       caption =  "Survey Date: 2021-06-23")

################ 6A Review #################

### Calculating 6A coral losses by geno
yellowman_losses6A <- yellowman6A %>%
  group_by(species, geno) %>%
  summarize(num_outplanted = sum(n_outplanted),
            num_survived = sum(n_present),
            mortality_rate = ((num_outplanted - num_survived) / num_outplanted)*100) %>%
  arrange(desc(mortality_rate))
print(yellowman_losses6A, n = 100)

### Calculating 6A weighted coral health by geno
yellowman_health6A <- yellowman6A %>%
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

yellowman_health_agg6A <- yellowman_health6A %>%
  group_by(geno) %>%
  summarize(avg_health = mean(weighted_health)) %>%
  arrange(desc(avg_health))


### Table of 6A net losses and weighted health by geno
yellowman_loss_health_table6A <- yellowman_losses6A %>%
  inner_join(yellowman_health_agg6A, by = "geno") %>%
  select(species, geno, avg_health, mortality_rate) %>%
  arrange(desc(avg_health)) 
print(yellowman_loss_health_table6A, n = 100)

### boxplot of 6A tissue health by geno
ggplot(yellowman_health6A, aes(x = factor(geno), y = weighted_health)) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 5)) +
  labs(x = "Acer Genotypes", 
       y = "Percent of Healthy Tissue",
       title = "Yellowman A",
       subtitle = "6-Month Survey",
       caption =  "Survey Date: 2021-10-12")

#### Comparing A3 with A6
yellowmanA_spread <- yellowman_loss_health_table3A %>%
  left_join(yellowman_loss_health_table6A, 
            by = c("species", "geno"), suffix = c("3", "6")) %>%
  mutate(perc_change = avg_health6 - avg_health3) %>%
  arrange(desc(perc_change))
print(yellowmanA_spread, n = 100)

yellowmanA_long <- yellowmanA_spread %>% 
  select(species, geno, avg_health3, avg_health6, perc_change) %>%
  pivot_longer("avg_health3":"avg_health6", 
                                   names_to = "survey_time",
                                   values_to = "values")

ggplot(yellowmanA_long, aes(x = factor(geno), y = values)) +
    geom_point(aes(color = survey_time)) +
    geom_line(aes(group = geno)) +
    scale_y_continuous(limits = c(0, 100), 
                       breaks = seq(45, 100, by = 5)) +
  scale_color_manual(labels = c("June 2021", "October 2021"), 
                       values = c("dodgerblue1", "magenta3")) +
    labs(x = "Acer Genotypes",
         y = "Percent of Healthy Tissue",
         color = "Survey",
         legend = c("June 2021", "October 2021"),
         title = "Yellowman: Average Change in Tissue Health",
         subtitle = "Area A") +
    theme_light()
