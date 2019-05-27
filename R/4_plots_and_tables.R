# Copyright 2018 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

####################################### ALL ##########################################

## Sum table for all sites and parameters. Need all clean df's for all parameters first.



###################################### PHOSPHORUS ####################################

# TP SUMMARY TABLE for growing season means
TP_sum_table <- TP_gs %>%
  group_by(EMS_ID) %>%
  summarise(Min=min(RESULT_mm), Max=max(RESULT_mm), Median=median(RESULT_mm), Mean=mean(RESULT_mm), n=length(RESULT_mm))

# TP line plot for each site for growing season monthly means data since 2000. Epilimion data.
TP_year <- TP_gs %>%
  filter(UPPER_DEPTH < 11) %>%
  filter(year(COLLECTION_START) > 1999) %>%
    group_by(EMS_ID, year(COLLECTION_START)) %>%
    mutate(avg_phos = mean(RESULT_mm, na.rm = TRUE)) %>%
    ungroup() %>%
  ggplot(aes(x = year(COLLECTION_START), y = avg_phos, color = MONITORING_LOCATION, group = MONITORING_LOCATION)) +
    geom_line() +
    geom_point() +
    labs(x = "Date", y = ("Total Phosphorus (ug/L)"), title = paste(unique(TP_gs$MONITORING_LOCATION), ": Total Phosphorus")) +
  geom_hline(yintercept = 10,linetype="dashed",color="red")
plot(TP_year)

# Salmon Arm Box Plot
salmon_arm_box <- filter(TP_gs, EMS_ID == "E206771") %>%
  filter(UPPER_DEPTH < 11) %>%
  filter(year(COLLECTION_START) > 1999) %>%
  ggplot(aes(group = year(COLLECTION_START), x = year(COLLECTION_START), y = RESULT_mm)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 15), colour = "red", linetype = "dashed") +
  ggtitle("Salmon Arm - E206771") +
  xlab("Date") +
  ylab("Growing Season TP (ug/L)")
plot(salmon_arm_box)

# Salmon Arm Point Plot
salmon_arm_point <- filter(TP_gs, EMS_ID == "E206771") %>%
  filter(UPPER_DEPTH < 11) %>%
  filter(year(COLLECTION_START) > 1999) %>%
  ggplot(aes(group = year(COLLECTION_START), x = year(COLLECTION_START), y = RESULT_mm)) +
  geom_point() +
  geom_hline(aes(yintercept = 15), colour = "red", linetype = "dashed") +
  ggtitle("Salmon Arm - E206771") +
  xlab("Date") +
  ylab("Growing Season TP (ug/L)")
plot(salmon_arm_point)

# Sicamous Box Plot
sicamous_box <- filter(TP_gs, EMS_ID == "0500124") %>%
  filter(UPPER_DEPTH < 11) %>%
  filter(year(COLLECTION_START) > 1999) %>%
  ggplot(aes(group = year(COLLECTION_START), x = year(COLLECTION_START), y = RESULT_mm)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 10), colour = "red", linetype = "dashed") +
  ggtitle("Sicamous Reach - 0500124") +
  xlab("Date") +
  ylab("Growing Season TP (ug/L)")
plot(sicamous_box)

# Main Arm Point Plot
main_arm_point <- filter(TP_gs, EMS_ID == "E208723") %>%
  filter(UPPER_DEPTH < 11) %>%
  filter(year(COLLECTION_START) > 1999) %>%
  ggplot(aes(group = year(COLLECTION_START), x = year(COLLECTION_START), y = RESULT_mm)) +
  geom_point() +
  geom_hline(aes(yintercept = 10), colour = "red", linetype = "dashed") +
  ggtitle("Main Arm - E208723") +
  xlab("Date") +
  ylab("Growing Season TP (ug/L)")
plot(main_arm_point)

# PLOT SALMON RUN NUMBERS TO COMPARE TO TP
# thompson_fish <- read_csv("data/thompson_fish_numbers.csv")
# fish <- ggplot(subset(thompson_fish, year>1994), aes(x = year, y = number_of_fish, color = location)) +
#   geom_line() +
#   #geom_dl(aes(label = location), method = "last.points") +
#   #scale_x_date(labels = date_format("%Y"), date_breaks = "1 year") +
#   xlab("Year of Fish Run") +
#   ylab("Number of Salmon")
# plot(fish)
# ggsave(filename = "fish_thompson.png", plot = fish, path = 'C:/R Projects/wqo_shuswap/outputs', width = 9, height = 5, units= "in")
