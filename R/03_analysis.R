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

###################################### PHOSPHORUS ####################################

# TP SUMMARY TABLE of clean, daily averaged data for the entire year (not monthly means)
TP_sum_table <- TP_clean %>%
  group_by(EMS_ID, MONITORING_LOCATION) %>%
  summarise(Min=min(RESULT_ugL_avg), Max=max(RESULT_ugL_avg), Median=median(RESULT_ugL_avg), n=length(RESULT_ugL_avg))

# TP line plot for each site for all year data (not just growing season). Can change to just look at growing season means per month over a year by changing the dataframe - would have to bind together each site's growing season dataframe.
sites <- c("E206771", "0500124", "E208723", "0500123")

for (s in sites){
  TP_year <- filter(TP_mm_gs, EMS_ID == s)
  plotTPyear <- ggplot(subset(TP_year, Year>2009), aes(x = Month, y = RESULT_month_mean, color = Year, group = Year)) +
    geom_line() +
    geom_point() +
    ggtitle(s) +
    scale_x_discrete(limits = month.abb) +
    xlab("Month") +
    ylab("Total Phosphorus (ug/L)")
  plot(plotTPyear)
}

# PLOT SALMON RUN NUMBERS TO COMPARE TO TP
thompson_fish <- read_csv("data/thompson_fish_numbers.csv")
fish <- ggplot(subset(thompson_fish, year>1994), aes(x = year, y = number_of_fish, color = location)) +
  geom_line() +
  #geom_dl(aes(label = location), method = "last.points") +
  #scale_x_date(labels = date_format("%Y"), date_breaks = "1 year") +
  xlab("Year of Fish Run") +
  ylab("Number of Salmon")
plot(fish)
ggsave(filename = "fish_thompson.png", plot = fish, path = 'C:/R Projects/wqo_shuswap/outputs', width = 9, height = 5, units= "in")

# PLOT GROWING SEASON MEANS
#
# Sorrento Box Plot
sorrento_box <- ggplot(subset(TP_0500123_gs, Year>1999), aes(group = Year, x = Year, y = RESULT_ugL_avg)) +
#facet_wrap(PARAMETER ~ EMS_ID, scales = "free_y")
geom_boxplot() +
  geom_hline(aes(yintercept = 10), colour = "red", linetype = "dashed") +
   ggtitle("Sorrento Reach - 0500123") +
    xlab("Date") +
    ylab("Growing Season TP (ug/L)")
  #scale_x_date(labels = date_format("%Y")) +
 plot(sorrento_box)

# Salmon Arm Box Plot
salmon_arm_box <- ggplot(subset(TP_E206771_gs, Year>1999), aes(group = Year, x = Year, y = RESULT_ugL_avg)) +
   geom_boxplot() +
  geom_hline(aes(yintercept = 15), colour = "red", linetype = "dashed") +
   ggtitle("Salmon Arm - E206771") +
   xlab("Date") +
   ylab("Growing Season TP (ug/L)")
plot(salmon_arm_box)

# Salmon Arm Point Plot
salmon_arm_point <- ggplot(subset(TP_E206771_gs, Year>1999), aes(group = Year, x = Year, y = RESULT_ugL_avg)) +
   geom_point() +
   geom_hline(aes(yintercept = 15), colour = "red", linetype = "dashed") +
   ggtitle("Salmon Arm - E206771") +
   xlab("Date") +
   ylab("Growing Season TP (ug/L)")
 plot(salmon_arm_point)

# Sicamous Box Plot
sicamous_box <- ggplot(subset(TP_0500124_gs, Year>1999), aes(group = Year, x = Year, y = RESULT_ugL_avg)) +
   geom_boxplot() +
  geom_hline(aes(yintercept = 10), colour = "red", linetype = "dashed") +
   ggtitle("Sicamous Reach - 0500124") +
   xlab("Date") +
   ylab("Growing Season TP (ug/L)")
 plot(sicamous_box)

# Main Arm Point Plot
main_arm_point <- ggplot(subset(TP_E208723_gs, Year>1999), aes(group = Year, x = Year, y = RESULT_ugL_avg)) +
   geom_point() +
   geom_hline(aes(yintercept = 10), colour = "red", linetype = "dashed") +
   ggtitle("Main Arm - E208723") +
   xlab("Date") +
   ylab("Growing Season TP (ug/L)")
 plot(main_arm_point)
