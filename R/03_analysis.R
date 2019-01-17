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

#Phosphorus Summary Table
salmon_arm_sum <- shuswap_TP_E206771 %>%
  #group_by(, Units, ResultLetter) %>%
  summarise(Min=min(RESULT_ugL_avg), Max=max(RESULT_ugL_avg), Median=median(RESULT_ugL_avg), n=length(RESULT_ugL_avg))
print(salmon_arm_sum)

# PLOT GROWING SEASON MEANS
# Load fish data to plot with TP
thompson_fish <- read_csv("data/thompson_fish_numbers.csv")

sorrento_box <- ggplot(subset(shuswap_TP_0500123_gs, Year>1999), aes(group = Year, x = Year, y = RESULT_ugL_avg)) +
#facet_wrap(PARAMETER ~ EMS_ID, scales = "free_y")
geom_boxplot() +
  geom_hline(aes(yintercept = 10), colour = "red", linetype = "dashed") +
   ggtitle("Sorrento Reach - 0500123") +
    xlab("Date") +
    ylab("Growing Season TP (ug/L)")
  #scale_x_date(labels = date_format("%Y")) +
 plot(sorrento_box)

salmon_arm_box <- ggplot(subset(shuswap_TP_E206771_gs, Year>1999), aes(group = Year, x = Year, y = RESULT_ugL_avg)) +
   geom_boxplot() +
  geom_hline(aes(yintercept = 15), colour = "red", linetype = "dashed") +
   ggtitle("Salmon Arm - E206771") +
   xlab("Date") +
   ylab("Growing Season TP (ug/L)")
plot(salmon_arm_box)


 salmon_arm_point <- ggplot(subset(shuswap_TP_E206771_gs, Year>1999), aes(group = Year, x = Year, y = RESULT_ugL_avg)) +
   geom_point() +
   geom_hline(aes(yintercept = 15), colour = "red", linetype = "dashed") +
   ggtitle("Salmon Arm - E206771") +
   xlab("Date") +
   ylab("Growing Season TP (ug/L)")
 plot(salmon_arm_point)


sicamous_box <- ggplot(subset(shuswap_TP_0500124_gs, Year>1999), aes(group = Year, x = Year, y = RESULT_ugL_avg)) +
   geom_boxplot() +
  geom_hline(aes(yintercept = 10), colour = "red", linetype = "dashed") +
   ggtitle("Sicamous Reach - 0500124") +
   xlab("Date") +
   ylab("Growing Season TP (ug/L)")
 plot(sicamous_box)

 main_arm_point <- ggplot(subset(shuswap_TP_E208723_gs, Year>1999), aes(group = Year, x = Year, y = RESULT_ugL_avg)) +
   geom_point() +
   geom_hline(aes(yintercept = 10), colour = "red", linetype = "dashed") +
   ggtitle("Main Arm - E208723") +
   xlab("Date") +
   ylab("Growing Season TP (ug/L)")
 plot(main_arm_point)

 fish <- ggplot(subset(thompson_fish, year>1994), aes(x = year, y = number_of_fish, color = location)) +
   geom_line() +
   #geom_dl(aes(label = location), method = "last.points") +
   #scale_x_date(labels = date_format("%Y"), date_breaks = "1 year") +
   xlab("Year of Fish Run") +
   ylab("Number of Salmon")
plot(fish)
ggsave(filename = "fish_thompson.png", plot = fish, path = 'C:/R Projects/wqo_shuswap/outputs', width = 9, height = 5, units= "in")

# Plot total P to figure out spring overturn P concentration
# sorrento_line <- ggplot(subset(shuswap_TP_0500123, Year>2009), aes(x = COLLECTION_START, y = RESULT_ugL)) +
#   geom_line() +
#   #geom_hline(aes(yintercept = 15), colour = "red", linetype = "dashed") +
#   ggtitle("Sorrento Reach-0500123 TP (ug/L)") +
#   xlab("Date") +
#   ylab("Total Phosphorus (ug/L)")
# plot(sorrento_line)
#
