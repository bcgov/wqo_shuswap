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


# PLOT GROWING SEASON MEANS
# Load fish data to plot with TP
thompson_fish <- read_csv("data/thompson_fish_numbers.csv")

sorrento_box <- ggplot(subset(shuswap_TP_0500123_gs, Year>1999), aes(group = Year, x = COLLECTION_START, y = RESULT_ugL)) +
#facet_wrap(PARAMETER ~ EMS_ID, scales = "free_y")
geom_boxplot() +
   ggtitle("Sorrento Reach") +
    xlab("Date") +
    ylab("Growing Season TP (ug/L)")
 plot(sorrento_box)

salmon_arm_box <- ggplot(subset(shuswap_TP_E206771_gs, Year>1999), aes(group = Year, x = COLLECTION_START, y = RESULT_ugL)) +
   geom_boxplot() +
   ggtitle("Salmon Arm") +
   xlab("Date") +
   ylab("Growing Season TP (ug/L)")
 plot(salmon_arm_box)

sicamous_box <- ggplot(subset(shuswap_TP_0500124_gs, Year>1999), aes(group = Year, x = COLLECTION_START, y = RESULT_ugL)) +
   geom_boxplot() +
   ggtitle("Sicamous Reach") +
   xlab("Date") +
   ylab("Growing Season TP (ug/L)")
 plot(sicamous_box)

 main_arm_point <- ggplot(subset(shuswap_TP_E208723_gs, Year>1999), aes(group = Year, x = COLLECTION_START, y = RESULT_ugL)) +
   geom_point() +
   ggtitle("Main Arm") +
   xlab("Date") +
   ylab("Growing Season TP (ug/L)")
 plot(main_arm_point)
