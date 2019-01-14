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
# Bind together all clean P datasets for the 4 sites
plotbox <- ggplot(subset(shuswap_TP_0500123_gs, Year>1999), aes(group = Year, x = COLLECTION_START, y = RESULT_ugL)) +
#facet_wrap(PARAMETER ~ EMS_ID, scales = "free_y")
geom_boxplot()
plot(plotbox)



# would be great to make box plots for various parameters (P and N) and then a bunch of scatterplots of the other parameters if needed. Or at least load the clean datasheet and then do plot code individually.
# May be able to join all the clean data together if its in the same form and then create a function to make them all together.
plotpoint <- ggplot(clean_TP_0500123_gs, aes(x = Date, y = Value_ugL)) +
  geom_point() +
  ggtitle("Growing season means 0500123") +
  xlab("Date") +
  ylab("Total Phosphorus (mg/L)")
  plot(plotpoint)

