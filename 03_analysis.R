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

## CREATE CSV OF DATA
write.csv(shuswap_df,
'C:/R Projects/wqo_shuswap/data/all_data_shuswap.csv', row.names = FALSE)

# PHOSPHORUS
# Initial Visualization
shuswap_TP <- filter(shuswap_df, PARAMETER == "Phosphorus Total")

## Remove 4 rows of 8/21/2002 and 2/11/2003 that look like they were entered wrong at 100 ug/L. They are entered twice, the second entry at < MDL of 2 ug/L which makes more sense.

## Separate df into growing season (May - October) and non-growing season (November to April)

# Change units from mg/L to ug/L
shuswap_TP <- transform(shuswap_TP, RESULT = RESULT*1000)
colnames(shuswap_TP)[6] <- "RESULT_ugL"

sites <- c("E206771", "0500124", "E208723", "0500123")
sites_P <- filter(shuswap_TP, MONITORING_LOCATION  %in% sites)

for (s in sites){
  P_plots <- filter(sites_P, Sites == s)
  plot <- ggplot(shuswap_TP, aes(x = COLLECTION_START, y = RESULT_ugL)) +
  geom_point() +
  facet_wrap(PARAMETER ~ EMS_ID, scales = "free_y")
  plot(plot)
}


