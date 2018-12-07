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
#
# View and filter out non water samples
# Don't want to use clean_wqdata function as this is for lake data with varying depths per day, and the clean function averages multiple daily measurments.
distinct(all_data_shuswap, SAMPLE_STATE)
shuswap_clean <- filter(all_data_shuswap, SAMPLE_STATE == "Fresh Water")

# View and remove blanks
distinct(all_data_shuswap, SAMPLE_CLASS)
shuswap_clean <- filter(shuswap_clean, SAMPLE_CLASS != "Blank - field")

# Refine dataframe (df) to include a subset of columns for simplified viewing analysis
shuswap_df <- select(shuswap_clean, EMS_ID, MONITORING_LOCATION, COLLECTION_START, PARAMETER, RESULT_LETTER, RESULT, UNIT, SAMPLE_CLASS, UPPER_DEPTH, LOWER_DEPTH)

# You can create a summary list of any column
#parameters <- distinct(shuswap_df, MONITORING_SITE = "0500123", PARAMETER)

##### PHOSPHORUS #####
# Initial Visualization
shuswap_TP <- filter(shuswap_df, PARAMETER == "Phosphorus Total")

# Change units from mg/L to ug/L
shuswap_TP <- transform(shuswap_TP, RESULT = RESULT*1000)
colnames(shuswap_TP)[6] <- "RESULT_ugL"

#Get rid of UNIT column which says 'mg/L'
shuswap_TP <- select(shuswap_TP, -UNIT)

sites <- c("E206771", "0500124", "E208723", "0500123")

# Don't need this I don't think as df only contains the 4 sites I'm wanting to plot
#sites_P <- filter(shuswap_TP, EMS_ID %in% sites)

for (s in sites){
  P_plots <- filter(shuswap_TP, EMS_ID == s)
  plotpoint <- ggplot(P_plots, aes(x = COLLECTION_START, y = RESULT_ugL)) +
  geom_point() +
  ggtitle(s) +
  xlab("Date") +
  ylab("Total Phosphorus (ug/L)")
  plot(plotpoint)
}

# CLEANING UP SITE 0500123 - SORRENTO

# Remove 4 rows of 8/21/2002 and 2/11/2003 that look like they were entered wrong at 100 ug/L. They are entered twice, the second entry at < MDL of 2 ug/L which makes more sense.
# Remove value of 20 (has a < so supposed to be <MDL)
# Include surface samples only (it's just most recent data that have deep water samples)

shuswap_TP_0500123 <- filter(shuswap_TP, EMS_ID == "0500123")
shuswap_TP_0500123 <- shuswap_TP_0500123[-c(2,4,6,8,25,142,173,198), ]

# Change format of the date to remove time

# Separate df into growing season (May - October) and non-growing season (November to April)

# Average values per day
#
##### NITROGEN #####

##### DISSOLVED OXYGEN #####

##### CHLOROPHYLL A

##### SECCHI DEPTH ######

##### E. coli #####


# BOXPLOT
plotbox <- ggplot(shuswap_TP, aes(x = MONITORING_LOCATION, y = RESULT_ugL)) +
#facet_wrap(PARAMETER ~ EMS_ID, scales = "free_y")
geom_boxplot()
plot(plotbox)

## CREATE CSV OF CLEAN DATA (DO THIS FOR RAW DF, AND ALL CLEANED UP PARAMETER DFs)
#write.csv(shuswap_TP,
#'C:/R Projects/wqo_shuswap/data/TP_shuswap.csv', row.names = FALSE)
