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
# distinct(all_data_shuswap, SAMPLE_STATE)
# shuswap_clean <- filter(all_data_shuswap, SAMPLE_STATE == "Fresh Water")
#
# # View and remove blanks
# distinct(all_data_shuswap, SAMPLE_CLASS)
# shuswap_clean <- filter(shuswap_clean, SAMPLE_CLASS != "Blank - field")
#
# # Refine dataframe (df) to include a subset of columns for simplified viewing analysis
# shuswap_df <- select(shuswap_clean, EMS_ID, MONITORING_LOCATION, COLLECTION_START, PARAMETER, RESULT_LETTER, RESULT, UNIT, SAMPLE_CLASS, UPPER_DEPTH, LOWER_DEPTH)
#
# # You can create a summary list of any column
# distinct(shuswap_df, MONITORING_SITE = "0500123", PARAMETER)
# #
# # CREATE CSV OF ALL RAW CLEAN DATA
# write.csv(shuswap_df, 'C:/R Projects/wqo_shuswap/data/all_data_shuswap_clean.csv', row.names = FALSE)

# Just load csv each time so don't have to download from EMS. Ensures dataset remains consistent.
all_data_shuswap <- read_csv("data/all_data_shuswap_clean.csv")

##### PHOSPHORUS #####
# Initial Visualization
shuswap_TP <- filter(all_data_shuswap, PARAMETER == "Phosphorus Total")

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

# CLEANING UP SITE 0500123 - SORRENTO REACH
# Remove 4 rows of 8/21/2002 and 2/11/2003 that look like they were entered wrong at 100 ug/L. According to Kevin, these are likely metals results that got lumped into the P test results. These days are entered twice, the second entry at < MDL of 2 ug/L which would be the P MDL.
# Remove value of 20 (has a < so supposed to be <MDL)
# Include surface samples only (it's just most recent data that have deep water samples)

shuswap_TP_0500123 <- filter(shuswap_TP, EMS_ID == "0500123")
shuswap_TP_0500123 <- shuswap_TP_0500123[-c(2,4,22,115,141,172,197), ]

# Change format of the date to remove time and average samples taken on the same day
shuswap_TP_0500123 <- shuswap_TP_0500123 %>%
  mutate(COLLECTION_START = date(COLLECTION_START)) %>%
  group_by(COLLECTION_START) %>%
  summarize(RESULT_ugL = sum(RESULT_ugL))

## CREATE CSV OF CLEAN DATA (DO THIS FOR RAW DF, AND ALL CLEANED UP PARAMETER DFs)
#write.csv(shuswap_TP_0500123,
 #         'C:/R Projects/wqo_shuswap/data/shuswap_TP_0500123.csv', row.names = FALSE)

# Separate df into growing season (May - October) and non-growing season (November to April) by adding Month and Year columns
shuswap_TP_0500123$Month <- as.character(format(shuswap_TP_0500123$COLLECTION_START, '%b'))
shuswap_TP_0500123$Year <- as.character(format(shuswap_TP_0500123$COLLECTION_START, '%Y'))
shuswap_TP_0500123_gs <- filter(shuswap_TP_0500123, Month == "May"|Month == "Jun"| Month == "Jul" |Month == "Aug"| Month == "Sept"| Month == "Oct")

# CLEANING UP SITE E206771 - SALMON ARM REACH
# Removing deep lake values and rows where the result has a result letter but the result is entered as 30 and 100 ug/L whereas I think it should be 3ug/L and 10 ug/L.

shuswap_TP_E206771 <- filter(shuswap_TP, EMS_ID == "E206771")
shuswap_TP_E206771 <- shuswap_TP_E206771[-c(2,4,6,9,11,13,15,60,114,123,195), ]

# Change format of the date to remove time and average samples taken on the same day
shuswap_TP_E206771 <- shuswap_TP_E206771 %>%
  mutate(COLLECTION_START = date(COLLECTION_START)) %>%
  group_by(COLLECTION_START) %>%
  summarize(RESULT_ugL = sum(RESULT_ugL))

## CREATE CSV OF CLEAN DATA (DO THIS FOR RAW DF, AND ALL CLEANED UP PARAMETER DFs)
#write.csv(shuswap_TP_E206771,
 #         'C:/R Projects/wqo_shuswap/data/shuswap_TP_E206771.csv', row.names = FALSE)

# Separate df into growing season (May - October) and non-growing season (November to April) by adding Month and Year columns
shuswap_TP_E206771$Month <- as.character(format(shuswap_TP_E206771$COLLECTION_START, '%b'))
shuswap_TP_E206771$Year <- as.character(format(shuswap_TP_E206771$COLLECTION_START, '%Y'))
shuswap_TP_E206771_gs <- filter(shuswap_TP_E206771, Month == "May"|Month == "Jun"| Month == "Jul" |Month == "Aug"| Month == "Sept"| Month == "Oct")

# CLEANING UP SITE 0500124 - SICAMOUS REACH
# Removed lower depth samples and a couple MDL errors
shuswap_TP_0500124 <- filter(shuswap_TP, EMS_ID == "0500124")
shuswap_TP_0500124 <- shuswap_TP_0500124[-c(2,4,161,224), ]

# Change format of the date to remove time and average samples taken on the same day
shuswap_TP_0500124 <- shuswap_TP_0500124 %>%
  mutate(COLLECTION_START = date(COLLECTION_START)) %>%
  group_by(COLLECTION_START) %>%
  summarize(RESULT_ugL = sum(RESULT_ugL))

## CREATE CSV OF CLEAN DATA (DO THIS FOR RAW DF, AND ALL CLEANED UP PARAMETER DFs)
#write.csv(shuswap_TP_0500124,
 #        'C:/R Projects/wqo_shuswap/data/shuswap_TP_0500124.csv', row.names = FALSE)

# Separate df into growing season (May - October) and non-growing season (November to April) by adding Month and Year columns
shuswap_TP_0500124$Month <- as.character(format(shuswap_TP_0500124$COLLECTION_START, '%b'))
shuswap_TP_0500124$Year <- as.character(format(shuswap_TP_0500124$COLLECTION_START, '%Y'))
shuswap_TP_0500124_gs <- filter(shuswap_TP_0500124, Month == "May"|Month == "Jun"| Month == "Jul" |Month == "Aug"| Month == "Sept"| Month == "Oct")
#
# CLEANING UP SITE E208723 - MAIN ARM
## Removed lower depth samples and a couple MDL errors
shuswap_TP_E208723 <- filter(shuswap_TP, EMS_ID == "E208723")
shuswap_TP_E208723 <- shuswap_TP_E208723[-c(50,52,55,64,73), ]

# Change format of the date to remove time and average samples taken on the same day
shuswap_TP_E208723 <- shuswap_TP_E208723 %>%
  mutate(COLLECTION_START = date(COLLECTION_START)) %>%
  group_by(COLLECTION_START) %>%
  summarize(RESULT_ugL = sum(RESULT_ugL))

## CREATE CSV OF CLEAN DATA (DO THIS FOR RAW DF, AND ALL CLEANED UP PARAMETER DFs)
#write.csv(shuswap_TP_E208723,
 #       'C:/R Projects/wqo_shuswap/data/shuswap_TP_E208723.csv', row.names = FALSE)

# Separate df into growing season (May - October) and non-growing season (November to April) by adding Month and Year columns
shuswap_TP_E208723$Month <- as.character(format(shuswap_TP_E208723$COLLECTION_START, '%b'))
shuswap_TP_E208723$Year <- as.character(format(shuswap_TP_E208723$COLLECTION_START, '%Y'))
shuswap_TP_E208723_gs <- filter(shuswap_TP_E208723, Month == "May"|Month == "Jun"| Month == "Jul" |Month == "Aug"| Month == "Sept"| Month == "Oct")
#
#
#
#
##### NITROGEN #####

##### DISSOLVED OXYGEN #####

##### CHLOROPHYLL A

##### SECCHI DEPTH ######

##### E. coli #####




