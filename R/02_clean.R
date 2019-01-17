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
# This script loads the clean raw data, allows for initital visualization of the data for each parameter for each monitoring site and then set-up of various dataframe structures to allow for plots in 03_analysis to call the dataframes.
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

# Add Month, Day, Year and Time columns
all_data_shuswap$Month <- as.character(format(all_data_shuswap$COLLECTION_START, '%b'))
all_data_shuswap$Day <- as.character(format(all_data_shuswap$COLLECTION_START, '%d'))
all_data_shuswap$Year <- as.character(format(all_data_shuswap$COLLECTION_START, '%Y'))
all_data_shuswap$Time <- as.character(format(all_data_shuswap$COLLECTION_START, '%H:%M:%S'))

############################## PHOSPHORUS #############################################
# Initial Visualization
TP <- filter(all_data_shuswap, PARAMETER == "Phosphorus Total")

# Change units from mg/L to ug/L
TP <- transform(TP, RESULT = RESULT*1000)
colnames(TP)[6] <- "RESULT_ugL"

# Get rid of UNIT column which says 'mg/L'
TP <- select(TP, -UNIT)

sites <- c("E206771", "0500124", "E208723", "0500123")

for (s in sites){
  TP_plots <- filter(TP, EMS_ID == s)
  plotpoint <- ggplot(TP_plots, aes(x = COLLECTION_START, y = RESULT_ugL)) +
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
TP_0500123 <- filter(TP, EMS_ID == "0500123")
TP_0500123 <- TP_0500123[-c(2,4,22,115,141,172,197), ]

# Remove time from COLLECTION_START and average samples (regular and repeat of surface samples) taken on the same day.
TP_0500123_avg <- TP_0500123 %>%
  mutate(COLLECTION_START = date(COLLECTION_START)) %>%
  group_by(COLLECTION_START) %>%
  summarize(RESULT_ugL_avg = mean(RESULT_ugL))

# Separate df into growing season (May - October) by adding Month and Year columns
TP_0500123_avg$Month <- as.character(format(TP_0500123_avg$COLLECTION_START, '%b'))
TP_0500123_avg$Year <- as.character(format(TP_0500123_avg$COLLECTION_START, '%Y'))
TP_0500123_gs <- filter(TP_0500123_avg, Month == "May"|Month == "Jun"| Month == "Jul" |Month == "Aug"| Month == "Sept"| Month == "Oct")

# CLEANING UP SITE E206771 - SALMON ARM REACH
# Removing deep lake values and rows where the result has a result letter but the result is entered as 30 and 100 ug/L whereas I think it should be 3ug/L and 10 ug/L.
TP_E206771 <- filter(TP, EMS_ID == "E206771")
TP_E206771 <- TP_E206771[-c(2,4,6,9,11,13,15,60,114,123,195), ]

# Remove time from COLLECTION_START and average samples (regular and repeat of surface samples) taken on the same day.
TP_E206771_avg <- TP_E206771 %>%
  mutate(COLLECTION_START = date(COLLECTION_START)) %>%
  group_by(COLLECTION_START) %>%
  summarize(RESULT_ugL_avg = mean(RESULT_ugL))

# Separate df into growing season (May - October) by adding Month and Year columns
TP_E206771_avg$Month <- as.character(format(TP_E206771_avg$COLLECTION_START, '%b'))
TP_E206771_avg$Year <- as.character(format(TP_E206771_avg$COLLECTION_START, '%Y'))
TP_E206771_gs <- filter(TP_E206771_avg, Month == "May"|Month == "Jun"| Month == "Jul" |Month == "Aug"| Month == "Sept"| Month == "Oct")

# CLEANING UP SITE 0500124 - SICAMOUS REACH
# Removed lower depth samples and a couple MDL errors
TP_0500124 <- filter(TP, EMS_ID == "0500124")
TP_0500124 <- TP_0500124[-c(2,4,161,224), ]

# Remove time from COLLECTION_START and average samples (regular and repeat of surface samples) taken on the same day.
TP_0500124_avg <- TP_0500124 %>%
  mutate(COLLECTION_START = date(COLLECTION_START)) %>%
  group_by(COLLECTION_START) %>%
  summarize(RESULT_ugL_avg = mean(RESULT_ugL))

# Separate df into growing season (May - October) and non-growing season (November to April) by adding Month and Year columns
TP_0500124_avg$Month <- as.character(format(TP_0500124_avg$COLLECTION_START, '%b'))
TP_0500124_avg$Year <- as.character(format(TP_0500124_avg$COLLECTION_START, '%Y'))
TP_0500124_gs <- filter(TP_0500124_avg, Month == "May"|Month == "Jun"| Month == "Jul" |Month == "Aug"| Month == "Sept"| Month == "Oct")

# CLEANING UP SITE E208723 - MAIN ARM
## Removed lower depth samples and a couple MDL errors
TP_E208723 <- filter(TP, EMS_ID == "E208723")
TP_E208723 <- TP_E208723[-c(50,52,55,64,73), ]

# Remove time from COLLECTION_START and average samples (regular and repeat of surface samples) taken on the same day.
TP_E208723_avg <- TP_E208723 %>%
  mutate(COLLECTION_START = date(COLLECTION_START)) %>%
  group_by(COLLECTION_START) %>%
  summarize(RESULT_ugL_avg = mean(RESULT_ugL))

# Separate df into growing season (May - October) and non-growing season (November to April) by adding Month and Year columns
TP_E208723_avg$Month <- as.character(format(TP_E208723_avg$COLLECTION_START, '%b'))
TP_E208723_avg$Year <- as.character(format(TP_E208723_avg$COLLECTION_START, '%Y'))
TP_E208723_gs <- filter(TP_E208723_avg, Month == "May"|Month == "Jun"| Month == "Jul" |Month == "Aug"| Month == "Sept"| Month == "Oct")

# Join clean TP data from all 4 sites together
TP_clean <- bind_rows(TP_0500123, TP_0500124, TP_E206771, TP_E208723)

# CREATE CSV OF CLEAN TP DATA
write.csv(shuswap_TP_E208723,
       'C:/R Projects/wqo_shuswap/data/TP_clean.csv', row.names = FALSE)#
#
#
#
################################### NITROGEN #########################################

##### DISSOLVED OXYGEN #####

##### CHLOROPHYLL A

##### SECCHI DEPTH ######

##### E. coli #####




