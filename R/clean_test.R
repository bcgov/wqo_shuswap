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
## LOAD .CSV DATA
all_data_shuswap <- read_csv("data/all_data_shuswap.csv")

## FILTER OUT NON-WATER SAMPLES
all_data_shuswap <- filter(all_data_shuswap, SAMPLE_STATE == "Fresh Water")

## TIDY DATASET
## Tidies water quality data downloaded from EMS database using the bcgov/wqbc package.
## It retains and renames required columns and sets the timezone to PST.
## It sets values that are flagged as being less than the minimum detection limit (MDL) to the MDL.
## Remove blanks and reference samples
shuswap_tidy <- tidy_ems_data(all_data_shuswap, cols = c("UPPER_DEPTH", "LOWER_DEPTH"), mdl_action = "mdl")
distinct(all_data_shuswap, SAMPLE_CLASS)
shuswap_tidy <- filter(shuswap_tidy, SAMPLE_CLASS != "Blank - field")
shuswap_tidy <- filter(shuswap_tidy, SAMPLE_CLASS != "Reference")

## You can create a summary list of any column
Variables <- distinct(shuswap_tidy, Station = "0500123", Variable)

##### PHOSPHORUS #####
## Initial Visualization
shuswap_TP <- filter(shuswap_tidy, Variable == "Phosphorus Total")

## Change units from mg/L to ug/L
shuswap_TP <- transform(shuswap_TP, Value = Value*1000)
colnames(shuswap_TP)[6] <- "Value_ugL"

## Delete'Units' column which says 'mg/L'
## Delete 'DetectionLimit' column which is in mg/L
shuswap_TP <- select(shuswap_TP, -Units)
#shuswap_TP <- select(shuswap_TP, -DetectionLimit)

## CLEANING UP SITE 0500123 - SORRENTO
## Filter out site df
## Remove 4 rows of 8/21/2002 and 2/11/2003 that look like they were entered wrong at 100 ug/L. According to Kevin, these are likely metals results that got lumped into the P test results. These days are entered twice, the second entry at < MDL of 2 ug/L which would be the P MDL.
## Remove value of 20 (has a < so supposed to be <MDL)
## Include surface samples only (it's just most recent data that have deep water samples)
shuswap_TP_0500123 <- filter(shuswap_TP, EMS_ID == "0500123")
shuswap_TP_0500123 <- shuswap_TP_0500123[-c(2,4,6,8,25,118,141,172,197), ]

## Change format of the date to remove time
shuswap_TP_0500123$DateTime <- as.Date(shuswap_TP_0500123$DateTime, format = "%Y %m %d")
clean_0500123_TP <- clean_wqdata(shuswap_TP_0500123, by = "EMS_ID", max_cv = Inf, sds = 10, ignore_undetected = TRUE, large_only = TRUE, delete_outliers = TRUE, remove_blanks = TRUE, messages = TRUE)


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



# Separate df into growing season (May - October) and non-growing season (November to April)
shuswap_TP_0500123$Month <- as.character(format(shuswap_TP_0500123$COLLECTION_START, '%b'))
shuswap_TP_0500123_gs <- filter(shuswap_TP_0500123, Month == "May"|Month == "Jun"| Month == "Jul" |Month == "Aug"| Month == "Sept"| Month == "Oct")

# PLOT GROWING SEASON MEANS
plotbox <- ggplot(shuswap_TP, aes(x = MONITORING_LOCATION, y = RESULT_ugL)) +
#facet_wrap(PARAMETER ~ EMS_ID, scales = "free_y")
geom_boxplot()
plot(plotbox)

## CREATE CSV OF CLEAN DATA (DO THIS FOR RAW DF, AND ALL CLEANED UP PARAMETER DFs)
#write.csv(shuswap_TP_0500123,
#'C:/R Projects/wqo_shuswap/data/TP_shuswap.csv', row.names = FALSE)



##### NITROGEN #####

##### DISSOLVED OXYGEN #####

##### CHLOROPHYLL A

##### SECCHI DEPTH ######

##### E. coli #####


