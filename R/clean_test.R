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

## Plot all sites together for an intial view of the data
sites <- c("E206771", "0500124", "E208723", "0500123")

# Don't need this I don't think as df only contains the 4 sites I'm wanting to plot
#sites_P <- filter(shuswap_TP, EMS_ID %in% sites)
for (s in sites){
  P_plots <- filter(shuswap_TP, EMS_ID == s)
  plotpoint <- ggplot(P_plots, aes(x = DateTime, y = Value)) +
  geom_point() +
  ggtitle(s) +
  xlab("Date") +
  ylab("Total Phosphorus (mg/L)")
  plot(plotpoint)
}

## CLEANING UP SITE 0500123 - SORRENTO
## Filter out site df
## Remove 4 rows of 8/21/2002 and 2/11/2003 that look like they were entered wrong at 100 ug/L. According to Kevin, these are likely metals results that got lumped into the P test results. These days are entered twice, the second entry at < MDL of 2 ug/L which would be the P MDL.
## Remove value of 20 (has a < so supposed to be <MDL)
## Include surface samples only (it's just most recent data that have deep water samples)
shuswap_TP_0500123 <- filter(shuswap_TP, EMS_ID == "0500123")
shuswap_TP_0500123 <- shuswap_TP_0500123[-c(2,4,6,8,25,118,141,172,197), ]

## Clean function averages daily samples from 201 to 101 rows, removes columns: sample state, sample descriptor, sample class, location type, upper depth and lower depth.
clean_TP_0500123 <- clean_wqdata(shuswap_TP_0500123, by = "EMS_ID", delete_outliers = TRUE, remove_blanks = TRUE)

## Change units from mg/L to ug/L
clean_TP_0500123 <- transform(clean_TP_0500123, Value = Value*1000)
colnames(clean_TP_0500123)[4] <- "Value_ugL"

## Delete'Units' column which says 'mg/L'
## Delete 'DetectionLimit' column which is in mg/L
clean_TP_0500123 <- select(clean_TP_0500123, -Units)
clean_TP_0500123 <- select(clean_TP_0500123, -DetectionLimit)

## Separate df into growing season (May - October) and non-growing season (November to April)
clean_TP_0500123$Month <- as.character(format(clean_TP_0500123$Date, '%b'))
clean_TP_0500123_gs <- filter(clean_TP_0500123, Month == "May"|Month == "Jun"| Month == "Jul" |Month == "Aug"| Month == "Sept"| Month == "Oct")

## Average by month
## The only month that there is two samples from is June 2015

## CREATE CSV OF CLEAN DATA (DO THIS FOR RAW DF, AND ALL CLEANED UP PARAMETER DFs)
write.csv(clean_TP_0500123,'C:/R Projects/wqo_shuswap/data/clean_TP_0500123.csv', row.names = FALSE)

## CLEANING UP SITE E208723 - MAIN ARM



##### NITROGEN #####

##### DISSOLVED OXYGEN #####

##### CHLOROPHYLL A

##### SECCHI DEPTH ######

##### E. coli #####


