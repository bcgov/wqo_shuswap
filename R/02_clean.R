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
#
#
#
#
#
#
## LOADS CLEAN RAW DATA, PRODUCES GRAPHS FOR INITIAL DATA VISUALIZATION FOR EACH WATER QUALITY PARAMETER FOR EACH OF THE FOUR MONITORING SITES. THREE DATAFRAMES OF DAILY MEANS, MONTHLY MEANS AND MONTHLY MEANS DURING THE GROWING SEASON (MAY - OCT) ARE PUT TOGETHER IN THE SCRIPT.
#
## View and filter out non water samples
#
## Don't want to use clean_wqdata function as this is for lake data with varying depths per day, and the clean function averages multiple daily measurments.
#
## Haven't used tidy_ems_data as I have left the data at the MDL (if there's a '<", the RESULT is the MDL anyway), and I don't want to change the column names as I don't need to run the clean function.
#
#distinct(all_data_shuswap, SAMPLE_STATE)
#shuswap_clean <- filter(all_data_shuswap, SAMPLE_STATE == "Fresh Water")
#
## View and remove blanks
#distinct(all_data_shuswap, SAMPLE_CLASS)
#shuswap_clean <- filter(shuswap_clean, SAMPLE_CLASS != "Blank - field")
#
## Refine dataframe (df) to include a subset of columns for simplified viewing analysis
#shuswap_df <- select(shuswap_clean, EMS_ID, MONITORING_LOCATION, COLLECTION_START, PARAMETER, RESULT_LETTER, RESULT, UNIT, SAMPLE_CLASS, UPPER_DEPTH, LOWER_DEPTH)

## You can create a summary list of any column
#distinct(shuswap_df, MONITORING_SITE = "0500123", PARAMETER)
#
# # CREATE CSV OF ALL RAW CLEAN DATA
#write.csv(shuswap_df, 'C:/R Projects/wqo_shuswap/data/all_data_shuswap_clean.csv', row.names = FALSE)

# Just load csv each time so don't have to download from EMS. Ensures dataset remains consistent.
all_data_shuswap <- read_csv("data/all_data_shuswap_clean.csv")

# Add Month, Day, Year and Time columns and remove time from COLLECTION_START
all_data_shuswap$MONTH <- as.character(format(all_data_shuswap$COLLECTION_START, '%b'))
all_data_shuswap$DAY <- as.character(format(all_data_shuswap$COLLECTION_START, '%d'))
all_data_shuswap$YEAR <- as.character(format(all_data_shuswap$COLLECTION_START, '%Y'))
all_data_shuswap$TIME <- as.character(format(all_data_shuswap$COLLECTION_START, '%H:%M:%S'))
all_data_shuswap <- mutate(all_data_shuswap, COLLECTION_START = date(COLLECTION_START))


############################## PHOSPHORUS ###########################################
#
# Initial Visualization
TP <- filter(all_data_shuswap, PARAMETER == "Phosphorus Total")

# Change units from mg/L to ug/L
TP <- transform(TP, RESULT = RESULT*1000)
TP$UNIT <- "ug/L"

sites <- c("E206771", "0500124", "E208723", "0500123")

for (s in sites){
  TP_plots <- filter(TP, EMS_ID == s)
  plotpoint <- ggplot(TP_plots, aes(x = COLLECTION_START, y = RESULT)) +
  geom_point() +
  ggtitle(s) +
  xlab("Date") +
  ylab("Total Phosphorus (ug/L)")
  plot(plotpoint)
}

# CLEANING UP SITE 0500123 - SORRENTO REACH
#
# Remove 4 rows of 8/21/2002 and 2/11/2003 that look like they were entered wrong at 100 ug/L. According to Kevin, these are likely metals results that got lumped into the P test results. These days are entered twice, the second entry at < MDL of 2 ug/L which would be the P MDL.
# Remove value of 20 (has a < so supposed to be <MDL)
# Include surface samples only (it's just most recent data that have deep water samples)
TP_0500123 <- filter(TP, EMS_ID == "0500123")
TP_0500123 <- TP_0500123[-c(2,4,22,115,141,172,197), ]

# Average samples (regular and repeat of surface samples) taken on the same day.
TP_0500123_avg <- TP_0500123 %>%
  group_by(COLLECTION_START, MONITORING_LOCATION, MONTH, DAY, YEAR, UNIT) %>%
  summarize(RESULT_avg = mean(RESULT))

# Monthly TP means
TP_0500123_mm <- TP_0500123_avg %>%
  group_by(COLLECTION_START, MONITORING_LOCATION, MONTH, DAY, YEAR, UNIT) %>%
  summarise(RESULT_month_mean = mean(RESULT_avg))

# Growing season monthly TP means
TP_0500123_gs <- filter(TP_0500123_mm, MONTH == "May"| MONTH == "Jun"| MONTH == "Jul" | MONTH == "Aug"| MONTH == "Sep"| MONTH == "Oct")


# CLEANING UP SITE E206771 - SALMON ARM REACH
#
# Removing deep lake values and rows where the result has a result letter but the result is entered as 30 and 100 ug/L whereas I think it should be 3ug/L and 10 ug/L.
TP_E206771 <- filter(TP, EMS_ID == "E206771")
TP_E206771 <- TP_E206771[-c(2,4,6,9,11,13,15,60,114,123,195), ]

# Average samples (regular and repeat of surface samples) taken on the same day.
TP_E206771_avg <- TP_E206771 %>%
  group_by(COLLECTION_START, MONITORING_LOCATION, EMS_ID UNIT) %>%
  summarize(RESULT_avg = mean(RESULT))

# Monthly TP means
TP_E206771_mm <- TP_E206771_avg %>%
  group_by(COLLECTION_START, MONITORING_LOCATION, MONTH, DAY, YEAR, UNIT) %>%
  summarise(RESULT_month_mean = mean(RESULT_avg))

# Separate df into growing season (May - October)
TP_E206771_gs <- filter(TP_E206771_mm, MONTH == "May"| MONTH == "Jun"| MONTH == "Jul" | MONTH == "Aug"| MONTH == "Sep"| MONTH == "Oct")


# CLEANING UP SITE 0500124 - SICAMOUS REACH
#
# Removed lower depth samples and a couple MDL errors
TP_0500124 <- filter(TP, EMS_ID == "0500124")
TP_0500124 <- TP_0500124[-c(2,4,161,224), ]

# Average samples (regular and repeat of surface samples) taken on the same day.
TP_0500124_avg <- TP_0500124 %>%
  group_by(COLLECTION_START) %>%
  summarise(RESULT_ugL_avg = mean(RESULT))

# Add back EMS_ID and MONITORING_LOCATION columns to avg df
TP_0500124_avg$EMS_ID <- "0500124"
TP_0500124_avg$MONITORING_LOCATION <- "SHUSWAP LK OPPOSITE MARBLE PT.-SICAMOUS REACH"

# Add month and year columns
TP_0500124_avg$Month <- as.character(format(TP_0500124_avg$COLLECTION_START, '%b'))
TP_0500124_avg$Year <- as.character(format(TP_0500124_avg$COLLECTION_START, '%Y'))

# Monthly TP means
TP_0500124_mm <- TP_0500124_avg %>%
  group_by(Month, Year) %>%
  summarise(RESULT_month_mean = mean(RESULT_ugL_avg))

# Add back EMS_ID and MONITORING_LOCATION columns to monthly mean(mm) df
TP_0500124_mm$EMS_ID <- "0500124"
TP_0500124_mm$MONITORING_LOCATION <- "SHUSWAP LK OPPOSITE MARBLE PT.-SICAMOUS REACH"

# Separate df into growing season (May - October) by adding Month and Year columns
TP_0500124_avg$Month <- as.character(format(TP_0500124_avg$COLLECTION_START, '%b'))
TP_0500124_avg$Year <- as.character(format(TP_0500124_avg$COLLECTION_START, '%Y'))
TP_0500124_gs <- filter(TP_0500124_mm, Month == "May"|Month == "Jun"| Month == "Jul" |Month == "Aug"| Month == "Sep"| Month == "Oct")


# CLEANING UP SITE E208723 - MAIN ARM
#
## Removed lower depth samples and a couple MDL errors
TP_E208723 <- filter(TP, EMS_ID == "E208723")
TP_E208723 <- TP_E208723[-c(50,52,55,64,73), ]

# Average samples (regular and repeat of surface samples) taken on the same day.
TP_E208723_avg <- TP_E208723 %>%
  group_by(COLLECTION_START) %>%
  summarize(RESULT_ugL_avg = mean(RESULT))

# Add back EMS_ID and MONITORING_LOCATION columns to avg df
TP_E208723_avg$EMS_ID <- "E208723"
TP_E208723_avg$MONITORING_LOCATION <- "SHUSWAP LK OFF ARMSTRONG PT-MAIN ARM REACH"

# Add month and year columns
TP_E208723_avg$Month <- as.character(format(TP_E208723_avg$COLLECTION_START, '%b'))
TP_E208723_avg$Year <- as.character(format(TP_E208723_avg$COLLECTION_START, '%Y'))

# Monthly TP means
TP_E208723_mm <- TP_E208723_avg %>%
  group_by(Month, Year) %>%
  summarise(RESULT_month_mean = mean(RESULT_ugL_avg))

# Add back EMS_ID and MONITORING_LOCATION columns to mm df
TP_E208723_mm$EMS_ID <- "E208723"
TP_E208723_mm$MONITORING_LOCATION <- "SHUSWAP LK OFF ARMSTRONG PT-MAIN ARM REACH"

# Separate df into growing season (May - October) and non-growing season (November to April) by adding Month and Year columns
TP_E208723_avg$Month <- as.character(format(TP_E208723_avg$COLLECTION_START, '%b'))
TP_E208723_avg$Year <- as.character(format(TP_E208723_avg$COLLECTION_START, '%Y'))
TP_E208723_gs <- filter(TP_E208723_mm, Month == "May"|Month == "Jun"| Month == "Jul" |Month == "Aug"| Month == "Sep"| Month == "Oct")

# Join clean TP average daily data and growing season monthly means from all 4 sites together
# *****Could these cleaning steps for done for all the sites at once, instead of each site? I think a few loops could be written here - at least for the adding the month and year columns and separating into growing season.
TP_avg <- bind_rows(TP_0500123_avg, TP_0500124_avg, TP_E206771_avg, TP_E208723_avg)
TP_mm_gs <- bind_rows(TP_0500123_gs, TP_0500124_gs, TP_E206771_gs, TP_E208723_gs)

## Add WQO column to growing season dataframe
TP_mm_gs$WQO <- NA

## Add WQO into column by monitoring location
TP_mm_gs$WQO[TP_mm_gs$EMS_ID == "0500123"] <- 10
TP_mm_gs$WQO[TP_mm_gs$EMS_ID == "0500124"] <- 10
TP_mm_gs$WQO[TP_mm_gs$EMS_ID == "E206771"] <- 15
TP_mm_gs$WQO[TP_mm_gs$EMS_ID == "E208723"] <- 10

# CREATE CSV OF CLEAN TP DATA
#write.csv(TP_avg,'C:/R Projects/wqo_shuswap/data/TP_avg.csv', row.names = FALSE)
#
#
################################### TOTAL NITROGEN ###################################
#
# Create a dataframe with just total nitrogen data
TN <- filter(all_data_shuswap, PARAMETER == "Nitrogen Total")
#
# Initial Visualization (sites object is listed above the Phosphorus code)
for (s in sites){
  TN_plots <- filter(TN, EMS_ID == s)
  plot_TN_point <- ggplot(subset(TN_plots, Year>2000), aes(x = COLLECTION_START, y = RESULT)) +
    geom_point() +
    ggtitle(s) +
    xlab("Date") +
    ylab("Total Nitrogen (mg/L)")
  plot(plot_TN_point)
}

# CLEANING UP SITE 0500123 - SORRENTO REACH
#
# Remove two rows with deep sample measurements as they are the only deep samples for this site in EMS. The deep sample values were similiar to the surface sample values.
TN_0500123 <- filter(TN, EMS_ID == "0500123")
TN_0500123 <- TN_0500123[-c(2, 4), ]

# Average samples (regular and repeat of surface samples) taken on the same day.
TN_0500123_avg <- TN_0500123 %>%
  group_by(COLLECTION_START) %>%
  summarize(RESULT_avg = mean(RESULT))

# Add back EMS_ID and MONITORING_LOCATION columns to avg df
TN_0500123_avg$EMS_ID <- "0500123"
TN_0500123_avg$MONITORING_LOCATION <- "SHUSWAP LK WEST OF SORRENTO-SORRENTO REACH"

# Add month and year columns (every time there's a summarize function, you have to add back columns you want)
TN_0500123_avg$Month <- as.character(format(TN_0500123_avg$COLLECTION_START, '%b'))
TN_0500123_avg$Year <- as.character(format(TN_0500123_avg$COLLECTION_START, '%Y'))

# Monthly TP means
TN_0500123_mm <- TN_0500123_avg %>%
  group_by(Month, Year) %>%
  summarise(RESULT_month_mean = mean(RESULT_avg))

# Add back EMS_ID and MONITORING_LOCATION columns to mm df
TN_0500123_mm$EMS_ID <- "0500123"
TN_0500123_mm$MONITORING_LOCATION <- "SHUSWAP LK WEST OF SORRENTO-SORRENTO REACH"

# Separate df into growing season (May - October) by adding Month and Year columns
TN_0500123_avg$Month <- as.character(format(TN_0500123_avg$COLLECTION_START, '%b'))
TN_0500123_avg$Year <- as.character(format(TN_0500123_avg$COLLECTION_START, '%Y'))
TN_0500123_gs <- filter(TN_0500123_mm, Month == "May"|Month == "Jun"| Month == "Jul" |Month == "Aug"| Month == "Sep"| Month == "Oct")


################################## DISSOLVED OXYGEN ##################################
#
DO <- filter(all_data_shuswap, PARAMETER == "Oxygen Dissolved"| PARAMETER == "Dissolved Oxygen-Field")
DO$PARAMETER <- "Dissolved Oxygen"

# Initial Visualization (sites object is listed above the Phosphorus code)
#
for (s in sites){
  DO_plots <- filter(DO, EMS_ID == s)
  plot_DO_point <- ggplot(subset(DO_plots, Year>2000), aes(x = COLLECTION_START, y = RESULT)) +
    geom_point() +
    ggtitle(s) +
    xlab("Date") +
    ylab("Surface Dissolved Oxygen (mg/L)")
  plot(plot_DO_point)
}


################################# CHLOROPHYLL A ######################################
#
 ChlA <- filter(all_data_shuswap, PARAMETER == "Chlorophyll A")

# Change units to ug/L
#
ChlA <- transform(ChlA, RESULT = RESULT*1000)
ChlA$UNIT <- "ug/L"

# Initial Visualization (sites object is listed above the Phosphorus code)
#
for (s in sites){
  ChlA_plots <- filter(ChlA, EMS_ID == s)
  plot_ChlA_point <- ggplot(subset(ChlA_plots, Year>2000), aes(x = COLLECTION_START, y = RESULT)) +
    geom_point() +
    ggtitle(s) +
    xlab("Date") +
    ylab("Chloropyll A (ug/L)")
  plot(plot_ChlA_point)
}

################################# SECCHI DEPTH ######################################

secchi <- filter(all_data_shuswap, PARAMETER == "Extinction Depth")

# Initial Visualization (sites object is listed above the Phosphorus code)
#
for (s in sites){
  secchi_plots <- filter(secchi, EMS_ID == s)
  plot_secchi_col <- ggplot(subset(secchi_plots, Year>2000), aes(x = COLLECTION_START, y = RESULT)) +
    geom_col() +
    scale_y_reverse() +
    ggtitle(s) +
    xlab("Date") +
    ylab("Secchi Depth (m)")
  plot(plot_secchi_col)
}

################################# E. coli ###########################################

#Use 1988 data and onward as that's when MPN was changed to cfu/100mL
#
ecoli <- filter(all_data_shuswap, PARAMETER == "Coliform - Fecal")

# Initial Visualization (sites object is listed above the Phosphorus code)

for (s in sites){
  ecoli_plots <- filter(ecoli, EMS_ID == s)
  plot_ecoli_point <- ggplot(subset(ecoli_plots), aes(x = COLLECTION_START, y = RESULT)) +
    geom_point() +
    ggtitle(s) +
    xlab("Date") +
    ylab("Fecal Coliforms (CFU/100mL)")
  plot(plot_ecoli_point)
}

################################# TOTAL ORGANIC CARBON  #############################

toc <- filter(all_data_shuswap, PARAMETER == "Carbon Total Organic")

# Initial Visualization (sites object is listed above the Phosphorus code)

for (s in sites){
  toc_plots <- filter(toc, EMS_ID == s)
  plot_toc_point <- ggplot(subset(toc_plots, Year>2000), aes(x = COLLECTION_START, y = RESULT)) +
    geom_point() +
    ggtitle(s) +
    xlab("Date") +
    ylab("Total Organic Carbon (mg/L)")
  plot(plot_toc_point)
}

############################# DISSOLVED ORGANIC CARBON ##############################

doc <- filter(all_data_shuswap, PARAMETER == "Carbon Dissolved Organic")

# Initial Visualization (sites object is listed above the Phosphorus code)

for (s in sites){
  doc_plots <- filter(doc, EMS_ID == s)
  plot_doc_point <- ggplot(subset(doc_plots, Year>2000), aes(x = COLLECTION_START, y = RESULT)) +
    geom_point() +
    ggtitle(s) +
    xlab("Date") +
    ylab("Dissolved Organic Carbon (mg/L)")
  plot(plot_doc_point)
}
