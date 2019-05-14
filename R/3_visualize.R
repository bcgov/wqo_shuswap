# Copyright 2018 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS
# IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

# Load clean data
shuswap_clean <- read_csv("data/shuswap_clean.csv")

############################## PHOSPHORUS ###########################################

# Initial Visualization
TP <- filter(shuswap_clean, PARAMETER == "Phosphorus Total")

# Change units from mg/L to ug/L
TP <- transform(TP, RESULT = RESULT*1000)
TP$UNIT <- "ug/L"


*********trying to put entire TP df through these dataframe organizing steps and then I can ********clean each site. Sort by EMS ID for this trial.

# Average samples taken on the same day at different depths.
TP_avg <- TP %>%
  group_by(EMS_ID, COLLECTION_START, MONTH, DAY, YEAR, UPPER_DEPTH, LOWER_DEPTH) %>%
  summarize(RESULT_avg = mean(RESULT))

# Monthly TP means
TP_mm <- TP %>%
  group_by(EMS_ID, MONTH, YEAR) %>%
  summarise(RESULT_mm = mean(RESULT))

# Growing season monthly TP means
TP_gs <- TP_mm %>%
  filter(TP_mm, MONTH == "May"| MONTH == "Jun"| MONTH == "Jul" | MONTH == "Aug"| MONTH == "Sep"| MONTH == "Oct")






sites <- c("E206771", "0500124", "E208723", "0500123")

for (s in sites){
  TP_plots <- filter(TP, EMS_ID == s)
  plotpoint <- ggplot(TP_plots, aes(x = COLLECTION_START, y = RESULT)) +
    geom_point() +
    ggtitle(s) +
    xlab("Date") +
    ylab("Total Phosphorus (µg/L)")
  plot(plotpoint)
}

# CLEANING UP SITE 0500123 - SORRENTO REACH
#
# Remove 4 rows of 8/21/2002 and 2/11/2003 that look like they were entered wrong at 100 ug/L. According to Kevin, these are likely metals results that got lumped into the P test results. These days are entered twice, the second entry at < MDL of 2 ug/L which would be the P MDL.
# Remove value of 20 (has a < so supposed to be <MDL)
# Include surface samples only (it's just most recent data that have deep water samples)
TP_0500123 <- filter(TP, EMS_ID == "0500123")
TP_0500123 <- TP_0500123[-c(2,4,22,115,141,172,197), ]

# CLEANING UP SITE E206771 - SALMON ARM REACH
#
# Removing deep lake values and rows where the result has a result letter but the result is entered as 30 and 100 ug/L whereas I think it should be 3ug/L and 10 ug/L.
TP_E206771 <- filter(TP, EMS_ID == "E206771")
TP_E206771 <- TP_E206771[-c(2,4,6,9,11,13,15,60,114,123,195), ]

# Average samples (regular and repeat of surface samples) taken on the same day.
TP_E206771_avg <- TP_E206771 %>%
  group_by(COLLECTION_START, EMS_ID, MONTH, YEAR) %>%
  summarize(RESULT_avg = mean(RESULT))

# Monthly TP means
TP_E206771_mm <- TP_E206771_avg %>%
  group_by(MONTH, YEAR, EMS_ID) %>%
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
  group_by(COLLECTION_START, EMS_ID, MONTH, YEAR) %>%
  summarise(RESULT_avg = mean(RESULT))

# Monthly TP means
TP_0500124_mm <- TP_0500124_avg %>%
  group_by(MONTH, YEAR, EMS_ID) %>%
  summarise(RESULT_month_mean = mean(RESULT_avg))

# Separate df into growing season (May - October)
TP_0500124_gs <- filter(TP_0500124_mm, MONTH == "May"| MONTH == "Jun"| MONTH == "Jul" | MONTH == "Aug"| MONTH == "Sep"| MONTH == "Oct")


# CLEANING UP SITE E208723 - MAIN ARM
#
## Removed lower depth samples and a couple MDL errors
TP_E208723 <- filter(TP, EMS_ID == "E208723")
TP_E208723 <- TP_E208723[-c(50,52,55,64,73), ]

# Average samples (regular and repeat of surface samples) taken on the same day.
TP_E208723_avg <- TP_E208723 %>%
  group_by(COLLECTION_START, EMS_ID, MONTH, YEAR) %>%
  summarize(RESULT_avg = mean(RESULT))

# Monthly TP means
TP_E208723_mm <- TP_E208723_avg %>%
  group_by(MONTH, YEAR, EMS_ID) %>%
  summarise(RESULT_month_mean = mean(RESULT_avg))

# Separate df into growing season (May - October)
TP_E208723_gs <- filter(TP_E208723_mm, MONTH == "May"| MONTH == "Jun"| MONTH == "Jul" | MONTH == "Aug"| MONTH == "Sep"| MONTH == "Oct")

# Join clean TP average daily data and growing season monthly means from all 4 sites together
TP_avg <- bind_rows(TP_0500123_avg, TP_0500124_avg, TP_E206771_avg, TP_E208723_avg)
TP_mm <- bind_rows(TP_0500123_mm, TP_0500124_mm, TP_E206771_mm, TP_E208723_mm)
TP_gs <- bind_rows(TP_0500123_gs, TP_0500124_gs, TP_E206771_gs, TP_E208723_gs)

## Add WQO column to growing season dataframe
TP_gs$WQO <- NA

## Add WQO into column by monitoring location
TP_gs$WQO[TP_gs$EMS_ID == "0500123"] <- 10
TP_gs$WQO[TP_gs$EMS_ID == "0500124"] <- 10
TP_gs$WQO[TP_gs$EMS_ID == "E206771"] <- 15
TP_gs$WQO[TP_gs$EMS_ID == "E208723"] <- 10

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
