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
shuswap_clean <- read_csv("data/shuswap_clean.csv") %>%
  # Select only parameters for assessing potential WQOs
  filter(PARAMETER %in% c("Nitrogen Total", "Phosphorus Total", "Oxygen Dissolved", "Dissolved Oxygen-Field", "Chlorophyll A", "Organic Carbon Total", "Organic Carbon Dissolved","Extinction Depth", "Coliform - Fecal")) %>%
  # Select only columns needed
  select(EMS_ID,
         MONITORING_LOCATION,
         PARAMETER,
         COLLECTION_START,
         SAMPLE_CLASS,
         RESULT,
         RESULT_LETTER,
         UNIT,
         METHOD_DETECTION_LIMIT,
         MDL_UNIT,
         UPPER_DEPTH,
         LOWER_DEPTH)

# Add Month, Day, Year and Time columns and remove time from COLLECTION_START
shuswap_clean$MONTH <- as.character(format(shuswap_clean$COLLECTION_START, '%b'))
shuswap_clean$DAY <- as.character(format(shuswap_clean$COLLECTION_START, '%d'))
shuswap_clean$YEAR <- as.character(format(shuswap_clean$COLLECTION_START, '%Y'))
shuswap_clean$TIME <- as.character(format(shuswap_clean$COLLECTION_START, '%H:%M:%S'))
shuswap_clean <- mutate(shuswap_clean, COLLECTION_START = date(COLLECTION_START))

## Create loop to produce a raw data plot of the 9 parameters for all 4 sites

ggplot(shuswap_clean, aes(x = COLLECTION_START, y = RESULT)) +
  geom_point() +
  facet_grid(PARAMETER ~ EMS_ID, scales = "free_y")
  labs(x="Date", y="PARAMETER") +
  ggtitle(EMS_ID)

############################## PHOSPHORUS ###########################################

# Create total phosphorus dataframe for tidying up data
TP <- shuswap_clean %>%
  filter(PARAMETER == "Phosphorus Total") %>%
# Change units from mg/L to ug/L
transform(RESULT = RESULT*1000)
TP$UNIT <- "ug/L"

# Create dataframe for each site and further clean
# Remove 4 rows of 8/21/2002 and 2/11/2003 that look like they were entered wrong at 100 ug/L. According to Kevin, these are likely metals results that got lumped into the P test results. These days are entered twice, the second entry at < MDL of 2 ug/L which would be the P MDL and is correct.
# Remove value of 20 (has a RESULT_LETTER OF < so supposed to be <MDL)
TP_0500123 <- filter(TP, EMS_ID == "0500123")
TP_0500123 <- TP_0500123 %>%
  filter(!RESULT == 100, !RESULT == 20)

# Remove MDL errors of 100 and 30 ug/L. I think RESULT should be 3 ug/L and 10 ug/L.
TP_E206771 <- filter(TP, EMS_ID == "E206771")
TP_E206771 <- TP_E206771 %>%
  filter(!RESULT == 100, !RESULT == 30)

# Removed two MDL errors. MDL should be 2 ug/L not 20 so these show up with a <.
TP_0500124 <- filter(TP, EMS_ID == "0500124")
TP_0500124 <- TP_0500124 %>%
  filter(!RESULT == 20)

# Removed one MDL error
TP_E208723 <- filter(TP, EMS_ID == "E208723")
TP_E208723 <- TP_E208723 %>%
  filter(!RESULT == 100)

# Put together TP dataframe with clean TP site data
TP <- bind_rows(TP_0500123, TP_0500124, TP_E206771, TP_E208723)

# Average samples taken on the same day at different sites on different days at different depths.
TP_avg <- TP %>%
  group_by(EMS_ID, COLLECTION_START, UPPER_DEPTH, LOWER_DEPTH) %>%
  summarize(RESULT_avg = mean(RESULT)) %>%
  ungroup()

# Monthly TP means
TP_mm <- TP_avg %>%
  group_by(EMS_ID, date = floor_date(COLLECTION_START,"month"), UPPER_DEPTH, LOWER_DEPTH) %>%
  summarise(RESULT_mm = mean(RESULT_avg)) %>%
  ungroup()
TP_mm$MONTH <- as.character(format(TP_mm$date, '%b'))

# Growing season monthly TP means
TP_gs <- filter(TP_mm, MONTH == "May"| MONTH == "Jun"| MONTH == "Jul" | MONTH == "Aug"| MONTH == "Sep"| MONTH == "Oct")

## Add WQO column to growing season dataframe
TP_gs$WQO <- NA

## Add WQO value into new column by monitoring location
TP_gs$WQO[TP_gs$EMS_ID == "0500123"] <- 10
TP_gs$WQO[TP_gs$EMS_ID == "0500124"] <- 10
TP_gs$WQO[TP_gs$EMS_ID == "E206771"] <- 15
TP_gs$WQO[TP_gs$EMS_ID == "E208723"] <- 10

# CREATE CSV OF CLEAN TP DATA
write.csv(TP_avg,'C:/R Projects/wqo_shuswap/data/TP_avg.csv', row.names = FALSE)


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
