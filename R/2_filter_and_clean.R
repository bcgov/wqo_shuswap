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

# SCRIPT SUMMARY
# Filtering to:
# Remove blanks
# Only fresh water
# Remove results that are n/a
# Remove results with q/c grade "F"
# Create object of results below detection limit ("<")
# Select only columns needed, and remove duplicates (some are duplicated via permit number) A
# A clean .CSV file is saved at the end to load in later scripts

# FILTER ALL RAW DATA
# Don't want to use clean_wqdata function as this is for lake data with varying depths per day, and the clean function averages multiple daily measurments within the same site.

# Haven't used tidy_ems_data as I have left the data at the MDL (if there's a '<", the RESULT is the MDL anyway), and I don't want to change the column names as I don't need to run the clean function.

# Load raw data
shuswap_clean <- read_csv("data/all_data_shuswap.csv")

shuswap_clean <- all_data_shuswap %>%
  # Take out blanks and Reference
  filter(!SAMPLE_CLASS == "Blank - field", !SAMPLE_CLASS == "Reference") %>%
  # Make sure only fresh water samples
  filter(SAMPLE_STATE =="Fresh Water") %>%
  # Remove results that have 'na'
  filter(!is.na(RESULT)) %>%
  # Remove results that failed q/c
  filter(!QA_INDEX_CODE == "F" | is.na(QA_INDEX_CODE)) %>%
  # Remove units for barometric pressure data
  filter(!UNIT == "kPa") %>%
  # Remove RESULTS = 0 and - from EMS system calculations (mostly N species)
  filter(!RESULT == 0.00, !RESULT <=0)

# Remove results below/above detection limit
# Pull out the below detection limit data to investigate
#below_detect <- shuswap_clean %>%
 #filter(RESULT_LETTER == "<")
# Investigated the above detect ... none present for Shuswap data.
#above_detect <- shuswap_clean %>%
 #filter(RESULT_LETTER == ">")

# Remove below detects from data (there's about 3000 for Shuswap. I've kept in and left result at MDL)
#shuswap_clean <- shuswap_clean %>%
 #filter(!RESULT_LETTER %in% c("<") | is.na(RESULT_LETTER))

# Remove duplicate entries (there aren't any for Shuswap), and keep only useful columns (no permits so removed this column)
shuswap_clean <- shuswap_clean %>%
  distinct() %>%
  select(c(EMS_ID, MONITORING_LOCATION, LOCATION_TYPE, COLLECTION_START, SAMPLE_CLASS, SAMPLE_STATE, SAMPLE_DESCRIPTOR, PARAMETER, PARAMETER_CODE, RESULT_LETTER, RESULT, UNIT, METHOD_DETECTION_LIMIT, MDL_UNIT, UPPER_DEPTH, LOWER_DEPTH))

# Add Month, Day, Year and Time columns and remove time from COLLECTION_START
shuswap_clean$MONTH <- as.character(format(shuswap_clean$COLLECTION_START, '%b'))
shuswap_clean$DAY <- as.character(format(shuswap_clean$COLLECTION_START, '%d'))
shuswap_clean$YEAR <- as.character(format(shuswap_clean$COLLECTION_START, '%Y'))
shuswap_clean$TIME <- as.character(format(shuswap_clean$COLLECTION_START, '%H:%M:%S'))
shuswap_clean <- mutate(shuswap_clean, COLLECTION_START = date(COLLECTION_START))

# CREATE CSV OF CLEAN DATA
write.csv(shuswap_clean, 'C:/R Projects/wqo_shuswap/data/shuswap_clean.csv', row.names = FALSE)

