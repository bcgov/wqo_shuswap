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
# SCRIPT SUMMARY
# Download all EMS data for 4 Shuswap Lake sites
# A file containing the raw data that is loaded and filtered by later scripts


# DOWNLOAD AND LOAD EMS DATA FROM DATA BC OPEN DATA OBJECT

# See rems readme file for more information: https://github.com/bcgov/rems/blob/master/README.Rmd

# install.packages("package name") if not already installed

#install_github("bcgov/rems")
#install_github("bcgov/wqbc", ref = "develop")

# Load Packages
library(tidyr)
library(devtools)
library(rems)
library(dplyr)
library(ggplot2)
library(wqbc)
library(reshape)
library(lubridate)
library(readr)
library(directlabels)

# Load the last 2 years of water quality data for 4 monitoring sites in Shuswap Lake from the BC Data Catalogue using bcgov/rems package.
# Two year data, four year data, and historic data can be downloaded.
# You can specify  which = "4yr"  to get the last four years of data

twoyear <- get_ems_data(which = "2yr", cols = "all")

filtered_twoyear <- filter_ems_data(twoyear, emsid = c("E206771", "0500124", "E208723", "0500123"))

#remove_data_cache("2yr")

# DOWNLOAD HISTORIC DATA
# If you need to download the historic data, uncomment the following line:
#download_historic_data(ask = FALSE)

# Attach the sq Lite database to R
hist_db <- attach_historic_data()

# Grab data from the 4 Shuswap Lake monitoring sites
filtered_historic <- hist_db %>%
  filter(EMS_ID %in% c("E206771", "0500124", "E208723", "0500123"))

# Convert the database object into a regular R data object
filtered_historic <- collect(filtered_historic) %>%
  # Make sure the time data is in proper format
  mutate(COLLECTION_START = ems_posix_numeric(COLLECTION_START),
         COLLECTION_END = ems_posix_numeric(COLLECTION_END))

# Combine the 2yr and the historic dataframes
all_data_shuswap <- bind_ems_data(filtered_twoyear, filtered_historic)

# CREATE CSV OF RAW DATA
write.csv(all_data_shuswap,'C:/R Projects/wqo_shuswap/data/all_data_shuswap.csv', row.names = FALSE)
