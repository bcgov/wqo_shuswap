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

## DOWNLOAD EMS DATA FROM DATA BC OPEN DATA OBJECT
## See rems readme file for more information: https://github.com/bcgov/rems/blob/master/README.Rmd

## install.packages("package name") if not already installed

library(devtools)
#install_github("bcgov/rems")
#install_github("bcgov/wqbc")
library(rems)
library(dplyr)
library(ggplot2)
library(wqbc)

# LOAD WATER QUALITY DATASET
# From BC Data Catalogue using bcgov/rems package.
# Two year data, four year data, and historic data can be downloaded.
# You can specify  which = "4yr"  to get the last four years of data

twoyear <- get_ems_data(which = "2yr", ask = TRUE)

filtered_twoyear <- filter_ems_data(twoyear,
                                    emsid = c("E206771", "0500124", "E208723", "0500123"),
                                    #parameter = ("Barium Total"),
                                    #from_date = "2011/05/06",
                                    to_date = "2018/12/04")

#remove_data_cache("2yr")

# DOWNLOAD HISTORIC DATA - JUST HAVE TO RUN THIS CODE ONCE
#download_historic_data(ask = FALSE)
#
# Run this part if historic code below isn't working
filtered_historic <- read_historic_data(
                                    emsid = c("E206771", "0500124", "E208723", "0500123"),
                                    #parameter = ("Barium Total"),
                                    #from_date = "2011/05/06",
                                    to_date = "2018/12/04")


hist_db <- attach_historic_data()
filtered_historic2 <- hist_db %>%
  select(EMS_ID, MONITORING_LOCATION, LOCATION_TYPE, COLLECTION_START, LOCATION_PURPOSE, SAMPLE_CLASS, SAMPLE_STATE,
         SAMPLE_DESCRIPTOR, PARAMETER_CODE, PARAMETER, ANALYTICAL_METHOD_CODE, ANALYTICAL_METHOD, RESULT_LETTER,
         RESULT, UNIT, METHOD_DETECTION_LIMIT) %>%
  filter(EMS_ID %in% c("E206771", "0500124", "E208723", "0500123"))

filtered_historic2 <- collect(filtered_historic2) %>%
  mutate(COLLECTION_START = ems_posix_numeric(COLLECTION_START))
#glimpse(filtered_historic2)

all_data_shuswap <- bind_ems_data(filtered_twoyear, filtered_historic2)

# CREATE CSV OF ALL RAW DATA
#write.csv(shuswap_df,
#'C:/R Projects/wqo_shuswap/data/all_data_shuswap.csv', row.names = FALSE'

# Just load csv each time so don't have to download from EMS. Ensures dataset remains consistent.
read.csv("all_data_shuswap")
