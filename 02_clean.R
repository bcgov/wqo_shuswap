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
# Don't want to use clean_wqdata function as this is for lake data with varying depths per day, and the clean function averages multiple daily
# measurments.
distinct(all_data_shuswap, SAMPLE_STATE)
shuswap_clean <- filter(all_data_shuswap, SAMPLE_STATE == "Fresh Water")

## View and remove blanks
distinct(all_data_shuswap, SAMPLE_CLASS)
shuswap_clean <- filter(shuswap_clean, SAMPLE_CLASS != "Blank - field")

## Refine dataframe (df) to include a subset of columns for simplified viewing analysis
shuswap_df <- select(shuswap_clean, EMS_ID, MONITORING_LOCATION, COLLECTION_START, PARAMETER, RESULT_LETTER, RESULT, SAMPLE_CLASS, UPPER_DEPTH, LOWER_DEPTH)

## You can create a list of any column
#parameters <- distinct(all_data, PARAMETER)
