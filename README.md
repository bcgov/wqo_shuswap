<!-- 
Add a project state badge

See <https://github.com/BCDevExchange/Our-Project-Docs/blob/master/discussion/projectstates.md> 
If you have bcgovr installed and you use RStudio, click the 'Insert BCDevex Badge' Addin.
-->
[![Lifecycle:Stable](https://img.shields.io/badge/Lifecycle-Stable-97ca00)](<Redirect-URL>)

wqo_shuswap
============================

### Usage

These scripts download water quality data from the British Columbia Ministry of Environment and Climate Change Strategy's [Environmental Monitoring System Database](https://catalogue.data.gov.bc.ca/dataset/bc-environmental-monitoring-system-results "EMS Database") using `rems`.

There are four core scripts that are required for the analysis, they need to be run in order:

-   1\_load.R
-   2\_filter_and_clean.R
-   3\_visualize.R
-   4\_plots_and_tables.R

#### Example

This set of scripts allows you to sort and visualize water quality lake data for a variety of parameters.

``` r
# Initial Visualization
TP <- filter(shuswap_clean, PARAMETER == "Phosphorus Total")

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
    ylab("Total Phosphorus (µg/L)")
  plot(plotpoint)
}
```

### Project Status

This project is in development and always changing. 

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/wqo_shuswap/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

```
Copyright 2018 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```
---
*This project was created using the [bcgovr](https://github.com/bcgov/bcgovr) package.* 
