# Australian Road Deaths Database Exploratory Data Analysis

<!-- badges: start -->
<!-- badges: end -->

The goal of ardd is to provide an updated version of the EDA-ARRD repository,
using more modern R packages and leveraging the tidyverse more heavily. Using 
tidyverse principles has made the code more compact and easier to understand.

### Data Description

The Australian Road Deaths Database (ARDD) is maintained and published by the
Bureau of Infrastructure, Transport and Regional Economics (BITRE).
It commenced in 1989 and is updated on a monthly basis.
The ARDD contains basic demographic and crash details of people who have died in
an Australian road crash. Every fatal road traffic crash in Australia is in
scope, and information is included for all people who were killed.

It is published in two forms:

* Fatalities: each record is a killed person
* Crashes: each record is a fatal crash

The database can be found [here](https://bitre.gov.au/statistics/safety/fatal_road_crash_database.aspx),
csv files can be found [here](https://data.gov.au/dataset/ds-dga-5b530fb8-526e-4fbf-b0f6-aa24e84e4277/details?q=crash),
data dictionary can be found [here](https://bitre.gov.au/statistics/safety/files/ARDD_Dictionary_V3.pdf).

Estimated population can be sourced from  [here](https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/3101.0Jun%202019?OpenDocument)
(TABLE 4. Estimated Resident Population, States and Territories (Number))

The data is already in a tidy format.

