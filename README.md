
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Project Status:
Active](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Travis build
status](https://travis-ci.com/stmarcin/DepartureTime.svg?branch=master)](https://travis-ci.com/stmarcin/DepartureTime)
<!-- badges: end -->

# DepartureTime

R package to generate departure times for accessibility analysis

## Description

The goal of DepartureTime is to prepare a `data.frame` which contains
generated departure times, using user-defined:

  - sampling method
  - date
  - time-window
  - path and file name

Supported sampling methods:

  - **Systematic** sampling method: departure times are selected using a
    regular interval defined by the frequency
  - **Simple Random** sampling method: a specified number of departure
    times (defined by the frequency) is randomly selected from the time
    window
  - **Hybrid** sampling method: departure times are randomly selected
    from given time intervals (resulted from applied temporal
    resolution)
  - **Constrained Random Walk Sampling** sampling method: a first
    departure time is randomly selected from the subset of the length
    defined by the frequency and beginning of the time window; then, the
    next departure time is randomly selected from the subset limited by
    \(Tn+f/2\) and \(Tn+f+f/2\)

Example (temporal resolution 20 minutes, time window 07:00 - 08:00):

| Sampling method   | Departure times           | Comments                                                                          |
| ----------------- | ------------------------- | --------------------------------------------------------------------------------- |
| **Systematic**    | 07:00; 07:20, 7:40, 08:00 | regular interval of 20 minutes<sup>1</sup>                                        |
| **Simple Random** | 07:18; 07:51; 07:55       | 3 randomly selected departure times from the time window<sup>2</sup>              |
| **Hybrid**        | 07:02; 07:23; 07:50       | One randomly selected departure time from each time interval period<sup>3</sup>   |
| **Random Walk**   | 07:15; 07:36; 07:49       | on average there should be 20-minute interval between departure times<sup>4</sup> |

<sup>1</sup> as 20-minute interval fits to 60 minute time window it
provides 4 departure times.  
<sup>2</sup> i.e. one per each 20 min. in 60-minute time window.  
<sup>3</sup> i.e. one from 07:00-07:19, one from 07:20-07:39 and one
from 07:40-07:59.  
<sup>4</sup> due to the nature of the sampling procedure, the number of
departure times might differ.

For details please consult [Owen & Murphy
(2018)](https://trid.trb.org/view/1497217).

## Installation

You can install `DepartureTime` pacakage from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("stmarcin/DepartureTime")
```

## Function syntax:

    DepartureTime <- function(method = "H",
                              dy = format(Sys.Date(), "%Y"),  
                              dm = format(Sys.Date(), "%m"), 
                              dd = format(Sys.Date(), "%d"),
                              tmin = 0, tmax = 24,
                              res = 5,
                              MMDD = TRUE,
                              ptw = FALSE)

## Function variables:

  - `method` - sampling method; Options:
      - `R` OR `Random`: Simple random sampling;
      - `S` OR `Systematic`: Systematic sampling;
      - `H` OR `Hybrid`: Hybrid sampling;
      - `W` OR `Walk`: Constrained random walk sampling;
  - `dy`, `dm` and `dd` - date of the analysis (formats: YYYY, MM, DD);
    **default: system date**;
  - `tmin` and `tmax` - limits of the time window (format: HH);
    **default: full day** (00:00 - 24:00);
  - `res` - temporal resolution; **default: 5 minutes**
  - `MMDD` - date format of the output (TRUE / FALSE) **default: TRUE**
      - `TRUE`: MM/DD/YYYY;
      - `FALSE`: DD/MM/YYYY;
  - `ptw` - print limits of subsetted time-windows; **default: FALSE**;

## Output

`data.frame` which contains generated departure times (to be used
e.g. in ArcGIS Network to generated ODs with time-dependent transport
data, e.g. GTFS). File structure:

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

ColumnName

</th>

<th style="text-align:left;">

Description

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

ID

</td>

<td style="text-align:left;">

rowID (integer), starts with 0

</td>

</tr>

<tr>

<td style="text-align:left;">

Date

</td>

<td style="text-align:left;">

Departure date & hour

</td>

</tr>

</tbody>

</table>

## Examples

Working example, uses all default variables and hybrid sampling method:

``` r
library(DepartureTime)

DepartureTime() %>% 
  head()
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:right;">

ID

</th>

<th style="text-align:left;">

Date

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

05/16/2020 00:01

</td>

</tr>

<tr>

<td style="text-align:right;">

1

</td>

<td style="text-align:left;">

05/16/2020 00:06

</td>

</tr>

<tr>

<td style="text-align:right;">

2

</td>

<td style="text-align:left;">

05/16/2020 00:12

</td>

</tr>

<tr>

<td style="text-align:right;">

3

</td>

<td style="text-align:left;">

05/16/2020 00:18

</td>

</tr>

<tr>

<td style="text-align:right;">

4

</td>

<td style="text-align:left;">

05/16/2020 00:22

</td>

</tr>

<tr>

<td style="text-align:right;">

5

</td>

<td style="text-align:left;">

05/16/2020 00:27

</td>

</tr>

</tbody>

</table>

Example with user-defined parameters:

``` r
DepartureTime(method = "S",    # systematic sampling method
  dm = 5, dd = 15,             # user-defined date: 15th May, 2020 (current year)
  tmin = 7, tmax = 9,          # user-defined time window (07:00 - 09:00)
  res = 20)                    # user-defined temporal resolution (20 minutes)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:right;">

ID

</th>

<th style="text-align:left;">

Date

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

05/15/2020 07:00

</td>

</tr>

<tr>

<td style="text-align:right;">

1

</td>

<td style="text-align:left;">

05/15/2020 07:20

</td>

</tr>

<tr>

<td style="text-align:right;">

2

</td>

<td style="text-align:left;">

05/15/2020 07:40

</td>

</tr>

<tr>

<td style="text-align:right;">

3

</td>

<td style="text-align:left;">

05/15/2020 08:00

</td>

</tr>

<tr>

<td style="text-align:right;">

4

</td>

<td style="text-align:left;">

05/15/2020 08:20

</td>

</tr>

<tr>

<td style="text-align:right;">

5

</td>

<td style="text-align:left;">

05/15/2020 08:40

</td>

</tr>

<tr>

<td style="text-align:right;">

6

</td>

<td style="text-align:left;">

05/15/2020 09:00

</td>

</tr>

</tbody>

</table>

## Direct export to `.dbf`

If you don’t want/need to revise departure times, you can easily export
them directly, e.g. to `.dbf` file using `{foreign}` package:

``` r
library(DepartureTime)
library(foreign)
library(dplyr)

# generate departure times for 8-10am time window 
# with 30-minute temporal resolution applying hybrid sampling model:
DepartureTime(tmin = 8, tmax = 10, res = 30) %>% 
  
  #save output in OD_analysis subfolder as My_Departure_Times.dbf
  write.dbf("OD_analysis/My_Departure_Times.dbf")
  
```

## Background

This script was inspired by [Owen & Murphy
(2018)](https://trid.trb.org/view/1497217) study and was developed when
working on [Stępniak et
al. (2019)](https://doi.org/10.1016/j.jtrangeo.2019.01.007) paper:

Stępniak, M., Pritchard, J.P., Geurs K.T., Goliszek S., 2019, *The
impact of temporal resolution on public transport accessibility
measurement: review and case study in Poland*, Journal of Transport
Geography.

### Funding statement

This document was created within the **MSCA CAlCULUS** project.

*This project has received funding from the European Union’s Horizon
2020 research and innovation Programme under the Marie Sklodowska-Curie
Grant Agreement no. 749761.*  
*The views and opinions expressed herein do not necessarily reflect
those of the European Commission.*
