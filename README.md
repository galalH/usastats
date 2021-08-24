# usastats

A minimalist package to tidy USA asylum data for UNHCR reporting purposes.

TO-DO list:
- [ ] Data sources
  - [x] USCIS
    - [x] import
    - [x] tidy
  - [x] EOIR
    - [x] import
    - [x] tidy
  - [ ] CBP
    - [ ] scrape
    - [ ] tidy
  - [x] WRAPSnet
    - [x] scrape
    - [x] tidy
- [x] Country mapping
- [ ] ASR/MYSR generation
- [ ] Shiny app?

The functions in the package read the raw data as shared by the government (excel, pdf, or web dashboards) and turn them into tidy tibbles ready for analysis.

For example, here's the resettlement data:

```
> read_wraps()
$flows
# A tibble: 16,752 x 6
   dataset  year month coo   flow           n
   <chr>   <dbl> <dbl> <chr> <chr>      <dbl>
 1 WRAPS    2020    10 BDI   admissions     0
 2 WRAPS    2020    11 BDI   admissions     0
 3 WRAPS    2020    12 BDI   admissions     0
 4 WRAPS    2021     1 BDI   admissions     0
 5 WRAPS    2021     2 BDI   admissions     0
 6 WRAPS    2021     3 BDI   admissions     0
 7 WRAPS    2021     4 BDI   admissions     1
 8 WRAPS    2021     5 BDI   admissions     9
 9 WRAPS    2021     6 BDI   admissions    30
10 WRAPS    2021     7 BDI   admissions    29
# ... with 16,742 more rows
```

And here's how to reproduce the RSD 2 table from the 2020 ASR:

```
library(tidyverse)
library(usastats)
library(popdata)

uscis <- read_uscis()
eoir <- read_eoir()

stock <- bind_rows(USCIS = uscis$stock, EOIR = eoir$stock, .id = "src")
flows <- bind_rows(USCIS = uscis$flows, EOIR = eoir$flows, .id = "src")
rsd <- pd_asr("rsd", year = 2019) |> filter(asylum == "USA")

rsd |> 
  filter(meta_decisionType == "EO") |> 
  count(origin, wt = totalEndYear, name = "yrstart") |> 
  full_join(stock |> 
              filter(src == "EOIR", dataset == "DEF", stock == "end") |> 
              count(origin = coo, wt = n, name = "yrend")) |> 
  full_join(flows |> 
              filter(src == "EOIR", dataset == "DEF") |> 
              count(origin = coo, flow, wt = n) |> 
              pivot_wider(names_from = flow, values_from = n)) |> 
  mutate(yrend = if_else(is.na(yrend), yrstart, yrend)) |> 
  select(origin, yrstart, applications, recognitions, rejections, closures, yrend)

# A tibble: 196 x 7
   origin yrstart applications recognitions rejections closures yrend
   <chr>    <dbl>        <dbl>        <dbl>      <dbl>    <dbl> <dbl>
 1 AFG         74           15           11          7        0    66
 2 AIA          0           NA           NA         NA       NA     0
 3 ALB        503           43           18         21        0   492
 4 ALG         40            7            0          0        0    40
 5 ANG        104          273            5         27        0   327
 6 ANT          0            0            0          0        0     0
 7 ARE        217           45           11         13        7   232
 8 ARG         93           16            0          9        0   100
 9 ARM       1510          221           49         29       21  1604
10 AUL          8            0            0          0        0     7
# ... with 186 more rows
```
