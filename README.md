# Importing data from CBRT Electronic Data Delivery System 
In this project I created a small R package which is useful for importing data from EDDS website of the CBRT.

The purpose of this package is to ease the data import from CBRT EDDS system to R.
You need API key from CBRT and the series code of the data you want to import.

Do not use 'evds_data()' function after 25th April

## Installation

``` r
# devtools is required
# install.packages("devtools")
devtools::install_github("pimaniye41/evds_data")
# most tidyverse packages, RCurl, XML are required.
```

## Basic usage

``` r
library(evdspackage)
# evds_data_2(anahtar = "yourkey", 
          verisetleri = c("TP.KTF10","TP.PR.ARZ01"),
          baslangic_tarihi = "30-01-2020",
          bitis_tarihi = "12-03-2023")
```
