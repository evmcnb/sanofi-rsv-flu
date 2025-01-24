## -----------------------------------------------------------------------------
## Title: Canada data
## Description: This is a script that is pulling the influenza and RSV data from Argentina
## Author: L.Lampro - Sanofi group
## Date: 24.01.2025
## left half way - asking Onayomi who found this data
## -----------------------------------------------------------------------------

#install.packages("ggthemes")

library(purrr)
library(readxl)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(stringr)

## Get the working directory

mywd <- setwd("C:/Users/icnarc246/OneDrive - ICNARC/Desktop/Trainings and personal docs/LSHTM/Data challenge/sanofi-rsv-flu/csv/Canada")
getwd()