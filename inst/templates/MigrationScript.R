# {{{ FileName }}}.R

# A. Meta Info -----------------------

# Study: {{{ Title }}}
# Author: {{{ Author }}}
# Date: {{{ Date }}}
# Description: The purpose of this script is to migrate data from.....


# B. Dependencies ------------------------


library(tidyverse) #import tidyverse

resultsPath <- here::here("exec/results")
exportPath <- here::here("exec/export")

# C. Script -----------------------


## 1. Import Raw ------------------

dat <- fs::path(studyPath, "<data_file>.csv") %>%
  readr::read_csv(show_col_types = FALSE)

## 2. Format --------------------

## Input formatting pipeline -------------------

## 3. Save as Export --------------

readr::write_csv(modifiedData, file = fs::path(exportPath, "<export_dile>.csv"))
