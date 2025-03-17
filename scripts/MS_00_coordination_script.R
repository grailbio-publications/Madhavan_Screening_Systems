#revised manuscript oriented scripts
#coordination script
library(tidyverse)
library(readxl)
library(scales)
library(patchwork)
library(diagram)

date_code<-"20240726"

four_system_color<-c(uspstf="#B73D27",mced_all="#602B5B",sced_ten="#94724F",mced_ten="#628592")

source("scripts/MS_01_system_definitions.R")
source("scripts/MS_02_seer_incidence.R")
source("scripts/MS_03_diagnostic_cost.R")

#first assign USPSTF cancers to USPSTF
#then using remaining incidence and population
#apply each other system definition separately
#these have separate specificity for each assay
source("scripts/MS_04_apply_uspstf.R")
source("scripts/MS_05_sced_ten.R")
#these have one global specificity so the code is different
source("scripts/MS_06_mced_ten.R")
source("scripts/MS_07_mced_all.R")
#now system results and outputs
source("scripts/MS_08_aggregate_results.R")
#tables
source("scripts/MS_09_table_one.R")
source("scripts/MS_10_table_two.R")
#figures
source("scripts/MS_11_figure_two.R")
source("scripts/MS_12_figure_three.R")
source("scripts/MS_13_figure_four.R")
source("scripts/MS_14_figure_five.R")
#misc assertions in body of paper
source("scripts/MS_15_misc_data.R")



#supplemental: population eligible computation from SEER/census
source("scripts/MS_101_frac_eligible.R")
source("scripts/MS_102_ten_cancers.R")