# Load libraries
# tinytex::install_tinytex()

library(shiny)
library(shinydashboard)
library(data.table)
library(DT)
library(mcr)
library(dplyr)
library(plotly)
library(rmarkdown)
library(tinytex)
library(shinyBS)
library(purrr)
library(pander)


# Load data
dt_BloodPressure = data.table::fread("./Data/df_SystBloodPressure.csv")
dt_Plasma = data.table::fread("./Data/df_PlasmaVolume.csv")
names(dt_Plasma) = c("subject", "reference", "test_1")
dt_t4 = data.table::fread("./Data/df_T4.csv")

# Global reference
dt_BloodPressure <<- dt_BloodPressure
dt_Plasma <<- dt_Plasma
dt_t4 <<- dt_t4

# List example data
data_list = sort(c("BloodPressure", "T4", "Plasma"))
data_list <<- data_list

dist_options = c("Example Blood Pressure" = 1,
                 "Example Plamas" = 2,
                 "Example T4" = 3,
                 "Upload file (CSV)" = 4)
dist_options <<- dist_options