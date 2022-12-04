

# Packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(
    tidyverse,
    highcharter,
    readxl
)



# Load in data
emp_cc_qtr <- read_excel("data-raw/empgva_pivot.xlsx", sheet = "empgva_cc")

industr_gva_summarytables <- read_excel(
    "data-raw/empgva_summarytables.xlsx",
    sheet = "tabout_empgva_bench"
)
empgva_summarytables_cc_share <- read_excel(
    "data-raw/empgva_summarytables.xlsx",
    sheet = "tabout_ccembedshare"
)
