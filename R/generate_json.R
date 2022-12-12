
# Source functions
source("R/util.R")
source("R/html_colours.R")



# Packages
p_load(
  dplyr,
  highcharter,
  readxl,
  readabs,
  jsonlite
)



# Parse data
emp_cc_qtr <- read_excel("data-raw/empgva_pivot.xlsx", "empgva_cc")
emp_embed_qtr <- read_excel("data-raw/empgva_pivot.xlsx", "empgva_embed") 
emp_total_qtr <- "labour-force-australia-detailed" %>% 
  download_abs_data_cube(cube = "EQ06") %>% 
  read_excel(sheet = "Data 1", skip = 3)





# Summarise data
emp_cc_qtr <- emp_cc_qtr  %>% 
  group_by(qtr)  %>% 
  summarise(
    emp_cc = sum(emp, na.rm = TRUE),
    gva_cc = sum(gva, na.rm = TRUE)
    )

emp_embed_qtr <- emp_embed_qtr  %>% 
  group_by(qtr)  %>% 
  summarise(
    emp_embed = sum(emp_embed, na.rm = TRUE),
    gva_embed = sum(gva_embed, na.rm = TRUE)
    )

emp_total_qtr <- emp_total_qtr %>% 
  filter(`State and territory (STT): ASGS (2011)` == "Victoria") %>%
  transmute(
    tot_emp = (`Employed full-time ('000)`+ `Employed part-time ('000)`) * 1e3, 
    qtr = `Mid-quarter month`
  ) %>% 
  group_by(qtr) %>%
  summarise(tot_emp = sum(tot_emp, na.rm = T)) %>%
  ungroup() 




# Join datasets & create proportion of total employment
emp_qtr <- full_join(emp_cc_qtr, emp_embed_qtr)  %>% 
  transmute(
    emp = round(emp_cc + emp_embed),
    gva = round((gva_cc + gva_embed) * 1e6), 
    qtr = as.Date(qtr)
  ) %>% 
  left_join(emp_total_qtr) %>% 
  mutate(cc_pc = (emp / tot_emp) * 100) %>% 
  select(-tot_emp)

rm(emp_cc_qtr, emp_embed_qtr, emp_total_qtr)




# 



emp_recent <- emp_qtr  %>% 
  slice_max(qtr, n = 1)

emp_qtr  %>% 
  select(x = qtr, y = emp) %>% 
  hchart(
    "area",
    hcaes(x = x, y = y),
    name = 'Employment'
  )  %>% 
  hc_yAxis(min = 2e5)  %>% 
  hc_add_theme(hc_sparkline_fill(line_colour = cv_purple4))  %>%  
  hc_size(height = 80) %>% 
  export_hc("test.json", )
