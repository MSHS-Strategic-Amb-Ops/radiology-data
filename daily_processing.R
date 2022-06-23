# Load packages ----------------------------------------------------------------
suppressMessages({
  memory.limit(size = 10000000)
  library(readxl)
  library(writexl)
  library(plyr)
  library(dplyr)
  library(data.table)
  library(zoo)
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinyWidgets)
  library(htmlwidgets)
  library(lubridate)
  library(tcltk)
  library(tidyverse)
  library(plotly)
  library(knitr)
  library(kableExtra)
  library(leaflet)
  library(grid)
  library(gridExtra)
  library(eeptools)
  library(ggQC)
  #library(zipcode)
  library(utils)
  library(scales)
  library(chron)
  library(bupaR)
  library(shiny)
  library(DT)
  library(DiagrammeR)
  library(shinyalert)
  library(edeaR)
  library(processmapR)
  library(processmonitR)
  library(processanimateR)
  library(tidyr)
  library(lubridate)
  library(RColorBrewer)
  library(DiagrammeR)
  library(ggplot2)
  library(leaflet)
  library(readr)
  library(highcharter)
  library(ggforce) # for 'geom_arc_bar'
  library(packcircles) # for packed circle graph
  library(viridis)
  library(ggiraph)
  library(treemapify)
  library(treemap)
  library(broom)
  library(extrafont)
  library(tis) # for US holidays
  library(vroom)
  library(sjmisc)
  library(tools)
  library(here)
  library(shinyBS)
  library(shinyscreenshot)
  library(fasttime)
  library(shinycssloaders)
  library(feather)
  # library(zipcodeR)
  library(formattable)
  library(shinyjs)
  library(janitor)
  library(patchwork)
})

dir_path <- getwd()

# 1. Read in daily radiology data -------------------------------------------------
daily_data <- read_xls(list.files(dir_path, 
                           pattern="mshp_Ambulatory.*\\.xls$", 
                           full.names=TRUE)) # Read in data that starts with "mshs_Ambulatory"

# Import in site mapping crosswalk --------------------------------------------- 
site_mappings <- read_csv(paste0(dir_path,"/radiology_site_mapping.csv"))

# Data Processing --------------------------------------------------------------
## Processing Function =========================================================
daily_process <- function(data){
  # Rename columns to resemble Epic patient access data (historical_data)
  original.cols <- c("DepartmentCode","Modality","requestor","Exam",
                     "SchedOnDTTM","DurationMinutes",
                     "Scheduled","Arrival","Begin","Complete","Depart","Cancel",
                     "Reason","DOB","Mrn","Preliminary","Finalized",
                     "Zip","ExamStatus","OrgCode")
  
  new.cols <- c("DepartmentCode","Modality","Requestor","Exam",
                     "Appt.Made.DTTM","Appt.Dur",
                     "Appt.DTTM","Arrived.DTTM","Begin.DTTM","Complete.DTTM","Depart.DTTM","Cancel.DTTM",
                     "Cancel.Reason","Birth.Date","MRN","Preliminary.DTTM","Finalized.DTTM",
                     "Zip.Code","ExamStatus","OrgCode")
  
  data <- data[original.cols]
  colnames(data) <- new.cols
  
  # Creating additional columns for analysis
  data$Campus <- site_mappings$Campus[match(data$OrgCode, site_mappings$ID)] # Map Campus Names
  data <- data %>%
    mutate(Department = "Radiology",
           Appt.DateYear = as.Date(Appt.DTTM, format="%Y-%m-%d"),
           Appt.MonthYear = format(Appt.DateYear, "%Y-%m"),
           Appt.Date = format(Appt.DateYear, "%m-%d"),
           Appt.Year = format(Appt.DateYear, "%Y"),
           Appt.Month = format(Appt.DateYear, "%b"),
           Appt.Quarter = quarters(Appt.DateYear),
           Appt.Week = floor_date(Appt.DateYear, unit="week", week_start = 1),
           Appt.Day = format(Appt.DateYear, "%a"),
           Appt.Time = format(as.POSIXct(as.ITime(Appt.DTTM, format = "%H:%M")), "%H:%M"),
           Appt.TM.Hr = format(strptime(as.ITime(floor_date(Appt.DTTM, "hour")), "%H:%M:%S"),'%H:%M'),
           Lead.Days = as.Date(Appt.DTTM, format="%Y-%m-%d")-as.Date(Cancel.DTTM, format="%Y-%m-%d"),
           Wait.Time = as.Date(Appt.DTTM, format="%Y-%m-%d")-as.Date(Appt.Made.DTTM, format="%Y-%m-%d")
    )
}


## Processed data output =======================================================
daily_data_processed <- daily_process(daily_data)

# Import in radiology_historical_data ------------------------------------------
repo_data <- readRDS(pate0(dir_path,"/radiology_historical_data.rds"))

# Merge daily processed data with historical data ------------------------------
merged_data <- bind_rows(repo_data, daily_data_processed)

# Write out appended radiology data --------------------------------------------
saveRDS(merged_data, "radiology_historical_data.rds")


# test <- readRDS("radiology_historical_data.rds")
# 
# 
# library(XML)
# library(textreadr)
# install.packages("textreadr")
# 
# url <- "http://10.5.4.230:1722/tools/tool_SOT_view.php?report_type=0"
# data <- as.data.frame(read_xml(url))
# 
# xml(url)
# 
# data <- data[23:nrow(data),]
# test <- as.data.frame(t(matrix(data,nrow = 22)))
# 
# data <- data %>%
#   mutate(colnew = as.integer(gl(n(), 22, n())),
#          rn = str_c('col', rowid(colnew))) %>%
#   pivot_wider(names_from = rn, values_from = read_xml(url)) %>%
#   select(-colnew)
# 
# set.seed(24)
# df1 <- data.frame(col1 = rnorm(57))
# 
# 
# library(XML)
# install.packages("RCurl")
# library(RCurl)
# library(rlist)
# 
# theurl <- getURL(url,.opts = list(ssl.verifypeer = FALSE) )
# tables <- readHTMLTable(theurl)
# tables <- list.clean(tables, fun = is.null, recursive = FALSE)
# n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
# 
# as.data.frame(n.rows)
