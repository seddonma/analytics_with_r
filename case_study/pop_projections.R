url <- "http://www.fin.gov.on.ca/en/economy/demographics/projections/table6.xls"
filename <- basename(url)
download.file(url, destfile = filename, mode="wb")
proj_dfs <- lapply(readxl::excel_sheets(filename), readxl::read_excel, path = filename, skip = 5)
projections <- do.call("rbind", lapply(proj_dfs, tidyr::gather, Year, Count, -Age))
rm(proj_dfs)
library(dplyr)
projections <- projections %>%
  filter(!is.na(Age), !is.na(Count)) %>%
  mutate(Age = stringr::str_replace(Age, ".000000", ""),
         Year = stringr::str_replace(Year, ".000000", "")) %>%
  mutate(Age = stringr::str_replace(Age, "90\\+", "90")) %>%
  mutate(Age = as.integer(Age), Year = as.integer(Year)) %>%
  group_by(Year, Age)
