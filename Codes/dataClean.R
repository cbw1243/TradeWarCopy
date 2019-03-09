rm(list = ls())

library(dplyr)
library(tidyr)
library(data.table)
library(countrycode)

#-------------------------------------------------------------------------------------------------------------#
# CLean the soybean data.
#-------------------------------------------------------------------------------------------------------------#
soyDatRaw <- fread('../Data/USSoybeanMonthExport.csv')

soyDat <- soyDatRaw %>%
  mutate(Year = substr(Year, 1, 4)) %>%
  select(1:15) %>%
  mutate(PartnerISO = countrycode(Partner, 'country.name', 'iso3c')) %>%
  gather(MonthName, ExportValue, 4:15) %>%
  mutate(ExportValue = as.numeric(gsub(",", "", ExportValue))) %>%
  drop_na() %>% # The data for December 2018 do not exist.
  mutate(Month = match(MonthName, month.name))

write.csv(soyDat, file = '../Data/USSoybeanMonthExportClean.csv', row.names = F)
# End.
#-------------------------------------------------------------------------------------------------------------#


