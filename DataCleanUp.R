# Dependencies
library(tidyverse)

### Loading Data ###
# Rental Data
# Rental Studio Inventory
RentalStudioInventory = read.csv("./rawData/F1_rentalInventory_Studio.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         AreaType == "neighborhood",
         Boro != "Bronx") %>%
  gather(., 
         key = "yearMonth", 
         value = "rentalStudioInventory", 
         X2010.01:X2018.08) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., Area, Boro, time, rentalStudioInventory)

# Rental Studio Median Listing Price
RentalStudioMedianPrice = read.csv("./rawData/F2_medianAskingRent_Studio.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         AreaType == "neighborhood",
         Boro != "Bronx") %>%
  gather(., 
         key = "yearMonth", 
         value = "rentalStudioMedPrice", 
         X2010.01:X2018.08) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., Area, Boro, time, rentalStudioMedPrice)

# Rental One Bedroom Inventory
RentalOneBdInventory = read.csv("./rawData/G1_rentalInventory_OneBd.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         AreaType == "neighborhood",
         Boro != "Bronx") %>%
  gather(., 
         key = "yearMonth", 
         value = "rentalOneBdInventory", 
         X2010.01:X2018.08) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., Area, Boro, time, rentalOneBdInventory)

# Rental One Bedroom Median Listing Price
RentalOneBdMedianPrice = read.csv("./rawData/G2_medianAskingRent_OneBd.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         AreaType == "neighborhood",
         Boro != "Bronx") %>%
  gather(., 
         key = "yearMonth", 
         value = "rentalOneBdMedPrice", 
         X2010.01:X2018.08) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., Area, Boro, time, rentalOneBdMedPrice)

# Rental Two Bedroom Inventory
RentalTwoBdInventory = read.csv("./rawData/H1_rentalInventory_TwoBd.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         AreaType == "neighborhood",
         Boro != "Bronx") %>%
  gather(., 
         key = "yearMonth", 
         value = "rentalTwoBdInventory", 
         X2010.01:X2018.08) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., Area, Boro, time, rentalTwoBdInventory)

# Rental Two Bedroom Median Listing Price
RentalTwoBdMedianPrice = read.csv("./rawData/H2_medianAskingRent_TwoBd.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         AreaType == "neighborhood",
         Boro != "Bronx") %>%
  gather(., 
         key = "yearMonth", 
         value = "rentalTwoBdMedPrice", 
         X2010.01:X2018.08) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., Area, Boro, time, rentalTwoBdMedPrice)

# Rental Three Bedroom Inventory
RentalThreeBdInventory = read.csv("./rawData/I1_rentalInventory_ThreePlusBd.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         AreaType == "neighborhood",
         Boro != "Bronx") %>%
  gather(., 
         key = "yearMonth", 
         value = "rentalThreeBdInventory", 
         X2010.01:X2018.08) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., Area, Boro, time, rentalThreeBdInventory)

# Rental Three Bedroom Median Listing Price
RentalThreeBdMedianPrice = read.csv("./rawData/I2_medianAskingRent_ThreePlusBd.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         AreaType == "neighborhood",
         Boro != "Bronx") %>%
  gather(., 
         key = "yearMonth", 
         value = "rentalThreeBdMedPrice", 
         X2010.01:X2018.08) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., Area, Boro, time, rentalThreeBdMedPrice)

# Sales Data
# Condo Inventory
CondoInventory = read.csv("./rawData/B1_totalInventory_Condo.csv", stringsAsFactors = FALSE) %>% 
  filter(., 
         AreaType == "neighborhood",
         Boro != "Bronx") %>% 
  gather(., 
         key = "yearMonth", 
         value = "condoInventory", 
         X2010.01:X2018.08) %>% 
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>% 
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., Area, Boro, time, condoInventory)

# Condo Median Asking Price
CondoMedAskPrice = read.csv("./rawData/B3_medianAskingPrice_Condo.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         AreaType == "neighborhood",
         Boro != "Bronx") %>%
  gather(., 
         key = "yearMonth", 
         value = "condoMedAskPrice", 
         X2010.01:X2018.08) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., Area, Boro, time, condoMedAskPrice)

# Condo Median Sale Price
CondoMedSalePrice = read.csv("./rawData/B6_medianSalePrice_Condo.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         AreaType == "neighborhood",
         Boro != "Bronx") %>%
  gather(., 
         key = "yearMonth", 
         value = "condoMedSalePrice", 
         X2010.01:X2018.08) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., Area, Boro, time, condoMedSalePrice)

# Single Family Inventory
SFInventory = read.csv("./rawData/D1_totalInventory_Sfr.csv", stringsAsFactors = FALSE) %>% 
  filter(., 
         AreaType == "neighborhood",
         Boro != "Bronx") %>% 
  gather(., 
         key = "yearMonth", 
         value = "sfInventory", 
         X2010.01:X2018.08) %>% 
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>% 
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., Area, Boro, time, sfInventory)

# Single Family Median Asking Price
SFMedAskPrice = read.csv("./rawData/D3_medianAskingPrice_Sfr.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         AreaType == "neighborhood",
         Boro != "Bronx") %>%
  gather(., 
         key = "yearMonth", 
         value = "sfMedAskPrice", 
         X2010.01:X2018.08) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., Area, Boro, time, sfMedAskPrice)

# Single Family Median Sale Price
SFMedSalePrice = read.csv("./rawData/D6_medianSalePrice_Sfr.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         AreaType == "neighborhood",
         Boro != "Bronx") %>%
  gather(., 
         key = "yearMonth", 
         value = "sfMedSalePrice", 
         X2010.01:X2018.08) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., Area, Boro, time, sfMedSalePrice)

### Combined Data ###
StreetEasyCombined = RentalStudioInventory %>% 
  left_join(., RentalStudioMedianPrice, by = c("Boro", "Area", "time")) %>%
  left_join(., RentalOneBdInventory, by = c("Boro", "Area", "time")) %>%
  left_join(., RentalOneBdMedianPrice, by = c("Boro", "Area", "time")) %>%
  left_join(., RentalTwoBdInventory, by = c("Boro", "Area", "time")) %>%
  left_join(., RentalTwoBdMedianPrice, by = c("Boro", "Area", "time")) %>%
  left_join(., RentalThreeBdInventory, by = c("Boro", "Area", "time")) %>%
  left_join(., RentalThreeBdMedianPrice, by = c("Boro", "Area", "time")) %>%
  left_join(., CondoInventory, by = c("Boro", "Area", "time")) %>% 
  left_join(., CondoMedAskPrice, by = c("Boro", "Area", "time")) %>%
  left_join(., CondoMedSalePrice, by = c("Boro", "Area", "time")) %>%
  left_join(., SFInventory, by = c("Boro", "Area", "time")) %>%
  left_join(., SFMedAskPrice, by = c("Boro", "Area", "time")) %>%
  left_join(., SFMedSalePrice, by = c("Boro", "Area", "time"))
  
### Write to File for backup ###
write.csv(StreetEasyCombined, file = "StreetEasyCombined.csv", row.names=FALSE)
