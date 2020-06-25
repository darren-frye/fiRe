library(plyr)
library(dplyr)

##RE Data
baseRE <- read.csv('Data/Real_Estate_Base_Data.csv', stringsAsFactors = FALSE)
baseRE <- baseRE %>%
  select(StreetNumber, StreetName, Unit, StateCode, GPIN, Zone, ParcelNumber, Acreage) %>%
  mutate(StreetNumber = gsub("[^0-9]", "", StreetNumber),
         StreetName = trimws(toupper(gsub("[^A-Z ]", "", StreetName))))

resRE <- read.csv('Data/Real_Estate_Residential_Details.csv', stringsAsFactors = FALSE)
resRE <- resRE %>%
  mutate(StreetNumber = gsub("[^0-9]", "", StreetNumber),
         StreetName = trimws(toupper(gsub("[^A-Z ]", "", StreetName)))) %>%
  select(ParcelNumber, UseCode, Style, Grade, Roof, Heating, Fireplace, YearBuilt, TotalRooms, Bedrooms, 
         FullBathrooms, BasementGarage, Basement, FinishedBasement, BasementType, ExternalWalls, 
         NumberOfStories, SquareFootageFinishedLiving, resStreetName = StreetName, resStreetNumber = StreetNumber, 
         resUnit = Unit)

commRE <- read.csv('Data/Real_Estate_Commercial_Details.csv', stringsAsFactors = FALSE)
commRE <- commRE %>%
  mutate(StreetNumber = gsub("[^0-9]", "", StreetNumber),
         StreetName = trimws(toupper(gsub("[^A-Z ]", "", StreetName)))) %>%
  select(ParcelNumber, UseCode, YearBuilt, GrossArea, StoryHeight, NumberOfStories, commStreetName = StreetName, 
         commStreetNumber = StreetNumber, commUnit = Unit)

RE <- join_all(list(baseRE, resRE, commRE), by = "ParcelNumber", type = "full", match = "first") %>%
  distinct()

dupes <- RE %>% 
  count(ParcelNumber) %>% 
  filter(n>1) %>%
  select(ParcelNumber)

dupeRE <- RE %>%
  filter(ParcelNumber %in% dupes$ParcelNumber)

##Parcel Data

areaPAR <- read.csv('Data/Parcel_Area_Details.csv', stringsAsFactors = FALSE)
areaPAR <- areaPAR %>%
  select(FileType, LotSquareFeet, TaxYear, Zoning, Assessment, 
         GPIN = GeoParcelIdentificationNumber, ParcelNumber, StreetNumber, StreetName, Unit) %>%
  mutate(StreetNumber = gsub("[^0-9]", "", StreetNumber),
         StreetName = trimws(toupper(gsub("[^A-Z ]", "", StreetName))))

pointsPAR <- read.csv('Data/Parcel_Owner_Points.csv', stringsAsFactors = FALSE)
pointsPAR <- pointsPAR %>%
  select(X, Y, GPIN = GeoParcelIdentificationNumber, OwnerName, ParcelNumber, StreetNumber, StreetName,
         Unit, ZipCode, Zone) %>%
  mutate(StreetNumber = gsub("[^0-9]", "", StreetNumber),
         StreetName = trimws(toupper(gsub("[^A-Z ]", "", StreetName))))

PAR <- join_all(list(areaPAR, pointsPAR), by = c("ParcelNumber", "GPIN"), type = "full", match = "first") %>%
  distinct()

##Permits and Structure

buildPER <- read.csv('Data/Building_Permits_Spatial_.csv', stringsAsFactors = FALSE)
buildPER <- buildPER %>%
  select(IssuedDate, ParcelNumber, SubType, Type, WorkDescription)

existSTR <- read.csv('Data/Existing_Structure_Area.csv', stringsAsFactors = FALSE)
existSTR <- existSTR %>%
  select(BIN, StreetNumber = ST_NUMBER, StreetName = STREET) %>%
  mutate(StreetNumber = gsub("[^0-9]", "", StreetNumber),
         StreetName = trimws(toupper(gsub("[^A-Z ]", "", StreetName))))

##Let's get wild

mainOut <- join_all(list(RE, PAR), by = "ParcelNumber", type ="full", match = "first")
mainOut <- join_all(list(mainOut, existSTR), by = c("StreetNumber", "StreetName"), type = "left", match = "first") %>%
  distinct()
  
write.csv(mainOut, "Data/Output.csv", row.names = FALSE)
