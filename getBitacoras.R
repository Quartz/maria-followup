library(tidyverse)
library(stringr)
library(readxl)

######################
### READ NEW CASES ###
######################

cases <- readxl::read_xlsx("data/Las víctimas de María (Responses).xlsx",col_types = 'text')
cases <- 
  cases %>%
  mutate(DeathDate = as.character(as.Date(as.numeric(`Fecha del fallecimiento`), origin="1899-12-30"))) %>%
  mutate(
    case_id = row_number(),
    Name = tolower(str_split(`Nombre del fallecido:`,' ',simplify = T)[,1]),
    LastName = chartr('ÁáàéÉíÍóÓúÚüÜñÑ', 'aaaeeiioouuuunn', tolower(str_split(`Apellidos:`,' ',simplify = T)[,1])),
    DeathMunicipality=chartr('ÁáàéÉíÍóÓúÚüÜñÑ', 'aaaeeiioouuuunn', str_squish(tolower(`Lugar del fallecimiento:`))),
    ResidenceMunicipality=chartr('ÁáàéÉíÍóÓúÚüÜñÑ', 'aaaeeiioouuuunn', str_squish(tolower(`Lugar de residencia:`))),
    SecondLastName = chartr('ÁáàéÉíÍóÓúÚüÜñÑ', 'aaaeeiioouuuunn', tolower(str_split(`Apellidos:`,' ',simplify = T)[,2])))
  


#####################
### CASE MATCHING ###
#####################

govt <- read_csv('data/govt_091817_061218.csv') 
matches <- cases %>%
  select(-Name) %>%
  # match on LastName & SecondLastName
  left_join(govt) %>%
  # Age differences <=5
  filter(as.numeric(`Edad del fallecido:`) <= (Age+5) & as.numeric(`Edad del fallecido:`) >= (Age-5)) %>%
  # Matching death municipality or residence municipality
  filter(DeathMunicipality == MunicipalityDeathPlace | ResidenceMunicipality == ResidencePlace) %>%
  # Matching death date: smaller than 10 day difference, OR matching DeathFacility:
  filter(
    (abs(as.numeric(as.Date(paste(DeathDate_Year, DeathDate_Month, DeathDate_Day, sep='-')) - as.Date(DeathDate)))<=10) 
    | ((DeathPlace == 'HOSPITALIZADO' | DeathPlace == 'AMBULATORIO/SALA DE EMERGENCIA') & `¿Dónde murió la persona?`=='Hospital/Hospital')
    | (DeathPlace == 'RESIDENCIA DE LA PERSONA FALLECIDA' & `¿Dónde murió la persona?`=='Residencia privada')
  ) %>%
  select(case_id, DN, DeathNumber)

matches <- cases %>% 
  filter(!(case_id %in% matches$case_id))%>%
  # Match on Name,LastName
  select(-SecondLastName) %>%
  left_join(govt) %>%
  # Age differences <=5
  filter(as.numeric(`Edad del fallecido:`) <= as.numeric(Age+5) & as.numeric(`Edad del fallecido:`) >= as.numeric(Age-5)) %>%
  # Matching death municipality or residence municipality
  filter(DeathMunicipality == MunicipalityDeathPlace | ResidenceMunicipality == ResidencePlace) %>%
  # Matching death date: smaller than 10 day difference, OR matching DeathFacility:
  filter(
    (abs(as.numeric(as.Date(paste(DeathDate_Year, DeathDate_Month, DeathDate_Day, sep='-')) - as.Date(DeathDate)))<=10) 
    | ((DeathPlace == 'HOSPITALIZADO' | DeathPlace == 'AMBULATORIO/SALA DE EMERGENCIA') & `¿Dónde murió la persona?`=='Hospital/Hospital')
    | (DeathPlace == 'RESIDENCIA DE LA PERSONA FALLECIDA' & `¿Dónde murió la persona?`=='Residencia privada')
  ) %>%
  select(case_id, DN, DeathNumber) %>%
  rbind(matches)

matches <- cases %>% 
  filter(!(case_id %in% matches$case_id))%>%
  # Match on Name, SecondLastName
  select(-LastName) %>%
  left_join(govt) %>%
  # Age differences <=5
  filter(as.numeric(`Edad del fallecido:`) <= as.numeric(Age+5) & as.numeric(`Edad del fallecido:`) >= as.numeric(Age-5)) %>%
  # Matching death municipality or residence municipality
  filter(DeathMunicipality == MunicipalityDeathPlace | ResidenceMunicipality == ResidencePlace) %>%
  # Matching death date: smaller than 10 day difference, OR matching DeathFacility:
  filter(
    (abs(as.numeric(as.Date(paste(DeathDate_Year, DeathDate_Month, DeathDate_Day, sep='-')) - as.Date(DeathDate)))<=10) 
    | ((DeathPlace == 'HOSPITALIZADO' | DeathPlace == 'AMBULATORIO/SALA DE EMERGENCIA') & `¿Dónde murió la persona?`=='Hospital/Hospital')
    | (DeathPlace == 'RESIDENCIA DE LA PERSONA FALLECIDA' & `¿Dónde murió la persona?`=='Residencia privada')
  ) %>%
  select(case_id, DN, DeathNumber) %>%
  rbind(matches)

matches <- cases %>% 
  filter(!(case_id %in% matches$case_id))%>%
  # Match on SecondLastName alone
  select(-LastName, -Name) %>%
  left_join(govt) %>%
  # Age differences <=5
  filter(as.numeric(`Edad del fallecido:`) <= as.numeric(Age+2) & as.numeric(`Edad del fallecido:`) >= as.numeric(Age-2)) %>%
  # Matching death municipality or residence municipality
  filter(DeathMunicipality == MunicipalityDeathPlace | ResidenceMunicipality == ResidencePlace) %>%
  # Matching death date: smaller than 10 day difference, OR matching DeathFacility:
  filter(
    (abs(as.numeric(as.Date(paste(DeathDate_Year, DeathDate_Month, DeathDate_Day, sep='-')) - as.Date(DeathDate)))<=10) 
    | ((DeathPlace == 'HOSPITALIZADO' | DeathPlace == 'AMBULATORIO/SALA DE EMERGENCIA') & `¿Dónde murió la persona?`=='Hospital/Hospital')
    | (DeathPlace == 'RESIDENCIA DE LA PERSONA FALLECIDA' & `¿Dónde murió la persona?`=='Residencia privada')
  ) %>%
  select(case_id, DN, DeathNumber) %>%
  rbind(matches)

matches <- cases %>% 
  filter(!(case_id %in% matches$case_id))%>%
  # Match on LastName alone
  select(-SecondLastName, -Name) %>%
  left_join(govt) %>%
  # Age differences <=5
  filter(as.numeric(`Edad del fallecido:`) <= as.numeric(Age+2) & as.numeric(`Edad del fallecido:`) >= as.numeric(Age-2)) %>%
  # Matching death municipality or residence municipality
  filter(DeathMunicipality == MunicipalityDeathPlace | ResidenceMunicipality == ResidencePlace) %>%
  # Matching death date: smaller than 10 day difference, OR matching DeathFacility:
  filter(
    (abs(as.numeric(as.Date(paste(DeathDate_Year, DeathDate_Month, DeathDate_Day, sep='-')) - as.Date(DeathDate)))<=10) 
    | ((DeathPlace == 'HOSPITALIZADO' | DeathPlace == 'AMBULATORIO/SALA DE EMERGENCIA') & `¿Dónde murió la persona?`=='Hospital/Hospital')
    | (DeathPlace == 'RESIDENCIA DE LA PERSONA FALLECIDA' & `¿Dónde murió la persona?`=='Residencia privada')
  ) %>%
  select(case_id, DN, DeathNumber) %>%
  rbind(matches)

matches <- cases %>% 
  filter(!(case_id %in% matches$case_id))%>%
  # Match on Name alone
  select(-SecondLastName, -LastName) %>%
  left_join(govt) %>%
  # Age differences <=5
  filter(as.numeric(`Edad del fallecido:`) <= as.numeric(Age+2) & as.numeric(`Edad del fallecido:`) >= as.numeric(Age-2)) %>%
  # Matching death municipality or residence municipality
  filter(DeathMunicipality == MunicipalityDeathPlace | ResidenceMunicipality == ResidencePlace) %>%
  # Matching death date: smaller than 10 day difference, OR matching DeathFacility:
  filter(
    (abs(as.numeric(as.Date(paste(DeathDate_Year, DeathDate_Month, DeathDate_Day, sep='-')) - as.Date(DeathDate)))<=10) 
    | ((DeathPlace == 'HOSPITALIZADO' | DeathPlace == 'AMBULATORIO/SALA DE EMERGENCIA') & `¿Dónde murió la persona?`=='Hospital/Hospital')
    | (DeathPlace == 'RESIDENCIA DE LA PERSONA FALLECIDA' & `¿Dónde murió la persona?`=='Residencia privada')
  ) %>%
  select(case_id, DN, DeathNumber) %>%
  rbind(matches)
  

matched_cases <- cases %>%
  filter(case_id %in% matches$case_id) %>%
  left_join(matches) %>%
  select(-Name,-LastName,-SecondLastName) %>%
  left_join(govt)
  

###########################
### TAKE OUT DUPLICATES ###
###########################

# load the existing cases
index <- read_csv('data/case_index.csv')

# cases not existing in the current database
matched_cases <- matched_cases %>%
  filter(!DN %in% index$DN)

# cases do not replicate themselves 
# -- if there are duplicated responses, just pick one
matched_cases <- matched_cases %>%
  arrange(DeathNumber) %>%
  mutate(last_case = lag(DeathNumber)) %>%
  filter(is.na(last_case) | (last_case != DeathNumber)) %>%
  select(-last_case)


##########################
### GENERATE BITACORAS ###
##########################

matched_cases <- matched_cases %>%
  mutate(id = max(max(index$id),3000) + row_number())

source("fillingBitacoras.R")

## ~~ reporters to process the bitacoras


