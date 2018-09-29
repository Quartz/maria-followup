# library(readxl)
# library(tidyverse)
# library(stringr)

library(purrr)

######################
### LOAD BITACORAS ###
######################


# read_bitacora() by larry
read_bitacora <- function(filename) {
  
  page1 <- read_excel(filename, sheet = "Información del caso", col_types = 'text')
  page2 <- read_excel(filename, sheet = "Cuestionario", col_types = 'text')
  page3 <- read_excel(filename, sheet = "Estatus del caso", col_types = 'text')
  
  page1_data <- data.frame(t(page1$`X__1`))
  colnames(page1_data) <-
    c(
      "id",
      "Certificate Number",
      "DN",
      "TypeOfDeath"
    )
  
  page2_data <-
    data.frame(t(page2[c(2:4, 17:21, 23, 25, 27:31, 33:39, 48),2:5]))
  colnames(page2_data) <-
    c(
      "InformantName",
      "InformantPhone",
      "InformantMail",
      "Source",
      "VictimName",
      "VictimMiddleName",
      "VictimLastName",
      "VictimSecondLastName",
      "1. InformantRelationship",
      "2. VictimAge",
      "3. VictimDOB",
      "4. VictimResidence",
      "4. VictimResidence1",
      "4. VictimResidence2",
      "4. VictimResidence3",
      "5. VictimDeathDate",
      "5.1. VictimDeathMunicipality",
      "6. VictimOccupation",
      "7. DeathFacility",
      "7.1. DeathFacilityAddress",
      "8. Cause of Death",
      "9. Circumstances and hurricane relevance",
      "Direct/indirect CDC criterion"
    )
  
  # Convert page 3 into a data frame
  page3_data <- data.frame(t(page3$`X__1`[c(1, 2, 5:11, 14, 15, 18:24)]))
  colnames(page3_data) <-
    c(
      paste(
        "First call",
        c(
          "Interviewer",
          "Date and Time",
          "Interview conducted",
          "Message on the phone",
          "Text message",
          "Number does not work",
          "No number",
          "Rescheduled call",
          "Person did not want to cooperate"
        ),
        sep = '-'
      ),
      paste(
        "Second call",
        c(
          "Interviewer",
          "Date and Time",
          "Interview conducted",
          "Message on the phone",
          "Text message",
          "Number does not work",
          "No number",
          "Rescheduled call",
          "Person did not want to cooperate"
        ),
        sep = '-'
      )
    )
  
  # Put it all in one
  data <- cbind(page1_data, page2_data, page3_data) %>%
    mutate(filename = filename)
  
  return(data)
}


bitacoras <-
  file.path("bitacoras/Empty", list.files('bitacoras/Empty')) %>%
  map(read_bitacora) %>%
  reduce(bind_rows) %>%
  mutate(id = as.character(round(parse_number(id))),
         `Certificate Number` = as.character(round(parse_number(`Certificate Number`))),
         DN = as.character(round(parse_number(DN))))







######################
### CLEAN UP CASES ###
######################



getUnaccented <- function(input) {
  return(chartr('ÁáàéÉíÍóÓúÚüÜñÑ', 'aaaeeiioouuuunn', input))
}

processCols <- function(dt) {
  # remove informant and caller info
  dt <- dt[ , !grepl( "Informant" , names(dt))]
  dt <- dt[ , !grepl( "call" , names(dt))]
  dt <- dt[ , !grepl( "TypeOfDeath" , names(dt))]
  dt <- dt[ , !grepl( "Certificate Number" , names(dt))]

  # take out question numbers from colnames
  colnames(dt) <- gsub('[1-9].?[1-9]?. ','',colnames(dt))

  return(dt)
}

bitacoras_cleaned <- processCols(bitacoras) %>%
  select(-filename, -VictimOccupation)


#VictimName
# If confirmed by interview, take DB name first
# If no DB name available, take SVY name
# part_name <-  
#   unverified_bitacoras[,3:15] %>%
#   mutate(
#     VictimName = ifelse(
#       is.na(DB.VictimName),
#       ifelse(toupper(INT.VictimName) == 'C' | is.na(INT.VictimName) | str_detect(INT.VictimName,'^c\\s') | INT.VictimName == 'correcto', 
#              SVY.VictimName, 
#              INT.VictimName),
#       DB.VictimName),
#     VictimMiddleName = ifelse(
#       is.na(DB.VictimMiddleName),
#       ifelse(toupper(INT.VictimMiddleName) == 'C' | is.na(INT.VictimMiddleName) | str_detect(INT.VictimMiddleName,'^C\\s') | INT.VictimMiddleName == 'correcto', 
#              '',INT.VictimMiddleName),
#       DB.VictimMiddleName),
#     VictimLastName = ifelse(
#       is.na(DB.VictimLastName),
#       ifelse(toupper(INT.VictimLastName) == 'C' | is.na(INT.VictimLastName) | str_detect(INT.VictimLastName,'^C\\s') | INT.VictimLastName == 'correcto', 
#              SVY.VictimLastName,INT.VictimLastName),
#       DB.VictimLastName),
#     VictimSecondLastName = ifelse(
#       is.na(DB.VictimSecondLastName),
#       ifelse(toupper(INT.VictimSecondLastName) == 'C' | is.na(INT.VictimSecondLastName) | str_detect(INT.VictimSecondLastName,'^C\\s') | INT.VictimSecondLastName == 'correcto', 
#              '',INT.VictimSecondLastName),
#       DB.VictimSecondLastName)
#   ) %>%
#   # mutate(VictimName = ifelse(is.na(VictimName), SVY.VictimName, VictimName),
#   #        VictimLastName = ifelse(is.na(VictimLastName), SVY.VictimLastName, VictimLastName)) %>%
#   select(VictimName, VictimMiddleName, VictimLastName, VictimSecondLastName) %>%
#   mutate(VictimName = chartr('ÁáàéÉíÍóÓúÚüÜñÑ', 'aaaeeiioouuuunn', tolower(VictimName)),
#          VictimMiddleName = chartr('ÁáàéÉíÍóÓúÚüÜñÑ', 'aaaeeiioouuuunn', tolower(VictimMiddleName)),
#          VictimLastName = chartr('ÁáàéÉíÍóÓúÚüÜñÑ', 'aaaeeiioouuuunn', tolower(VictimLastName)),
#          VictimSecondLastName = chartr('ÁáàéÉíÍóÓúÚüÜñÑ', 'aaaeeiioouuuunn', tolower(VictimSecondLastName))
#   )
# 
# #age
# part_age <- 
#   unverified_bitacoras[,16:22] %>%
#   mutate(
#     VictimAge = ifelse(
#       # the following may require some tweaking
#       (toupper(INT.VictimAge) == 'C' | is.na(INT.VictimAge) | str_detect(INT.VictimAge,'^C\\s') | INT.VictimAge == 'correcto'), 
#       DB.VictimAge, 
#       paste(str_extract_all(INT.VictimAge,'[:digit:]', simplify = T)[,1],
#             str_extract_all(INT.VictimAge,'[:digit:]', simplify = T)[,2], sep=''))
#   ) %>%
#   mutate(VictimAge = ifelse(is.na(VictimAge), SVY.VictimAge, VictimAge)) %>%
#   # View()
#   select(VictimAge)
# 
# 
# ## residential address
# municipalities <- readxl::read_excel('municipalities.xlsx')
# mu <- getUnaccented(paste(tolower(municipalities$Municipality),collapse = '|'))
# municipalities %>%
#   select(Municipality) %>%
#   mutate(abb = getUnaccented(tolower(Municipality))) %>%
#   clipr::write_clip()
# part_res <-
#   unverified_bitacoras[,23:35] %>%
#   bind_cols(unverified_bitacoras[,69]) %>%
#   # View()
#   mutate(
#     VictimResidenceMunicipality = ifelse(
#       is.na(DB.VictimResidence),
#       ifelse(toupper(INT.VictimResidence) == 'C' | is.na(INT.VictimResidence) | str_detect(INT.VictimResidence,'^C\\s') | str_detect(INT.VictimResidence,'^c\\s') | INT.VictimResidence == 'correcto',
#              SVY.VictimResidence,
#              ifelse(
#                # if there is a municipality in INT.VictimResidence
#                str_detect(getUnaccented(tolower(INT.VictimResidence)), mu),
#                # then take this municipality
#                str_extract_all(getUnaccented(tolower(INT.VictimResidence)), mu, simplify = T)[,1],
#                # otherwise take the survey VictimResidence
#                SVY.VictimResidence
#              )
#       ), 
#       DB.VictimResidence)
#   ) %>%
#   mutate(VictimResidenceMunicipality = ifelse(
#     (is.na(VictimResidenceMunicipality) | VictimResidenceMunicipality == ''), 
#     CAP.VictimResidence, 
#     VictimResidenceMunicipality
#   )) %>%
#   select(VictimResidenceMunicipality) %>%
#   mutate(VictimResidenceMunicipality = tolower(VictimResidenceMunicipality))
# 
# 
# # death date
# part_deathdate <-
#   unverified_bitacoras[,36:40] %>%
#   mutate(tmp = as.numeric(INT.VictimDeathDate),
#          tmp2 = as.numeric(SVY.VictimDeathDate),
#          tmp3 = as.numeric(DB.VictimDeathDate)) %>%
#   mutate(INT.VictimDeathDate = ifelse(is.na(tmp), INT.VictimDeathDate,
#                                       as.character(as.Date(tmp, origin="1899-12-30"))),
#          SVY.VictimDeathDate = ifelse(is.na(tmp2), SVY.VictimDeathDate,
#                                       as.character(as.Date(tmp2, origin="1899-12-30"))),
#          DB.VictimDeathDate = ifelse(is.na(tmp3), DB.VictimDeathDate,
#                                      as.character(as.Date(tmp3, origin="1899-12-30")))
#   ) %>%
#   select(-tmp,-tmp2,-tmp3) %>%
#   # View()
#   mutate(
#     VictimDeathDate = ifelse(
#       (toupper(INT.VictimDeathDate) == 'C' | is.na(INT.VictimDeathDate) | str_detect(INT.VictimDeathDate,'^C\\s') | INT.VictimDeathDate == 'correcta' | str_detect(INT.VictimDeathDate,'correcto')),
#       DB.VictimDeathDate,
#       INT.VictimDeathDate)
#   ) %>%
#   mutate(
#     VictimDeathDate = 
#       ifelse(is.na(VictimDeathDate),
#              ifelse(str_detect(SVY.VictimDeathDate,'2018|2017'),
#                     SVY.VictimDeathDate,NA),
#              VictimDeathDate)
#   ) %>%
#   mutate(VictimDeathDate = ifelse(str_detect(VictimDeathDate,'\\d+ (de )?septiembre (de 2017)?'), paste('2017-09-',str_squish(str_sub(VictimDeathDate,1,2)),sep=''), VictimDeathDate)) %>%
#   mutate(VictimDeathDate = ifelse(str_detect(VictimDeathDate,'\\d+ (de )?abril (de 2017)?'), paste('2018-02-',str_squish(str_sub(VictimDeathDate,1,2)),sep=''), VictimDeathDate)) %>%
#   mutate(VictimDeathDate = ifelse(str_detect(VictimDeathDate,'\\d+ (de )?octubre (de 2017)?'), paste('2017-10-',str_squish(str_sub(VictimDeathDate,1,2)),sep=''), VictimDeathDate)) %>%
#   mutate(VictimDeathDate = ifelse(str_detect(VictimDeathDate,'\\d+ (de )?noviembre (de 2017)?'), paste('2017-11-',str_squish(str_sub(VictimDeathDate,1,2)),sep=''), VictimDeathDate)) %>%
#   mutate(VictimDeathDate = ifelse(str_detect(VictimDeathDate,'\\d+ (de )?diciembre (de 2017)?'), paste('2017-12-',str_squish(str_sub(VictimDeathDate,1,2)),sep=''), VictimDeathDate)) %>%
#   mutate(VictimDeathDate = ifelse(str_detect(VictimDeathDate,'\\d+ (de )?febrero (de 2018)?'), paste('2018-02-',str_squish(str_sub(VictimDeathDate,1,2)),sep=''), VictimDeathDate)) %>%
#   mutate(VictimDeathDate = ifelse(VictimDeathDate == 'Murió el 15, pero la certificaron el 17', '2017-11-15', VictimDeathDate)) %>%
#   mutate(VictimDeathDate = ifelse(str_detect(VictimDeathDate, 'de defunción pero ella cree'), '2017-10-03', VictimDeathDate)) %>%
#   select(VictimDeathDate) %>%
#   mutate(VictimDeathDate = ifelse(VictimDeathDate == '1899-12-30', '2017-10-03', VictimDeathDate)) %>%
#   mutate(VictimDeathDate = ifelse(VictimDeathDate == 'Septiembre 25, 2017', '2017-09-25', VictimDeathDate)) %>%
#   mutate(VictimDeathDate = ifelse(VictimDeathDate == 'octubre 19,2017', '2017-10-19', VictimDeathDate)) %>%
#   mutate(VictimDeathDate = ifelse(VictimDeathDate == '2017-09-En', '2017-09-23', VictimDeathDate)) %>%
#   mutate(VictimDeathDate = ifelse(VictimDeathDate == '2017-10-La', '2017-10-01', VictimDeathDate)) %>%
#   mutate(VictimDeathDate = ifelse(VictimDeathDate == '18/abril/18', '2018-4-18', VictimDeathDate)) %>%
#   mutate(VictimDeathDate = ifelse(VictimDeathDate == '7/octubre/2017', '2017-10-07', VictimDeathDate)) %>%
#   mutate(VictimDeathDate = ifelse(VictimDeathDate == '12/Febrero/2018', '2018-2-12', VictimDeathDate)) %>%
#   mutate(VictimDeathDate = ifelse(str_sub(VictimDeathDate,1,5) == 'no sa', '2017-11-30', VictimDeathDate)) %>%
#   mutate(DeathYear = as.numeric(str_sub(VictimDeathDate,1,4)))
# 
# # death province
# part_deathPl <-
#   unverified_bitacoras[,41:45] %>%
#   # View()
#   mutate(
#     VictimDeathMunicipality = ifelse(
#       (toupper(INT.VictimDeathMunicipality) == 'C' | is.na(INT.VictimDeathMunicipality) | str_detect(INT.VictimDeathMunicipality,'correcta|correcto|^C\\s|^c\\s')), 
#       DB.VictimDeathMunicipality,
#       ifelse(
#         # if there is a municipality in INT.VictimResidence
#         str_detect(getUnaccented(tolower(INT.VictimDeathMunicipality)), mu),
#         # then take this municipality
#         str_extract_all(getUnaccented(tolower(INT.VictimDeathMunicipality)), mu, simplify = T)[,1],
#         # otherwise take the survey VictimResidence
#         SVY.VictimDeathMunicipality
#       ))
#   ) %>%
#   select(VictimDeathMunicipality) %>%
#   mutate(VictimDeathMunicipality = tolower(str_replace(VictimDeathMunicipality,'PUERTO RICO, ','')))
# 
# part_DeathFacility <- 
#   unverified_bitacoras[,50:58] %>%
#   # View()
#   mutate(DeathFacility = ifelse(
#     toupper(INT.DeathFacility) == 'C' | is.na(INT.DeathFacility) | str_detect(INT.DeathFacility,'correcta|correcto'),
#     DB.DeathFacility,INT.DeathFacility
#   )) %>%
#   mutate(DeathFacility = ifelse(
#     is.na(DeathFacility),
#     ifelse(!is.na(SVY.DeathFacility), SVY.DeathFacility, CAP.DeathFacility),
#     DeathFacility
#   )) %>%
#   mutate(DeathFacilityAddress = ifelse(
#     toupper(INT.DeathFacilityAddress) == 'C' | is.na(INT.DeathFacilityAddress) | str_detect(INT.DeathFacilityAddress,'correcta|correcto'),
#     DB.DeathFacilityAddress,INT.DeathFacilityAddress
#   )) %>%
#   mutate(DeathFacilityAddress = ifelse(
#     is.na(DeathFacilityAddress),
#     ifelse(!is.na(SVY.DeathFacilityAddress), SVY.DeathFacilityAddress, CAP.DeathFacilityAddress),
#     DeathFacilityAddress
#   )) %>%
#   select(DeathFacility,DeathFacilityAddress)
# 
# part_cause <-
#   unverified_bitacoras[,60:68]%>%
#   bind_cols(unverified_bitacoras[,70])
# 
# output <- unverified_bitacoras[,1:2] %>%
#   bind_cols(part_name) %>%
#   bind_cols(part_age) %>%
#   bind_cols(part_res) %>%
#   bind_cols(part_deathdate) %>%
#   bind_cols(part_deathPl) %>%
#   bind_cols(part_DeathFacility) %>%
#   bind_cols(part_cause)# %>%
# 



## ~~~ copy edits and translation
