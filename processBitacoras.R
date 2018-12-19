# library(xlsx)
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
  file.path("bitacoras/Filled", list.files('bitacoras/Filled')) %>%
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

getDB <- function(var_name){
    bitacoras_cleaned %>%
        select(Source, var_name, DN,id) %>%
        group_by(DN,id) %>%
        spread(Source, var_name) %>%
        rename(DB=`Base de datos causa de muerte`, CMT= Comentarios, SVY=`Encuesta/survey`, INT=`Respuesta del entrevistado`)
}


##############################################
##### GET BASIC CASE INFO FROM BITACORAS #####
##############################################


#VictimName
# If confirmed by interview, take DB name first
# If no DB name available, take SVY name
name1 <- getDB("VictimName") %>%
    mutate(VictimName = ifelse(toupper(INT) == 'C' | is.na(INT) | str_detect(INT,'^c\\s') | INT == 'correcto', DB, INT)) %>%
    select(VictimName)
name2 <- getDB("VictimMiddleName") %>%
    mutate(VictimMiddleName = ifelse(toupper(INT) == 'C' | is.na(INT) | str_detect(INT,'^c\\s') | INT == 'correcto', DB, INT)) %>%
    select(VictimMiddleName)
name3 <- getDB("VictimLastName") %>%
    mutate(VictimLastName = ifelse(toupper(INT) == 'C' | is.na(INT) | str_detect(INT,'^c\\s') | INT == 'correcto', DB, INT)) %>%
    select(VictimLastName)
name4 <- getDB("VictimSecondLastName") %>%
    mutate(VictimSecondLastName = ifelse(toupper(INT) == 'C' | is.na(INT) | str_detect(INT,'^c\\s') | INT == 'correcto', DB, INT)) %>%
    select(VictimSecondLastName)

part_name <- name1 %>% merge(name2) %>% merge(name3) %>% merge(name4)


# age
part_age <- getDB("VictimAge") %>%
    mutate(VictimAge = ifelse(toupper(INT) == 'C' | is.na(INT) | str_detect(INT,'^c\\s') | INT == 'correcto', DB, INT)) %>%
    select(VictimAge)

# residential address
municipalities <- readxl::read_excel('data/municipalities.xlsx')
mu <- getUnaccented(paste(tolower(municipalities$Municipality),collapse = '|'))
part_res <- getDB("VictimResidence") %>%
    mutate(VictimResidenceMunicipality = ifelse(
        toupper(INT) == 'C' | is.na(INT) | str_detect(INT,'^C\\s') | str_detect(INT,'^c\\s') | INT == 'correcto',
        DB,
        ifelse(
            # if there is a municipality in INT.VictimResidence
            str_detect(getUnaccented(tolower(INT)), mu),
            # then take this municipality
            str_extract_all(getUnaccented(tolower(INT)), mu, simplify = T)[,1],
            # otherwise take the DB
            DB)
        )) %>%
    select(VictimResidenceMunicipality) %>%
    mutate(VictimResidenceMunicipality = tolower(VictimResidenceMunicipality))
    


# death date
part_deathdate <- getDB("VictimDeathDate") %>%
    mutate(INT = ifelse(!is.na(as.numeric(INT)), as.character(as.Date(as.numeric(INT), origin="1899-12-30")), INT)) %>%
    mutate(VictimDeathDate = ifelse(toupper(INT) == 'C' | is.na(INT) | str_detect(INT,'^C\\s') | str_detect(INT,'correct'), DB, INT)) %>%
    mutate(VictimDeathDate = ifelse(str_detect(VictimDeathDate,'\\d+ (de )?septiembre (de 2017)?'), paste('2017-09-',str_squish(str_sub(VictimDeathDate,1,2)),sep=''), VictimDeathDate)) %>%
    mutate(VictimDeathDate = ifelse(str_detect(VictimDeathDate,'\\d+ (de )?octubre (de 2017)?'), paste('2017-10-',str_squish(str_sub(VictimDeathDate,1,2)),sep=''), VictimDeathDate)) %>%
    mutate(VictimDeathDate = ifelse(str_detect(VictimDeathDate,'\\d+ (de )?noviembre (de 2017)?'), paste('2017-11-',str_squish(str_sub(VictimDeathDate,1,2)),sep=''), VictimDeathDate)) %>%
    mutate(VictimDeathDate = ifelse(str_detect(VictimDeathDate,'\\d+ (de )?diciembre (de 2017)?'), paste('2017-12-',str_squish(str_sub(VictimDeathDate,1,2)),sep=''), VictimDeathDate)) %>%
    mutate(VictimDeathDate = ifelse(str_detect(VictimDeathDate,'\\d+ (de )?enero (de 2018)?'), paste('2018-01-',str_squish(str_sub(VictimDeathDate,1,2)),sep=''), VictimDeathDate)) %>%
    mutate(VictimDeathDate = ifelse(str_detect(VictimDeathDate,'\\d+ (de )?febrero (de 2018)?'), paste('2018-02-',str_squish(str_sub(VictimDeathDate,1,2)),sep=''), VictimDeathDate)) %>%
    mutate(VictimDeathDate = ifelse(str_detect(VictimDeathDate,'\\d+ (de )?abril (de 2017)?'), paste('2018-02-',str_squish(str_sub(VictimDeathDate,1,2)),sep=''), VictimDeathDate)) %>%
    select(VictimDeathDate)



# death province
part_deathPl <- getDB('VictimDeathMunicipality') %>%
    mutate(
        VictimDeathMunicipality = ifelse(
            (toupper(INT) == 'C' | is.na(INT) | str_detect(INT,'correcta|correcto|^C\\s|^c\\s')),
            DB,
            ifelse(
                # if there is a municipality in INT
                str_detect(getUnaccented(tolower(INT)), mu),
                # then take this municipality
                str_extract_all(getUnaccented(tolower(INT)), mu, simplify = T)[,1],
                # otherwise take the survey VictimResidence
                SVY
      ))
  ) %>%
  select(VictimDeathMunicipality) %>%
  mutate(VictimDeathMunicipality = tolower(str_replace(VictimDeathMunicipality,'PUERTO RICO, ','')))

part_cause <- getDB('Direct/indirect CDC criterion') %>%
    mutate(cause = INT) %>%
    select(cause)
    
output <- part_name %>%
    merge(part_age) %>% 
    merge(part_res) %>%
    merge(part_deathdate) %>%
    merge(part_deathPl) %>%
    merge(part_cause) %>%
    filter(is.na(cause) | cause != 'no relacionada') %>%
    mutate(VictimAge =as.numeric(VictimAge)) %>%
    mutate(id = as.numeric(id)) %>%
    mutate(DN = as.numeric(DN)) %>%
    mutate(source = 'survey') %>%
    select(-cause)


####################################
##### ADD DEMOGRAPHIC ANALYSIS #####
####################################

grouped_cause <- read_csv('data/govt_091817_061218.csv') %>% select(DeathNumber,nchsti) %>%
    filter(!is.na(nchsti)) %>%
    mutate(causes = nchsti, DN= DeathNumber) %>%
    select(DN, causes)

output <- output %>% 
    left_join(grouped_cause)

######################################
##### FORMAT FOR THE INTERACTIVE #####
######################################

library(lubridate)
basic <- output %>%
    mutate(
        VictimMiddleName = ifelse(is.na(VictimMiddleName),'',VictimMiddleName),
        VictimSecondLastName = ifelse(is.na(VictimSecondLastName),'',VictimSecondLastName),
        name = str_squish(paste(VictimName, VictimMiddleName, VictimLastName, VictimSecondLastName)),
        age = VictimAge,
        dmu = tolower(VictimDeathMunicipality),
        rmu = tolower(VictimResidenceMunicipality),
        date = VictimDeathDate,
        month = month(as.Date(VictimDeathDate))
    ) %>%
    select(id, DN, name, age, dmu, rmu,date, month, source, causes)


#############################################
##### ADD IN CAUSES OF DEATH CATEGORIES #####
#############################################

causes <- c('Condición de salud directamente relacionada con el huracán',
            'Daños ocasionados por el huracán',
            "Falta de electricidad","Falta de agua o comida",
            "Falta de acceso a atención médica",
            "Falta de acceso a las comunicaciones")

basic <- 
    basic %>%
    merge(getDB('Cause of Death') %>%
    select(SVY, DN,id) %>%
    mutate(
        c1 = ifelse(str_detect(SVY, causes[1]),1,0),
        c2 = ifelse(str_detect(SVY, causes[2]),1,0),
        c3 = ifelse(str_detect(SVY, causes[3]),1,0),
        c4 = ifelse(str_detect(SVY, causes[4]),1,0),
        c5 = ifelse(str_detect(SVY, causes[5]),1,0),
        c6 = ifelse(str_detect(SVY, causes[6]),1,0)
    ) %>%
    select(DN,id, c1,c2,c3,c4,c5,c6))


##############################
##### GET INTERVIEW DATA #####
##############################

interview <- getDB('Circumstances and hurricane relevance') %>%
    mutate(text_field_es = ifelse(nchar(INT) < 50 | is.na(INT), SVY, INT)) %>% 
    select(text_field_es)
    
basic %>%
    merge(interview) %>%
    mutate(causes_en = causes, 
           causes_es = '', text_field_en = '',
           PlaceOfDeath=dmu, 
           DateOfDeath = date) %>%
    select(id, name, DateOfDeath, causes_en, causes_es, text_field_en, text_field_es) %>%
    write.xlsx('data/forProcess/for_translation_copyedits.xlsx',row.names=F)
    
basic %>%
  write.xlsx('data/forProcess/rest_for_merge.xlsx',row.names=F)


## ~~~ copy edits and translation
