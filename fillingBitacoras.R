### CREATE EMPTY FORMS ###
library(xlsx)
library(openxlsx)

tmp1 <- readxl::read_xlsx('bitacoras/template/bitácora.xlsx', sheet = 1)
tmp2 <- readxl::read_xlsx('bitacoras/template/bitácora.xlsx', sheet = 2)
tmp3 <- readxl::read_xlsx('bitacoras/template/bitácora.xlsx', sheet = 3)


for (i in 1:nrow(matched_cases)) {
  # i=1
  print(i)
  
  form <- createWorkbook()
  caseInfo <- createSheet(form, sheetName="Información del caso")
  questionnaire <- createSheet(form, sheetName="Cuestionario")
  statusInfo <- createSheet(form, sheetName="Estatus del caso")
  
  #####################
  ## FILL CASE SHEET ##
  #####################
  
  caseRows <- c('Información del caso',tmp1$`Información del caso`)
  col2 <- rep('',length(caseRows))
  caseData <- data.frame(col1 = caseRows, col2 = col2)
  addDataFrame(x=caseData, sheet=caseInfo, row.names = F, col.names = F)
  caseCells = getCells(getRows(caseInfo), colIndex=1:2)
  
  # ADD STYLES
  
  al <- Alignment(wrapText = T)
  headBorder <- Border(color='black',position='TOP', pen='BORDER_MEDIUM')
  headStyle <- CellStyle(form) +CellProtection(locked=T) +Font(form, isBold = T, heightInPoints = 14) + Alignment(wrapText = T)
  setCellStyle(caseCells$`1.1`, headStyle)
  # add border
  CB.setBorder(CellBlock(caseInfo, 1,1,1,2,FALSE), headBorder, colIndex=1:2, rowIndex=1)
  # spacing
  setColumnWidth(caseInfo, 1, 30)
  
  
  ########################
  ## FILL QUESTIONNAIRE ##
  ########################
  
  col1 <- c('Información del informante',tmp2$`Información del informante`)
  col1 <- ifelse(is.na(col1),'', col1)
  
  col2 <- c('',tmp2$X__1)
  col2 <- ifelse(is.na(col2),'', col2)
  
  col3 <- c('',tmp2$X__2)
  col3 <- ifelse(is.na(col3),'', col3)
  
  col4 <- c('',tmp2$X__3)
  col4 <- ifelse(is.na(col4),'', col4)
  
  col5 <- c('',tmp2$X__4)
  col5 <- ifelse(is.na(col5),'', col5)
  
  
  qData <- data.frame(col1 = col1, col2 = col2, col3 = col3, col4 = col4, col5 = col5)
  addDataFrame(x=qData, sheet=questionnaire, row.names = F, col.names = F)
  qCells = getCells(getRows(questionnaire), colIndex=1:5)
  
  # ADD STYLES
  
  # spacing
  setColumnWidth(questionnaire, 1, 60)
  setColumnWidth(questionnaire, 2, 40)
  setColumnWidth(questionnaire, 3, 40)
  setColumnWidth(questionnaire, 4, 40)
  setColumnWidth(questionnaire, 5, 40)
  
  # cell stlyes
  setCellStyle(qCells$`1.1`, headStyle)
  setCellStyle(qCells$`7.1`, headStyle)
  setCellStyle(qCells$`10.1`, headStyle)
  setCellStyle(qCells$`13.1`, headStyle)
  setCellStyle(qCells$`16.1`, headStyle)
  setCellStyle(qCells$`45.1`, headStyle)
  setCellStyle(qCells$`48.1`, headStyle)
  
  setCellStyle(qCells$`17.1`, CellStyle(form)+CellProtection(locked=T)+Font(form, isBold = T))
  
  sourceStyle <- CellStyle(form)+CellProtection(locked=T)+Font(form, color = 'red', isBold=T)
  setCellStyle(qCells$`2.2`, sourceStyle)
  setCellStyle(qCells$`2.3`, sourceStyle)
  setCellStyle(qCells$`18.2`, sourceStyle)
  setCellStyle(qCells$`18.3`, sourceStyle)
  
  intStyle <- CellStyle(form)+CellProtection(locked=T)+Font(form, color = 'blue', isBold=T)
  setCellStyle(qCells$`18.4`, intStyle)
  setCellStyle(qCells$`18.5`, intStyle)
  
  qStyle <- CellStyle(form)+CellProtection(locked=T)+Alignment(wrapText = T)+Font(form, isItalic = T)
  setCellStyle(qCells$`8.1` ,qStyle)
  setCellStyle(qCells$`11.1` ,qStyle)
  setCellStyle(qCells$`14.1` ,qStyle)
  setCellStyle(qCells$`24.1` ,qStyle)
  setCellStyle(qCells$`26.1` ,qStyle)
  setCellStyle(qCells$`28.1` ,qStyle)
  setCellStyle(qCells$`29.1` ,qStyle)
  setCellStyle(qCells$`34.1` ,qStyle)
  setCellStyle(qCells$`35.1` ,qStyle)
  setCellStyle(qCells$`36.1` ,qStyle)
  setCellStyle(qCells$`37.1` ,qStyle)
  setCellStyle(qCells$`38.1` ,qStyle)
  setCellStyle(qCells$`39.1` ,qStyle)
  setCellStyle(qCells$`40.1` ,qStyle)
  setCellStyle(qCells$`41.1` ,qStyle)
  setCellStyle(qCells$`42.1` ,qStyle)
  setCellStyle(qCells$`43.1` ,qStyle)
  setCellStyle(qCells$`46.1` ,qStyle)
  
  # add note style
  noteFontHed = Font(form, color = 'gray30',isBold = T)
  noteFont = Font(form, color = 'gray40')
  CB.setBorder(CellBlock(questionnaire, 54,1,1,5,FALSE), headBorder, colIndex=1:5, rowIndex=1)
  CB.setFont(CellBlock(questionnaire, 54,1,11,1,FALSE), noteFont, colIndex=1, rowIndex=1:11)
  setCellStyle(qCells$`55.1`, CellStyle(form)+noteFont+Alignment(wrapText = T))
  setCellStyle(qCells$`56.1`, CellStyle(form)+noteFont+Alignment(wrapText = T))
  setCellStyle(qCells$`57.1`, CellStyle(form)+noteFont+Alignment(wrapText = T))
  setCellStyle(qCells$`59.1`, CellStyle(form)+noteFont+Alignment(wrapText = T))
  setCellStyle(qCells$`61.1`, CellStyle(form)+noteFont+Alignment(wrapText = T))
  setCellStyle(qCells$`62.1`, CellStyle(form)+noteFont+Alignment(wrapText = T))
  
  # fill
  CB.setFill(CellBlock(questionnaire, 19,4,4,1,FALSE), Fill(backgroundColor = 'lightblue') , colIndex=1, rowIndex=1:4)
  setCellStyle(qCells$`24.4`,CellStyle(form)+Fill(backgroundColor = 'lightblue'))
  setCellStyle(qCells$`26.4`,CellStyle(form)+Fill(backgroundColor = 'lightblue'))
  CB.setFill(CellBlock(questionnaire, 28,4,2,1,FALSE), Fill(backgroundColor = 'lightblue') , colIndex=1, rowIndex=1:2)
  CB.setFill(CellBlock(questionnaire, 34,4,7,1,FALSE), Fill(backgroundColor = 'lightblue') , colIndex=1, rowIndex=1:7)
  setCellStyle(qCells$`49.4`,CellStyle(form)+Fill(backgroundColor = 'lightblue'))
  # CB.setFill(CellBlock(questionnaire, 29,3,4,1,FALSE), Fill(backgroundColor = '#FFC0CB'), colIndex=1, rowIndex=1:4)
  # setCellStyle(qCells$`39.3`,CellStyle(form)+Fill(backgroundColor = '#FFC0CB'))
  
  
  filledStyle <- CellStyle(form)+CellProtection(locked=T)+Alignment(wrapText = T)
  setCellStyle(qCells$`36.3`, filledStyle)
  setCellStyle(qCells$`39.2`, filledStyle)
  setCellStyle(qCells$`39.3`, filledStyle)
  setCellStyle(qCells$`40.2`, filledStyle)
  
  
  ############################
  ## FILL INTERVIEWER SHEET ##
  ############################
  
  interviewerRows <- c('Primera llamada',tmp3$`Primera llamada`)
  col2 <- rep('',length(interviewerRows))
  interviewerData <- data.frame(col1 = interviewerRows, col2 = col2)
  addDataFrame(x=interviewerData, sheet=statusInfo, row.names = F, col.names = F)
  statusCells = getCells(getRows(statusInfo), colIndex=1:2)
  
  # ADD STYLES
  setCellStyle(statusCells$`1.1`, headStyle)
  setCellStyle(statusCells$`14.1`, headStyle)
  CB.setBorder(CellBlock(statusInfo, 14,1,1,2,FALSE), headBorder, colIndex=1:2, rowIndex=1)
  setColumnWidth(statusInfo, 1, 30)
  setColumnWidth(statusInfo, 2, 30)
  
  createFreezePane(questionnaire, rowSplit = 1, colSplit = 2)
  
  #### end of creating sheet
  #########################
  #### ADD DATA ###########
  #########################
  
  case <- matched_cases[i,]
  print(case$`Nombre del fallecido:`)
  # if (i >= 32 & i <42) {case$id = case$id +1 }
  # if (i >= 42 & i <60) {case$id = case$id +2 }
  # if (i >= 60) {case$id = case$id +3 }
  
  
  # Case Info
  setCellValue(caseCells$`2.2`,case$id,showNA = F)
  
  # Questionnare Info
  
  # Intro
  setCellValue(qCells$`3.2`, paste(case$`Su nombre:`, case$`Sus apellidos:`, sep=' '), showNA = F)
  setCellValue(qCells$`4.2`, case$`Su teléfono:`, showNA = F)
  setCellValue(qCells$`5.2`, case$`Su correo electrónico:`, showNA = F)
  
  # Section 4
  setCellValue(qCells$`19.2`, case$`Nombre del fallecido:`, showNA = F)
  setCellValue(qCells$`21.2`, case$`Apellidos:`, showNA = F)
  # --relatoinship
  setCellValue(qCells$`24.2`, case$`Su relación con el fallecido es:`, showNA = F)
  #-- age
  setCellValue(qCells$`26.2`, case$`Edad del fallecido:`, showNA = F)
  
  #-- birth date
  # setCellValue(qCells$`28.2`, case$`Fecha de nacimiento del fallecido:`, showNA = F)
  
  #--residency place
  setCellValue(qCells$`29.2`, case$`Lugar de residencia:`, showNA=F)
  setCellValue(qCells$`30.2`, case$`La dirección física exacta del fallecido es:`, showNA = F)
  # --5 date of death
  setCellValue(qCells$`34.2`, case$DeathDate, showNA=F)
  # -- 5.1 municipality of death
  setCellValue(qCells$`35.2`, case$`Lugar del fallecimiento:`, showNA=F)
  # --7 where did the person die
  setCellValue(qCells$`37.2`, case$`¿Dónde murió la persona?`, showNA = F)
  #---8 cause of death
  setCellValue(qCells$`39.2`, case$`El fallecido murió a causa de:`, showNA = F)
  #---9 how is it related to maria
  setCellValue(qCells$`40.2`, case$`¿Puedes describir lo que sucedió y cómo se relaciona la muerte con María o la crisis causada por ese huracán?`, showNA = F)
  
  #### govt database fills
  
  setCellValue(caseCells$`3.2`,case$CertificateNumber,showNA = F)
  setCellValue(caseCells$`4.2`,case$DN,showNA = F)
  setCellValue(caseCells$`5.2`,case$TypeOfDeath,showNA = F)
  
  setCellValue(qCells$`3.3`, case$InformantName, showNA = F)
  setCellValue(qCells$`19.3`, case$Name, showNA = F)
  setCellValue(qCells$`20.3`, case$MiddleName, showNA = F)
  setCellValue(qCells$`21.3`, case$LastName, showNA = F)
  setCellValue(qCells$`22.3`, case$SecondLastName, showNA = F)
  setCellValue(qCells$`24.3`, case$InformantRelationship, showNA = F)
  setCellValue(qCells$`26.3`, case$Age, showNA = F)
  setCellValue(qCells$`28.3`, paste(case$BirthDate_Year, case$BirthDate_Month, case$BirthDate_Day,sep="-"), showNA = F)
  setCellValue(qCells$`29.3`, case$ResidencePlace, showNA=F)
  setCellValue(qCells$`30.3`, case$ResidencePlaceAddress1, showNA=F)
  setCellValue(qCells$`31.3`, case$ResidencePlaceAddress2, showNA=F)
  setCellValue(qCells$`32.3`, case$ResidencePlaceAddress3, showNA=F)
  setCellValue(qCells$`34.3`, paste(case$DeathDate_Year, case$DeathDate_Month, case$DeathDate_Day, sep='-'), showNA=F)
  setCellValue(qCells$`35.3`, case$MunicipalityDeathPlace, showNA=F)
  # -- 6.occupation
  setCellValue(qCells$`36.3`, tolower(paste(case$Industry, case$Occupation,sep="|")), showNA=F)  
  setCellValue(qCells$`37.3`, case$DeathPlace, showNA = F)
  # --7.1 facility name
  setCellValue(qCells$`38.3`, case$DeathFacility, showNA = F)
  setCellValue(qCells$`39.3`, case$nchsti, showNA = F)
  
  saveWorkbook(form, paste("bitacoras/Empty/bitacora_", case$id, '.xlsx',sep=""))
  
}

