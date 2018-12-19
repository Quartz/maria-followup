## merge it back into the database

interview <- readxl::read_excel('data/forProcess/for_translation_copyedits.xlsx')
basis <- readxl::read_excel('data/forProcess/rest_for_merge.xlsx')


interview %>%
  left_join(basis %>% select(-name, -causes)) %>%
  mutate(attr='form_interview') %>%
  mutate(month = month(date)) %>%
  mutate(text_field = text_field_en, causes = causes_en) %>%
  select(age, dmu, rmu, date, month, source, causes, c1,c2,c3,c4,c5,c6,attr,id,name,text_field) %>%
  write_csv(paste('data/forProcess/data_',
                  paste(month(Sys.Date()), day(Sys.Date()), year(Sys.Date()), sep='_'),
                  '_EN.csv',sep = ''))

interview %>%
  left_join(basis %>% select(-name, -causes)) %>%
  mutate(attr='form_interview') %>%
  mutate(month = month(date)) %>%
  mutate(text_field = text_field_es, causes = causes_es) %>%
  select(age, dmu, rmu, date, month, source, causes, c1,c2,c3,c4,c5,c6,attr,id,name,text_field) %>%
  write_csv(paste('data/forProcess/data_',
                  paste(month(Sys.Date()), day(Sys.Date()), year(Sys.Date()), sep='_'),
                  '_ES.csv',sep = ''))



## output the current index file with 3 columns:id, deathnumber, dn
existingCases = read_csv('data/case_index.csv')
newCases <- 
interview %>%
  left_join(basis %>% select(-name, -causes)) %>%
  mutate(DeathNumber = DN, DN = paste(DN, year(date),sep='_')) %>%
  select(id, DN, DeathNumber)# %>%
  # write_csv('data/case_index.csv')

existingCases %>%
  bind_rows(newCases) %>%
  write_csv('data/case_index.csv')

# ~~~ submit pull request

