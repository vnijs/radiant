load('data/brfs11.rda')


state.fips <- read.csv('H:/old/G/data/census/state_fips.csv',header=FALSE,col.names=c('Abr','X_STATE','State'))

brfss11 <- brfs11 %>%
  filter(SEATBELT %in% c(1,2,3,4,5), !is.na(Red_Blue), !X_EDUCAG=='NA', !X_EDUCAG==9, !INCOME2 %in% c(77,99),!INCOME2=='NA',!X_AGEG5YR==14) %>%
  mutate(SEATBELT=factor(SEATBELT,labels=c('Always','Nearly always', 'Sometimes','Seldom','Never')),
         INCOME=factor(INCOME2,labels=c('0-10K',
                                        '10-15K',
                                        '15-20K',
                                        '20-25K',
                                        '25-35K',
                                        '35-50K',
                                        '50-75K',
                                        '>75K')),
         AGE=factor(X_AGEG5YR,labels=c('18-24',
                                       '25-29',
                                       '30-34',
                                       '35-39',
                                       '40-44',
                                       '45-49',
                                       '50-54',
                                       '55-59',
                                       '60-64',
                                       '65-69',
                                       '70-74',
                                       '75-79',
                                       '80+')),
         EDU=factor(X_EDUCAG, labels=c('< HS', 'HS','Some College','College'))) %>%
  select(SEATBELT,EDU,INCOME,Red_Blue,AGE,X_STATE) %>%
  left_join(select(state.fips,X_STATE,State),by='X_STATE') %>%
  select(-X_STATE)



save(brfss11,file='data/brfss11_sub.rda')



