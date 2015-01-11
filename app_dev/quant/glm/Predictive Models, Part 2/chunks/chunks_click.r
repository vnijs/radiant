library(dplyr)

load('E:/data/web/click.rda')


clicks <- tbl_dt(click) %>%
  filter(hour %in% c(14100100,14100101,14100102,14100103,14100104),
         banner_pos %in% c(0,1),
         site_category %in% c('49d2d84e','688746e6','7103faa6','7e5068fc','a89b3dd2','d41d8cd9','db4fcb5b','e878bf21'),
         app_category %in% c('688746e6','7e5068fc','a89b3dd2','d41d8cd9','db4fcb5b'),
         device_os %in% c('2f3f71f2','990c0803','9e304d4e','c31b3236'),
         C19 %in% c(36,50,250),
         C21 %in% c(0,2,3),
         !C24 %in% c('108\r','134\r','20\r','24\r','47\r','52\r','70\r','72\r','82\r','91\r')) %>%
  select(id,click,hour,banner_pos,site_id,site_category,app_category,device_os,C18,C21,C24)


clicks <- clicks %>%
  mutate(hour=factor(hour),
         banner_pos=factor(banner_pos),
         site_category=factor(site_category),
         app_category=factor(app_category),
         C18=factor(C18),
         device_os=factor(device_os),
         C21=factor(C21),
         C24=factor(C24))


save(clicks,file='data/clicks.rda')



# no.click <- clicks %>%
#   filter(hour==14100100,click==0)
# 
# yes.click <- clicks %>%
#   filter(hour==14100100,click==1) 
# 
# click.data <- rbind(no.click,yes.click[sample.int(dim(yes.click)[1],0.01*dim(no.click)[1]),])
# 
# 
# for (the.hour in c(14100101,14100102,14100103,14100104)){
#   
#   no.click <- clicks %>%
#     filter(hour==the.hour,click==0)
#   
#   yes.click <- clicks %>%
#     filter(hour==the.hour,click==1) 
#   
#   click.data <- rbind(click.data,no.click,yes.click[sample.int(dim(yes.click)[1],0.01*dim(no.click)[1]),])
#     
# }

       
click.logit.A <- glm(click~hour + banner_pos + site_category + app_category + device_os + C18 + C21 + C24,
                        data=clicks,
                        family=binomial(link="logit"))



predict.data <- data.frame(hour=levels(clicks$hour)[1],
                           banner_pos=levels(clicks$banner_pos)[1],
                           #site_category=levels(clicks$site_category)[1],
                           site_category=levels(clicks$site_category),
                           device_os=levels(clicks$device_os)[1],
                           C18=levels(clicks$C18)[1],
                           C21=levels(clicks$C21)[1],
                           C24=levels(clicks$C24)[1],
                           app_category=levels(clicks$app_category)[1])
                           #app_category=levels(clicks$app_category))


predict(click.logit.A, newdata = predict.data, type = "response")

