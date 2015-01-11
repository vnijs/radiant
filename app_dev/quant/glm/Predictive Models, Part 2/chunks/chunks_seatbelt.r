brfss11 %>%
  group_by(State) %>%
  summarize(state.n=n(),
            prob.no=sum(SEATBELT %in% c('Seldom','Never'))/n() ) %>%
  arrange(desc(prob.no))



## @knitr logit.seat
seatbelt.logit.A <- glm(SEATBELT~Red_Blue + EDU + INCOME + AGE,
                        data=brfss11,
                        family=binomial(link="logit"))
seatbelt.logit.B <- glm(SEATBELT~State + EDU + INCOME + AGE,
                        data=brfss11,
                        family=binomial(link="logit"))
