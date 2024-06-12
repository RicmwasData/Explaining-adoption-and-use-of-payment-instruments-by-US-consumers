## ----setup, include=FALSE-------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- message=FALSE,warning=FALSE-----------------------------------------------------------
library(tidyverse)
library(knitr)
library(corrplot)
#library(kableExtra)


## -------------------------------------------------------------------------------------------
space<- read_csv("SPACE_wide_2022.csv")
space[space==999998] <- NA


## -------------------------------------------------------------------------------------------
df<- space %>%
  select(AGE, D1, EDUCATION, D6_1, D6_2A, D6_2B, HHSIZE, DEPCHILDREN, INCOME,COUNTRY,
         QQ1A_1,,QQ1A_2,QQ1A_3,QQ1D,
         starts_with(c("QA5A_", "QA6A_","QA7A_","QA7AI_", "QA7AII_","QA8A_", "QA8AI_1")))%>%
  mutate(AGE=case_when(AGE==1~"18-24",
                       AGE==2~"25-29",
                       AGE==3~"30-34",
                       AGE==4~"35-39",
                       AGE==5~"40-44",
                       AGE==6~"45-49",
                       AGE==7~"50-54",
                       AGE==8~"55-59",
                       AGE==9~"60-64",
                       AGE==10~"64-69",
                       AGE==11~"70-74",
                       AGE==12~"75+"),
         D1=case_when(D1==1~'Male',
                      D1==2~"Female",
                      D1==3~"Other, non-binary"),
         EDUCATION= case_when(EDUCATION==1~"Primary/lower secondary education",
                              EDUCATION==2~"Upper/post-secondary education",
                              EDUCATION==3 ~"University/PhD/research",
                              EDUCATION=='=999998'~"Refusal / No answer"),
         D6_1= case_when(D6_1==1~"Self-employed",
                         D6_1==2~"Employee",
                         D6_1==3~"Without a professional activity or student",
                         D6_1=="999998"~"Refusal / No answer"),
         D6_2A= case_when(D6_2A==1~"General management, director or top management",
                          D6_2A==2~"Middle management or other management (e.g. department head)",
                          D6_2A==3~"Employed professional (e.g. doctor, lawyer, accountant, architect)",
                          D6_2A==4~"Employed position, working mainly at a desk",
                          D6_2A==5~"Employed position, not at a desk",
                          D6_2A==6~"Manual worker",
                          D6_2A==7~"Other employee",
                          D6_2A==999998~"Refusal / No answer"),
         D6_2B= case_when(D6_2B==1~	"Responsible for housework (e.g. ordinary shopping, looking after home and family)",
                          D6_2B==2~	"Student (full-time)",
                          D6_2B==3~	"Unemployed or temporarily not working",
                          D6_2B==4~	"Retired or unable to work through illness",
                          D6_2B==5~	"Other",
                          D6_2B==999998~	"Refusal / No answer"),
         INCOME= case_when(INCOME==1~	"EUR 750 or less",
                           INCOME==2~	"Between EUR 751 and EUR 1,500",
                           INCOME==3~	"Between EUR 1,501 and EUR 2,500",
                           INCOME==4~"Between EUR 2,501 and EUR 4,000",
                           INCOME==5~	"More than EUR 4,000",
                           INCOME==999998~	"Refusal")
         
  )


## -------------------------------------------------------------------------------------------
### correlation ####
df_cor<- space %>%
  select(AGE, D1, EDUCATION, D6_1, D6_2A, D6_2B, HHSIZE, DEPCHILDREN, INCOME)

df_cor[df_cor>100]<- NA

cor_mat<-cor(df_cor,use='pairwise.complete.obs')


## ----fig.cap ="Correlation Matrix",fig.width=12, fig.height=8-------------------------------
corrplot(cor_mat, method = 'number',col = COL2('RdYlBu', 2),
         type = 'lower', tl.col='black')


## -------------------------------------------------------------------------------------------

ggplot(df, aes(AGE)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), fill="blue", color="black") + 
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", title = "Age Distribution in %")+
  theme_classic()

#The plot shows that most of the people who took part in the
# survey falls between 64-69 years of age. The group with over 75 years had the least people



## -------------------------------------------------------------------------------------------
# Gender
ggplot(df, aes(D1)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="blue", color="black") + 
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="day") +
  labs(y = "Percent", title = "Gender Distribution in %")+
  theme_classic()
#Females are slightly higher than males in the survey. 



## -------------------------------------------------------------------------------------------
# Education
ggplot(df, aes(EDUCATION, fill=EDUCATION)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", title = "Education Distribution in %")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))+
  theme(
    # axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
# most the people who took part in the survey have attained secondary education.



## -------------------------------------------------------------------------------------------
#Income 
ggplot(df, aes(INCOME, fill= INCOME)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", title = "Income Distribution in %")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))+
  theme(
    # axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

#Majority of the people earn between 1510 and 2500 euros



## -------------------------------------------------------------------------------------------
#HHSIZE
ggplot(df, aes(factor(HHSIZE))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="blue", color="black") + 
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", title = "Household size",
       x="Household size")+
  theme_classic()

#majority of the households have atmost 2 people per household.


## -------------------------------------------------------------------------------------------
#Activity status
ggplot(df, aes(factor(D6_1), fill=factor(D6_1))) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", title = "Activity status",
       x="Status",fill="Activity")+
  theme_classic()+
  theme(
    # axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

#Almost 50% of the people are employees. 


## -------------------------------------------------------------------------------------------
#DEPCHILDREN
ggplot(df, aes(factor(DEPCHILDREN))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="blue", color="black") + 
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", title = "Number of children or economically dependent",
       x="DEPCHILDREN")+
  theme_classic()

#Over 40% of the participants (majority) do not have dependents.


## -------------------------------------------------------------------------------------------
#Country
ggplot(df, aes(COUNTRY)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="blue", color="black") + 
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", title = "Country Distribution in %")+
  theme_classic()

#Most of the participants are from france followed by italy and spain comes in at third position



## -------------------------------------------------------------------------------------------
POS<- space%>%
  select(QQ1A_1,,QQ1A_2,QQ1A_3,QQ1D)%>%
  mutate(QQ1D= case_when(
    QQ1D==1~	"Yes, it is available for me",
    QQ1D==2~	"No, it is not available for me",
    QQ1D==3~	"heard aboutservice, not sure if available",
    QQ1D==4~	"I have not heard about this service",
    QQ1D==999999~	"Don't know"
    
  ))

Bundles<-count(POS,QQ1A_1,QQ1A_2, QQ1A_3,QQ1D ) %>% ungroup()%>%
  mutate(n= round((n/sum(n))*100,2))%>%
  arrange(desc(n))%>%
  mutate(n= paste0(n," %"))%>%
  head(15)

names(Bundles)<- c("account","crdit.dbit",
                   "crypto", "instnt",
                   "Pop")
kable(Bundles, align = "l")



## -------------------------------------------------------------------------------------------
#### Age

POS<- df%>%
  select(QQ1A_1,,QQ1A_2,QQ1A_3,QQ1D, AGE)%>%
  mutate(QQ1D= case_when(
    QQ1D==1~	"Yes, it is available for me",
    QQ1D==2~	"No, it is not available for me",
    QQ1D==3~	"heard aboutservice, not sure if available",
    QQ1D==4~	"I have not heard about this service",
    QQ1D==999999~	"Don't know"
    
  ))

Bundles<-count(POS,QQ1A_1,QQ1A_2, QQ1A_3,QQ1D ,AGE) %>% ungroup()%>%
  mutate(n= round((n/sum(n))*100,2))%>%
  arrange(desc(n))%>%
  mutate(n= paste0(n," %"))%>%
  head(15)

names(Bundles)<- c("account","crdit.dbit",
                   "crypto", "instnt","AGE",
                   "Pop")

kable(Bundles, align = "l")



## -------------------------------------------------------------------------------------------
#Education 
POS<- df%>%
  select(QQ1A_1,,QQ1A_2,QQ1A_3,QQ1D, EDUCATION)%>%
  mutate(QQ1D= case_when(
    QQ1D==1~	"Yes, it is available for me",
    QQ1D==2~	"No, it is not available for me",
    QQ1D==3~	"heard aboutservice, not sure if available",
    QQ1D==4~	"I have not heard about this service",
    QQ1D==999999~	"Don't know"

  ))

Bundles<-count(POS,QQ1A_1,QQ1A_2, QQ1A_3,QQ1D ,EDUCATION) %>% ungroup()%>%
  mutate(n= round((n/sum(n))*100,2))%>%
  arrange(desc(n))%>%
  mutate(n= paste0(n," %"))%>%
  head(15)

names(Bundles)<- c("account","crdit.dbit",
                   "crypto", "instnt","EDUCATION",
                   "Pop")
kable(Bundles,align = "l")


## -------------------------------------------------------------------------------------------
## Activity Status
POS<- df%>%
  select(QQ1A_1,,QQ1A_2,QQ1A_3,QQ1D, D6_1)%>%
  mutate(QQ1D= case_when(
    QQ1D==1~	"Yes, it is available for me",
    QQ1D==2~	"No, it is not available for me",
    QQ1D==3~	"heard aboutservice, not sure if available",
    QQ1D==4~	"I have not heard about this service",
    QQ1D==999999~	"Don't know"
    
  ))

Bundles<-count(POS,QQ1A_1,QQ1A_2, QQ1A_3,QQ1D ,D6_1) %>% ungroup()%>%
  mutate(n= round((n/sum(n))*100,2))%>%
  arrange(desc(n))%>%
  mutate(n= paste0(n," %"))%>%
  head(15)

names(Bundles)<- c("account","crdit.dbit",
                   "crypto", "instnt","Activity",
                   "Pop")
kable(Bundles, align = "l")


## -------------------------------------------------------------------------------------------

## Employment status
POS<- df%>%
  select(QQ1A_1,,QQ1A_2,QQ1A_3,QQ1D, D6_2A)%>%
  mutate(QQ1D= case_when(
    QQ1D==1~	"Yes, it is available for me",
    QQ1D==2~	"No, it is not available for me",
    QQ1D==3~	"heard aboutservice, not sure if available",
    QQ1D==4~	"I have not heard about this service",
    QQ1D==999999~	"Don't know"
    
  ))

Bundles<-count(POS,QQ1A_1,QQ1A_2, QQ1A_3,QQ1D ,D6_2A) %>% ungroup()%>%
  mutate(n= round((n/sum(n))*100,2))%>%
  arrange(desc(n))%>%
  mutate(n= paste0(n," %"))%>%
  head(15)

names(Bundles)<- c("account","crdit.dbit",
                   "crypto", "instnt","Employment_Status",
                   "Pop")
kable(Bundles, align = "l")



## -------------------------------------------------------------------------------------------
###  Inactivity status
POS<- df%>%
  select(QQ1A_1,,QQ1A_2,QQ1A_3,QQ1D, D6_2B)%>%
  mutate(QQ1D= case_when(
    QQ1D==1~	"Yes, it is available for me",
    QQ1D==2~	"No, it is not available for me",
    QQ1D==3~	"heard aboutservice, not sure if available",
    QQ1D==4~	"I have not heard about this service",
    QQ1D==999999~	"Don't know"
    
  ))

Bundles<-count(POS,QQ1A_1,QQ1A_2, QQ1A_3,QQ1D ,D6_2B) %>% ungroup()%>%
  mutate(n= round((n/sum(n))*100,2))%>%
  arrange(desc(n))%>%
  mutate(n= paste0(n," %"))%>%
  head(15)

names(Bundles)<- c("account","crdit.dbit",
                   "crypto", "instnt","Inactivity_Status",
                   "Pop")
kable(Bundles, align = "l")



## -------------------------------------------------------------------------------------------
###  Household size
POS<- df%>%
  select(QQ1A_1,,QQ1A_2,QQ1A_3,QQ1D, HHSIZE)%>%
  mutate(QQ1D= case_when(
    QQ1D==1~	"Yes, it is available for me",
    QQ1D==2~	"No, it is not available for me",
    QQ1D==3~	"heard aboutservice, not sure if available",
    QQ1D==4~	"I have not heard about this service",
    QQ1D==999999~	"Don't know"
    
  ))

Bundles<-count(POS,QQ1A_1,QQ1A_2, QQ1A_3,QQ1D ,HHSIZE) %>% ungroup()%>%
  mutate(n= round((n/sum(n))*100,2))%>%
  arrange(desc(n))%>%
  mutate(n= paste0(n," %"))%>%
  head(15)

names(Bundles)<- c("account","crdit.dbit",
                   "crypto", "instnt","HHSIZE",
                   "Pop")
kable(Bundles, align = "l")


## -------------------------------------------------------------------------------------------
### INCOME
POS<- df%>%
  select(QQ1A_1,,QQ1A_2,QQ1A_3,QQ1D, INCOME)%>%
  mutate(QQ1D= case_when(
    QQ1D==1~	"Yes, it is available for me",
    QQ1D==2~	"No, it is not available for me",
    QQ1D==3~	"heard aboutservice, not sure if available",
    QQ1D==4~	"I have not heard about this service",
    QQ1D==999999~	"Don't know"
    
  ))

Bundles<-count(POS,QQ1A_1,QQ1A_2, QQ1A_3,QQ1D ,INCOME) %>% ungroup()%>%
  mutate(n= round((n/sum(n))*100,2))%>%
  arrange(desc(n))%>%
  mutate(n= paste0(n," %"))%>%
  head(15)

names(Bundles)<- c("account","crdit.dbit",
                   "crypto", "instnt","INCOME",
                   "Pop")
kable(Bundles, align = 'l')



## -------------------------------------------------------------------------------------------
### country 

POS<- df%>%
  select(QQ1A_1,,QQ1A_2,QQ1A_3,QQ1D, COUNTRY)

for(country in unique(POS$COUNTRY)){
  dd<- POS%>%
    filter(COUNTRY %in% country)
  
  Bundles2<-count(dd,QQ1A_1,QQ1A_2, QQ1A_3,QQ1D ,COUNTRY) %>% ungroup()%>%
    mutate(n= round((n/sum(n))*100,2))%>%
    arrange(desc(n))%>%
    mutate(n= paste0(n," %"))%>%
    head(15)
  
  names(Bundles2)<- c("account","crdit.dbit",
                     "crypto", "instnt","COUNTRY",
                     "Pop")
  print(kable(Bundles2, align = 'l', caption = paste0(country,"'s Population Holdings of the Top 15 Bundles of Payment Instruments")))
  
}



## -------------------------------------------------------------------------------------------
# online payment

online<- space%>%
  select(starts_with(c("QB1_","QB3_", "QB4_")))#,QB3_n,QB4_n)
online[online>100 & !is.na(online)]<- NA

df_1<- online%>%
  select(ends_with("_1"))%>%
  mutate(QB1_1= case_when(
    QB1_1==1~	"Clothes_sportswear",
    QB1_1==2~	"Electronic_goods",
    QB1_1==3~	"Food_daily.supplies",
    QB1_1==4~	"Medicine",
    QB1_1==5~	"entertainment",
    QB1_1==6~	"Donations",
    QB1_1==7~	"Accommodation",
    QB1_1==8~	"Furniture",
    QB1_1==9~	"Tickets",
    QB1_1==10~	"Luxury_goods",
    QB1_1==11~	"Financial_products",
    QB1_1==12~	"Household_related",
    QB1_1==13~	"Other",
    QB1_1==999999~	"Don't know"

  ),
  QB4_1= case_when(
    QB4_1==1~"Card",
    QB4_1==3~"PayPal",
    QB4_1==4~	"Other.online",
    QB4_1==5~	"Direct",
    QB4_1==6~	"Credit",
    QB4_1==8~	"Loyalty",
    QB4_1==10~	"Crypto-assets",
    QB4_1==11~	"Other",
    QB4_1==999999~	"Don't know"

  ))

df_1.clean<- df_1%>%
  drop_na()

grouped_1<-df_1.clean%>%
  group_by(QB4_1, QB1_1)%>%
  summarise(mean= mean(QB3_1, na.rm = T))

dd<-pivot_wider(grouped_1, names_from = QB4_1, 
            values_from = mean)
kable(dd, digits = 2,  align = "l")


## ----message=FALSE--------------------------------------------------------------------------
#compare by demographic
df_3<- data.frame(df_1, AGE= df$AGE)%>%
  drop_na()

for(age in unique(df_3$AGE)){
  df_3.sub<- df_3%>%
    filter(AGE== age)
  grouped_3<-df_3.sub%>%
    group_by(QB4_1, QB1_1)%>%
    summarise(mean= mean(QB3_1, na.rm = T),)
  
  dd_3<-pivot_wider(grouped_3, names_from = QB4_1, 
                    values_from = mean)
  print(kable(dd_3, digits = 2, align = "l", 
              caption=paste0(age, "'s online Payment")))
  
}


## ---- message=FALSE-------------------------------------------------------------------------
#compare by country 

df_2<- data.frame(df_1, Country= space$COUNTRY)%>%
  drop_na()



for( country in unique(df_2$Country)){
  df_2.sub<- df_2%>%
    filter(Country == country)
  grouped_2<-df_2.sub%>%
    group_by(QB4_1, QB1_1)%>%
    summarise(mean= mean(QB3_1, na.rm = T))
  
  dd_2<-pivot_wider(grouped_2, names_from = QB4_1, 
                  values_from = mean)
  print(kable(dd_2,digits = 2,
              align = "l",
              caption=paste0(country, "'s online Payment")))
}


## -------------------------------------------------------------------------------------------
#One chart for adoption Rate 

adoption<- space%>%
  select(QQ1A_1,QQ1A_2, QQ1A_3, QQ1A_4, QQ1A_5)
names(adoption)<- c("Cash", "Credit.debit","Crypto","None","Not.Sure")

ratios <- apply(data.matrix(adoption)[,], 2, function(x) length(which(x == 1)) / nrow(adoption))

ff<- data.frame(#instruments=c("Cash", "Credit.debit","Crypto","None","Not.Sure"),
                Adoption.Rate= ratios)
kable(ff, align = "l", caption = "Adoption Rate")


## -------------------------------------------------------------------------------------------
# by country 
adoption<- space%>%
  select(QQ1A_1,QQ1A_2, QQ1A_3, QQ1A_4, QQ1A_5, COUNTRY)

for( country in unique(adoption$COUNTRY)){
  dat<- adoption%>%
    filter(COUNTRY== country)%>%
    select(-COUNTRY)
  names(dat)<- c("Payment.account", "Credit.debit","Crypto","None","Not.Sure")
  ratios <- apply(data.matrix(dat)[,], 2, function(x) length(which(x == 1)) / nrow(dat))
  
  ff<- data.frame(#instruments=c("Cash", "Credit.debit","Crypto","None","Not.Sure"),
    Adoption.Rate= ratios)
  print(kable(ff, align = "l", caption=paste0(country, "'s Adoption Rate")))
  
}


## -------------------------------------------------------------------------------------------
# by country 
adoption<- space%>%
  select(QQ1A_1,QQ1A_2, QQ1A_3, QQ1A_4, QQ1A_5)
adoption$AGE<- df$AGE

for(age in unique(adoption$AGE)){
  dat<- adoption%>%
    filter(AGE== age)%>%
    select(-AGE)
  names(dat)<- c("Payment.account", "Credit.debit","Crypto","None","Not.Sure")
  ratios <- apply(data.matrix(dat)[,], 2, function(x) length(which(x == 1)) / nrow(dat))
  
  ff<- data.frame(#instruments=c("Cash", "Credit.debit","Crypto","None","Not.Sure"),
    Adoption.Rate= ratios)
  print(kable(ff, align = "l", caption=paste0(age, "'s Adoption Rate")))
  
}


## -------------------------------------------------------------------------------------------
# by country 
adoption<- space%>%
  select(QQ1A_1,QQ1A_2, QQ1A_3, QQ1A_4, QQ1A_5)
adoption$D1<- df$D1

for(d1 in unique(adoption$D1)){
  dat<- adoption%>%
    filter(D1== d1)%>%
    select(-D1)%>%
    drop_na()
  names(dat)<- c("Payment.account", "Credit.debit","Crypto","None","Not.Sure")
  ratios <- apply(data.matrix(dat)[,], 2, function(x) length(which(x == 1)) / nrow(dat))
  
  ff<- data.frame(Adoption.Rate= ratios)
  print(kable(ff, align = "l", caption=paste0(d1, "'s Adoption Rate")))
  
}


## -------------------------------------------------------------------------------------------
# by country 
adoption<- space%>%
  select(QQ1A_1,QQ1A_2, QQ1A_3, QQ1A_4, QQ1A_5)
adoption$D6_1<- df$D6_1

for(d6_1 in unique(adoption$D6_1)){
  dat<- adoption%>%
    filter(D6_1== d6_1)%>%
    select(-D6_1)%>%
    drop_na()
  names(dat)<- c("Payment.account", "Credit.debit","Crypto","None","Not.Sure")
  ratios <- apply(data.matrix(dat)[,], 2, function(x) length(which(x == 1)) / nrow(dat))
  
  ff<- data.frame(Adoption.Rate= ratios)
  print(kable(ff, align = "l", caption=paste0(d6_1, "'s Adoption Rate")))
  
}


## -------------------------------------------------------------------------------------------
# by country 
adoption<- space%>%
  select(QQ1A_1,QQ1A_2, QQ1A_3, QQ1A_4, QQ1A_5)
adoption$EDUCATION<- df$EDUCATION

for(edu in unique(adoption$EDUCATION)){
  dat<- adoption%>%
    filter(EDUCATION== edu)%>%
    select(-EDUCATION)
  names(dat)<- c("Payment.account", "Credit.debit","Crypto","None","Not.Sure")
  ratios <- apply(data.matrix(dat)[,], 2, function(x) length(which(x == 1)) / nrow(dat))
  
  ff<- data.frame(#instruments=c("Cash", "Credit.debit","Crypto","None","Not.Sure"),
    Adoption.Rate= ratios)
  print(kable(ff, align = "l", caption=paste0(edu, "'s Adoption Rate")))
  
}


## -------------------------------------------------------------------------------------------
# by country 
adoption<- space%>%
  select(QQ1A_1,QQ1A_2, QQ1A_3, QQ1A_4, QQ1A_5)
adoption$INCOME<- df$INCOME

for(income in unique(adoption$INCOME)){
  dat<- adoption%>%
    filter(INCOME== income)%>%
    select(-INCOME)
  names(dat)<- c("Payment.account", "Credit.debit","Crypto","None","Not.Sure")
  ratios <- apply(data.matrix(dat)[,], 2, function(x) length(which(x == 1)) / nrow(dat))
  
  ff<- data.frame(#instruments=c("Cash", "Credit.debit","Crypto","None","Not.Sure"),
    Adoption.Rate= ratios)
  print(kable(ff, align = "l", caption=paste0(income, "'s Adoption Rate")))
  
}


## -------------------------------------------------------------------------------------------
# by country 
adoption<- space%>%
  select(QQ1A_1,QQ1A_2, QQ1A_3, QQ1A_4, QQ1A_5)
adoption$HHSIZE<- df$HHSIZE

for(hhsize in unique(adoption$HHSIZE)){
  dat<- adoption%>%
    filter(HHSIZE== hhsize)%>%
    select(-HHSIZE)
  names(dat)<- c("Payment.account", "Credit.debit","Crypto","None","Not.Sure")
  ratios <- apply(data.matrix(dat)[,], 2, function(x) length(which(x == 1)) / nrow(dat))
  
  ff<- data.frame(#instruments=c("Cash", "Credit.debit","Crypto","None","Not.Sure"),
    Adoption.Rate= ratios)
  print(kable(ff, align = "l", caption=paste0(hhsize, "'s Adoption Rate")))
  
}


## -------------------------------------------------------------------------------------------
#cash advantage
cash.advg<- space%>%
  select(QQ13A_1,QQ13A_2,QQ13A_3,QQ13A_4,QQ13A_5,
         QQ13A_6, QQ13A_7, QQ13A_8,QQ13A_9,QQ13A_10,QQ13A_11)
names(cash.advg)<- c("cash.acceptance", "cash.faster","cash.privacy","cash.easier",
                     "cash.safer","immediately_settled","aware_spending",
                     "other_advantage","do_not_use_cash","no_advantage",
                     "dont_know")
ratios <- apply(data.matrix(cash.advg)[,], 2, function(x) length(which(x == 1)) / nrow(cash.advg))

cash.advg.rate<- data.frame(#instruments=c("Cash", "Credit.debit","Crypto","None","Not.Sure"),
  Advantage.Rating= ratios)
kable(cash.advg.rate, align = "l", caption = "Cash advatange ratio")


## -------------------------------------------------------------------------------------------
cash.advg.age<- data.frame(cash.advg, AGE= df$AGE)

for(age in unique(cash.advg.age$AGE)){
  dd<- cash.advg.age%>%
    filter(AGE==age)%>%
    drop_na()
  
  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))
  
  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(age, "'s Cash advatange ratio")))
}


## -------------------------------------------------------------------------------------------
cash.advg.D1<- data.frame(cash.advg, D1= df$D1)%>%
  drop_na()

for(d1 in unique(cash.advg.D1$D1)){
  dd<- cash.advg.D1%>%
    filter(D1==d1)%>%
    drop_na()
  
  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))
  
  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(d1, "'s Cash advatange ratio")))
  
}


## -------------------------------------------------------------------------------------------
cash.advg.D6_1<- data.frame(cash.advg, D6_1= df$D6_1)%>%
  drop_na()

for(d6_1 in unique(cash.advg.D6_1$D6_1)){
  dd<- cash.advg.D6_1%>%
    filter(D6_1==d6_1)%>%
    select(-D6_1)%>%
    drop_na()
  
  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))
  
  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(d6_1, "'s Cash advatange ratio")))
  
}


## -------------------------------------------------------------------------------------------
cash.advg.edu<- data.frame(cash.advg, EDU= df$EDUCATION)

for(edu in unique(cash.advg.edu$EDU)){
  dd<- cash.advg.edu%>%
    filter(EDU==edu)%>%
    drop_na()
  
  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))
  
  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(edu, "'s Cash advatange ratio")))
}


## -------------------------------------------------------------------------------------------
cash.advg.income<- data.frame(cash.advg, INCOME= df$INCOME)%>%
  drop_na()

for(income in unique(cash.advg.income$INCOME)){
  dd<- cash.advg.income%>%
    filter(INCOME==income)%>%
    drop_na()
  
  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))
  
  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(income, "'s Cash advatange ratio")))
  
}


## -------------------------------------------------------------------------------------------
cash.advg.hhsize<- data.frame(cash.advg, HHSIZE= df$HHSIZE)

for(hhsize in unique(cash.advg.hhsize$HHSIZE)){
  dd<- cash.advg.hhsize%>%
    filter(HHSIZE==hhsize)%>%
    drop_na()
  
  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))
  
  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(hhsize, "'s Cash advatange ratio")))
  
}


## -------------------------------------------------------------------------------------------
#card advantages

card.advg<- space%>%
select(QQ13B_1,QQ13B_2,QQ13B_3,QQ13B_4,QQ13B_5,
       QQ13B_6, QQ13B_7, QQ13B_8,QQ13B_9,QQ13B_10)
names(card.advg)<- c("card.acceptance", "card.faster","card.easier",
                "card.safer","no_worry_carrying_cash","aware_spending",
                "other_advantage","no_advantage","do_not_use_card",
                "dont_know")

ratios <- apply(data.matrix(card.advg)[,], 2, function(x) length(which(x == 1)) / nrow(card.advg))

card.advg.rate<- data.frame(#instruments=c("Cash", "Credit.debit","Crypto","None","Not.Sure"),
  Advantage.Rating= ratios)

kable(card.advg.rate, align = "l", caption = "Card advatange ratio")


## -------------------------------------------------------------------------------------------
card.advg.age<- data.frame(card.advg, AGE= df$AGE)

for(age in unique(card.advg.age$AGE)){
  dd<- card.advg.age%>%
    filter(AGE==age)%>%
    drop_na()

  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))

  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(age, "'s Card advatange ratio")))
}


## -------------------------------------------------------------------------------------------
card.advg.D1<- data.frame(card.advg, D1= df$D1)%>%
  drop_na()

for(d1 in unique(card.advg.D1$D1)){
  dd<- card.advg.D1%>%
    filter(D1==d1)%>%
    drop_na()
  
  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))
  
  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(d1, "'s Card advatange ratio")))
  
}


## -------------------------------------------------------------------------------------------
card.advg.D6_1<- data.frame(card.advg, D6_1= df$D6_1)%>%
  drop_na()

for(d6_1 in unique(card.advg.D6_1$D6_1)){
  dd<- card.advg.D6_1%>%
    filter(D6_1==d6_1)%>%
    drop_na()
  
  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))
  
  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(d6_1, "'s Card advatange ratio")))
  
}


## -------------------------------------------------------------------------------------------
card.advg.edu<- data.frame(card.advg, EDU= df$EDUCATION)

for(edu in unique(card.advg.edu$EDU)){
  dd<- card.advg.edu%>%
    filter(EDU==edu)%>%
    drop_na()
  
  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))
  
  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(edu, "'s Card advatange ratio")))
}


## -------------------------------------------------------------------------------------------
card.advg.income<- data.frame(card.advg, INCOME= df$INCOME)%>%
  drop_na()

for(income in unique(card.advg.income$INCOME)){
  dd<- card.advg.income%>%
    filter(INCOME==income)%>%
    drop_na()
  
  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))
  
  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(income, "'s Card advatange ratio")))
  
}


## -------------------------------------------------------------------------------------------
card.advg.hhsize<- data.frame(card.advg, HHSIZE= df$HHSIZE)

for(hhsize in unique(card.advg.hhsize$HHSIZE)){
  dd<- card.advg.hhsize%>%
    filter(HHSIZE==hhsize)%>%
    drop_na()
  
  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))
  
  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(hhsize, "'s Card advatange ratio")))
  
}


## -------------------------------------------------------------------------------------------
Covid<- space%>%
  select(QQ19A)%>%
  mutate(QQ19A=case_when(
    QQ19A==1~	"Much more often",
    QQ19A==2~	"Somewhat more often",
    QQ19A==3~	"The same as before",
    QQ19A==4~	"Somewhat less often",
    QQ19A==5~	"Much less often",
    QQ19A==999999~	"Don't know",

  ))

pp<-prop.table(table(Covid$QQ19A))

covid_dist<- data.frame( Use.of.cash= pp)

kable(covid_dist, align = 'l', caption = "are you using cash instead of non-cash payment methods more")


## -------------------------------------------------------------------------------------------
Covid.age<- data.frame(Covid, AGE= df$AGE)

for(age in unique(Covid.age$AGE)){
  dd<- Covid.age%>%
    filter(AGE==age)%>%
    select(-AGE)%>%
    drop_na()
  
  pp<-prop.table(table(dd$QQ19A))

  covid_dist<- data.frame( Use.of.cash= pp)

  print(kable(covid_dist, align = "l", caption =paste0(age, "'s cash freq ratio")))
}


## -------------------------------------------------------------------------------------------
Covid.d1<- data.frame(Covid, D1= df$D1)

for(d1 in unique(Covid.d1$D1)){
  dd<- Covid.d1%>%
    filter(D1==d1)%>%
    select(-D1)%>%
    drop_na()
  
  pp<-prop.table(table(dd$QQ19A))

  covid_dist<- data.frame( Use.of.cash= pp)

  print(kable(covid_dist, align = "l", caption =paste0(d1, "'s cash freq ratio")))
}


## -------------------------------------------------------------------------------------------
Covid.D6_1<- data.frame(Covid, D6_1= df$D6_1)

for(d6_1 in unique(Covid.D6_1$D6_1)){
  dd<- Covid.D6_1%>%
    filter(D6_1==d6_1)%>%
    select(-D6_1)%>%
    drop_na()
  
  pp<-prop.table(table(dd$QQ19A))

  covid_dist<- data.frame( Use.of.cash= pp)

  print(kable(covid_dist, align = "l", caption =paste0(d6_1, "'s cash freq ratio")))
}


## -------------------------------------------------------------------------------------------
Covid.edu<- data.frame(Covid, EDUCATION= df$EDUCATION)

for(edu in unique(Covid.edu$EDUCATION)){
  dd<- Covid.edu%>%
    filter(EDUCATION==edu)%>%
    drop_na()
  
  pp<-prop.table(table(dd$QQ19A))

  covid_dist<- data.frame( Use.of.cash= pp)

  print(kable(covid_dist, align = "l", caption =paste0(edu, "'s cash freq ratio")))
}


## -------------------------------------------------------------------------------------------
Covid.hhsize<- data.frame(Covid, HHSIZE= df$HHSIZE)

for(hhsize in unique(Covid.hhsize$HHSIZE)){
  dd<- Covid.hhsize%>%
    filter(HHSIZE==hhsize)%>%
    drop_na()
  
  pp<-prop.table(table(dd$QQ19A))

  covid_dist<- data.frame( Use.of.cash= pp)

  print(kable(covid_dist, align = "l", caption =paste0(hhsize, "'s cash freq ratio")))
}


## -------------------------------------------------------------------------------------------
Covid.income<- data.frame(Covid, INCOME= df$INCOME)

for(income in unique(Covid.income$INCOME)){
  dd<- Covid.income%>%
    filter(INCOME==income)%>%
    drop_na()
  
  pp<-prop.table(table(dd$QQ19A))

  covid_dist<- data.frame( Use.of.cash= pp)

  print(kable(covid_dist, align = "l", caption =paste0(income, "'s cash freq ratio")))
}


## -------------------------------------------------------------------------------------------
#Less cash 
less.cash<- space%>%
  select(starts_with("Q19C_"))
names(less.cash)<- c('difficult.withdraw','fear.virus',
                     'cash.not.accepted','advice.not.use.cash',
                     'gover.recomm', 'electronic.better',
                     'new.means','other.reason',
                     'dont.know')
less.cash2<- less.cash
less.cash<- na.omit(less.cash)
ratios <- apply(data.matrix(less.cash)[,], 2, function(x) length(which(x == 1)) / nrow(less.cash))

df.less<- data.frame( Advantage.Rating= ratios)
kable(df.less, align = "l", caption =paste0("Less Cash", ""))


## -------------------------------------------------------------------------------------------
Covid.age<- data.frame(less.cash2, AGE= df$AGE)

for(age in unique(Covid.age$AGE)){
  dd<- Covid.age%>%
    filter(AGE==age)%>%
    select(-AGE)%>%
    drop_na()
  
ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))

df.less<- data.frame( Advantage.Rating= ratios)
print(kable(df.less, align = "l", caption =paste0(age, "'s less cash freq ratio")))
}


## -------------------------------------------------------------------------------------------
Covid.income<- data.frame(less.cash2, INCOME= df$INCOME)

for(income in unique(Covid.income$INCOME)){
  dd<- Covid.income%>%
    filter(INCOME==income)%>%
    select(-INCOME)%>%
    drop_na()
  
ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))

df.less<- data.frame( Advantage.Rating= ratios)
print(kable(df.less, align = "l", caption =paste0(income, "'s less cash freq ratio")))
}


## -------------------------------------------------------------------------------------------
Covid.d1<- data.frame(less.cash2, D1= df$D1)

for(d1 in unique(Covid.d1$D1)){
  dd<- Covid.d1%>%
    filter(D1==d1)%>%
    select(-D1)%>%
    drop_na()
  
ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))

df.less<- data.frame( Advantage.Rating= ratios)
print(kable(df.less, align = "l", caption =paste0(d1, "'s less cash freq ratio")))
}


## -------------------------------------------------------------------------------------------
Covid.edu<- data.frame(less.cash2, EDUCATION= df$EDUCATION)

for(edu in unique(Covid.edu$EDUCATION)){
  dd<- Covid.edu%>%
    filter(EDUCATION==edu)%>%
    select(-EDUCATION)%>%
    drop_na()
  
ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))

df.less<- data.frame( Advantage.Rating= ratios)
print(kable(df.less, align = "l", caption =paste0(edu, "'s less cash freq ratio")))
}


## -------------------------------------------------------------------------------------------
Covid.d6_1<- data.frame(less.cash2, D6_1= df$D6_1)

for(d6_1 in unique(Covid.edu$EDUCATION)){
  dd<- Covid.d6_1%>%
    filter(D6_1==d6_1)%>%
    select(-D6_1)%>%
    drop_na()
  
ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))

df.less<- data.frame( Advantage.Rating= ratios)
print(kable(df.less, align = "l", caption =paste0(d6_1, "'s less cash freq ratio")))
}


## -------------------------------------------------------------------------------------------
Covid.hhsize<- data.frame(less.cash2, HHSIZE= df$HHSIZE)

for(hhsize in unique(Covid.hhsize$HHSIZE)){
  dd<- Covid.hhsize%>%
    filter(HHSIZE== hhsize)%>%
    select(-HHSIZE)%>%
    drop_na()
  
ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))

df.less<- data.frame( Advantage.Rating= ratios)
print(kable(df.less, align = "l", caption =paste0(hhsize, "'s less cash freq ratio")))
}


## -------------------------------------------------------------------------------------------
card.advg<- space%>%
select(QQ13B_1,QQ13B_2,QQ13B_3,QQ13B_4,QQ13B_5,
       QQ13B_6, QQ13B_7, QQ13B_8,QQ13B_9,QQ13B_10)
names(card.advg)<- c("card.acceptance", "card.faster","card.easier",
                "card.safer","no_worry_carrying_cash","aware_spending",
                "other_advantage","no_advantage","do_not_use_card",
                "dont_know")
#Covariance matrix 
card.advg2<- na.omit(card.advg)


## ----fig.cap ="Covariance Matrix for Ratings of credit Cards",fig.width=12, fig.height=8----
corrplot(cov(card.advg2), method = 'number',col = COL2('RdYlBu', 2),
         type = 'lower', tl.col='black')


## -------------------------------------------------------------------------------------------
#by months 
paymethod<- space%>%
  select(QA7A_1, QA7AI_1, QA7AII_1)%>%
  mutate(QA7A_1= case_when(QA7A_1==1~	"Cash",
                           QA7A_1==2~	"Card",
                           QA7A_1==3~	"Mobile phone app",
                           QA7A_1==4~	"Bank cheque",
                           QA7A_1==5~	"Credit transfer",
                          QA7A_1==6~	"Loyalty points",
                          QA7A_1==7~	"Other",
                         # QA7A_1==999999~	"Don't know",
),
QA7AI_1= case_when(
  QA7AI_1==1~"By inserting the card into a terminal",
  QA7AI_1==2~	"Using contactless technology",
  #QA7AI_1==999999	~"Don't know"

),
QA7AII_1=case_when(
  QA7AII_1== 1~	"Using my bank's mobile application",
QA7AII_1==2~	"Using ApplePay",
QA7AII_1==3~	"Using GooglePay",
QA7AII_1==4~	"Other",
QA7AII_1==5~	"Payconiq by Bancontact",
QA7AII_1==6~	"mTasku",
QA7AII_1==8	~"Jiffy",
QA7AII_1==9~	"Swedbank mobila lietotne",
QA7AII_1==10~	"MoQ",
QA7AII_1==11~	"Digicash",
QA7AII_1==12~	"Bank of Valletta",
QA7AII_1==13~	"Bankomatkarte mobil",
QA7AII_1==14~	"MBway",
QA7AII_1==15~	"NLB pay",
QA7AII_1==16~	"MobilePay TB",
QA7AII_1==17~	"MobilePay",
QA7AII_1==18~	"Swedbanki mobiilipank",
QA7AII_1==19~	"Viva",
QA7AII_1==20~	"Samsung Pay",
QA7AII_1==22~	"Satispay",
QA7AII_1==23~	"Citadele mobila aplikacija",
QA7AII_1==24~	"Revolut",
QA7AII_1==25~	"Bluecode",
QA7AII_1==26~	"mDenarnic@",
QA7AII_1==27~	"mBank SK",
QA7AII_1==28~	"Siirto",
QA7AII_1==29~	"Paypal",
QA7AII_1==30~	"Android pay",
QA7AII_1==31~	"SEB mobila lietotne",
QA7AII_1==32~	"kWallet",
QA7AII_1==33~	"Wave2Pay",
QA7AII_1==34~	"VÚB Mobil Banking",
QA7AII_1==35~	"my Alpha wallet",
QA7AII_1==36~	"Lydia",
QA7AII_1==37~	"Mobilly (parking, train tickets)",
QA7AII_1==38~	"ZOIN",
QA7AII_1==39~	"mBills",
QA7AII_1==40~	"i-bank Pay",
QA7AII_1==41~	"Pumpkin",
QA7AII_1==42~	"Paylib",
QA7AII_1==43~	"Pivo",
#QA7AII_1==999999~"	Don't know",
  
))

paymethod1<- paymethod%>%
  drop_na(QA7A_1)
prop.table(table(paymethod1$QA7A_1))%>%
  data.frame()%>%
  arrange(desc(Freq))%>%
  kable(align = "l", col.names = c("Instrument of POS payment","Population"),
        caption = "Instrument of POS payment by population")


## -------------------------------------------------------------------------------------------

paymethod2<- paymethod%>%
  drop_na(QA7AI_1)
prop.table(table(paymethod2$QA7AI_1))%>%
  data.frame()%>%
  arrange(desc(Freq))%>%
  kable(align = "l", col.names = c("Card payment","Population"),
        caption = "Card payment by population")


## -------------------------------------------------------------------------------------------
paymethod3<- paymethod%>%
  drop_na(QA7AII_1)
prop.table(table(paymethod3$QA7AII_1))%>%
  data.frame()%>%
  arrange(desc(Freq))%>%
  kable(align = "l", col.names = c("Mobile payment","Population"),
        caption = "Mobile payment  by population")

