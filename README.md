PDFRpoert
================
my\_name
2023-08-25

\#load libraries

``` r
library(tidyverse)
library(knitr)
library(corrplot)
library(kableExtra)
```

# load data

``` r
space<- read_csv("SPACE_wide_2022.csv")
```

    ## Warning: One or more parsing issues, see `problems()` for details

    ## Rows: 39766 Columns: 224
    ## ── Column specification ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (2): ID, COUNTRY
    ## dbl (221): ROUND, MONTH, WEEKDAY, LANG, D1, D6_1, D6_2A, D6_2B, Q1_1, Q1_2, ...
    ## lgl   (1): QA7AII_8
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
space[space==999998] <- NA
```

# Data preparatio

``` r
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
         D6_2B= case_when(D6_2B==1~ "Responsible for housework (e.g. ordinary shopping, looking after home and family)",
                          D6_2B==2~ "Student (full-time)",
                          D6_2B==3~ "Unemployed or temporarily not working",
                          D6_2B==4~ "Retired or unable to work through illness",
                          D6_2B==5~ "Other",
                          D6_2B==999998~    "Refusal / No answer"),
         INCOME= case_when(INCOME==1~   "EUR 750 or less",
                           INCOME==2~   "Between EUR 751 and EUR 1,500",
                           INCOME==3~   "Between EUR 1,501 and EUR 2,500",
                           INCOME==4~"Between EUR 2,501 and EUR 4,000",
                           INCOME==5~   "More than EUR 4,000",
                           INCOME==999998~  "Refusal")
         
  )
```

# Correlation for Demographic

``` r
### correlation ####
df_cor<- space %>%
  select(AGE, D1, EDUCATION, D6_1, D6_2A, D6_2B, HHSIZE, DEPCHILDREN, INCOME)

df_cor[df_cor>100]<- NA

cor_mat<-cor(df_cor,use='pairwise.complete.obs')
```

    ## Warning in cor(df_cor, use = "pairwise.complete.obs"): the standard deviation
    ## is zero

``` r
corrplot(cor_mat, method = 'number',col = COL2('RdYlBu', 2),
         type = 'lower', tl.col='black')
```

![Correlation Matrix](README_files/figure-gfm/unnamed-chunk-5-1.png)

The correlation matrix depicts the relationships between various
variables. Notable correlations include a moderate positive relationship
between AGE and D6\_2B (0.5399) and a strong positive relationship
between HHSIZE and DEPCHILDREN (0.7473), suggesting larger households
tend to have more dependent children. There’s a moderate negative
correlation between DEPCHILDREN and both AGE (-0.3418) and HHSIZE
(-0.3834), implying that as the number of dependent children increases,
household size and the age of respondents tend to decrease. Education
shows a strong negative correlation with D6\_2A (-0.3705), suggesting
that higher education levels are associated with less agreement with a
particular statement (D6\_2A). Additionally, D1 exhibits a moderate
negative correlation with D6\_2B (-0.2106), implying that as the level
of agreement with D1 increases, agreement with D6\_2B tends to decrease.
However, some correlations, like D6\_1 with D6\_2A and D6\_2B, have
missing values, indicating a lack of correlation data for these pairs.

# Visualization of the variables

``` r
ggplot(df, aes(AGE)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), fill="blue", color="black") + 
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", title = "Age Distribution in %")+
  theme_classic()
```

    ## Warning: The dot-dot notation (`..count..`) was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `after_stat(count)` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
#The plot shows that most of the people who took part in the
# survey falls between 64-69 years of age. The group with over 75 years had the least people
```

``` r
# Gender
ggplot(df, aes(D1)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="blue", color="black") + 
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", fill="day") +
  labs(y = "Percent", title = "Gender Distribution in %")+
  theme_classic()
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
#Females are slightly higher than males in the survey. 
```

``` r
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
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# most the people who took part in the survey have attained secondary education.
```

``` r
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
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
#Majority of the people earn between 1510 and 2500 euros
```

``` r
#HHSIZE
ggplot(df, aes(factor(HHSIZE))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="blue", color="black") + 
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", title = "Household size",
       x="Household size")+
  theme_classic()
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
#majority of the households have atmost 2 people per household.
```

``` r
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
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
#Almost 50% of the people are employees. 
```

``` r
#DEPCHILDREN
ggplot(df, aes(factor(DEPCHILDREN))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="blue", color="black") + 
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", title = "Number of children or economically dependent",
       x="DEPCHILDREN")+
  theme_classic()
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
#Over 40% of the participants (majority) do not have dependents.
```

``` r
#Country
ggplot(df, aes(COUNTRY)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="blue", color="black") + 
  scale_y_continuous(labels=scales::percent) +
  labs(y = "Percent", title = "Country Distribution in %")+
  theme_classic()
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
#Most of the participants are from france followed by italy and spain comes in at third position
```

# Pop holding Bundles of payment instruments in percentage

``` r
POS<- space%>%
  select(QQ1A_1,,QQ1A_2,QQ1A_3,QQ1D)%>%
  mutate(QQ1D= case_when(
    QQ1D==1~    "Yes, it is available for me",
    QQ1D==2~    "No, it is not available for me",
    QQ1D==3~    "heard aboutservice, not sure if available",
    QQ1D==4~    "I have not heard about this service",
    QQ1D==999999~   "Don't know"
    
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
```

| account | crdit.dbit | crypto | instnt                                    | Pop     |
| :------ | :--------- | :----- | :---------------------------------------- | :------ |
| 1       | 1          | 0      | Yes, it is available for me               | 39.36 % |
| 1       | 1          | 0      | heard aboutservice, not sure if available | 14.05 % |
| 1       | 1          | 0      | No, it is not available for me            | 13.24 % |
| 1       | 1          | 0      | I have not heard about this service       | 11.18 % |
| 0       | 1          | 0      | Yes, it is available for me               | 4.22 %  |
| 1       | 0          | 0      | Yes, it is available for me               | 3.54 %  |
| 1       | 1          | 1      | Yes, it is available for me               | 2.69 %  |
| 0       | 1          | 0      | No, it is not available for me            | 2.29 %  |
| 0       | 1          | 0      | heard aboutservice, not sure if available | 1.66 %  |
| 0       | 1          | 0      | I have not heard about this service       | 1.45 %  |
| 1       | 0          | 0      | No, it is not available for me            | 1.3 %   |
| 1       | 0          | 0      | heard aboutservice, not sure if available | 1.07 %  |
| 1       | 0          | 0      | I have not heard about this service       | 0.99 %  |
| 1       | 1          | 1      | heard aboutservice, not sure if available | 0.54 %  |
| 1       | 1          | 1      | I have not heard about this service       | 0.37 %  |

The table presents the distribution of people holding specific payment
instruments based on their access or awareness of various payment
services. The columns represent different conditions: “account” (whether
they have an account), “credit/debit” (whether they have a credit/debit
card), “crypto” (whether they have access to cryptocurrency), “instant”
(whether they have access to instant payment services), and “Pop” (the
percentage of respondents falling into each category). For example,
39.36% of respondents have an account, hold a credit/debit card, and
have access to instant payment services, while 14.05% have the same but
are uncertain if instant payment is available to them. This table
provides insights into the intersections of various payment methods and
respondents’ access or awareness of these methods, helping to understand
the usage patterns and familiarity with different payment instruments
among the surveyed population.

The above anlysis haave been broken down by various demographic
variables as shwon below.

# Pop holding Bundles of payment instruments in percentage by Age

``` r
#### Age

POS<- df%>%
  select(QQ1A_1,,QQ1A_2,QQ1A_3,QQ1D, AGE)%>%
  mutate(QQ1D= case_when(
    QQ1D==1~    "Yes, it is available for me",
    QQ1D==2~    "No, it is not available for me",
    QQ1D==3~    "heard aboutservice, not sure if available",
    QQ1D==4~    "I have not heard about this service",
    QQ1D==999999~   "Don't know"
    
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
```

| account | crdit.dbit | crypto | instnt                                    | AGE   | Pop    |
| :------ | :--------- | :----- | :---------------------------------------- | :---- | :----- |
| 1       | 1          | 0      | Yes, it is available for me               | 35-39 | 4.37 % |
| 1       | 1          | 0      | Yes, it is available for me               | 64-69 | 4.24 % |
| 1       | 1          | 0      | Yes, it is available for me               | 50-54 | 3.93 % |
| 1       | 1          | 0      | Yes, it is available for me               | 45-49 | 3.87 % |
| 1       | 1          | 0      | Yes, it is available for me               | 30-34 | 3.54 % |
| 1       | 1          | 0      | Yes, it is available for me               | 40-44 | 3.52 % |
| 1       | 1          | 0      | Yes, it is available for me               | 55-59 | 3.49 % |
| 1       | 1          | 0      | Yes, it is available for me               | 18-24 | 3.37 % |
| 1       | 1          | 0      | Yes, it is available for me               | 60-64 | 3.09 % |
| 1       | 1          | 0      | Yes, it is available for me               | 25-29 | 2.55 % |
| 1       | 1          | 0      | Yes, it is available for me               | 70-74 | 2.49 % |
| 1       | 1          | 0      | No, it is not available for me            | 64-69 | 1.99 % |
| 1       | 1          | 0      | heard aboutservice, not sure if available | 64-69 | 1.96 % |
| 1       | 1          | 0      | I have not heard about this service       | 64-69 | 1.63 % |
| 1       | 1          | 0      | heard aboutservice, not sure if available | 35-39 | 1.36 % |

# Pop holding Bundles of payment instruments in percentage by education

``` r
#Education 
POS<- df%>%
  select(QQ1A_1,,QQ1A_2,QQ1A_3,QQ1D, EDUCATION)%>%
  mutate(QQ1D= case_when(
    QQ1D==1~    "Yes, it is available for me",
    QQ1D==2~    "No, it is not available for me",
    QQ1D==3~    "heard aboutservice, not sure if available",
    QQ1D==4~    "I have not heard about this service",
    QQ1D==999999~   "Don't know"

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
```

| account | crdit.dbit | crypto | instnt                                    | EDUCATION                         | Pop     |
| :------ | :--------- | :----- | :---------------------------------------- | :-------------------------------- | :------ |
| 1       | 1          | 0      | Yes, it is available for me               | Upper/post-secondary education    | 17.06 % |
| 1       | 1          | 0      | Yes, it is available for me               | University/PhD/research           | 15.99 % |
| 1       | 1          | 0      | heard aboutservice, not sure if available | Upper/post-secondary education    | 6.34 %  |
| 1       | 1          | 0      | Yes, it is available for me               | Primary/lower secondary education | 6.29 %  |
| 1       | 1          | 0      | No, it is not available for me            | Upper/post-secondary education    | 5.61 %  |
| 1       | 1          | 0      | I have not heard about this service       | Upper/post-secondary education    | 5.26 %  |
| 1       | 1          | 0      | heard aboutservice, not sure if available | University/PhD/research           | 5.25 %  |
| 1       | 1          | 0      | No, it is not available for me            | University/PhD/research           | 4.83 %  |
| 1       | 1          | 0      | I have not heard about this service       | University/PhD/research           | 3.94 %  |
| 1       | 1          | 0      | No, it is not available for me            | Primary/lower secondary education | 2.8 %   |
| 1       | 1          | 0      | heard aboutservice, not sure if available | Primary/lower secondary education | 2.45 %  |
| 1       | 1          | 0      | I have not heard about this service       | Primary/lower secondary education | 1.97 %  |
| 0       | 1          | 0      | Yes, it is available for me               | Upper/post-secondary education    | 1.85 %  |
| 1       | 0          | 0      | Yes, it is available for me               | Upper/post-secondary education    | 1.64 %  |
| 0       | 1          | 0      | Yes, it is available for me               | University/PhD/research           | 1.62 %  |

# Pop holding Bundles of payment instruments in percentage by Activity status

``` r
## Activity Status
POS<- df%>%
  select(QQ1A_1,,QQ1A_2,QQ1A_3,QQ1D, D6_1)%>%
  mutate(QQ1D= case_when(
    QQ1D==1~    "Yes, it is available for me",
    QQ1D==2~    "No, it is not available for me",
    QQ1D==3~    "heard aboutservice, not sure if available",
    QQ1D==4~    "I have not heard about this service",
    QQ1D==999999~   "Don't know"
    
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
```

| account | crdit.dbit | crypto | instnt                                    | Activity                                   | Pop     |
| :------ | :--------- | :----- | :---------------------------------------- | :----------------------------------------- | :------ |
| 1       | 1          | 0      | Yes, it is available for me               | Employee                                   | 21.25 % |
| 1       | 1          | 0      | Yes, it is available for me               | Without a professional activity or student | 13.31 % |
| 1       | 1          | 0      | heard aboutservice, not sure if available | Employee                                   | 6.5 %   |
| 1       | 1          | 0      | heard aboutservice, not sure if available | Without a professional activity or student | 5.94 %  |
| 1       | 1          | 0      | No, it is not available for me            | Without a professional activity or student | 5.89 %  |
| 1       | 1          | 0      | No, it is not available for me            | Employee                                   | 5.72 %  |
| 1       | 1          | 0      | I have not heard about this service       | Without a professional activity or student | 5.09 %  |
| 1       | 1          | 0      | I have not heard about this service       | Employee                                   | 4.81 %  |
| 1       | 1          | 0      | Yes, it is available for me               | Self-employed                              | 4.67 %  |
| 0       | 1          | 0      | Yes, it is available for me               | Employee                                   | 2.08 %  |
| 1       | 0          | 0      | Yes, it is available for me               | Employee                                   | 1.93 %  |
| 1       | 1          | 1      | Yes, it is available for me               | Employee                                   | 1.74 %  |
| 1       | 1          | 0      | No, it is not available for me            | Self-employed                              | 1.56 %  |
| 1       | 1          | 0      | heard aboutservice, not sure if available | Self-employed                              | 1.55 %  |
| 0       | 1          | 0      | Yes, it is available for me               | Without a professional activity or student | 1.53 %  |

# Pop holding Bundles of payment instruments in percentage by employment status

``` r
## Employment status
POS<- df%>%
  select(QQ1A_1,,QQ1A_2,QQ1A_3,QQ1D, D6_2A)%>%
  mutate(QQ1D= case_when(
    QQ1D==1~    "Yes, it is available for me",
    QQ1D==2~    "No, it is not available for me",
    QQ1D==3~    "heard aboutservice, not sure if available",
    QQ1D==4~    "I have not heard about this service",
    QQ1D==999999~   "Don't know"
    
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
```

| account | crdit.dbit | crypto | instnt                                    | Employment\_Status                                                 | Pop     |
| :------ | :--------- | :----- | :---------------------------------------- | :----------------------------------------------------------------- | :------ |
| 1       | 1          | 0      | Yes, it is available for me               | NA                                                                 | 18.14 % |
| 1       | 1          | 0      | heard aboutservice, not sure if available | NA                                                                 | 7.54 %  |
| 1       | 1          | 0      | No, it is not available for me            | NA                                                                 | 7.53 %  |
| 1       | 1          | 0      | Yes, it is available for me               | Employed position, working mainly at a desk                        | 7.08 %  |
| 1       | 1          | 0      | I have not heard about this service       | NA                                                                 | 6.38 %  |
| 1       | 1          | 0      | Yes, it is available for me               | Middle management or other management (e.g. department head)       | 3.62 %  |
| 1       | 1          | 0      | Yes, it is available for me               | Employed position, not at a desk                                   | 3.4 %   |
| 1       | 1          | 0      | Yes, it is available for me               | Employed professional (e.g. doctor, lawyer, accountant, architect) | 3.01 %  |
| 1       | 1          | 0      | heard aboutservice, not sure if available | Employed position, working mainly at a desk                        | 2.17 %  |
| 0       | 1          | 0      | Yes, it is available for me               | NA                                                                 | 2.13 %  |
| 1       | 1          | 0      | Yes, it is available for me               | Manual worker                                                      | 2.08 %  |
| 1       | 1          | 0      | No, it is not available for me            | Employed position, working mainly at a desk                        | 1.8 %   |
| 1       | 0          | 0      | Yes, it is available for me               | NA                                                                 | 1.61 %  |
| 1       | 1          | 0      | I have not heard about this service       | Employed position, working mainly at a desk                        | 1.51 %  |
| 0       | 1          | 0      | No, it is not available for me            | NA                                                                 | 1.39 %  |

# Pop holding Bundles of payment instruments in percentage inactivity status

``` r
###  Inactivity status
POS<- df%>%
  select(QQ1A_1,,QQ1A_2,QQ1A_3,QQ1D, D6_2B)%>%
  mutate(QQ1D= case_when(
    QQ1D==1~    "Yes, it is available for me",
    QQ1D==2~    "No, it is not available for me",
    QQ1D==3~    "heard aboutservice, not sure if available",
    QQ1D==4~    "I have not heard about this service",
    QQ1D==999999~   "Don't know"
    
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
```

| account | crdit.dbit | crypto | instnt                                    | Inactivity\_Status                                                                | Pop     |
| :------ | :--------- | :----- | :---------------------------------------- | :-------------------------------------------------------------------------------- | :------ |
| 1       | 1          | 0      | Yes, it is available for me               | NA                                                                                | 26.05 % |
| 1       | 1          | 0      | heard aboutservice, not sure if available | NA                                                                                | 8.11 %  |
| 1       | 1          | 0      | Yes, it is available for me               | Retired or unable to work through illness                                         | 7.5 %   |
| 1       | 1          | 0      | No, it is not available for me            | NA                                                                                | 7.36 %  |
| 1       | 1          | 0      | I have not heard about this service       | NA                                                                                | 6.09 %  |
| 1       | 1          | 0      | No, it is not available for me            | Retired or unable to work through illness                                         | 4.03 %  |
| 1       | 1          | 0      | heard aboutservice, not sure if available | Retired or unable to work through illness                                         | 3.69 %  |
| 1       | 1          | 0      | I have not heard about this service       | Retired or unable to work through illness                                         | 3.43 %  |
| 0       | 1          | 0      | Yes, it is available for me               | NA                                                                                | 2.69 %  |
| 1       | 0          | 0      | Yes, it is available for me               | NA                                                                                | 2.41 %  |
| 1       | 1          | 1      | Yes, it is available for me               | NA                                                                                | 2.17 %  |
| 1       | 1          | 0      | Yes, it is available for me               | Student (full-time)                                                               | 1.98 %  |
| 1       | 1          | 0      | Yes, it is available for me               | Responsible for housework (e.g. ordinary shopping, looking after home and family) | 1.97 %  |
| 1       | 1          | 0      | Yes, it is available for me               | Unemployed or temporarily not working                                             | 1.57 %  |
| 0       | 1          | 0      | No, it is not available for me            | NA                                                                                | 1.15 %  |

# Pop holding Bundles of payment instruments in percentage by household size

``` r
###  Household size
POS<- df%>%
  select(QQ1A_1,,QQ1A_2,QQ1A_3,QQ1D, HHSIZE)%>%
  mutate(QQ1D= case_when(
    QQ1D==1~    "Yes, it is available for me",
    QQ1D==2~    "No, it is not available for me",
    QQ1D==3~    "heard aboutservice, not sure if available",
    QQ1D==4~    "I have not heard about this service",
    QQ1D==999999~   "Don't know"
    
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
```

| account | crdit.dbit | crypto | instnt                                    | HHSIZE | Pop    |
| :------ | :--------- | :----- | :---------------------------------------- | :----- | :----- |
| 1       | 1          | 0      | Yes, it is available for me               | 2      | 13.2 % |
| 1       | 1          | 0      | Yes, it is available for me               | 3      | 8.95 % |
| 1       | 1          | 0      | Yes, it is available for me               | 4      | 7.7 %  |
| 1       | 1          | 0      | Yes, it is available for me               | 1      | 6.58 % |
| 1       | 1          | 0      | heard aboutservice, not sure if available | 2      | 4.92 % |
| 1       | 1          | 0      | No, it is not available for me            | 2      | 4.85 % |
| 1       | 1          | 0      | I have not heard about this service       | 2      | 4.08 % |
| 1       | 1          | 0      | heard aboutservice, not sure if available | 3      | 3.22 % |
| 1       | 1          | 0      | Yes, it is available for me               | 5      | 2.86 % |
| 1       | 1          | 0      | No, it is not available for me            | 3      | 2.67 % |
| 1       | 1          | 0      | No, it is not available for me            | 1      | 2.65 % |
| 1       | 1          | 0      | heard aboutservice, not sure if available | 1      | 2.59 % |
| 1       | 1          | 0      | heard aboutservice, not sure if available | 4      | 2.39 % |
| 1       | 1          | 0      | I have not heard about this service       | 3      | 2.36 % |
| 1       | 1          | 0      | No, it is not available for me            | 4      | 2.23 % |

# Pop holding Bundles of payment instruments in percentage by income

``` r
### INCOME
POS<- df%>%
  select(QQ1A_1,,QQ1A_2,QQ1A_3,QQ1D, INCOME)%>%
  mutate(QQ1D= case_when(
    QQ1D==1~    "Yes, it is available for me",
    QQ1D==2~    "No, it is not available for me",
    QQ1D==3~    "heard aboutservice, not sure if available",
    QQ1D==4~    "I have not heard about this service",
    QQ1D==999999~   "Don't know"
    
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
```

| account | crdit.dbit | crypto | instnt                                    | INCOME                          | Pop     |
| :------ | :--------- | :----- | :---------------------------------------- | :------------------------------ | :------ |
| 1       | 1          | 0      | Yes, it is available for me               | Between EUR 2,501 and EUR 4,000 | 11.86 % |
| 1       | 1          | 0      | Yes, it is available for me               | Between EUR 1,501 and EUR 2,500 | 11.28 % |
| 1       | 1          | 0      | Yes, it is available for me               | More than EUR 4,000             | 6.73 %  |
| 1       | 1          | 0      | Yes, it is available for me               | Between EUR 751 and EUR 1,500   | 6.34 %  |
| 1       | 1          | 0      | heard aboutservice, not sure if available | Between EUR 1,501 and EUR 2,500 | 4.23 %  |
| 1       | 1          | 0      | No, it is not available for me            | Between EUR 1,501 and EUR 2,500 | 3.93 %  |
| 1       | 1          | 0      | heard aboutservice, not sure if available | Between EUR 2,501 and EUR 4,000 | 3.88 %  |
| 1       | 1          | 0      | No, it is not available for me            | Between EUR 2,501 and EUR 4,000 | 3.73 %  |
| 1       | 1          | 0      | I have not heard about this service       | Between EUR 1,501 and EUR 2,500 | 3.27 %  |
| 1       | 1          | 0      | I have not heard about this service       | Between EUR 2,501 and EUR 4,000 | 2.95 %  |
| 1       | 1          | 0      | heard aboutservice, not sure if available | Between EUR 751 and EUR 1,500   | 2.84 %  |
| 1       | 1          | 0      | I have not heard about this service       | Between EUR 751 and EUR 1,500   | 2.29 %  |
| 1       | 1          | 0      | No, it is not available for me            | Between EUR 751 and EUR 1,500   | 2.21 %  |
| 1       | 1          | 0      | No, it is not available for me            | More than EUR 4,000             | 1.88 %  |
| 1       | 1          | 0      | Yes, it is available for me               | EUR 750 or less                 | 1.88 %  |

# Pop holding Bundles of payment instruments in percentage country

``` r
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
```

    ## 
    ## 
    ## Table: AT's Population Holdings of the Top 15 Bundles of Payment Instruments
    ## 
    ## |account |crdit.dbit |crypto |instnt |COUNTRY |Pop     |
    ## |:-------|:----------|:------|:------|:-------|:-------|
    ## |1       |1          |0      |1      |AT      |28.43 % |
    ## |1       |1          |0      |4      |AT      |17.45 % |
    ## |1       |1          |0      |2      |AT      |15.36 % |
    ## |1       |1          |0      |3      |AT      |14.76 % |
    ## |1       |0          |0      |1      |AT      |4.22 %  |
    ## |1       |1          |1      |1      |AT      |3.38 %  |
    ## |0       |1          |0      |1      |AT      |2.49 %  |
    ## |1       |0          |0      |2      |AT      |2.13 %  |
    ## |1       |0          |0      |4      |AT      |2.13 %  |
    ## |0       |1          |0      |2      |AT      |1.65 %  |
    ## |1       |0          |0      |3      |AT      |1.49 %  |
    ## |0       |1          |0      |4      |AT      |1.29 %  |
    ## |1       |1          |1      |3      |AT      |1.29 %  |
    ## |0       |1          |0      |3      |AT      |0.88 %  |
    ## |1       |1          |1      |4      |AT      |0.88 %  |
    ## 
    ## 
    ## Table: BE's Population Holdings of the Top 15 Bundles of Payment Instruments
    ## 
    ## |account |crdit.dbit |crypto |instnt |COUNTRY |Pop     |
    ## |:-------|:----------|:------|:------|:-------|:-------|
    ## |1       |1          |0      |1      |BE      |39.73 % |
    ## |1       |1          |0      |3      |BE      |16.44 % |
    ## |1       |1          |0      |2      |BE      |12.17 % |
    ## |1       |1          |0      |4      |BE      |10.42 % |
    ## |1       |0          |0      |1      |BE      |4 %     |
    ## |0       |1          |0      |1      |BE      |3.63 %  |
    ## |0       |1          |0      |2      |BE      |2.69 %  |
    ## |1       |1          |1      |1      |BE      |2.25 %  |
    ## |0       |1          |0      |3      |BE      |1.58 %  |
    ## |1       |0          |0      |2      |BE      |1.41 %  |
    ## |0       |1          |0      |4      |BE      |1.28 %  |
    ## |1       |0          |0      |3      |BE      |1.18 %  |
    ## |1       |0          |0      |4      |BE      |1.14 %  |
    ## |1       |1          |1      |3      |BE      |0.34 %  |
    ## |0       |0          |0      |4      |BE      |0.3 %   |
    ## 
    ## 
    ## Table: CY's Population Holdings of the Top 15 Bundles of Payment Instruments
    ## 
    ## |account |crdit.dbit |crypto |instnt |COUNTRY |Pop     |
    ## |:-------|:----------|:------|:------|:-------|:-------|
    ## |1       |1          |0      |1      |CY      |42.72 % |
    ## |1       |1          |0      |3      |CY      |12.5 %  |
    ## |0       |1          |0      |1      |CY      |9.84 %  |
    ## |1       |1          |0      |2      |CY      |9.65 %  |
    ## |1       |1          |0      |4      |CY      |5.81 %  |
    ## |1       |1          |1      |1      |CY      |5.51 %  |
    ## |0       |1          |0      |2      |CY      |2.66 %  |
    ## |0       |1          |0      |3      |CY      |2.66 %  |
    ## |0       |1          |0      |4      |CY      |2.36 %  |
    ## |1       |0          |0      |1      |CY      |1.48 %  |
    ## |1       |1          |0      |999999 |CY      |0.79 %  |
    ## |1       |0          |0      |4      |CY      |0.69 %  |
    ## |0       |0          |0      |2      |CY      |0.49 %  |
    ## |0       |1          |1      |1      |CY      |0.49 %  |
    ## |1       |1          |1      |2      |CY      |0.49 %  |
    ## 
    ## 
    ## Table: EE's Population Holdings of the Top 15 Bundles of Payment Instruments
    ## 
    ## |account |crdit.dbit |crypto |instnt |COUNTRY |Pop     |
    ## |:-------|:----------|:------|:------|:-------|:-------|
    ## |1       |1          |0      |1      |EE      |45.09 % |
    ## |1       |1          |0      |3      |EE      |16.67 % |
    ## |1       |1          |0      |4      |EE      |13.15 % |
    ## |1       |1          |0      |2      |EE      |7.24 %  |
    ## |1       |1          |1      |1      |EE      |3.65 %  |
    ## |0       |1          |0      |1      |EE      |2.99 %  |
    ## |1       |0          |0      |1      |EE      |2.86 %  |
    ## |0       |1          |0      |4      |EE      |1.46 %  |
    ## |0       |1          |0      |2      |EE      |1.39 %  |
    ## |1       |0          |0      |3      |EE      |1.2 %   |
    ## |1       |0          |0      |4      |EE      |1.13 %  |
    ## |0       |1          |0      |3      |EE      |0.93 %  |
    ## |1       |1          |0      |999999 |EE      |0.6 %   |
    ## |1       |1          |1      |4      |EE      |0.4 %   |
    ## |1       |0          |0      |2      |EE      |0.27 %  |
    ## 
    ## 
    ## Table: ES's Population Holdings of the Top 15 Bundles of Payment Instruments
    ## 
    ## |account |crdit.dbit |crypto |instnt |COUNTRY |Pop     |
    ## |:-------|:----------|:------|:------|:-------|:-------|
    ## |1       |1          |0      |1      |ES      |46.48 % |
    ## |1       |1          |0      |2      |ES      |19.03 % |
    ## |1       |1          |0      |3      |ES      |8.13 %  |
    ## |1       |1          |0      |4      |ES      |6.28 %  |
    ## |0       |1          |0      |1      |ES      |5.1 %   |
    ## |1       |1          |1      |1      |ES      |3.07 %  |
    ## |1       |0          |0      |1      |ES      |2.78 %  |
    ## |0       |1          |0      |2      |ES      |2.43 %  |
    ## |1       |0          |0      |2      |ES      |1.49 %  |
    ## |0       |1          |0      |3      |ES      |1.25 %  |
    ## |0       |1          |0      |4      |ES      |1.16 %  |
    ## |1       |0          |0      |4      |ES      |0.56 %  |
    ## |1       |0          |0      |3      |ES      |0.33 %  |
    ## |1       |1          |1      |2      |ES      |0.31 %  |
    ## |1       |1          |1      |3      |ES      |0.31 %  |
    ## 
    ## 
    ## Table: FI's Population Holdings of the Top 15 Bundles of Payment Instruments
    ## 
    ## |account |crdit.dbit |crypto |instnt |COUNTRY |Pop     |
    ## |:-------|:----------|:------|:------|:-------|:-------|
    ## |1       |1          |0      |1      |FI      |47.92 % |
    ## |1       |1          |0      |2      |FI      |22.69 % |
    ## |1       |1          |0      |3      |FI      |9.29 %  |
    ## |1       |1          |0      |4      |FI      |8.99 %  |
    ## |1       |1          |1      |1      |FI      |3.67 %  |
    ## |1       |1          |0      |999999 |FI      |1.22 %  |
    ## |0       |1          |0      |1      |FI      |0.86 %  |
    ## |1       |0          |0      |2      |FI      |0.86 %  |
    ## |1       |0          |0      |1      |FI      |0.79 %  |
    ## |1       |1          |1      |2      |FI      |0.73 %  |
    ## |0       |1          |0      |2      |FI      |0.66 %  |
    ## |1       |1          |1      |4      |FI      |0.5 %   |
    ## |1       |1          |1      |3      |FI      |0.46 %  |
    ## |1       |0          |0      |4      |FI      |0.3 %   |
    ## |0       |1          |0      |4      |FI      |0.26 %  |
    ## 
    ## 
    ## Table: FR's Population Holdings of the Top 15 Bundles of Payment Instruments
    ## 
    ## |account |crdit.dbit |crypto |instnt |COUNTRY |Pop     |
    ## |:-------|:----------|:------|:------|:-------|:-------|
    ## |1       |1          |0      |1      |FR      |39.68 % |
    ## |1       |1          |0      |2      |FR      |15.53 % |
    ## |1       |1          |0      |3      |FR      |15.15 % |
    ## |1       |1          |0      |4      |FR      |9.69 %  |
    ## |1       |0          |0      |1      |FR      |4.52 %  |
    ## |0       |1          |0      |1      |FR      |3.57 %  |
    ## |0       |1          |0      |2      |FR      |2.45 %  |
    ## |1       |1          |1      |1      |FR      |1.58 %  |
    ## |0       |1          |0      |3      |FR      |1.43 %  |
    ## |0       |1          |0      |4      |FR      |1.32 %  |
    ## |1       |0          |0      |2      |FR      |1.14 %  |
    ## |1       |0          |0      |4      |FR      |0.92 %  |
    ## |1       |0          |0      |3      |FR      |0.61 %  |
    ## |1       |1          |1      |2      |FR      |0.31 %  |
    ## |0       |0          |0      |4      |FR      |0.23 %  |
    ## 
    ## 
    ## Table: GR's Population Holdings of the Top 15 Bundles of Payment Instruments
    ## 
    ## |account |crdit.dbit |crypto |instnt |COUNTRY |Pop     |
    ## |:-------|:----------|:------|:------|:-------|:-------|
    ## |1       |1          |0      |1      |GR      |31.3 %  |
    ## |1       |1          |0      |2      |GR      |18.73 % |
    ## |1       |1          |0      |4      |GR      |13.38 % |
    ## |1       |1          |0      |3      |GR      |12.2 %  |
    ## |0       |1          |0      |2      |GR      |6.37 %  |
    ## |0       |1          |0      |1      |GR      |3.85 %  |
    ## |1       |1          |1      |1      |GR      |2.73 %  |
    ## |1       |0          |0      |1      |GR      |1.98 %  |
    ## |0       |1          |0      |3      |GR      |1.93 %  |
    ## |0       |1          |0      |4      |GR      |1.44 %  |
    ## |1       |1          |1      |3      |GR      |0.91 %  |
    ## |1       |1          |0      |999999 |GR      |0.75 %  |
    ## |1       |0          |0      |4      |GR      |0.7 %   |
    ## |0       |0          |0      |2      |GR      |0.64 %  |
    ## |0       |0          |0      |4      |GR      |0.64 %  |
    ## 
    ## 
    ## Table: IE's Population Holdings of the Top 15 Bundles of Payment Instruments
    ## 
    ## |account |crdit.dbit |crypto |instnt |COUNTRY |Pop     |
    ## |:-------|:----------|:------|:------|:-------|:-------|
    ## |1       |1          |0      |1      |IE      |30.87 % |
    ## |1       |1          |0      |4      |IE      |17.91 % |
    ## |1       |1          |0      |3      |IE      |14.61 % |
    ## |1       |1          |0      |2      |IE      |8.31 %  |
    ## |0       |1          |0      |1      |IE      |7.07 %  |
    ## |1       |0          |0      |1      |IE      |3.87 %  |
    ## |1       |1          |1      |1      |IE      |3.36 %  |
    ## |0       |1          |0      |4      |IE      |2.79 %  |
    ## |0       |1          |0      |2      |IE      |2.32 %  |
    ## |0       |1          |0      |3      |IE      |2.01 %  |
    ## |1       |0          |0      |2      |IE      |1.19 %  |
    ## |1       |0          |0      |4      |IE      |1.14 %  |
    ## |1       |0          |0      |3      |IE      |0.83 %  |
    ## |1       |1          |1      |3      |IE      |0.62 %  |
    ## |1       |1          |1      |4      |IE      |0.62 %  |
    ## 
    ## 
    ## Table: IT's Population Holdings of the Top 15 Bundles of Payment Instruments
    ## 
    ## |account |crdit.dbit |crypto |instnt |COUNTRY |Pop    |
    ## |:-------|:----------|:------|:------|:-------|:------|
    ## |1       |1          |0      |1      |IT      |47.6 % |
    ## |1       |1          |0      |3      |IT      |15.5 % |
    ## |1       |1          |0      |4      |IT      |8.68 % |
    ## |1       |1          |0      |2      |IT      |7 %    |
    ## |0       |1          |0      |1      |IT      |5.38 % |
    ## |0       |1          |0      |3      |IT      |3.15 % |
    ## |1       |0          |0      |1      |IT      |2.83 % |
    ## |0       |1          |0      |2      |IT      |1.74 % |
    ## |1       |1          |1      |1      |IT      |1.49 % |
    ## |0       |1          |0      |4      |IT      |1.36 % |
    ## |1       |0          |0      |3      |IT      |1.18 % |
    ## |1       |0          |0      |2      |IT      |1 %    |
    ## |1       |0          |0      |4      |IT      |0.78 % |
    ## |0       |0          |0      |4      |IT      |0.54 % |
    ## |1       |1          |0      |999999 |IT      |0.33 % |
    ## 
    ## 
    ## Table: LT's Population Holdings of the Top 15 Bundles of Payment Instruments
    ## 
    ## |account |crdit.dbit |crypto |instnt |COUNTRY |Pop     |
    ## |:-------|:----------|:------|:------|:-------|:-------|
    ## |1       |1          |0      |1      |LT      |28.57 % |
    ## |1       |1          |0      |3      |LT      |19.96 % |
    ## |1       |1          |0      |4      |LT      |16.49 % |
    ## |1       |1          |0      |2      |LT      |8.41 %  |
    ## |0       |1          |0      |1      |LT      |6.01 %  |
    ## |1       |0          |0      |1      |LT      |5.34 %  |
    ## |1       |0          |0      |3      |LT      |2.94 %  |
    ## |0       |1          |0      |3      |LT      |2.27 %  |
    ## |0       |1          |0      |4      |LT      |1.74 %  |
    ## |1       |0          |0      |4      |LT      |1.74 %  |
    ## |0       |1          |0      |2      |LT      |1.4 %   |
    ## |1       |0          |0      |2      |LT      |1.4 %   |
    ## |1       |1          |1      |1      |LT      |1.4 %   |
    ## |1       |1          |1      |3      |LT      |0.47 %  |
    ## |1       |1          |0      |999999 |LT      |0.4 %   |
    ## 
    ## 
    ## Table: LU's Population Holdings of the Top 15 Bundles of Payment Instruments
    ## 
    ## |account |crdit.dbit |crypto |instnt |COUNTRY |Pop     |
    ## |:-------|:----------|:------|:------|:-------|:-------|
    ## |1       |1          |0      |1      |LU      |50.7 %  |
    ## |1       |1          |0      |2      |LU      |12.92 % |
    ## |1       |1          |0      |3      |LU      |12.13 % |
    ## |1       |1          |1      |1      |LU      |6.46 %  |
    ## |1       |1          |0      |4      |LU      |5.37 %  |
    ## |0       |1          |0      |1      |LU      |3.98 %  |
    ## |1       |0          |0      |1      |LU      |1.89 %  |
    ## |0       |1          |0      |2      |LU      |1.49 %  |
    ## |1       |1          |1      |3      |LU      |0.89 %  |
    ## |0       |1          |0      |3      |LU      |0.8 %   |
    ## |0       |1          |0      |4      |LU      |0.8 %   |
    ## |1       |1          |1      |4      |LU      |0.7 %   |
    ## |1       |0          |0      |3      |LU      |0.5 %   |
    ## |1       |1          |1      |2      |LU      |0.5 %   |
    ## |1       |0          |0      |2      |LU      |0.3 %   |
    ## 
    ## 
    ## Table: LV's Population Holdings of the Top 15 Bundles of Payment Instruments
    ## 
    ## |account |crdit.dbit |crypto |instnt |COUNTRY |Pop     |
    ## |:-------|:----------|:------|:------|:-------|:-------|
    ## |1       |1          |0      |1      |LV      |37.25 % |
    ## |1       |1          |0      |3      |LV      |20.34 % |
    ## |1       |1          |0      |4      |LV      |15.59 % |
    ## |1       |1          |0      |2      |LV      |9.41 %  |
    ## |1       |0          |0      |1      |LV      |3.64 %  |
    ## |0       |1          |0      |1      |LV      |2.02 %  |
    ## |1       |1          |1      |1      |LV      |1.82 %  |
    ## |1       |0          |0      |3      |LV      |1.72 %  |
    ## |0       |1          |0      |3      |LV      |1.62 %  |
    ## |1       |0          |0      |2      |LV      |1.21 %  |
    ## |1       |0          |0      |4      |LV      |1.01 %  |
    ## |0       |1          |0      |4      |LV      |0.81 %  |
    ## |1       |1          |1      |3      |LV      |0.81 %  |
    ## |1       |1          |0      |999999 |LV      |0.61 %  |
    ## |0       |1          |0      |2      |LV      |0.4 %   |
    ## 
    ## 
    ## Table: MT's Population Holdings of the Top 15 Bundles of Payment Instruments
    ## 
    ## |account |crdit.dbit |crypto |instnt |COUNTRY |Pop     |
    ## |:-------|:----------|:------|:------|:-------|:-------|
    ## |1       |1          |0      |1      |MT      |38.13 % |
    ## |1       |1          |0      |3      |MT      |10.87 % |
    ## |0       |1          |0      |1      |MT      |9.66 %  |
    ## |1       |1          |0      |4      |MT      |9.46 %  |
    ## |1       |1          |0      |2      |MT      |9.36 %  |
    ## |1       |0          |0      |1      |MT      |5.33 %  |
    ## |0       |1          |0      |2      |MT      |3.42 %  |
    ## |1       |1          |1      |1      |MT      |3.42 %  |
    ## |0       |1          |0      |4      |MT      |2.11 %  |
    ## |0       |1          |0      |3      |MT      |2.01 %  |
    ## |1       |0          |0      |2      |MT      |1.51 %  |
    ## |0       |0          |0      |4      |MT      |0.6 %   |
    ## |1       |0          |0      |3      |MT      |0.5 %   |
    ## |1       |0          |0      |4      |MT      |0.5 %   |
    ## |1       |1          |1      |3      |MT      |0.5 %   |
    ## 
    ## 
    ## Table: PT's Population Holdings of the Top 15 Bundles of Payment Instruments
    ## 
    ## |account |crdit.dbit |crypto |instnt |COUNTRY |Pop     |
    ## |:-------|:----------|:------|:------|:-------|:-------|
    ## |1       |1          |0      |1      |PT      |26.76 % |
    ## |1       |1          |0      |3      |PT      |11.92 % |
    ## |1       |1          |0      |2      |PT      |11.62 % |
    ## |1       |1          |0      |4      |PT      |7.9 %   |
    ## |1       |0          |0      |1      |PT      |7.14 %  |
    ## |0       |1          |0      |1      |PT      |5.99 %  |
    ## |0       |1          |0      |2      |PT      |4.68 %  |
    ## |1       |0          |0      |2      |PT      |4.02 %  |
    ## |1       |0          |0      |3      |PT      |3.57 %  |
    ## |0       |1          |0      |3      |PT      |3.47 %  |
    ## |1       |1          |1      |1      |PT      |3.32 %  |
    ## |0       |1          |0      |4      |PT      |3.17 %  |
    ## |1       |0          |0      |4      |PT      |2.16 %  |
    ## |1       |1          |1      |3      |PT      |1.01 %  |
    ## |1       |1          |1      |4      |PT      |0.7 %   |
    ## 
    ## 
    ## Table: SI's Population Holdings of the Top 15 Bundles of Payment Instruments
    ## 
    ## |account |crdit.dbit |crypto |instnt |COUNTRY |Pop     |
    ## |:-------|:----------|:------|:------|:-------|:-------|
    ## |1       |1          |0      |1      |SI      |23.43 % |
    ## |1       |1          |0      |4      |SI      |19.9 %  |
    ## |1       |1          |0      |3      |SI      |13.43 % |
    ## |1       |1          |0      |2      |SI      |11.92 % |
    ## |0       |1          |0      |1      |SI      |5.15 %  |
    ## |1       |0          |0      |1      |SI      |4.65 %  |
    ## |1       |1          |1      |1      |SI      |3.54 %  |
    ## |0       |1          |0      |2      |SI      |2.93 %  |
    ## |0       |1          |0      |3      |SI      |2.73 %  |
    ## |0       |1          |0      |4      |SI      |2.63 %  |
    ## |1       |0          |0      |2      |SI      |2.02 %  |
    ## |1       |1          |1      |4      |SI      |1.82 %  |
    ## |1       |0          |0      |3      |SI      |1.72 %  |
    ## |1       |1          |1      |2      |SI      |1.01 %  |
    ## |1       |0          |0      |4      |SI      |0.91 %  |
    ## 
    ## 
    ## Table: SK's Population Holdings of the Top 15 Bundles of Payment Instruments
    ## 
    ## |account |crdit.dbit |crypto |instnt |COUNTRY |Pop     |
    ## |:-------|:----------|:------|:------|:-------|:-------|
    ## |1       |1          |0      |1      |SK      |38.32 % |
    ## |1       |1          |0      |3      |SK      |19.67 % |
    ## |1       |1          |0      |4      |SK      |16.92 % |
    ## |1       |1          |0      |2      |SK      |8.75 %  |
    ## |1       |0          |0      |1      |SK      |3.61 %  |
    ## |1       |1          |1      |1      |SK      |1.72 %  |
    ## |1       |0          |0      |3      |SK      |1.52 %  |
    ## |1       |0          |0      |4      |SK      |1.19 %  |
    ## |0       |1          |0      |4      |SK      |1.07 %  |
    ## |1       |1          |1      |3      |SK      |1.07 %  |
    ## |0       |1          |0      |1      |SK      |0.99 %  |
    ## |0       |1          |0      |2      |SK      |0.99 %  |
    ## |1       |0          |0      |2      |SK      |0.99 %  |
    ## |0       |0          |0      |4      |SK      |0.9 %   |
    ## |0       |1          |0      |3      |SK      |0.53 %  |

# Average spending on various commodities using different instruments

``` r
# online payment

online<- space%>%
  select(starts_with(c("QB1_","QB3_", "QB4_")))#,QB3_n,QB4_n)
online[online>100 & !is.na(online)]<- NA

df_1<- online%>%
  select(ends_with("_1"))%>%
  mutate(QB1_1= case_when(
    QB1_1==1~   "Clothes_sportswear",
    QB1_1==2~   "Electronic_goods",
    QB1_1==3~   "Food_daily.supplies",
    QB1_1==4~   "Medicine",
    QB1_1==5~   "entertainment",
    QB1_1==6~   "Donations",
    QB1_1==7~   "Accommodation",
    QB1_1==8~   "Furniture",
    QB1_1==9~   "Tickets",
    QB1_1==10~  "Luxury_goods",
    QB1_1==11~  "Financial_products",
    QB1_1==12~  "Household_related",
    QB1_1==13~  "Other",
    QB1_1==999999~  "Don't know"

  ),
  QB4_1= case_when(
    QB4_1==1~"Card",
    QB4_1==3~"PayPal",
    QB4_1==4~   "Other.online",
    QB4_1==5~   "Direct",
    QB4_1==6~   "Credit",
    QB4_1==8~   "Loyalty",
    QB4_1==10~  "Crypto-assets",
    QB4_1==11~  "Other",
    QB4_1==999999~  "Don't know"

  ))

df_1.clean<- df_1%>%
  drop_na()

grouped_1<-df_1.clean%>%
  group_by(QB4_1, QB1_1)%>%
  summarise(mean= mean(QB3_1, na.rm = T))
```

    ## `summarise()` has grouped output by 'QB4_1'. You can override using the
    ## `.groups` argument.

``` r
dd<-pivot_wider(grouped_1, names_from = QB4_1, 
            values_from = mean)
kable(dd, digits = 2,  align = "l")
```

| QB1\_1               | Card  | Credit | Crypto-assets | Direct | Loyalty | Other | Other.online | PayPal |
| :------------------- | :---- | :----- | :------------ | :----- | :------ | :---- | :----------- | :----- |
| Accommodation        | 41.69 | 37.08  | NA            | 35.03  | NA      | 19.76 | 22.64        | 46.59  |
| Clothes\_sportswear  | 37.79 | 43.42  | NA            | 33.28  | 33.38   | 43.36 | 45.41        | 36.82  |
| Donations            | 26.31 | 26.36  | 62.50         | 22.10  | 30.00   | 15.03 | 16.67        | 28.42  |
| Electronic\_goods    | 38.77 | 34.23  | 30.00         | 40.56  | 38.52   | 39.50 | 33.48        | 35.32  |
| entertainment        | 28.09 | 28.86  | NA            | 22.36  | 26.61   | 18.98 | 28.44        | 24.90  |
| Financial\_products  | 47.08 | 42.48  | 17.90         | 35.10  | NA      | 73.75 | 30.25        | 43.55  |
| Food\_daily.supplies | 25.87 | 31.67  | 14.13         | 29.23  | 16.76   | 21.97 | 25.61        | 28.67  |
| Furniture            | 35.81 | 52.59  | NA            | 32.56  | 38.61   | 32.74 | 49.17        | 35.52  |
| Household\_related   | 35.29 | 47.69  | 60.80         | 43.72  | 23.99   | 34.72 | 33.79        | 34.95  |
| Luxury\_goods        | 53.29 | 48.63  | NA            | 75.00  | NA      | 13.18 | NA           | 42.96  |
| Medicine             | 27.50 | 31.33  | 5.96          | 34.51  | 21.60   | 23.38 | 32.33        | 32.58  |
| Other                | 26.81 | 34.34  | 77.00         | 32.71  | 20.68   | 19.37 | 24.35        | 28.54  |
| Tickets              | 34.36 | 42.91  | NA            | 52.92  | 51.50   | 28.67 | 30.17        | 26.01  |

The table above shows the average spending on various commodities using
different instruments. For example the average spending on on donations
using credit card is 26.31 This comprison has also be broken down by
demographics and country.

# Compare average spending on various commodities using different instruments by AGE

``` r
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
```

    ## 
    ## 
    ## Table: 64-69's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Direct |Loyalty |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:------|:-------|:-----|:------------|:------|
    ## |Accommodation       |46.93 |47.47  |9.57   |NA      |1.70  |2.80         |51.45  |
    ## |Clothes_sportswear  |43.13 |52.97  |NA     |7.20    |30.00 |17.90        |29.52  |
    ## |Donations           |20.00 |28.75  |12.00  |NA      |6.07  |25.00        |17.00  |
    ## |Electronic_goods    |44.74 |32.90  |58.50  |53.16   |NA    |40.47        |25.18  |
    ## |entertainment       |34.26 |26.05  |46.60  |50.00   |27.03 |NA           |34.83  |
    ## |Financial_products  |48.67 |68.72  |40.00  |NA      |NA    |NA           |25.00  |
    ## |Food_daily.supplies |29.56 |42.27  |32.88  |NA      |23.47 |22.34        |20.11  |
    ## |Furniture           |31.95 |67.09  |60.00  |NA      |30.00 |61.50        |25.50  |
    ## |Household_related   |30.92 |47.44  |91.33  |NA      |20.00 |NA           |35.84  |
    ## |Luxury_goods        |68.08 |NA     |NA     |NA      |NA    |NA           |81.00  |
    ## |Medicine            |34.50 |24.04  |23.12  |18.50   |14.94 |16.91        |39.77  |
    ## |Other               |31.53 |28.70  |38.98  |22.60   |13.91 |22.26        |27.76  |
    ## |Tickets             |26.82 |61.00  |98.00  |NA      |NA    |NA           |NA     |
    ## 
    ## 
    ## Table: 30-34's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Crypto-assets |Direct |Loyalty |Other  |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:-------------|:------|:-------|:------|:------------|:------|
    ## |Accommodation       |49.31 |31.05  |NA            |NA     |NA      |52.00  |NA           |60.00  |
    ## |Clothes_sportswear  |35.20 |34.93  |NA            |31.20  |17.02   |23.73  |47.92        |34.31  |
    ## |Donations           |17.75 |14.00  |NA            |10.00  |NA      |NA     |5.00         |21.00  |
    ## |Electronic_goods    |33.20 |21.74  |NA            |21.76  |100.00  |NA     |46.65        |39.09  |
    ## |entertainment       |29.45 |20.90  |NA            |20.34  |23.45   |20.03  |26.95        |20.54  |
    ## |Financial_products  |28.69 |23.32  |NA            |30.00  |NA      |87.50  |60.02        |23.38  |
    ## |Food_daily.supplies |21.33 |31.76  |20            |31.07  |12.54   |14.29  |28.82        |25.89  |
    ## |Furniture           |27.60 |12.50  |NA            |12.50  |NA      |100.00 |NA           |36.68  |
    ## |Household_related   |38.91 |30.50  |NA            |29.33  |NA      |17.48  |51.90        |41.55  |
    ## |Medicine            |24.57 |26.82  |NA            |45.55  |27.50   |16.35  |73.05        |20.30  |
    ## |Other               |21.83 |32.18  |NA            |25.81  |17.54   |22.42  |14.78        |31.00  |
    ## |Tickets             |26.30 |20.00  |NA            |NA     |NA      |NA     |22.67        |26.94  |
    ## |Luxury_goods        |NA    |NA     |NA            |NA     |NA      |NA     |NA           |70.00  |
    ## 
    ## 
    ## Table: 35-39's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Crypto-assets |Direct |Loyalty |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:-------------|:------|:-------|:-----|:------------|:------|
    ## |Accommodation       |38.12 |39.68  |NA            |35.30  |NA      |10.00 |4.00         |51.31  |
    ## |Clothes_sportswear  |38.11 |40.44  |NA            |40.80  |55.87   |42.51 |94.35        |34.01  |
    ## |Donations           |56.00 |7.50   |62.50         |13.75  |10.00   |NA    |NA           |47.00  |
    ## |Electronic_goods    |35.25 |35.12  |NA            |22.00  |NA      |22.00 |30.00        |42.17  |
    ## |entertainment       |27.70 |30.10  |NA            |28.54  |15.14   |25.25 |32.16        |16.97  |
    ## |Financial_products  |47.56 |35.00  |20.17         |33.28  |NA      |NA    |40.00        |31.54  |
    ## |Food_daily.supplies |25.07 |43.21  |2.40          |33.55  |19.07   |23.69 |36.75        |40.04  |
    ## |Furniture           |31.19 |38.00  |NA            |NA     |100.00  |NA    |NA           |42.83  |
    ## |Household_related   |36.66 |36.15  |NA            |34.14  |NA      |NA    |23.49        |10.00  |
    ## |Luxury_goods        |55.96 |11.00  |NA            |NA     |NA      |NA    |NA           |56.50  |
    ## |Medicine            |29.33 |32.63  |5.96          |42.56  |14.78   |29.19 |48.85        |35.55  |
    ## |Other               |26.03 |36.75  |NA            |41.10  |32.47   |17.37 |19.78        |31.95  |
    ## |Tickets             |38.54 |35.51  |NA            |59.00  |NA      |12.00 |12.00        |25.12  |
    ## 
    ## 
    ## Table: 18-24's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Crypto-assets |Direct |Loyalty |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:-------------|:------|:-------|:-----|:------------|:------|
    ## |Accommodation       |37.09 |23.70  |NA            |47.33  |NA      |14.88 |7.95         |47.20  |
    ## |Clothes_sportswear  |33.08 |41.10  |NA            |20.00  |26.85   |43.11 |35.75        |40.20  |
    ## |Donations           |18.25 |40.00  |NA            |NA     |NA      |NA    |NA           |2.00   |
    ## |Electronic_goods    |36.91 |36.19  |30.0          |NA     |12.49   |56.50 |24.00        |31.43  |
    ## |entertainment       |24.39 |26.05  |NA            |22.00  |16.26   |4.99  |26.84        |24.81  |
    ## |Financial_products  |34.99 |58.26  |NA            |35.00  |NA      |NA    |58.41        |22.40  |
    ## |Food_daily.supplies |21.76 |27.60  |NA            |29.64  |14.08   |17.59 |18.54        |20.04  |
    ## |Furniture           |24.64 |100.00 |NA            |NA     |39.48   |5.25  |56.00        |10.45  |
    ## |Household_related   |25.32 |47.34  |60.8          |NA     |NA      |NA    |24.92        |25.95  |
    ## |Luxury_goods        |53.33 |NA     |NA            |NA     |NA      |NA    |NA           |21.33  |
    ## |Medicine            |23.20 |14.25  |NA            |34.62  |NA      |19.80 |14.56        |20.94  |
    ## |Other               |19.81 |22.65  |NA            |34.68  |10.00   |16.32 |16.14        |20.12  |
    ## |Tickets             |28.92 |24.67  |NA            |37.50  |NA      |NA    |37.89        |36.15  |
    ## 
    ## 
    ## Table: 25-29's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Direct |Loyalty |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:------|:-------|:-----|:------------|:------|
    ## |Accommodation       |39.41 |20.25  |NA     |NA      |NA    |75.00        |44.67  |
    ## |Clothes_sportswear  |39.04 |38.98  |43.73  |NA      |35.40 |47.08        |32.31  |
    ## |Donations           |5.00  |10.00  |80.00  |NA      |NA    |NA           |26.25  |
    ## |Electronic_goods    |47.90 |NA     |NA     |28.29   |32.00 |26.99        |38.43  |
    ## |entertainment       |26.00 |31.15  |17.04  |40.50   |33.00 |14.97        |18.97  |
    ## |Financial_products  |39.71 |26.38  |32.39  |NA      |NA    |NA           |NA     |
    ## |Food_daily.supplies |23.58 |25.60  |32.37  |14.73   |21.24 |19.24        |29.38  |
    ## |Furniture           |39.79 |14.85  |8.00   |40.00   |NA    |30.00        |37.67  |
    ## |Household_related   |38.71 |NA     |16.12  |24.99   |23.02 |30.00        |56.00  |
    ## |Luxury_goods        |49.92 |NA     |80.00  |NA      |NA    |NA           |20.00  |
    ## |Medicine            |28.01 |15.40  |60.54  |30.80   |26.37 |14.14        |27.02  |
    ## |Other               |25.61 |30.33  |32.11  |NA      |13.47 |25.86        |15.02  |
    ## |Tickets             |48.63 |80.00  |61.33  |NA      |NA    |32.47        |15.00  |
    ## 
    ## 
    ## Table: 60-64's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Crypto-assets |Direct |Loyalty |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:-------------|:------|:-------|:-----|:------------|:------|
    ## |Accommodation       |44.63 |80.00  |NA            |95.20  |NA      |NA    |7.80         |50.00  |
    ## |Clothes_sportswear  |38.72 |39.46  |NA            |35.00  |22.00   |NA    |50.09        |38.00  |
    ## |Donations           |16.67 |NA     |NA            |32.00  |NA      |25.00 |NA           |5.00   |
    ## |Electronic_goods    |28.34 |30.29  |NA            |43.94  |NA      |NA    |25.88        |42.50  |
    ## |entertainment       |27.12 |39.10  |NA            |9.50   |18.61   |NA    |55.00        |32.08  |
    ## |Food_daily.supplies |27.60 |26.56  |NA            |18.83  |12.58   |24.01 |22.38        |35.31  |
    ## |Furniture           |37.37 |NA     |NA            |NA     |32.25   |NA    |NA           |9.85   |
    ## |Household_related   |33.14 |38.20  |NA            |46.66  |NA      |60.00 |25.88        |45.00  |
    ## |Luxury_goods        |83.00 |NA     |NA            |70.00  |NA      |NA    |NA           |NA     |
    ## |Medicine            |27.76 |38.29  |NA            |29.13  |NA      |30.25 |64.68        |34.09  |
    ## |Other               |29.61 |42.49  |77            |37.82  |24.53   |19.70 |24.43        |23.12  |
    ## |Tickets             |40.29 |48.00  |NA            |60.00  |NA      |40.00 |NA           |40.52  |
    ## |Financial_products  |NA    |25.21  |NA            |NA     |NA      |NA    |NA           |100.00 |
    ## 
    ## 
    ## Table: 55-59's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Direct |Loyalty |Other  |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:------|:-------|:------|:------------|:------|
    ## |Accommodation       |47.58 |16.08  |NA     |NA      |2.60   |30.60        |55.00  |
    ## |Clothes_sportswear  |35.84 |52.61  |35.88  |NA      |39.00  |29.92        |37.59  |
    ## |Donations           |28.58 |30.00  |16.75  |NA      |NA     |NA           |42.50  |
    ## |Electronic_goods    |41.95 |20.13  |65.94  |29.99   |27.00  |40.00        |44.39  |
    ## |entertainment       |32.68 |22.60  |21.50  |15.04   |2.00   |15.00        |30.90  |
    ## |Financial_products  |49.84 |26.38  |22.80  |NA      |100.00 |20.00        |42.00  |
    ## |Food_daily.supplies |29.99 |27.11  |11.46  |32.50   |23.00  |21.30        |29.09  |
    ## |Furniture           |44.88 |43.10  |NA     |42.90   |NA     |NA           |27.56  |
    ## |Household_related   |32.93 |54.04  |18.40  |23.50   |46.67  |20.00        |47.50  |
    ## |Luxury_goods        |59.00 |NA     |NA     |NA      |NA     |NA           |NA     |
    ## |Medicine            |24.76 |46.79  |7.48   |18.94   |25.82  |22.50        |38.38  |
    ## |Other               |26.38 |22.77  |19.00  |23.80   |21.28  |30.57        |32.99  |
    ## |Tickets             |30.82 |NA     |30.00  |NA      |NA     |45.50        |11.40  |
    ## 
    ## 
    ## Table: 50-54's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Direct |Loyalty |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:------|:-------|:-----|:------------|:------|
    ## |Accommodation       |40.28 |42.96  |20.30  |NA      |NA    |20.00        |53.27  |
    ## |Clothes_sportswear  |44.70 |37.00  |32.08  |39.12   |23.33 |96.00        |37.78  |
    ## |Donations           |8.00  |27.50  |NA     |NA      |NA    |NA           |15.00  |
    ## |Electronic_goods    |46.00 |33.49  |50.00  |24.24   |NA    |NA           |36.12  |
    ## |entertainment       |30.76 |36.20  |NA     |24.66   |5.00  |31.05        |23.19  |
    ## |Financial_products  |58.32 |41.36  |46.14  |NA      |NA    |10.00        |35.09  |
    ## |Food_daily.supplies |27.11 |30.07  |42.60  |16.49   |28.63 |17.54        |27.05  |
    ## |Furniture           |41.67 |59.96  |NA     |NA      |14.22 |NA           |63.20  |
    ## |Household_related   |34.27 |50.32  |95.25  |NA      |NA    |NA           |27.46  |
    ## |Medicine            |27.44 |22.79  |23.45  |NA      |22.34 |10.10        |34.73  |
    ## |Other               |28.99 |31.69  |38.14  |10.00   |24.34 |31.34        |22.77  |
    ## |Tickets             |44.29 |23.93  |60.00  |80.00   |10.00 |12.00        |33.33  |
    ## |Luxury_goods        |NA    |87.89  |NA     |NA      |NA    |NA           |14.00  |
    ## 
    ## 
    ## Table: 45-49's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Crypto-assets |Direct |Loyalty |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:-------------|:------|:-------|:-----|:------------|:------|
    ## |Accommodation       |45.35 |50.00  |NA            |NA     |NA      |NA    |NA           |45.57  |
    ## |Clothes_sportswear  |37.46 |45.42  |NA            |25.11  |30.00   |73.40 |49.45        |43.80  |
    ## |Donations           |30.92 |NA     |NA            |90.00  |50.00   |2.00  |NA           |32.67  |
    ## |Electronic_goods    |35.48 |8.89   |NA            |40.14  |NA      |41.49 |17.99        |26.47  |
    ## |entertainment       |24.65 |24.95  |NA            |16.87  |54.92   |18.00 |10.00        |20.15  |
    ## |Financial_products  |64.80 |75.00  |14.5          |35.30  |NA      |NA    |19.82        |100.00 |
    ## |Food_daily.supplies |28.94 |21.95  |NA            |26.68  |20.00   |16.54 |19.36        |30.08  |
    ## |Furniture           |34.75 |66.63  |NA            |10.00  |52.99   |NA    |NA           |31.34  |
    ## |Household_related   |34.83 |49.26  |NA            |32.73  |NA      |NA    |18.23        |24.87  |
    ## |Luxury_goods        |46.00 |NA     |NA            |NA     |NA      |13.18 |NA           |46.70  |
    ## |Medicine            |23.45 |47.24  |NA            |20.35  |5.00    |16.09 |20.98        |32.25  |
    ## |Other               |28.70 |37.57  |NA            |21.93  |15.20   |20.77 |24.24        |30.31  |
    ## |Tickets             |40.32 |40.75  |NA            |NA     |23.00   |NA    |21.60        |20.00  |
    ## 
    ## 
    ## Table: 75+'s online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Direct |Loyalty |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:------|:-------|:-----|:------------|:------|
    ## |Accommodation       |46.48 |40.90  |2.80   |NA      |NA    |NA           |NA     |
    ## |Clothes_sportswear  |54.20 |NA     |NA     |NA      |NA    |NA           |44.17  |
    ## |Donations           |40.00 |26.00  |10.00  |NA      |NA    |NA           |90.00  |
    ## |Electronic_goods    |47.71 |100.00 |42.20  |NA      |NA    |25.65        |15.00  |
    ## |entertainment       |37.70 |44.83  |19.50  |NA      |NA    |43.45        |49.12  |
    ## |Food_daily.supplies |28.08 |23.20  |49.00  |8.25    |24.66 |38.00        |29.69  |
    ## |Furniture           |29.39 |NA     |NA     |NA      |NA    |NA           |NA     |
    ## |Household_related   |59.75 |42.47  |NA     |NA      |58.00 |NA           |NA     |
    ## |Luxury_goods        |21.00 |NA     |NA     |NA      |NA    |NA           |NA     |
    ## |Medicine            |36.32 |91.83  |NA     |NA      |22.48 |15.00        |25.52  |
    ## |Other               |33.55 |58.85  |21.99  |20.00   |14.74 |NA           |28.94  |
    ## |Tickets             |36.00 |17.00  |50.00  |NA      |NA    |50.00        |10.00  |
    ## |Financial_products  |NA    |15.00  |25.00  |NA      |NA    |3.50         |NA     |
    ## 
    ## 
    ## Table: 70-74's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Crypto-assets |Direct |Loyalty |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:-------------|:------|:-------|:-----|:------------|:------|
    ## |Accommodation       |28.68 |33.00  |NA            |NA     |NA      |NA    |NA           |59.62  |
    ## |Clothes_sportswear  |39.92 |64.99  |NA            |NA     |80.00   |25.00 |NA           |32.54  |
    ## |Donations           |35.00 |34.00  |NA            |NA     |NA      |30.00 |NA           |35.00  |
    ## |Electronic_goods    |43.90 |47.50  |NA            |22.83  |50.00   |NA    |NA           |31.81  |
    ## |entertainment       |30.48 |32.47  |NA            |12.99  |32.65   |8.25  |NA           |38.77  |
    ## |Food_daily.supplies |29.94 |40.46  |20            |33.33  |5.32    |27.69 |43.11        |29.29  |
    ## |Furniture           |46.08 |35.00  |NA            |42.50  |NA      |NA    |NA           |31.63  |
    ## |Household_related   |47.88 |67.33  |NA            |65.00  |NA      |NA    |50.21        |33.90  |
    ## |Luxury_goods        |19.00 |47.00  |NA            |NA     |NA      |NA    |NA           |66.00  |
    ## |Medicine            |28.51 |9.15   |NA            |46.80  |NA      |35.56 |35.88        |53.93  |
    ## |Other               |27.82 |35.40  |NA            |32.35  |NA      |25.47 |38.25        |46.98  |
    ## |Tickets             |39.14 |90.00  |NA            |13.00  |NA      |30.00 |NA           |NA     |
    ## |Financial_products  |NA    |80.00  |NA            |36.00  |NA      |20.00 |NA           |NA     |
    ## 
    ## 
    ## Table: 40-44's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Direct |Loyalty |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:------|:-------|:-----|:------------|:------|
    ## |Accommodation       |44.46 |34.27  |NA     |NA      |NA    |25.00        |9.20   |
    ## |Clothes_sportswear  |35.02 |50.47  |27.79  |12.99   |68.05 |40.59        |37.20  |
    ## |Electronic_goods    |40.31 |54.00  |NA     |21.04   |47.75 |NA           |39.13  |
    ## |entertainment       |26.12 |37.08  |19.63  |38.66   |5.65  |31.66        |26.02  |
    ## |Financial_products  |62.70 |57.34  |11.30  |NA      |NA    |NA           |50.00  |
    ## |Food_daily.supplies |24.45 |34.60  |16.29  |25.43   |22.05 |39.69        |26.75  |
    ## |Furniture           |43.27 |49.97  |24.95  |12.99   |NA    |NA           |24.50  |
    ## |Household_related   |38.09 |57.82  |48.88  |NA      |30.13 |NA           |33.34  |
    ## |Medicine            |28.97 |22.62  |41.49  |25.00   |13.07 |20.08        |34.91  |
    ## |Other               |26.17 |37.02  |33.44  |11.61   |18.49 |31.60        |33.61  |
    ## |Tickets             |30.72 |67.50  |NA     |NA      |40.00 |NA           |25.75  |
    ## |Donations           |NA    |15.00  |NA     |NA      |NA    |20.00        |NA     |
    ## |Luxury_goods        |NA    |NA     |NA     |NA      |NA    |NA           |39.00  |

# compare average spending on various commodities using different instruments by country

``` r
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
```

    ## 
    ## 
    ## Table: AT's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Direct |Loyalty |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:------|:-------|:-----|:------------|:------|
    ## |Accommodation       |36.75 |NA     |20.30  |NA      |6.30  |NA           |24.92  |
    ## |Clothes_sportswear  |37.69 |58.70  |77.00  |29.15   |53.00 |39.44        |46.29  |
    ## |Donations           |13.00 |50.00  |NA     |NA      |10.00 |NA           |40.34  |
    ## |Electronic_goods    |35.60 |32.50  |37.51  |NA      |NA    |31.90        |33.00  |
    ## |entertainment       |31.54 |25.87  |35.38  |21.00   |20.00 |31.40        |24.91  |
    ## |Financial_products  |20.00 |56.05  |45.00  |NA      |NA    |3.50         |25.00  |
    ## |Food_daily.supplies |31.15 |26.70  |15.21  |25.00   |31.12 |27.59        |30.30  |
    ## |Furniture           |35.09 |100.00 |NA     |12.00   |NA    |NA           |50.50  |
    ## |Household_related   |26.04 |NA     |90.00  |NA      |35.00 |NA           |3.00   |
    ## |Luxury_goods        |81.90 |49.44  |NA     |NA      |NA    |NA           |81.00  |
    ## |Medicine            |28.81 |47.08  |22.11  |NA      |26.70 |32.55        |31.15  |
    ## |Other               |33.62 |46.61  |27.02  |8.88    |24.23 |44.00        |41.20  |
    ## |Tickets             |30.75 |29.50  |35.00  |NA      |NA    |NA           |21.23  |
    ## 
    ## 
    ## Table: BE's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Crypto-assets |Direct |Loyalty |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:-------------|:------|:-------|:-----|:------------|:------|
    ## |Accommodation       |9.70  |NA     |NA            |NA     |NA      |NA    |8.70         |51.93  |
    ## |Clothes_sportswear  |38.32 |56.59  |NA            |30.00  |7.20    |44.44 |41.00        |38.01  |
    ## |Donations           |48.17 |18.33  |25            |90.00  |NA      |NA    |NA           |11.75  |
    ## |Electronic_goods    |41.00 |22.66  |NA            |NA     |51.04   |NA    |30.51        |33.79  |
    ## |entertainment       |29.60 |30.31  |NA            |6.50   |35.28   |30.65 |49.82        |31.96  |
    ## |Financial_products  |60.49 |40.73  |NA            |48.40  |NA      |20.00 |60.02        |14.04  |
    ## |Food_daily.supplies |27.36 |13.69  |NA            |57.20  |NA      |23.97 |13.27        |29.87  |
    ## |Furniture           |32.85 |100.00 |NA            |NA     |NA      |8.45  |NA           |47.33  |
    ## |Household_related   |30.85 |NA     |NA            |33.98  |NA      |NA    |41.09        |30.33  |
    ## |Luxury_goods        |46.97 |NA     |NA            |70.00  |NA      |NA    |NA           |23.00  |
    ## |Medicine            |31.44 |29.66  |NA            |58.00  |32.39   |12.30 |48.38        |27.96  |
    ## |Other               |29.53 |39.53  |NA            |48.07  |36.11   |24.70 |36.29        |25.53  |
    ## |Tickets             |37.70 |29.40  |NA            |60.00  |NA      |NA    |52.47        |28.33  |
    ## 
    ## 
    ## Table: CY's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Direct |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:------|:-----|:------------|:------|
    ## |Accommodation       |61.50 |NA     |NA     |NA    |NA           |NA     |
    ## |Clothes_sportswear  |42.93 |20.00  |33.65  |NA    |25.49        |16.33  |
    ## |Donations           |32.50 |10.00  |NA     |NA    |NA           |NA     |
    ## |Electronic_goods    |33.07 |NA     |NA     |16.00 |NA           |NA     |
    ## |entertainment       |12.35 |10.00  |6.99   |28.13 |NA           |18.91  |
    ## |Financial_products  |45.25 |50.00  |37.80  |NA    |NA           |NA     |
    ## |Food_daily.supplies |19.64 |20.25  |33.00  |24.38 |NA           |42.50  |
    ## |Household_related   |36.98 |NA     |12.90  |NA    |NA           |NA     |
    ## |Luxury_goods        |2.50  |NA     |NA     |NA    |NA           |NA     |
    ## |Medicine            |27.58 |NA     |NA     |NA    |NA           |13.50  |
    ## |Other               |19.40 |57.02  |32.50  |23.98 |NA           |13.21  |
    ## |Tickets             |25.19 |30.00  |NA     |NA    |NA           |20.00  |
    ## 
    ## 
    ## Table: EE's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Direct |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:------|:-----|:------------|:------|
    ## |Accommodation       |26.20 |29.43  |NA     |6.00  |NA           |NA     |
    ## |Clothes_sportswear  |40.57 |46.03  |NA     |NA    |NA           |NA     |
    ## |Donations           |10.00 |18.25  |10.50  |NA    |NA           |NA     |
    ## |Electronic_goods    |47.10 |12.50  |NA     |NA    |NA           |30.50  |
    ## |entertainment       |26.91 |28.28  |14.55  |3.50  |7.50         |9.95   |
    ## |Food_daily.supplies |20.21 |32.14  |NA     |19.50 |20.47        |NA     |
    ## |Furniture           |40.00 |NA     |NA     |NA    |NA           |NA     |
    ## |Household_related   |51.48 |56.00  |NA     |50.00 |NA           |NA     |
    ## |Medicine            |24.06 |19.99  |NA     |NA    |15.00        |12.33  |
    ## |Other               |18.55 |34.87  |15.00  |17.13 |28.50        |47.43  |
    ## |Tickets             |43.00 |17.86  |NA     |NA    |34.30        |NA     |
    ## |Financial_products  |NA    |22.17  |5.30   |NA    |10.00        |NA     |
    ## 
    ## 
    ## Table: ES's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Crypto-assets |Direct |Loyalty |Other  |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:-------------|:------|:-------|:------|:------------|:------|
    ## |Accommodation       |46.05 |NA     |NA            |NA     |NA      |10.00  |75.00        |44.66  |
    ## |Clothes_sportswear  |38.27 |50.00  |NA            |32.08  |62.32   |28.43  |36.30        |30.02  |
    ## |Donations           |42.67 |7.50   |NA            |31.67  |10.00   |5.20   |25.00        |45.00  |
    ## |Electronic_goods    |29.79 |NA     |NA            |62.45  |29.66   |NA     |NA           |25.93  |
    ## |entertainment       |28.26 |55.54  |NA            |17.00  |29.65   |NA     |50.90        |27.78  |
    ## |Financial_products  |41.10 |26.00  |NA            |18.43  |NA      |NA     |NA           |30.00  |
    ## |Food_daily.supplies |27.29 |46.27  |20            |27.17  |18.02   |20.91  |10.94        |28.81  |
    ## |Furniture           |38.16 |NA     |NA            |24.95  |42.90   |100.00 |NA           |28.52  |
    ## |Household_related   |35.79 |70.00  |NA            |NA     |NA      |NA     |30.00        |34.23  |
    ## |Luxury_goods        |19.00 |NA     |NA            |NA     |NA      |13.18  |NA           |16.00  |
    ## |Medicine            |26.83 |NA     |NA            |16.19  |18.76   |37.49  |36.35        |37.00  |
    ## |Other               |24.73 |50.00  |NA            |25.91  |NA      |11.44  |27.38        |24.99  |
    ## |Tickets             |35.09 |NA     |NA            |NA     |NA      |NA     |12.00        |38.94  |
    ## 
    ## 
    ## Table: FI's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Direct |Loyalty |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:------|:-------|:-----|:------------|:------|
    ## |Accommodation       |33.57 |33.38  |53.53  |NA      |NA    |20.81        |NA     |
    ## |Clothes_sportswear  |37.90 |46.69  |58.30  |NA      |NA    |57.42        |26.71  |
    ## |Donations           |20.00 |19.33  |15.00  |NA      |10.00 |20.00        |NA     |
    ## |Electronic_goods    |51.30 |41.58  |NA     |NA      |NA    |39.94        |14.26  |
    ## |entertainment       |27.03 |36.96  |18.37  |33.88   |7.50  |28.24        |18.16  |
    ## |Financial_products  |42.11 |44.21  |20.99  |NA      |NA    |NA           |NA     |
    ## |Food_daily.supplies |22.55 |25.96  |25.66  |21.00   |27.32 |34.43        |36.00  |
    ## |Furniture           |32.93 |38.65  |NA     |NA      |5.25  |43.00        |NA     |
    ## |Household_related   |58.30 |53.01  |51.33  |NA      |20.00 |19.17        |65.20  |
    ## |Medicine            |32.49 |35.41  |35.36  |NA      |20.70 |47.85        |NA     |
    ## |Other               |27.79 |30.90  |31.93  |NA      |15.99 |33.64        |31.32  |
    ## |Tickets             |38.50 |83.75  |98.00  |NA      |NA    |1.00         |25.00  |
    ## 
    ## 
    ## Table: FR's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Crypto-assets |Direct |Loyalty |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:-------------|:------|:-------|:-----|:------------|:------|
    ## |Accommodation       |53.66 |12.16  |NA            |30.00  |NA      |NA    |NA           |82.16  |
    ## |Clothes_sportswear  |40.43 |97.50  |NA            |38.00  |19.99   |55.90 |50.00        |41.59  |
    ## |Donations           |45.16 |10.00  |NA            |5.00   |NA      |NA    |5.00         |37.50  |
    ## |Electronic_goods    |41.22 |NA     |30.00         |45.15  |48.59   |23.50 |23.00        |28.01  |
    ## |entertainment       |30.90 |5.00   |NA            |17.86  |26.27   |6.74  |25.49        |23.22  |
    ## |Financial_products  |55.00 |100.00 |10.00         |NA     |NA      |NA    |NA           |53.77  |
    ## |Food_daily.supplies |30.00 |36.20  |2.40          |26.16  |15.69   |16.83 |35.12        |32.34  |
    ## |Furniture           |35.37 |75.00  |NA            |43.33  |46.73   |NA    |NA           |30.55  |
    ## |Household_related   |34.42 |18.10  |NA            |24.99  |29.99   |58.00 |35.00        |39.87  |
    ## |Luxury_goods        |61.59 |NA     |NA            |NA     |NA      |NA    |NA           |46.70  |
    ## |Medicine            |30.48 |8.67   |5.96          |22.13  |13.00   |18.96 |6.80         |33.23  |
    ## |Other               |29.70 |20.47  |NA            |30.00  |22.30   |20.55 |32.33        |29.54  |
    ## |Tickets             |36.03 |20.00  |NA            |81.00  |NA      |NA    |NA           |27.97  |
    ## 
    ## 
    ## Table: GR's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Crypto-assets |Direct |Loyalty |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:-------------|:------|:-------|:-----|:------------|:------|
    ## |Accommodation       |34.57 |NA     |NA            |NA     |NA      |NA    |NA           |NA     |
    ## |Clothes_sportswear  |36.99 |NA     |NA            |22.30  |NA      |24.50 |61.33        |19.71  |
    ## |Donations           |1.00  |10.00  |NA            |NA     |NA      |26.00 |NA           |NA     |
    ## |Electronic_goods    |34.99 |NA     |NA            |100.00 |NA      |34.00 |35.00        |39.85  |
    ## |entertainment       |22.64 |20.00  |NA            |10.83  |5.05    |21.00 |20.50        |7.73   |
    ## |Financial_products  |22.60 |NA     |0.5           |28.27  |NA      |NA    |NA           |55.18  |
    ## |Food_daily.supplies |25.45 |70.22  |NA            |34.85  |2.43    |20.64 |22.90        |22.16  |
    ## |Furniture           |30.09 |NA     |NA            |NA     |NA      |NA    |NA           |20.00  |
    ## |Household_related   |32.12 |NA     |60.8          |75.00  |NA      |26.82 |NA           |14.81  |
    ## |Medicine            |19.81 |24.90  |NA            |NA     |NA      |19.94 |10.10        |18.17  |
    ## |Other               |21.54 |65.89  |NA            |40.62  |NA      |9.13  |8.67         |23.00  |
    ## |Tickets             |20.12 |NA     |NA            |NA     |NA      |NA    |42.00        |5.00   |
    ## |Luxury_goods        |NA    |47.00  |NA            |NA     |NA      |NA    |NA           |100.00 |
    ## 
    ## 
    ## Table: IE's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Crypto-assets |Direct |Loyalty |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:-------------|:------|:-------|:-----|:------------|:------|
    ## |Accommodation       |35.20 |45.00  |NA            |56.00  |NA      |NA    |NA           |32.00  |
    ## |Clothes_sportswear  |41.36 |45.84  |NA            |46.00  |12.99   |50.00 |NA           |43.93  |
    ## |Donations           |15.25 |NA     |NA            |NA     |NA      |NA    |NA           |42.00  |
    ## |Electronic_goods    |44.49 |NA     |NA            |25.49  |NA      |NA    |NA           |39.99  |
    ## |entertainment       |23.34 |8.99   |NA            |16.07  |25.00   |65.00 |NA           |24.29  |
    ## |Financial_products  |33.38 |NA     |NA            |59.09  |NA      |NA    |NA           |62.29  |
    ## |Food_daily.supplies |27.13 |52.76  |20            |38.83  |50.30   |25.10 |33.52        |37.60  |
    ## |Furniture           |40.02 |NA     |NA            |12.50  |NA      |NA    |NA           |65.50  |
    ## |Household_related   |39.43 |58.00  |NA            |40.00  |NA      |25.00 |NA           |25.95  |
    ## |Luxury_goods        |18.00 |NA     |NA            |NA     |NA      |NA    |NA           |90.00  |
    ## |Medicine            |32.53 |32.88  |NA            |71.00  |25.00   |15.00 |NA           |38.48  |
    ## |Other               |31.21 |34.83  |NA            |39.84  |10.00   |30.83 |30.00        |27.01  |
    ## |Tickets             |46.00 |90.00  |NA            |NA     |NA      |NA    |32.47        |27.50  |
    ## 
    ## 
    ## Table: IT's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Direct |Loyalty |Other  |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:------|:-------|:------|:------------|:------|
    ## |Accommodation       |47.71 |85.00  |NA     |NA      |3.50   |NA           |37.52  |
    ## |Clothes_sportswear  |36.65 |39.74  |NA     |28.27   |25.04  |28.00        |35.56  |
    ## |Donations           |12.50 |54.00  |NA     |50.00   |NA     |NA           |11.75  |
    ## |Electronic_goods    |41.38 |40.86  |NA     |40.00   |41.49  |NA           |48.25  |
    ## |entertainment       |26.69 |30.57  |25.52  |20.87   |31.50  |21.96        |23.45  |
    ## |Financial_products  |58.04 |87.00  |57.14  |NA      |100.00 |NA           |30.00  |
    ## |Food_daily.supplies |27.05 |56.00  |NA     |10.63   |28.69  |18.20        |26.37  |
    ## |Furniture           |31.19 |75.00  |NA     |34.75   |NA     |NA           |70.00  |
    ## |Household_related   |24.53 |35.00  |NA     |11.99   |2.10   |NA           |20.12  |
    ## |Luxury_goods        |20.00 |NA     |NA     |NA      |NA     |NA           |39.25  |
    ## |Medicine            |23.41 |51.42  |NA     |16.75   |20.00  |16.00        |31.85  |
    ## |Other               |23.96 |37.59  |36.06  |18.71   |19.45  |17.77        |30.39  |
    ## |Tickets             |35.34 |NA     |NA     |NA      |45.00  |NA           |20.36  |
    ## 
    ## 
    ## Table: LT's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Direct |Loyalty |Other  |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:------|:-------|:------|:------------|:------|
    ## |Accommodation       |30.10 |NA     |4.75   |NA      |20.85  |7.80         |NA     |
    ## |Clothes_sportswear  |33.64 |37.30  |8.30   |NA      |80.00  |70.88        |43.25  |
    ## |Donations           |5.00  |NA     |20.00  |NA      |14.00  |NA           |NA     |
    ## |Electronic_goods    |29.14 |36.27  |22.62  |NA      |100.00 |NA           |33.38  |
    ## |entertainment       |21.27 |24.67  |25.16  |22      |NA     |19.99        |35.00  |
    ## |Financial_products  |57.00 |69.67  |53.69  |NA      |100.00 |20.00        |54.00  |
    ## |Food_daily.supplies |22.29 |39.55  |30.99  |NA      |18.44  |34.50        |24.28  |
    ## |Furniture           |46.78 |38.00  |NA     |NA      |NA     |NA           |51.50  |
    ## |Household_related   |28.84 |36.61  |34.25  |NA      |NA     |23.49        |56.55  |
    ## |Medicine            |21.51 |27.68  |46.63  |NA      |11.58  |33.30        |46.77  |
    ## |Other               |26.00 |27.99  |26.25  |20      |29.56  |NA           |17.28  |
    ## |Tickets             |48.50 |100.00 |26.00  |80      |10.00  |NA           |30.00  |
    ## 
    ## 
    ## Table: LU's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Crypto-assets |Direct |Loyalty |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:-------------|:------|:-------|:-----|:------------|:------|
    ## |Accommodation       |30.00 |80.00  |NA            |NA     |NA      |NA    |NA           |50.00  |
    ## |Clothes_sportswear  |39.60 |42.67  |NA            |NA     |58      |NA    |NA           |54.53  |
    ## |Electronic_goods    |50.18 |51.00  |NA            |14.27  |NA      |NA    |NA           |53.14  |
    ## |entertainment       |39.14 |30.48  |NA            |79.90  |NA      |22.00 |NA           |35.12  |
    ## |Food_daily.supplies |26.66 |39.61  |NA            |10.30  |NA      |8.53  |34.66        |28.82  |
    ## |Furniture           |80.21 |78.91  |NA            |NA     |40      |NA    |NA           |NA     |
    ## |Household_related   |23.66 |65.41  |NA            |NA     |NA      |NA    |NA           |77.25  |
    ## |Luxury_goods        |76.28 |NA     |NA            |NA     |NA      |NA    |NA           |22.67  |
    ## |Medicine            |27.40 |25.00  |NA            |NA     |NA      |NA    |NA           |38.89  |
    ## |Other               |35.03 |49.09  |77            |53.78  |NA      |35.69 |14.30        |66.77  |
    ## |Tickets             |6.06  |32.00  |NA            |19.00  |NA      |NA    |50.00        |8.00   |
    ## |Donations           |NA    |50.00  |100           |NA     |NA      |NA    |NA           |52.00  |
    ## |Financial_products  |NA    |2.05   |NA            |NA     |NA      |NA    |NA           |NA     |
    ## 
    ## 
    ## Table: LV's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Direct |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:------|:-----|:------------|:------|
    ## |Accommodation       |14.85 |32.30  |1.89   |NA    |14.50        |NA     |
    ## |Clothes_sportswear  |21.79 |35.49  |21.89  |5.93  |NA           |33.63  |
    ## |Donations           |11.00 |NA     |10.00  |NA    |NA           |NA     |
    ## |Electronic_goods    |56.33 |38.40  |28.99  |NA    |25.00        |NA     |
    ## |entertainment       |41.84 |15.54  |14.17  |NA    |50.00        |NA     |
    ## |Financial_products  |36.95 |19.55  |4.00   |NA    |NA           |NA     |
    ## |Food_daily.supplies |19.66 |28.76  |44.33  |10.25 |28.91        |15.60  |
    ## |Furniture           |40.50 |35.75  |NA     |NA    |NA           |NA     |
    ## |Household_related   |38.97 |40.00  |25.00  |25.00 |NA           |8.01   |
    ## |Medicine            |15.73 |35.45  |55.72  |6.20  |NA           |72.95  |
    ## |Other               |30.78 |27.35  |11.64  |12.07 |3.41         |3.00   |
    ## |Tickets             |NA    |15.04  |56.00  |30.00 |NA           |NA     |
    ## 
    ## 
    ## Table: MT's online Payment
    ## 
    ## |QB1_1               |Card   |Credit |Crypto-assets |Direct |Other |Other.online |PayPal |
    ## |:-------------------|:------|:------|:-------------|:------|:-----|:------------|:------|
    ## |Accommodation       |100.00 |NA     |NA            |NA     |NA    |NA           |NA     |
    ## |Clothes_sportswear  |38.00  |NA     |NA            |NA     |65.00 |NA           |36.07  |
    ## |Electronic_goods    |32.71  |NA     |NA            |NA     |NA    |25.00        |42.15  |
    ## |entertainment       |46.25  |NA     |NA            |NA     |26.98 |NA           |26.39  |
    ## |Financial_products  |20.00  |NA     |24            |NA     |NA    |NA           |NA     |
    ## |Food_daily.supplies |30.63  |4.5    |NA            |NA     |12.45 |5.10         |14.72  |
    ## |Furniture           |13.55  |NA     |NA            |NA     |NA    |NA           |10.00  |
    ## |Household_related   |26.00  |NA     |NA            |25     |15.00 |25.88        |40.52  |
    ## |Medicine            |42.17  |NA     |NA            |65     |NA    |6.50         |29.33  |
    ## |Other               |33.27  |9.0    |NA            |NA     |22.65 |14.54        |14.40  |
    ## |Tickets             |70.00  |NA     |NA            |NA     |NA    |NA           |NA     |
    ## 
    ## 
    ## Table: PT's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Crypto-assets |Direct |Loyalty |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:-------------|:------|:-------|:-----|:------------|:------|
    ## |Accommodation       |60.60 |20.00  |NA            |NA     |NA      |69.00 |NA           |47.33  |
    ## |Clothes_sportswear  |31.43 |40.49  |NA            |13.78  |29.99   |70.00 |27.80        |25.16  |
    ## |Electronic_goods    |27.24 |25.45  |NA            |58.50  |NA      |NA    |NA           |31.40  |
    ## |entertainment       |17.54 |21.25  |NA            |NA     |8.50    |NA    |10.00        |19.58  |
    ## |Financial_products  |73.33 |25.00  |27.5          |25.00  |NA      |75.00 |NA           |100.00 |
    ## |Food_daily.supplies |21.94 |22.50  |NA            |7.88   |40.00   |14.25 |23.82        |21.51  |
    ## |Furniture           |40.90 |NA     |NA            |40.00  |NA      |NA    |61.50        |7.29   |
    ## |Household_related   |42.88 |54.52  |NA            |57.75  |NA      |NA    |20.00        |NA     |
    ## |Luxury_goods        |74.00 |NA     |NA            |80.00  |NA      |NA    |NA           |NA     |
    ## |Medicine            |32.68 |78.66  |NA            |NA     |35.00   |11.17 |34.99        |37.33  |
    ## |Other               |25.25 |27.34  |NA            |30.21  |NA      |14.46 |19.92        |18.49  |
    ## |Tickets             |46.33 |35.00  |NA            |NA     |NA      |NA    |18.63        |10.00  |
    ## |Donations           |NA    |NA     |NA            |16.67  |NA      |NA    |NA           |NA     |
    ## 
    ## 
    ## Table: SI's online Payment
    ## 
    ## |QB1_1               |Card   |Credit |Direct |Loyalty |Other  |Other.online |PayPal |
    ## |:-------------------|:------|:------|:------|:-------|:------|:------------|:------|
    ## |Accommodation       |15.10  |NA     |NA     |NA      |NA     |NA           |NA     |
    ## |Clothes_sportswear  |33.34  |NA     |NA     |NA      |NA     |NA           |56.20  |
    ## |Electronic_goods    |58.30  |NA     |NA     |35.00   |NA     |NA           |9.05   |
    ## |entertainment       |45.67  |NA     |NA     |97.76   |NA     |36.00        |50.00  |
    ## |Financial_products  |2.94   |NA     |45.00  |NA      |NA     |NA           |23.38  |
    ## |Food_daily.supplies |21.64  |24.12  |22.48  |NA      |25.98  |NA           |18.81  |
    ## |Furniture           |56.00  |NA     |26.50  |NA      |NA     |NA           |NA     |
    ## |Household_related   |22.15  |NA     |100.00 |NA      |100.00 |NA           |NA     |
    ## |Luxury_goods        |100.00 |NA     |NA     |NA      |NA     |NA           |NA     |
    ## |Medicine            |40.34  |3.75   |16.55  |NA      |NA     |NA           |14.30  |
    ## |Other               |27.58  |21.04  |14.58  |NA      |19.36  |20.55        |25.00  |
    ## 
    ## 
    ## Table: SK's online Payment
    ## 
    ## |QB1_1               |Card  |Credit |Direct |Loyalty |Other |Other.online |PayPal |
    ## |:-------------------|:-----|:------|:------|:-------|:-----|:------------|:------|
    ## |Accommodation       |33.74 |43.00  |NA     |NA      |35.00 |NA           |NA     |
    ## |Clothes_sportswear  |34.32 |25.69  |NA     |NA      |44.74 |36.84        |28.04  |
    ## |Donations           |27.50 |NA     |5.00   |NA      |NA    |NA           |10.00  |
    ## |Electronic_goods    |30.96 |26.70  |NA     |NA      |40.83 |41.00        |22.50  |
    ## |entertainment       |22.54 |16.15  |5.99   |NA      |5.20  |28.75        |24.37  |
    ## |Financial_products  |32.75 |34.76  |33.57  |NA      |NA    |39.41        |NA     |
    ## |Food_daily.supplies |20.15 |22.62  |25.00  |16.29   |21.06 |28.38        |32.07  |
    ## |Furniture           |24.70 |32.49  |NA     |NA      |25.00 |NA           |28.38  |
    ## |Household_related   |36.44 |19.07  |25.00  |NA      |39.97 |47.42        |NA     |
    ## |Luxury_goods        |80.00 |NA     |NA     |NA      |NA    |NA           |NA     |
    ## |Medicine            |26.89 |20.09  |35.78  |5.00    |26.22 |29.18        |23.83  |
    ## |Other               |21.42 |27.70  |22.50  |NA      |23.68 |17.47        |25.93  |
    ## |Tickets             |16.80 |23.50  |30.00  |23.00   |21.00 |NA           |20.00  |

# Adoption Rate

``` r
#One chart for adoption Rate 

adoption<- space%>%
  select(QQ1A_1,QQ1A_2, QQ1A_3, QQ1A_4, QQ1A_5)
names(adoption)<- c("Cash", "Credit.debit","Crypto","None","Not.Sure")

ratios <- apply(data.matrix(adoption)[,], 2, function(x) length(which(x == 1)) / nrow(adoption))

ff<- data.frame(#instruments=c("Cash", "Credit.debit","Crypto","None","Not.Sure"),
                Adoption.Rate= ratios)
kable(ff, align = "l", caption = "Adoption Rate")
```

|              | Adoption.Rate |
| :----------- | :------------ |
| Cash         | 0.8926721     |
| Credit.debit | 0.9201328     |
| Crypto       | 0.0443092     |
| None         | 0.0097319     |
| Not.Sure     | 0.0005029     |

Adoption Rate

The table above shows how people have adopted various instruments. over
89 % of the population have a payment account. The comparison have also
been broken down by demographics as shown below.

# comapare adoption by country

``` r
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
```

    ## 
    ## 
    ## Table: AT's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.9284278     |
    ## |Credit.debit    |0.8898271     |
    ## |Crypto          |0.0695617     |
    ## |None            |0.0064335     |
    ## |Not.Sure        |0.0004021     |
    ## 
    ## 
    ## Table: BE's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8988235     |
    ## |Credit.debit    |0.9149580     |
    ## |Crypto          |0.0366387     |
    ## |None            |0.0067227     |
    ## |Not.Sure        |0.0000000     |
    ## 
    ## 
    ## Table: CY's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8110236     |
    ## |Credit.debit    |0.9616142     |
    ## |Crypto          |0.0718504     |
    ## |None            |0.0068898     |
    ## |Not.Sure        |0.0000000     |
    ## 
    ## 
    ## Table: EE's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.9276228     |
    ## |Credit.debit    |0.9442231     |
    ## |Crypto          |0.0484728     |
    ## |None            |0.0006640     |
    ## |Not.Sure        |0.0000000     |
    ## 
    ## 
    ## Table: ES's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8911398     |
    ## |Credit.debit    |0.9387801     |
    ## |Crypto          |0.0414069     |
    ## |None            |0.0073464     |
    ## |Not.Sure        |0.0011131     |
    ## 
    ## 
    ## Table: FI's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.9781746     |
    ## |Credit.debit    |0.9751984     |
    ## |Crypto          |0.0548942     |
    ## |None            |0.0029762     |
    ## |Not.Sure        |0.0000000     |
    ## 
    ## 
    ## Table: FR's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8998026     |
    ## |Credit.debit    |0.9157618     |
    ## |Crypto          |0.0294505     |
    ## |None            |0.0097071     |
    ## |Not.Sure        |0.0018098     |
    ## 
    ## 
    ## Table: GR's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8437667     |
    ## |Credit.debit    |0.9464955     |
    ## |Crypto          |0.0481541     |
    ## |None            |0.0171215     |
    ## |Not.Sure        |0.0000000     |
    ## 
    ## 
    ## Table: IE's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8409912     |
    ## |Credit.debit    |0.9143005     |
    ## |Crypto          |0.0562726     |
    ## |None            |0.0129066     |
    ## |Not.Sure        |0.0000000     |
    ## 
    ## 
    ## Table: IT's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8710685     |
    ## |Credit.debit    |0.9290654     |
    ## |Crypto          |0.0229757     |
    ## |None            |0.0140531     |
    ## |Not.Sure        |0.0004461     |
    ## 
    ## 
    ## Table: LT's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8778371     |
    ## |Credit.debit    |0.8791722     |
    ## |Crypto          |0.0280374     |
    ## |None            |0.0046729     |
    ## |Not.Sure        |0.0000000     |
    ## 
    ## 
    ## Table: LU's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.9274354     |
    ## |Credit.debit    |0.9691849     |
    ## |Crypto          |0.0874751     |
    ## |None            |0.0009940     |
    ## |Not.Sure        |0.0000000     |
    ## 
    ## 
    ## Table: LV's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.9433198     |
    ## |Credit.debit    |0.9159919     |
    ## |Crypto          |0.0374494     |
    ## |None            |0.0050607     |
    ## |Not.Sure        |0.0010121     |
    ## 
    ## 
    ## Table: MT's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8088531     |
    ## |Credit.debit    |0.9064386     |
    ## |Crypto          |0.0553320     |
    ## |None            |0.0160966     |
    ## |Not.Sure        |0.0000000     |
    ## 
    ## 
    ## Table: PT's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8118712     |
    ## |Credit.debit    |0.8148893     |
    ## |Crypto          |0.0588531     |
    ## |None            |0.0211268     |
    ## |Not.Sure        |0.0000000     |
    ## 
    ## 
    ## Table: SI's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8626263     |
    ## |Credit.debit    |0.9050505     |
    ## |Crypto          |0.0767677     |
    ## |None            |0.0000000     |
    ## |Not.Sure        |0.0000000     |
    ## 
    ## 
    ## Table: SK's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.9449692     |
    ## |Credit.debit    |0.9055441     |
    ## |Crypto          |0.0353183     |
    ## |None            |0.0209446     |
    ## |Not.Sure        |0.0000000     |

# comapare adoption by AGE

``` r
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
```

    ## 
    ## 
    ## Table: 64-69's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.9008161     |
    ## |Credit.debit    |0.9255074     |
    ## |Crypto          |0.0169492     |
    ## |None            |0.0087884     |
    ## |Not.Sure        |0.0004185     |
    ## 
    ## 
    ## Table: 55-59's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.9140401     |
    ## |Credit.debit    |0.9177650     |
    ## |Crypto          |0.0223496     |
    ## |None            |0.0120344     |
    ## |Not.Sure        |0.0002865     |
    ## 
    ## 
    ## Table: 35-39's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8927277     |
    ## |Credit.debit    |0.9318676     |
    ## |Crypto          |0.0681324     |
    ## |None            |0.0082145     |
    ## |Not.Sure        |0.0004832     |
    ## 
    ## 
    ## Table: 30-34's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8998420     |
    ## |Credit.debit    |0.9178515     |
    ## |Crypto          |0.0875197     |
    ## |None            |0.0113744     |
    ## |Not.Sure        |0.0003160     |
    ## 
    ## 
    ## Table: 60-64's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.9171410     |
    ## |Credit.debit    |0.9237824     |
    ## |Crypto          |0.0173941     |
    ## |None            |0.0094877     |
    ## |Not.Sure        |0.0000000     |
    ## 
    ## 
    ## Table: 50-54's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.9057991     |
    ## |Credit.debit    |0.9273074     |
    ## |Crypto          |0.0296760     |
    ## |None            |0.0078955     |
    ## |Not.Sure        |0.0000000     |
    ## 
    ## 
    ## Table: 18-24's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8018092     |
    ## |Credit.debit    |0.9026864     |
    ## |Crypto          |0.0756579     |
    ## |None            |0.0150768     |
    ## |Not.Sure        |0.0027412     |
    ## 
    ## 
    ## Table: 45-49's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.9175747     |
    ## |Credit.debit    |0.9240011     |
    ## |Crypto          |0.0413523     |
    ## |None            |0.0078234     |
    ## |Not.Sure        |0.0000000     |
    ## 
    ## 
    ## Table: 40-44's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.9033816     |
    ## |Credit.debit    |0.9112319     |
    ## |Crypto          |0.0570652     |
    ## |None            |0.0093599     |
    ## |Not.Sure        |0.0003019     |
    ## 
    ## 
    ## Table: 75+'s Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8884181     |
    ## |Credit.debit    |0.9173729     |
    ## |Crypto          |0.0134181     |
    ## |None            |0.0098870     |
    ## |Not.Sure        |0.0007062     |
    ## 
    ## 
    ## Table: 25-29's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8722152     |
    ## |Credit.debit    |0.9041614     |
    ## |Crypto          |0.0899538     |
    ## |None            |0.0088272     |
    ## |Not.Sure        |0.0000000     |
    ## 
    ## 
    ## Table: 70-74's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8925265     |
    ## |Credit.debit    |0.9282407     |
    ## |Crypto          |0.0112434     |
    ## |None            |0.0082672     |
    ## |Not.Sure        |0.0006614     |

# comapare adoption by Gender

``` r
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
```

    ## 
    ## 
    ## Table: Female's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8872720     |
    ## |Credit.debit    |0.9180248     |
    ## |Crypto          |0.0292976     |
    ## |None            |0.0109139     |
    ## |Not.Sure        |0.0005336     |
    ## 
    ## 
    ## Table: Male's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8988123     |
    ## |Credit.debit    |0.9225135     |
    ## |Crypto          |0.0604824     |
    ## |None            |0.0084236     |
    ## |Not.Sure        |0.0004709     |
    ## 
    ## 
    ## Table: Other, non-binary's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.7297297     |
    ## |Credit.debit    |0.8648649     |
    ## |Crypto          |0.0540541     |
    ## |None            |0.0270270     |
    ## |Not.Sure        |0.0000000     |

# comapare adoption by Activity

``` r
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
```

    ## 
    ## 
    ## Table: Without a professional activity or student's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8760717     |
    ## |Credit.debit    |0.9167966     |
    ## |Crypto          |0.0250714     |
    ## |None            |0.0140296     |
    ## |Not.Sure        |0.0006495     |
    ## 
    ## 
    ## Table: Employee's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.9047399     |
    ## |Credit.debit    |0.9245196     |
    ## |Crypto          |0.0567256     |
    ## |None            |0.0064053     |
    ## |Not.Sure        |0.0005124     |
    ## 
    ## 
    ## Table: Self-employed's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8966397     |
    ## |Credit.debit    |0.9147171     |
    ## |Crypto          |0.0561463     |
    ## |None            |0.0089324     |
    ## |Not.Sure        |0.0000000     |
    ## 
    ## 
    ## Table: NA's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |NaN           |
    ## |Credit.debit    |NaN           |
    ## |Crypto          |NaN           |
    ## |None            |NaN           |
    ## |Not.Sure        |NaN           |

# comapare adoption by EDUCutation

``` r
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
```

    ## 
    ## 
    ## Table: Upper/post-secondary education's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8897723     |
    ## |Credit.debit    |0.9152138     |
    ## |Crypto          |0.0417400     |
    ## |None            |0.0110739     |
    ## |Not.Sure        |0.0006247     |
    ## 
    ## 
    ## Table: University/PhD/research's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.9052680     |
    ## |Credit.debit    |0.9325984     |
    ## |Crypto          |0.0560470     |
    ## |None            |0.0066015     |
    ## |Not.Sure        |0.0002641     |
    ## 
    ## 
    ## Table: Primary/lower secondary education's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8727325     |
    ## |Credit.debit    |0.9054421     |
    ## |Crypto          |0.0254249     |
    ## |None            |0.0131410     |
    ## |Not.Sure        |0.0007142     |
    ## 
    ## 
    ## Table: NA's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |NaN           |
    ## |Credit.debit    |NaN           |
    ## |Crypto          |NaN           |
    ## |None            |NaN           |
    ## |Not.Sure        |NaN           |

# comapare adoption by INCOME

``` r
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
```

    ## 
    ## 
    ## Table: NA's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |NaN           |
    ## |Credit.debit    |NaN           |
    ## |Crypto          |NaN           |
    ## |None            |NaN           |
    ## |Not.Sure        |NaN           |
    ## 
    ## 
    ## Table: Between EUR 2,501 and EUR 4,000's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.9138765     |
    ## |Credit.debit    |0.9417936     |
    ## |Crypto          |0.0453426     |
    ## |None            |0.0036493     |
    ## |Not.Sure        |0.0002737     |
    ## 
    ## 
    ## Table: More than EUR 4,000's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.9355072     |
    ## |Credit.debit    |0.9605072     |
    ## |Crypto          |0.0717391     |
    ## |None            |0.0028986     |
    ## |Not.Sure        |0.0001812     |
    ## 
    ## 
    ## Table: Between EUR 751 and EUR 1,500's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8555377     |
    ## |Credit.debit    |0.8900482     |
    ## |Crypto          |0.0330391     |
    ## |None            |0.0164526     |
    ## |Not.Sure        |0.0000000     |
    ## 
    ## 
    ## Table: Between EUR 1,501 and EUR 2,500's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.9015225     |
    ## |Credit.debit    |0.9257238     |
    ## |Crypto          |0.0390742     |
    ## |None            |0.0061603     |
    ## |Not.Sure        |0.0003520     |
    ## 
    ## 
    ## Table: EUR 750 or less's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8024194     |
    ## |Credit.debit    |0.8185484     |
    ## |Crypto          |0.0447214     |
    ## |None            |0.0366569     |
    ## |Not.Sure        |0.0003666     |

# comapare adoption by HHSIZE

``` r
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
```

    ## 
    ## 
    ## Table: 2's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.9025989     |
    ## |Credit.debit    |0.9266513     |
    ## |Crypto          |0.0356691     |
    ## |None            |0.0093082     |
    ## |Not.Sure        |0.0002234     |
    ## 
    ## 
    ## Table: 4's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8797603     |
    ## |Credit.debit    |0.9106525     |
    ## |Crypto          |0.0519308     |
    ## |None            |0.0091877     |
    ## |Not.Sure        |0.0005326     |
    ## 
    ## 
    ## Table: 1's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.9091962     |
    ## |Credit.debit    |0.9259957     |
    ## |Crypto          |0.0385228     |
    ## |None            |0.0095583     |
    ## |Not.Sure        |0.0005793     |
    ## 
    ## 
    ## Table: 5's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8631324     |
    ## |Credit.debit    |0.9095634     |
    ## |Crypto          |0.0564796     |
    ## |None            |0.0128205     |
    ## |Not.Sure        |0.0006930     |
    ## 
    ## 
    ## Table: 3's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |0.8858641     |
    ## |Credit.debit    |0.9176615     |
    ## |Crypto          |0.0513221     |
    ## |None            |0.0098181     |
    ## |Not.Sure        |0.0004463     |
    ## 
    ## 
    ## Table: NA's Adoption Rate
    ## 
    ## |                |Adoption.Rate |
    ## |:---------------|:-------------|
    ## |Payment.account |NaN           |
    ## |Credit.debit    |NaN           |
    ## |Crypto          |NaN           |
    ## |None            |NaN           |
    ## |Not.Sure        |NaN           |

# Cash advantage

``` r
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
```

|                      | Advantage.Rating |
| :------------------- | :--------------- |
| cash.acceptance      | 0.2912790        |
| cash.faster          | 0.1699190        |
| cash.privacy         | 0.3754464        |
| cash.easier          | 0.1849319        |
| cash.safer           | 0.1677312        |
| immediately\_settled | 0.2921340        |
| aware\_spending      | 0.3664437        |
| other\_advantage     | 0.0343007        |
| do\_not\_use\_cash   | 0.0410401        |
| no\_advantage        | 0.0497410        |
| dont\_know           | 0.0120203        |

Cash advatange ratio

The table above the ration of people who believe that cash has advantage
for given reason. e.g. 29.12% of the population believe that cash is
advantageous since it is easily accepted.

We will break down the same comparison suing different demographics.

# Age Comaprios for Cash Advantage

``` r
cash.advg.age<- data.frame(cash.advg, AGE= df$AGE)

for(age in unique(cash.advg.age$AGE)){
  dd<- cash.advg.age%>%
    filter(AGE==age)%>%
    drop_na()
  
  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))
  
  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(age, "'s Cash advatange ratio")))
}
```

    ## 
    ## 
    ## Table: 64-69's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2815452        |
    ## |cash.faster         |0.1633424        |
    ## |cash.privacy        |0.3529288        |
    ## |cash.easier         |0.1822381        |
    ## |cash.safer          |0.1631325        |
    ## |immediately_settled |0.2922528        |
    ## |aware_spending      |0.3407516        |
    ## |other_advantage     |0.0377913        |
    ## |do_not_use_cash     |0.0529078        |
    ## |no_advantage        |0.0629855        |
    ## |dont_know           |0.0146966        |
    ## |AGE                 |1.0000000        |
    ## 
    ## 
    ## Table: 55-59's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2852214        |
    ## |cash.faster         |0.1687752        |
    ## |cash.privacy        |0.4008051        |
    ## |cash.easier         |0.1868890        |
    ## |cash.safer          |0.1650374        |
    ## |immediately_settled |0.3266245        |
    ## |aware_spending      |0.4010926        |
    ## |other_advantage     |0.0345026        |
    ## |do_not_use_cash     |0.0342151        |
    ## |no_advantage        |0.0485911        |
    ## |dont_know           |0.0094882        |
    ## |AGE                 |1.0000000        |
    ## 
    ## 
    ## Table: 35-39's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2950740        |
    ## |cash.faster         |0.1681631        |
    ## |cash.privacy        |0.3901966        |
    ## |cash.easier         |0.1943703        |
    ## |cash.safer          |0.1732589        |
    ## |immediately_settled |0.2841543        |
    ## |aware_spending      |0.3620480        |
    ## |other_advantage     |0.0296045        |
    ## |do_not_use_cash     |0.0351856        |
    ## |no_advantage        |0.0487746        |
    ## |dont_know           |0.0075224        |
    ## |AGE                 |1.0000000        |
    ## 
    ## 
    ## Table: 30-34's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.3113716        |
    ## |cash.faster         |0.1735825        |
    ## |cash.privacy        |0.3842255        |
    ## |cash.easier         |0.1808679        |
    ## |cash.safer          |0.1716820        |
    ## |immediately_settled |0.2850808        |
    ## |aware_spending      |0.3791574        |
    ## |other_advantage     |0.0291416        |
    ## |do_not_use_cash     |0.0370605        |
    ## |no_advantage        |0.0370605        |
    ## |dont_know           |0.0088692        |
    ## |AGE                 |1.0000000        |
    ## 
    ## 
    ## Table: 60-64's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2753024        |
    ## |cash.faster         |0.1747295        |
    ## |cash.privacy        |0.3822406        |
    ## |cash.easier         |0.1804583        |
    ## |cash.safer          |0.1521324        |
    ## |immediately_settled |0.3045831        |
    ## |aware_spending      |0.3835137        |
    ## |other_advantage     |0.0327817        |
    ## |do_not_use_cash     |0.0388288        |
    ## |no_advantage        |0.0585614        |
    ## |dont_know           |0.0149586        |
    ## |AGE                 |1.0000000        |
    ## 
    ## 
    ## Table: 50-54's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2884458        |
    ## |cash.faster         |0.1578804        |
    ## |cash.privacy        |0.3783119        |
    ## |cash.easier         |0.1756351        |
    ## |cash.safer          |0.1576072        |
    ## |immediately_settled |0.2952745        |
    ## |aware_spending      |0.3813166        |
    ## |other_advantage     |0.0346900        |
    ## |do_not_use_cash     |0.0357826        |
    ## |no_advantage        |0.0527178        |
    ## |dont_know           |0.0125649        |
    ## |AGE                 |1.0000000        |
    ## 
    ## 
    ## Table: 18-24's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.3196924        |
    ## |cash.faster         |0.1691843        |
    ## |cash.privacy        |0.3858830        |
    ## |cash.easier         |0.1867619        |
    ## |cash.safer          |0.1919802        |
    ## |immediately_settled |0.2930514        |
    ## |aware_spending      |0.3737984        |
    ## |other_advantage     |0.0269157        |
    ## |do_not_use_cash     |0.0288382        |
    ## |no_advantage        |0.0373524        |
    ## |dont_know           |0.0093381        |
    ## |AGE                 |1.0000000        |
    ## 
    ## 
    ## Table: 45-49's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2996633        |
    ## |cash.faster         |0.1705948        |
    ## |cash.privacy        |0.4043210        |
    ## |cash.easier         |0.1860269        |
    ## |cash.safer          |0.1705948        |
    ## |immediately_settled |0.2988215        |
    ## |aware_spending      |0.3905724        |
    ## |other_advantage     |0.0297419        |
    ## |do_not_use_cash     |0.0356341        |
    ## |no_advantage        |0.0448934        |
    ## |dont_know           |0.0120651        |
    ## |AGE                 |1.0000000        |
    ## 
    ## 
    ## Table: 40-44's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2982456        |
    ## |cash.faster         |0.1805808        |
    ## |cash.privacy        |0.3871748        |
    ## |cash.easier         |0.1730188        |
    ## |cash.safer          |0.1760436        |
    ## |immediately_settled |0.2828191        |
    ## |aware_spending      |0.3826376        |
    ## |other_advantage     |0.0329704        |
    ## |do_not_use_cash     |0.0402299        |
    ## |no_advantage        |0.0483969        |
    ## |dont_know           |0.0111918        |
    ## |AGE                 |1.0000000        |
    ## 
    ## 
    ## Table: 75+'s Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2611781        |
    ## |cash.faster         |0.1745919        |
    ## |cash.privacy        |0.2924060        |
    ## |cash.easier         |0.2129170        |
    ## |cash.safer          |0.1504613        |
    ## |immediately_settled |0.2810504        |
    ## |aware_spending      |0.3073101        |
    ## |other_advantage     |0.0610362        |
    ## |do_not_use_cash     |0.0731015        |
    ## |no_advantage        |0.0610362        |
    ## |dont_know           |0.0276792        |
    ## |AGE                 |1.0000000        |
    ## 
    ## 
    ## Table: 25-29's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.3102867        |
    ## |cash.faster         |0.1838111        |
    ## |cash.privacy        |0.3798482        |
    ## |cash.easier         |0.1956155        |
    ## |cash.safer          |0.1800169        |
    ## |immediately_settled |0.2833052        |
    ## |aware_spending      |0.3655143        |
    ## |other_advantage     |0.0345700        |
    ## |do_not_use_cash     |0.0408938        |
    ## |no_advantage        |0.0387858        |
    ## |dont_know           |0.0080101        |
    ## |AGE                 |1.0000000        |
    ## 
    ## 
    ## Table: 70-74's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2686170        |
    ## |cash.faster         |0.1732048        |
    ## |cash.privacy        |0.3354388        |
    ## |cash.easier         |0.1911569        |
    ## |cash.safer          |0.1595745        |
    ## |immediately_settled |0.2809176        |
    ## |aware_spending      |0.3144947        |
    ## |other_advantage     |0.0462101        |
    ## |do_not_use_cash     |0.0601729        |
    ## |no_advantage        |0.0598404        |
    ## |dont_know           |0.0169548        |
    ## |AGE                 |1.0000000        |

\#Gender comariosn for cash advantage

``` r
cash.advg.D1<- data.frame(cash.advg, D1= df$D1)%>%
  drop_na()

for(d1 in unique(cash.advg.D1$D1)){
  dd<- cash.advg.D1%>%
    filter(D1==d1)%>%
    drop_na()
  
  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))
  
  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(d1, "'s Cash advatange ratio")))
  
}
```

    ## 
    ## 
    ## Table: Female's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2851928        |
    ## |cash.faster         |0.1647196        |
    ## |cash.privacy        |0.3718847        |
    ## |cash.easier         |0.1791764        |
    ## |cash.safer          |0.1645736        |
    ## |immediately_settled |0.2921534        |
    ## |aware_spending      |0.3886293        |
    ## |other_advantage     |0.0347547        |
    ## |do_not_use_cash     |0.0405471        |
    ## |no_advantage        |0.0480919        |
    ## |dont_know           |0.0121690        |
    ## |D1                  |1.0000000        |
    ## 
    ## 
    ## Table: Male's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2998267        |
    ## |cash.faster         |0.1769340        |
    ## |cash.privacy        |0.3820703        |
    ## |cash.easier         |0.1926369        |
    ## |cash.safer          |0.1724174        |
    ## |immediately_settled |0.2942072        |
    ## |aware_spending      |0.3452025        |
    ## |other_advantage     |0.0339793        |
    ## |do_not_use_cash     |0.0419096        |
    ## |no_advantage        |0.0518880        |
    ## |dont_know           |0.0119742        |
    ## |D1                  |1.0000000        |
    ## 
    ## 
    ## Table: Other, non-binary's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.4054054        |
    ## |cash.faster         |0.1081081        |
    ## |cash.privacy        |0.4054054        |
    ## |cash.easier         |0.1351351        |
    ## |cash.safer          |0.1621622        |
    ## |immediately_settled |0.3513514        |
    ## |aware_spending      |0.4054054        |
    ## |other_advantage     |0.0810811        |
    ## |do_not_use_cash     |0.0270270        |
    ## |no_advantage        |0.0540541        |
    ## |dont_know           |0.0000000        |
    ## |D1                  |1.0000000        |

# Activity Status comariosn for cash advantage

``` r
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
```

    ## 
    ## 
    ## Table: Without a professional activity or student's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2844282        |
    ## |cash.faster         |0.1697436        |
    ## |cash.privacy        |0.3512949        |
    ## |cash.easier         |0.1908800        |
    ## |cash.safer          |0.1683737        |
    ## |immediately_settled |0.2910170        |
    ## |aware_spending      |0.3661035        |
    ## |other_advantage     |0.0378368        |
    ## |do_not_use_cash     |0.0447518        |
    ## |no_advantage        |0.0534934        |
    ## |dont_know           |0.0136995        |
    ## 
    ## 
    ## Table: Employee's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2962278        |
    ## |cash.faster         |0.1681057        |
    ## |cash.privacy        |0.3908418        |
    ## |cash.easier         |0.1795149        |
    ## |cash.safer          |0.1664097        |
    ## |immediately_settled |0.2976668        |
    ## |aware_spending      |0.3747045        |
    ## |other_advantage     |0.0328914        |
    ## |do_not_use_cash     |0.0422448        |
    ## |no_advantage        |0.0476925        |
    ## |dont_know           |0.0111522        |
    ## 
    ## 
    ## Table: Self-employed's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.3017941        |
    ## |cash.faster         |0.1834686        |
    ## |cash.privacy        |0.4017514        |
    ## |cash.easier         |0.1943614        |
    ## |cash.safer          |0.1781290        |
    ## |immediately_settled |0.2808629        |
    ## |aware_spending      |0.3481418        |
    ## |other_advantage     |0.0301153        |
    ## |do_not_use_cash     |0.0228535        |
    ## |no_advantage        |0.0472021        |
    ## |dont_know           |0.0098249        |

# Education comaprion for cash advantange

``` r
cash.advg.edu<- data.frame(cash.advg, EDU= df$EDUCATION)

for(edu in unique(cash.advg.edu$EDU)){
  dd<- cash.advg.edu%>%
    filter(EDU==edu)%>%
    drop_na()
  
  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))
  
  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(edu, "'s Cash advatange ratio")))
}
```

    ## 
    ## 
    ## Table: Upper/post-secondary education's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2923577        |
    ## |cash.faster         |0.1787770        |
    ## |cash.privacy        |0.3702627        |
    ## |cash.easier         |0.1918277        |
    ## |cash.safer          |0.1776942        |
    ## |immediately_settled |0.2991395        |
    ## |aware_spending      |0.3814327        |
    ## |other_advantage     |0.0311734        |
    ## |do_not_use_cash     |0.0395509        |
    ## |no_advantage        |0.0506069        |
    ## |dont_know           |0.0125378        |
    ## |EDU                 |1.0000000        |
    ## 
    ## 
    ## Table: University/PhD/research's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.3052980        |
    ## |cash.faster         |0.1573510        |
    ## |cash.privacy        |0.3935762        |
    ## |cash.easier         |0.1689404        |
    ## |cash.safer          |0.1572185        |
    ## |immediately_settled |0.2939735        |
    ## |aware_spending      |0.3627815        |
    ## |other_advantage     |0.0401987        |
    ## |do_not_use_cash     |0.0509934        |
    ## |no_advantage        |0.0544371        |
    ## |dont_know           |0.0127152        |
    ## |EDU                 |1.0000000        |
    ## 
    ## 
    ## Table: Primary/lower secondary education's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2643893        |
    ## |cash.faster         |0.1785560        |
    ## |cash.privacy        |0.3571121        |
    ## |cash.easier         |0.2058275        |
    ## |cash.safer          |0.1689393        |
    ## |immediately_settled |0.2763026        |
    ## |aware_spending      |0.3444811        |
    ## |other_advantage     |0.0301421        |
    ## |do_not_use_cash     |0.0238266        |
    ## |no_advantage        |0.0384671        |
    ## |dont_know           |0.0094732        |
    ## |EDU                 |1.0000000        |
    ## 
    ## 
    ## Table: NA's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |NaN              |
    ## |cash.faster         |NaN              |
    ## |cash.privacy        |NaN              |
    ## |cash.easier         |NaN              |
    ## |cash.safer          |NaN              |
    ## |immediately_settled |NaN              |
    ## |aware_spending      |NaN              |
    ## |other_advantage     |NaN              |
    ## |do_not_use_cash     |NaN              |
    ## |no_advantage        |NaN              |
    ## |dont_know           |NaN              |
    ## |EDU                 |NaN              |

# Income comaprison for cash advantage

``` r
cash.advg.income<- data.frame(cash.advg, INCOME= df$INCOME)%>%
  drop_na()

for(income in unique(cash.advg.income$INCOME)){
  dd<- cash.advg.income%>%
    filter(INCOME==income)%>%
    drop_na()
  
  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))
  
  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(income, "'s Cash advatange ratio")))
  
}
```

    ## 
    ## 
    ## Table: Between EUR 2,501 and EUR 4,000's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.3020165        |
    ## |cash.faster         |0.1639780        |
    ## |cash.privacy        |0.4021998        |
    ## |cash.easier         |0.1775435        |
    ## |cash.safer          |0.1555454        |
    ## |immediately_settled |0.3141155        |
    ## |aware_spending      |0.3582951        |
    ## |other_advantage     |0.0296059        |
    ## |do_not_use_cash     |0.0362053        |
    ## |no_advantage        |0.0456462        |
    ## |dont_know           |0.0107241        |
    ## |INCOME              |1.0000000        |
    ## 
    ## 
    ## Table: More than EUR 4,000's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2707313        |
    ## |cash.faster         |0.1460715        |
    ## |cash.privacy        |0.4102704        |
    ## |cash.easier         |0.1529668        |
    ## |cash.safer          |0.1415351        |
    ## |immediately_settled |0.3235347        |
    ## |aware_spending      |0.3215387        |
    ## |other_advantage     |0.0480856        |
    ## |do_not_use_cash     |0.0709490        |
    ## |no_advantage        |0.0586101        |
    ## |dont_know           |0.0127019        |
    ## |INCOME              |1.0000000        |
    ## 
    ## 
    ## Table: Between EUR 751 and EUR 1,500's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.3012614        |
    ## |cash.faster         |0.1847826        |
    ## |cash.privacy        |0.3442029        |
    ## |cash.easier         |0.2069243        |
    ## |cash.safer          |0.1909554        |
    ## |immediately_settled |0.2709340        |
    ## |aware_spending      |0.4002952        |
    ## |other_advantage     |0.0365003        |
    ## |do_not_use_cash     |0.0367687        |
    ## |no_advantage        |0.0493827        |
    ## |dont_know           |0.0114063        |
    ## |INCOME              |1.0000000        |
    ## 
    ## 
    ## Table: Between EUR 1,501 and EUR 2,500's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2989399        |
    ## |cash.faster         |0.1751767        |
    ## |cash.privacy        |0.3742049        |
    ## |cash.easier         |0.1860424        |
    ## |cash.safer          |0.1705830        |
    ## |immediately_settled |0.2958481        |
    ## |aware_spending      |0.3780035        |
    ## |other_advantage     |0.0258834        |
    ## |do_not_use_cash     |0.0351590        |
    ## |no_advantage        |0.0477915        |
    ## |dont_know           |0.0129859        |
    ## |INCOME              |1.0000000        |
    ## 
    ## 
    ## Table: EUR 750 or less's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2858193        |
    ## |cash.faster         |0.1976488        |
    ## |cash.privacy        |0.3310066        |
    ## |cash.easier         |0.2215283        |
    ## |cash.safer          |0.2086701        |
    ## |immediately_settled |0.2575312        |
    ## |aware_spending      |0.3783982        |
    ## |other_advantage     |0.0400441        |
    ## |do_not_use_cash     |0.0378398        |
    ## |no_advantage        |0.0484938        |
    ## |dont_know           |0.0084497        |
    ## |INCOME              |1.0000000        |

# HHsize cash advantage comparison

``` r
cash.advg.hhsize<- data.frame(cash.advg, HHSIZE= df$HHSIZE)

for(hhsize in unique(cash.advg.hhsize$HHSIZE)){
  dd<- cash.advg.hhsize%>%
    filter(HHSIZE==hhsize)%>%
    drop_na()
  
  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))
  
  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(hhsize, "'s Cash advatange ratio")))
  
}
```

    ## 
    ## 
    ## Table: 2's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2878289        |
    ## |cash.faster         |0.1655203        |
    ## |cash.privacy        |0.3797847        |
    ## |cash.easier         |0.1763606        |
    ## |cash.safer          |0.1600628        |
    ## |immediately_settled |0.2945574        |
    ## |aware_spending      |0.3539922        |
    ## |other_advantage     |0.0393989        |
    ## |do_not_use_cash     |0.0475478        |
    ## |no_advantage        |0.0543511        |
    ## |dont_know           |0.0139803        |
    ## |HHSIZE              |0.0000000        |
    ## 
    ## 
    ## Table: 4's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.3091929        |
    ## |cash.faster         |0.1811865        |
    ## |cash.privacy        |0.3701229        |
    ## |cash.easier         |0.1928113        |
    ## |cash.safer          |0.1707643        |
    ## |immediately_settled |0.2847408        |
    ## |aware_spending      |0.3752004        |
    ## |other_advantage     |0.0312667        |
    ## |do_not_use_cash     |0.0307322        |
    ## |no_advantage        |0.0423570        |
    ## |dont_know           |0.0109567        |
    ## |HHSIZE              |0.0000000        |
    ## 
    ## 
    ## Table: 1's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2830957        |
    ## |cash.faster         |0.1606052        |
    ## |cash.privacy        |0.3818737        |
    ## |cash.easier         |0.1803899        |
    ## |cash.safer          |0.1668606        |
    ## |immediately_settled |0.3149549        |
    ## |aware_spending      |0.3565610        |
    ## |other_advantage     |0.0372418        |
    ## |do_not_use_cash     |0.0549898        |
    ## |no_advantage        |0.0548443        |
    ## |dont_know           |0.0116381        |
    ## |HHSIZE              |1.0000000        |
    ## 
    ## 
    ## Table: 5's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2885417        |
    ## |cash.faster         |0.1670139        |
    ## |cash.privacy        |0.3715278        |
    ## |cash.easier         |0.1906250        |
    ## |cash.safer          |0.1777778        |
    ## |immediately_settled |0.2815972        |
    ## |aware_spending      |0.3989583        |
    ## |other_advantage     |0.0357639        |
    ## |do_not_use_cash     |0.0385417        |
    ## |no_advantage        |0.0427083        |
    ## |dont_know           |0.0111111        |
    ## |HHSIZE              |0.0000000        |
    ## 
    ## 
    ## Table: 3's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |0.2936766        |
    ## |cash.faster         |0.1780638        |
    ## |cash.privacy        |0.3754896        |
    ## |cash.easier         |0.1963067        |
    ## |cash.safer          |0.1767208        |
    ## |immediately_settled |0.2858422        |
    ## |aware_spending      |0.3814214        |
    ## |other_advantage     |0.0267487        |
    ## |do_not_use_cash     |0.0305540        |
    ## |no_advantage        |0.0481253        |
    ## |dont_know           |0.0106323        |
    ## |HHSIZE              |0.0000000        |
    ## 
    ## 
    ## Table: NA's Cash advatange ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |cash.acceptance     |NaN              |
    ## |cash.faster         |NaN              |
    ## |cash.privacy        |NaN              |
    ## |cash.easier         |NaN              |
    ## |cash.safer          |NaN              |
    ## |immediately_settled |NaN              |
    ## |aware_spending      |NaN              |
    ## |other_advantage     |NaN              |
    ## |do_not_use_cash     |NaN              |
    ## |no_advantage        |NaN              |
    ## |dont_know           |NaN              |
    ## |HHSIZE              |NaN              |

# Card Advantage comparison

``` r
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
```

|                           | Advantage.Rating |
| :------------------------ | :--------------- |
| card.acceptance           | 0.2162400        |
| card.faster               | 0.4297390        |
| card.easier               | 0.4142232        |
| card.safer                | 0.2946487        |
| no\_worry\_carrying\_cash | 0.5827843        |
| aware\_spending           | 0.1862395        |
| other\_advantage          | 0.0197405        |
| no\_advantage             | 0.0223055        |
| do\_not\_use\_card        | 0.0238646        |
| dont\_know                | 0.0153900        |

Card advatange ratio

Similar to the cash advantage table, the table displays the % of
population who think card has advantages for various reasons. inthis
case, 42.97% of the people suggested that card has advantages since it
is faster to use. We will do a similar compariosn by breaking it down
suing demographics.

# Age Comaprios for Card Advantage

``` r
card.advg.age<- data.frame(card.advg, AGE= df$AGE)

for(age in unique(card.advg.age$AGE)){
  dd<- card.advg.age%>%
    filter(AGE==age)%>%
    drop_na()

  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))

  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(age, "'s Card advatange ratio")))
}
```

    ## 
    ## 
    ## Table: 64-69's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2347260        |
    ## |card.faster            |0.3923997        |
    ## |card.easier            |0.3816922        |
    ## |card.safer             |0.3151375        |
    ## |no_worry_carrying_cash |0.6057107        |
    ## |aware_spending         |0.1744699        |
    ## |other_advantage        |0.0191056        |
    ## |no_advantage           |0.0239345        |
    ## |do_not_use_card        |0.0266639        |
    ## |dont_know              |0.0151165        |
    ## |AGE                    |1.0000000        |
    ## 
    ## 
    ## Table: 55-59's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2412306        |
    ## |card.faster            |0.4045428        |
    ## |card.easier            |0.3898792        |
    ## |card.safer             |0.2964347        |
    ## |no_worry_carrying_cash |0.6138585        |
    ## |aware_spending         |0.1584244        |
    ## |other_advantage        |0.0235768        |
    ## |no_advantage           |0.0296147        |
    ## |do_not_use_card        |0.0247269        |
    ## |dont_know              |0.0161012        |
    ## |AGE                    |1.0000000        |
    ## 
    ## 
    ## Table: 35-39's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2050473        |
    ## |card.faster            |0.4673623        |
    ## |card.easier            |0.4231983        |
    ## |card.safer             |0.2911915        |
    ## |no_worry_carrying_cash |0.5738898        |
    ## |aware_spending         |0.2016501        |
    ## |other_advantage        |0.0167435        |
    ## |no_advantage           |0.0165008        |
    ## |do_not_use_card        |0.0211114        |
    ## |dont_know              |0.0194128        |
    ## |AGE                    |1.0000000        |
    ## 
    ## 
    ## Table: 30-34's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.1973392        |
    ## |card.faster            |0.4802027        |
    ## |card.easier            |0.4459930        |
    ## |card.safer             |0.2869813        |
    ## |no_worry_carrying_cash |0.5749129        |
    ## |aware_spending         |0.2150776        |
    ## |other_advantage        |0.0193221        |
    ## |no_advantage           |0.0183719        |
    ## |do_not_use_card        |0.0186886        |
    ## |dont_know              |0.0126703        |
    ## |AGE                    |1.0000000        |
    ## 
    ## 
    ## Table: 60-64's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2374284        |
    ## |card.faster            |0.3936983        |
    ## |card.easier            |0.4042011        |
    ## |card.safer             |0.3042648        |
    ## |no_worry_carrying_cash |0.5926162        |
    ## |aware_spending         |0.1543603        |
    ## |other_advantage        |0.0241884        |
    ## |no_advantage           |0.0311903        |
    ## |do_not_use_card        |0.0283259        |
    ## |dont_know              |0.0152769        |
    ## |AGE                    |1.0000000        |
    ## 
    ## 
    ## Table: 50-54's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2141491        |
    ## |card.faster            |0.3968861        |
    ## |card.easier            |0.3889648        |
    ## |card.safer             |0.3018301        |
    ## |no_worry_carrying_cash |0.5946463        |
    ## |aware_spending         |0.1660748        |
    ## |other_advantage        |0.0215788        |
    ## |no_advantage           |0.0251297        |
    ## |do_not_use_card        |0.0240371        |
    ## |dont_know              |0.0191205        |
    ## |AGE                    |1.0000000        |
    ## 
    ## 
    ## Table: 18-24's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.1917056        |
    ## |card.faster            |0.4886020        |
    ## |card.easier            |0.4358693        |
    ## |card.safer             |0.2746498        |
    ## |no_worry_carrying_cash |0.5347432        |
    ## |aware_spending         |0.2383960        |
    ## |other_advantage        |0.0192255        |
    ## |no_advantage           |0.0140071        |
    ## |do_not_use_card        |0.0214227        |
    ## |dont_know              |0.0112606        |
    ## |AGE                    |1.0000000        |
    ## 
    ## 
    ## Table: 45-49's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2247475        |
    ## |card.faster            |0.4332211        |
    ## |card.easier            |0.4175084        |
    ## |card.safer             |0.2904040        |
    ## |no_worry_carrying_cash |0.5934343        |
    ## |aware_spending         |0.1826599        |
    ## |other_advantage        |0.0179574        |
    ## |no_advantage           |0.0179574        |
    ## |do_not_use_card        |0.0230079        |
    ## |dont_know              |0.0143098        |
    ## |AGE                    |1.0000000        |
    ## 
    ## 
    ## Table: 40-44's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2105263        |
    ## |card.faster            |0.4633999        |
    ## |card.easier            |0.4452511        |
    ## |card.safer             |0.2785844        |
    ## |no_worry_carrying_cash |0.5946763        |
    ## |aware_spending         |0.1808832        |
    ## |other_advantage        |0.0205687        |
    ## |no_advantage           |0.0187538        |
    ## |do_not_use_card        |0.0184513        |
    ## |dont_know              |0.0157290        |
    ## |AGE                    |1.0000000        |
    ## 
    ## 
    ## Table: 75+'s Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2122072        |
    ## |card.faster            |0.3406671        |
    ## |card.easier            |0.4002839        |
    ## |card.safer             |0.3136977        |
    ## |no_worry_carrying_cash |0.5777147        |
    ## |aware_spending         |0.1611072        |
    ## |other_advantage        |0.0248403        |
    ## |no_advantage           |0.0468417        |
    ## |do_not_use_card        |0.0347764        |
    ## |dont_know              |0.0184528        |
    ## |AGE                    |1.0000000        |
    ## 
    ## 
    ## Table: 25-29's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.1947723        |
    ## |card.faster            |0.5223440        |
    ## |card.easier            |0.4704890        |
    ## |card.safer             |0.2731872        |
    ## |no_worry_carrying_cash |0.5623946        |
    ## |aware_spending         |0.2242833        |
    ## |other_advantage        |0.0160202        |
    ## |no_advantage           |0.0130691        |
    ## |do_not_use_card        |0.0223440        |
    ## |dont_know              |0.0109612        |
    ## |AGE                    |1.0000000        |
    ## 
    ## 
    ## Table: 70-74's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2287234        |
    ## |card.faster            |0.3680186        |
    ## |card.easier            |0.4072473        |
    ## |card.safer             |0.3231383        |
    ## |no_worry_carrying_cash |0.5851064        |
    ## |aware_spending         |0.1811835        |
    ## |other_advantage        |0.0172872        |
    ## |no_advantage           |0.0265957        |
    ## |do_not_use_card        |0.0299202        |
    ## |dont_know              |0.0166223        |
    ## |AGE                    |1.0000000        |

\#Gender compariosn for card advantage

``` r
card.advg.D1<- data.frame(card.advg, D1= df$D1)%>%
  drop_na()

for(d1 in unique(card.advg.D1$D1)){
  dd<- card.advg.D1%>%
    filter(D1==d1)%>%
    drop_na()
  
  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))
  
  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(d1, "'s Card advatange ratio")))
  
}
```

    ## 
    ## 
    ## Table: Female's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2178738        |
    ## |card.faster            |0.4377434        |
    ## |card.easier            |0.4094139        |
    ## |card.safer             |0.2826129        |
    ## |no_worry_carrying_cash |0.6034852        |
    ## |aware_spending         |0.1881815        |
    ## |other_advantage        |0.0185456        |
    ## |no_advantage           |0.0215148        |
    ## |do_not_use_card        |0.0230238        |
    ## |dont_know              |0.0144568        |
    ## |D1                     |1.0000000        |
    ## 
    ## 
    ## Table: Male's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2162702        |
    ## |card.faster            |0.4244000        |
    ## |card.easier            |0.4222467        |
    ## |card.safer             |0.3099102        |
    ## |no_worry_carrying_cash |0.5648338        |
    ## |aware_spending         |0.1855995        |
    ## |other_advantage        |0.0211123        |
    ## |no_advantage           |0.0233181        |
    ## |do_not_use_card        |0.0249987        |
    ## |dont_know              |0.0165432        |
    ## |D1                     |1.0000000        |
    ## 
    ## 
    ## Table: Other, non-binary's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.1351351        |
    ## |card.faster            |0.4054054        |
    ## |card.easier            |0.5675676        |
    ## |card.safer             |0.2702703        |
    ## |no_worry_carrying_cash |0.5945946        |
    ## |aware_spending         |0.1621622        |
    ## |other_advantage        |0.0540541        |
    ## |no_advantage           |0.0270270        |
    ## |do_not_use_card        |0.0000000        |
    ## |dont_know              |0.0000000        |
    ## |D1                     |1.0000000        |

# Activity Status compariosn for card advantage

``` r
card.advg.D6_1<- data.frame(card.advg, D6_1= df$D6_1)%>%
  drop_na()

for(d6_1 in unique(card.advg.D6_1$D6_1)){
  dd<- card.advg.D6_1%>%
    filter(D6_1==d6_1)%>%
    drop_na()
  
  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))
  
  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(d6_1, "'s Card advatange ratio")))
  
}
```

    ## 
    ## 
    ## Table: Without a professional activity or student's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2206928        |
    ## |card.faster            |0.4092243        |
    ## |card.easier            |0.3974167        |
    ## |card.safer             |0.2956488        |
    ## |no_worry_carrying_cash |0.5747929        |
    ## |aware_spending         |0.1816818        |
    ## |other_advantage        |0.0217235        |
    ## |no_advantage           |0.0291604        |
    ## |do_not_use_card        |0.0279209        |
    ## |dont_know              |0.0155914        |
    ## |D6_1                   |1.0000000        |
    ## 
    ## 
    ## Table: Employee's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2176997        |
    ## |card.faster            |0.4604790        |
    ## |card.easier            |0.4361702        |
    ## |card.safer             |0.2965361        |
    ## |no_worry_carrying_cash |0.5963100        |
    ## |aware_spending         |0.1891767        |
    ## |other_advantage        |0.0178333        |
    ## |no_advantage           |0.0172166        |
    ## |do_not_use_card        |0.0199918        |
    ## |dont_know              |0.0145955        |
    ## |D6_1                   |1.0000000        |
    ## 
    ## 
    ## Table: Self-employed's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2011961        |
    ## |card.faster            |0.3844511        |
    ## |card.easier            |0.3889364        |
    ## |card.safer             |0.2909013        |
    ## |no_worry_carrying_cash |0.5692012        |
    ## |aware_spending         |0.1954293        |
    ## |other_advantage        |0.0219991        |
    ## |no_advantage           |0.0215720        |
    ## |do_not_use_card        |0.0271252        |
    ## |dont_know              |0.0183682        |
    ## |D6_1                   |1.0000000        |

# Education comaprion for card advantange

``` r
card.advg.edu<- data.frame(card.advg, EDU= df$EDUCATION)

for(edu in unique(card.advg.edu$EDU)){
  dd<- card.advg.edu%>%
    filter(EDU==edu)%>%
    drop_na()
  
  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))
  
  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(edu, "'s Card advatange ratio")))
}
```

    ## 
    ## 
    ## Table: Upper/post-secondary education's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2252237        |
    ## |card.faster            |0.4355160        |
    ## |card.easier            |0.4164814        |
    ## |card.safer             |0.2845501        |
    ## |no_worry_carrying_cash |0.5793583        |
    ## |aware_spending         |0.1734769        |
    ## |other_advantage        |0.0195475        |
    ## |no_advantage           |0.0268992        |
    ## |do_not_use_card        |0.0240497        |
    ## |dont_know              |0.0133356        |
    ## |EDU                    |1.0000000        |
    ## 
    ## 
    ## Table: University/PhD/research's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2015894        |
    ## |card.faster            |0.4538411        |
    ## |card.easier            |0.4505298        |
    ## |card.safer             |0.3015894        |
    ## |no_worry_carrying_cash |0.6221854        |
    ## |aware_spending         |0.1998675        |
    ## |other_advantage        |0.0208609        |
    ## |no_advantage           |0.0176821        |
    ## |do_not_use_card        |0.0201325        |
    ## |dont_know              |0.0143046        |
    ## |EDU                    |1.0000000        |
    ## 
    ## 
    ## Table: Primary/lower secondary education's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2297976        |
    ## |card.faster            |0.3720396        |
    ## |card.easier            |0.3380221        |
    ## |card.safer             |0.3111813        |
    ## |no_worry_carrying_cash |0.5180135        |
    ## |aware_spending         |0.1926224        |
    ## |other_advantage        |0.0182288        |
    ## |no_advantage           |0.0212430        |
    ## |do_not_use_card        |0.0320080        |
    ## |dont_know              |0.0232525        |
    ## |EDU                    |1.0000000        |
    ## 
    ## 
    ## Table: NA's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |NaN              |
    ## |card.faster            |NaN              |
    ## |card.easier            |NaN              |
    ## |card.safer             |NaN              |
    ## |no_worry_carrying_cash |NaN              |
    ## |aware_spending         |NaN              |
    ## |other_advantage        |NaN              |
    ## |no_advantage           |NaN              |
    ## |do_not_use_card        |NaN              |
    ## |dont_know              |NaN              |
    ## |EDU                    |NaN              |

# Income comaprison for card advantage

``` r
card.advg.income<- data.frame(card.advg, INCOME= df$INCOME)%>%
  drop_na()

for(income in unique(card.advg.income$INCOME)){
  dd<- card.advg.income%>%
    filter(INCOME==income)%>%
    drop_na()
  
  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))
  
  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(income, "'s Card advatange ratio")))
  
}
```

    ## 
    ## 
    ## Table: Between EUR 2,501 and EUR 4,000's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2340055        |
    ## |card.faster            |0.4265811        |
    ## |card.easier            |0.4155820        |
    ## |card.safer             |0.3088909        |
    ## |no_worry_carrying_cash |0.5946838        |
    ## |aware_spending         |0.1879010        |
    ## |other_advantage        |0.0157654        |
    ## |no_advantage           |0.0157654        |
    ## |do_not_use_card        |0.0237397        |
    ## |dont_know              |0.0179652        |
    ## |INCOME                 |1.0000000        |
    ## 
    ## 
    ## Table: More than EUR 4,000's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2427872        |
    ## |card.faster            |0.4545455        |
    ## |card.easier            |0.4795863        |
    ## |card.safer             |0.3130103        |
    ## |no_worry_carrying_cash |0.6276538        |
    ## |aware_spending         |0.1932499        |
    ## |other_advantage        |0.0188713        |
    ## |no_advantage           |0.0148793        |
    ## |do_not_use_card        |0.0157866        |
    ## |dont_know              |0.0128833        |
    ## |INCOME                 |1.0000000        |
    ## 
    ## 
    ## Table: Between EUR 751 and EUR 1,500's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.1977992        |
    ## |card.faster            |0.4276704        |
    ## |card.easier            |0.4002952        |
    ## |card.safer             |0.2758991        |
    ## |no_worry_carrying_cash |0.5720612        |
    ## |aware_spending         |0.1755233        |
    ## |other_advantage        |0.0224101        |
    ## |no_advantage           |0.0304616        |
    ## |do_not_use_card        |0.0258991        |
    ## |dont_know              |0.0127483        |
    ## |INCOME                 |1.0000000        |
    ## 
    ## 
    ## Table: Between EUR 1,501 and EUR 2,500's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2196113        |
    ## |card.faster            |0.4421378        |
    ## |card.easier            |0.4102473        |
    ## |card.safer             |0.2972615        |
    ## |no_worry_carrying_cash |0.5894876        |
    ## |aware_spending         |0.1918728        |
    ## |other_advantage        |0.0162544        |
    ## |no_advantage           |0.0178445        |
    ## |do_not_use_card        |0.0242049        |
    ## |dont_know              |0.0148410        |
    ## |INCOME                 |1.0000000        |
    ## 
    ## 
    ## Table: EUR 750 or less's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.1715650        |
    ## |card.faster            |0.4125643        |
    ## |card.easier            |0.3901543        |
    ## |card.safer             |0.2560617        |
    ## |no_worry_carrying_cash |0.5003674        |
    ## |aware_spending         |0.1855253        |
    ## |other_advantage        |0.0334313        |
    ## |no_advantage           |0.0488611        |
    ## |do_not_use_card        |0.0315944        |
    ## |dont_know              |0.0135929        |
    ## |INCOME                 |1.0000000        |

# HHsize card advantage comparison

``` r
card.advg.hhsize<- data.frame(card.advg, HHSIZE= df$HHSIZE)

for(hhsize in unique(card.advg.hhsize$HHSIZE)){
  dd<- card.advg.hhsize%>%
    filter(HHSIZE==hhsize)%>%
    drop_na()
  
  ratios <- apply(data.matrix(dd)[,], 2, function(x) length(which(x == 1)) / nrow(dd))
  
  print(kable(data.frame(Advantage.Rating= ratios), align = "l", caption =paste0(hhsize, "'s Card advatange ratio")))
  
}
```

    ## 
    ## 
    ## Table: 2's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2213666        |
    ## |card.faster            |0.4196322        |
    ## |card.easier            |0.4182117        |
    ## |card.safer             |0.3003888        |
    ## |no_worry_carrying_cash |0.5962171        |
    ## |aware_spending         |0.1840610        |
    ## |other_advantage        |0.0193630        |
    ## |no_advantage           |0.0248953        |
    ## |do_not_use_card        |0.0231011        |
    ## |dont_know              |0.0165969        |
    ## |HHSIZE                 |0.0000000        |
    ## 
    ## 
    ## Table: 4's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2069749        |
    ## |card.faster            |0.4486905        |
    ## |card.easier            |0.4107429        |
    ## |card.safer             |0.2975681        |
    ## |no_worry_carrying_cash |0.5795029        |
    ## |aware_spending         |0.1941475        |
    ## |other_advantage        |0.0176376        |
    ## |no_advantage           |0.0148316        |
    ## |do_not_use_card        |0.0255211        |
    ## |dont_know              |0.0136291        |
    ## |HHSIZE                 |0.0000000        |
    ## 
    ## 
    ## Table: 1's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2326157        |
    ## |card.faster            |0.4189700        |
    ## |card.easier            |0.4330812        |
    ## |card.safer             |0.2768403        |
    ## |no_worry_carrying_cash |0.5904859        |
    ## |aware_spending         |0.1873727        |
    ## |other_advantage        |0.0242944        |
    ## |no_advantage           |0.0261856        |
    ## |do_not_use_card        |0.0226942        |
    ## |dont_know              |0.0152749        |
    ## |HHSIZE                 |1.0000000        |
    ## 
    ## 
    ## Table: 5's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2125000        |
    ## |card.faster            |0.4503472        |
    ## |card.easier            |0.4118056        |
    ## |card.safer             |0.2927083        |
    ## |no_worry_carrying_cash |0.5680556        |
    ## |aware_spending         |0.1913194        |
    ## |other_advantage        |0.0187500        |
    ## |no_advantage           |0.0256944        |
    ## |do_not_use_card        |0.0194444        |
    ## |dont_know              |0.0114583        |
    ## |HHSIZE                 |0.0000000        |
    ## 
    ## 
    ## Table: 3's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |0.2085059        |
    ## |card.faster            |0.4380526        |
    ## |card.easier            |0.4038053        |
    ## |card.safer             |0.3024063        |
    ## |no_worry_carrying_cash |0.5739228        |
    ## |aware_spending         |0.1835478        |
    ## |other_advantage        |0.0191382        |
    ## |no_advantage           |0.0209289        |
    ## |do_not_use_card        |0.0263011        |
    ## |dont_know              |0.0166760        |
    ## |HHSIZE                 |0.0000000        |
    ## 
    ## 
    ## Table: NA's Card advatange ratio
    ## 
    ## |                       |Advantage.Rating |
    ## |:----------------------|:----------------|
    ## |card.acceptance        |NaN              |
    ## |card.faster            |NaN              |
    ## |card.easier            |NaN              |
    ## |card.safer             |NaN              |
    ## |no_worry_carrying_cash |NaN              |
    ## |aware_spending         |NaN              |
    ## |other_advantage        |NaN              |
    ## |no_advantage           |NaN              |
    ## |do_not_use_card        |NaN              |
    ## |dont_know              |NaN              |
    ## |HHSIZE                 |NaN              |

# Covid19 comaprison

``` r
Covid<- space%>%
  select(QQ19A)%>%
  mutate(QQ19A=case_when(
    QQ19A==1~   "Much more often",
    QQ19A==2~   "Somewhat more often",
    QQ19A==3~   "The same as before",
    QQ19A==4~   "Somewhat less often",
    QQ19A==5~   "Much less often",
    QQ19A==999999~  "Don't know",

  ))

pp<-prop.table(table(Covid$QQ19A))

covid_dist<- data.frame( Use.of.cash= pp)

kable(covid_dist, align = 'l', caption = "are you using cash instead of non-cash payment methods more")
```

| Use.of.cash.Var1    | Use.of.cash.Freq |
| :------------------ | :--------------- |
| Don’t know          | 0.0021627        |
| Much less often     | 0.1429110        |
| Much more often     | 0.0696826        |
| Somewhat less often | 0.1563647        |
| Somewhat more often | 0.1021979        |
| The same as before  | 0.5266811        |

are you using cash instead of non-cash payment methods more

The table shows a representation of how use of cash has changed before
and during covid 19. Over 52.66% of the population continued to use cash
in the same frequecy as they did before covid 19. The same analysis
above has been broken down by demographic

# Comapare cash frequecy use during and before covid 19 by demographics

##### 

# Age Comaprios for Caash frequecy use during and before covid 19

``` r
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
```

    ## 
    ## 
    ## Table: 64-69's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0020925        |
    ## |Much less often     |0.1537979        |
    ## |Much more often     |0.0537769        |
    ## |Somewhat less often |0.1468927        |
    ## |Somewhat more often |0.0947897        |
    ## |The same as before  |0.5486503        |
    ## 
    ## 
    ## Table: 55-59's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0014327        |
    ## |Much less often     |0.1489971        |
    ## |Much more often     |0.0659026        |
    ## |Somewhat less often |0.1664756        |
    ## |Somewhat more often |0.0896848        |
    ## |The same as before  |0.5275072        |
    ## 
    ## 
    ## Table: 35-39's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0016912        |
    ## |Much less often     |0.1365064        |
    ## |Much more often     |0.0804542        |
    ## |Somewhat less often |0.1613916        |
    ## |Somewhat more often |0.1152452        |
    ## |The same as before  |0.5047113        |
    ## 
    ## 
    ## Table: 30-34's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0012638        |
    ## |Much less often     |0.1330174        |
    ## |Much more often     |0.0913112        |
    ## |Somewhat less often |0.1573460        |
    ## |Somewhat more often |0.1143760        |
    ## |The same as before  |0.5026856        |
    ## 
    ## 
    ## Table: 60-64's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0025300        |
    ## |Much less often     |0.1584440        |
    ## |Much more often     |0.0594560        |
    ## |Somewhat less often |0.1514864        |
    ## |Somewhat more often |0.0882353        |
    ## |The same as before  |0.5398482        |
    ## 
    ## 
    ## Table: 50-54's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0021781        |
    ## |Much less often     |0.1393956        |
    ## |Much more often     |0.0620746        |
    ## |Somewhat less often |0.1668935        |
    ## |Somewhat more often |0.0898448        |
    ## |The same as before  |0.5396134        |
    ## 
    ## 
    ## Table: 18-24's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0016447        |
    ## |Much less often     |0.1244518        |
    ## |Much more often     |0.0940241        |
    ## |Somewhat less often |0.1694079        |
    ## |Somewhat more often |0.1293860        |
    ## |The same as before  |0.4810855        |
    ## 
    ## 
    ## Table: 45-49's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0008382        |
    ## |Much less often     |0.1411009        |
    ## |Much more often     |0.0676167        |
    ## |Somewhat less often |0.1514389        |
    ## |Somewhat more often |0.1036602        |
    ## |The same as before  |0.5353451        |
    ## 
    ## 
    ## Table: 40-44's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0024155        |
    ## |Much less often     |0.1440217        |
    ## |Much more often     |0.0745773        |
    ## |Somewhat less often |0.1654589        |
    ## |Somewhat more often |0.1008454        |
    ## |The same as before  |0.5126812        |
    ## 
    ## 
    ## Table: 75+'s cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0070621        |
    ## |Much less often     |0.1525424        |
    ## |Much more often     |0.0459040        |
    ## |Somewhat less often |0.1341808        |
    ## |Somewhat more often |0.0819209        |
    ## |The same as before  |0.5783898        |
    ## 
    ## 
    ## Table: 25-29's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0029424        |
    ## |Much less often     |0.1311475        |
    ## |Much more often     |0.0748214        |
    ## |Somewhat less often |0.1555275        |
    ## |Somewhat more often |0.1361917        |
    ## |The same as before  |0.4993695        |
    ## 
    ## 
    ## Table: 70-74's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0033069        |
    ## |Much less often     |0.1537698        |
    ## |Much more often     |0.0565476        |
    ## |Somewhat less often |0.1352513        |
    ## |Somewhat more often |0.0770503        |
    ## |The same as before  |0.5740741        |

# Gender Comaprios for Caash frequecy use during and before covid 19

``` r
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
```

    ## 
    ## 
    ## Table: Female's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0015037        |
    ## |Much less often     |0.1438203        |
    ## |Much more often     |0.0730016        |
    ## |Somewhat less often |0.1568685        |
    ## |Somewhat more often |0.1031238        |
    ## |The same as before  |0.5216822        |
    ## 
    ## 
    ## Table: Male's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0028776        |
    ## |Much less often     |0.1422069        |
    ## |Much more often     |0.0661853        |
    ## |Somewhat less often |0.1557055        |
    ## |Somewhat more often |0.1011877        |
    ## |The same as before  |0.5318370        |
    ## 
    ## 
    ## Table: Other, non-binary's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Much more often     |0.0270270        |
    ## |Somewhat less often |0.2162162        |
    ## |Somewhat more often |0.1081081        |
    ## |The same as before  |0.6486486        |

# Activity Status Comaprios for Caash frequecy use during and before covid 19

``` r
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
```

    ## 
    ## 
    ## Table: Without a professional activity or student's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0027929        |
    ## |Much less often     |0.1431541        |
    ## |Much more often     |0.0616394        |
    ## |Somewhat less often |0.1525721        |
    ## |Somewhat more often |0.0934658        |
    ## |The same as before  |0.5463757        |
    ## 
    ## 
    ## Table: Employee's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0016910        |
    ## |Much less often     |0.1466052        |
    ## |Much more often     |0.0722009        |
    ## |Somewhat less often |0.1607481        |
    ## |Somewhat more often |0.1065847        |
    ## |The same as before  |0.5121701        |
    ## 
    ## 
    ## Table: Self-employed's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0021268        |
    ## |Much less often     |0.1263292        |
    ## |Much more often     |0.0865589        |
    ## |Somewhat less often |0.1507869        |
    ## |Somewhat more often |0.1135687        |
    ## |The same as before  |0.5206295        |
    ## 
    ## 
    ## Table: NA's cash freq ratio
    ## 
    ## |Use.of.cash |
    ## |:-----------|

# EDUCATION Comaprios for Caash frequecy use during and before covid 19

``` r
Covid.edu<- data.frame(Covid, EDUCATION= df$EDUCATION)

for(edu in unique(Covid.edu$EDUCATION)){
  dd<- Covid.edu%>%
    filter(EDUCATION==edu)%>%
    drop_na()
  
  pp<-prop.table(table(dd$QQ19A))

  covid_dist<- data.frame( Use.of.cash= pp)

  print(kable(covid_dist, align = "l", caption =paste0(edu, "'s cash freq ratio")))
}
```

    ## 
    ## 
    ## Table: Upper/post-secondary education's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0019308        |
    ## |Much less often     |0.1379408        |
    ## |Much more often     |0.0693395        |
    ## |Somewhat less often |0.1515134        |
    ## |Somewhat more often |0.0977341        |
    ## |The same as before  |0.5415413        |
    ## 
    ## 
    ## Table: University/PhD/research's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0019805        |
    ## |Much less often     |0.1604832        |
    ## |Much more often     |0.0696462        |
    ## |Somewhat less often |0.1698574        |
    ## |Somewhat more often |0.1008054        |
    ## |The same as before  |0.4972274        |
    ## 
    ## 
    ## Table: Primary/lower secondary education's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0031424        |
    ## |Much less often     |0.1175546        |
    ## |Much more often     |0.0707042        |
    ## |Somewhat less often |0.1395515        |
    ## |Somewhat more often |0.1164119        |
    ## |The same as before  |0.5526353        |
    ## 
    ## 
    ## Table: NA's cash freq ratio
    ## 
    ## |Use.of.cash |
    ## |:-----------|

# HHSIZE Comaprios for Caash frequecy use during and before covid 19

``` r
Covid.hhsize<- data.frame(Covid, HHSIZE= df$HHSIZE)

for(hhsize in unique(Covid.hhsize$HHSIZE)){
  dd<- Covid.hhsize%>%
    filter(HHSIZE==hhsize)%>%
    drop_na()
  
  pp<-prop.table(table(dd$QQ19A))

  covid_dist<- data.frame( Use.of.cash= pp)

  print(kable(covid_dist, align = "l", caption =paste0(hhsize, "'s cash freq ratio")))
}
```

    ## 
    ## 
    ## Table: 2's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0025318        |
    ## |Much less often     |0.1511654        |
    ## |Much more often     |0.0598704        |
    ## |Somewhat less often |0.1525802        |
    ## |Somewhat more often |0.0953161        |
    ## |The same as before  |0.5385360        |
    ## 
    ## 
    ## Table: 4's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0019973        |
    ## |Much less often     |0.1411451        |
    ## |Much more often     |0.0777630        |
    ## |Somewhat less often |0.1648469        |
    ## |Somewhat more often |0.1098535        |
    ## |The same as before  |0.5043941        |
    ## 
    ## 
    ## Table: 1's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0026068        |
    ## |Much less often     |0.1501810        |
    ## |Much more often     |0.0590876        |
    ## |Somewhat less often |0.1459812        |
    ## |Somewhat more often |0.0887762        |
    ## |The same as before  |0.5533671        |
    ## 
    ## 
    ## Table: 5's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0024255        |
    ## |Much less often     |0.1243936        |
    ## |Much more often     |0.0928621        |
    ## |Somewhat less often |0.1604297        |
    ## |Somewhat more often |0.1115731        |
    ## |The same as before  |0.5083160        |
    ## 
    ## 
    ## Table: 3's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0012273        |
    ## |Much less often     |0.1323218        |
    ## |Much more often     |0.0785451        |
    ## |Somewhat less often |0.1619993        |
    ## |Somewhat more often |0.1135780        |
    ## |The same as before  |0.5123285        |
    ## 
    ## 
    ## Table: NA's cash freq ratio
    ## 
    ## |Use.of.cash |
    ## |:-----------|

# INCOME Comaprios for Caash frequecy use during and before covid 19

``` r
Covid.income<- data.frame(Covid, INCOME= df$INCOME)

for(income in unique(Covid.income$INCOME)){
  dd<- Covid.income%>%
    filter(INCOME==income)%>%
    drop_na()
  
  pp<-prop.table(table(dd$QQ19A))

  covid_dist<- data.frame( Use.of.cash= pp)

  print(kable(covid_dist, align = "l", caption =paste0(income, "'s cash freq ratio")))
}
```

    ## 
    ## 
    ## Table: NA's cash freq ratio
    ## 
    ## |Use.of.cash |
    ## |:-----------|
    ## 
    ## 
    ## Table: Between EUR 2,501 and EUR 4,000's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0016422        |
    ## |Much less often     |0.1447861        |
    ## |Much more often     |0.0675121        |
    ## |Somewhat less often |0.1713347        |
    ## |Somewhat more often |0.0989873        |
    ## |The same as before  |0.5157376        |
    ## 
    ## 
    ## Table: More than EUR 4,000's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0018116        |
    ## |Much less often     |0.1820652        |
    ## |Much more often     |0.0648551        |
    ## |Somewhat less often |0.1769928        |
    ## |Somewhat more often |0.0795290        |
    ## |The same as before  |0.4947464        |
    ## 
    ## 
    ## Table: Between EUR 751 and EUR 1,500's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0009363        |
    ## |Much less often     |0.1288122        |
    ## |Much more often     |0.0793205        |
    ## |Somewhat less often |0.1405832        |
    ## |Somewhat more often |0.1143660        |
    ## |The same as before  |0.5359818        |
    ## 
    ## 
    ## Table: Between EUR 1,501 and EUR 2,500's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0022001        |
    ## |Much less often     |0.1377277        |
    ## |Much more often     |0.0635396        |
    ## |Somewhat less often |0.1523365        |
    ## |Somewhat more often |0.1097421        |
    ## |The same as before  |0.5344539        |
    ## 
    ## 
    ## Table: EUR 750 or less's cash freq ratio
    ## 
    ## |Use.of.cash.Var1    |Use.of.cash.Freq |
    ## |:-------------------|:----------------|
    ## |Don't know          |0.0029326        |
    ## |Much less often     |0.1198680        |
    ## |Much more often     |0.0986070        |
    ## |Somewhat less often |0.1180352        |
    ## |Somewhat more often |0.1147361        |
    ## |The same as before  |0.5458211        |

# Less cash table

``` r
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
```

|                     | Advantage.Rating |
| :------------------ | :--------------- |
| difficult.withdraw  | 0.0892362        |
| fear.virus          | 0.2836736        |
| cash.not.accepted   | 0.1078901        |
| advice.not.use.cash | 0.2965297        |
| gover.recomm        | 0.2507352        |
| electronic.better   | 0.5851609        |
| new.means           | 0.1159566        |
| other.reason        | 0.0864633        |
| dont.know           | 0.0013444        |

Less Cash

The table shows the percentage in each caterory as to why people used
less cash during covid 19. .eg 58.51% of people used less cash since
electronic method was much better. Now lets break the analysis by
demographic

\#AGE Comaprios for Caash frequecy use during and before covid 19

``` r
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
```

    ## 
    ## 
    ## Table: 64-69's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0807237        |
    ## |fear.virus          |0.3020181        |
    ## |cash.not.accepted   |0.1016006        |
    ## |advice.not.use.cash |0.3354210        |
    ## |gover.recomm        |0.2769659        |
    ## |electronic.better   |0.5775922        |
    ## |new.means           |0.0828114        |
    ## |other.reason        |0.0946416        |
    ## |dont.know           |0.0006959        |
    ## 
    ## 
    ## Table: 55-59's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.1099001        |
    ## |fear.virus          |0.2779292        |
    ## |cash.not.accepted   |0.0962761        |
    ## |advice.not.use.cash |0.2888283        |
    ## |gover.recomm        |0.2370572        |
    ## |electronic.better   |0.5903724        |
    ## |new.means           |0.1226158        |
    ## |other.reason        |0.0881017        |
    ## |dont.know           |0.0009083        |
    ## 
    ## 
    ## Table: 35-39's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0916464        |
    ## |fear.virus          |0.2887267        |
    ## |cash.not.accepted   |0.1111111        |
    ## |advice.not.use.cash |0.2627737        |
    ## |gover.recomm        |0.2311436        |
    ## |electronic.better   |0.5920519        |
    ## |new.means           |0.1362530        |
    ## |other.reason        |0.0867802        |
    ## |dont.know           |0.0016221        |
    ## 
    ## 
    ## Table: 30-34's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0837867        |
    ## |fear.virus          |0.2872688        |
    ## |cash.not.accepted   |0.1142546        |
    ## |advice.not.use.cash |0.2927095        |
    ## |gover.recomm        |0.2437432        |
    ## |electronic.better   |0.5560392        |
    ## |new.means           |0.1229597        |
    ## |other.reason        |0.0881393        |
    ## |dont.know           |0.0000000        |
    ## 
    ## 
    ## Table: 60-64's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0816327        |
    ## |fear.virus          |0.2979592        |
    ## |cash.not.accepted   |0.1010204        |
    ## |advice.not.use.cash |0.3163265        |
    ## |gover.recomm        |0.2612245        |
    ## |electronic.better   |0.6020408        |
    ## |new.means           |0.0969388        |
    ## |other.reason        |0.0816327        |
    ## |dont.know           |0.0010204        |
    ## 
    ## 
    ## Table: 50-54's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.1022222        |
    ## |fear.virus          |0.2915556        |
    ## |cash.not.accepted   |0.1013333        |
    ## |advice.not.use.cash |0.2640000        |
    ## |gover.recomm        |0.2515556        |
    ## |electronic.better   |0.5600000        |
    ## |new.means           |0.1226667        |
    ## |other.reason        |0.0933333        |
    ## |dont.know           |0.0026667        |
    ## 
    ## 
    ## Table: 18-24's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0736940        |
    ## |fear.virus          |0.2462687        |
    ## |cash.not.accepted   |0.1380597        |
    ## |advice.not.use.cash |0.3227612        |
    ## |gover.recomm        |0.2397388        |
    ## |electronic.better   |0.5746269        |
    ## |new.means           |0.1473881        |
    ## |other.reason        |0.0951493        |
    ## |dont.know           |0.0009328        |
    ## 
    ## 
    ## Table: 45-49's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0888252        |
    ## |fear.virus          |0.2683859        |
    ## |cash.not.accepted   |0.1079274        |
    ## |advice.not.use.cash |0.2846227        |
    ## |gover.recomm        |0.2483286        |
    ## |electronic.better   |0.5807068        |
    ## |new.means           |0.1337154        |
    ## |other.reason        |0.0773639        |
    ## |dont.know           |0.0009551        |
    ## 
    ## 
    ## Table: 40-44's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0936585        |
    ## |fear.virus          |0.2634146        |
    ## |cash.not.accepted   |0.1170732        |
    ## |advice.not.use.cash |0.2682927        |
    ## |gover.recomm        |0.2214634        |
    ## |electronic.better   |0.6156098        |
    ## |new.means           |0.1102439        |
    ## |other.reason        |0.0800000        |
    ## |dont.know           |0.0000000        |
    ## 
    ## 
    ## Table: 75+'s less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0960591        |
    ## |fear.virus          |0.3325123        |
    ## |cash.not.accepted   |0.0714286        |
    ## |advice.not.use.cash |0.2733990        |
    ## |gover.recomm        |0.2980296        |
    ## |electronic.better   |0.5640394        |
    ## |new.means           |0.0985222        |
    ## |other.reason        |0.0886700        |
    ## |dont.know           |0.0049261        |
    ## 
    ## 
    ## Table: 25-29's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0835777        |
    ## |fear.virus          |0.2551320        |
    ## |cash.not.accepted   |0.1260997        |
    ## |advice.not.use.cash |0.3416422        |
    ## |gover.recomm        |0.2214076        |
    ## |electronic.better   |0.6026393        |
    ## |new.means           |0.1158358        |
    ## |other.reason        |0.0674487        |
    ## |dont.know           |0.0000000        |
    ## 
    ## 
    ## Table: 70-74's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0869565        |
    ## |fear.virus          |0.3112128        |
    ## |cash.not.accepted   |0.0926773        |
    ## |advice.not.use.cash |0.3043478        |
    ## |gover.recomm        |0.2986270        |
    ## |electronic.better   |0.6041190        |
    ## |new.means           |0.0938215        |
    ## |other.reason        |0.0869565        |
    ## |dont.know           |0.0045767        |

# income Comaprios for Caash frequecy use during and before covid 19

``` r
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
```

    ## 
    ## 
    ## Table: NA's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |NaN              |
    ## |fear.virus          |NaN              |
    ## |cash.not.accepted   |NaN              |
    ## |advice.not.use.cash |NaN              |
    ## |gover.recomm        |NaN              |
    ## |electronic.better   |NaN              |
    ## |new.means           |NaN              |
    ## |other.reason        |NaN              |
    ## |dont.know           |NaN              |
    ## 
    ## 
    ## Table: Between EUR 2,501 and EUR 4,000's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0911977        |
    ## |fear.virus          |0.2972583        |
    ## |cash.not.accepted   |0.1088023        |
    ## |advice.not.use.cash |0.3125541        |
    ## |gover.recomm        |0.2704185        |
    ## |electronic.better   |0.5800866        |
    ## |new.means           |0.1229437        |
    ## |other.reason        |0.0868687        |
    ## |dont.know           |0.0005772        |
    ## 
    ## 
    ## Table: More than EUR 4,000's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0857719        |
    ## |fear.virus          |0.2719475        |
    ## |cash.not.accepted   |0.1412714        |
    ## |advice.not.use.cash |0.3567104        |
    ## |gover.recomm        |0.2643794        |
    ## |electronic.better   |0.6473259        |
    ## |new.means           |0.1271443        |
    ## |other.reason        |0.0852674        |
    ## |dont.know           |0.0015136        |
    ## 
    ## 
    ## Table: Between EUR 751 and EUR 1,500's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0928500        |
    ## |fear.virus          |0.2785501        |
    ## |cash.not.accepted   |0.0824230        |
    ## |advice.not.use.cash |0.2462761        |
    ## |gover.recomm        |0.2199603        |
    ## |electronic.better   |0.5799404        |
    ## |new.means           |0.0933466        |
    ## |other.reason        |0.0858987        |
    ## |dont.know           |0.0024826        |
    ## 
    ## 
    ## Table: Between EUR 1,501 and EUR 2,500's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0849515        |
    ## |fear.virus          |0.2773058        |
    ## |cash.not.accepted   |0.1080097        |
    ## |advice.not.use.cash |0.3064320        |
    ## |gover.recomm        |0.2554612        |
    ## |electronic.better   |0.5661408        |
    ## |new.means           |0.1201456        |
    ## |other.reason        |0.0776699        |
    ## |dont.know           |0.0006068        |
    ## 
    ## 
    ## Table: EUR 750 or less's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.1001541        |
    ## |fear.virus          |0.2604006        |
    ## |cash.not.accepted   |0.0893683        |
    ## |advice.not.use.cash |0.1972265        |
    ## |gover.recomm        |0.1972265        |
    ## |electronic.better   |0.5269646        |
    ## |new.means           |0.0755008        |
    ## |other.reason        |0.1278891        |
    ## |dont.know           |0.0015408        |

# Gender Comaprios for Caash frequecy use during and before covid 19

``` r
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
```

    ## 
    ## 
    ## Table: Female's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0921116        |
    ## |fear.virus          |0.2950476        |
    ## |cash.not.accepted   |0.1167930        |
    ## |advice.not.use.cash |0.3100500        |
    ## |gover.recomm        |0.2500403        |
    ## |electronic.better   |0.5755767        |
    ## |new.means           |0.1095338        |
    ## |other.reason        |0.0814648        |
    ## |dont.know           |0.0014518        |
    ## 
    ## 
    ## Table: Male's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0858799        |
    ## |fear.virus          |0.2711626        |
    ## |cash.not.accepted   |0.0981735        |
    ## |advice.not.use.cash |0.2817000        |
    ## |gover.recomm        |0.2514928        |
    ## |electronic.better   |0.5953635        |
    ## |new.means           |0.1231120        |
    ## |other.reason        |0.0920267        |
    ## |dont.know           |0.0012294        |
    ## 
    ## 
    ## Table: Other, non-binary's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.250            |
    ## |fear.virus          |0.375            |
    ## |cash.not.accepted   |0.125            |
    ## |advice.not.use.cash |0.375            |
    ## |gover.recomm        |0.250            |
    ## |electronic.better   |0.750            |
    ## |new.means           |0.000            |
    ## |other.reason        |0.000            |
    ## |dont.know           |0.000            |

# Education Comaprios for Caash frequecy use during and before covid 19

``` r
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
```

    ## 
    ## 
    ## Table: Upper/post-secondary education's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0835786        |
    ## |fear.virus          |0.2768295        |
    ## |cash.not.accepted   |0.1059447        |
    ## |advice.not.use.cash |0.3015499        |
    ## |gover.recomm        |0.2458309        |
    ## |electronic.better   |0.5773985        |
    ## |new.means           |0.1094762        |
    ## |other.reason        |0.0833824        |
    ## |dont.know           |0.0019619        |
    ## 
    ## 
    ## Table: University/PhD/research's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0889289        |
    ## |fear.virus          |0.2811751        |
    ## |cash.not.accepted   |0.0965228        |
    ## |advice.not.use.cash |0.2907674        |
    ## |gover.recomm        |0.2398082        |
    ## |electronic.better   |0.6155076        |
    ## |new.means           |0.1139089        |
    ## |other.reason        |0.0835332        |
    ## |dont.know           |0.0007994        |
    ## 
    ## 
    ## Table: Primary/lower secondary education's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.1061111        |
    ## |fear.virus          |0.3100000        |
    ## |cash.not.accepted   |0.1450000        |
    ## |advice.not.use.cash |0.2983333        |
    ## |gover.recomm        |0.2950000        |
    ## |electronic.better   |0.5227778        |
    ## |new.means           |0.1400000        |
    ## |other.reason        |0.1033333        |
    ## |dont.know           |0.0011111        |
    ## 
    ## 
    ## Table: NA's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |NaN              |
    ## |fear.virus          |NaN              |
    ## |cash.not.accepted   |NaN              |
    ## |advice.not.use.cash |NaN              |
    ## |gover.recomm        |NaN              |
    ## |electronic.better   |NaN              |
    ## |new.means           |NaN              |
    ## |other.reason        |NaN              |
    ## |dont.know           |NaN              |

# Activity status Comaprios for Caash frequecy use during and before covid 19

``` r
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
```

    ## 
    ## 
    ## Table: Upper/post-secondary education's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |NaN              |
    ## |fear.virus          |NaN              |
    ## |cash.not.accepted   |NaN              |
    ## |advice.not.use.cash |NaN              |
    ## |gover.recomm        |NaN              |
    ## |electronic.better   |NaN              |
    ## |new.means           |NaN              |
    ## |other.reason        |NaN              |
    ## |dont.know           |NaN              |
    ## 
    ## 
    ## Table: University/PhD/research's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |NaN              |
    ## |fear.virus          |NaN              |
    ## |cash.not.accepted   |NaN              |
    ## |advice.not.use.cash |NaN              |
    ## |gover.recomm        |NaN              |
    ## |electronic.better   |NaN              |
    ## |new.means           |NaN              |
    ## |other.reason        |NaN              |
    ## |dont.know           |NaN              |
    ## 
    ## 
    ## Table: Primary/lower secondary education's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |NaN              |
    ## |fear.virus          |NaN              |
    ## |cash.not.accepted   |NaN              |
    ## |advice.not.use.cash |NaN              |
    ## |gover.recomm        |NaN              |
    ## |electronic.better   |NaN              |
    ## |new.means           |NaN              |
    ## |other.reason        |NaN              |
    ## |dont.know           |NaN              |
    ## 
    ## 
    ## Table: NA's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |NaN              |
    ## |fear.virus          |NaN              |
    ## |cash.not.accepted   |NaN              |
    ## |advice.not.use.cash |NaN              |
    ## |gover.recomm        |NaN              |
    ## |electronic.better   |NaN              |
    ## |new.means           |NaN              |
    ## |other.reason        |NaN              |
    ## |dont.know           |NaN              |

# HHSIZE Comaprios for Caash frequecy use during and before covid 19

``` r
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
```

    ## 
    ## 
    ## Table: 2's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0858053        |
    ## |fear.virus          |0.2853641        |
    ## |cash.not.accepted   |0.1073793        |
    ## |advice.not.use.cash |0.3064477        |
    ## |gover.recomm        |0.2520226        |
    ## |electronic.better   |0.6006374        |
    ## |new.means           |0.1086051        |
    ## |other.reason        |0.0835989        |
    ## |dont.know           |0.0012258        |
    ## 
    ## 
    ## Table: 4's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0931245        |
    ## |fear.virus          |0.2798085        |
    ## |cash.not.accepted   |0.1022628        |
    ## |advice.not.use.cash |0.2654482        |
    ## |gover.recomm        |0.2354221        |
    ## |electronic.better   |0.5935596        |
    ## |new.means           |0.1279373        |
    ## |other.reason        |0.0805048        |
    ## |dont.know           |0.0004352        |
    ## 
    ## 
    ## Table: 1's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0772616        |
    ## |fear.virus          |0.2929095        |
    ## |cash.not.accepted   |0.1207824        |
    ## |advice.not.use.cash |0.3256724        |
    ## |gover.recomm        |0.2674817        |
    ## |electronic.better   |0.5887531        |
    ## |new.means           |0.1100244        |
    ## |other.reason        |0.0855746        |
    ## |dont.know           |0.0014670        |
    ## 
    ## 
    ## Table: 5's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.1046229        |
    ## |fear.virus          |0.2579075        |
    ## |cash.not.accepted   |0.1046229        |
    ## |advice.not.use.cash |0.2700730        |
    ## |gover.recomm        |0.2165450        |
    ## |electronic.better   |0.5754258        |
    ## |new.means           |0.1350365        |
    ## |other.reason        |0.0985401        |
    ## |dont.know           |0.0036496        |
    ## 
    ## 
    ## Table: 3's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |0.0959060        |
    ## |fear.virus          |0.2854435        |
    ## |cash.not.accepted   |0.1050038        |
    ## |advice.not.use.cash |0.2945413        |
    ## |gover.recomm        |0.2596664        |
    ## |electronic.better   |0.5549659        |
    ## |new.means           |0.1141016        |
    ## |other.reason        |0.0921152        |
    ## |dont.know           |0.0015163        |
    ## 
    ## 
    ## Table: NA's less cash freq ratio
    ## 
    ## |                    |Advantage.Rating |
    ## |:-------------------|:----------------|
    ## |difficult.withdraw  |NaN              |
    ## |fear.virus          |NaN              |
    ## |cash.not.accepted   |NaN              |
    ## |advice.not.use.cash |NaN              |
    ## |gover.recomm        |NaN              |
    ## |electronic.better   |NaN              |
    ## |new.means           |NaN              |
    ## |other.reason        |NaN              |
    ## |dont.know           |NaN              |

## Table 5

``` r
card.advg<- space%>%
select(QQ13B_1,QQ13B_2,QQ13B_3,QQ13B_4,QQ13B_5,
       QQ13B_6, QQ13B_7, QQ13B_8,QQ13B_9,QQ13B_10)
names(card.advg)<- c("card.acceptance", "card.faster","card.easier",
                "card.safer","no_worry_carrying_cash","aware_spending",
                "other_advantage","no_advantage","do_not_use_card",
                "dont_know")
#Covariance matrix 
card.advg2<- na.omit(card.advg)
```

``` r
corrplot(cov(card.advg2), method = 'number',col = COL2('RdYlBu', 2),
         type = 'lower', tl.col='black')
```

![Covariance Matrix for Ratings of credit
Cards](README_files/figure-gfm/unnamed-chunk-63-1.png) Covariance
measures the degree to which two variables change together; a positive
covariance indicates that when one variable increases, the other tends
to increase as well, while a negative covariance suggests that when one
variable increases, the other tends to decrease. Here, for instance, the
covariance between “card.acceptance” and “card.faster” is 0.1699,
implying a positive relationship where higher acceptance of cards is
associated with a faster card transaction experience. Conversely, the
negative covariance between “card.acceptance” and
“no\_worry\_carrying\_cash” (-0.0124) suggests that as card acceptance
increases, the tendency to worry about carrying cash decreases. This
table provides insights into how these variables move in relation to
each other, but it doesn’t standardize for the scales of the variables,
making it difficult to compare the strengths of these relationships
directly.

``` r
#by months 
paymethod<- space%>%
  select(QA7A_1, QA7AI_1, QA7AII_1)%>%
  mutate(QA7A_1= case_when(QA7A_1==1~   "Cash",
                           QA7A_1==2~   "Card",
                           QA7A_1==3~   "Mobile phone app",
                           QA7A_1==4~   "Bank cheque",
                           QA7A_1==5~   "Credit transfer",
                          QA7A_1==6~    "Loyalty points",
                          QA7A_1==7~    "Other",
                         # QA7A_1==999999~  "Don't know",
),
QA7AI_1= case_when(
  QA7AI_1==1~"By inserting the card into a terminal",
  QA7AI_1==2~   "Using contactless technology",
  #QA7AI_1==999999  ~"Don't know"

),
QA7AII_1=case_when(
  QA7AII_1== 1~ "Using my bank's mobile application",
QA7AII_1==2~    "Using ApplePay",
QA7AII_1==3~    "Using GooglePay",
QA7AII_1==4~    "Other",
QA7AII_1==5~    "Payconiq by Bancontact",
QA7AII_1==6~    "mTasku",
QA7AII_1==8 ~"Jiffy",
QA7AII_1==9~    "Swedbank mobila lietotne",
QA7AII_1==10~   "MoQ",
QA7AII_1==11~   "Digicash",
QA7AII_1==12~   "Bank of Valletta",
QA7AII_1==13~   "Bankomatkarte mobil",
QA7AII_1==14~   "MBway",
QA7AII_1==15~   "NLB pay",
QA7AII_1==16~   "MobilePay TB",
QA7AII_1==17~   "MobilePay",
QA7AII_1==18~   "Swedbanki mobiilipank",
QA7AII_1==19~   "Viva",
QA7AII_1==20~   "Samsung Pay",
QA7AII_1==22~   "Satispay",
QA7AII_1==23~   "Citadele mobila aplikacija",
QA7AII_1==24~   "Revolut",
QA7AII_1==25~   "Bluecode",
QA7AII_1==26~   "mDenarnic@",
QA7AII_1==27~   "mBank SK",
QA7AII_1==28~   "Siirto",
QA7AII_1==29~   "Paypal",
QA7AII_1==30~   "Android pay",
QA7AII_1==31~   "SEB mobila lietotne",
QA7AII_1==32~   "kWallet",
QA7AII_1==33~   "Wave2Pay",
QA7AII_1==34~   "VÚB Mobil Banking",
QA7AII_1==35~   "my Alpha wallet",
QA7AII_1==36~   "Lydia",
QA7AII_1==37~   "Mobilly (parking, train tickets)",
QA7AII_1==38~   "ZOIN",
QA7AII_1==39~   "mBills",
QA7AII_1==40~   "i-bank Pay",
QA7AII_1==41~   "Pumpkin",
QA7AII_1==42~   "Paylib",
QA7AII_1==43~   "Pivo",
#QA7AII_1==999999~" Don't know",
  
))

paymethod1<- paymethod%>%
  drop_na(QA7A_1)
prop.table(table(paymethod1$QA7A_1))%>%
  data.frame()%>%
  arrange(desc(Freq))%>%
  kable(align = "l", col.names = c("Instrument of POS payment","Population"),
        caption = "Instrument of POS payment by population")
```

| Instrument of POS payment | Population |
| :------------------------ | :--------- |
| Cash                      | 0.5918299  |
| Card                      | 0.3478498  |
| Mobile phone app          | 0.0329047  |
| Credit transfer           | 0.0130103  |
| Other                     | 0.0084612  |
| Loyalty points            | 0.0031540  |
| Bank cheque               | 0.0027901  |

Instrument of POS payment by population

The table presents the distribution of the population using various
methods of point-of-sale (POS) payments. It outlines the proportions of
individuals or users utilizing different payment instruments, including
cash, cards, mobile phone apps, credit transfers, other methods, loyalty
points, and bank cheques. For instance, approximately 59.18% of the
population prefers cash as their POS payment method, while 34.79% opt
for cards, and a smaller percentage, 3.29%, rely on mobile phone apps.
Credit transfers, other methods, loyalty points, and bank cheques are
used by 1.30%, 0.85%, 0.32%, and 0.28% of the population, respectively.
This table provides valuable insights into the payment preferences
within the population, which can be instrumental for businesses and
policymakers in tailoring their financial services and strategies to
meet the diverse needs of their customer base.

``` r
paymethod2<- paymethod%>%
  drop_na(QA7AI_1)
prop.table(table(paymethod2$QA7AI_1))%>%
  data.frame()%>%
  arrange(desc(Freq))%>%
  kable(align = "l", col.names = c("Card payment","Population"),
        caption = "Card payment by population")
```

| Card payment                          | Population |
| :------------------------------------ | :--------- |
| Using contactless technology          | 0.6406756  |
| By inserting the card into a terminal | 0.3593244  |

Card payment by population

The table illustrates the distribution of card payment methods within a
population. It showcases the proportions of individuals utilizing two
distinct ways of making payments with their cards. Approximately 64.07%
of the population prefers the convenience of contactless technology for
their card payments, allowing for quick and secure transactions by
tapping the card on a terminal. In contrast, a smaller portion,
accounting for 35.93% of the population, opts for the traditional method
of card payment by inserting the card into a terminal. This table
highlights the growing popularity of contactless payments, which offer
speed and ease of use, reflecting a shift in consumer preferences and
technology adoption trends within the payment landscape.

``` r
paymethod3<- paymethod%>%
  drop_na(QA7AII_1)
prop.table(table(paymethod3$QA7AII_1))%>%
  data.frame()%>%
  arrange(desc(Freq))%>%
  kable(align = "l", col.names = c("Mobile payment","Population"),
        caption = "Mobile payment  by population")
```

| Mobile payment                     | Population |
| :--------------------------------- | :--------- |
| Using ApplePay                     | 0.2730627  |
| Using my bank’s mobile application | 0.2601476  |
| Using GooglePay                    | 0.1771218  |
| Other                              | 0.0950185  |
| MobilePay                          | 0.0396679  |
| MBway                              | 0.0359779  |
| Samsung Pay                        | 0.0193727  |
| Satispay                           | 0.0138376  |
| Payconiq by Bancontact             | 0.0129151  |
| Revolut                            | 0.0129151  |
| Digicash                           | 0.0119926  |
| Swedbank mobila lietotne           | 0.0083026  |
| Swedbanki mobiilipank              | 0.0073801  |
| Paypal                             | 0.0046125  |
| Android pay                        | 0.0036900  |
| Bank of Valletta                   | 0.0027675  |
| Bankomatkarte mobil                | 0.0027675  |
| i-bank Pay                         | 0.0027675  |
| Bluecode                           | 0.0018450  |
| mTasku                             | 0.0018450  |
| Paylib                             | 0.0018450  |
| Pivo                               | 0.0018450  |
| Viva                               | 0.0018450  |
| Citadele mobila aplikacija         | 0.0009225  |
| Lydia                              | 0.0009225  |
| mBank SK                           | 0.0009225  |
| Mobilly (parking, train tickets)   | 0.0009225  |
| NLB pay                            | 0.0009225  |
| SEB mobila lietotne                | 0.0009225  |
| Siirto                             | 0.0009225  |

Mobile payment by population

The table presents the distribution of mobile payment methods within a
population, highlighting the preferences of individuals when it comes to
making payments using mobile technology. Among the various options,
ApplePay stands out as the most popular choice, with approximately
27.31% of the population opting for this mobile payment solution.
Following closely is the use of the bank’s mobile application,
accounting for 26.01%. GooglePay is also a commonly used method, chosen
by 17.71% of users. Other mobile payment options, such as MobilePay,
MBway, and Samsung Pay, are selected by smaller proportions of the
population, ranging from 3.60% to 1.93%. This table reveals the diverse
landscape of mobile payment preferences, reflecting the influence of
both platform-specific and region-specific solutions, which can provide
valuable insights for businesses and financial institutions seeking to
cater to their customers’ varied needs in the mobile payment sector.
