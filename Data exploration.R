library(readr)
library(ggplot2)
## Read dataframe (read_csv reads data as Tibbles)
HH_clean<- read.csv("C:/Users/user/Desktop/Data/New folder/FinRosEI_Clean.csv")

head(HH_clean)
library(dplyr)
filter(HH_clean, sex =="Female") 
fem.Bus<-arrange(filter(HH_clean, sex =="Female"), ent_bus_i)

#introduce new variable for inclome level

income_level<-summary(select(mutate(HH_clean, income_level = cut(rc_job_income, breaks = c(-999,0, 150, 250, 350, 450, 550, 650, 850,1000, 1500, 2000, 2500))), 
       rc_job_income, income_level))

ggplot(data = HH_clean, mapping = aes(x = rc_job_income)) +
  geom_histogram()
summary(rc_job_income)

HH_clean%>% filter(!rc_job_income==0)%>% ggplot(aes(x = rc_job_income)) +  
  geom_histogram() 
  
  
#how many man and women 
sum(HH_clean$sex=="Female") #16642
sum(HH_clean$sex=="Male")#16206

#mean income  broken down by gender
gender.income <- aggregate(rc_job_income ~ sex, FUN=mean, na.rm=TRUE, data=HH_clean)
#mean income for female is 185.20 and for male is 296.20

colnames(gender.income) <- c("Gender", "Income")
library(knitr)
kable(gender.income)

round((gender.income[2,2] - gender.income[1,2]), 2)#male income - female income
#that women are making 
round((100 *(gender.income[1,2])/(gender.income[2,2])), 2)# female income/ make income 
#% women are making 63.17% of what men make, on average.

#plot
ggplot(HH_clean, aes(x = sex, y = rc_job_income, fill = sex)) +  
  geom_boxplot() +
  ggtitle("Mean Income by Gender")+
  labs(x="Gender", y="Income")

#t-test to find the statistical signifigance of this difference#
test.gender <- t.test(HH_clean$rc_job_income ~ HH_clean$sex, conf.level = 0.99)
test.gender
round(test.gender$p.value, 4)

#is there any difference on MEAN earnings based on the sex and marital status 
Mean.sex.income<-summarise(group_by(HH_clean, sex, marital), 
          n = n(), 
          meanincome = mean(rc_job_income, na.rm=TRUE), 
          sdincome=sd(rc_job_income, na.rm=TRUE), 
          missing=sum(is.na(rc_job_income)))


Plot_income<-HH_clean %>% ggplot(aes(x = sex, y=rc_job_income, colour = marital)) + 
  geom_boxplot()

####Labor Force Status
str(HH_clean$lab_status1)

Mean.sex.income.lab<-summarise(group_by(HH_clean, sex, marital,lab_status1), 
          n = n(), 
          meanincome = mean(rc_job_income, na.rm=TRUE), 
          sdincome=sd(rc_job_income, na.rm=TRUE), 
          missing=sum(is.na(rc_job_income)))

HH_clean %>% filter(!is.na(lab_status1)) %>%
  ggplot(aes(x = sex, y=rc_job_income, colour = marital)) + 
  geom_boxplot()+facet_grid(lab_status1~.)+ 
  ggtitle("Income differentials by marital and labor force staus")

#plot
Plot_income_sta<-Mean.sex.income.lab %>% filter(!is.na(lab_status1)) %>%
  ggplot(aes(x = sex, y=meanincome, colour = marital)) + 
  geom_count()+facet_grid(lab_status1~.)+ 
  ggtitle("Income differentials by marital and labor force staus")

## Income and job type
library(plyr)
str(HH_clean$job_emp)
job_emp<-Fin.Ros.EI$job_emp
job_emp<-as.factor(job_emp)
job_emp[job_emp == -999] <- NA
job_emp[job_emp == -998] <- NA
job_emp[job_emp == 999] <- NA

job_emp<-revalue(job_emp, c("1"="Government", "2"="State-owned enterprise",
                            "3"="Private company ", "4"="International organization ", "5"="NGO",
                            "6"="Self-employed ","7"="Other " ))

HH_clean$job_emp<-job_emp

HH_clean %>% filter(!is.na(job_emp)) %>%
  ggplot(aes(x = job_emp, y=rc_job_income,colour = sex)) + 
  geom_boxplot()+facet_grid(sex~.)+ 
  ggtitle("a")+theme(axis.text.x = element_text(angle =75 , hjust = 1))+
  labs(x = "Job Type", y ="Income",title = "Organisation Type & Paid Work")

HH_clean %>% filter(!is.na(job_emp)) %>%
  ggplot(aes(x = job_emp, y=rc_job_income,colour = sex)) + 
  geom_boxplot()+coord_flip()+
  theme(axis.text.x = element_text(angle =75 , hjust = 1))+
  labs(x = "Job Type", y ="Income",title = "Organisation Type & Paid Work")


#same output different representation of the graph  
HH_clean %>% filter(!is.na(job_emp)) %>%
  ggplot(aes(x = sex, y=rc_job_income,colour = sex)) + 
  geom_boxplot()+facet_grid(job_emp~.)
  ggtitle("a")+theme(axis.text.x = element_text(angle =75 , hjust = 1))+
  labs(x = "Job Type", y ="Income",title = "Organisation Type & Paid Work")


#compare mean income by gender and organisation
meanInc<-dplyr::summarise(group_by(HH_clean, sex, job_emp), 
                   n = n(), 
                   meanincome = mean(rc_job_income, na.rm=TRUE), 
                   sdincome=sd(rc_job_income, na.rm=TRUE), 
                   missing=sum(is.na(rc_job_income)))

#extract all percentage levels in one go
#table difference
table.org <- with(HH_clean, tapply(rc_job_income, INDEX = list(job_emp, sex), FUN = mean, na.rm=TRUE))
table.org  <- transform(table.org, diff = Male - Female, perc = round(100 * ((Female)/(Male)), 2))
table.org  <- table.org[order(table.org$perc), ]
colnames(table.org) <- c("Female", "Male", "Abs Diff", "% Diff")
kable(table.org)

#highet education COMPLETED ed_highest
str(Fin.Ros.EI$ed_highest)#not found 

#*In the last 4 weeks, has $(hh_memberX) looked for work 
HH_clean %>% filter(!is.na(es_active)) %>%
  ggplot(aes(x = es_active, fill = sex)) + 
  geom_bar()+
  theme(axis.text.x = element_text())+
  labs(x = "Gender",title = "Employment Search Information
       /Persons Not in Employment")

#es_months F4) How many months has  been looking for a job or trying to establish
summary(HH_clean$es_months)
#improve cleaning
es_months<-as.numeric(HH_clean$es_months)
summary(es_months)
es_months[es_months == -998] <- NA
HH_clean$es_months<-es_months
summary(HH_clean$es_months)
str(HH_clean$es_months)

#look if gender plays a role here
dplyr::summarise(group_by(HH_clean,sex), 
          n = n(), 
          meanweek= mean(es_months, na.rm=TRUE), 
          missing=sum(is.na(es_months)))

#do people search for job more as they age
# ps_agenot found.. we found different code but same question What is $(ps_first)'s age in years?
str(Fin.Ros.EI$rc_ps_age)

#include variable age from the main dataset
rc_ps_age<-as.numeric(Fin.Ros.EI$rc_ps_age)
summary(rc_ps_age)
HH_clean$rc_ps_age<-rc_ps_age

#plot lm line
HH_clean %>% 
  ggplot(aes(x = rc_ps_age, y=es_months, colour=sex)) + geom_point() + geom_smooth(method = "lm")+
  theme(axis.text.x = element_text())+
  labs(x = "Age",y = "weeks search", title = "Is Age Affecting Job Search")

#plot and check for trends
HH_clean %>% 
  ggplot(aes(x = rc_ps_age, y=es_months, colour=sex)) + geom_point() + geom_smooth()+
  theme(axis.text.x = element_text())+ 
  labs(x = "Age",y = "weeks search", title = "Is Age Affecting Job Search")
summary(HH_clean$es_months)# average month spend on job search 19.82

#man and womes average time spent lookingfor job
dplyr::summarise(group_by(HH_clean, sex), 
          n = n(), 
          meanage = mean(es_months, na.rm=TRUE),
          missing=sum(is.na(es_months)))

#correlation coefficient 
cor(rc_ps_age,es_months, use="complete.obs")

#merital status and age
HH_clean %>% 
  ggplot(aes(x = marital, y=rc_ps_age,colour = sex)) + 
  geom_boxplot()+coord_flip()+
  theme(axis.text.x = element_text(angle =75 , hjust = 1))+
    labs(x = "Merital Status", y ="Age",title = "Age and Merital Status effect")

#reorder and plot We use median age in each marital status to reorder the categories
str(reorder(HH_clean$marital, HH_clean$rc_ps_age, FUN=median, na.rm=TRUE))

#assign in a new meritalR variable
HH_clean$maritalR <- reorder(HH_clean$marital, HH_clean$rc_ps_age, FUN=median, na.rm=TRUE)
HH_clean %>% 
  ggplot(aes(x = maritalR, y=rc_ps_age,colour = sex)) + 
  geom_boxplot()+coord_flip()+
theme(axis.text.x = element_text(angle =75 , hjust = 1))+
  labs(x = "Merital Status", y ="Age",title = "Age and Merital Status effect")
 
#####
#ent_bus_i Have you, alone or with others, already started or are currentlytrying to start a new business, 
#including any self-employment or selling any goods or services to others? (select all)
summary(ent_bus_i)
str(Fin.Ros.EI$ent_bus_i)
ent_bus_i<-as.factor(Fin.Ros.EI$ent_bus_i)
ent_bus_i[ent_bus_i == -999] <- NA
ent_bus_i[ent_bus_i == -998] <- NA
#24244 empty values
#assign those as NA
ent_bus_i[ent_bus_i == ""]<- NA
ent_bus_i<-revalue(ent_bus_i, c("0"="Not involved in business", "1"="Yes,I have 
started a business ","2"="Yes,trying 
                                to start a business"))
HH_clean$ent_bus_i<-ent_bus_i
summary(HH_clean$ent_bus_i)
#plot
HH_clean %>% filter(!is.na(ent_bus_i)) %>%
  ggplot(aes(x = ent_bus_i, fill = sex)) + 
  geom_bar()+
  theme(axis.text.x = element_text())+
  labs(x = "Gender",title = "Have you already started or 
        trying to start a new business")+
  theme(axis.text.x = element_text(angle =75 , hjust = 1, size = 8))

###return the values description to original
str(HH_merged$ent_bus_i)
str(Fin.Ros.EI$ent_bus_i)
ent_bus_i<-as.factor(Fin.Ros.EI$ent_bus_i)
ent_bus_i[ent_bus_i == -999] <- NA
ent_bus_i[ent_bus_i == -998] <- NA
#24244 empty values
#assign those as NA
ent_bus_i[ent_bus_i == ""]<- NA
ent_bus_i<-revalue(ent_bus_i, c("0"="No, I am not currently involved in my own business", "1"="Yes, I have already started a business ","2"="Yes, I am currently trying to start a new business"))
HH_clean$ent_bus_i<-ent_bus_i
summary(HH_clean$ent_bus_i)

#ent_reg Has the business been formally registered with the Kosovo Business Registrati
str(HH_merged$ent_reg)

HH_clean %>% filter(!is.na(ent_reg)) %>%
  ggplot (aes(x=ent_reg,fill = sex)) + geom_bar()+
labs(x = " ",title = "Has the business been formally registered")


#ent_reg_r Why have you not formally registered your business with KBRA
#Use the raw data for easier manipulation  
ent_reg_r<-as.factor(Fin.Ros.EI$ent_reg_r)
str(ent_reg_r)
ent_reg_r[ent_reg_r == -999] <- NA
ent_reg_r[ent_reg_r == -998] <- NA
ent_reg_r[ent_reg_r == ""] <- NA
#add same variable different lables
summary(ent_reg_r)
HH_clean$ent_reg_rX<-ent_reg_r
summary(HH_clean$ent_reg_r)

#first category
HH_clean %>% filter(!is.na(ent_reg_rX)) %>%
  group_by(sex) %>%
  dplyr::summarise(
    perc = sum(ent_reg_rX =="1")/n()*100,
    n = n()
  )%>% 
  ggplot(aes(x = sex, y=perc, fill=sex)) + geom_bar(stat = "identity")+
  xlab("Gender") + 
  ylab("Percent") + 
  ggtitle("Don't know how (or where) to register new business ")

#Thired category in the #ent_reg_r variable and the highest in terms of response
HH_clean %>% filter(!is.na(ent_reg_rX)) %>%
  group_by(sex) %>%
  dplyr::summarise(
    perc = sum(ent_reg_rX =="3")/n()*100,
    n = n()
  )%>% 
  ggplot(aes(x = sex, y=perc, fill=sex)) + geom_bar(stat = "identity")+
  xlab("Gender") + 
  ylab("Percent") + 
  ggtitle("Don't see benefit in doing so ")

#forth category in the #ent_reg_r variable 
HH_clean %>% filter(!is.na(ent_reg_rX)) %>%
  group_by(sex) %>%
  dplyr::summarise(
    perc = sum(ent_reg_rX =="4")/n()*100,
    n = n()
  )%>% 
  ggplot(aes(x = sex, y=perc, fill=sex)) + geom_bar(stat = "identity")+
  xlab("Gender") + 
  ylab("Percent") + 
  ggtitle("Cannot afford to register new business to KBRA ")


#ent_const_coded What is the main obstacle in growing your business?
summary(HH_clean$ent_const_coded) #it is heard to draw any conclusion about this question

#f_discr In your past or present employment, do you feel you are/were treated EQUALLY within the workplace by your employer(s) 
#when compared with members of the opposite sex that you work with? This includes both negative and
summary(HH_clean$f_discr)
f_discr<-HH_clean$f_discr
f_discr[f_discr =="N/A" ] <- NA
summary(f_discr)
HH_clean$f_discr<-f_discr

#percent
HH_clean %>% filter(!is.na(f_discr)) %>%
  group_by(sex) %>%
  dplyr::summarise(
    perc = sum(f_discr =="Yes")/n()*100,
    n = n()
  )

#f_adv Do you feel this unequal treatment has prevented you from advancing professional"
summary(HH_clean$f_adv)
HH_clean %>% filter(!is.na(f_adv)) %>%
  group_by(sex) %>%
  dplyr::summarise(
    perc = sum(f_adv =="Yes")/n()*100,
    n = n()
  )

#f_equal Do you believe that men and women in Kosovo are generally treated equally in the workplace?
summary(HH_clean$f_equal)
str(Fin.Ros.EI$f_equal)
str(HH_clean$f_equal)

attributes(Fin.Ros.EI$f_equal)
f_equal<-as.factor(Fin.Ros.EI$f_equal)
f_equal[f_equal == -999] <- NA
f_equal[f_equal == -998] <- NA
f_equal<-revalue(f_equal, c("1"="Yes", "0"="No"))
summary(f_equal)

HH_clean$f_equal<-f_equal
summary(HH_clean$f_equal)

#calculate percentage of perople who believe that men and women in Kosovo are generally treated equally in the workplace
HH_clean %>% filter(!is.na(f_equal)) %>%
  group_by(sex) %>%
  dplyr::summarise(
    perc = sum(f_equal =="Yes")/n()*100,
    n = n()
  )

#f_discr_f	Has concern about discrimination influenced your career decisions?
summary(HH_clean$f_discr_f)
HH_clean %>% filter(!is.na(f_discr_f)) %>%
  group_by(sex) %>%
  dplyr::summarise(
    perc = sum(f_discr_f =="No")/n()*100,
    n = n()
  )

####Mass media use *tu_mm
str(Fin.Ros.EI$tu_mm)
media<-Fin.Ros.EI%>%select(tu_mm,sex)
region<-HH_clean$region_hr
media$region<-region
summary(Fin.Ros.EI$tu_mm)

##What is the highest level of education completed by
str(Fin.Ros.EI$rc_ed_highest)
attributes(Fin.Ros.EI$rc_ed_highest)
rc_ed_highest<-as.factor(Fin.Ros.EI$rc_ed_highest)
summary(rc_ed_highest)
rc_ed_highest[rc_ed_highest == -999] <- NA
rc_ed_highest[rc_ed_highest == -998] <- NA
library(plyr)
rc_ed_highest<-revalue(rc_ed_highest, c("1"="Not finish primary school", "2"="Primary education",
                                        "3"="Lower secondary education", "4"="Upper secondary - general",
                                        "5"="Upper secondary - vocational", "6"="Post secondary - vocational",
                                        "7"="Tertiary", "8"="Post graduate or Doctorate"))
HH_clean$rc_ed_highest<-rc_ed_highest

HH_clean %>% filter(!is.na(rc_ed_highest)) %>%
  ggplot(aes(x = rc_ed_highest, y=rc_job_income)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle =75 , hjust = 1))+
  labs(x = "Highest level of education completed", y ="Income",title = " Relationship Between Income by Education Level")

#include sex
HH_clean %>% 
  ggplot(aes(x = rc_ed_highest, y=rc_job_income,colour = sex)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle =75 , hjust = 1))+
  labs(x = "Highest level of education completed", y ="Income",title = "Income by Education Level")

HH_clean %>%dplyr::summarise(group_by(rc_ed_highest), 
                 n = n(), 
                 meanincome = mean(rc_job_income, na.rm=TRUE),
                 missing=sum(is.na(rc_job_income)))

#add filter on only employed individuals
HH_clean %>%filter(!is.na(rc_job_income)) %>%
  group_by(rc_ed_highest) %>% 
  mean(rc_job_income)
str(rc_job_income)
