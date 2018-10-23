
#DATA EXPLORATION MCC Time Use Survey 

#read .dta data file
library(haven)
Final.EI<- read_dta("C:/Users/user/Desktop/Data/New folder/Final EI Data Anonymized.dta")

Final.Roster<- read_dta("C:/Users/user/Desktop/Data/New folder/Final Roster Data Anonymized.dta")

Fin.Ros.EI<- read_dta("C:/Users/user/Desktop/Data/New folder/Final Roster EI TUS Merged Data Anonymized.dta")

Final.TUS<- read_dta("C:/Users/user/Desktop/Data/New folder/Final TUS Data Anonymized.dta")

head(names(Final.Roster.Merged))

#tabel of missing values summary
table(rowSums(is.na(Final.Roster.Merged)))
str(Final.Roster.Merged)
summary(Final.Roster.Merged$educ)

Q1<-as.character(Final.Roster.Merged$marital)
Q2<-as.factor(Final.Roster.Merged$marital)
summary(Q2)

#change variable structure
str(Final.Roster.Merged$food)
food<-as.factor(Final.Roster.Merged$food)
summary(food)

str(Final.Roster.Merged$food_likert)
food_infl<-as.factor(Final.Roster.Merged$food_likert)
summary(food_infl)

#assigne -999, -998 as NA
food_infl[food_infl == -999] <- NA
food_infl[food_infl == -998] <- NA

sex<-as.factor(Final.Roster.Merged$f_sex)
levels(sex)
summary(sex)

#two different sex variables, I choose the sex variable over f_sex because there are less missing values 
#and the total value corresponds with the  total number of the sample size. 
sex1<-as.factor(Final.Roster.Merged$sex)
summary(sex1)

#rename levels ofthe factor variable
library(plyr)
sex<-revalue(sex, c("1"="Male", "2"="Female"))
summary(sex)

##### Households gender norm attitudes scale#######

#subset on variables
HH_merged<- Fin.Ros.EI %>% select(hhid_anon, region_hr,sex, marital,nationality, region_t, ps_student,
                                  job_emp, rc_job_income, ue_reas,es_active,es_ia_r,es_months, 
                                  lab_status1, job_act, job_func_f,credit_de,  es_type, ed_field, 
                                  ei_wage, ei_wage_d,ent_bus_i, ent_main, ent_reg, 
                                  ent_reg_r, ent_plan, ent_choice,rc_ent_act, rc_ent_act_text_coded,
                                  ent_sup, ent_ment, ent_const_coded, ent_bus, ent_skill, f_emp, f_discr,
                                  f_discr_pn, f_adv, f_care, f_ccare, f_transp, f_equal,f_discr_f, f_discr_fh_coded, 
                                  gnas_1, gnas_2, gnas_3,gnas_4, gnas_5, gnas_6, gnas_7, gnas_8, gnas_9, gnas_10, 
                                  gnas_11, gnas_12, gnas_13, gnas_14, gnas_15)
                                  
str(HH_merged) 
str(HH_merged$f_region)

#format variables and same in a factor format 
HH_merged$f_region<-as.factor(HH_merged$f_region)
summary(HH_merged$f_region)#to many empty datapoints Check why?? 
ggplot(HH_merged, mapping = aes(x=f_region))+
  geom_bar()

str(Fin.Ros.EI$region_hr)                          
region_hr<-as.factor(Fin.Ros.EI$region_hr)                             
summary(region_hr) 
#this variables looks more accurate, no missing values 
#and includes the is part of hh main background  info                      
#replace this with f_region above

#format variables and same in a factor format 
HH_merged$region_hr<-as.factor(HH_merged$region_hr)
summary(HH_merged$region_hr)#this looks good
ggplot(HH_merged, mapping = aes(x=region_hr))+
  geom_bar()

library(plyr)
#change the label "Pee" to "Pej"
region_hr<-HH_merged$region_hr
region_hr<-revalue(region_hr, c("Pee"="Pej"))
HH_merged$region_hr<-region_hr
summary(HH_merged$region_hr)

#format sex variable
sex<-HH_merged$sex
sex<-revalue(sex, c("1"="Male", "2"="Female"))
HH_merged$sex<-sex
summary(HH_merged$sex)

#format marital 
HH_merged$marital<-as.factor(HH_merged$marital)
summary(HH_merged$marital)#some weird points recorded as -998 and -999 replace with N/A
#assigne -999, -998 as NA
marital<-HH_merged$marital
marital[marital == -999] <- NA
marital[marital == -998] <- NA
marital<-revalue(marital, c("1"="Single", "2"="Married or factual relationship","3"="Widowed",
                            "4"="Divorced or legally separated" ))
summary(marital)
HH_merged$marital<-marital
ggplot(HH_merged, mapping = aes(x=marital))+
  geom_bar()

#format nationality 
HH_merged$nationality<-as.factor(HH_merged$nationality)
summary(HH_merged$nationality)
#assigne -999, -998 as NA
nationality<-HH_merged$nationality
nationality[nationality == -999] <- NA
nationality[nationality == -998] <- NA
HH_merged$nationality<-nationality #not as interesting variable

#format ps_student *Is $(ps_first) a full-time student?
HH_merged$ps_student<-as.factor(HH_merged$ps_student)
ps_student<-HH_merged$ps_student
ps_student<-revalue(ps_student, c("1"="Yes", "2"="No"))
HH_merged$ps_student<-ps_student

#employment status

3*1400


#include income from job *What is the net monthly pay (â,¬)  gets from his/her main job
str(Fin.Ros.EI$rc_job_income)
HH_merged$rc_job_income<-as.numeric(Fin.Ros.EI$rc_job_income)
summary(HH_merged$rc_job_income)
#rc_income_p 
#How much of your total ANNUAL household income (???)  reported just now do you personally contribute?
#assigne -999, -998 as NA
rc_job_income<-HH_merged$rc_job_income
rc_job_income[rc_job_income == -999] <- NA
rc_job_income[rc_job_income == -998] <- NA
HH_merged$rc_job_income<-rc_job_income
summary(rc_job_income)

#format D1.h) What type of employer does $(hh_memberX) have?
str(HH_merged$job_emp)
HH_merged$job_emp<-as.factor(HH_merged$job_emp)
summary(HH_merged$job_emp)
job_emp<-HH_merged$job_emp
job_emp[job_emp == -999] <- NA
job_emp[job_emp == -998] <- NA
job_emp[job_emp == 999] <- NA
summary(job_emp)
job_emp<-revalue(job_emp, c("1"="Government, public sector or security forces", "2"="State-owned enterprise",
                                  "3"="Private company ", "4"="International organization ", "5"="NGO/ humanitarian organization ",
                                  "6"="Self-employed ","7"="Other private individual " ))
HH_merged$job_emp<-job_emp

# ue_reas	E3) What is the main reason he/she stopped working?
str(HH_merged$ue_reas)
ue_reas<-HH_merged$ue_reas
ue_reas<-as.factor(ue_reas)
ue_reas[ue_reas == -999] <- NA
ue_reas[ue_reas == -998] <- NA
ue_reas<-revalue(ue_reas, c("1"="Dismissed or made redundant", "2"="A job of limited duration has ended ",
                            "3"="Looking after children ", "4"="4 Looking after disabled adults", "5"="Own illness or disability",
                            "6"="Pregnancy / child birth ","7"="Discriminatory ","8"="Family/spouse did not support work", "9"="Education or training",
                            "10"="Early retirement","11"="Normal retirement","12"="Lack of reliable / safe / affordable transportation"))
HH_merged$ue_reas<-ue_reas

#es_active F1) In the last 4 weeks, has $(hh_memberX) looked for work (even if for a minor job of as little as one hour per week) or tried to establish his/her own business?
str(HH_merged$es_active)
es_active<-as.factor(HH_merged$es_active)
summary(es_active)
es_active[es_active == -999] <- NA
es_active[es_active == -998] <- NA
es_active<-revalue(es_active, c("1"="Yes", "0"="No"))
HH_merged$es_active<-es_active

#es_ia_r not deal with this for the moment

#es_months F4) How many months has  been looking for a job or trying to establish 
str(HH_merged$es_months)
es_months<-as.numeric(HH_merged$es_months)
summary(es_months)
es_months[es_months == -999] <- NA
HH_merged$es_months<-es_months

#B1) During the last week, has  worked in a regular job (at least one h
str(HH_merged$lab_status1)
lab_status1<-as.factor(HH_merged$lab_status1)
summary(lab_status1)
lab_status1[lab_status1 == -999] <- NA
lab_status1[lab_status1 == -998] <- NA
lab_status1<-revalue(lab_status1, c("1"="Yes", "0"="No"))
HH_merged$lab_status1<-lab_status1

#job_act What is the main economic activity that â???Ts employer (or if self-e"
str(HH_merged$job_act)# not so important 

#job_func_f

#a) What is the main type of work that  does for his/her job (name of
str(HH_merged$job_func_f)#leave if for now


#credit_de Who in your household is primarily responsible for decisions on borrowing?
str(HH_merged$credit_de)
credit_de<-as.factor(HH_merged$credit_de)
summary(credit_de)
credit_de[credit_de == -999] <- NA
credit_de[credit_de == -998] <- NA
credit_de<-revalue(credit_de, c("1"="Self", "2"="Spouse/partner ",
                            "3"="Self and spouse/partner jointly", "4"="Self and others jointly", "5"="Spouse/partner and others jointly",
                            "6"=" Other household member "))
HH_merged$credit_de<-credit_de


#es_type 6) What type of employment is  pursuing?
str(HH_merged$es_type)
es_type<-as.factor(HH_merged$es_type)
summary(es_type)
es_type[es_type == -999] <- NA
es_type[es_type == -998] <- NA
es_type<-revalue(es_type, c("1"="Self-employment/own business", "2"="Full-time employee ",
                                "3"="Part-time employee"))
HH_merged$es_type<-es_type

#ed_field What is the primary field of â???Ts highest level of education compled
str(HH_merged$ed_field)
ed_field<-as.factor(HH_merged$ed_field)
summary(ed_field)
ed_field[ed_field == -999] <- NA
ed_field[ed_field == -998] <- NA
HH_merged$ed_field<-ed_field

#ei_wage In the past year, have you personally earned any wage or salary?
str(HH_merged$ei_wage)
ei_wage<-as.factor(HH_merged$ei_wage)
summary(ei_wage)
ei_wage[ei_wage == -999] <- NA
ei_wage<-revalue(ei_wage, c("1"="Yes", "0"="No"))
HH_merged$ei_wage<-ei_wage

#ei_wage_d Over the past year, who made decisions about how the majority of your wage or salary income was spent?
str(HH_merged$ei_wage_d)
ei_wage_d<-as.factor(HH_merged$ei_wage_d)
summary(ei_wage_d)
ei_wage_d[ei_wage == -999] <- NA
ei_wage_d<-revalue(ei_wage_d, c("1"="Self", "2"="Spouse/partner","3"="Self and spouse/partner jointly", "4"="Self and others jointly",
                                "5"="Spouse/partner and others jointly", "6"="Other household member(s)"))
HH_merged$ei_wage_d<-ei_wage_d

#ent_bus_iHave you, alone or with others, already started or are currentlytrying to start a new business, 
#including any self-employment or selling any goods or services to others? (select all)
str(HH_merged$ent_bus_i)
ent_bus_i<-as.factor(HH_merged$ent_bus_i)
summary(ent_bus_i)
ent_bus_i[ent_bus_i == -999] <- NA
ent_bus_i[ent_bus_i == -998] <- NA
#24244 empty values
#assign those as NA
ent_bus_i[ent_bus_i == ""]<- NA
ent_bus_i<-revalue(ent_bus_i, c("0"="No, I am not currently involved in my own business", "1"="Yes, I have already started a business ","2"="Yes, I am currently trying to start a new business"))
HH_merged$ent_bus_i<-ent_bus_i

#ent_main Which do you consider to be your main business
str(HH_merged$ent_main)# no further action

#ent_reg Has the business been formally registered with the Kosovo Business Registrati
str(HH_merged$ent_reg)
ent_reg<-as.factor(HH_merged$ent_reg)
summary(ent_reg)
ent_reg[ent_reg == -999] <- NA
ent_reg[ent_reg == -998] <- NA
ent_reg<-revalue(ent_reg, c("1"="Yes", "0"="No"))
HH_merged$ent_reg<-ent_reg

#ent_reg_r Why have you not formally registered your business with KBRA
str(HH_merged$ent_reg_r)
ent_reg_r<-as.factor(HH_merged$ent_reg_r)
summary(ent_reg_r)
ent_reg_r[ent_reg_r == -999] <- NA
ent_reg_r[ent_reg_r == -998] <- NA
ent_reg_r<-revalue(ent_reg_r, c("1"="Don't know how to do so", "2"="Have not had the time", "3"="Don't see benefit in doing so", "4"="Cannot afford to do so"))
HH_merged$ent_reg_r<-ent_reg_r

#ent_plan Over the past 12 months, have you done anything to help start this new busine
str(HH_merged$ent_plan)
ent_plan<-as.factor(HH_merged$ent_plan)
summary(ent_plan)
ent_plan<-revalue(ent_plan, c("1"="Yes", "0"="No"))
HH_merged$ent_plan<-ent_plan

#ent_choice Why are you involved in this business?
str(HH_merged$ent_choice)
ent_choice<-as.factor(HH_merged$ent_choice)
summary(ent_choice)
ent_choice[ent_choice == -999] <- NA
ent_choice[ent_choice == -998] <- NA
ent_choice[ent_choice == 999] <- NA
ent_choice<-revalue(ent_choice, c("1"="I want to take advantage of business opportunity", 
                                  "2"="No other opportunities available to me", 
                                  "3"="Combination of both 1 and 2", "4"="Other"))
HH_merged$ent_plan<-ent_choice

#rc_ent_act  What is the main economic activity that this business i
str(HH_merged$rc_ent_act)# checnk the coded variable below

#rc_ent_act_text_coded What is the main economic activity that this business i
str(HH_merged$rc_ent_act_text_coded)
rc_ent_act_text_coded<-as.factor(HH_merged$rc_ent_act_text_coded)
summary(rc_ent_act_text_coded)# look at the codes for reference 

#ent_sup What was the main source of start-up capital for this business?
str(HH_merged$ent_sup)
ent_sup<-as.factor(HH_merged$ent_sup)
summary(ent_sup)
ent_sup[ent_sup == -999] <- NA
ent_sup[ent_sup == -998] <- NA
ent_sup[ent_sup == 999] <- NA
ent_sup<-revalue(ent_sup, c("1"="Own resources / savings", 
                                  "2"="Formal loan", 
                                  "3"="Informal loan", "4"="Remittances "))

HH_merged$ent_sup<-ent_sup

#ent_ment Various people may give you advice on your business. Have you received advice 
str(HH_merged$ent_ment)
ent_ment<-as.factor(HH_merged$ent_ment) #to many levels check later if you need

#ent_const_coded What is the main obstacle in growing your business?
str(HH_merged$ent_const_coded)
ent_const_coded<-as.factor(HH_merged$ent_const_coded)
summary(ent_const_coded)
ent_const_coded[ent_const_coded == -999] <- NA
attributes(Fin.Ros.EI$ent_const_coded)
ent_const_coded<-revalue(ent_const_coded, c("1"="Lack of financial means ", "2"=" Lack of market and clients ","3"="High competition", "4"="Lack of suitable workplace",
                                "5"="Institutional barriers", "6"="Lack of working tools and professionalism",
                                "7"="High Interest Rates", "8"="Unfavorable climate conditions",
                                "9"="Personal reasons", "10"=" Business hasn't started yet"))
HH_merged$ent_const_coded<-ent_const_coded

#ent_bus Are you, alone or with others, expecting to start a new business, including a
str(HH_merged$ent_bus)
attributes(HH_merged$ent_bus)
ent_bus<-as.factor(HH_merged$ent_bus)
ent_bus[ent_bus == -999] <- NA
ent_bus[ent_bus == -998] <- NA
ent_bus<-revalue(ent_bus, c("1"="Yes", "0"="No"))

HH_merged$ent_bus<-ent_bus

#ent_skill Do you have the knowledge, skill and experience required to start a new busin
str(HH_merged$ent_skill)
attributes(HH_merged$ent_skill)
ent_skill<-as.factor(HH_merged$ent_skill)
ent_skill[ent_skill == -999] <- NA
ent_skill[ent_skill == -998] <- NA
ent_skill<-revalue(ent_skill, c("1"="Yes", "0"="No"))
summary(ent_skill)

HH_merged$ent_skill<-ent_skill

#f_emp Have you ever been in regular employment before (job as employee or unpaid family member)?
str(HH_merged$f_emp)

#f_discr In your past or present employment, do you feel you are/were treated EQUALLY within the workplace by your employer(s) 
#when compared with members of the opposite sex that you work with? This includes both negative and positive treatment.
str(HH_merged$f_discr)
attributes(Fin.Ros.EI$f_discr)
f_discr<-as.factor(HH_merged$f_discr)
f_discr[f_discr == -999] <- NA
f_discr[f_discr == -998] <- NA
f_discr<-revalue(f_discr, c("1"="Yes", "0"="No", "2"="N/A"))
summary(f_discr)

HH_merged$f_discr<-f_discr

#f_discr_pn Was this unequal treatment positive or negative?
str(HH_merged$f_discr_pn)
attributes(HH_merged$f_discr_pn)
f_discr_pn<-as.factor(HH_merged$f_discr_pn)
f_discr_pn[f_discr_pn == -999] <- NA
f_discr_pn[f_discr_pn == -998] <- NA
f_discr_pn<-revalue(f_discr_pn, c("1"="Positive", "2"="Negative"))
summary(f_discr_pn)

HH_merged$f_discr_pn<-f_discr_pn

#f_adv Do you feel this unequal treatment has prevented you from advancing professional"
str(HH_merged$f_adv)
attributes(HH_merged$f_adv)
f_adv<-as.factor(HH_merged$f_adv)
f_adv[f_adv == -999] <- NA
f_adv[f_adv == -998] <- NA
f_adv<-revalue(f_adv, c("1"="Yes", "0"="No"))
summary(f_adv)

HH_merged$f_adv<-f_adv

#f_care In order to care for a child or family member, have you ever: (select all)
str(HH_merged$f_care)# not interested 

#f_ccare Has access to child care (or lack thereof) influenced your career decisions?
str(HH_merged$f_ccare)
attributes(HH_merged$f_ccare)
f_ccare<-as.factor(HH_merged$f_ccare)
f_ccare[f_ccare == -999] <- NA
f_ccare[f_ccare == -998] <- NA
f_ccare<-revalue(f_ccare, c("1"="Yes", "0"="No"))
summary(f_ccare)

HH_merged$f_ccare<-f_ccare

#f_transp *Has lack of access to safe, reliable, / and/or affordable transport influenced your career decisions?
str(HH_merged$f_transp)
attributes(HH_merged$f_transp)
f_transp<-as.factor(HH_merged$f_transp)
f_transp[f_transp == -999] <- NA
f_transp[f_transp == -998] <- NA
f_transp<-revalue(f_transp, c("1"="Yes", "0"="No", "2"="N/A"))
summary(f_transp)

HH_merged$f_transp<-f_transp

#f_equal Do you believe that men and women in Kosovo are generally treated equally in the workplace?
attributes(Fin.Ros.EI$f_equal)
f_equal<-as.factor(Fin.Ros.EI$f_equal)
f_equal[f_equal == -999] <- NA
f_equal[f_equal == -998] <- NA
f_equal<-revalue(f_equal, c("1"="Yes", "0"="No"))
summary(f_equal) 

HH_merged$f_equal<-f_equal

#f_discr_f Has concern about discrimination influenced your career decisions?
str(HH_merged$f_discr_f)
attributes(HH_merged$f_discr_f)
f_discr_f<-as.factor(HH_merged$f_discr_f)
f_discr_f[f_discr_f == -999] <- NA
f_discr_f[f_discr_f == -998] <- NA
f_discr_f<-revalue(f_discr_f, c("1"="Yes", "0"="No"))
summary(f_discr_f) 

HH_merged$f_discr_f<-f_discr_f

#f_discr_fh_coded In what ways has concern about discrimination influenced your career decision
str(HH_merged$f_discr_fh_coded)
attributes(HH_merged$f_discr_fh_coded)
f_discr_fh_coded<-as.factor(HH_merged$f_discr_fh_coded)
f_discr_fh_coded[f_discr_fh_coded == -999] <- NA
f_discr_fh_coded[f_discr_fh_coded == -998] <- NA
f_discr_fh_coded<-revalue(f_discr_fh_coded, c("1"="Gender differences", "2"=" Wage differences ", "3"="Nepotism", "4"="Ethnic differences", "5"="Unfair treatment", "6"="Discrimination","7"="Hard work and overtime", "8"="Sexual harassment" ))
summary(f_discr_fh_coded) 

HH_merged$f_discr_fh_coded<-f_discr_fh_coded

#gnas_1 It is important that sons have more education than daughters
str(HH_merged$gnas_1)
attributes(HH_merged$gnas_1)
gnas_1<-as.factor(HH_merged$gnas_1)
gnas_1[gnas_1 == -999] <- NA
gnas_1[gnas_1 == -998] <- NA
gnas_1<-revalue(gnas_1, c("1"="Agree", "0"="Disagree"))
summary(gnas_1) 

HH_merged$gnas_1<-gnas_1


#gnas_2 Daughters should be sent to school only if they are not needed to help at home"
str(HH_merged$gnas_2)
attributes(HH_merged$gnas_2)
gnas_2<-as.factor(HH_merged$gnas_2)
gnas_2[gnas_2 == -999] <- NA
gnas_2[gnas_2 == -998] <- NA
gnas_2<-revalue(gnas_2, c("1"="Agree", "0"="Disagree"))
summary(gnas_2) 

HH_merged$gnas_2<-gnas_2

#gnas_3,The most important reason that sons should be more educated than daughters is so"
# they can better look after their parents when they are older
str(HH_merged$gnas_3)
attributes(HH_merged$gnas_3)
gnas_3<-as.factor(HH_merged$gnas_3)
gnas_3[gnas_3 == -999] <- NA
gnas_3[gnas_3 == -998] <- NA
gnas_3<-revalue(gnas_3, c("1"="Agree", "0"="Disagree"))
summary(gnas_3) 

HH_merged$gnas_3<-gnas_3

#gnas_4 If there is a limited amount of money to pay for tutoring, it should be spent on sons first
str(HH_merged$gnas_4)
attributes(HH_merged$gnas_4)
gnas_4<-as.factor(HH_merged$gnas_4)
gnas_4[gnas_4 == -999] <- NA
gnas_4[gnas_4 == -998] <- NA
gnas_4<-revalue(gnas_4, c("1"="Agree", "0"="Disagree"))
summary(gnas_4) 

HH_merged$gnas_4<-gnas_4

#gnas_5 A woman should take good care of her own children and notworry about other people's affairs

str(HH_merged$gnas_5)
attributes(HH_merged$gnas_5)
gnas_5<-as.factor(HH_merged$gnas_5)
gnas_5[gnas_5 == -999] <- NA
gnas_5[gnas_5 == -998] <- NA
gnas_5<-revalue(gnas_5, c("1"="Agree", "0"="Disagree"))
summary(gnas_5) 

HH_merged$gnas_5<-gnas_5

#gnas_6 Women should leave politics to the men
str(HH_merged$gnas_6)
attributes(HH_merged$gnas_6)
gnas_6<-as.factor(HH_merged$gnas_6)
gnas_6[gnas_6 == -999] <- NA
gnas_6[gnas_6 == -998] <- NA
gnas_6<-revalue(gnas_6, c("1"="Agree", "0"="Disagree"))
summary(gnas_6) 

HH_merged$gnas_6<-gnas_6

#gnas_7, A woman has to have a husband or sons or some other male kinsman to protect her

str(HH_merged$gnas_7)
attributes(HH_merged$gnas_7)
gnas_7<-as.factor(HH_merged$gnas_7)
gnas_7[gnas_7 == -999] <- NA
gnas_7[gnas_7 == -998] <- NA
gnas_7<-revalue(gnas_7, c("1"="Agree", "0"="Disagree"))
summary(gnas_7) 

HH_merged$gnas_7<-gnas_7

#gnas_8 The only thing a woman can really rely on in her old age is her sons
str(HH_merged$gnas_8)
gnas_8<-as.factor(HH_merged$gnas_8)
gnas_8[gnas_8 == -999] <- NA
gnas_8[gnas_8 == -998] <- NA
gnas_8<-revalue(gnas_8, c("1"="Agree", "0"="Disagree"))
summary(gnas_8) 

HH_merged$gnas_8<-gnas_8

#gnas_9,A good woman never questions her husband's opinions, even if she is not sure she agrees with them
str(HH_merged$gnas_9)
gnas_9<-as.factor(HH_merged$gnas_9)
gnas_9[gnas_9 == -999] <- NA
gnas_9[gnas_9 == -998] <- NA
gnas_9<-revalue(gnas_9, c("1"="Agree", "0"="Disagree"))
summary(gnas_9) 

HH_merged$gnas_9<-gnas_9

# gnas_10, When it is a question of children's health, it is best to do what the father wants
str(HH_merged$gnas_10)
gnas_10<-as.factor(HH_merged$gnas_10)
gnas_10[gnas_10 == -999] <- NA
gnas_10[gnas_10 == -998] <- NA
gnas_10<-revalue(gnas_10, c("1"="Agree", "0"="Disagree"))
summary(gnas_10) 

HH_merged$gnas_10<-gnas_10

#gnas_11, Daughters should be able to work outside the home after they have children if they want to
str(HH_merged$gnas_11)
gnas_11<-as.factor(HH_merged$gnas_11)
gnas_11[gnas_11 == -999] <- NA
gnas_11[gnas_11 == -998] <- NA
gnas_11<-revalue(gnas_11, c("1"="Agree", "0"="Disagree"))
summary(gnas_11) 

HH_merged$gnas_11<-gnas_11

#gnas_12 Daughters should have just the same chance to work outside of homes as sons
str(HH_merged$gnas_12)
gnas_12<-as.factor(HH_merged$gnas_12)
gnas_12[gnas_12 == -999] <- NA
gnas_12[gnas_12 == -998] <- NA
gnas_12<-revalue(gnas_12, c("1"="Agree", "0"="Disagree"))
summary(gnas_12) 

HH_merged$gnas_12<-gnas_12

#gnas_13 Daughters should be told that an important reason not to have
#too many children is so they can work outside the home and earn money
str(HH_merged$gnas_13)
gnas_13<-as.factor(HH_merged$gnas_13)
gnas_13[gnas_13 == -999] <- NA
gnas_13[gnas_13 == -998] <- NA
gnas_13<-revalue(gnas_13, c("1"="Agree", "0"="Disagree"))
summary(gnas_13) 

HH_merged$gnas_13<-gnas_13

#gnas_14 I would like my daughter to be able to work outside the home so she can support herself if necessary
str(HH_merged$gnas_14)
gnas_14<-as.factor(HH_merged$gnas_14)
gnas_14[gnas_14 == -999] <- NA
gnas_14[gnas_14 == -998] <- NA
gnas_14<-revalue(gnas_14, c("1"="Agree", "0"="Disagree"))
summary(gnas_14) 

HH_merged$gnas_14<-gnas_14

#gnas_15 Daughters should have the same inheritance rights as sons
str(HH_merged$gnas_15)
gnas_15<-as.factor(HH_merged$gnas_15)
gnas_15[gnas_15 == -999] <- NA
gnas_15[gnas_15 == -998] <- NA
gnas_15<-revalue(gnas_15, c("1"="Agree", "0"="Disagree"))
summary(gnas_15) 

HH_merged$gnas_15<-gnas_15

summary(HH_merged) 

#write and save to csv
write.csv(HH_merged, file = "FinRosEI_Clean.csv")








ggplot(HH_merged, mapping = aes(x=f_region))+
  geom_bar()













































library(ggplot2)
ggplot(Final.Roster.Merged, mapping = aes(x=educ))+
  geom_histogram()















































































#--------------------------------
#Code 

timeuse.main<- read.csv("C:/Users/user/Desktop/Data/New folder/LFTUS_Main.csv")

timeuse<- read.csv("C:/Users/user/Desktop/Data/New folder/LFTUS_Time_Use.csv")

names(timeuse.main)
names(timeuse)
library(dplyr)
library(tidyr)
timeuse.main.long <- timeuse.main %>% gather(measurement, value, -(1:6)) 
timeuse.main.long %>% glimpse()

#subset only first 26 variables
timeuse.main1<-timeuse.main[c(1:26)]
timeuse.main1.long <- timeuse.main1 %>% gather(Employment, value, -(1:14)) 
timeuse.main1.long %>% glimpse()
head(timeuse.main1.long$Employment)

