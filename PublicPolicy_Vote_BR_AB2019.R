# Analysis of the 2019 AmericasBarometer

packages<-c("readstata13", "logisticPCA", "survey", "ggplot2", "knitr", "gridExtra")
lapply(packages, require, character.only=T)

# Uploading the survey data, Brazil 2017
df<-read.dta13("Brazil LAPOP AmericasBarometer 2019 v1.0_W.dta")
names(df); nrow(df)

####################
### Demographics ###
####################
# Female
table(df$q1); table(as.numeric(df$q1))
df$female<-as.numeric(df$q1)-1
table(df$q1, df$female)

# Age
table(df$q2)
df$age<-df$q2; df$age_sq<-df$age*df$age

# Urbanity
table(df$ur)
df$urban<-df$ur

# Education
table(df$ed)
df$educ_cat<-ifelse(df$ed<8, "Less than primary",
                    ifelse(df$ed>7 & df$ed<11, "Primary complete",
                           ifelse(df$ed==11, "Secondary complete",
                                  ifelse(df$ed>11, "More than secondary", "Error"))))
table(df$educ_cat)
df$educ_cat<-relevel(factor(df$educ_cat, levels=c("Less than primary", "Primary complete", "Secondary complete", "More than secondary")), ref="Less than primary")
table(df$ed, df$educ_cat)

# Religion
table(df$q3c)
df$religion<-as.character(df$q3c)
df$religion<-ifelse(grepl("Cató", df$religion), "Catholic",
                    ifelse(grepl("Protestante", df$religion), "Evangelical",
                           ifelse(grepl("Pentecostal", df$religion), "Pentecostal",
                                  ifelse(grepl("ateo|Ninguna", df$religion), "No religion/Atheist",
                                         ifelse(grepl("Espí", df$religion), "Spiritist", "other")))))
table(df$religion, df$q3c)

# Measure of wealth
table(df$r1) # TV
table(df$r3) # Fridge
table(df$r4) # Landline phone
table(df$r4a) # Cellphone
table(df$r5); df$car<-as.factor(ifelse(df$r5=="No", "Não", "Sim")) # Automobiles, how many
table(df$r6) # Washing machine
table(df$r7) # Microwave
table(df$r8) # Motorcycle
table(df$r12) # Potable water
table(df$r14) # Bathroom inside hh
table(df$r15) # Computer

goods<-sapply(df[,c("r1", "r3", "r4", "r4a", "car", "r6", "r7", "r8", "r12", "r14", "r15")], as.numeric)-1

logpca_model<-logisticPCA(goods, k=1)
df$wealth<-fitted(logpca_model)[,1]-mean(fitted(logpca_model)[,1], na.rm=T)
summary(df$wealth)

wealth_reg<-lm(wealth ~ female + age + urban + educ_cat + religion, data=df)
summary(wealth_reg)

# "Ethnicity"
table(df$etid)
df$ethnic<-as.character(df$etid)
df$ethnic<-ifelse(df$ethnic=="Blanca", "Branca",
                  ifelse(df$ethnic=="Indígena", "Indígena",
                         ifelse(df$ethnic=="Negra", "Preta",
                                ifelse(df$ethnic=="Mulata", "Pardo",
                                       ifelse(df$ethnic=="Amarillo", "Amarela",
                                              ifelse(df$ethnic=="Otra", "Outra", "error"))))))
table(df$ethnic)

# Marital status
table(df$q11n)
df$q11n<-as.character(df$q11n)
df$marit_status<-ifelse(df$q11n=="NS/NR", NA, 
                        ifelse(grepl("Divorciado|Separado|Viudo", df$q11n), "Divorc., Separa. ou Viúvx",
                               ifelse(grepl("Casado|Libre|civil", df$q11n), "Casadx", df$q11n)))
df$marit_status[df$marit_status=="Soltero"]<-"Solteirx"
table(df$marit_status, df$q11n)

# Number of people in HH
table(df$q12c)
df$people_hh<-df$q12c

# Children (not necessarily sons and daughters) under 13 at home?
table(df$q12bn)
df$children_athome<-df$q12bn

wealth_reg<-lm(wealth ~ female + age + urban + educ_cat + religion + people_hh + children_athome, data=df)
summary(wealth_reg)

# Occupation
table(df$ocup4a)
df$occupation<-ifelse(grepl("Trabajando", df$ocup4a), "Assalariado",
                      ifelse(grepl("activamente", df$ocup4a), "Desempregado",
                             ifelse(grepl("hogar", df$ocup4a), "Afazeres de casa",
                                    ifelse(grepl("jubilado", df$ocup4a), "Aposentado",
                                           ifelse(grepl("estudiante", df$ocup4a), "Estudante",       
                                                  "Outro")))))
table(df$ocup4a, df$occupation)

wealth_reg<-lm(wealth ~ female + age + urban + educ_cat + religion + people_hh + children_athome + occupation, data=df)
summary(wealth_reg)

################################
## Geo and Survey Method vars ##
################################
# Region (estratopri)
table(df$estratopri)
df$region<-as.character(df$estratopri)
df$region[grepl("Sul", df$region)]<-"Sul"
df$region[grepl("Sud", df$region)]<-"Sudeste"
df$region[grepl("Centro", df$region)]<-"Centro-Oeste"
df$region[grepl("Nord", df$region)]<-"Nordeste"
df$region[grepl("Norte", df$region)]<-"Norte"
table(df$estratopri, df$region)

# State
table(df$prov)
df$state<-as.character(df$prov)

# Municipality
table(df$municipio)
df$municipality<-tolower(df$municipio)
table(df$municipality)

# Date
table(df$fecha); class(df$fecha)
df$date<-df$fecha

df$day_survey<-as.numeric(df$date-min(df$date, na.rm=T)+1)
summary(df$day_survey)

# Cluster
table(df$upm)
df$cluster<-NULL
df$cluster<-df$upm

# Weights
df$weight<-df$wt

#######################
# Vote for Bolsonaro ##
#######################
table(df$vb3n)
df$voto2018<-ifelse(df$vb3n=="Jair Bolsonaro (PSL)", "Bolsonaro",
                    ifelse(df$vb3n=="Fernando Haddad (PT)", "Haddad", 
                           "Other"))
df$voto2018[is.na(df$voto2018)]<-"Other"
table(df$voto2018)

df$voto2018_2<-df$voto2018
table(df$vb11)
df$voto2018_2[df$vb11=="PSL (Partido Social Liberal)"]<-"PSLista"
table(df$voto2018_2)
df$voto2018_2<-relevel(factor(df$voto2018_2), ref="Haddad")

#########################################################
### Comparing Preferences for Public Policy Influence ###
#########################################################

df$population<-df$brapp1
df$civilsoc<-df$brapp2
df$professors<-df$brapp3
df$parties<-df$brapp4

design<-svydesign(~cluster, strata=~estratopri, weights=~weight, data=df)

table<-matrix(NA, 4, 3)
svyby(~professors, ~voto2018_2, design, svymean, vartype = "ci", na.rm=T)
svyby(~parties, ~voto2018_2, design, svymean, na.rm=T)
svyby(~population, ~voto2018_2, design, svymean, vartype = "ci", na.rm=T)
svyby(~civilsoc, ~voto2018_2, design, svymean, vartype = "ci", na.rm=T)

summary(svyglm(professors ~ female + age + age_sq + people_hh + children_athome + region + educ_cat + urban + religion +
                 voto2018_2, design))
summary(svyglm(parties ~ female + age + age_sq + people_hh + children_athome + region + educ_cat + urban + religion +
                 voto2018_2, design))
summary(svyglm(civilsoc ~ female + age + age_sq + people_hh + children_athome + region + educ_cat + urban + religion +
                 voto2018_2, design))
summary(svyglm(population ~ female + age + age_sq + people_hh + children_athome + region + educ_cat + urban + religion +
                 voto2018_2, design))