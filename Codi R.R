setwd("C:/Users/marta/OneDrive/Escriptori/metodologia_TFG")

#Packages utilitzats
library(haven)
library(descr)
library(car)
library(corrplot)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggeffects)
library(sjPlot)
library(rio)
library(nnet)
library(jtools)
library(DescTools)
library(gmodels)

#Base de dades
dades <- read_sav("3aOnada_2023.sav")

#Establiment variables d'interès
dt<-data.frame(dades$SEXE, dades$EDAT, dades$EDAT_GR, dades$ESTUDIS_1_15, dades$INGRESSOS_1_15, dades$SIT_LAB, dades$ACTITUD_INDEPENDENCIA, dades$PART_CONGRES, dades$SIT_LAB_NO_ACTIU, dades$ESTUDIS_1_15, dades$IDEOL_0_10, dades$VAL_GOV_CAT, dades$INTERES_POL_PUBLICS, dades$LLENGUA_IDENT_1_3, dades$LLOC_NAIX, dades$PART_PARLAMENT)

names(dt)=c("sexe","edat", "edat_grups", "estudis","ingressos","situaciolaboral","independencia","vot","noactiu", 
            "estudis", "ideologia", "valGen", "interespol", "llengua", "procedencia", "vot_darreres")

freq(dt$sexe)
dt$estudis[dt$estudis==98]<-NA
dt$estudis[dt$estudis==99]<-NA
dt$sexe<-factor(dt$sexe, levels=c(1,2), labels=c("Masculí", "Femení"))

freq(dt$edat)
dt$edat<-as.numeric(dt$edat)
dt$edat_grups <- factor(dades$EDAT_GR,
                        levels = c(1,2,3,4,5),
                        labels = c("De 18 a 24 anys",
                                   "De 25 a 34 anys",
                                   "De 35 a 49 anys",
                                   "De 50 a 64 anys",
                                   "65 anys o més"))
freq(dt$edat_grups)

freq(dt$estudis)
dt$estudis[dt$estudis==98]<-NA
dt$estudis[dt$estudis==99]<-NA
dt$estudis[dt$estudis==1]<-1
dt$estudis[dt$estudis==2]<-2
dt$estudis[dt$estudis==3]<-4
dt$estudis[dt$estudis==4]<-3
dt$estudis[dt$estudis==5]<-4
dt$estudis[dt$estudis==6]<-5
dt$estudis[dt$estudis==7]<-5
dt$estudis[dt$estudis==8]<-6
dt$estudis[dt$estudis==9]<-6
dt$estudis[dt$estudis==10]<-6
dt$estudis[dt$estudis==11]<-6
dt$estudis[dt$estudis==12]<-6
dt$estudis[dt$estudis==13]<-7
dt$estudis[dt$estudis==14]<-7
dt$estudis[dt$estudis==15]<-7

dt$estudis<-factor(dt$estudis, levels=c(1,2,3,4,5,6,7), 
                   labels=c("Sense estudis", "Primària", "Sec.obligatòria", "FP inicial i mig", "FP superior i batxillerat", "Estudis universitaris", "Estudis post-grau"))

freq(dt$ingressos)
dt$ingressos[dt$ingressos==98]<-NA
dt$ingressos[dt$ingressos==99]<-NA
dt$ingressos[dt$ingressos==1]<-0
dt$ingressos[dt$ingressos==2]<-1
dt$ingressos[dt$ingressos==3]<-1
dt$ingressos[dt$ingressos==4]<-1
dt$ingressos[dt$ingressos==5]<-1
dt$ingressos[dt$ingressos==6]<-1
dt$ingressos[dt$ingressos==7]<-2
dt$ingressos[dt$ingressos==8]<-2
dt$ingressos[dt$ingressos==9]<-2
dt$ingressos[dt$ingressos==10]<-2
dt$ingressos[dt$ingressos==11]<-2
dt$ingressos[dt$ingressos==12]<-3
dt$ingressos[dt$ingressos==13]<-3
dt$ingressos[dt$ingressos==14]<-3
dt$ingressos[dt$ingressos==15]<-3
dt$ingressos<-factor(dt$ingressos, levels=c(0,1,2,3), labels=c("Sense ingressos",
                                                               "Ingressos baixos", "Ingressos mitjans", "Ingressos alts"))
freq(dt$ideologia)
dt$ideologia[dt$ideologia==98]<-NA
dt$ideologia[dt$ideologia==99]<-NA

freq(dt$independencia)
dt$independencia[dt$independencia == 98] <- NA
dt$independencia[dt$independencia == 99] <- NA
dt$independencia[dt$independencia == 1] <- 1
dt$independencia[dt$independencia == 2] <- 0
dt$independencia<-factor(dt$independencia)

freq(dt$valGen)
dt$valGen[dt$valGen==98]<-NA
dt$valGen[dt$valGen==99]<-NA

freq(dt$interespol)
dt$interespol[dt$interespol==98]<-NA
dt$interespol[dt$interespol==99]<-NA
dt$interespol[dt$interespol==1]<-1
dt$interespol[dt$interespol==2]<-1
dt$interespol[dt$interespol==3]<-0
dt$interespol[dt$interespol==4]<-0
dt$interespol<-factor(dt$interespol, levels=c(0,1), labels=c("No interessat", "Interessat"))

freq(dt$situaciolaboral)
dt$situaciólaboral[dt$situaciolaboral==98]<-NA
dt$situaciolaboral[dt$situaciolaboral==99]<-NA
dt$situaciolaboral[dt$situaciolaboral==1]<-1
dt$situaciolaboral[dt$situaciolaboral==2]<-0
dt$situaciolaboral[dt$situaciolaboral==3]<-1
dt$situaciolaboral<-factor(dt$situaciolaboral, levels=c(0,1), labels=c("No actiu", "Actiu"))

freq(dt$llengua)
dt$llengua[dt$llengua==1]<-1
dt$llengua[dt$llengua==2]<-2
dt$llengua[dt$llengua==80]<-3
dt$llengua[dt$llengua==98]<-NA
dt$llengua[dt$llengua==99]<-NA
dt$llengua<-factor(dt$llengua, levels=c(1,2,3), labels=c("Català", "Castellà", "Altres"))

freq(dt$procedencia)
dt$procedencia[dt$procedencia==1]<-1
dt$procedencia[dt$procedencia==2]<-1
dt$procedencia[dt$procedencia==5]<-2
dt$procedencia<-factor(dt$procedencia, levels=c(1,2), labels=c("Nacional", "Estranger"))


freq(dt$vot_darreres)
dt$vot_darreres[dt$vot_darreres==1]<-0
dt$vot_darreres[dt$vot_darreres==2]<-0
dt$vot_darreres[dt$vot_darreres==3]<-0
dt$vot_darreres[dt$vot_darreres==4]<-0
dt$vot_darreres[dt$vot_darreres==5]<-1
dt$vot_darreres[dt$vot_darreres==98]<-NA
dt$vot_darreres[dt$vot_darreres==99]<-NA
dt$vot_darreres<-factor(dt$vot_darreres, levels=c(0,1), labels=c("Abstenció", "Vot"))

#Recodificar Vot
freq(dt$vot)
dt$vot[dt$vot==98]<-NA
dt$vot[dt$vot==99]<-NA
dt$vot[dt$vot==1]<-0
dt$vot[dt$vot==2]<-0
dt$vot[dt$vot==3]<-0
dt$vot[dt$vot==4]<-0
dt$vot[dt$vot==5]<-1
dt$vot<-factor(dt$vot, levels=c(0,1), labels=c("Abstencio", "vot"))


rlmb<-glm(data=dt, formula=vot~sexe+edat_grups+procedencia+ingressos+estudis+situaciolaboral+llengua+ideologia+valGen+interespol+independencia+vot_darreres, family="binomial")
summary(rlmb)
plot_summs(rlmb)
PseudoR2(rlmb, which = c ("CoxSnell", "Nagelkerke", "McFadden"))


effect_plot(rlmb, pred = sexe, interval = TRUE, plot.points = TRUE)
effect_plot(rlmb, pred = edat_grups, interval = TRUE, plot.points = TRUE)
effect_plot(rlmb, pred = ingressos, interval = TRUE, plot.points = TRUE)
effect_plot(rlmb, pred = situaciolaboral, interval = TRUE, plot.points = TRUE)
effect_plot(rlmb, pred = estudis, interval = TRUE, plot.points = TRUE)
effect_plot(rlmb, pred = ideologia, interval = TRUE, plot.points = TRUE)
effect_plot(rlmb, pred = interespol, interval = TRUE, plot.points = TRUE)
effect_plot(rlmb, pred = valGen, interval = TRUE, plot.points = TRUE)
effect_plot(rlmb, pred = procedencia, interval = TRUE, plot.points = TRUE)
effect_plot(rlmb, pred = llengua, interval = TRUE, plot.points = TRUE)
effect_plot(rlmb, pred = vot_darreres, interval = TRUE, plot.points = TRUE)
dt$ideologia <- as.numeric(dt$ideologia)

sexeg<-ggpredict(rlmb, terms="sexe")
plot(sexeg)+labs(x="Gènere", y="Probabilitats de vot", title=NULL)

edatg<-ggpredict(rlmb, terms="edat_grups")
plot(edatg)+labs(x="Edat", y="Probabilitats de vot", title=NULL)

laboral<-ggpredict(rlmb, terms="situaciolaboral")
plot(laboral)+labs(x="Situació laboral", y="Probabilitats de vot", title=NULL)


estudisg<-ggpredict(rlmb, terms="estudis")
plot(estudisg)+labs(x="Estudis", y="Probabilitats de vot", title=NULL)

ideologiag<-ggpredict(rlmb, terms="ideologia")
plot(ideologiag)+labs(x="ideologia", y="Probabilitats de vot", title=NULL)

valGeng<-ggpredict(rlmb, terms="valGen")
plot(valGeng)+labs(x="Valoració govern català", y="Probabilitats de vot", title=NULL)     

proced<-ggpredict(rlmb, terms="procedencia")
plot(proced)+labs(x="Procedència", y="Probabilitats de vot", title=NULL)     

interespolg<-ggpredict(rlmb, terms="interespol")
plot(interespolg)+labs(x="Interès política", y="Probabilitats de vot", title=NULL)     

ingressosg<-ggpredict(rlmb, terms="ingressos")
plot(ingressosg)+labs(x="Ingressos", y="Probabilitats de vot", title=NULL) 

llengua<-ggpredict(rlmb, terms="llengua")
plot(llengua)+labs(x="Llengua pròpia", y="Probabilitats de vot", title=NULL)

votdarreres<-ggpredict(rlmb, terms="vot_darreres")
plot(votdarreres)+labs(x="Participació darreres eleccions", y="Probabilitats de vot", title=NULL)

#creuaments
table(dt$vot, dt$sexe)
prop.table(table(dt$vot, dt$sexe),1)
crosstab(dt$vot, dt$sexe, prop.c="T")
summary(dt$sexe)

table(dt$vot, dt$edat)
prop.table(table(dt$vot, dt$edat),1)
crosstab(dt$vot, dt$edat_grups, prop.c="T")
summary(dt$edat_grups)

table(dt$vot, dt$ingressos)
prop.table(table(dt$vot, dt$ingressos),1)
crosstab(dt$vot, dt$ingressos, prop.c="T")
summary(dt$ingressos)

table(dt$vot, dt$estudis)
prop.table(table(dt$vot, dt$estudis),1)
crosstab(dt$vot, dt$estudis, prop.c="T")
summary(dt$estudis)

table(dt$vot, dt$situaciolaboral)
prop.table(table(dt$vot, dt$situaciolaboral),1)
crosstab(dt$vot, dt$situaciolaboral, prop.c="T")
summary(dt$situaciolaboral)

table(dt$vot, dt$valGen)
prop.table(table(dt$vot, dt$valGen),1)
crosstab(dt$vot, dt$valGen, prop.c="T")
summary(dt$valGen)

table(dt$vot, dt$interespol)
prop.table(table(dt$vot, dt$interespol),1)
crosstab(dt$vot, dt$interespol, prop.c="T")
summary(dt$interespol)

table(dt$vot, dt$llengua)
prop.table(table(dt$vot, dt$llengua),1)
crosstab(dt$vot, dt$llengua, prop.c="T")
summary(dt$llengua)

table(dt$vot, dt$vot_darreres)
prop.table(table(dt$vot, dt$vot_darreres),1)
crosstab(dt$vot, dt$vot_darreres, prop.c="T")
summary(dt$vot_darreres)

table(dt$vot, dt$ideologia)
prop.table(table(dt$vot, dt$ideologia),1)
crosstab(dt$vot, dt$ideologia, prop.c="T")
summary(dt$ideologia)

table(dt$vot, dt$procedencia)
prop.table(table(dt$vot, dt$procedencia),1)
crosstab(dt$vot, dt$procedencia, prop.c="T")
summary(dt$procedencia)

table(dt$vot, dt$llengua)
prop.table(table(dt$vot, dt$llengua),1)
crosstab(dt$vot, dt$llengua, prop.c="T")
summary(dt$llengua)

table(dt$vot, dt$independencia)
prop.table(table(dt$vot, dt$independencia),1)
crosstab(dt$vot, dt$independencia, prop.c="T")
summary(dt$inde)

#Anàlisi independentisme
independentistes <- subset(dt, independencia == 1)

table(independentistes$sexe)
sum(is.na(dt$independencia))


rlmb_inde<-glm(data=independentistes, formula=vot~sexe+edat_grups+ingressos+estudis+situaciolaboral+ideologia+valGen+interespol+llengua+procedencia+vot_darreres, family="binomial")
summary(rlmb_inde)
plot_summs(rlmb, rlmb_inde)
PseudoR2(rlmb_inde, which = c ("CoxSnell", "Nagelkerke", "McFadden"))


effect_plot(rlmb_inde, pred = sexe, interval = TRUE, plot.points = TRUE)
effect_plot(rlmb_inde, pred = edat_grups, interval = TRUE, plot.points = TRUE)
effect_plot(rlmb_inde, pred = ingressos, interval = TRUE, plot.points = TRUE)
effect_plot(rlmb_inde, pred = situaciolaboral, interval = TRUE, plot.points = TRUE)
effect_plot(rlmb_inde, pred = estudis, interval = TRUE, plot.points = TRUE)
effect_plot(rlmb_inde, pred = ideologia, interval = TRUE, plot.points = TRUE)
effect_plot(rlmb_inde, pred = interespol, interval = TRUE, plot.points = TRUE)
effect_plot(rlmb_inde, pred = valGen, interval = TRUE, plot.points = TRUE)
effect_plot(rlmb_inde, pred = llengua, interval = TRUE, plot.points = TRUE)
effect_plot(rlmb_inde, pred = procedencia, interval = TRUE, plot.points = TRUE)
effect_plot(rlmb_inde, pred = vot_darreres, interval = TRUE, plot.points = TRUE)


#creuaments
table(independentistes$vot, independentistes$sexe)
prop.table(table(independentistes$vot, independentistes$sexe),1)
crosstab(independentistes$vot, independentistes$sexe, prop.c="T")

table(independentistes$vot, independentistes$edat)
prop.table(table(independentistes$vot, independentistes$edat),1)
crosstab(independentistes$vot, independentistes$edat_grups, prop.c="T")
summary(independentistes$edat)

table(independentistes$vot, independentistes$ingressos)
prop.table(table(independentistes$vot, independentistes$ingressos),1)
crosstab(independentistes$vot, independentistes$ingressos, prop.c="T")
summary(independentistes$ingressos)

table(independentistes$vot, independentistes$estudis)
prop.table(table(independentistes$vot, dt$estudis),1)
crosstab(independentistes$vot, independentistes$estudis, prop.c="T")
summary(independentistes$estudis)

table(independentistes$vot, independentistes$procedencia)
prop.table(table(independentistes$vot, independentistes$procedencia),1)
crosstab(independentistes$vot, independentistes$procedencia, prop.c="T")
summary(independentistes$procedencia)


table(independentistes$vot, independentistes$situaciolaboral)
prop.table(table(independentistes$vot, independentistes$situaciolaboral),1)
crosstab(independentistes$vot, independentistes$situaciolaboral, prop.c="T")
summary(independentistes$situaciolaboral)

table(independentistes$vot, independentistes$valGen)
prop.table(table(independentistes$vot, independentistes$valGen),1)
crosstab(independentistes$vot, independentistes$valGen, prop.c="T")
summary(independentistes$valGen)

table(independentistes$vot, independentistes$interespol)
prop.table(table(independentistes$vot, independentistes$interespol),1)
crosstab(independentistes$vot, independentistes$interespol, prop.c="T")
summary(independentistes$interespol)

table(independentistes$vot, independentistes$ideologia)
prop.table(table(independentistes$vot, independentistes$ideologia),1)
crosstab(independentistes$vot, independentistes$ideologia, prop.c="T")
summary(independentistes$ideologia)

table(independentistes$vot, independentistes$llengua)
prop.table(table(independentistes$vot, independentistes$llengua),1)
crosstab(independentistes$vot, independentistes$llengua, prop.c="T")
summary(independentistes$llengua)

table(independentistes$vot, independentistes$vot_darreres)
prop.table(table(independentistes$vot, independentistes$vot_darreres),1)
crosstab(independentistes$vot, independentistes$vot_darreres, prop.c="T")
summary(independentistes$vot_darreres)

sexeginde<-ggpredict(rlmb_inde, terms="sexe")
plot(sexeginde)+labs(x="Gènere", y="Probabilitats de vot", title=NULL)

edatginde<-ggpredict(rlmb_inde, terms="edat_grups")
plot(edatginde)+labs(x="Edat", y="Probabilitats de vot", title=NULL)

estudisginde<-ggpredict(rlmb_inde, terms="estudis")
plot(estudisginde)+labs(x="Estudis", y="Probabilitats de vot", title=NULL)

laboralinde<-ggpredict(rlmb_inde, terms="situaciolaboral")
plot(laboralinde)+labs(x="Situació laboral", y="Probabilitats de vot", title=NULL)

ideologiag<-ggpredict(rlmb, terms="ideologia")
plot(ideologiag)+labs(x="ideologia", y="Probabilitats de vot", title=NULL)

valGeng<-ggpredict(rlmb_inde, terms="valGen")
plot(valGeng)+labs(x="Valoració govern català", y="Probabilitats de vot", title=NULL)     

procedinde<-ggpredict(rlmb_inde, terms="procedencia")
plot(procedinde)+labs(x="Procedència", y="Probabilitats de vot", title=NULL)     

interespolg<-ggpredict(rlmb_inde, terms="interespol")
plot(interespolg)+labs(x="Interès política", y="Probabilitats de vot", title=NULL)     

ingressosginde<-ggpredict(rlmb_inde, terms="ingressos")
plot(ingressosginde)+labs(x="Ingressos", y="Probabilitats de vot", title=NULL) 

llenguainde<-ggpredict(rlmb_inde, terms="llengua")
plot(llenguainde)+labs(x="Llengua pròpia", y="Probabilitats de vot", title=NULL)

darreresinde<-ggpredict(rlmb_inde, terms="vot_darreres")
plot(darreresinde)+labs(x="Participació darreres eleccions", y="Probabilitats de vot", title=NULL)
