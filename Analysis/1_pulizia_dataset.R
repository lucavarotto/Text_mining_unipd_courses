rm(list=ls())
setwd("C:/Users/Utente/OneDrive/Universita/2023-24/BigData/Progetto_MSBD/Cocco_Quartuccio_Varotto")
dat <- read.csv("unipd_courses.csv")
dim(dat)

dat[dat==""] <- NA
apply(dat, 2, function(x) mean(is.na(x)))
(table(dat[,1]))
(sort(table(dat[,2]),decreasing=F))
# per un paio di corsi l'algoritmo non ha funzionato
# ha preso troppi pochi esami. Li aggiungiamo?

most_freq <- sort(table(dat$Course),decreasing=T)
head(most_freq,10)
# le cose più frequenti sono tirocini e testi
# che però non hanno descrizione


# Rimozione dei non esami e dei corsi con soli NA -------------------------

head(dat[dat[,3] == "TIROCINIO",])

non_esami <- (dat$Course == "PROVA FINALE" |
  dat$Course == "LINGUA INGLESE B2 (ABILITA' RICETTIVE)" |
  dat$Course == "TIROCINIO"  |
  dat$Course == "TIROCINIO (TERZO ANNO)"  |
  dat$Course == "STAGE")

all_null <- is.na(dat$Conoscenze.e.abilita..da.acquisire) &
  is.na(dat$Contenuti)

dat <- dat[!(non_esami | all_null),]
dim(dat) #3456 u.s.

length(unique(dat$URL))
# 2589 URL unici su 3456 osservazioni
# quindi circa 900 da rimuovere


# Rimozione degli URL duplicati -------------------------------------------

(sort(table(dat$URL),decreasing=T))[1:20]

url_to_delete = base::duplicated(dat$URL)

(dat[url_to_delete,])

length(unique(dat$URL))

dat <- dat[!url_to_delete,] 
dim(dat) #2589 u.s.
(dat)
(sort(table(dat$URL),decreasing=T))[1:5]
(round(prop.table(table(dat[,1]))*100,2))
# solo il 3.45% delle osservazioni da giurisprudenza
(sort(table(dat[,2]),decreasing=F))
(sort(table(dat[,3]),decreasing=F))

apply(dat, 2, function(x) mean(is.na(x))*100)
#ancora un 2.29% di NA su contenuti, come li gestiamo?


# Rimozione dei testi duplicati -------------------------------------------

library(digest)
library(TextWiller)
Conoscenze.hash <- sapply(normalizzaTesti(dat$Conoscenze.e.abilita..da.acquisire), digest2int)
Contenuti.hash <- sapply(normalizzaTesti(dat$Contenuti), digest2int)

Conoscenze.duplicated <- base::duplicated(Conoscenze.hash)
Contenuti.duplicated <- base::duplicated(Contenuti.hash)

#View(dat[Conoscenze.duplicated&Contenuti.duplicated, ])

dat <- dat[!(Conoscenze.duplicated&Contenuti.duplicated), ]
dim(dat) #2135 u.s.

# Conviene fare lo stemming prima di questa parte?

View(dat[dat$Course=="ANTROPOLOGIA CULTURALE",])

(round(prop.table(table(dat[,1]))*100,2))
# solo il 4.12% delle osservazioni da giurisprudenza
head(sort(table(dat[,2]),decreasing=F))
head(sort(table(dat[,3]),decreasing=F))

# Problema dei moduli e pari/dispari --------------------------------------
# modulo A e B etc

library(data.table)
cognome <- dat$Course %like% "cognome"
canale <- dat$Course %like% "canale"
pari_dispari <- dat$Course %like% "pari" | dat$Course %like% "dispari"

#View(dat[pari_dispari,])

Conoscenze.hash <- sapply(normalizzaTesti(dat$Conoscenze.e.abilita..da.acquisire), digest2int)
Contenuti.hash <- sapply(normalizzaTesti(dat$Contenuti), digest2int)

Conoscenze.duplicated <- base::duplicated(Conoscenze.hash)
Contenuti.duplicated <- base::duplicated(Contenuti.hash)

to_delete <- ((Conoscenze.duplicated|Contenuti.duplicated)&cognome) |
  ((Conoscenze.duplicated|Contenuti.duplicated)&canale) |
  ((Conoscenze.duplicated|Contenuti.duplicated)&pari_dispari) 

View(dat[dat$Course %like% "COMPETENZE INFORMATICHE DI BASE",])

dat <- dat[!to_delete, ]

dim(dat) #2092


# Traduzione --------------------------------------------------------------

#library(googleLanguageR)
#translate("", source.)

# Salvataggio -------------------------------------------------------------

save(dat, file="unipd_courses_cleaned.Rdata")
