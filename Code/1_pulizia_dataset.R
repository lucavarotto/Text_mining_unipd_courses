################################################################################
#                                                                              #
#   1_PULIZIA_DATASET.R                                                        #
#                                                                              #
#   Primo stadio della pipeline: dal CSV grezzo prodotto da scraping.py al     #
#   dataset ripulito su cui lavorano gli stadi successivi.                     #
#                                                                              #
#   Input  : ../Datasets/unipd_courses.csv            (3845 righe)             #
#   Output : ../Datasets/unipd_courses_cleaned.Rdata  (2092 u.s.)              #
#                                                                              #
#   Il CSV ha sei colonne: School, Degree, Course, i due campi di testo del    #
#   Syllabus ("Conoscenze e abilita' da acquisire" e "Contenuti") e l'URL      #
#   della pagina di origine. Nel seguito l'unita' statistica (u.s.) e' il      #
#   singolo insegnamento.                                                      #
#                                                                              #
#   La pulizia procede per sottrazioni successive, in quattro passaggi:        #
#                                                                              #
#     1. si eliminano le righe che non sono veri insegnamenti (prova finale,   #
#        tirocini, stage, idoneita' linguistiche) e quelle senza testo;        #
#     2. si eliminano gli URL duplicati, che nascono quando lo stesso          #
#        insegnamento compare in piu' corsi di laurea;                        #
#     3. si eliminano i testi duplicati, confrontati tramite hash, perche' lo  #
#        stesso programma puo' essere ripubblicato sotto URL diversi;          #
#     4. si gestiscono gli insegnamenti sdoppiati per canale, per iniziale     #
#        del cognome o per matricola pari/dispari, che hanno descrizioni       #
#        identiche ma sono erogazioni distinte dello stesso corso.             #
#                                                                              #
#   Dopo ogni passaggio dim(dat) documenta quante u.s. restano, e i commenti   #
#   a fine riga riportano il valore ottenuto nell'esecuzione del progetto.     #
#                                                                              #
#   Autori: Andrea Cocco, Andrea Quartuccio, Luca Varotto                      #
#   Esame di Metodi Statistici per i Big Data - Universita' di Padova          #
#                                                                              #
################################################################################

rm(list=ls())   # si parte da un ambiente pulito
# Eseguire con la working directory impostata su questa cartella (Code/).
# In RStudio: Session > Set Working Directory > To Source File Location.
dat <- read.csv("../Datasets/unipd_courses.csv")
dim(dat)        # 3845 righe per 6 colonne

# BeautifulSoup restituisce stringhe vuote dove il campo non c'era: qui
# diventano NA, cosi' da poterle contare e filtrare con gli strumenti di R.
dat[dat==""] <- NA

# quota di valori mancanti colonna per colonna
apply(dat, 2, function(x) mean(is.na(x)))

# distribuzione delle u.s. per Scuola (colonna 1) e per corso di laurea
# (colonna 2), in ordine crescente per vedere subito i casi piu' scarni
(table(dat[,1]))
(sort(table(dat[,2]),decreasing=F))
# per un paio di corsi l'algoritmo non ha funzionato
# ha preso troppi pochi esami. Li aggiungiamo?

# nomi di insegnamento piu' ricorrenti nell'intero ateneo
most_freq <- sort(table(dat$Course),decreasing=T)
(most_freq)[1:20]
# le cose più frequenti sono tirocini e testi
# che però non hanno descrizione


# Rimozione dei non esami e dei corsi con soli NA -------------------------

# esempio di cio' che si sta per eliminare: le righe "TIROCINIO" non hanno
# un programma da analizzare
(dat[dat[,3] == "TIROCINIO",])

# voci che compaiono nel catalogo ma non sono insegnamenti con un programma:
# prova finale, idoneita' di lingua, tirocini e stage
non_esami <- (dat$Course == "PROVA FINALE" |
  dat$Course == "LINGUA INGLESE B2 (ABILITA' RICETTIVE)" |
  dat$Course == "TIROCINIO"  |
  dat$Course == "TIROCINIO (TERZO ANNO)"  |
  dat$Course == "STAGE")

# righe prive di entrambi i campi di testo: senza testo non c'e' nulla da
# classificare, mentre basta che uno dei due sia presente per tenerle
all_null <- is.na(dat$Conoscenze.e.abilita..da.acquisire) &
  is.na(dat$Contenuti)

dat <- dat[!(non_esami | all_null),]   # si tengono le righe che non sono ne' l'uno ne' l'altro
dim(dat) #3456 u.s.

# quanti URL distinti restano: ogni URL e' una pagina del Syllabus, quindi
# la differenza con il numero di righe misura la duplicazione
length(unique(dat$URL))
# 2589 URL unici su 3456 osservazioni
# quindi circa 900 da rimuovere


# Rimozione degli URL duplicati -------------------------------------------

# gli URL piu' ripetuti: sono insegnamenti condivisi tra piu' corsi di laurea,
# che lo scraper ha incontrato una volta per ciascun corso
(sort(table(dat$URL),decreasing=T))[1:20]

# duplicated() marca TRUE ogni occorrenza successiva alla prima,
# quindi di ogni URL sopravvive esattamente una riga
url_to_delete = base::duplicated(dat$URL)

(dat[url_to_delete,])      # ispezione di cio' che verra' rimosso

length(unique(dat$URL))

dat <- dat[!url_to_delete,]
dim(dat) #2589 u.s.
(dat)
(sort(table(dat$URL),decreasing=T))[1:5]   # ora nessun URL compare piu' di una volta

# composizione per Scuola in percentuale, per vedere quanto e' sbilanciata
# la variabile che i modelli dovranno prevedere
(round(prop.table(table(dat[,1]))*100,2))
# solo il 3.45% delle osservazioni da giurisprudenza
(sort(table(dat[,2]),decreasing=F))
(sort(table(dat[,3]),decreasing=F))

apply(dat, 2, function(x) mean(is.na(x))*100)
#ancora un 2.29% di NA su contenuti, come li gestiamo?


# Rimozione dei testi duplicati -------------------------------------------

# Lo stesso programma puo' comparire sotto URL diversi, quindi il controllo
# sugli URL non basta: qui si confrontano i testi. Per non confrontare
# stringhe lunghe si passa da normalizzazione a hash intero:
#   normalizzaTesti() uniforma maiuscole, accenti e punteggiatura, cosi' due
#   testi che differiscono solo per questi dettagli danno lo stesso hash;
#   digest2int() comprime ogni testo in un singolo numero.
library(digest)
library(TextWiller)
Conoscenze.hash <- sapply(normalizzaTesti(dat$Conoscenze.e.abilita..da.acquisire), digest2int)
Contenuti.hash <- sapply(normalizzaTesti(dat$Contenuti), digest2int)

Conoscenze.duplicated <- base::duplicated(Conoscenze.hash)
Contenuti.duplicated <- base::duplicated(Contenuti.hash)

#View(dat[Conoscenze.duplicated&Contenuti.duplicated, ])

# si elimina solo chi ha ENTRAMBI i campi gia' visti (AND, non OR): un
# insegnamento che condivide le conoscenze ma ha contenuti propri resta
dat <- dat[!(Conoscenze.duplicated&Contenuti.duplicated), ]
dim(dat) #2135 u.s.

# Conviene fare lo stemming prima di questa parte?

View(dat[dat$Course=="ANTROPOLOGIA CULTURALE",])   # controllo su un caso noto

(round(prop.table(table(dat[,1]))*100,2))
# solo il 4.12% delle osservazioni da giurisprudenza
(sort(table(dat[,2]),decreasing=F))
(sort(table(dat[,3]),decreasing=F))

# Problema dei moduli e pari/dispari --------------------------------------
# modulo A e B etc

# Alcuni insegnamenti sono sdoppiati per gestire molti studenti: due docenti
# si dividono l'aula per canale, per iniziale del cognome o per matricola
# pari/dispari. Le righe hanno descrizioni identiche ma sono erogazioni
# distinte, quindi vanno riconosciute dal nome del corso prima di decidere.
library(data.table)
cognome <- dat$Course %like% "cognome"                              # "... (A-L)" per cognome
canale <- dat$Course %like% "canale"                                # canale 1, canale 2, ...
pari_dispari <- dat$Course %like% "pari" | dat$Course %like% "dispari"   # matricole pari/dispari

#View(dat[pari_dispari,])

# gli hash vanno ricalcolati: le righe sono cambiate dopo l'ultimo filtro
Conoscenze.hash <- sapply(normalizzaTesti(dat$Conoscenze.e.abilita..da.acquisire), digest2int)
Contenuti.hash <- sapply(normalizzaTesti(dat$Contenuti), digest2int)

Conoscenze.duplicated <- base::duplicated(Conoscenze.hash)
Contenuti.duplicated <- base::duplicated(Contenuti.hash)

# qui il criterio e' piu' permissivo del precedente: basta UNO dei due campi
# duplicato (OR), ma solo per le righe riconosciute come sdoppiate
to_delete <- ((Conoscenze.duplicated|Contenuti.duplicated)&cognome) |
  ((Conoscenze.duplicated|Contenuti.duplicated)&canale) |
  ((Conoscenze.duplicated|Contenuti.duplicated)&pari_dispari)

View(dat[dat$Course %like% "COMPETENZE INFORMATICHE DI BASE",])   # verifica su un corso sdoppiato

dat <- dat[!to_delete, ]

dim(dat) #2092


# Traduzione --------------------------------------------------------------

# Alcuni insegnamenti hanno il programma in inglese. La traduzione automatica
# e' rimasta allo stato di traccia: googleLanguageR richiede le credenziali
# dell'API di Google, quindi la chiamata qui sotto non e' completa e
# interrompe lo script se eseguita cosi' com'e'.
library(googleLanguageR)
translate("", source.)

# Salvataggio -------------------------------------------------------------

# il dataset ripulito e' l'input di 2_analisi_lessicale_ed_esplorativa.R
save(dat, file="../Datasets/unipd_courses_cleaned.Rdata")
