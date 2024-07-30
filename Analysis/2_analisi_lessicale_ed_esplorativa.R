rm(list=ls())

library(tidyverse)
library(tidytext)
library(tidymodels)
library(TextWiller)

load("unipd_courses_cleaned.Rdata")

dat[dat==""] <- NA
apply(dat, 2, function(x) mean(is.na(x)))

glimpse(dat)

dat <- dat %>% mutate(full.text = paste(Contenuti, Conoscenze.e.abilita..da.acquisire))

dat <- dat %>% mutate(
  full.text = normalizzaTesti(full.text)
)
glimpse(dat)
nrow(dat)

# Rappresentazione ####

library(wordcloud)
library(colorspace)

tidy_text <- tibble(text = dat[, 7]) %>% unnest_tokens(word, text)

x11()
tidy_text %>% 
  count(word, sort=TRUE) %>% 
  filter(n>(dim(tidy_text)[1]*0.002)) %>% mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n)) + geom_col() + coord_flip()

set.seed(123)
word_counts <- tidy_text %>%
  count(word, sort = TRUE)
x11()
wordcloud::wordcloud(words = word_counts$word, 
                     freq = word_counts$n, 
                     min.freq = 500, 
                     max.words = 100, 
                     random.order = FALSE, 
                     colors = brewer.pal(8, "Dark2"))

bigrammi_text <- dat %>% 
  unnest_tokens(bigrammi,full.text,token='ngrams',n=2) %>% count(bigrammi, sort=T)

trigrammi_text <- dat %>% 
  unnest_tokens(trigrammi,full.text,token='ngrams',n=3) %>% 
  count(trigrammi, sort=T)

# Stemming ####

library(tm)
dat <- dat %>% mutate( full.text=stemDocument(full.text,
                                              language = "italian") )

tidy_text <- tibble(text = dat[, 7]) %>% unnest_tokens(word, text)
View(tidy_text %>% table() %>% sort(decreasing = TRUE))

# Stopword extra ####

stop_list <- c("cors", "student", "conoscent", "na")

for (word in stop_list){
  dat <- dat %>% 
    mutate(full.text=
             sapply(full.text, FUN=gsub,
                    pattern=word,
                    replacement="" ) )
}

# Unione bigrammi ---------------------------------------------------------

bigrammi_text <- dat %>% 
  unnest_tokens(bigrammi,full.text,token='ngrams',n=2) %>% 
  count(bigrammi, sort=T)

bigrammi <- list(c("unione europea", "unione_europea"),
                 c("diritti umani", "diritti_umani"),
                 c("guerra fredda", "guerra_fredda"),
                 c("terzo settore", "terzo_settore"),
                 c("primo settore", "primo_settore"),
                 c("secondo settore", "secondo_settore"),
                 c("campo magnetico", "campo_magnetico"),
                 c("sistemi lineari", "sistemi_lineari"),
                 c("equazioni differenziali", "equazioni_differenziali"),
                 c("applicazioni lineari", "applicazioni_lineari"),
                 c("spazio vettoriale", "spazio_vettoriale"),
                 c("corpo rigido", "corpo_rigido"),
                 c("reti elettriche", "reti_elettriche"),
                 c("energia cinetica", "energia_cinetica"),
                 c("sistemi riferimento", "sistemi_riferimento"),
                 c("legge gauss", "legge_gauss"),
                 c("circuiti elettronici", "circuiti_elettronici"),
                 c("equazioni maxwell", "equazioni_maxwell"),
                 c("legge gauss", "legge_gauss"),
                 c("leggi kirchhoff", "leggi_kirchhoff"),
                 c("analisi reti", "analisi_reti"),
                 c("sistema nervoso", "sistema_nervoso"),
                 c("corpo umano", "corpo_umano"),
                 c("fattori rischio", "fattori_rischio"),
                 c("medicina legale", "medicina_legale"),
                 c("primo soccorso", "primo soccorso"),
                 c("ciclo krebs", "ciclo_krebs"),
                 c("algebra lineare", "algebra_lineare"),
                 c("esercizio fisico", "esercizio_fisico"),
                 c("attività fisica", "attività_fisica"),
                 c("test ipotesi", "test_ipotesi"),
                 c("malattie infettive", "malattie_infettive"),
                 c("casi clinici", "casi_clinici"),
                 c("momento angolare", "momento_angolare"),
                 c("testo letterario", "testo_letterario"),
                 c("storia arte", "storia_arte"),
                 c("patrimonio culturale", "patrimonio_culturale"),
                 c("media digitali", "media_digitali"),
                 c("letteratura italiana", "letteratura_italiana"),
                 c("analisi dati", "analisi_dati"),
                 c("meccanica quantistica", "meccanica_quantistica"),
                 c("terminologia scientifica", "terminologia_scientifica"),
                 c("chimica organica", "chimica_organica"),
                 c("acidi nucleici", "acidi_nucleici"),
                 c("reazioni chimiche", "reazioni_chimiche"),
                 c("proprietà fisiche", "proprietà_fisiche"),
                 c("sistema solare", "sistema_solare"),
                 c("legge ohm", "legge_ohm"),
                 c("minimi quadrati", "minimi_quadrati"),
                 c("tavola periodica", "tavola_periodica"),
                 c("unità misura", "unità_misura"),
                 c("psicologia clinica", "psicologia_clinica"),
                 c("inferenza statistica", "inferenza_statistica"),
                 c("statistiche descrittive", "statistiche_descrittive"),
                 c("regressione linere", "regressione_lineare"),
                 c("psicologia sociale", "psicologia_sociale"),
                 c("psicologia clinica", "psicologia_clinica"),
                 c("onde elettromagnetiche", "onde_elettromagnetiche"),
                 c("equazioni non lineari", "equazioni_non_lineari"),
                 c("dati personali", "dati_personali"),
                 c("lingua inglese", "lingua_inglese"),
                 c("leggi gas", "leggi_dei_gas"),
                 c("età evolutiva", "età_evolutiva"),
                 c("casi studio", "casi_di_studio"),
                 c("acidi basi", "acidi&basi"),
                 c("lezioni frontali", "lezioni_frontali"),
                 c("variabili aleatorie", "variabili_aleatorie"),
                 c("[[:digit:]][[:digit:]] credito", ""),
                 c("[[:digit:]][[:digit:]] cfu", ""),
                 c("[[:digit:]][[:digit:]] ore", ""),
                 c("[[:digit:]] credito", ""),
                 c("[[:digit:]] cfu", ""),
                 c("[[:digit:]] ore", ""),
                 c("[[:digit:]]° / [[:digit:]]° credito", ""),
                 c("[[:digit:]]° e [[:digit:]]° credito", ""),
                 c("[[:digit:]]° credito", ""),
                 c("parte [[:digit:]]",""))

for (bigramma in bigrammi){
  dat <- dat %>% 
    mutate(full.text=
             sapply(full.text, FUN=gsub,
                    pattern=stemDocument(bigramma[1], language="italian"),
                    replacement=bigramma[2]) ) 
}

# Trigrammi ####

trigrammi_text <- dat %>% 
  unnest_tokens(bigrammi,full.text,token='ngrams',n=3) %>% count(bigrammi, sort=T)

trigrammi <- list(c("alimenti origine animale", 
                    "alimenti_origine_animale"), #15
                  c("prodotti origine animale", 
                    "prodotti_origine_animale"),#7
                  c("microrganismi interesse alimentare", "microrganismi_interesse_alimentare"),#5
                  c("tessuti origine animale", 
                    "tessuti_origine_animale"),#4
                  c("gestione ciclo fruttificazione", "gestione_ciclo_fruttificazione"),#7
                  c("prodotto panorama varietale", 
                    "prodotto_panorama_varietale"),#7
                  c("reazioni ossido riduzione", 
                    "reazioni_ossido_riduzione"),#5
                  c("acidi carbossilici anidridi", 
                    "acidi_carbossilici_anidridi"),#4
                  c("comparto suinicolo nazionale", 
                    "comparto_suinicolo_nazionale"),#4S
                  c("conservazione energia meccanica", "conservazione_energia_meccanica"),#4
                  c("effetto ione comune", 
                    "effetto_ione_comune"),#4
                  c("legame ionico covalente", 
                    "legame_ionico_covalente"),#4
                  c("proprietà chimico fisiche", 
                    "proprietà_chimico_fisiche"),#4
                  c("stati aggregazione materia", 
                    "stati_aggregazione_materia"),#4
                  c("stima breding value",
                    "stima_breeding_value"),#4
                  c("teoria orbitale molecolare",
                    "teoria_orbitale_molecolare"),#4
                  c("van der wals", "Van_Der_Waals"), #4S
                  c("vitello carne bianca", 
                    "vitello_carne_bianca"),#4
                  c("abilità comunicative publiche", 
                    "abilità_comunicative_publiche"),#7
                  c("linguaggio vocabolario giuridico",  "linguaggio_vocabolario_giuridico"),#11
                  c("rapporti causa effetto", 
                    "rapporti_causa_effetto"),#6
                  c("strumenti politica economica", 
                    "strumenti_politica_economica"),#6
                  c("principi fondamentali diritto", 
                    "principi_fondamentali_diritto"),#5
                  c("struttura codice civile", 
                    "struttura_codice_civile"),#5
                  c("istituti diritto privato", 
                    "istituti_diritto_privato"),#4
                  c("linguaggio politologico contemporaneo", "linguaggio_politologico_contemporaneo"),#4
                  c("linguaggio tecnico appropriato", "linguaggio_tecnico_appropriato"),#4
                  c("modello regressione lineare", 
                    "modello_regressione_lineare"),#7S
                  c("prima guerra mondiale", 
                    "prima_guerra_mondiale"),#7S
                  c("seconda guerra mondiale", 
                    "seconda_guerra_mondiale"),#7S
                  c("vicende rapporto obbligatorio", 
                    "vicende_rapporto_obbligatorio"),#6
                  c("fatti illeciti responsabilità", 
                    "fatti_illeciti_responsabilità"),#5
                  c("fonti non contrattuali", 
                    "fonti_non_contattuali"),#5
                  c("introduzione linguaggio giuridico", "introduzione_linguaggio_giuridico"),#5
                  c("situazioni giuridiche soggettive", 
                    "situazioni_giuridiche_soggettive"),#5
                  c("beni mercati finanziari", 
                    "beni_mercati_finanziari"),#4
                  c("beni proprietà diritti", 
                    "beni_proprietà_diritti"),#4
                  c("capitale umano sociale", 
                    "capitale_umano_sociale"),#4
                  c("materia civile commerciale", 
                    "materia_civile_commerciale"),#4
                  c("processo integrazione europea", 
                    "processo_integrazione_europea"),#4
                  c("attuale sistema giuridico", 
                    "attuale_sistema_giuridico"),#6
                  c("positivo attuale sistema", 
                    "positivo_sistema_attuale"),#6
                  c("concorso persone reato", 
                    "concorso_persone_reato"),#3
                  c("protezione dati personali", 
                    "protezione_dati_personali"),#3
                  c("2019 2161 ue","2161_ue_2019"),#2
                  c("2019 770 ue","770_ue_2019"),#2
                  c("2019 771 ue","771_ue_2019"),#2
                  c("770 ue 2019","770_ue_2019"),#2
                  c("771 ue 2019","771_ue_2019"),#2
                  c("calcolo differenziale integrale", 
                    "calcolo_differenziale_integrale"),#9
                  c("generatori_ideali_indipendenti",
                    "generatori_ideali_indipendenti"),
                  c("ideali_indipendenti_tensione",
                    "ideali_indipendenti_tensione"),
                  c("circuiti_logici_combinatori",
                    "circuiti_logici_combinatori"),
                  c("principali_periferiche_interfacciamento",
                    "principali_periferiche_interfacciamento"),
                  c("tempo_reale_sistemi","tempo_reale_sistemi"),
                  c("funzioni variabile reale", 
                    "funzioni_variabile_reale"),
                  c("fondamentale calcolo integrale",
                    "fondamentale_calcolo_integrale"),
                  c("gramschmidt proiezioni",
                    "gramschmidt_proiezioni"),
                  c("principio lavori virtuali",
                    "principio_lavori_virtuali"),
                  c("schmidt proiezioni ortogonali",
                    "gramschmidt_proiezioni"),
                  c("idealità amplificatori operazionali",
                    "idealità_amplificatori_operazionali"),
                  c("legge ampere maxwell",
                    "legge_ampere_maxwell"),
                  c("legge elementare laplace",
                    "legge_elementare_laplace"),
                  c("cauchy schwarz disuguaglianza",
                    "disuguaglianza_cauchy_schwarz"),
                  c("de saint venant","de_saint_venant"),
                  c("disuguaglianza cauchy schwarz",
                    "disuguaglianza_cauchy_schwarz"),
                  c("principali aspetti legislativi",
                    "principali_aspetti_legislativi"),
                  c("servizio sanitario nazionale",
                    "servizio_sanitario_nazionale"),
                  c("basi cellulari molecolari",
                    "basi_cellulari_molecolari"),
                  c("cantieri temporanei mobili",
                    "cantieri_temporanei_mobili"),
                  c("disturbi spettro autistico",
                    "disturbi_spettro_autistico"),
                  c("lgs 81 08","lgs_81_08"),
                  c("ossidazione acidi grassi",
                    "ossidazione_acidi_grassi"),
                  c("scienze motorie sportive",
                    "scienze_motorie_sportive"),
                  c("lgs 502 1992","lgs_502_1992"),
                  c("regressione lineare semplice",
                    "regressione_lineare_semplice"),
                  c("sicurezza alimentare qualità",
                    "sicurezza_alimentare_qualità"),
                  c("spettro autistico adhd",
                    "spettro_autistico_adhd"),
                  c("personalità differenze individuali",
                    "personalità_differenze_individuali"),
                  c("principali modelli teorici",
                    "principali_modelli_teorici"),
                  c("reti_neurali_artificiali",
                    "reti_neurali_artificiali"),
                  c("linguaggio comunicativo sviluppo",
                    "linguaggio_comunicativo_sviluppo"),
                  c("modello_cognitivo_sviluppato",
                    "modello_cognitivo_sviluppato"),
                  c("scambi materia energia",
                    "scambi_materia_energia"),
                  c("tessuti animali vegetali",
                    "tessuti_animali_vegetali"),
                  c("acidi carbossilici derivati",
                    "acidi_carbossilici_derivati"),
                  c("derivati acidi carbossilici",
                    "derivati_acidi_carbossilici"),
                  c("principio esclusione pauli",
                    "principio_esclusione_pauli"),
                  c("legge lambert ber","legge_lambert_ber"),
                  c("teorema limite centrale",
                    "teorema_limite_centrale"),
                  c("arrhenius brønsted lowry",
                    "arrhenius_brønsted_lowry"),
                  c("parallelo regole kirchhoff",
                    "parallelo_regole_kirchhoff"),
                  c("xix xx secolo","xix_xx_secolo"),
                  c("xx xxi secolo","xx_xxi_secolo"),
                  c("tempi präsens perfekt",
                    "tempi_präsens_perfekt"))

for (trigramma in trigrammi){
  dat <<- dat %>% 
    mutate(full.text=
             sapply(full.text, FUN=gsub,
                    pattern=stemDocument(trigramma[1], language="italian"),
                    replacement=trigramma[2]) ) 
}

#library(data.table)
#
#tidy_text <- data.frame(text = dat[, 7]) %>% unnest_tokens(word, text)
#t <- tidy_text %>% table()
#t[t %like% "_"]

# Salva ####
save(dat, file="unipd_courses_vFinale.Rdata")










# Parte 2: Analisi esplorativa ####
## valutazione ####

tidy_text <- tibble(text = dat[, 7]) %>% unnest_tokens(word, text)

wc_cono <- wordcloud(
  tidy_text %>% count(word) %>% pull(word),
  tidy_text %>% count(word) %>% pull(n), max.words = 200, min.freq=(dim(tidy_text)[1]*0.5)
)

tidy_text %>% 
  count(word, sort=TRUE) %>% 
  filter(n>(dim(tidy_text)[1]*0.0026)) %>% mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n)) + geom_col() + coord_flip()

library(data.table)
t <- tidy_text %>% base::table()
View(t[names(t) %like% "_"])

(dat[dat$full.text %like% "prima_guerra_mondialee6",])

bigrammi_text <- dat %>% 
  unnest_tokens(bigrammi,full.text,token='ngrams',n=2) %>% 
  count(bigrammi, sort=T)

trigrammi_text <- dat %>% 
  unnest_tokens(trigrammi,full.text,token='ngrams',n=3) %>% 
  count(trigrammi, sort=T)


## Analisi esplorativa (Andrea Cocco) -----------------------------------------

# Si pensava di fare text mining sia su tutti i corsi sia sui corsi specifici per le scuole
# Una idea buona può essere fare un wordcloud per capire le tematiche più in voga per le varie scuole
text_amv1 <- data_frame(text=dat[dat$School=="Scuola di Agraria e Medicina Veterinaria",7])
text_esp1 <- data_frame(text=dat[dat$School=="Scuola di Economia e Scienze politiche",7])
text_g1 <- data_frame(text=dat[dat$School=="Scuola di Giurisprudenza",7])
text_i1 <- data_frame(text=dat[dat$School=="Scuola di Ingegneria",7])
text_mc1 <- data_frame(text=dat[dat$School=="Scuola di Medicina e Chirurgia",7])
text_p1 <- data_frame(text=dat[dat$School=="Scuola di Psicologia",7])
text_s1 <- data_frame(text=dat[dat$School=="Scuola di Scienze",7])
text_susp1 <- data_frame(text=dat[dat$School=="Scuola di Scienze umane, sociali e del patrimonio culturale",7])

tidy_amv1 <- text_amv1 %>%
  unnest_tokens(word, text)
tidy_esp1 <- text_esp1 %>%
  unnest_tokens(word, text)
tidy_g1 <- text_g1 %>%
  unnest_tokens(word, text)
tidy_i1 <- text_i1 %>%
  unnest_tokens(word, text)
tidy_mc1 <- text_mc1 %>%
  unnest_tokens(word, text)
tidy_p1 <- text_p1 %>%
  unnest_tokens(word, text)
tidy_s1 <- text_s1 %>%
  unnest_tokens(word, text)
tidy_susp1 <- text_susp1 %>%
  unnest_tokens(word, text)

word_counts <- tidy_amv1 %>%
  count(word, sort = TRUE)
wordcloud::wordcloud(words = word_counts$word, 
          freq = word_counts$n, 
          min.freq = 10, 
          max.words = 100, 
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

# Conteggio delle parole
word_counts <- tidy_esp1 %>%
  count(word, sort = TRUE)
# Creazione della wordcloud
wordcloud(words = word_counts$word, 
          freq = word_counts$n, 
          min.freq = 10, 
          max.words = 100, 
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

# Conteggio delle parole
word_counts <- tidy_g1 %>%
  count(word, sort = TRUE)
# Creazione della wordcloud
x11()
wordcloud(words = word_counts$word, 
          freq = word_counts$n, 
          min.freq = 10, 
          max.words = 100, 
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

# Conteggio delle parole
word_counts <- tidy_i1 %>%
  count(word, sort = TRUE)
# Creazione della wordcloud
x11()
wordcloud(words = word_counts$word, 
          freq = word_counts$n, 
          min.freq = 10, 
          max.words = 100, 
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

# Conteggio delle parole
word_counts <- tidy_i2 %>%
  count(word, sort = TRUE)
# Creazione della wordcloud
wordcloud(words = word_counts$word, 
          freq = word_counts$n, 
          min.freq = 10, 
          max.words = 100, 
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

# Conteggio delle parole
word_counts <- tidy_mc1 %>%
  count(word, sort = TRUE)
# Creazione della wordcloud
x11()
wordcloud(words = word_counts$word, 
          freq = word_counts$n, 
          min.freq = 10, 
          max.words = 100, 
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

# Conteggio delle parole
word_counts <- tidy_p1 %>%
  count(word, sort = TRUE)
# Creazione della wordcloud
wordcloud(words = word_counts$word, 
          freq = word_counts$n, 
          min.freq = 10, 
          max.words = 100, 
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

# Conteggio delle parole
word_counts <- tidy_s1 %>%
  count(word, sort = TRUE)
# Creazione della wordcloud
wordcloud(words = word_counts$word, 
          freq = word_counts$n, 
          min.freq = 10, 
          max.words = 100, 
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

# Conteggio delle parole
word_counts <- tidy_susp1 %>%
  count(word, sort = TRUE)
x11()
# Creazione della wordcloud
wordcloud(words = word_counts$word, 
          freq = word_counts$n, 
          min.freq = 10, 
          max.words = 100, 
          random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

## Istogrammi ####

tidy_amv1 %>% 
  count(word, sort=TRUE) %>% 
  filter(n>(dim(tidy_amv1)[1]*0.002)) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n)) + geom_col() + coord_flip()

tidy_amv2 %>% 
  count(word, sort=TRUE) %>% 
  filter(n>(dim(tidy_amv2)[1]*0.002)) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n)) + geom_col() + coord_flip()

tidy_esp1 %>% 
  count(word, sort=TRUE) %>% 
  filter(n>(dim(tidy_esp1)[1]*0.002)) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n)) + geom_col() + coord_flip()

tidy_esp2 %>% 
  count(word, sort=TRUE) %>% 
  filter(n>(dim(tidy_esp2)[1]*0.002)) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n)) + geom_col() + coord_flip()

tidy_g1 %>% 
  count(word, sort=TRUE) %>% 
  filter(n>(dim(tidy_g1)[1]*0.002)) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n)) + geom_col() + coord_flip()
tidy_g2 %>% 
  count(word, sort=TRUE) %>% 
  filter(n>(dim(tidy_g2)[1]*0.002)) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n)) + geom_col() + coord_flip()
tidy_i1 %>% 
  count(word, sort=TRUE) %>% 
  filter(n>(dim(tidy_i1)[1]*0.002)) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n)) + geom_col() + coord_flip()
tidy_i2 %>% 
  count(word, sort=TRUE) %>% 
  filter(n>(dim(tidy_i2)[1]*0.002)) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n)) + geom_col() + coord_flip()
tidy_mc1 %>% 
  count(word, sort=TRUE) %>% 
  filter(n>(dim(tidy_mc1)[1]*0.002)) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n)) + geom_col() + coord_flip()
tidy_mc2 %>% 
  count(word, sort=TRUE) %>% 
  filter(n>(dim(tidy_mc2)[1]*0.002)) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n)) + geom_col() + coord_flip()
tidy_p1 %>% 
  count(word, sort=TRUE) %>% 
  filter(n>(dim(tidy_p1)[1]*0.002)) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n)) + geom_col() + coord_flip()
tidy_p2 %>% 
  count(word, sort=TRUE) %>% 
  filter(n>(dim(tidy_p2)[1]*0.002)) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n)) + geom_col() + coord_flip()
tidy_s1 %>% 
  count(word, sort=TRUE) %>% 
  filter(n>(dim(tidy_s1)[1]*0.002)) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n)) + geom_col() + coord_flip()
tidy_s2 %>% 
  count(word, sort=TRUE) %>% 
  filter(n>(dim(tidy_s2)[1]*0.002)) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n)) + geom_col() + coord_flip()
tidy_susp1 %>% 
  count(word, sort=TRUE) %>% 
  filter(n>(dim(tidy_susp1)[1]*0.002)) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n)) + geom_col() + coord_flip()
tidy_susp2 %>% 
  count(word, sort=TRUE) %>% 
  filter(n>(dim(tidy_susp2)[1]*0.002)) %>% 
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n)) + geom_col() + coord_flip()


## Ricerca bigrammi --------------------------------------------------------

bigrammiamv1 <- text_amv1 %>% 
  unnest_tokens(bigrammi,text,token='ngrams',n=2) %>% 
  count(bigrammi, sort=T)

bigrammiamv2 <- text_amv2 %>% 
  unnest_tokens(bigrammi,text,token='ngrams',n=2) %>% 
  count(bigrammi, sort=T)

bigrammiesp1 <- text_esp1 %>% 
  unnest_tokens(bigrammi,text,token='ngrams',n=2) %>% 
  count(bigrammi, sort=T)

bigrammiesp2 <- text_esp2 %>% 
  unnest_tokens(bigrammi,text,token='ngrams',n=2) %>% 
  count(bigrammi, sort=T)

bigrammig1 <- text_g1 %>% 
  unnest_tokens(bigrammi,text,token='ngrams',n=2) %>% 
  count(bigrammi, sort=T)

bigrammig2 <- text_g2 %>% 
  unnest_tokens(bigrammi,text,token='ngrams',n=2) %>% 
  count(bigrammi, sort=T)

bigrammii1 <- text_i1 %>% 
  unnest_tokens(bigrammi,text,token='ngrams',n=2) %>% 
  count(bigrammi, sort=T)

bigrammii2 <- text_i2 %>% 
  unnest_tokens(bigrammi,text,token='ngrams',n=2) %>% 
  count(bigrammi, sort=T)

bigrammimc1 <- text_mc1 %>% 
  unnest_tokens(bigrammi,text,token='ngrams',n=2) %>% 
  count(bigrammi, sort=T)

bigrammimc2 <- text_mc2 %>% 
  unnest_tokens(bigrammi,text,token='ngrams',n=2) %>% 
  count(bigrammi, sort=T)

bigrammip1 <- text_p1 %>% 
  unnest_tokens(bigrammi,text,token='ngrams',n=2) %>% 
  count(bigrammi, sort=T)

bigrammip2 <- text_p2 %>% 
  unnest_tokens(bigrammi,text,token='ngrams',n=2) %>% 
  count(bigrammi, sort=T)

bigrammis1 <- text_s1 %>% 
  unnest_tokens(bigrammi,text,token='ngrams',n=2) %>% 
  count(bigrammi, sort=T)

bigrammis2 <- text_s2 %>% 
  unnest_tokens(bigrammi,text,token='ngrams',n=2) %>% 
  count(bigrammi, sort=T)

bigrammisusp1 <- text_susp1 %>% 
  unnest_tokens(bigrammi,text,token='ngrams',n=2) %>% 
  count(bigrammi, sort=T)

bigrammisusp2 <- text_susp2 %>% 
  unnest_tokens(bigrammi,text,token='ngrams',n=2) %>% 
  count(bigrammi, sort=T)



## Creare un vettore di parole inutili ####
dizionario <- c("di", "le", "la", "delle", "della", "lo", "del", "dei", "studente", "il", "conoscenze", "degli", "corso", "principali", "con", "che", "ed", "base", "gli", "alla", "una", "al", "grado", "da", "comprendere", "sulle", "studenti", "particolare", "loro", "acquisire", "nel", "abilità", "sulla", "acquisisce", "alle", "conoscere", "studio", "essere", "caratteristiche", "agli", "nozioni", "capacità", "struttura", "fornire","3", "1", "riconoscere", "è", "dovrà", "sui", "competenze", "2", "nelle", "dati", "anche", "ai", "saper", "si", "attraverso", "credito", "cfu", "ore", "4", "cenni", "6", "5", "tra", "saranno", "strumenti", "conoscenza", "termine", "svilupperanno", "abilita", "loro", "conoscenze", "analizzare", "analitiche", "più", "utilizzare", "identificare", "interpretare", "applicare", "pratichegli", "cognitiveal", "trasversaligli", "anche", "fornire", "diversi", "nella", "fondamentali")
tidy_amv1 %>% 
  count(word, sort=TRUE) %>% 
  filter(n>(dim(tidy_amv1)[1]*0.002) & (!word %in% dizionario)) %>% mutate(word = sapply(word, FUN=gsub, pattern = "animale", replacement="animali")) %>%
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n)) + geom_col() + coord_flip()
tidy_amv2 %>% 
  count(word, sort=TRUE) %>% 
  filter(n>(dim(tidy_amv2)[1]*0.002) & (!word %in% dizionario)) %>% mutate(word = sapply(word, FUN=gsub, pattern = "animale", replacement="animali")) %>%
  mutate(word=reorder(word,n)) %>% 
  ggplot(aes(word,n)) + geom_col() + coord_flip()

library(forcats)

tidy_esp1 %>% 
  count(word, sort=TRUE) %>% 
  filter(n>(dim(tidy_esp1)[1]*0.002) & (!word %in% dizionario)) %>% mutate(word = sapply(word, FUN=gsub, pattern = "politica", replacement="politiche")) %>%
  mutate(word=fct_reorder(word,n, .desc = T)) %>% 
  ggplot(aes(word,n)) + geom_col() + coord_flip()
tidy_esp1 %>%
  count(word, sort = TRUE) %>%
  filter(n > (dim(tidy_esp1)[1] * 0.002) & (!word %in% dizionario)) %>%
  mutate(word = gsub("politica", "politiche", word)) %>%
  mutate(word = fct_reorder(word, n, .desc = FALSE)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip()
tidy_esp1 %>%
  count(word, sort = TRUE) %>%
  filter(n > (dim(tidy_esp1)[1] * 0.002) & (!word %in% dizionario)) %>%
  mutate(word = gsub("politica", "politiche", word)) %>% # Porta "politiche" alla fine
  mutate(word = fct_reorder(word, n)) %>%  # Porta "diritto" subito prima di "politiche"
  mutate(word = fct_relevel(word, "politiche", after = Inf)) %>% 
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip()