# Text Mining on the University of Padua Course Catalogue

![R](https://img.shields.io/badge/R-tm%20%7C%20ggplot2-276DC3?logo=r&logoColor=white)
![Python](https://img.shields.io/badge/Python-3.12-3776AB?logo=python&logoColor=white)
![BeautifulSoup](https://img.shields.io/badge/BeautifulSoup-4.12.3-43B02A?logo=python&logoColor=white)
![LaTeX](https://img.shields.io/badge/LaTeX-report-008080?logo=latex&logoColor=white)

Can the School a university course belongs to be recognised from the text of
its syllabus alone? This project scrapes the whole Bachelor's degree catalogue
of the University of Padua, turns the course descriptions into a
document-term matrix, and trains two classifiers to predict the School from
the description.

The pipeline covers the three stages end to end: **web scraping** in Python,
**text cleaning and lexical analysis** in R, and **topic modelling and
classification** with a classification tree and a one-layer neural network,
compared under two weighting schemes of the DTM, TF and normalized TF-IDF.

**Academic context.** Written by Andrea Cocco, Andrea Quartuccio and Luca
Varotto for the *Metodi Statistici per i Big Data* (Statistical Methods for Big
Data) exam of the BSc in *Statistics for Technology and Science* at the
University of Padua, May 2024. The report is in Italian, and so are the
comments in the source files.

---

## The pipeline

Run the stages in order: each one consumes what the previous one saved in
`Datasets/`.

| Stage | File | What it does |
| --- | --- | --- |
| 1. Scraping | `scraping.py` | Walks `didattica.unipd.it` with three nested loops, School then Degree then Teaching, and writes one row per course with the fields *Conoscenze e abilità da acquisire*, *Contenuti* and the source URL. |
| 2. Cleaning | `Code/1_pulizia_dataset.R` | Drops rows where both text fields are missing, removes duplicate teachings by hashing their content with `digest`, and handles the courses split across channels or surname ranges. |
| 3. Lexical analysis | `Code/2_analisi_lessicale_ed_esplorativa.R` | Detects bigrams and trigrams, removes stopwords and the stems common to every School, applies Italian stemming with `tm`, and produces the wordclouds, frequency histograms and the aggregated exploratory analysis. |
| 4. Modelling | `Code/3_modellazione.Rmd` | Builds the DTM, explores it with PCA, t-SNE and *Latent Dirichlet Allocation*, then fits a classification tree and a one-layer neural network under both weighting schemes. |

The scripts assume the working directory is `Code/`, which is what RStudio
gives with *Session > Set Working Directory > To Source File Location* and what
knitr uses when rendering the Rmd.

---

## Repository structure

```
Text_mining_unipd_courses/
├── scraping.py                       # stage 1: the scraper
├── Code/
│   ├── 1_pulizia_dataset.R           # stage 2: cleaning
│   ├── 2_analisi_lessicale_ed_esplorativa.R   # stage 3: lexical analysis
│   └── 3_modellazione.Rmd            # stage 4: topic modelling and classifiers
├── Datasets/
│   ├── unipd_courses.csv             # raw scraping output, 3845 rows
│   ├── unipd_courses_cleaned.Rdata   # after stage 2
│   └── unipd_courses_vFinale.Rdata   # after stage 3, input of the models
├── Figures/                          # wordclouds, histograms, PCA, t-SNE, LDA, CV error
├── Report/
│   ├── Cocco-Quartuccio-Varotto.pdf  # the report (Italian)
│   ├── Cocco-Quartuccio-Varotto.tex  # its LaTeX source
│   ├── tau-book.cls                  # tau-book template, CC BY 4.0
│   ├── tau.sty                       #   "
│   └── tau-book-cls-READMe.md        # template documentation and attribution
├── .gitignore
└── README.md
```

The raw dataset has six columns — `School`, `Degree`, `Course`, the two
syllabus fields and the source `URL` — and covers **8 Schools, 81 Bachelor's
degrees and 3845 teachings**. Cleaning brings the corpus down to the **2092
teachings** that carry usable text.

---

## Results

Figures and numbers below come from the report, which discusses every step in
full.

**Topic modelling.** Latent Dirichlet Allocation over the 2092 teachings
recovers ten readable topics: Chemistry, Medicine, Mathematics, Law, Biology,
Agriculture, Economics, History and Literature, Physics and Pedagogy. Many
teachings mix several topics at once, which already anticipates the difficulty
the classifiers run into.

**Classification.** The target is the School, eight classes.

| Model | TF | Normalized TF-IDF |
| --- | --- | --- |
| Classification tree, 54 leaves | 55% | — |
| Tree pruned to 15 leaves | 49.2% | 54% |
| Neural network, 1 layer, 8 nodes | 67% (710 iterations) | **68%** (850 iterations) |

* The neural network is clearly the more accurate of the two, at the price of
  any interpretability. The tree instead gives readable rules: under TF, a
  course whose description contains the stem *diritt* goes to the School of Law.
* Pruning the tree from 54 to 15 leaves costs about six points of accuracy and
  buys a model that can be read at a glance.
* Switching from TF to normalized TF-IDF improves both models slightly, but the
  gain is small enough that a different train/test split could explain it.
* The errors are not spread evenly. The School of Engineering is recognised 73%
  of the time, while the School of Science only 51%, with 15% of its teachings
  assigned to Engineering.

**Conclusion.** The two syllabus fields alone do not separate the Schools
cleanly. Schools such as Science and Engineering share much of their
vocabulary, since mathematics and physics recur in both, and the overlap is
already visible in the PCA and t-SNE projections and in the topic model.

---

## References

1. Hastie, Tibshirani, Friedman (2009). *The Elements of Statistical Learning*.
   Springer, Berlin.
2. *Slides del corso di Metodi Statistici per i Big Data*, A. Canale, F. Denti,
   B. Scarpa.
3. M. Melucci. *Information Retrieval, macchine e motori di ricerca*.

---

## Authors

**Andrea Cocco**, **Andrea Quartuccio** and **Luca Varotto** — BSc in
Statistics for Technology and Science, University of Padua. Report written for
the *Metodi Statistici per i Big Data* exam, May 2024.

The report is typeset with the *tau-book* template by Guillermo Jimenez,
distributed under CC BY 4.0; see `Report/tau-book-cls-READMe.md`.
