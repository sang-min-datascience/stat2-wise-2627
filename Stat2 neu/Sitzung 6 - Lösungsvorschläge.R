# ==============================================================================
# Statistik 2: Fragebögen und Latente Konstrukte
# Skript 2: Musterlösung & Screencast-Checklisten
# ==============================================================================

library(tidyverse)
library(psych)
library(corrplot)
library(broom)
# library(DEIN_PAKET_NAME) 

nur_fragen <- festival_umfrage %>% select(Q01_Musik:Q12_Merch_Preis)

# ------------------------------------------------------------------------------
# Block 1 & 2: Korrelationen und Dimensionsreduktion (Scree-Plot)
# ------------------------------------------------------------------------------

# Wir sehen visuell starke Blöcke (z.B. Q01 bis Q04)
korrelationen <- cor(nur_fragen)
corrplot(korrelationen, method = "color", type = "upper", tl.col = "black")

# Die Analyse zeigt exakt 3 Faktoren über der roten Linie.
fa.parallel(nur_fragen, fa = "fa", fm = "minres")

# --- SCREENCAST-CHECKLISTE: SCREE-PLOT ----------------------------------------
# [ ] Erklären Sie, warum Sie überhaupt eine Faktoranalyse machen (Problem der 
#     Multikollinearität im Fragebogen).
# [ ] Zeigen Sie den Scree-Plot und erklären Sie die Entscheidungsregel 
#     (Blaue Dreiecke vs. Rote gestrichelte Zufallslinie).
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Block 3: Faktorladungen interpretieren
# ------------------------------------------------------------------------------

modell_efa <- fa(nur_fragen, nfactors = 3, rotate = "oblimin", fm = "minres")
print(modell_efa$loadings, cutoff = 0.3, sort = TRUE)

# Interpretation der Matrix (Ihre MR-Zahlen können je nach R-Version leicht variieren):
# MR1: Lädt hoch auf Ticket, Bier, Essen, Merch. -> Name: "Preisempfinden"
# MR2: Lädt hoch auf Klos, Einlass, Gastro_Warte, Security. -> Name: "Organisation"
# MR3: Lädt hoch auf Musik, Licht, Stimmung, Sound. -> Name: "Atmosphäre"

# --- SCREENCAST-CHECKLISTE: FAKTORLADUNGEN ------------------------------------
# [ ] Blenden Sie die Ladungs-Matrix ein.
# [ ] Erklären Sie anhand eines Beispiels (z.B. Faktor MR1), wie Sie aus den 
#     mathematischen Werten (den Ladungen) auf den inhaltlichen Namen des 
#     latenten Konstrukts geschlossen haben.
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Block 4: Die End-to-End Pipeline (Supervised Learning)
# ------------------------------------------------------------------------------

# Scores extrahieren und benennen (Passen Sie die MR-Zahlen an Ihren Output an!)
scores_df <- as.data.frame(modell_efa$scores) %>% 
  rename(
    faktor_preis = MR1, 
    faktor_orga  = MR2, 
    faktor_atmos = MR3
  )

# Zielvariable hinzufügen
finale_daten <- bind_cols(scores_df, kommt_wieder = festival_umfrage$kommt_wieder)

# Das saubere, logistische Regressionsmodell ohne Multikollinearität
modell_final <- glm(kommt_wieder ~ faktor_preis + faktor_orga + faktor_atmos, 
                    data = finale_daten, 
                    family = "binomial")

tidy(modell_final)

# ERGEBNIS FÜR DAS MANAGEMENT:
# Der p-Wert für 'faktor_orga' ist extrem hoch (nicht signifikant). 
# Das bedeutet: Ob die Toiletten sauber waren und der Einlass schnell ging, 
# hat in diesen Daten absolut keinen nachweisbaren Einfluss darauf, ob die 
# Leute nächstes Jahr wiederkommen. Die entscheidenden Treiber (hochsignifikant) 
# sind der Vibe (Atmosphäre) und der Preis. 

# --- SCREENCAST-CHECKLISTE: DAS FINALE MODELL ---------------------------------
# [ ] Erklären Sie, warum das Nutzen der 3 Faktor-Scores im Regressionsmodell 
#     mathematisch "sicherer" ist als alle 12 Einzelfragen hineinzuwerfen.
# [ ] Ziehen Sie ein datengetriebenes Geschäfts-Fazit auf Basis der p-Werte 
#     der drei neuen Dimensionen.
# ------------------------------------------------------------------------------