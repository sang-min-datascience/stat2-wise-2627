# ==============================================================================
# Statistik 2: Fragebögen und Latente Konstrukte
# Skript 1: Aufgaben & Scaffolding
# ==============================================================================

library(tidyverse)
library(psych)
library(corrplot) # Für schöne Korrelations-Graphen
# library(DEIN_PAKET_NAME) 

# Wir isolieren die reinen 12 Fragen (ohne die Zielvariable)
nur_fragen <- festival_umfrage %>% select(Q01_Musik:Q12_Merch_Preis)

# ------------------------------------------------------------------------------
# Block 1: Das Chaos erkennen (Multikollinearität)
# ------------------------------------------------------------------------------

# Berechnen Sie die Korrelationsmatrix und plotten Sie sie.
korrelationen <- cor(_______)
corrplot(korrelationen, method = "color", type = "upper", tl.col = "black")

# Welche Fragen scheinen extrem stark zusammenzuhängen? 
# (Das sind die dunklen Quadrate abseits der Hauptdiagonale).


# ------------------------------------------------------------------------------
# Block 2: Wie viele Faktoren? (Der Scree-Plot)
# ------------------------------------------------------------------------------

# Wir bitten R, das Chaos zu strukturieren. 
# Aufgabe: Führen Sie die Parallelanalyse durch.
fa.parallel(_______, fa = "fa", fm = "minres")

# Screencast-Check: Wo ist der Knick? Wie viele blaue Dreiecke liegen über 
# der roten Linie? Notieren Sie Ihre Entscheidung: 
# Wir suchen nach _______ Faktoren.


# ------------------------------------------------------------------------------
# Block 3: Faktorladungen interpretieren (Namen vergeben)
# ------------------------------------------------------------------------------

# Wir extrahieren die Faktoren. (Setzen Sie Ihre Zahl bei nfactors ein!)
modell_efa <- fa(nur_fragen, nfactors = _______, rotate = "oblimin", fm = "minres")

# Wir betrachten die Ladungen (alles unter 0.3 wird ausgeblendet)
print(modell_efa$loadings, cutoff = 0.3, sort = TRUE)

# KREATIVE AUFGABE: Betrachten Sie die Spalten (MR1, MR2, MR3).
# Welche Fragen gehören zu MR1? Wie würden Sie diesen Faktor nennen? -> "________"
# Welche Fragen gehören zu MR2? Wie würden Sie diesen Faktor nennen? -> "________"
# Welche Fragen gehören zu MR3? Wie würden Sie diesen Faktor nennen? -> "________"


# ------------------------------------------------------------------------------
# Block 4: Die End-to-End Pipeline (Supervised Learning)
# ------------------------------------------------------------------------------

# Wir holen uns die berechneten Scores für jeden einzelnen Besucher
scores_df <- as.data.frame(modell_efa$scores)

# Wir benennen die Spalten (MR1, MR2...) mit unseren neuen, kreativen Namen um
scores_df <- scores_df %>% 
  rename(
    faktor_preis = MR1, # ACHTUNG: Prüfen Sie in Ihrem Output, welche MR-Nummer was war!
    faktor_atmos = MR2, 
    faktor_orga  = MR3
  )

# Wir kleben die Zielvariable aus dem Originaldatensatz wieder dran
finale_daten <- bind_cols(scores_df, kommt_wieder = festival_umfrage$kommt_wieder)

# DAS FINALE MODELL: Sagen Sie die Rückkehr (kommt_wieder) durch die 3 Faktoren vorher!
# Denken Sie an Kapitel 4 (Logistische Regression!).
modell_final <- glm(_______ ~ _______ + _______ + _______, 
                    data = finale_daten, 
                    family = "_______")

library(broom)
tidy(modell_final)

# Was ist Ihre Erkenntnis für das Management? Welche der drei Dimensionen ist 
# für die Kundenbindung völlig irrelevant?