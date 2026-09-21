# ==============================================================================
# Statistik 2: Modellselektion & Kreuzvalidierung
# Skript 1: Aufgaben & Scaffolding
# ==============================================================================

library(tidyverse)
library(broom)
library(caret) # Wichtig für die Folds!
# library(DEIN_PAKET_NAME) 

# ------------------------------------------------------------------------------
# Block 1: Die Overfitting-Falle
# ------------------------------------------------------------------------------

# Wir schätzen zwei Modelle für den Gastro-Umsatz.
# 1. Das logische Modell: Umsatz hängt von Temperatur und Verweildauer ab.
modell_logisch <- lm(gastro_ausgaben_euro ~ temperatur_c + verweildauer_h, data = festival_gastro)

# 2. Das Müll-Modell: Wir werfen alles rein (inkl. Schuhgröße!).
# Tipp: Ein '.' nach der Tilde bedeutet "Nimm alle verbleibenden Variablen".
modell_muell <- lm(gastro_ausgaben_euro ~ ., data = festival_gastro %>% select(-besucher_id))

# Aufgabe: Vergleichen Sie die R-Squared Metriken beider Modelle mit glance()
glance(_______) |> select(r.squared, adj.r.squared, AIC)
glance(_______) |> select(r.squared, adj.r.squared, AIC)

# Was fällt Ihnen auf? Welcher Wert ist beim Müll-Modell gestiegen, welcher gesunken?


# ------------------------------------------------------------------------------
# Block 3: Kreuzvalidierung (Cross-Validation)
# ------------------------------------------------------------------------------

# Das Adjusted R-Squared ist nur Theorie. Wir testen nun die harte Praxis.
set.seed(42)

# 1. Daten in 4 Folds aufteilen
folds <- createFolds(festival_gastro$gastro_ausgaben_euro, k = 4)

# 2. Leerer Container
cv_ergebnisse_muell <- data.frame()

# 3. Der Loop
for(i in 1:4) {
  test_indizes <- folds[[i]]
  
  test_daten  <- festival_gastro[test_indizes, ]
  train_daten <- festival_gastro[-test_indizes, ] 
  
  # Modell auf Trainingsdaten anpassen
  modell_cv <- lm(gastro_ausgaben_euro ~ ., data = train_daten %>% select(-besucher_id))
  
  # ACHTUNG! Hier hat sich ein logischer Fehler in den Code eingeschlichen.
  # Wir wollen überprüfen, wie gut das Modell auf NEUEN Daten performt.
  # Reparieren Sie das Argument 'newdata'.
  vorhersage <- predict(modell_cv, newdata = train_daten) # <-- FEHLER!
  
  # Fehler berechnen und speichern
  fold_ergebnis <- data.frame(
    Fold = paste("Fold", i),
    Beobachtet = test_daten$gastro_ausgaben_euro,
    Vorhergesagt = vorhersage,
    Fehler = test_daten$gastro_ausgaben_euro - vorhersage
  )
  
  cv_ergebnisse_muell <- bind_rows(cv_ergebnisse_muell, fold_ergebnis)
}

# 4. Den Gesamtfehler berechnen (Summe der Fehlerquadrate)
sum(cv_ergebnisse_muell$Fehler^2)

# Aufgabe: Kopieren Sie den reparierten Loop, führen Sie ihn noch einmal für 
# das 'modell_logisch' aus und vergleichen Sie den Gesamtfehler!