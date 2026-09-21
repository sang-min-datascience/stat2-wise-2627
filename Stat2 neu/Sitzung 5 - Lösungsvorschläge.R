# ==============================================================================
# Statistik 2: Modellselektion & Kreuzvalidierung
# Skript 2: Musterlösung & Screencast-Checklisten
# ==============================================================================

library(tidyverse)
library(broom)
library(caret) 
# library(DEIN_PAKET_NAME) 

# ------------------------------------------------------------------------------
# Block 1 & 2: Overfitting und Metriken (adj. R-Squared & AIC)
# ------------------------------------------------------------------------------

modell_logisch <- lm(gastro_ausgaben_euro ~ temperatur_c + verweildauer_h, data = festival_gastro)
modell_muell <- lm(gastro_ausgaben_euro ~ ., data = festival_gastro %>% select(-besucher_id))

glance(modell_logisch) |> select(r.squared, adj.r.squared, AIC)
# r.squared: ~ 0.505 | adj.r.squared: ~ 0.503 | AIC: ~ 3726

glance(modell_muell) |> select(r.squared, adj.r.squared, AIC)
# r.squared: ~ 0.506 | adj.r.squared: ~ 0.502 | AIC: ~ 3729

# Erkenntnis: Das reguläre R-Squared ist beim Müll-Modell leicht gestiegen 
# (Overfitting-Illusion). Das Adjusted R-Squared ist jedoch gesunken und der 
# AIC-Wert ist gestiegen. Beide Metriken bestrafen die nutzlosen Variablen 
# (Schuhgröße etc.) korrekt. Das logische Modell ist besser!

# --- SCREENCAST-CHECKLISTE: METRIKEN ------------------------------------------
# [ ] Erklären Sie klar den Unterschied zwischen regulärem und angepasstem R2.
# [ ] Demonstrieren Sie anhand Ihrer glance()-Ausgabe, warum das Müll-Modell 
#     mathematisch bestraft wurde.
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Block 3: Kreuzvalidierung (Cross-Validation)
# ------------------------------------------------------------------------------

set.seed(42)
folds <- createFolds(festival_gastro$gastro_ausgaben_euro, k = 4)

# FEHLERBEHEBUNG: Die predict() Funktion muss zwingend auf 'test_daten' 
# angewendet werden. Wenn wir sie auf 'train_daten' anwenden, messen wir nur, 
# wie gut das Modell auswendig gelernt hat, nicht wie gut es vorhersagt!

# --- Loop für das logische Modell ---
cv_ergebnisse_logisch <- data.frame()
for(i in 1:4) {
  test_indizes <- folds[[i]]
  test_daten  <- festival_gastro[test_indizes, ]
  train_daten <- festival_gastro[-test_indizes, ] 
  
  modell_cv <- lm(gastro_ausgaben_euro ~ temperatur_c + verweildauer_h, data = train_daten)
  vorhersage <- predict(modell_cv, newdata = test_daten) # Korrigiert!
  
  fold_ergebnis <- data.frame(
    Fold = paste("Fold", i),
    Beobachtet = test_daten$gastro_ausgaben_euro,
    Vorhergesagt = vorhersage,
    Fehler = test_daten$gastro_ausgaben_euro - vorhersage
  )
  cv_ergebnisse_logisch <- bind_rows(cv_ergebnisse_logisch, fold_ergebnis)
}

sse_logisch <- sum(cv_ergebnisse_logisch$Fehler^2)
print(paste("CV Gesamtfehler (Logisch):", round(sse_logisch, 0)))


# ------------------------------------------------------------------------------
# Block 4: Vorhersagefehler Visualisieren & Interpretieren
# ------------------------------------------------------------------------------

# Visualisierung der Stabilität über alle 4 Folds
ggplot(cv_ergebnisse_logisch, aes(x = Vorhergesagt, y = Fehler)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  facet_wrap(~Fold) +
  theme_minimal() +
  labs(title = "Out-of-Sample Fehler: Das Modell ist stabil",
       x = "Vorhergesagter Gastro-Umsatz (Euro)",
       y = "Vorhersagefehler (Euro)")

# --- SCREENCAST-CHECKLISTE: CROSS-VALIDATION ----------------------------------
# [ ] Definieren Sie kurz die Begriffe "Trainingsdaten" und "Testdaten".
# [ ] Erklären Sie, warum das Reparieren des 'newdata'-Arguments in der 
#     predict() Funktion absolut entscheidend war.
# [ ] Zeigen Sie den Gesamtfehler (SSE) beider Modelle und ziehen Sie ein 
#     Geschäfts-Fazit (Welches Modell deployen wir?).
# ------------------------------------------------------------------------------