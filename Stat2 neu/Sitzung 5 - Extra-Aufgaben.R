# ==============================================================================
# Statistik 2: Modellselektion & Kreuzvalidierung
# Skript 3: Extra-Challenge - Profi-Workflow mit 'caret'
# ==============================================================================

library(tidyverse)
library(caret)

# Sie haben gerade mühsam eine 30-zeilige for-Schleife geschrieben, Listen 
# indiziert und Dataframes manuell zusammengeklebt. Das war wichtig für das 
# Verständnis. 
# 
# In der echten Arbeitswelt automatisieren Data Scientists diesen Prozess. 
# Das 'caret' Paket (Classification And REgression Training) kann das gesamte 
# Training, das Folds-Splitting und die Evaluation in ZWEI Zeilen erledigen.

set.seed(42)

# 1. Wir definieren, WIE trainiert werden soll (Hier: 4-fache Cross-Validation)
train_kontrolle <- trainControl(method = "cv", number = 4)

# 2. Wir trainieren das Modell mit der Funktion train() statt lm()
automatisches_modell <- train(
  gastro_ausgaben_euro ~ temperatur_c + verweildauer_h, 
  data = festival_gastro, 
  method = "lm",           # Wir nutzen OLS-Regression
  trControl = train_kontrolle
)

# Schauen Sie sich das Ergebnis an:
print(automatisches_modell)

# Die Ausgabe zeigt Ihnen direkt den "RMSE" (Root Mean Squared Error) über die 
# Kreuzvalidierung gemittelt. 

# KI-Prompt zur Reflexion:
# "Mein caret train() Output in R zeigt mir für meine 4-fache Kreuzvalidierung 
# Metriken wie RMSE, Rsquared und MAE an. Erkläre mir, was RMSE und MAE im 
# Kontext meiner Umsatzvorhersage in Euro bedeuten und welche Metrik für das 
# Management verständlicher ist."


# --- SCREENCAST-CHECKLISTE: EXTRA-CHALLENGE -----------------------------------
# Wenn Sie diesen Workflow im Screencast präsentieren:
# [ ] Zeigen Sie den Kontrast: Wie viel Code spart die train() Funktion im 
#     Vergleich zur manuellen for-Schleife?
# [ ] Erklären Sie die Metrik MAE (Mean Absolute Error) anhand der KI-Antwort: 
#     "Im Durchschnitt irrt sich unser Modell bei neuen Daten um X Euro."
# ------------------------------------------------------------------------------