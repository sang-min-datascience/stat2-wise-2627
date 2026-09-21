# ==============================================================================
# Statistik 2: Logistische Regression
# Skript 1: Aufgaben & Scaffolding
# ==============================================================================

library(tidyverse)
library(broom)
# library(DEIN_PAKET_NAME) 

# ------------------------------------------------------------------------------
# Block 1: Der Crash der linearen Regression
# ------------------------------------------------------------------------------

# Wir wollen vorhersagen, ob jemand kauft (1) oder nicht (0), basierend auf der Zeit.
# Aufgabe 1: Schätzen Sie das Modell mit der normalen lm() Funktion.
modell_falsch <- lm(_______ ~ _______, data = shop_conversion)

# Aufgabe 2: Schätzen Sie das korrekte logistische Modell. 
# ACHTUNG: Hier fehlt ein entscheidendes Argument, R wird meckern oder falsch rechnen.
# Fixen Sie es mithilfe der KI.
modell_logit <- glm(hat_gekauft ~ verweildauer_min, data = shop_conversion) # <-- Fehler!

tidy(modell_logit)
# Schauen Sie auf den p-Wert. Ist die Verweildauer signifikant?


# ------------------------------------------------------------------------------
# Block 2: Profile bauen & Wahrscheinlichkeiten vorhersagen
# ------------------------------------------------------------------------------

# Die Estimates (Log-Odds) im tidy-Output sind schwer greifbar. 
# Wir wollen harte Wahrscheinlichkeiten in Prozent!
# Wir bauen drei fiktive Nutzer: Einen schnellen, einen mittleren, einen langsamen.

fiktive_user <- data.frame(
  verweildauer_min = c(3, 8, 13)
)

# Nutzen Sie die predict() Funktion. Das Argument type = "response" ist zwingend, 
# um Wahrscheinlichkeiten (0 bis 1) auszugeben!
fiktive_user$vorhersage_p <- predict(_______, newdata = _______, type = "response")
print(fiktive_user)


# ------------------------------------------------------------------------------
# Block 3: Der nicht-konstante marginale Effekt
# ------------------------------------------------------------------------------

# In einer linearen Regression ist der Effekt einer Minute immer gleich. 
# Gilt das hier auch? Berechnen Sie die Vorhersagen für:
check_marginal <- data.frame(
  verweildauer_min = c(2, 3,    8, 9)
)

check_marginal$p <- predict(modell_logit, newdata = check_marginal, type = "response")
print(check_marginal)

# Aufgabe: Rechnen Sie (im Kopf oder per Code): 
# Wie viel % bringt der Sprung von Minute 2 auf 3?
# Wie viel % bringt der Sprung von Minute 8 auf 9? 
# Warum sind diese Zahlen unterschiedlich?


# ------------------------------------------------------------------------------
# Block 4: Interaktionen (Der Design-Faktor)
# ------------------------------------------------------------------------------

# Das Management glaubt, dass langes Verweilen in Design A Frust bedeutet (man 
# findet den Button nicht), in Design B aber Interesse. 
# Modellieren Sie eine Interaktion zwischen Zeit und Gruppe!

modell_interaktion <- glm(_______ ~ _______ * _______, 
                          data = shop_conversion, 
                          family = "binomial")
tidy(modell_interaktion)

# Wir bauen Profile für beide Gruppen bei 12 Minuten Verweildauer:
profil_12_min <- data.frame(
  verweildauer_min = c(12, 12),
  gruppe = c("A_Alt", "B_Neu")
)

# Vorhersage generieren:
predict(_______, newdata = _______, type = "response")

# Screencast-Vorbereitung: Erklären Sie Ihrem Nachbarn, warum Design A sofort 
# abgeschaltet werden sollte.