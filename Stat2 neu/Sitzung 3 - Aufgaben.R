# ==============================================================================
# Statistik 2: Lineare Regression (OLS)
# Skript 1: Aufgaben & Scaffolding
# ==============================================================================

library(tidyverse)
library(broom)
# library(DEIN_PAKET_NAME) # Hier ent-kommentieren

# ------------------------------------------------------------------------------
# Block 1: Die einfache OLS-Regression (und Fehler-Management)
# ------------------------------------------------------------------------------

# Wir wollen vorhersagen, ob Nutzer mehr Geld ausgeben, je länger sie im Shop sind.
# ACHTUNG: Der Code für das Modell enthält einen typischen R-Anfängerfehler.
# Lassen Sie den Fehler krachen, lesen Sie die Konsole, fragen Sie die KI.

modell_einfach <- lm(ausgaben_euro , verweildauer_min, data = festival_umsatz) # <-- Fehler!

# Wenn repariert, betrachten Sie die Ergebnisse:
tidy(modell_einfach)

# Notieren Sie hier Ihre Übersetzung ins Deutsche:
# Intercept (b0): "Wenn ein Nutzer 0 Minuten auf der Seite ist, erwarten wir..."
# Steigung (b1): "Für jede zusätzliche Minute auf der Seite, ..."


# ------------------------------------------------------------------------------
# Block 2: Multiple Regression (Ceteris Paribus)
# ------------------------------------------------------------------------------

# Erweitern Sie das Modell um das Shop-Design (Variable 'gruppe'). 
# Nutzen Sie ein '+' Zeichen.
modell_mult <- lm(_______ ~ _______ + _______, data = festival_umsatz)
tidy(modell_mult)

# Frage an Ihren Sitznachbarn: Wie hoch ist der durchschnittliche Umsatzunterschied 
# zwischen Design A und Design B, *wenn man die Verweildauer konstant hält*?


# ------------------------------------------------------------------------------
# Block 3: Interaktionseffekte (Der Bruch von Ceteris Paribus)
# ------------------------------------------------------------------------------

# Vielleicht ist langes Verweilen in Design A gut (Nutzer stöbern), aber in 
# Design B schlecht (Nutzer finden den Kaufen-Button nicht). 
# Bauen Sie eine Interaktion ein (nutzen Sie das '*' Zeichen).

modell_int <- lm(_______ ~ _______ * _______, data = festival_umsatz)
tidy(modell_int)

# Die harte Nuss (Partielle Ableitung):
# Wie hoch ist der marginale Effekt der Verweildauer...
# a) ... für Nutzer in Gruppe A_Alt?  Antwort: _______ Euro pro Minute.
# b) ... für Nutzer in Gruppe B_Neu?  Antwort: _______ Euro pro Minute.


# ------------------------------------------------------------------------------
# Block 4: Diagnostik (LUNG-Bedingungen)
# ------------------------------------------------------------------------------

# Wir extrahieren die Residuen (Vorhersagefehler) unseres Interaktionsmodells.
resid_daten <- augment(modell_int)

# 1. Bedingung N (Normalverteilung): Plotten Sie ein Histogramm der '.resid'
ggplot(resid_daten, aes(x = _______)) +
  geom_histogram()

# 2. Bedingung G (Gleiche Varianz): Plotten Sie '.fitted' (x-Achse) gegen '.resid' (y-Achse)
ggplot(resid_daten, aes(x = _______, y = _______)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red")