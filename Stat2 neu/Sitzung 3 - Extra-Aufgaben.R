# ==============================================================================
# Statistik 2: Lineare Regression (OLS)
# Skript 3: Extra-Challenge - Die Multikollinearitäts-Falle
# ==============================================================================

library(tidyverse)
library(broom)
library(car) # Neues Paket für den VIF-Test (Variance Inflation Factor)
# install.packages("car") # Falls noch nicht installiert

# Setzen Sie den Datensatz aus dem Hauptskript voraus.
# Wir simulieren nun, dass das Marketing-Team Ihnen eine zusätzliche Variable 
# geliefert hat: Die Anzahl der Klicks auf der Webseite.
# Logischerweise gilt: Wer länger bleibt, klickt auch öfter.

set.seed(42)
festival_falle <- festival_umsatz %>% 
  mutate(
    # Wir erzeugen eine Variable, die extrem stark mit der Verweildauer zusammenhängt
    klicks = verweildauer_min * 4.2 + rnorm(n(), mean = 0, sd = 2)
  )

# ------------------------------------------------------------------------------
# Teil 1: Das Modell crasht (ohne Fehlermeldung)
# ------------------------------------------------------------------------------

# Wir schätzen ein Modell nur für das alte Design A, um es simpel zu halten.
daten_design_A <- festival_falle %>% filter(gruppe == "A_Alt")

# Aufgabe 1: Schätzen Sie ein simples Modell: Ausgaben ~ Verweildauer
modell_sauber <- lm(ausgaben_euro ~ verweildauer_min, data = daten_design_A)
tidy(modell_sauber)
# Notieren Sie sich den p-Wert und den Koeffizienten für die Verweildauer. 
# (Er sollte hochsignifikant und positiv sein).

# Aufgabe 2: Das Marketing-Team will unbedingt die Klicks im Modell haben.
# Schätzen Sie: Ausgaben ~ Verweildauer + Klicks
modell_kaputt <- lm(ausgaben_euro ~ verweildauer_min + klicks, data = daten_design_A)
tidy(modell_kaputt)

# KI-Prompt zur Reflexion:
# "In meinem ersten R-Modell war die Verweildauer hochsignifikant positiv. 
# Nachdem ich die extrem stark korrelierende Variable 'Klicks' hinzugefügt habe, 
# ist der p-Wert der Verweildauer plötzlich explodiert und der Effekt oft 
# unsichtbar. Erkläre mir anhand des Begriffs 'Multikollinearität', warum die 
# OLS-Mathematik hier versagt."


# ------------------------------------------------------------------------------
# Teil 2: Der Detektiv-Beweis (VIF)
# ------------------------------------------------------------------------------

# Man kann diese Falle im Vorfeld aufdecken, indem man die Korrelation prüft:
cor(daten_design_A$verweildauer_min, daten_design_A$klicks)
# Ein Wert nahe 1 ist ein massives Warnsignal.

# Der professionelle Test nach der Modellierung ist der VIF 
# (Variance Inflation Factor) aus dem 'car' Paket.
vif(modell_kaputt)

# Faustregel aus der Praxis: 
# Ein VIF > 5 (manche sagen > 10) bedeutet kritische Multikollinearität.
# Die Lösung? Werfen Sie einen der beiden Prädiktoren aus dem Modell! 
# Beide messen im Grunde das exakt Gleiche ("Engagement auf der Seite").


# --- SCREENCAST-CHECKLISTE: EXTRA-CHALLENGE -----------------------------------
# Wenn Sie diese Challenge in Ihren Screencast einbauen, zeigen Sie:
# [ ] Den direkten Vorher-Nachher-Vergleich der summary()/tidy() Outputs: 
#     Wie zerstört die Aufnahme der 'Klicks' die Signifikanz der 'Verweildauer'?
# [ ] Eine Erklärung des Ceteris-Paribus-Problems: Warum ist es in der Realität 
#     unmöglich, die Verweildauer zu erhöhen, *während* man die Klicks 
#     konstant hält?
# [ ] Den VIF-Score als handfesten Beweis für das Problem.
# ------------------------------------------------------------------------------