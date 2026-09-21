# ==============================================================================
# Statistik 2: Fragebögen und Latente Konstrukte
# Skript 3: Extra-Challenge - Reliabilität prüfen (Cronbachs Alpha)
# ==============================================================================

library(tidyverse)
library(psych)

# ------------------------------------------------------------------------------
# Teil 1: Vertrauen ist gut, Kontrolle ist besser
# ------------------------------------------------------------------------------
# Die Faktoranalyse (EFA) hat uns gezeigt, dass z. B. die Fragen Q01 bis Q04 
# mathematisch einen Faktor ("Atmosphäre") bilden. 
#
# Bevor wir daraus aber einen offiziellen Management-KPI machen, prüfen wir 
# die "Reliabilität" (Interne Konsistenz). Wir wollen wissen: Messen diese 
# vier Fragen wirklich alle dasselbe abstrakte Konzept?
# 
# Das Standardmaß dafür ist "Cronbachs Alpha".
# Faustregel der Empirie:
# < 0.6 = Inakzeptabel (Die Fragen streuen zu wild)
# 0.7 bis 0.8 = Akzeptabel bis gut
# > 0.8 = Sehr gut (Hohe interne Konsistenz)

# 1. Wir isolieren nur die Fragen, die zu unserem Faktor "Atmosphäre" gehören:
items_atmos <- festival_umfrage %>% 
  select(Q01_Musik, Q02_Licht, Q03_Stimmung, Q04_Sound)

# 2. Wir berechnen Cronbachs Alpha
reliabilitaet_atmos <- alpha(items_atmos)

# Schauen Sie sich den gigantischen Output an:
print(reliabilitaet_atmos)


# ------------------------------------------------------------------------------
# Teil 2: Wie man den Output liest (Die "Dropped"-Analyse)
# ------------------------------------------------------------------------------
# Der Output erschlägt einen, aber für die Praxis sind nur zwei Tabellen relevant:
#
# 1. Ganz oben: 'raw_alpha'. 
#    Das ist das Gesamt-Alpha für diese 4 Fragen. Liegt es über 0.7?
#
# 2. Die Tabelle 'Reliability if an item is dropped':
#    Das ist das mächtigste Feature. Es zeigt Ihnen, wie sich das Gesamt-Alpha 
#    verändern würde, wenn Sie eine spezifische Frage aus dem Fragebogen werfen.
#    -> Wenn das Alpha plötzlich drastisch ansteigt, sobald Sie z. B. "Q02_Licht" 
#       löschen, bedeutet das: Diese Frage war ein Störfaktor! Sie passt 
#       inhaltlich nicht wirklich zu den anderen drei.

# KI-Prompt zur Reflexion:
# "Ich habe in R Cronbachs Alpha für vier Umfrage-Items berechnet. In der Tabelle 
# 'Reliability if an item is dropped' sehe ich, dass das Alpha steigen würde, 
# wenn ich Frage 2 lösche. Erkläre mir als Data Scientist, ob ich Frage 2 einfach 
# blind löschen sollte, oder ob es einen Trade-off zwischen statistischer 
# Perfektion und inhaltlicher Breite des Fragebogens gibt."


# --- SCREENCAST-CHECKLISTE: EXTRA-CHALLENGE -----------------------------------
# Wenn Sie diese professionelle Qualitätskontrolle im Screencast einbauen:
# [ ] Erklären Sie in einem einfachen Satz, was Cronbachs Alpha misst 
#     (Interne Konsistenz / Zuverlässigkeit der Skala).
# [ ] Nennen Sie das berechnete `raw_alpha` und fällen Sie ein klares Urteil, 
#     ob der von Ihnen benannte Faktor verlässlich gemessen wurde.
# [ ] Bewerten Sie anhand der "dropped"-Tabelle, ob eine der vier Fragen das 
#     Konstrukt schwächt und im nächsten Jahr aus der Umfrage fliegen sollte.
# ------------------------------------------------------------------------------