# ==============================================================================
# Statistik 2: Angewandte Inferenz (A/B-Testing)
# Skript 1: Aufgaben & Scaffolding
# ==============================================================================

# Setup
library(tidyverse)
library(DEIN_PAKET_NAME) 

# Datensatz 'shop_daten' ist durch das Paket geladen.


# ------------------------------------------------------------------------------
# Block 1: Datenaufbereitung und Fehlersuche
# ------------------------------------------------------------------------------

# ACHTUNG: Wir wollen für den Umsatz-Test später nur die Leute betrachten, die 
# tatsächlich etwas gekauft haben. Im folgenden Code ist ein Logik-Fehler. 
# Lassen Sie ihn von der KI finden und korrigieren!

kaeufer_only <- shop_daten %>% 
  filter(ausgaben_euro == 0) # <-- Hier ist der Fehler!

# ------------------------------------------------------------------------------
# Block 2: Diagnostik & A/B-Test für Anteile (prop.test)
# ------------------------------------------------------------------------------

# Frage: Hat das neue Design (B) signifikant mehr Ticketkäufe generiert als das alte (A)?

# 1. Diagnostik: Erfüllen wir die Erfolgs-/Misserfolgs-Bedingung (> 10 je Gruppe)?
tabelle_anteile <- table(shop_daten$gruppe, shop_daten$hat_gekauft)
print(tabelle_anteile)

# 2. Testdurchführung: Extrahieren Sie die Erfolge und Gruppengrößen aus der Tabelle
# Erfolge (hat_gekauft = 1) für A und B
erfolge <- c(_______, _______) 
# Gesamtzahl (n) für A und B
versuche <- c(400, 400) 

# Führen Sie den Test durch
ergebnis_prop <- prop.test(x = _______, n = _______)
print(ergebnis_prop)

# KI-Prompt für Ihr Verständnis:
# "Erkläre mir den p-Wert und das Konfidenzintervall dieses R-Outputs so, 
# dass ich als Eventmanager entscheiden kann, welches Design online gehen soll."


# ------------------------------------------------------------------------------
# Block 3: A/B-Test für Mittelwerte (t.test)
# ------------------------------------------------------------------------------

# Frage: Geben Käufer im neuen Design (B) im Durchschnitt signifikant mehr Geld aus?
# WICHTIG: Nutzen Sie hierfür den bereinigten Datensatz 'kaeufer_only'!

# 1. Diagnostik: Boxplot erstellen, um Ausreißer und Überschneidungen zu prüfen.
ggplot(_______, aes(x = _______, y = _______, fill = _______)) +
  geom_boxplot() +
  theme_minimal()

# 2. Testdurchführung: Nutzen Sie die Tilden-Schreibweise (Y ~ X)
ergebnis_t <- t.test(_______ ~ _______, data = _______)
print(ergebnis_t)