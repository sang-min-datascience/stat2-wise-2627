# ==============================================================================
# Statistik 2: Grundlagen der Inferenz mit Stichproben
# Skript 1: Aufgaben & Scaffolding
# ==============================================================================

# Willkommen im Skript! 
# Erinnerung: RStudio ist kein Taschenrechner. Nutzen Sie dieses Skript, um Ihre 
# Gedanken, Ihren Code und die Erklärungen der KI an einem Ort zu speichern.
# Ziel heute: Vorbereitung auf Ihren Screencast. Sie müssen den Code am Ende 
# verbal erklären können.

# ------------------------------------------------------------------------------
# Block 1: Setup und Frust-Management (Fehlersuche)
# ------------------------------------------------------------------------------

# 1. Paket-Installation (Nur einmalig ausführen!)
# Entfernen Sie die Rauten (#) in den nächsten zwei Zeilen, falls Sie das 
# Kurspaket noch nicht installiert haben. Danach wieder mit # auskommentieren.
# install.packages("pak")
# pak::pkg_install("DEIN_GITHUB_NAME/DEIN_PAKET_NAME") 

# 2. Pakete laden
library(tidyverse)
library(DEIN_PAKET_NAME) 

# Der Datensatz 'population_events' ist durch das Paket nun direkt verfügbar!
# Sie können ihn sich im Environment (oben rechts) ansehen.

# ACHTUNG: Im folgenden Code hat sich ein Fehler eingeschlichen. 
# Aufgabe: Führen Sie den Code aus, kopieren Sie die rote Fehlermeldung aus der 
# Konsole, füttern Sie damit eine KI (z.B. ChatGPT/Copilot) und reparieren Sie den Code.

gefilterte_events <- population_events %>% 
  filter(besucher > 50, aufbauzeit > 2) # <-- Hier ist der Fehler!

# ------------------------------------------------------------------------------
# Block 2: Das "Gott"-Spiel - Population vs. Stichprobe
# ------------------------------------------------------------------------------

# Wir spielen Gott: Berechnen Sie den *wahren* Mittelwert der Aufbauzeit für 
# alle Events in der Population.
wahres_mu <- mean(_______)

# Jetzt die Realität: Sie sind Eventmanager und haben nur Budget, um 50 Events 
# zu untersuchen. Ziehen Sie eine zufällige Stichprobe (n = 50) aus der Population.
# Tipp: Nutzen Sie die Funktion slice_sample()
meine_stichprobe <- population_events %>% 
  _______(n = 50)

# Berechnen Sie nun den Mittelwert Ihrer kleinen Stichprobe.
mein_x_bar <- _______

# Screencast-Training (KI-Prompt):
# "Erkläre mir den Unterschied zwischen 'wahres_mu' und 'mein_x_bar' in diesem 
# R-Code so, als müsste ich es in einem Video einem Fachfremden erklären."
# -> Notieren Sie die beste Erklärung hier als Kommentar:
#
#


# ------------------------------------------------------------------------------
# Block 3: Die Magie der Stichprobenverteilung
# ------------------------------------------------------------------------------

# Was passiert, wenn 1000 Eventmanager gleichzeitig jeweils 50 Events untersuchen?
# Wir simulieren das jetzt. Füllen Sie die Lücken aus, um 1000 Stichproben zu ziehen.

anzahl_simulationen <- 1000
mittelwerte_50 <- numeric(anzahl_simulationen) # Ein leerer Vektor für unsere Ergebnisse

for(i in 1:anzahl_simulationen) {
  
  # 1. Ziehe eine Stichprobe von n = 50
  temp_sample <- population_events %>% _______(n = 50)
  
  # 2. Berechne den Mittelwert und speichere ihn an der i-ten Stelle im Vektor
  mittelwerte_50[i] <- mean(_______)
  
}

# Plotten Sie nun ein Histogramm der 1000 Mittelwerte.
# Tipp: hist() reicht für einen schnellen Blick, ggplot() ist eleganter.
_______


# ------------------------------------------------------------------------------
# Block 4: Der Standardfehler in der Praxis
# ------------------------------------------------------------------------------

# Kopieren Sie Ihre for-Schleife aus Block 3 und fügen Sie sie hier unten ein.
# Ändern Sie EINE Sache: Jeder der 1000 Manager befragt nun n = 500 Events 
# (statt n = 50). Speichern Sie die Ergebnisse im Vektor `mittelwerte_500`.

mittelwerte_500 <- numeric(anzahl_simulationen)

# [Ihre Schleife hier]


# Plotten Sie das neue Histogramm.
_______


# Screencast-Generalprobe (Tandem-Arbeit):
# Betrachten Sie beide Histogramme.
# Erklären Sie Ihrem Partner (und nehmen Sie es ggf. auf): 
# 1. Was sehen wir in diesen Plots? (Was ist eine Stichprobenverteilung?)
# 2. Warum ist das zweite Histogramm schmaler? (Was ist der Standardfehler?)
# 3. Warum hilft das einem echten Eventmanager bei der Planung?