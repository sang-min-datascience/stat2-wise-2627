# ==============================================================================
# Statistik 2: Grundlagen der Inferenz mit Stichproben
# Skript 2: Musterlösung & Screencast-Checklisten
# ==============================================================================

# Setup
library(tidyverse)
# library(DEIN_PAKET_NAME) # Sobald dein Paket online ist, hier ent-kommentieren

# ------------------------------------------------------------------------------
# Block 1: Setup und Frust-Management (Die Lösung)
# ------------------------------------------------------------------------------

# FEHLERBEHEBUNG: 
# Die Fehlermeldung lautete "object 'aufbauzeit' not found". 
# Ein Blick in das Environment oder der Befehl names(population_events) zeigt: 
# Die Spalte heißt korrekt 'aufbauzeit_h'.

# Weg A: Der moderne Tidyverse-Weg (Empfohlen für den Kurs)
gefilterte_events <- population_events %>% 
  filter(besucher > 50, aufbauzeit_h > 2) 

# Weg B: Die klassische Base-R Alternative (gut zu kennen, schwerer zu lesen)
# gefilterte_events_alt <- population_events[population_events$besucher > 50 & population_events$aufbauzeit_h > 2, ]


# ------------------------------------------------------------------------------
# Block 2: Das "Gott"-Spiel - Population vs. Stichprobe
# ------------------------------------------------------------------------------

# Die Gott-Perspektive: Der wahre Parameter (Mü) der Grundgesamtheit
wahres_mu <- mean(population_events$aufbauzeit_h)

# Die Realität: Eine einzige Stichprobe (n = 50)
meine_stichprobe <- population_events %>% 
  slice_sample(n = 50)

# Das Ergebnis der Stichprobe: Die Stichprobenstatistik (x-quer)
mein_x_bar <- mean(meine_stichprobe$aufbauzeit_h)

# --- SCREENCAST-CHECKLISTE ----------------------------------------------------
# Ein erfolgreicher Screencast zu diesem Block enthält:
# [ ] Eine klare Unterscheidung der Begriffe "Population" und "Stichprobe".
# [ ] Die Zuordnung, welcher Code-Teil den wahren Parameter und welcher die 
#     Stichprobenstatistik berechnet.
# [ ] Eine kurze Erklärung in eigenen Worten, warum `wahres_mu` und 
#     `mein_x_bar` fast nie exakt den gleichen Wert haben.
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Block 3: Die Magie der Stichprobenverteilung
# ------------------------------------------------------------------------------

anzahl_simulationen <- 1000

# Weg A: Die for-Schleife (Sehr explizit, zeigt Schritt-für-Schritt was passiert)
mittelwerte_50 <- numeric(anzahl_simulationen) 

for(i in 1:anzahl_simulationen) {
  temp_sample <- population_events %>% slice_sample(n = 50)
  mittelwerte_50[i] <- mean(temp_sample$aufbauzeit_h)
}

# Weg B: Die replicate() Funktion (Die elegante Base-R Alternative)
# Das macht exakt das Gleiche wie die Schleife, aber in einer Zeile Code!
# mittelwerte_50_alt <- replicate(anzahl_simulationen, {
#   mean(sample(population_events$aufbauzeit_h, 50))
# })

# Visualisierung
# Weg A: Quick & Dirty (hist)
# hist(mittelwerte_50, main = "Stichprobenverteilung (n=50)", xlab = "Mittlere Aufbauzeit")

# Weg B: Publikationsreif (ggplot2)
tibble(mittelwert = mittelwerte_50) %>% 
  ggplot(aes(x = mittelwert)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  geom_vline(xintercept = wahres_mu, color = "red", size = 1.5) +
  theme_minimal() +
  labs(title = "Stichprobenverteilung (n = 50)",
       x = "Mittlere Aufbauzeit (Stunden)",
       y = "Häufigkeit",
       subtitle = "Rote Linie = Wahres Populationsmittel")


# ------------------------------------------------------------------------------
# Block 4: Der Standardfehler in der Praxis
# ------------------------------------------------------------------------------

# Wir wiederholen die Simulation, aber diesmal mit n = 500 Events pro Stichprobe.
mittelwerte_500 <- numeric(anzahl_simulationen)

for(i in 1:anzahl_simulationen) {
  temp_sample <- population_events %>% slice_sample(n = 500)
  mittelwerte_500[i] <- mean(temp_sample$aufbauzeit_h)
}

# Plot für n = 500 (Achten Sie auf die x-Achse im Vergleich zum vorherigen Plot!)
tibble(mittelwert = mittelwerte_500) %>% 
  ggplot(aes(x = mittelwert)) +
  geom_histogram(fill = "darkgreen", color = "white", bins = 30) +
  geom_vline(xintercept = wahres_mu, color = "red", size = 1.5) +
  theme_minimal() +
  # Wir fixieren die x-Achse auf die gleichen Werte wie bei n=50 für einen fairen Vergleich
  coord_cartesian(xlim = c(12, 16)) + 
  labs(title = "Stichprobenverteilung (n = 500)",
       x = "Mittlere Aufbauzeit (Stunden)",
       y = "Häufigkeit")

# --- SCREENCAST-CHECKLISTE ----------------------------------------------------
# Ein erfolgreicher Screencast zu diesem Block enthält:
# [ ] Die korrekte Benennung dessen, was in den Histogrammen zu sehen ist 
#     (Stichprobenverteilung, NICHT die Rohdaten).
# [ ] Eine Erklärung des Begriffs "Standardfehler" anhand der visuellen 
#     Streuung der Histogramme.
# [ ] Die Schlussfolgerung für die Event-Praxis: Warum bietet ein größeres 'n' 
#     mehr Planungssicherheit?
# ------------------------------------------------------------------------------