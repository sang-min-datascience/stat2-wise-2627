# ==============================================================================
# Statistik 2: Logistische Regression
# Skript 2: Musterlösung & Screencast-Checklisten
# ==============================================================================

library(tidyverse)
library(broom)
# library(DEIN_PAKET_NAME) 

# ------------------------------------------------------------------------------
# Block 1: Das korrekte logistische Modell
# ------------------------------------------------------------------------------

# FEHLERBEHEBUNG: Ohne family = "binomial" rechnet glm() einfach ein normales 
# lineares OLS-Modell. 
modell_logit <- glm(hat_gekauft ~ verweildauer_min, 
                    data = shop_conversion, 
                    family = "binomial")

tidy(modell_logit)


# ------------------------------------------------------------------------------
# Block 2 & 3: Vorhersagen und nicht-konstante marginale Effekte
# ------------------------------------------------------------------------------

check_marginal <- data.frame(
  verweildauer_min = c(2, 3, 8, 9)
)

check_marginal$p <- predict(modell_logit, newdata = check_marginal, type = "response")
print(check_marginal)

# Interpretation der marginalen Effekte:
# - Der Sprung von Minute 2 auf 3 erhöht die Kaufwahrscheinlichkeit von ca. 36 % 
#   auf 40 %. Das ist ein marginaler Effekt von +4 Prozentpunkten.
# - Der Sprung von Minute 8 auf 9 erhöht die Wahrscheinlichkeit von ca. 65 % 
#   auf 69 %. Der marginale Effekt liegt ebenfalls bei ca. +4 Prozentpunkten.
# HINWEIS: Hier in der Mitte der S-Kurve sind die Effekte am stärksten. An den 
# extremen Rändern (z.B. bei 1 oder 25 Minuten) flacht die Kurve ab und der 
# marginale Effekt nähert sich 0.

# --- SCREENCAST-CHECKLISTE: MARGINALE EFFEKTE ---------------------------------
# [ ] Erklären Sie, warum Sie die Koeffizienten (Estimate) aus der tidy-Tabelle 
#     nicht direkt ablesen, sondern die predict() Funktion verwenden.
# [ ] Demonstrieren Sie an zwei unterschiedlichen Zeitpunkten, dass eine weitere 
#     Minute NICHT immer exakt den gleichen Wahrscheinlichkeitszuwachs bringt 
#     (Die S-Kurven-Logik).
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Block 4: Interaktionen (Der Design-Faktor)
# ------------------------------------------------------------------------------

modell_interaktion <- glm(hat_gekauft ~ verweildauer_min * gruppe, 
                          data = shop_conversion, 
                          family = "binomial")

# Wir visualisieren die Vorhersagen für das gesamte Spektrum, um die 
# Interaktion für das Management sofort greifbar zu machen.

# 1. Datenbasis für den Plot schaffen (alle Zeiten von 1 bis 15 für beide Gruppen)
plot_daten <- expand_grid(
  verweildauer_min = seq(1, 15, by = 0.5),
  gruppe = c("A_Alt", "B_Neu")
)

# 2. Wahrscheinlichkeiten vorhersagen
plot_daten$kauf_wahrscheinlichkeit <- predict(modell_interaktion, 
                                              newdata = plot_daten, 
                                              type = "response")

# 3. Den Interaktionseffekt plotten
ggplot(plot_daten, aes(x = verweildauer_min, y = kauf_wahrscheinlichkeit, color = gruppe)) +
  geom_line(linewidth = 1.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  labs(title = "Kaufwahrscheinlichkeit: Interaktion zwischen Zeit und Design",
       x = "Verweildauer (Minuten)",
       y = "Vorhergesagte Conversion-Rate",
       color = "Shop-Design")

# --- SCREENCAST-CHECKLISTE: INTERAKTIONEN -------------------------------------
# [ ] Erklären Sie den Plot: Was passiert in Design A, wenn Nutzer länger bleiben? 
#     Was in Design B?
# [ ] Formulieren Sie eine knallharte Handlungsempfehlung für das Management 
#     auf Basis der unterschiedlichen marginalen Effekte der beiden Gruppen.
# ------------------------------------------------------------------------------