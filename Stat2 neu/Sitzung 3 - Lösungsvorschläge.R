# ==============================================================================
# Statistik 2: Lineare Regression (OLS)
# Skript 2: Musterlösung & Screencast-Checklisten
# ==============================================================================

library(tidyverse)
library(broom)
# library(DEIN_PAKET_NAME) 

# ------------------------------------------------------------------------------
# Block 1: Die einfache OLS-Regression
# ------------------------------------------------------------------------------

# FEHLERBEHEBUNG: In der Formel-Syntax von R MUSS eine Tilde (~) stehen, um das 
# Y vom X zu trennen. Ein Komma trennt Argumente in einer Funktion.
modell_einfach <- lm(ausgaben_euro ~ verweildauer_min, data = festival_umsatz)

tidy(modell_einfach)

# --- SCREENCAST-CHECKLISTE: EINFACHE REGRESSION -------------------------------
# [ ] Erklären Sie, warum der Intercept (b0) hier zwar mathematisch notwendig, 
#     aber inhaltlich oft unsinnig ist (niemand ist 0 Minuten im Shop und kauft).
# [ ] Interpretieren Sie den Koeffizienten der Verweildauer (b1) exakt: "Für 
#     jede zusätzliche Minute erwarten wir im Schnitt X Euro mehr Umsatz."
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Block 2: Multiple Regression (Ceteris Paribus)
# ------------------------------------------------------------------------------

modell_mult <- lm(ausgaben_euro ~ verweildauer_min + gruppe, data = festival_umsatz)
tidy(modell_mult)

# Interpretation: 
# Der Koeffizient 'gruppeB_Neu' zeigt den Ceteris-Paribus-Effekt. Nutzer in 
# Design B geben im Schnitt deutlich mehr Geld aus als in Design A – ABER NUR 
# unter der Annahme, dass beide Gruppen exakt gleich lang im Shop verweilen.


# ------------------------------------------------------------------------------
# Block 3: Interaktionseffekte (Partielle Ableitung)
# ------------------------------------------------------------------------------

modell_int <- lm(ausgaben_euro ~ verweildauer_min * gruppe, data = festival_umsatz)
tidy(modell_int)

# --- SCREENCAST-CHECKLISTE: INTERAKTIONEN (Die Königsdisziplin) ---------------
# Wenn Sie diesen Teil im Screencast behandeln, ist höchste Präzision gefragt:
# [ ] Weisen Sie auf den p-Wert der Interaktion hin (Zeile: verweildauer_min:gruppeB_Neu).
# [ ] Berechnen Sie den marginalen Effekt für Gruppe A (Der Haupteffekt von verweildauer_min).
# [ ] Berechnen Sie den marginalen Effekt für Gruppe B (Haupteffekt + Interaktionseffekt).
# [ ] Geschäftliches Fazit: Warum ist eine lange Verweildauer im alten Shop ein 
#     gutes Zeichen, im neuen Shop aber fast irrelevant?
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Block 4: Diagnostik (LUNG-Bedingungen) und Modellgüte
# ------------------------------------------------------------------------------

glance(modell_int) 
# Das R-Quadrat (r.squared) sagt uns, wie viel Prozent der Streuung der Ausgaben 
# durch unsere beiden Variablen erklärt wird.

resid_daten <- augment(modell_int)

# N - Normalität prüfen
ggplot(resid_daten, aes(x = .resid)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Normalverteilung der Residuen?", x = "Residuen (Fehler in Euro)")
# Fazit: Sieht ausreichend symmetrisch (glockenförmig) aus.

# G - Gleiche Varianz (Homoskedastizität) prüfen
ggplot(resid_daten, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Konstante Fehlestreuung?", 
       x = "Vorhergesagte Ausgaben", y = "Residuen")
# Fazit: Die Punkte streuen gleichmäßig um die Nulllinie herum, ohne ein 
# Trichter-Muster zu bilden. Bedingung G ist erfüllt.

# --- SCREENCAST-CHECKLISTE: DIAGNOSTIK ----------------------------------------
# [ ] Nennen Sie den R-Quadrat-Wert und bewerten Sie, ob das Modell den Großteil 
#     der Realität abbildet oder ob viele Variablen fehlen.
# [ ] Zeigen Sie mindestens einen der Residuen-Plots.
# [ ] Erklären Sie in einem Satz, was ein Residuum ist (Der Abstand zwischen dem 
#     wahren Datenpunkt und der Vorhersage des Modells).
# ------------------------------------------------------------------------------