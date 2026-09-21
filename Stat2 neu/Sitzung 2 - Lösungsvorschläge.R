# ==============================================================================
# Statistik 2: Angewandte Inferenz (A/B-Testing)
# Skript 2: Musterlösung & Screencast-Checklisten
# ==============================================================================

library(tidyverse)
# library(DEIN_PAKET_NAME) 


# ------------------------------------------------------------------------------
# Block 1: Datenaufbereitung
# ------------------------------------------------------------------------------

# Korrektur: Wir wollen nur User behalten, deren Ausgaben GRÖSSER als 0 sind.
kaeufer_only <- shop_daten %>% 
  filter(ausgaben_euro > 0) 


# ------------------------------------------------------------------------------
# Block 2: Diagnostik & A/B-Test für Anteile (prop.test)
# ------------------------------------------------------------------------------

tabelle_anteile <- table(shop_daten$gruppe, shop_daten$hat_gekauft)
# Ergebnis zeigt: Gruppe A hat 45 Erfolge, Gruppe B hat 68 Erfolge. 
# Beide Werte sind > 10. Die Erfolgs-/Misserfolgs-Bedingung ist erfüllt!

# Testdurchführung
erfolge <- c(45, 68) 
versuche <- c(400, 400) 

prop.test(x = erfolge, n = versuche)

# --- SCREENCAST-CHECKLISTE: ANTEILE -------------------------------------------
# Ein erfolgreicher Screencast zu diesem Test enthält:
# [ ] Den Nachweis, dass die Erfolgs-/Misserfolgs-Bedingung erfüllt ist.
# [ ] Die korrekte Interpretation des p-Werts (Ist der Unterschied von 11,2 % 
#     zu 17,0 % statistisch signifikant?).
# [ ] Eine geschäftliche Empfehlung auf Basis des Konfidenzintervalls.
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Block 3: A/B-Test für Mittelwerte (t.test)
# ------------------------------------------------------------------------------

# Diagnostik via Boxplot
ggplot(kaeufer_only, aes(x = gruppe, y = ausgaben_euro, fill = gruppe)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  theme_minimal() +
  labs(title = "Umsatz pro Käufer nach Design-Gruppe",
       y = "Ausgaben in Euro", x = "Design-Gruppe")
# Interpretation: Die Boxen überschneiden sich stark (hohes Rauschen), es gibt
# aber keine extremen Ausreißer, die den t-Test verzerren würden.

# Testdurchführung
# R verwendet standardmäßig den Welch-t-Test für unabhängige Stichproben.
t.test(ausgaben_euro ~ gruppe, data = kaeufer_only)

# --- SCREENCAST-CHECKLISTE: MITTELWERTE ---------------------------------------
# Ein erfolgreicher Screencast zu diesem Test enthält:
# [ ] Die Begründung, warum für diese Daten ein t-Test (Mittelwerte) und 
#     kein prop.test (Anteile) genutzt wird.
# [ ] Einen visuellen Check auf Ausreißer mittels Boxplot vor dem Test.
# [ ] Die korrekte Lesart des Welch-t-Test Outputs (p-Wert und Konfidenzintervall)
#     mit einer klaren Aussage, ob das Rauschen das Signal übertönt.
# ------------------------------------------------------------------------------