# ==============================================================================
# Statistik 2: Angewandte Inferenz (A/B-Testing)
# Skript 3: Extra-Challenge - Power Analyse
# ==============================================================================

# Willkommen in der Extra-Challenge! 
# Sie haben in den vorherigen Skripten gelernt, Daten im Nachhinein zu 
# analysieren. In der Realität müssen Sie aber oft VOR einem A/B-Test wissen: 
# Wie viele Beobachtungen brauche ich überhaupt, damit der Test aussagekräftig ist?

# ------------------------------------------------------------------------------
# Teil 1: Die Theorie (Fehler 2. Art)
# ------------------------------------------------------------------------------
# Ein Hypothesentest ist nie perfekt.
# - Fehler 1. Art (Alpha): Wir behaupten, ein neues Design ist besser, obwohl 
#   es in Wahrheit keinen Unterschied gibt. (Standard: 5 % Risiko).
# - Fehler 2. Art (Beta): Das neue Design ist in Wahrheit besser, aber unser 
#   Test erkennt es nicht (z. B. weil unsere Stichprobe zu klein war).
# 
# Die "Power" (Teststärke) ist 1 minus Beta. Eine Power von 80 % bedeutet: 
# Wenn es einen echten Effekt gibt, haben wir eine 80%ige Chance, diesen 
# in unserem Test auch signifikant nachzuweisen.

# ------------------------------------------------------------------------------
# Teil 2: A-Priori Power Analyse für Anteile (prop.test)
# ------------------------------------------------------------------------------

# Szenario: Sie wollen nächstes Jahr ein neues "VIP-Upgrade"-Popup im Ticketshop 
# testen. Das alte Popup hatte eine Conversion-Rate von 5 % (p1 = 0.05). 
# Damit sich die Entwicklungskosten lohnen, muss das neue Popup mindestens 
# 8 % Conversion erreichen (p2 = 0.08). 
# Sie wollen eine Power von 80 % (power = 0.80) bei einem Signifikanzniveau 
# von 5 % (sig.level = 0.05).

# Aufgabe: Wie groß muss die Stichprobe (n) PRO GRUPPE sein?
# Tipp: Übergeben Sie den gesuchten Wert als `NULL`, damit R ihn berechnet.

ergebnis_power_prop <- power.prop.test(
  p1 = 0.05, 
  p2 = 0.08, 
  power = 0.80, 
  sig.level = 0.05, 
  n = NULL 
)

print(ergebnis_power_prop)

# Die Ausgabe zeigt Ihnen 'n'. Da es halbe Menschen nicht gibt, runden 
# Sie diesen Wert für die echte Planung immer auf die nächste ganze Zahl auf!

# ------------------------------------------------------------------------------
# Teil 3: Was passiert bei zu wenig Budget? (t.test)
# ------------------------------------------------------------------------------

# Szenario: Sie testen zwei Catering-Anbieter. Sie wollen wissen, ob Anbieter B 
# im Schnitt schneller arbeitet (Aufbauzeit). 
# Sie haben nur Budget, um beide Anbieter auf jeweils 20 Events zu testen (n = 20).
# Die Standardabweichung (sd) der Aufbauzeit liegt erfahrungsgemäß bei 3 Stunden.
# Sie wollen einen echten Unterschied von 2 Stunden (delta = 2) erkennen können.

# Aufgabe: Reicht Ihr Budget (n = 20) aus, um eine Power von 80 % zu erreichen?
# Berechnen Sie die tatsächliche Power Ihres geplanten Tests.

ergebnis_power_t <- power.t.test(
  n = 20,
  delta = 2,
  sd = 3,
  sig.level = 0.05,
  power = NULL,
  type = "two.sample"
)

print(ergebnis_power_t)

# KI-Prompt zur Reflexion: 
# "Mein power.t.test in R ergab eine Power von ca. XX %. Was bedeutet das für 
# mein geplantes Experiment mit n=20? Erkläre mir, ob ich das Experiment so 
# durchführen sollte."


# --- SCREENCAST-CHECKLISTE: EXTRA-CHALLENGE -----------------------------------
# Wenn Sie diese Challenge in Ihren Screencast einbauen, zeigen Sie:
# [ ] Eine kurze, eigene Definition: Was genau ist die Power eines Tests?
# [ ] Die Erklärung des `power.prop.test` Ergebnisses: Wie viele Nutzer 
#     braucht der VIP-Popup-Test für beide Gruppen insgesamt und warum?
# [ ] Ihre datengetriebene Empfehlung für den Catering-Test: Reicht das 
#     Budget aus oder laufen wir Gefahr, Zeit und Geld zu verschwenden?
# ------------------------------------------------------------------------------