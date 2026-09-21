# ==============================================================================
# Statistik 2: Logistische Regression
# Skript 3: Extra-Challenge - Die Illusion der Sicherheit an den Rändern
# ==============================================================================

library(tidyverse)
library(broom)

# ------------------------------------------------------------------------------
# Teil 1: Die unsichtbare Gefahr der S-Kurve
# ------------------------------------------------------------------------------
# Wenn unser Modell einen hochsignifikanten p-Wert für die 'verweildauer_min' 
# ausspuckt, neigen wir dazu zu glauben, die Vorhersagen der Kurve seien überall 
# gleich belastbar. Das ist ein fataler Trugschluss.
#
# Wo wir wenig Daten haben (z.B. Nutzer, die extrem lange im Shop bleiben), 
# wird die Schätzung extrem unsicher. Der marginale Effekt mag in der Theorie 
# existieren, ist in der Praxis an diesen Stellen aber statistisch nicht mehr 
# von Null zu unterscheiden.

modell_logit <- glm(hat_gekauft ~ verweildauer_min, 
                    data = shop_conversion, 
                    family = "binomial")

# ------------------------------------------------------------------------------
# Teil 2: Konfidenzintervalle richtig berechnen (Logit-Skala)
# ------------------------------------------------------------------------------
# Wenn wir Standardfehler (se.fit) für unsere Wahrscheinlichkeiten wollen, 
# dürfen wir R NICHT nach type = "response" fragen. R würde die Fehler linear 
# addieren, was zu Konfidenzintervallen > 100 % oder < 0 % führen kann.
# 
# Stattdessen: Wir sagen auf der Log-Odds-Skala (type = "link") vorher, 
# bauen dort das Intervall (+/- 1.96 * Standardfehler) und rechnen erst ganz 
# am Ende alles per plogis() in Prozent um.

# 1. Spektrum für Vorhersagen definieren (Wir schauen auch auf extreme Zeiten)
plot_daten <- data.frame(verweildauer_min = seq(1, 25, by = 0.5))

# 2. Vorhersage inkl. Standardfehler auf Log-Odds-Skala (link)
vorhersagen <- predict(modell_logit, newdata = plot_daten, type = "link", se.fit = TRUE)

# 3. Das 95% Konfidenzintervall bauen und umrechnen
plot_daten <- plot_daten %>%
  mutate(
    fit_logodds = vorhersagen$fit,
    se_logodds = vorhersagen$se.fit,
    
    # 95% CI auf der Logit-Skala
    lower_logodds = fit_logodds - 1.96 * se_logodds,
    upper_logodds = fit_logodds + 1.96 * se_logodds,
    
    # Umrechnung in echte Wahrscheinlichkeiten (0 bis 1) mit plogis()
    # plogis(x) ist mathematisch identisch zu exp(x)/(1+exp(x))
    p_fit = plogis(fit_logodds),
    p_lower = plogis(lower_logodds),
    p_upper = plogis(upper_logodds)
  )

# ------------------------------------------------------------------------------
# Teil 3: Die Unsicherheit visualisieren
# ------------------------------------------------------------------------------

ggplot(plot_daten, aes(x = verweildauer_min)) +
  geom_line(aes(y = p_fit), color = "blue", linewidth = 1.2) +
  geom_ribbon(aes(ymin = p_lower, ymax = p_upper), alpha = 0.2, fill = "blue") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  labs(title = "Kaufwahrscheinlichkeit mit 95% Konfidenzintervall",
       subtitle = "Beachten Sie, wie das graue Band an den Rändern explodiert!",
       x = "Verweildauer (Minuten)",
       y = "Vorhergesagte Conversion")

# Erkenntnis: Bei 5 Minuten ist das Konfidenzband extrem schmal (hohe Sicherheit).
# Aber schauen Sie auf Minute 22: Das Band reicht von ca. 30 % bis fast 90 %.
# Wenn das Management fragt: "Bringt es etwas, wenn wir Nutzer von 22 auf 23 
# Minuten auf der Seite halten?", lautet die statistische Antwort: "Wir haben 
# absolut keine Ahnung." Der marginale Effekt ist dort nicht mehr signifikant 
# von purem Rauschen zu unterscheiden.


# --- SCREENCAST-CHECKLISTE: EXTRA-CHALLENGE -----------------------------------
# Wenn Sie diese Profi-Visualisierung im Screencast nutzen:
# [ ] Zeigen Sie den Plot und erklären Sie, was das farbige Band bedeutet 
#     (Die Unsicherheit der Schätzung).
# [ ] Erklären Sie den Business-Impact: Warum dürfen wir dem Management für den 
#     Bereich über 15 Minuten Verweildauer keine harten Garantien mehr geben, 
#     obwohl der p-Wert in der tidy()-Tabelle signifikant war?
# ------------------------------------------------------------------------------