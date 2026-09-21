#' Grundgesamtheit der hypothetischen Musikfestivals in Deutschland
#'
#' Ein simulierter Datensatz, der die wahre Grundgesamtheit (Population) von
#' über 24.000 Musikfestivals repräsentiert. Dieser Datensatz wird genutzt, um
#' den Unterschied zwischen Populationsparametern (Gott-Perspektive) und 
#' Stichprobenstatistiken (Realität) zu demonstrieren.
#'
#' @format Ein Tibble mit 24.812 Zeilen und 4 Variablen:
#' \describe{
#'   \item{event_id}{Eindeutige Identifikationsnummer des Events.}
#'   \item{besucher}{Anzahl der Besucher (rechtsschief log-normalverteilt, min. 50).}
#'   \item{aufbauzeit_h}{Dauer des Bühnenaufbaus in Stunden (normalverteilt).}
#'   \item{pa_ausfall}{Binärer Indikator, ob das PA-System (Audio) ausfiel (1 = Ja, 0 = Nein).}
#' }
#' @source Simuliert für das Modul Statistik 2.
#' @examples
#' # Wahres Mittel der Aufbauzeit berechnen:
#' mean(population_events$aufbauzeit_h)
"population_events"

# --- Skript zur Generierung ---
library(tidyverse)

# Reproduzierbarkeit sichern
set.seed(42)
N <- 25000

population_events <- tibble(
  event_id = 1:N,
  besucher = round(rlnorm(N, meanlog = 7.5, sdlog = 1.2)),
  aufbauzeit_h = rnorm(N, mean = 14, sd = 3),
  pa_ausfall = rbinom(N, size = 1, prob = plogis(-3 + 0.0001 * besucher))
) %>% 
  filter(besucher > 50, aufbauzeit_h > 2)


#' A/B-Testing Datensatz für einen Festival-Ticketshop
#'
#' Ein simulierter Datensatz, der das Nutzerverhalten in zwei verschiedenen 
#' Webshop-Designs (Design A_Alt vs. Design B_Neu) eines Musikfestivals abbildet. 
#' Dieser Datensatz wird genutzt, um Hypothesentests für Anteile (Conversion Rate) 
#' und Mittelwerte (Umsatz pro Käufer) in der Praxis anzuwenden.
#'
#' @format Ein Tibble mit 800 Zeilen und 4 Variablen:
#' \describe{
#'   \item{user_id}{Eindeutige Identifikationsnummer des Webseitenbesuchers.}
#'   \item{gruppe}{Das zufällig zugewiesene Shop-Design ("A_Alt" oder "B_Neu").}
#'   \item{hat_gekauft}{Binärer Indikator für die Conversion (1 = Ticket gekauft, 0 = kein Kauf).}
#'   \item{ausgaben_euro}{Generierter Umsatz in Euro. Liegt bei 0, wenn kein Ticket gekauft wurde.}
#' }
#' @source Simuliert für das Modul Statistik 2.
#' @examples
#' # Conversion-Rate pro Gruppe berechnen:
#' table(shop_daten$gruppe, shop_daten$hat_gekauft)
"shop_daten"

# --- Skript zur Generierung ---
library(tidyverse)

# Reproduzierbarkeit sichern
set.seed(123)

shop_daten <- tibble(
  user_id = 1:800,
  gruppe = rep(c("A_Alt", "B_Neu"), each = 400),
  
  # Gruppe B konvertiert leicht besser (18% vs 12%)
  hat_gekauft = c(rbinom(400, 1, 0.12), rbinom(400, 1, 0.18)),
  
  # Gruppe B gibt im Schnitt etwas mehr Geld aus (52 Euro vs 45 Euro)
  # Nur Personen mit hat_gekauft == 1 bekommen einen Umsatz zugewiesen
  ausgaben_euro = ifelse(
    hat_gekauft == 1, 
    c(rnorm(sum(hat_gekauft[1:400]), mean = 45, sd = 10), 
      rnorm(sum(hat_gekauft[401:800]), mean = 52, sd = 12)), 
    0
  )
) %>% 
  # Rundung der Ausgaben auf realistische Cent-Beträge
  mutate(ausgaben_euro = round(ausgaben_euro, 2))


#' Regressions-Datensatz: Umsatztreiber im Festival-Ticketshop
#'
#' Dieser Datensatz ist eine Erweiterung des A/B-Testing-Szenarios. Er enthält 
#' ausschließlich die Daten der Nutzer, die tatsächlich ein Ticket gekauft haben 
#' (N = 340). Er dient der Modellierung kontinuierlicher Zielvariablen (OLS) 
#' und der Analyse von Interaktionseffekten.
#'
#' @format Ein Tibble mit 340 Zeilen und 4 Variablen:
#' \describe{
#'   \item{user_id}{Eindeutige Identifikationsnummer des Käufers.}
#'   \item{gruppe}{Das genutzte Shop-Design ("A_Alt" oder "B_Neu").}
#'   \item{verweildauer_min}{Zeit, die der Nutzer im Shop verbracht hat, in Minuten.}
#'   \item{ausgaben_euro}{Tatsächlicher Umsatz des Nutzers in Euro.}
#' }
#' @source Simuliert für das Modul Statistik 2.
#' @examples
#' # Einfaches Regressionsmodell schätzen:
#' lm(ausgaben_euro ~ verweildauer_min, data = festival_umsatz)
"festival_umsatz"

# --- Skript zur Generierung ---
library(tidyverse)

set.seed(42)

# Wir simulieren direkt N=340 Käufer
N <- 340

festival_umsatz <- tibble(
  user_id = 1:N,
  gruppe = rep(c("A_Alt", "B_Neu"), times = c(150, 190)),
  # Verweildauer (Basis: 3 bis 15 Minuten)
  verweildauer_min = runif(N, min = 3, max = 15)
) |> 
  mutate(
    # Wir bauen den Interaktionseffekt ein: 
    # In Design A bringt mehr Verweildauer deutlich mehr Umsatz (+3 Euro pro Minute).
    # In Design B (dem optimierten Shop) geht der Kauf schneller, die Verweildauer 
    # hat kaum noch einen Effekt (+0.5 Euro pro Minute).
    ausgaben_euro = case_when(
      gruppe == "A_Alt" ~ 25 + 3 * verweildauer_min + rnorm(n(), 0, 8),
      gruppe == "B_Neu" ~ 45 + 0.5 * verweildauer_min + rnorm(n(), 0, 8)
    ),
    ausgaben_euro = round(ausgaben_euro, 2),
    verweildauer_min = round(verweildauer_min, 1)
  )


#' Conversion-Datensatz: Logistische Modellierung im Ticketshop
#'
#' Ein Datensatz zur Modellierung binärer Zielvariablen (Kaufentscheidungen). 
#' Er verknüpft das Shop-Design (A/B-Test) mit dem kontinuierlichen Nutzerverhalten 
#' (Verweildauer) und enthält einen eingebauten Interaktionseffekt: In Design A 
#' führt langes Suchen zu Kaufabbrüchen, in Design B erhöht Verweildauer die Kaufchance.
#'
#' @format Ein Tibble mit 1000 Zeilen und 4 Variablen:
#' \describe{
#'   \item{user_id}{Eindeutige Identifikationsnummer des Besuchers.}
#'   \item{gruppe}{Das genutzte Shop-Design ("A_Alt" oder "B_Neu").}
#'   \item{verweildauer_min}{Zeit, die der Nutzer im Shop verbracht hat, in Minuten.}
#'   \item{hat_gekauft}{Binäre Zielvariable (1 = Ticket gekauft, 0 = Abbruch).}
#' }
#' @source Simuliert für das Modul Statistik 2.
"shop_conversion"

# --- Skript zur Generierung ---
library(tidyverse)

set.seed(42)
N <- 1000

shop_conversion <- tibble(
  user_id = 1:N,
  gruppe = sample(c("A_Alt", "B_Neu"), N, replace = TRUE),
  verweildauer_min = runif(N, min = 1, max = 15)
) %>% 
  mutate(
    # Wahrscheinlichkeit (S-Kurve) über Log-Odds definieren
    # Interaktion: Design A hat einen negativen Slope für Zeit, Design B einen positiven
    log_odds = case_when(
      gruppe == "A_Alt" ~ 1.5 - 0.4 * verweildauer_min,
      gruppe == "B_Neu" ~ -3.5 + 0.6 * verweildauer_min
    ),
    wahrscheinlichkeit = exp(log_odds) / (1 + exp(log_odds)),
    hat_gekauft = rbinom(N, 1, wahrscheinlichkeit),
    verweildauer_min = round(verweildauer_min, 1)
  ) %>% 
  select(-log_odds, -wahrscheinlichkeit)

#' Gastro-Umsatz Datensatz: Predictive Analytics und Overfitting
#'
#' Ein Datensatz für das Training und die Kreuzvalidierung von Regressionsmodellen. 
#' Er enthält historische Daten zu den Ausgaben von Festivalbesuchern an den Essens- 
#' und Getränkeständen. Um Overfitting zu demonstrieren, enthält der Datensatz 
#' gezielt Variablen ("Rauschen"), die in der Realität keinen Einfluss auf den Umsatz haben.
#'
#' @format Ein Tibble mit 500 Zeilen und 6 Variablen:
#' \describe{
#'   \item{besucher_id}{Eindeutige Identifikationsnummer.}
#'   \item{temperatur_c}{Außentemperatur in Grad Celsius (Echter Prädiktor).}
#'   \item{verweildauer_h}{Zeit auf dem Gelände in Stunden (Echter Prädiktor).}
#'   \item{schuhgroesse}{Schuhgröße des Besuchers (Rauschen).}
#'   \item{anzahl_armbaender}{Anzahl der alten Festivalbändchen am Arm (Rauschen).}
#'   \item{gastro_ausgaben_euro}{Zielvariable: Ausgaben an den Ständen in Euro.}
#' }
#' @source Simuliert für das Modul Statistik 2.
"festival_gastro"

# --- Skript zur Generierung ---
library(tidyverse)

set.seed(42)
N <- 500

festival_gastro <- tibble(
  besucher_id = 1:N,
  temperatur_c = round(rnorm(N, mean = 25, sd = 4), 1),
  verweildauer_h = round(runif(N, min = 2, max = 12), 1),
  schuhgroesse = sample(36:47, N, replace = TRUE),
  anzahl_armbaender = rpois(N, lambda = 2)
) %>% 
  mutate(
    # Wahre Formel: Umsatz steigt bei Hitze und längerer Zeit.
    # Schuhgröße und Armbänder haben NULL echten Effekt.
    gastro_ausgaben_euro = 10 + 1.5 * temperatur_c + 3 * verweildauer_h + rnorm(N, 0, 10),
    gastro_ausgaben_euro = round(pmax(gastro_ausgaben_euro, 0), 2) # Keine negativen Ausgaben
  )

#' Festival-Umfrage Datensatz: Explorative Faktoranalyse
#'
#' Ein Datensatz basierend auf einer fiktiven Post-Festival-Befragung (N = 800).
#' Er enthält 12 Items auf einer 5-stufigen Likert-Skala (1 = Trifft gar nicht zu, 
#' 5 = Trifft voll zu) sowie die Zielvariable, ob der Besucher plant, im nächsten 
#' Jahr wiederzukommen. Der Datensatz ist so konstruiert, dass die 12 Items auf 
#' exakt 3 latente Konstrukte (Atmosphäre, Organisation, Preisempfinden) laden.
#'
#' @format Ein Tibble mit 800 Zeilen und 13 Variablen:
#' \describe{
#'   \item{Q01_Musik}{"Die Bandauswahl war fantastisch." (Atmosphäre)}
#'   \item{Q02_Licht}{"Die visuelle Gestaltung der Bühnen war beeindruckend." (Atmosphäre)}
#'   \item{Q03_Stimmung}{"Die generelle Stimmung unter den Besuchern war toll." (Atmosphäre)}
#'   \item{Q04_Sound}{"Die Soundqualität war auf allen Bühnen hervorragend." (Atmosphäre)}
#'   \item{Q05_Klos}{"Die sanitären Anlagen waren stets sauber." (Organisation)}
#'   \item{Q06_Einlass}{"Der Einlass aufs Gelände ging schnell und reibungslos." (Organisation)}
#'   \item{Q07_Gastro_Warte}{"Die Wartezeiten an den Essensständen waren kurz." (Organisation)}
#'   \item{Q08_Security}{"Das Sicherheitspersonal war hilfsbereit und präsent." (Organisation)}
#'   \item{Q09_Ticketpreis}{"Der Ticketpreis war absolut gerechtfertigt." (Preis)}
#'   \item{Q10_Bierpreis}{"Die Getränkepreise waren fair." (Preis)}
#'   \item{Q11_Essen_Preis}{"Das Preis-Leistungs-Verhältnis beim Essen stimmte." (Preis)}
#'   \item{Q12_Merch_Preis}{"Die Preise für Merchandise waren angemessen." (Preis)}
#'   \item{kommt_wieder}{Binäre Zielvariable: Plant Kauf für nächstes Jahr (1 = Ja, 0 = Nein).}
#' }
#' @source Simuliert für das Modul Statistik 2.
"festival_umfrage"

# --- Skript zur Generierung ---
library(tidyverse)
library(psych)

set.seed(42)
N <- 800

# Wir simulieren zuerst die 3 wahren, latenten Faktoren (normalverteilt)
f_atmos <- rnorm(N, 0, 1)
f_orga  <- rnorm(N, 0, 1)
f_preis <- rnorm(N, 0, 1)

# Wir bauen die 12 Fragen basierend auf den Faktoren + Rauschen
festival_umfrage <- tibble(
  Q01_Musik        = 3 + 0.8 * f_atmos + rnorm(N, 0, 0.5),
  Q02_Licht        = 3 + 0.7 * f_atmos + rnorm(N, 0, 0.6),
  Q03_Stimmung     = 3 + 0.9 * f_atmos + rnorm(N, 0, 0.4),
  Q04_Sound        = 3 + 0.7 * f_atmos + rnorm(N, 0, 0.6),
  
  Q05_Klos         = 3 + 0.8 * f_orga + rnorm(N, 0, 0.5),
  Q06_Einlass      = 3 + 0.7 * f_orga + rnorm(N, 0, 0.5),
  Q07_Gastro_Warte = 3 + 0.7 * f_orga + rnorm(N, 0, 0.6),
  Q08_Security     = 3 + 0.6 * f_orga + rnorm(N, 0, 0.7),
  
  Q09_Ticketpreis  = 3 + 0.8 * f_preis + rnorm(N, 0, 0.5),
  Q10_Bierpreis    = 3 + 0.9 * f_preis + rnorm(N, 0, 0.4),
  Q11_Essen_Preis  = 3 + 0.7 * f_preis + rnorm(N, 0, 0.6),
  Q12_Merch_Preis  = 3 + 0.6 * f_preis + rnorm(N, 0, 0.8)
) %>% 
  # Likert-Skalen erzwingen (1 bis 5 runden)
  mutate(across(everything(), ~round(pmin(pmax(., 1), 5)))) %>% 
  
  # Zielvariable berechnen: Atmosphäre und Preis treiben die Rückkehr, Orga ist egal
  mutate(
    log_odds = -1 + 0.8 * f_atmos + 0.0 * f_orga + 0.6 * f_preis,
    p_wieder = exp(log_odds) / (1 + exp(log_odds)),
    kommt_wieder = rbinom(N, 1, p_wieder)
  ) %>% 
  select(-p_wieder, -log_odds)