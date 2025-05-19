## Sannhetstabell og løsningsformler-----

# Installerer pakker
install.packages("readxl")   # Les Excel-filer
install.packages("QCA")      # Kjøre QCA-analyse
install.packages("writexl")  # Lagre til Excel

# Last inn nødvendige pakker
library(readxl)
library(QCA)
library(writexl)

# Lese inn Excel-filen (endre filnavnet hvis nødvendig)
data <- read_excel("celine_data/excel_tabell_analyse.xlsx")
View(data)

# Konverter til data.frame for å unngå feil
data <- as.data.frame(data)
head(data)

## Definerer betingelser og utfallssett------
# Nå fjerner vi "Fylke" (som bare er en ID) og definerer betingelser + utfallssett.

# Definer avhengig variabel (utfallssett)
outcome <- "outcome"

# Definer betingelser (alle variabler unntatt "Fylke" og "Utfallssett")
conditions <- names(data)[!names(data) %in% c("fylke", "outcome")]

# Konverter alle betingelser til numeriske verdier (i tilfelle)
data[, conditions] <- lapply(data[, conditions], as.numeric)

# Fjern spesialtegn fra alle kolonnenavn
colnames(data) <- gsub("[^A-Za-z0-9_]", "", colnames(data))

# Sjekk at variabelnavnene er renset
print(colnames(data))

# Oppdater betingelser etter rensing
conditions <- names(data)[!names(data) %in% c("fylke", "outcome")]

# Sjekk at variablene ser riktig ut
print(conditions)
print(outcome)


## Lag sannhetstabell------
# Nå lager vi sannhetstabellen, som viser alle kombinasjoner av betingelser og 
# hvilke som fører til utfallet.

# Generer sannhetstabellen
truth_table <- truthTable(
  data = data, 
  outcome = outcome, 
  conditions = conditions,
  incl.cut = 0.75,
  complete = TRUE,  # Viser også kombinasjoner uten empiri
  show.cases = TRUE # Viser hvilke caser som er i hver rad
)

# Vis sannhetstabellen
print(truth_table)

# Konverter til data.frame
truth_table_df <- as.data.frame(truth_table$tt)

# Lagre til Excel
write_xlsx(truth_table_df, "Sannhetstabell.xlsx")
getwd()


## Genererer sannhetstabell med fylkesnavn--------

# Generer sannhetstabellen og gi den nytt navn
truth_table_fylke <- truthTable(
  data = data, 
  outcome = outcome, 
  conditions = conditions, 
  complete = TRUE,   # Viser også kombinasjoner uten empiri
  show.cases = TRUE  # Viser hvilke fylker (caser) som er i hver rad
)

# Konverter sannhetstabellen til en data.frame
truth_table_with_fylker <- as.data.frame(truth_table_fylke$tt)

# Lag en ny kolonne for fylker og fyll den med NA
truth_table_with_fylker$Fylker <- NA

# Fyll inn fylker der det finnes data
valid_rows <- 1:length(truth_table_fylke$cases)  # Kun rader med faktiske caser
truth_table_with_fylker$Fylker[valid_rows] <- truth_table_fylke$cases

# Sjekk at fylker er lagt til riktig
head(truth_table_with_fylker)

# Fikk feil, skulle ikke ha tall men navn på fylke
# Lag en oppslagstabell som kobler fylkesnummer til navn
fylke_mapping <- setNames(data$Fylke, as.character(1:nrow(data)))

# Sjekk at fylke_mapping ser riktig ut
print(fylke_mapping)

# Funksjon for å erstatte tallene med fylkesnavn
replace_fylke_numbers <- function(x) {
  if (is.na(x) | x == "") return(NA)  # Håndter tomme verdier
  fylke_nums <- unlist(strsplit(x, ","))  # Splitt tallene i en liste
  fylke_names <- fylke_mapping[fylke_nums]  # Finn navnene i oppslagstabellen
  return(paste(na.omit(fylke_names), collapse = ", "))  # Sett dem sammen
}

# Erstatt tallene med fylkesnavn i sannhetstabellen
truth_table_with_fylker$Fylker <- sapply(truth_table_with_fylker$Fylker, replace_fylke_numbers)

# Sjekk at fylkene nå er riktig
head(truth_table_with_fylker)

library(writexl)
write_xlsx(truth_table_with_fylker, "Sannhetstabell_med_fylkenavn.xlsx")





## Lager løsningsformlene -------

# Nå skal vi bruke Quine-McCluskey-algoritmen for å finne de enkleste 
# løsningsformlene. Dette gjør vi ved å bruke logisk minimering av 
# sannhetstabellen.

print(truth_table_fylke)

truth_table <- truthTable(
  data = data, 
  outcome = outcome, 
  conditions = conditions, 
  complete = TRUE,   
  show.cases = TRUE,  
  incl.cut = 0.75  # Setter en terskel for tilstrekkelighet
)

truth_table_fylke$OUT <- ifelse(truth_table_fylke$consistency >= 0.75, 1, 0)

## Minimal løsning-------

# Vi bruker minimize()-funksjonen fra QCA-pakken for å finne de enkleste 
# årsakskombinasjonene for utfallet.

# Minimeringsanalysen:
solution <- minimize(
  truth_table,  # Bruk sannhetstabellen vi laget
  include = "?",      # Tar med logiske restverdier
  details = TRUE,     # Gir mer informasjon om løsningen
  show.cases = TRUE   # Viser hvilke caser som er i hver løsning
)
# Vis resultatene
print(solution)

## Kompleks løsning------

solution_complex <- minimize(truth_table, details = TRUE, include = "C")
print(solution_complex)

#Sjekker hvilke fylker som tilhører numrene i den komplekse løsingen
print(fylke_mapping)

## Mellomliggende løsning-------

#Skal ha teoretiske forventinger. Se rekkefølge: 
print(conditions)

#Første hypotese
#~oppslutning, mobilisering, storby, etnisk_mangfold, ~utdanning

solution_intermediate1 <- minimize(
  truth_table,  
  include = "?",      # Bruk plausible logiske restverdier
  details = TRUE,     
  show.cases = TRUE,
  dir.exp = c(0, 1, 1, 1, 0)
)

# Vis resultatene
print(solution_intermediate1)

#Andre hypotese
#oppslutning, mobilisering, storby, etnisk_mangfold, ~utdanning

solution_intermediate2 <- minimize(
  truth_table,  
  include = "?",      # Bruk plausible logiske restverdier
  details = TRUE,     
  show.cases = TRUE,
  dir.exp = c(1, 1, 1, 1, 0)
)

# Vis resultatene
print(solution_intermediate2)
