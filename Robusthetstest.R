#Robusthet nr 2

# Last inn nødvendige pakker
library(readxl)
library(QCA)
library(writexl)


#1 Lese inn Excel-filen (endre filnavnet hvis nødvendig)
robust <- read_excel("celine_data/robusthetstester_excel.xlsx")
View(robust)

#1 Konverter til data.frame for å unngå feil
robust <- as.data.frame(robust)
head(robust)

##3 Definerer betingelser og utfallssett------
# Nå fjerner vi "Fylke" (som bare er en ID) og definerer betingelser + utfallssett.

# Definer avhengig variabel (utfallssett)
outcome <- "outcome"

# Definer betingelser (alle variabler unntatt "Fylke" og "Utfallssett")
conditions <- names(robust)[!names(robust) %in% c("fylke", "utfallssett")]

# Konverter alle betingelser til numeriske verdier (i tilfelle)
robust[, conditions] <- lapply(robust[, conditions], as.numeric)

# Fjern spesialtegn fra alle kolonnenavn
colnames(robust) <- gsub("[^A-Za-z0-9_]", "", colnames(robust))

# Sjekk at variabelnavnene er renset
print(colnames(robust))

#4 Oppdater betingelser etter rensing
conditions <- names(robust)[!names(robust) %in% c("fylke", "utfallssett")]

# Sjekk at variablene ser riktig ut
print(conditions)
print(outcome)

conditions <- names(robust)[!names(robust) %in% c("fylke", "outcome")]

# Generer sannhetstabellen
truth_table <- truthTable(
  data = robust, 
  outcome = "outcome", 
  conditions = conditions,
  incl.cut = 0.75, 
  complete = TRUE,
  show.cases = TRUE
)

# Vis sannhetstabellen
print(truth_table)

## Genererer sannhetstabell med fylkesnavn--------

# Generer sannhetstabellen og gi den nytt navn
truth_table_fylke2 <- truthTable(
  data = robust, 
  outcome = "outcome", 
  conditions = conditions, 
  complete = TRUE,   # Viser også kombinasjoner uten empiri
  show.cases = TRUE  # Viser hvilke fylker (caser) som er i hver rad
)

# Konverter sannhetstabellen til en data.frame
truth_table_with_fylker2 <- as.data.frame(truth_table_fylke2$tt)

# Lag en ny kolonne for fylker og fyll den med NA
truth_table_with_fylker2$fylker <- NA

# Fyll inn fylker der det finnes data
valid_rows <- 1:length(truth_table_fylke2$cases)  # Kun rader med faktiske caser
truth_table_with_fylker2$fylker[valid_rows] <- truth_table_fylke2$cases

# Sjekk at fylker er lagt til riktig
head(truth_table_with_fylker2)

# Fikk feil, skulle ikke ha tall men navn på fylke
# Lag en oppslagstabell som kobler fylkesnummer til navn

fylke_mapping <- setNames(robust$fylke, as.character(1:nrow(robust)))

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
truth_table_with_fylker2$fylker <- sapply(truth_table_with_fylker2$fylker, replace_fylke_numbers)

# Sjekk at fylkene nå er riktig
head(truth_table_with_fylker2)

library(writexl)
write_xlsx(truth_table_with_fylker2, "Sannhetstabell_med_fylkenavn2.xlsx")


## Lager løsningsformlene -------
# Vis sannhetstabellen
print(truth_table)
# Nå skal vi bruke Quine-McCluskey-algoritmen for å finne de enkleste 
# løsningsformlene. Dette gjør vi ved å bruke logisk minimering av 
# sannhetstabellen.

print(truth_table)

truth_table <- truthTable(
  data = robust, 
  outcome = "outcome", 
  conditions = conditions, 
  complete = TRUE,   
  show.cases = TRUE,  
  incl.cut = 0.75  # Setter en terskel for tilstrekkelighet
)

truth_table$OUT <- ifelse(truth_table$consistency >= 0.75, 1, 0)

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
