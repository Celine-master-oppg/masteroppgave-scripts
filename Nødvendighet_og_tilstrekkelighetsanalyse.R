## Nødvendighetsrelasjoner og tilstrekkelighetsanalyse

install.packages("readxl")  # For å lese Excel-filer
install.packages("QCA")     # For å kjøre QCA-analyse

library(readxl)
library(QCA)

# Lese inn Excel-filen (endre filnavnet hvis nødvendig)
data <- read_excel("celine_data/excel_tabell_analyse.xlsx")

# Sjekk at dataene ble lastet inn riktig
View(data)

# Konverterer datasett til data.frame
data <- as.data.frame(data)

## Fjerner "Fylke" og definerer variabler

# Definer utfallssettet (avhengig variabel)
outcome <- "Utfallssett"  # Endre hvis ditt utfallssett heter noe annet

# Definer betingelser (alle variabler unntatt "Fylke" og "Utfallssett")
conditions <- names(data)[!names(data) %in% c("Fylke", "Utfallssett")]

# Sjekk at variablene er riktige
print(conditions)
print(outcome)

## Konverterer variablene til numeriske

# Konverter alle betingelser til numeriske verdier
data[, conditions] <- lapply(data[, conditions], function(x) as.numeric(as.character(x)))

# Sjekk at alle variabler nå er numeriske
str(data)

## Nøvendighetsanalysen-----
# Kjør nødvendighetsanalysen
necessity_results <- pof(data[, conditions], data[, outcome])

# Vis resultatene
print(necessity_results)

## Tilstrekkelighetsanalyse-------

# Kjør tilstrekkelighetsanalysen
sufficiency_results <- pof(data[, conditions], data[, outcome], relation = "sufficiency")

# Vis resultatene
print(sufficiency_results)
str(sufficiency_results)

# Laster ned

# Ekstraher resultatene fra sufficiency_results
sufficiency_table <- as.data.frame(sufficiency_results$result)
sufficiency_table <- as.data.frame(sufficiency_results$incl.cov)


# Sjekk at tabellen ser riktig ut
head(sufficiency_table)

library(writexl)
write_xlsx(sufficiency_table, "Tilstrekkelighetsanalyse.xlsx")

library(writexl)
write_xlsx(sufficiency_table, "Tilstrekkelighetsanalyse.xlsx")

