filepath <- "C:/Users/Nelson/Documents/Projects"

#### Duration ####
## Annuals
annuals <- read.csv(file = paste0(filepath,
                                  "/usda_plants_list_",
                                  "annuals",
                                  ".csv"),
                    stringsAsFactors = FALSE)
# Because synonyms are screwing with things
annuals$Accepted.Symbol[annuals$Synonym.Symbol != ""] <- annuals$Synonym.Symbol[annuals$Synonym.Symbol != ""]

## Perennials
perennials <- read.csv(file = paste0(filepath,
                                  "/usda_plants_list_",
                                  "perennials",
                                  ".csv"),
                    stringsAsFactors = FALSE)
perennials$Accepted.Symbol[perennials$Synonym.Symbol != ""] <- perennials$Synonym.Symbol[perennials$Synonym.Symbol != ""]

## Cleanup
# Anything that *could* be perennial will be treated as such
annual_only <- annuals[!(annuals$Accepted.Symbol %in% perennials$Accepted.Symbol), ]

durations <- rbind(annual_only,
                   perennials)[c("Accepted.Symbol", "Scientific.Name", "Duration")]

bad_codes <- names(table(durations$Accepted.Symbol))[table(durations$Accepted.Symbol) > 1]

#### Growth Habit ####
## Graminoids
graminoids <- read.csv(file = paste0(filepath,
                                  "/usda_plants_list_",
                                  "graminoids",
                                  ".csv"),
                    stringsAsFactors = FALSE)
# Because synonyms are screwing with things
graminoids$Accepted.Symbol[graminoids$Synonym.Symbol != ""] <- graminoids$Synonym.Symbol[graminoids$Synonym.Symbol != ""]

## Forbs
forbs <- read.csv(file = paste0(filepath,
                                  "/usda_plants_list_",
                                  "forbs",
                                  ".csv"),
                    stringsAsFactors = FALSE)
forbs$Accepted.Symbol[forbs$Synonym.Symbol != ""] <- forbs$Synonym.Symbol[forbs$Synonym.Symbol != ""]

## Lichens
lichens <- read.csv(file = paste0(filepath,
                                  "/usda_plants_list_",
                                  "lichenous",
                                  ".csv"),
                    stringsAsFactors = FALSE)
lichens$Accepted.Symbol[lichens$Synonym.Symbol != ""] <- lichens$Synonym.Symbol[lichens$Synonym.Symbol != ""]

## Nonvascular
nonvasculars <- read.csv(file = paste0(filepath,
                                  "/usda_plants_list_",
                                  "nonvascular",
                                  ".csv"),
                    stringsAsFactors = FALSE)
nonvasculars$Accepted.Symbol[nonvasculars$Synonym.Symbol != ""] <- nonvasculars$Synonym.Symbol[nonvasculars$Synonym.Symbol != ""]

## Shrubs
shrubs <- read.csv(file = paste0(filepath,
                                  "/usda_plants_list_",
                                  "shrubs",
                                  ".csv"),
                    stringsAsFactors = FALSE)
shrubs$Accepted.Symbol[shrubs$Synonym.Symbol != ""] <- shrubs$Synonym.Symbol[shrubs$Synonym.Symbol != ""]

## Subshrubs
subshrubs <- read.csv(file = paste0(filepath,
                                  "/usda_plants_list_",
                                  "subshrubs",
                                  ".csv"),
                    stringsAsFactors = FALSE)
subshrubs$Accepted.Symbol[subshrubs$Synonym.Symbol != ""] <- subshrubs$Synonym.Symbol[subshrubs$Synonym.Symbol != ""]

## Trees
trees <- read.csv(file = paste0(filepath,
                                  "/usda_plants_list_",
                                  "trees",
                                  ".csv"),
                    stringsAsFactors = FALSE)
trees$Accepted.Symbol[trees$Synonym.Symbol != ""] <- trees$Synonym.Symbol[trees$Synonym.Symbol != ""]

## Vines
vines <- read.csv(file = paste0(filepath,
                                  "/usda_plants_list_",
                                  "vines",
                                  ".csv"),
                    stringsAsFactors = FALSE)
vines$Accepted.Symbol[vines$Synonym.Symbol != ""] <- vines$Synonym.Symbol[vines$Synonym.Symbol != ""]

## Cleanup
# Vines are probably other things, so no forbs, subshrubs, or shrubs
vines_only <- vines[!(vines$Accepted.Symbol %in% forbs$Accepted.Symbol) &
                      !(vines$Accepted.Symbol %in% subshrubs$Accepted.Symbol) &
                      !(vines$Accepted.Symbol %in% shrubs$Accepted.Symbol), ]

# Subshrubs are probably other things, so no forbs or shrubs
subshrubs_only <- subshrubs[!(subshrubs$Accepted.Symbol %in% forbs$Accepted.Symbol) &
                              !(subshrubs$Accepted.Symbol %in% shrubs$Accepted.Symbol), ]

# Shrubs may be other things, so no forbs, trees, or graminoids
shrubs_only <- shrubs[!(shrubs$Accepted.Symbol %in% forbs$Accepted.Symbol) &
                        !(shrubs$Accepted.Symbol %in% trees$Accepted.Symbol) &
                        !(shrubs$Accepted.Symbol %in% graminoids$Accepted.Symbol), ]

# If it's a forb or graminoid, it isn't a tree (NO LIGNIN!!!!). No vines or subshrubs either
trees_only <- trees[!(trees$Accepted.Symbol %in% forbs$Accepted.Symbol) &
                      !(trees$Accepted.Symbol %in% graminoids$Accepted.Symbol) &
                      !(trees$Accepted.Symbol %in% vines$Accepted.Symbol) &
                      !(trees$Accepted.Symbol %in% subshrubs$Accepted.Symbol), ]

growth_habits <- rbind(forbs,
                       graminoids,
                       lichens,
                       nonvasculars,
                       trees_only,
                       shrubs_only,
                       subshrubs_only,
                       vines_only)[c("Accepted.Symbol", "Scientific.Name", "Growth.Habit")]

bad_codes <- names(table(growth_habits$Accepted.Symbol))[table(growth_habits$Accepted.Symbol) > 1]

View(growth_habits[growth_habits$Accepted.Symbol %in% bad_codes, ])

#### Combining!

table(durations$Accepted.Symbol %in% growth_habits$Accepted.Symbol)
table(growth_habits$Accepted.Symbol %in% durations$Accepted.Symbol)

plants_lookup <- merge(x = durations,
                       y = growth_habits,
                       by = c("Accepted.Symbol", "Scientific.Name"),
                       all = TRUE)

plants_lookup$Duration[is.na(plants_lookup$Duration) | plants_lookup$Duration == "NA"] <- "Unknown"
plants_lookup$Growth.Habit[is.na(plants_lookup$Growth.Habit) | plants_lookup$Growth.Habit == "NA"] <- "Unknown"


bad_codes <- names(table(plants_lookup$Accepted.Symbol))[table(plants_lookup$Accepted.Symbol) > 1]

plants_lookup[grepl(plants_lookup$Accepted.Symbol,
      pattern = "^\\d+$"), ]

#### Write that shit out
write.csv(x = plants_lookup,
          file = paste0(filepath,
                        "/usda_plants_characteristics_lookup.csv"),
          row.names = FALSE)

garbage <- read.csv(paste0(filepath,
                           "/usda_plants_characteristics_lookup.csv"),
                    stringsAsFactors = FALSE)

species_code_var <- "Accepted.Symbol"

garbage$Accepted.Symbol[2] <- "2-Feb"

correct_species_codes <- function(lookup_table,
                                  species_code_var) {
  month_lookup <- data.frame(month = c("Jan", "Feb", "Mar", "Apr", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                             correction = c("JANU", "FEBR", "MARC", "APRI", "JUNE", "JULY", "AUGU", "SEPT", "OCTO", "NOVE", "DECE"),
                             stringsAsFactors = FALSE)
  
  for (row in seq_len(nrow(month_lookup))) {
    current_month <- month_lookup$month[row]
    current_correction <- month_lookup$correction[row]
    search_string <- paste0("^\\d+-", current_month, "$")
    
    offending_indices <- grep(x = lookup_table[[species_code_var]],
                              pattern = search_string)
    
    if (length(offending_indices) > 0) {
      offenders <- lookup_table[["species_code_var"]][offending_indices]
      offenders_corrected <- paste0(current_correction,
                                    gsub(offenders,
                                         pattern = paste0("-", current_month),
                                         replacement = ""))
      lookup_table[["species_code_var"]][offending_indices] <- offenders_corrected
    }
  }
  
  lookup_table
}

