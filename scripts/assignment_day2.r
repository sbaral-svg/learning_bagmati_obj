inv <- read.csv("./data/raw/bagmati_raw_inventory.csv")

dim(inv)
str(inv)
head(inv)
summary(inv)
names(inv)
unique(inv$Plot_ID)
length(unique(inv$Plot_ID))
unique(inv$Stand_ID)
length(unique(inv$Stand_ID))
unique(inv$Tree_ID)
length(unique(inv$Tree_ID))
unique(inv$Species)
length(unique(inv$Species))

inv[
  inv$Species == "S. robusta" | inv$Species == "Shorea rubusta",
]$Species <- "Shorea robusta"

unique(inv$Species)

inv$DBH_cm <- ifelse(inv$DBH_cm == -99, 29.9, inv$DBH_cm)

inv[inv$DBH_cm < 0, c("DBH_cm", "Height_m", "Species")]

inv["Carbon_Tree"] <- inv$Biomass_kg * 0.47
names(inv)

inv["Carbon_Megagrams"] <- inv$Carbon_Tree / 1000
inv["Carbon_Density_per_Hectare"] <-
  sum(inv$Carbon_Megagrams) / sum(inv$Plot_Area_ha)

calculate_carbon_stock <- function(biomass_kg) {
  biomass_kg * 0.47
}

convert_kg_to_megagrams <- function(kg) {
  kg / 1000
}

carbon_density_per_ha <- function(carbon_stock, sampled_area) {
  sum(carbon_stock) / sum(sampled_area)
}

carbon_stock <- calculate_carbon_stock(inv$Biomass_kg)
carbon_stock[1:10]

write.csv(inv, paste("./data/processed/", "cleaned_data.csv", sep = ""))
