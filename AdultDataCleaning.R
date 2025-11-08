capture_path <- "./Data/Data-Analysis-Mosquito/Data-entomologique/"
output_path <- "./2. Adult_results/CleanedData/"

dir.create(output_path, showWarnings = FALSE)
# origin = "1899-12-30"

capture_2018_2019 <- read.xlsx(paste0(capture_path, "CAPTURE 2020.xlsx"),
                               sheet = "CDC (3)", startRow = 9) %>% 
  filter(!is.na(X)|!is.na(Y)) %>% 
  dplyr::rename(Installation_date = "Date_de_pose",
                Collection_date = "Date_de_releve",
                Installation_hour = "Heure_de_pose",
                Collection_hour = "Heure_de_relève",
                Longitude = Y, Latitude = X,
                Trap_type = Type_de_piège) %>% 
  mutate(Installation_date = as.Date(Installation_date, origin = "1899-12-30"),
         Collection_date = as.Date(Collection_date, origin = "1899-12-30"),
         Installation_hour = convert_hm(Installation_hour),
         Collection_hour = convert_hm(Collection_hour),
         Longitude = abs(Longitude),
         Latitude = -abs(Latitude),
         Latitude = ifelse(Latitude/13 < -1, Latitude/1e6, Latitude)
  ) %>% 
  dplyr::select(-Date_dernier_AID, -N_de_piègeage, -Référence_piège, -Cadre_de_la_capture, 
                -Batterie, -N, -OUI_NON, -Si_Non_pourquoi, -starts_with("X"), -Vérif.Mina, 
                -absence.GPS, -observation, -`dont_nb_Gorgé?`, -`autres_traitement?`,
                -`Présence_MIILD`, -`situation_int_ext`) %>% 
  mutate(NB = as.numeric(NB))
# names(capture_2018_2019) %>% edit()

capture_2020 <- read.xlsx(paste0(capture_path, "CAPTURE 2020.xlsx"), 
                          sheet = "CAPTURE ADULTES 2020", startRow = 9) %>%
  filter(!is.na(X)|!is.na(Y)) %>% 
  dplyr::rename(Installation_date = "Date.de.pose",
                Collection_date = "Date.de.relevé",
                Installation_hour = "Heure.de.pose",
                Collection_hour = "Heure.de.relève",
                Trap_type = "Type.de.piège",
                Longitude = Y, Latitude = X) %>% 
  mutate(Installation_date = as.Date(Installation_date, origin = "1899-12-30"),
         Collection_date = as.Date(Collection_date, origin = "1899-12-30"),
         Longitude = abs(Longitude),
         Latitude = -abs(Latitude)) %>% 
  dplyr::select(-Numéro, -Date_dernier_AID, -N.de.piègeage, -cadre.de.la.capture, -starts_with("X"), 
                -horaires, -observations, -date, -numéro.de.cas, -type.de.piège, 
                -Cadre.de.la.capture, -`N°.cas`, -`Présence_MIILD`) %>% 
  mutate(NB = as.numeric(NB))

# capture_2020 %>% View()

capture_2021 <- read.xlsx(paste0(capture_path, "CAPTURE 2022.xlsx"), 
                          sheet = "CAPTURE ADULTES 2021", startRow = 9) %>%
  filter(!is.na(X)|!is.na(Y)) %>% 
  dplyr::rename(Installation_date = "Date.de.pose",
                Collection_date = "Date.de.relevé",
                Installation_hour = "Heure.de.pose",
                Collection_hour = "Heure.de.relève",
                Trap_type = "Type.de.piège",
                Longitude = Y, Latitude = X) %>% 
  mutate(Installation_date = as.Date(Installation_date, origin = "1899-12-30"),
         Collection_date = as.Date(Collection_date, origin = "1899-12-30"),
         Longitude = abs(Longitude %>% as.numeric()),
         Latitude = -abs(Latitude %>% as.numeric())) %>% 
  dplyr::select(-Numéro, -Date_dernier_AID, -N.de.piègeage, -cadre.de.la.capture, -starts_with("X"), 
                -horaires, -observations, -date, -numéro.de.cas, -type.de.piège, -`N°.cas`,
                -`Cadre.de.la.capture`, -`Présence_MIILD`) %>% 
  mutate(NB = as.numeric(NB))

capture_2022 <- read.xlsx(paste0(capture_path, "CAPTURE 2022.xlsx"), 
                          sheet = "CAPTURE ADULTES 2021 (2)", startRow = 9) %>%
  filter(!is.na(X)|!is.na(Y)) %>% 
  dplyr::rename(Installation_date = "Date.de.pose",
                Collection_date = "Date.de.relevé",
                Installation_hour = "Heure.de.pose",
                Collection_hour = "Heure.de.relève",
                Trap_type = "Type.de.piège",
                Longitude = Y, Latitude = X) %>% 
  mutate(Installation_date = as.Date(Installation_date, origin = "1899-12-30"),
         Collection_date = as.Date(Collection_date, origin = "1899-12-30"),
         Longitude = abs(Longitude %>% as.numeric()),
         Latitude = -abs(Latitude %>% as.numeric())) %>% 
  dplyr::select(-Date_dernier_AID, -N.de.piègeage, -cadre.de.la.capture, -starts_with("X"), 
                -horaires, -observations, -date, -numéro.de.cas, -type.de.piège, -`N°.cas`,
                -`Cadre.de.la.capture`, -`Présence_MIILD`) %>% 
  mutate(NB = as.numeric(NB))

names(capture_2018_2019) <- c("Trap_type", "Installation_date", "Collection_date", "Installation_hour", 
                              "Collection_hour", "Installation_wind", "Installation_cloud", "Installation_rain", 
                              "Collection_wind", "Collection_cloud", "Collection_rain",
                              "Commune", "Village", "Neighbourhood", "Environment", "House_type", 
                              "Latitude", "Longitude", "Type", "Species", "Sex", "Nb")

capture_2020 <- capture_2020 %>% 
  dplyr::rename(
    Installation_wind = "Vent_pose",
    Installation_rain = "Pluie_pose",
    Neighbourhood = "Quartier_Lieu.dit",
    Environment = "Environnement",
    House_type = "Type.de.maison",
    Species = "espèce",
    Sex = "sexe",
    Nb = "NB"
  ) %>% dplyr::select(-"Intérieur/extérieur")


capture_2021 <- capture_2021 %>% 
  dplyr::rename(
    Installation_wind = "Vent",
    Installation_rain = "Pluie",
    Neighbourhood = "Quartier_Lieu.dit",
    Environment = "Environnement",
    House_type = "Type.de.maison",
    Species = "espèce",
    Sex = "sexe",
    Nb = "NB"
  ) %>% dplyr::select(-"Intérieur/extérieur")


capture_2022 <- capture_2022 %>% 
  mutate(espèce = ifelse(is.na(espèce), Espèces, espèce)) %>% 
  dplyr::rename(
    Installation_wind = "Vent",
    Installation_rain = "Pluie",
    Neighbourhood = "Quartier_Lieu.dit",
    Environment = "Environnement",
    House_type = "Type.de.maison",
    Species = "espèce",
    Sex = "sexe",
    Nb = "NB"
  ) %>% dplyr::select(-"Intérieur/extérieur", -Numéro.Maison, -Espèces)

capture_2018_2019[which(!names(capture_2018_2019) %in% names(capture_2022))]

capture <- bind_rows(capture_2018_2019, capture_2020, capture_2021, capture_2022) %>% 
  dplyr::select(-Installation_cloud, -Collection_wind, -Collection_cloud, -Collection_rain, -Type) %>% 
  dplyr::mutate(Village = gsub("é", "e", Village) %>% toupper())

capture <- capture %>% 
  mutate_at(c("Installation_wind", "Installation_rain", "Trap_type", 
              "Commune", "Village", "Neighbourhood", "Environment",
              "Sex", "Species"),
            list(factor)
  ) %>% dplyr::select(-Espèces) %>% 
  filter(!is.na(Sex)) %>% as.data.frame()

capture$Village[(capture$Village %in% "HAMJAGO"|is.na(capture$Village)) & capture$Commune %in% "BANDRABOUA"] <- "M'TSANGADOUA"

# capture_com_village %>% 
#   full_join(commune_village, by = c("Commune" = "commune"), suffix = c("_capture", "_shp")) %>% 
#   kableExtra::kable()

# Correction des noms de village
capture <- capture %>% 
  mutate(Village2 = gsub("'", "", Village) %>% minuscule())


# Jointure de capture_com_village avec commune_village
capture <- capture %>% 
  mutate(
    Village2 = case_when(Village2 == "Vahibe" ~ "Vahibé",
                         Village2 == "Kaweni" ~ "Kawéni",
                         Village2 == "Dzoumogne" ~ "Dzoumogné",
                         Village2 == "Mtsapere" ~ "Mtsapéré",
                         Village2 == "Majicavo-koropa" ~ "Majicavo-Koropa",
                         Village2 == "Cavani mamoudzou" ~ "Cavani",
                         Village2 == "Tsoundzou1" ~ "Tsoundzou I",
                         Village2 == "Tsoundzou2" ~ "Tsoundzou II",
                         Village2 == "Majicavo-lamir" ~ "Majicavo-Lamir",
                         Village2 == "Mirereni" ~ "Mréréni",
                         Village2 == "Mrouale" ~ "Mroalé",
                         Village2 == "Bambo ouest" ~ "Bambo Ouest",
                         TRUE ~ Village2)
  ) %>% mutate(Village2 = as.factor(Village2))

# missing values by column
mis_val <- 100*colMeans(is.na(capture))

# Because HAMJAGO has only one observation, we can assign it to the missing villages.
capture %>% dplyr::select(Commune, Village, Longitude, Latitude) %>% filter(is.na(Longitude))
capture$Longitude[is.na(capture$Longitude) & capture$Village == "M'TSAPERE"] <- 45.217330

# names of features exceeding 14% of missing values
miss_feat <- which(mis_val > 14) %>% names()

# removing those features from the capture data
capture <- capture %>% dplyr::select(-all_of(miss_feat)) %>% droplevels() %>% 
  data.frame() %>% mutate(Sex = toupper(Sex) %>% as.factor())

# Cleaning the Species
capture <- capture %>% 
  mutate(Species = minuscule(Species),
         Year = year(Collection_date) %>% as.factor()) %>% 
  mutate(
    Species = case_when(
      Species == "Albopictus" ~ "Ae. albopictus",
      Species == "Aegypti" ~ "Ae. aegypti",
      Species == "Quinquevittatus" ~ "Er. quinquevittatus",
      Species == "Quinquefasciatus (pipiens)" ~ "Cx. quinquefasciatus",
      Species == "Carleti" ~ "Cx. quinquefasciatus",
      Species == "Funestus" ~ "An. funestus",
      Species == "An.funestus" ~ "An. funestus",
      Species == "Gambiae" ~ "An. gambiae",
      Species == "Coustani" ~ "An. coustani",
      Species == "-" ~ NA_character_,
      TRUE ~ Species
    )
  )

capture <- capture %>% 
  # filter(year(Collection_date) %in% 2019:2021) %>%
  mutate(Collection_hour = ifelse(nchar(Collection_hour) < 8, paste0(Collection_hour, ":00"), Collection_hour)) %>% 
  mutate(Collection_hour = str_trim(Collection_hour))

capture$Environment[capture$Environment == "U: zone urbain"] <- "U: zone urbaine"
capture$Sex <- toupper(capture$Sex)
capture <- capture %>% droplevels()

capture_adult0 <- capture %>% 
  # on aggrege par date, commune, environnement 
  group_by(Year, Trap_type, Installation_date, Collection_date, Installation_wind, Installation_rain, 
           Commune, Village, Village2, Environment, House_type, Latitude, Longitude, Species, Sex) %>% 
  summarise(Nb = sum(Nb), .groups = "drop")

write.xlsx(capture_adult0, paste0(output_path, "capture_adult.xlsx"))

# * Les captures sont effectue plusieurs (suivant les heures) par jours. On doit donc faire la somme des captures de moustique par jour.
capture_adult1 <- capture %>% 
  # on aggrege par date, commune, environnement 
  group_by(Year, Trap_type, Installation_date, Collection_date, Installation_wind, Installation_rain, 
           Commune, Village, Village2, Environment, House_type, Latitude, Longitude) %>% 
  summarise(Nb = sum(Nb), .groups = "drop")

write.xlsx(capture_adult1, paste0(output_path, "capture_adult1.xlsx"))