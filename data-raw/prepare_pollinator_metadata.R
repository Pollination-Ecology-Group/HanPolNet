## code to prepare `pollinator_metadata` dataset
## This script builds the metadata from scratch using the raw interaction data.

library(dplyr)
library(stringr)

# --- 1. Main Script Logic ---

# Read the raw interaction data
raw_interactions <- read.csv("data-raw/jine_ctverce_spojene_11_24_vz_20250515 - jine_ctverce_spojene_11_24_vz_20250409.csv")

# Create a clean, definitive pollinator ID column first
# Also includes the special name corrections directly
cleaned_interactions <- raw_interactions %>%
  mutate(
    pollinator_id_final = if_else(druh_final == "", NA_character_, as.character(druh_final)),
    pollinator_id = coalesce(pollinator_id_final, as.character(druhO_opraveno)),
    pollinator_id_lower = tolower(pollinator_id),

    pollinator_id = case_when(
      grepl(ignore.case = TRUE,"apis", pollinator_id_lower) ~ "Apis_mellifera",
      grepl(ignore.case = TRUE,"scatophagidae|scathophagidae", pollinator_id_lower) ~ "Scathophagidae",
      grepl(ignore.case = TRUE,"m\u00ed\u0161a", pollinator_id_lower) ~ "Sarcophagidae",
      grepl(ignore.case = TRUE,"miridae|myridae", pollinator_id_lower) ~ "Miridae",
      TRUE ~ pollinator_id # Keep all other names as they are
    )
  ) %>%
  filter(!is.na(pollinator_id) & pollinator_id != "")

# Get unique taxa and their verification status
unique_taxa <- cleaned_interactions %>%
  group_by(pollinator_id) %>%
  summarize(verified = any(verified == 1, na.rm = TRUE), .groups = 'drop')

# --- 2. Classify Taxa using a Robust `case_when` ---
classified_taxa <- unique_taxa %>%
  mutate(
    pollinator_id_lower = tolower(pollinator_id),

    order = case_when(
      grepl(ignore.case = TRUE,"larva|pupa|nympha|imago", pollinator_id_lower) & grepl(ignore.case = TRUE,"lepidoptera|geometridae", pollinator_id_lower) ~ "Lepidoptera",
      grepl(ignore.case = TRUE,"larva|pupa|nympha|imago", pollinator_id_lower) & grepl(ignore.case = TRUE,"coleoptera", pollinator_id_lower) ~ "Coleoptera",
      grepl(ignore.case = TRUE,"larva|pupa|nympha|imago", pollinator_id_lower) & grepl(ignore.case = TRUE,"hymenoptera|symphyta", pollinator_id_lower) ~ "Hymenoptera",
      grepl(ignore.case = TRUE,"larva|pupa|nympha|imago", pollinator_id_lower) & grepl(ignore.case = TRUE,"diptera|chrysops", pollinator_id_lower) ~ "Diptera",

      # Hymenoptera
      grepl(ignore.case = TRUE,"apis|anthidiellum|anthidiini|anthidini|anthophila|ammophila|braconidae|bombus|andrena|halictus|lasioglossum|osmia|megachile|colletes|hylaeus|nomada|chelostoma|vespula|ichneumonidae|mellinus|sphecidae|crabronidae|\u010cern\u00e1_kutilka_s_oran\u017eov\u00fdma_nohama|vespa|vespi|Sphecodes|Scolia|Ceratina|Chalcidoidea|Chrysididae|Ectemnius|Epeoloides|Eumeninae|Gasteruption|Halict|Hymenoptera|Macropis|Megachilidae|Polistes|Pseudoanthidium|Psithyrus|tenthredinidae|arge|athalia|Tenthredo|Sympetrum|formica|Lasius|myrmica|larva_Symphyta|Formici|Pompilidae", pollinator_id_lower) ~ "Hymenoptera",
      # Diptera
      grepl(ignore.case = TRUE,"Pollenia|anasimyia|syrphidae|syrphus|eristalis|episyrphus|melanostoma|platycheirus|sphaerophoria|cheilosia|eristalinus|helophilus|leucozona|chrysotoxum|dasysyrphus|scaeva|syritta|volucella|rhingia|\u010cern\u00e1_pestřenka|\u010cerven\u00e1_pestrenka_se_zakalenymi_kridly|metalicky_zbarvena_pestrenka|Xylota|Xanthandrus_sp|Tachina|Tabanidae|Syrphinae|Stomorhina|Sericomyia|Chrysogaster|Cylindromyia|didea|Cynomya|Diptera|Eupeodes|Handrkovske_hovadko|Mala_pruhovana_pestrenka|Mallota|Melangyn|Melyridaea_sp|Pyrophaena|Pipiz|Paragus|PPP|Orthonevra|Myathropa|Melangyna|anthomyiidae|anthrax|bellardia|brachycera|calliphoridae|calliphora|pconopidae|empididae|lucilia|musca|muscidae|tachinidae|therevidae|oxycera|bibionidae|rhagionidae|stratiomyidae|tephritidae|bila_diptera|\u010cern\u00e1_moucha|mnm|Tipulidae|Sicus|Sepsi|Sciaridae|Scatophagidae|Scathophagidae|Sarcophaga|Calyptrata|Cecidomyidae|Conopidae|Conops|Ectophasia|Eriothrix|Graphomyia|Gymnosoma_sp|scathophaga|Phasi|Neoascia|Muscid|asilidae|bibionidae|ceratopogonidae|chironomidae|chloropidae|dolichopodidae|phoridae|sarcophagidae|sepsidae|scatopsidae|sciomyzidae|lausaniidae|larva_Chrysops|Culicidae|Lipoptena|Nematocera|ceratopogonidae|chironomidae|chloropidae|dolichopodidae|phoridae|sarcophagidae|sepsidae|scatopsidae|sciomyzidae|lausaniidae|culicidae|nematocera", pollinator_id_lower) ~ "Diptera",
      # Coleoptera
      grepl(ignore.case = TRUE,"cantharidae|mordellidae|oedemera|staphylinidae|cerambycidae|dasytes|malachius|psilothrix|adalia|alleculinae|alticinae|alticini|anthaxia|cantharidae|coccinellidae|mordellidae|oedemera|staphylinidae|cerambycidae|dasytes|malachius|psilothrix|elateridae|curculionidae|Stictoleptura|Stenurella|Staphylinae|nitidulidae|Rhagonycha|Chrysomela|Coccinel|Clytra|Coleoptera|Cryptocephalus|Harmonia|Lagriinae|Mekokrovecnik|Meligethes_sp|Phytoecia|Oxythyrea|Oulema|Oedemeridae|Melyridae|Meligethes|apionidae|chrysomelidae|clambidae|cleroidea|dermestidae|silphidae|tenebrionidae|Cassid|Coleoptera_larva", pollinator_id_lower) ~ "Coleoptera",
      # Lepidoptera
      grepl(ignore.case = TRUE,"adelidae|araschnia|agrostis|anthocharis|argynnis|boloria|b\u011blop\u00e1sek|pieris|nymphalidae|vanessa|pyronia|manolia|coenonympha|gonepteryx|Lepidoptera|lycaenidae|polyommatus|inachis|aglais|melanargia|thymelicus|Zygena|Satyrini|Heliconi|Hn\u011bd\u00e1sek|Iphiclides|Issoria|Lasiommata|Maniola_jurtina|Pyraustra|Polygonia_c-album|Pontia_edusa|Melitaea|Coenonympha|hesperiidae|hesperidae|autographa|geometridae|noctuidae|tortricidae|sphingidae|crambidae|pyralidae", pollinator_id_lower) ~ "Lepidoptera",
      grepl(ignore.case = TRUE, "araneae|argiope|Thomisidae|Salticidae|Pisaura|Philodromidae|Misumena", pollinator_id_lower)~ "Araneae",
      grepl(ignore.case = TRUE, "acari", pollinator_id_lower)~ "Acari",
      grepl(ignore.case = TRUE, "opiliones", pollinator_id_lower)~ "Opiliones",
      grepl(ignore.case = TRUE, "neuroptera|Chrysopa|Chrysoperla|Chrysopidae", pollinator_id_lower)~ "Neuroptera",
      grepl(ignore.case = TRUE, "trichoptera", pollinator_id_lower)~ "Trichoptera",
      grepl(ignore.case = TRUE, "Graphosoma|hemiptera|aphididae|cicadellidae|cercopidae|Coreidae|Lygaeus|Nabidae|Miridae|Myridae", pollinator_id_lower)~ "Hemiptera",
      grepl(ignore.case = TRUE, "acrididae|caelifera|Tetrigidae|Ensifera", pollinator_id_lower)~ "Orthoptera",
      grepl(ignore.case = TRUE, "aphidoidea", pollinator_id_lower)~ "Hemiptera",
      grepl(ignore.case = TRUE, "thysanoptera", pollinator_id_lower)~ "Thysanoptera",
      grepl(ignore.case = TRUE, "psocoptera", pollinator_id_lower)~ "Psocoptera",
      grepl(ignore.case = TRUE, "collembola", pollinator_id_lower)~ "Collembola",
      grepl(ignore.case = TRUE, "isopoda", pollinator_id_lower)~ "Isopoda",
      grepl(ignore.case = TRUE, "chilopoda", pollinator_id_lower)~ "Chilopoda",
      grepl(ignore.case = TRUE, "diplopoda", pollinator_id_lower)~ "Diplopoda",
      grepl(ignore.case = TRUE, "odonata|Ischnura|Lestes|Platycnemis", pollinator_id_lower)~ "Odonata",
      grepl(ignore.case = TRUE, "plecoptera", pollinator_id_lower)~ "Plecoptera",
      grepl(ignore.case = TRUE, "ephemeroptera", pollinator_id_lower)~ "Ephemeroptera",
      grepl(ignore.case = TRUE, "mantodea", pollinator_id_lower)~ "Mantodea",
      grepl(ignore.case = TRUE, "blatta|blattodea", pollinator_id_lower)~ "Blattodea",
      grepl(ignore.case = TRUE, "phasmida", pollinator_id_lower)~ "Phasmida",
      grepl(ignore.case = TRUE, "dermaptera", pollinator_id_lower)~ "Dermaptera",
      grepl(ignore.case = TRUE, "siphonaptera", pollinator_id_lower)~ "Siphonaptera",
      grepl(ignore.case = TRUE, "thysanura", pollinator_id_lower)~ "Thysanura",
      grepl(ignore.case = TRUE, "embioptera", pollinator_id_lower)~ "Embioptera",
      grepl(ignore.case = TRUE, "zygentoma", pollinator_id_lower)~ "Zygentoma",
      grepl(ignore.case = TRUE, "mantophasmatodea", pollinator_id_lower)~ "Mantophasmatodea",
      grepl(ignore.case = TRUE, "protura", pollinator_id_lower)~ "Protura",
      grepl(ignore.case = TRUE, "diplura", pollinator_id_lower)~ "Diplura",
      grepl(ignore.case = TRUE, "scolopendra", pollinator_id_lower)~ "Chilopoda",
      grepl(ignore.case = TRUE, "geophilus", pollinator_id_lower)~ "Chilopoda",
      grepl(ignore.case = TRUE, "lithobius", pollinator_id_lower)~ "Chilopoda",
      grepl(ignore.case = TRUE, "scutigera", pollinator_id_lower)~ "Chilopoda",
      grepl(ignore.case = TRUE, "symphyla", pollinator_id_lower)~ "Symphyla",
      grepl(ignore.case = TRUE, "pauropoda", pollinator_id_lower)~ "Pauropoda",
      grepl(ignore.case = TRUE, "arachnida", pollinator_id_lower)~ "Arachnida",
      grepl(ignore.case = TRUE, "myriapoda", pollinator_id_lower)~ "Myriapoda",
      grepl(ignore.case = TRUE, "insecta", pollinator_id_lower)~ "Insecta",
      grepl(ignore.case = TRUE, "arthropoda", pollinator_id_lower)~ "Arthropoda",
      grepl(ignore.case = TRUE, "animalia", pollinator_id_lower)~ "Animalia",
      grepl(ignore.case = TRUE, "auchenorrhyncha", pollinator_id_lower)~ "Hemiptera",
      grepl(ignore.case = TRUE, "heteroptera|Reduviidae|Pentatomidae|Miridae", pollinator_id_lower)~ "Hemiptera",
      grepl(ignore.case = TRUE, "Mollusca", pollinator_id_lower)~ "Mollusca",
      grepl(ignore.case = TRUE, "coleorrhyncha", pollinator_id_lower)~ "Hemiptera",
      grepl(ignore.case = TRUE, "Panorpa", pollinator_id_lower)~ "Mecoptera",
      grepl(ignore.case = TRUE, "Opilionidae", pollinator_id_lower)~ "Opiliones",
      grepl(ignore.case = TRUE, "Rana|Hyla", pollinator_id_lower)~ "Anura",
      grepl(ignore.case = TRUE, "Felis", pollinator_id_lower)~ "Carnivora",
      TRUE ~ "Unknown"
    ),

    is_pollinator = case_when(

      # Larva
      grepl(ignore.case = TRUE,"larva|pupa|nympha|imago|apionidae|chrysomelidae|clambidae|cleroidea|dermestidae|silphidae|tenebrionidae|Cassid|Coleoptera_larva", pollinator_id_lower) ~ FALSE,

      # Hymenoptera
      order == "Hymenoptera" & grepl(ignore.case = TRUE,"apis|anthidiellum|anthidiini|anthidini|anthophila|ammophila|braconidae|bombus|andrena|halictus|lasioglossum|osmia|megachile|colletes|hylaeus|nomada|chelostoma|vespula|ichneumonidae|mellinus|sphecidae|crabronidae|\u010cern\u00e1_kutilka_s_oran\u017eov\u00fdma_nohama|vespa|vespi|Sphecodes|Scolia|Ceratina|Chalcidoidea|Chrysididae|Ectemnius|Epeoloides|Eumeninae|Gasteruption|Halict|Hymenoptera|Macropis|Megachilidae|Polistes|Pseudoanthidium|Psithyrus|tenthredinidae|arge|athalia|Tenthredo|Sympetrum|Pompilidae", pollinator_id_lower) ~ TRUE,
      grepl(ignore.case = TRUE,"formica|Lasius|myrmica|larva_Symphyta|Formici", pollinator_id_lower) ~ FALSE,
      # Diptera
      order == "Diptera" & grepl(ignore.case = TRUE," Pollenia|anasimyia|syrphidae|syrphus|eristalis|episyrphus|melanostoma|platycheirus|sphaerophoria|cheilosia|eristalinus|helophilus|leucozona|chrysotoxum|dasysyrphus|scaeva|syritta|volucella|rhingia|\u010cern\u00e1_pestřenka|\u010cerven\u00e1_pestrenka_se_zakalenymi_kridly|metalicky_zbarvena_pestrenka|Xylota|Xanthandrus_sp|Tachina|Tabanidae|Syrphinae|Stomorhina|Sericomyia|Chrysogaster|Cylindromyia|didea|Cynomya|Diptera|Eupeodes|Handrkovske_hovadko|Mala_pruhovana_pestrenka|Mallota|Melangyn|Melyridaea_sp|Pyrophaena|Pipiz|Paragus|PPP|Orthonevra|Myathropa|Melangyna|anthomyiidae|anthrax|bellardia|brachycera|calliphoridae|calliphora|pconopidae|empididae|lucilia|musca|muscidae|tachinidae|therevidae|oxycera|bibionidae|rhagionidae|stratiomyidae|tephritidae|bila_diptera|\u010cern\u00e1_moucha|mnm|Tipulidae|Sicus|Sepsi|Sciaridae|Scatophagidae|Scathophagidae|Sarcophaga|Calyptrata|Cecidomyidae|Conopidae|Conops|Ectophasia|Eriothrix|Graphomyia|Gymnosoma_sp|scathophaga|Phasi|Neoascia|Musci", pollinator_id_lower) ~ TRUE,
      grepl(ignore.case = TRUE," asilidae|bibionidae|ceratopogonidae|chironomidae|chloropidae|dolichopodidae|phoridae|sepsidae|scatopsidae|sciomyzidae|lausaniidae|larva_Chrysops|Culicidae|Lipoptena|Nematocera", pollinator_id_lower) ~ FALSE,
      # Lepidoptera
      order == "Lepidoptera" & grepl(ignore.case = TRUE,"   adelidae|araschnia|agrostis|anthocharis|argynnis|boloria|b\u011blop\u00e1sek|pieris|nymphalidae|vanessa|pyronia|manolia|coenonympha|gonepteryx|Lepidoptera|lycaenidae|polyommatus|inachis|aglais|melanargia|thymelicus|Zygena|Satyrini|Heliconi|Hn\u011bd\u00e1sek|Iphiclides|Issoria|Lasiommata|Maniola_jurtina|Pyraustra|Polygonia_c-album|Pontia_edusa|Melitaea|Coenonympha|hesperiidae|hesperidae|autographa|geometridae|noctuidae|tortricidae|sphingidae|crambidae|pyralidae
", pollinator_id_lower) ~ TRUE,
      grepl(ignore.case = TRUE," larva_Lepidoptera|larva_Geometridae", pollinator_id_lower) ~ FALSE,
      # Coleptera
      order == "Coleoptera" & grepl(ignore.case = TRUE,"adalia|alleculinae|alticinae|alticini|anthaxia|cantharidae|coccinellidae|mordellidae|oedemera|staphylinidae|cerambycidae|dasytes|malachius|psilothrix|elateridae|curculionidae|Stictoleptura|Stenurella|Staphylinae|nitidulidae|Rhagonycha|Chrysomela|Coccinel|Clytra|Coleoptera|Cryptocephalus|Harmonia|Lagriinae|Mekokrovecnik|Meligethes_sp|Phytoecia|Oxythyrea|Oulema|Oedemeridae|Melyridae|Meligethes", pollinator_id_lower) ~ TRUE,
      order == "Coleoptera" & grepl(ignore.case = TRUE,"apionidae|chrysomelidae|clambidae|cleroidea|dermestidae|silphidae|tenebrionidae|Cassid|Coleoptera_larva", pollinator_id_lower) ~ FALSE,
      grepl(ignore.case = TRUE,"thysanoptera", pollinator_id_lower) ~ TRUE,
      TRUE ~ FALSE
    )
  )

# Add placeholder columns and arrange
pollinator_metadata <- classified_taxa %>%
  mutate(
    family = NA_character_,
    genus = NA_character_,
    notes = NA_character_
  ) %>%
  select(pollinator_id, verified, is_pollinator, order, family, genus, notes) %>%
  arrange(pollinator_id)

# --- 3. Save the Data ---
usethis::use_data(pollinator_metadata, overwrite = TRUE)
write.csv(pollinator_metadata, "data-raw/pollinator_metadata_autogen.csv", row.names = FALSE, na = "")

message("✅ `pollinator_metadata.rda` and `pollinator_metadata_autogen.csv` have been created.")
