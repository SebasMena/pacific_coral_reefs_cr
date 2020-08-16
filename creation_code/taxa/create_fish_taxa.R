## 
## 
## 
## Objetivo: 
## 
## Método: 1. Se cargan los datos de uvc
##         2. Se extraen rasgos troficos de fishbase
##         3. 
##         4. 
##         5. 
##         6. Se guarda el archivo en formato .rda
##
## Fecha modificacion: 2020-07-10
##
## Autora: Andrea Arriaga-Madrigal
##


library(rfishbase)
library(rredlist)
library(taxize)
library(usethis)
library(pbapply)


# 1. cargando los datos de uvc
raw_locale <- "data_raw/"

load(paste0(raw_locale,"fish/fish_herb.rda"))

# 2. extrayendo rasgos funcionales de fishbase
## 2.1 definiendo lista de especies
spp_list <- dat$Species %>% as.character %>% unique

troph <- ecology(spp_list, fields=c("Species", "FoodTroph", "DietTroph",  "FeedingType"))
troph <- troph[!duplicated(troph), ]
troph <- troph %>%
  dplyr::select(Species,
                trofico_food = FoodTroph,
                trofico_diet = DietTroph,
                alim = FeedingType)

troph$alim %<>% as.factor()
troph %<>% drop_na(Species)
troph$Species %<>% as.factor()

# Subconjunto de peces herbivoros segun forma de alimentacion
herbalim <- troph[troph$alim=="browsing on substrate"  | troph$alim=="grazing on aquatic plants",]
herbalim <- herbalim[complete.cases(herbalim$alim), ]
herbalim_spp <- sapply(herbalim$Species, as.character)
herbalim_spp

# ## Parametros a y b
# ayb <-  length_weight(herbalim_spp) %>% unique()
# ayb %<>%
#   dplyr::select(Species,
#                 a,
#                 b, 
#                 aT = aTL)
# ayb <- ayb[!duplicated(ayb$Species), ] %>% drop_na(b)
# 

## Juntado la base de datos con herbalim
datherb <- dat %>%
  left_join(herbalim)
datherb %<>% drop_na(alim)

# ```
# 
# ```{limpiando de simbolos no alfanumericos}
# limpiar lista de especies de signos no alfanumericos
herbalim_spp <- sapply(herbalim_spp, function(x)
  paste(strsplit(gsub("[^[:alnum:] ]", "", x), " +")[[1]], collapse = " "), USE.NAMES = FALSE)

# ```
# WORMS
# ```{Extraccion de informacion taxonomica}
# obtener los IDs the las especies de "worms"
worms_ids <- pbsapply(herbalim_spp, function(x)
  try(get_wormsid(x, kingdom = "Animalia", rows = 1, messages = FALSE), silent = TRUE), USE.NAMES = TRUE)

# verificar errores
names(worms_ids)[sapply(worms_ids, function(x) grepl("Error", x))]

# remover errores
worms_ids <- worms_ids[!sapply(worms_ids, function(x) grepl("Error", x))]

tax.data <- pbsapply(worms_ids, function(x)
  try(classification(x, db = "worms"), silent = TRUE), USE.NAMES = TRUE)

# equivalente al anterior sin barra de progreso
# tax.data <- classification(worms_ids, db = "worms")
#transponiendo las especies
ttax.data <- lapply(tax.data, function(x) {
  t(x)
})

# convertir a data frames
tax.info <- sapply(ttax.data, function(x) {
  txd <- as.data.frame(t(x[1, ]), stringsAsFactors = FALSE)
  colnames(txd) <- x[2,]
  
  return(txd)
})

##Uniendo las especies en una sola de bases y poniendo NA a los niveles taxonomicos que no tienen (con dplyr)
tax.info1 <- do.call(bind_rows, tax.info)

tax.info <- tax.info1[, c("Kingdom", "Phylum", "Class", "Order", "Family", "Species")]
tax.info %<>% separate(Species, c("Genus", "Epithet"), " ", remove = FALSE)

# # anadir epiteto y genero y uniendo con base de datos
fish_taxa <- datherb %>% 
  left_join(tax.info)

# %<>%
#   left_join(ayb)

# ordenar columnas taxonomicamente
fish_taxa <- fish_taxa[, c("region", "locality", "sites", "depth_m", "zone", "exposure", "ID_transect", "area_uvc", "month", "day", "year", "observer", "speciesB", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "Epithet", "abund", "size_cm", "alim", "trofico_food", "trofico_diet")]
  


# ## Comprobando que no hayan documentos llamados igual
file.name <- "fish_taxa.rda"

# x <- "a"
# while(file.exists(file.name)) {
#   file.name <- paste(x, file.name, sep = "", collapse = NULL)
# }

#guardando datos intermedios
save_locale <- "data_intermediate/taxa/"

# save sites
save(fish_taxa,
     file = paste0(save_locale, file.name))
