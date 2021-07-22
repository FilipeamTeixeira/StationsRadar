#edit
library(dplyr)

#Todo: Check time to load all rds stuff
gtfs_df <- readRDS("data/gtfs_data.rds")
indicators_df <- readRDS("data/indicators_df.rds")
indicators_base <- readRDS("data/indicators_base.rds")
#indicators_df_VR_23_Gent <- readRDS("data/indicators_df_VR_23_Gent.rds")
#indicators_df_FR_74_Gent <- readRDS("data/indicators_df_FR_74_Gent.rds")
dimensions_df <- readRDS("data/dimensions_df.rds")
dimensions_base <- readRDS("data/dimensions_base.rds")
dimensions_df_Gent <- readRDS("data/dimensions_df_Gent.rds")
flows <- readRDS("data/flows.rds")

walkdist <- readRDS("data/walk_dist.rds")
ang <- readRDS("data/ang.rds")
deg_week <- readRDS("data/deg_week.rds")
clos_week <- readRDS("data/clos_week.rds")
deg_tuesday <- readRDS("data/deg_tuesday.rds")
clos_tuesday <- readRDS("data/clos_tuesday.rds")

stations <- readRDS("data/stations.rds")
#stations_VR_27_Aalst <- readRDS("data/stations_VR_27_Aalst.rds")
stations_VR_26_Leuven <- readRDS("data/stations_VR_26_Leuven.rds")
stations_all <- readRDS("data/stations_all.rds")

fiets <- readRDS("data/Fietssnelweg.rds")
regios <- readRDS("data/Vervoerregios.rds")
wandel <- readRDS("data/wandelbuffers.rds")
cambio <- readRDS("data/cambio.rds")

aarschot <- readRDS("data/Aarschot.rds")
haacht <- readRDS("data/Haacht.rds")
testelt <- readRDS("data/Testelt.rds")
wespelaar <- readRDS("data/Wespelaar.rds")
wezemaal <- readRDS("data/Wezemaal.rds")
wijgmaal <- readRDS("data/Wijgmaal.rds")

allstops <- readRDS("data/allstops.rds")
selectionstops <- readRDS("data/selectionstops.rds")

metadata <- readRDS("data/metadata.rds")
overzicht <- readRDS("data/overzicht.rds")

transportIcons <- leaflet::iconList(
  train = leaflet::makeIcon(iconUrl = "www/train.svg",
                   iconWidth = 16.2, iconHeight = 19),
  bus = leaflet::makeIcon(iconUrl = "www/bus.svg",
                 iconWidth = 16.2, iconHeight = 19),
  subway = leaflet::makeIcon(iconUrl = "www/subway.svg",
                    iconWidth = 16.2, iconHeight = 19)
)