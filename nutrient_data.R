library(dplyr)
library(readr)
library(tidyr)

# load environment ----
source("load.R"); paste0("Figures in ", FIGS_DIR)

a<- getQuery(NPflows_proj, "ag production by tech")

NPflows_proj <- loadProject("NPflows.proj")
#
# listQueries(NPflows_proj)

# analysis constants ----
ANALYSIS_YEARS <- c(2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070, 2075, 2080, 2085, 2090, 2095, 2100)
ANALYSIS_YEARS_FUTURE <- c(2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070, 2075, 2080, 2085, 2090, 2095, 2100)
ANALYSIS_REGIONS <- c("Africa_Southern", "Brazil", "China", "India", "USA", "Indonesia")

# plot vars ----
# TODO: rename refined liquids to petroleum
FIGS_SAVE <- TRUE  # set to TRUE to save figures to FIGS_DIR

# plots ----

###############################################################################%


# Define which years will be processed, from the model output
ANALYSIS_YEARS <- c(2015, 2021, seq(2025, 2100, 5))

# Read in external input data from N fertilizer database (Zhang et al. 2015) and crop mapping file
#N_deposition <- read_csv("nutrient_input_data/N_deposition.csv")
N_fixation <- read_csv("nutrient_input_data/N_fixation.csv")
crop_mapping <- read_csv("nutrient_input_data/crop_mapping.csv")
crop_nutrient_contents <- read_csv("nutrient_input_data/crop_nutrient_contents.csv")

# Read in external input data from N deposition database (Vishwakarma et al. 2022)
# https://datadryad.org/dataset/doi:10.5061/dryad.msbcc2g1x
# The authors recommend using the 4th datasheet (WL_update2022.xlsx) is it shows better agreement with observed data
# Description: includes the nitrogen deposition (kg N/ha yr) data at country scale using Wang et al. N deposition maps and HYDE 3.3 cropland maps'
# JS attributed the 236 countries to 'iso' and 'GCAM region' manually, and created 2 csv files: one with all the countries from the dataset- whether or not there was an assigned 'iso' and 'GCAM Region' (WL_countriesfull)
# and one retaining the countries that had an 'iso' and 'GCAM region' assigned (WL)
N_deposition_WL <- read_csv("nutrient_input_data/N_deposition_WL.csv")

# gcamdata extdata files and cached (drake) data tables, copied locally in order to avoid potential disagreements wrt branches
basin_to_country_mapping <- read_csv("nutrient_input_data/basin_to_country_mapping.csv")
GCAM_region_names <- read_csv("nutrient_input_data/GCAM_region_names.csv")
iso_GCAM_regID <- read_csv("nutrient_input_data/iso_GCAM_regID.csv")
L100.LDS_ag_HA_ha <- read_csv("nutrient_input_data/L100.LDS_ag_HA_ha.csv")
L100.LDS_ag_prod_t <- read_csv("nutrient_input_data/L100.LDS_ag_prod_t.csv")
L101.ag_HA_bm2_R_C_Y_GLU <- read_csv("nutrient_input_data/L101.ag_HA_bm2_R_C_Y_GLU.csv")

# Read in the model output data, currently read into a second query/output file. The block below assumes the
# generic code in otaq_data.R has been run (not copying it here)
#nutrient_queries <- list()
#for( fn in FILES ) {
#  nutrient_queries[[ fn ]] <- parse_mi_output( fn )
#}

# Equation for nitrogen surplus, in each basin:
# N_surplus = BNF + N_input + N_deposition - N_harvested
#  BNF = biological nitrogen fixation = land_allocation (ha) * N_fixation_rate (kgN/ha)
#   N_fixation_rate is calculated by GCAM commodity class and region from crop and country specific N fixation quantities, and historical land areas
#  Ndeposition = N_deposition_rate (kg/ha) * cropland area (ha)
# N_harvested = production * Ncontent

# Equation for phosphorus surplus, in each basin:
# P_surplus = P_input- P_harvested

# Nitrogen calculations ----

# region_map table
region_map <- left_join(iso_GCAM_regID, GCAM_region_names, by = "GCAM_region_ID") %>%
  select(iso, region)
basin_map <- basin_to_country_mapping %>%
  select(GLU = GLU_code, basin = GLU_name)

# Share table for downscaling from country-crop to basin-country-crop
HA_basin_shares_crop_ctry <- L100.LDS_ag_HA_ha %>%
  group_by(iso, GTAP_crop) %>%
  mutate(basin_share = value / sum(value)) %>%
  ungroup() %>%
  select(iso, GLU, GTAP_crop, basin_share)

# BNF (biological nitrogen fixation) history: calculate the mean across reported years, downscale to the basin, aggregate by GCAM commodity and region/basin
BNFhist_R_C_GLU_MtN <- N_fixation %>%
  group_by(iso, crop) %>%
  summarise(Nfix_kg = mean(Nfix_kg), .groups = "drop") %>%
  left_join(select(crop_mapping, GTAP_crop, crop, GCAM_commodity, GCAM_subsector), by = "crop") %>%
  inner_join(HA_basin_shares_crop_ctry, by = c("iso", "GTAP_crop")) %>%
  mutate(Nfix_Mt = Nfix_kg * basin_share * 1e-9) %>%
  left_join(region_map, by = "iso") %>%
  left_join(basin_map, by = "GLU") %>%
  group_by(region, GCAM_commodity, GCAM_subsector, basin) %>%
  summarise(Nfix_Mt = sum(Nfix_Mt), .groups = "drop")

# Divide by harvested area to get the N fixation rate (kgN/m2) by GCAM region, commodity, basin
# Harvested area by region, commodity, basin (R_C_GLU)
ag_HA_bm2_R_C_GLU <- L101.ag_HA_bm2_R_C_Y_GLU %>%
  left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
  left_join(basin_map, by = "GLU") %>%
  group_by(region, GCAM_commodity, GCAM_subsector, basin) %>%
  summarise(HA_bm2 = mean(value), .groups = "drop")

BNF_R_C_GLU_kgNm2 <- BNFhist_R_C_GLU_MtN %>%
  left_join(ag_HA_bm2_R_C_GLU, by = c("region", "GCAM_commodity", "GCAM_subsector", "basin")) %>%
  mutate(Nfix_kgNm2 = Nfix_Mt / HA_bm2) %>%
  select(region, GCAM_subsector, basin, Nfix_kgNm2)

# Multiply BNF rates by land cover to get BNF by region/basin/year
BNF_Mt_R_GLU_Y <- getQuery(NPflows_proj, "land_cover") %>%
  filter(grepl("IRR|RFD", land_allocation),
         year %in% ANALYSIS_YEARS) %>%
  separate(land_allocation, into = c("GCAM_subsector", "basin", "IRR_RFD", "mgmt")) %>%
  group_by(scenario, region, GCAM_subsector, basin, year) %>%
  summarise(land_bm2 = sum(value), .groups = "drop") %>%
  left_join(BNF_R_C_GLU_kgNm2, by = c("region", "GCAM_subsector", "basin")) %>%
  replace_na(list(Nfix_kgNm2 = 0)) %>%
  mutate(BNF_Mt = Nfix_kgNm2 * land_bm2) %>%
  group_by(scenario, region, basin, year) %>%
  summarise(BNF_Mt = sum(BNF_Mt), .groups = "drop")

# N deposition: aggregate from country to GCAM region, using harvested areas as weights
HA_ha_ctry <- L100.LDS_ag_HA_ha %>%
  left_join(iso_GCAM_regID, by = "iso")%>%
  group_by(iso, country_name, GCAM_region_ID) %>%
  summarise(HA_ha = sum(value), .groups = "drop")

# Ndep from Vishwakarma et al. 2022, see line 37 for more details
Ndep_R_kgNm2 <- N_deposition_WL %>%
  pivot_longer(
    cols = '1961':'2022',   
    names_to = "Year",      
    values_to = "N_deposition_kgha") %>% # units: kg N/ha yr
  group_by(iso) %>%
  summarise(N_deposition_kgha = mean(N_deposition_kgha), .groups = "drop") %>%
  inner_join(HA_ha_ctry, by = "iso") %>%
  mutate(N_deposition_kg = N_deposition_kgha * HA_ha) %>%
  left_join(region_map, by = "iso") %>%
  group_by(region) %>%
  summarise(N_deposition_kg = sum(N_deposition_kg),
            HA_ha = sum(HA_ha),
            .groups = "drop") %>%
  mutate(Ndep_kgNm2 = N_deposition_kg * 1e-4 / HA_ha) %>%
  select(region, Ndep_kgNm2)

# Nitrogen deposition for purposes of nutrient balances is only considered on agricultural land, not on natural ecosystems
# https://www.oecd.org/en/publications/trends-and-drivers-of-agri-environmental-performance-in-oecd-countries_b59b1142-en/full-report/nutrient-balances-in-agriculture_ab6ac5dc.html
# Data are filtered to only cropland with the IRR|RFD filter.
N_deposition_Mt_R_GLU_Y <- getQuery(NPflows_proj, "land_cover") %>%
  filter(grepl("IRR|RFD", land_allocation),
         year %in% ANALYSIS_YEARS) %>%
  separate(land_allocation, into = c("GCAM_subsector", "basin", "IRR_RFD", "mgmt")) %>%
  group_by(scenario, region, basin, year) %>%
  summarise(land_bm2 = sum(value), .groups = "drop") %>%
  left_join(Ndep_R_kgNm2, by = c("region")) %>%
  mutate(N_deposition_Mt = land_bm2 * Ndep_kgNm2) %>%
  select(scenario, region, basin, year, N_deposition_Mt)

# inputs: compile "N fertilizer" and "P fertilizer" inputs from model run
NP_inputs_Mt_R_GLU_Y <- getQuery(NPflows_proj, "nutrient inputs by ag tech") %>%
  filter(year %in% ANALYSIS_YEARS) %>%
  separate(subsector, into = c("GCAM_subsector", "basin")) %>%
  group_by(scenario, region, basin, year, input) %>%
  summarise(input_Mt = sum(value), .groups = "drop")

N_inputs_Mt_R_GLU_Y <- filter(NP_inputs_Mt_R_GLU_Y, input == "N fertilizer") %>%
  select(scenario, region, basin, year, input_N_Mt = input_Mt)
P_inputs_Mt_R_GLU_Y <- filter(NP_inputs_Mt_R_GLU_Y, input == "P fertilizer") %>%
  select(-input)

# Harvested nutrients: the % of N and P in mass of crops harvested by GCAM region, crop, basin
crop_nutrient_contents_lower <- crop_nutrient_contents %>%
  mutate(crop = tolower(item)) %>%
  select(-item)

nutrient_contents_ctry <- L100.LDS_ag_prod_t %>%
  left_join(region_map, by = "iso") %>%
  left_join(basin_map, by = "GLU") %>%
  left_join(distinct(crop_mapping, crop, GTAP_crop, GCAM_commodity, GCAM_subsector),
            by = "GTAP_crop") %>%
  left_join(crop_nutrient_contents_lower, by = "crop") %>%
  drop_na(contentN)

N_contents_R_C_GLU <- nutrient_contents_ctry %>%
  mutate(totalN = value * contentN) %>%
  group_by(region, GCAM_commodity, GCAM_subsector, basin) %>%
  summarise(prod_t = sum(value),
            totalN = sum(totalN), .groups = "drop") %>%
  mutate(contentN = totalN / prod_t) %>%
  select(-prod_t, -totalN)

P_contents_R_C_GLU <- nutrient_contents_ctry %>%
  drop_na(contentP) %>%
  mutate(totalP = value * contentP) %>%
  group_by(region, GCAM_commodity, GCAM_subsector, basin) %>%
  summarise(prod_t = sum(value),
            totalP = sum(totalP), .groups = "drop") %>%
  mutate(contentP = totalP / prod_t) %>%
  select(-prod_t, -totalP)

# Crops are generally computed from a simple unweighted global median
# Fodder crops are not in the nutrient data. Fodder are assumed 1.25% though the values differ throughout the year,
# and the conventions on wet/dry in the FAO data are not clear https://fieldreport.caes.uga.edu/publications/B1350/alfalfa-management-in-georgia/
# Biomass contents are assumed to be 0.6% nitrogen of dry matter: https://www.osti.gov/servlets/purl/3008554
# Grass biomass harvests are typically later in the year after translocation of nitrogen to roots
default_N_contents <- N_contents_R_C_GLU %>%
  group_by(GCAM_commodity) %>%
  summarise(default_contentN = median(contentN)) %>%
  filter(!GCAM_commodity %in% c("FodderGrass", "FodderHerb")) %>%
  add_row(GCAM_commodity = "FodderGrass", default_contentN = 0.0125) %>%
  add_row(GCAM_commodity = "FodderHerb", default_contentN = 0.0125) %>%
  add_row(GCAM_commodity = "biomass", default_contentN = 0.006)

# biomass production is in EJ, so convert to biomass Mt for consistency with N content data
N_harvested_Mt_R_GLU_Y <- getQuery(NPflows_proj, "ag production by tech") %>%
  filter(!sector %in% c("Forest", "Pasture"),
         output == sector,
         year %in% ANALYSIS_YEARS) %>%
  mutate(value = if_else(sector == "biomass", value / 0.0175, value)) %>%
  separate(subsector, into = c("GCAM_subsector", "basin"), sep = "_") %>%
  left_join(N_contents_R_C_GLU, by = c("region", sector = "GCAM_commodity", "GCAM_subsector", "basin")) %>%
  left_join(default_N_contents, by = c(sector = "GCAM_commodity")) %>%
  mutate(contentN = if_else(is.na(contentN), default_contentN, contentN),
         harvested_N_Mt = value * contentN) %>%
  group_by(scenario, region, basin, year) %>%
  summarise(harvested_N_Mt = sum(harvested_N_Mt), .groups = "drop")

# Combine all elements and calculate the surplus
join_vars <- c("scenario", "region", "basin", "year")
N_surplus_Mt_R_GLU_Y <- BNF_Mt_R_GLU_Y %>%
  full_join(N_deposition_Mt_R_GLU_Y, by = join_vars) %>%
  full_join(N_inputs_Mt_R_GLU_Y, by = join_vars) %>%
  full_join(N_harvested_Mt_R_GLU_Y, by = join_vars) %>%
  mutate(N_surplus_Mt = BNF_Mt + N_deposition_Mt + input_N_Mt - harvested_N_Mt)

# calculate Nitrogen Use Efficiency 
NUE_R_GLU_Y <- N_surplus_Mt_R_GLU_Y %>%
  group_by(region, basin, year) %>%
  mutate(NUE = ((1-((N_surplus_Mt)/(BNF_Mt+N_deposition_Mt+input_N_Mt)))*100))

# Phosphorus calculations ----

# Crops are generally computed from a simple unweighted global median
# Fodder crops are not in the nutrient data. Fodder are assumed 1.25% though the values differ throughout the year,
# and the conventions on wet/dry in the FAO data are not clear https://fieldreport.caes.uga.edu/publications/B1350/alfalfa-management-in-georgia/
# Biomass contents are assumed to be 0.6% nitrogen of dry matter: https://www.osti.gov/servlets/purl/3008554
# Grass biomass harvests are typically later in the year after translocation of nitrogen to roots
default_P_contents <- P_contents_R_C_GLU %>%
  group_by(GCAM_commodity) %>%
  summarise(default_contentP = median(contentP)) %>%
  filter(!GCAM_commodity %in% c("FodderGrass", "FodderHerb")) %>%
  add_row(GCAM_commodity = "FodderGrass", default_contentP = 0.0125) %>%
  add_row(GCAM_commodity = "FodderHerb", default_contentP = 0.0125) %>%
  add_row(GCAM_commodity = "biomass", default_contentP = 0.006)

# biomass production is in EJ, so convert to biomass Mt for consistency with P content data
P_harvested_Mt_R_GLU_Y <- getQuery(NPflows_proj, "ag production by tech") %>%
  filter(!sector %in% c("Forest", "Pasture"),
         output == sector,
         year %in% ANALYSIS_YEARS) %>%
  mutate(value = if_else(sector == "biomass", value / 0.0175, value)) %>%
  separate(subsector, into = c("GCAM_subsector", "basin"), sep = "_") %>%
  left_join(P_contents_R_C_GLU, by = c("region", sector = "GCAM_commodity", "GCAM_subsector", "basin")) %>%
  left_join(default_P_contents, by = c(sector = "GCAM_commodity")) %>%
  mutate(contentP = if_else(is.na(contentP), default_contentP, contentP),
         harvested_P_Mt = value * contentP) %>%
  group_by(scenario, region, basin, year) %>%
  summarise(harvested_P_Mt = sum(harvested_P_Mt), .groups = "drop")      

# Combine all elements and calculate the surplus
join_vars_P <- c("scenario", "region", "basin", "year")
P_surplus_Mt_R_GLU_Y <- P_inputs_Mt_R_GLU_Y  %>%
  full_join(P_harvested_Mt_R_GLU_Y, by = join_vars) %>%
  mutate(P_surplus_Mt = input_Mt - harvested_P_Mt)

# calculate Nitrogen Use Efficiency 
PUE_R_GLU_Y <- P_surplus_Mt_R_GLU_Y %>%
  group_by(region, basin, year) %>%
  mutate(PUE = ((1-((P_surplus_Mt)/(input_N_Mt)))*100))
