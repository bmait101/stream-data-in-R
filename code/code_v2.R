# Working with stream data in R - 2022 WI AFS R Expo
# Bryan M Maitland  
# 9 Feb 2022


# Goals: 
# 1. Wrangle stream (lines), watershed (polygons), and site (points) data
# 2. Make some maps (ggplot2-based)
# 3. Spatially link study sites to attributed stream data
# 4. Get and plot USGS flow data


# Packages
pkgs <- c(
  "tidyverse",      # for pipe, readr, dplyr, ggplot2, purrr, etc.
  "here",           # a better way to find project files
  "janitor",        # for cleaning variable names
  "sf",             # for working with spatial data
  "nngeo",          # for nearest neighbor spatial joins
  "wdnr.gis",       # for spatial layers from WDNR ArcGIS Rest API
  "dataRetrieval"   # for USGS data pulls
  )

vapply(pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)

# Check working directior
here()
# Let's set the plotting theme
theme_set(theme_bw(base_family = "sans", base_size = 12))



# Load trout data  =============================================================

# read in trout data
df_trout <- readRDS(here("data", "df_cpes.rds"))

# make a spatially aware site location object
sf_sites <- df_trout %>% 
  distinct(site.seq.no, .keep_all = TRUE) %>% 
  sf::st_as_sf(
    coords = c("longitude", "latitude"), 
    crs = 4326
  ) %>% 
  st_transform(crs = 3071)  # NAD83(HARN) / Wisconsin Transverse Mercator


# load and set coords for a WI polygon
sf_wi_poly <- wdnr.gis::wi_poly %>% 
  st_transform(crs = 3071) 


# plot site locations
ggplot() +
  geom_sf(data = sf_wi_poly, color = "black", fill = "grey") +
  geom_sf(data = sf_sites, 
          shape = 21, size = 1, color = "black", fill = "blue", alpha = 0.5) + 
  labs(title = "Trout surveys sites")


# let's focus in on the Kickapoo


# Spatial data  ================================================================

# We'll use wdnr.gis package for pulling WHD24k layers -------------------------

# get a vector of huc8 codes
huc8_codes <- wdnr.gis::watershed_lookup %>% 
  filter(huc_codes == "07070006") %>%  
  pull(huc_codes)

# get a vector of huc10 codes
huc10_codes <- wdnr.gis::watershed_lookup %>% 
  filter(huc_level == "HUC_10") %>% 
  filter(str_detect(huc_codes, "^07070006")) %>%  
  pull(huc_codes)


# pull huc8 layer
huc8_poly <-
  get_watershed_layer(watershed_code = huc8_codes) %>%
  clean_names() %>% 
  st_transform(crs = 3071) 

# loop (map) same function to get all huc10s
huc10_polys <- huc10_codes %>%  
  map_df(~get_watershed_layer(watershed_code = .x)) %>%
  clean_names() %>% 
  st_transform(crs = 3071) 


# get flowlines for the kickapoo (huc8)
lines_kpoo <- get_hydro_layer(
  watershed_code = huc8_codes,
  layer_type = "flowlines"
  ) %>% 
  clean_names() %>% 
  st_transform(crs = 3071) 


# Clip sites to Kickapoo and filter trout data ---------------------------------

# clip sites layer by kpoo poly
sf_sites_kpoo <- sf_sites %>% 
  st_intersection(huc8_poly) 

# filter trout daterz
df_trout_kpoo <- df_trout %>% 
  filter(site.seq.no %in% sf_sites_kpoo$site.seq.no)




# Plot watersheds, streams, and sites ==========================================

# plot
ggplot() +
  geom_sf(data = huc8_poly, color = "black", alpha = 0) +
  geom_sf(data = huc10_polys, fill = "white", color = "black", alpha = 0) +
  geom_sf(data = lines_kpoo %>% filter(stream_order > 1), color = "blue") +
  geom_sf(data = sf_sites_kpoo, 
          shape = 21, size = 2, color = "black", fill = "red", alpha = 0.5) + 
  labs(title = "W. Fk. Kickapoo") + 
  scale_x_continuous(breaks = seq(-91.0, -90.4, .2))





# Get watershed-derived stream attributes from WHDPlus =========================

# The WHD24kPLUS GDB contains lines attributed with various watershed data
# (available online)

# List layers in 24kWHDPlus VA and Baselayer
st_layers(dsn = here("data", "WDNR_HYDRO_24K_VA.gdb"))

# load some WHDPlus attribute data
df_whd_attr <- 
  st_read(
    dsn = here("data", "WDNR_HYDRO_24K_VA.gdb"), 
    layer = "WD_HYDRO_VA_BASE_ATTR_REF") %>% 
  janitor::clean_names() %>% 
  as_tibble() 

# Lets keep just a few commonly used metrics
df_whd_attr <- df_whd_attr %>% 
  select(reachid, sinuosity, max_elev_fix,
         gradient, c_length, c_eco, stream_order)

# check it
glimpse(df_whd_attr)



# How do we link these data back to our fish/site data????

### ReachIDs is key ###



# Spatially xref sites with WHD lines for reachids =============================

# load whd line segments
lines_whd_va <-
  st_read(
    dsn = here("data", "WDNR_HYDRO_24K_VA.gdb"), 
    layer = "WD_HYDRO_VA_FLWLN_NTWRK_LN_24K"
  ) %>% 
  st_as_sf() %>%
  st_transform(crs = 3071) %>% 
  janitor::clean_names()

# check it
glimpse(lines_whd_va)


# clip statewide lines to Kickapoo huc8 polygon
lines_whd_va_kpoo <- lines_whd_va %>% 
  st_intersection(huc8_poly) 


# join sites to flowlines using the nearest neighbor
sf_sites_kpoo_va <- sf_sites_kpoo %>% 
  # first remove uneeded/dup columns
  select(-objectid, -huc8_code, -huc8_name) %>%  
  # do the join
  st_join(
    lines_whd_va_kpoo, 
    join = nngeo::st_nn, 
    k = 1
    ) %>% 
  # remove new uneeded columns
  select(-seedtype, -confl2conflid, -traceid, -shape_length, -objectid)


# Let's also attribute our site data with their huc 10s
sf_sites_kpoo_va <- sf_sites_kpoo_va %>% 
  st_join(huc10_polys, join = st_within) %>% 
  select(-objectid)


# drop the geometry and just keep the key columns for linking data
xref <- sf_sites_kpoo_va %>% 
  st_drop_geometry() %>% 
  select(site.seq.no, hydroid, reachid, huc10_code, huc10_name)



# Link it all up to our trout data ---------------------------------------------


df_trout_kpoo_va <- df_trout_kpoo %>% 
  # link the reachids and huc codes
  left_join(xref, by = "site.seq.no") %>% 
  # use reachids to link in the attrbute data
  left_join(df_whd_attr, by = "reachid")

# check it
glimpse(df_trout_kpoo_va)

# Nice! 




# Let's make some plots using our attributed data ==============================

# How about plotting mean age-1+ CPEs by huc10 subwatersheds
df_trout_kpoo_va %>% 
  
  # filter data
  filter(yoy == "N", survey.year <= 2019) %>% 
  
  # summarize data
  group_by(survey.year, huc10_name, species) %>%
  summarise(
    mean_cpe = mean(cpe, na.rm = TRUE),
    se = sd(cpe)/sqrt(length(cpe)),
    .groups = "drop"
  ) %>%
  complete(survey.year, huc10_name, species) %>% 
  
  # plot it
  ggplot(aes(x = survey.year, y = mean_cpe, color = species)) +
  facet_wrap(vars(huc10_name), nrow = 2, scales="free") +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin=mean_cpe-se, ymax=mean_cpe+se), width=.1) +
  geom_line(aes(linetype=species), size = .75) +
  scale_x_continuous(breaks = seq(1995,2020,5), limits = c(1995, 2020)) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  labs(x = "Year", y = expression(CPE~(fish~mile^{-1}))) + 
  guides(linetype = 'none') + 
  theme(legend.position = "top")


# How about by stream order?
df_trout_kpoo_va %>% 
  
  # filter data
  filter(yoy == "N", survey.year <= 2019, between(stream_order,2,4)) %>% 
  
  # make stream order a factor
  mutate(stream_order = as_factor(stream_order)) %>% 
  
  # summarize data
  group_by(survey.year, stream_order, species) %>%
  summarise(
    mean_cpe = mean(cpe, na.rm = TRUE),
    se = sd(cpe)/sqrt(length(cpe)),
    .groups = "drop"
  ) %>%
  complete(survey.year, stream_order, species) %>% 
  
  # plot it
  ggplot(aes(x = survey.year, y = mean_cpe, color = species)) +
  facet_wrap(vars(stream_order), nrow = 3, scales="free") +
  geom_point(size = 1) +
  geom_errorbar(aes(ymin=mean_cpe-se, ymax=mean_cpe+se), width=.1) +
  geom_line(aes(linetype=species), size = .75) +
  scale_x_continuous(breaks = seq(1995,2020,5), limits = c(1995, 2020)) +
  # scale_y_continuous(limits = c(0,1300), breaks = seq(0,1300,250)) + 
  scale_color_brewer(palette = "Set1", direction = -1) +
  labs(x = "Year", y = expression(CPE~(fish~mile^{-1}))) + 
  guides(linetype = 'none') + 
  theme(legend.position = "top")




# 4. USGS gage data ============================================================


# find available stations and data and tidy it, make it spatial --------------
df_usgs_sites <- 
  # pull available data
  whatNWISdata(
    huc = "07070006",  #  HUC8 Kickapoo
    siteType = "ST",   # data for streams/rivers
    service = "dv",    # daily value data
    parameterCd = c(
      "00010",  # temperature data
      "00060"   # discharge (flow) data
    ),
    statCd = c(
      "00001",  # max
      "00002",  # min
      "00003"   # mean
    )  
  ) %>%
  # rename the data type codes
  mutate(
    parm_cd = case_when(
      parm_cd == "00010" ~ "Temperature", 
      parm_cd == "00060" ~ "Streamflow"), 
    stat_cd = case_when(
      stat_cd == "00001" ~ 'Max', 
      stat_cd == "00002" ~ 'Min',
      stat_cd == "00003" ~ 'Mean'),
    span = 
      lubridate::interval(begin_date, end_date) %>% 
      lubridate::time_length(unit = "year") %>% 
      round(digits = 0)
  ) %>% 
  # keep only stations with end dates after 1990
  filter(end_date >= "1990-01-01") %>% 
  # make spatial object
  st_as_sf(
    coords = c("dec_long_va", "dec_lat_va"), 
    crs = 4326
  )


# check it 
glimpse(df_usgs_sites)


# plot gage stations and color sites by their period of record
ggplot() +
  geom_sf(data = huc8_poly, color = "black", alpha = 0) +
  geom_sf(data = huc10_polys, fill = "white", color = "black", alpha = 0) +
  geom_sf(data = lines_kpoo %>% filter(stream_order > 1), color = "blue") +
  # add usgs sites
  geom_sf(data = df_usgs_sites, 
          aes(fill = as_factor(span)),
          shape = 21, size = 5, color = "black") +
  # label sites
  ggrepel::geom_label_repel(
    data = df_usgs_sites %>% select(site_no, geometry),
    aes(label = site_no, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,  
    max.overlaps = Inf, 
    box.padding = 0.5, 
    segment.color = 'black',
    segment.size = 1,
    arrow = arrow(length = unit(0.01, 'npc')),
  ) + 
  scale_fill_viridis_d(direction = -1) +
  scale_x_continuous(breaks = seq(-91.0, -90.4, .2)) + 
  labs(title = "USGS flow sites", x ="", y = "", fill = "Time span")

# nowplot and color just by site
p.usgs.sites <- ggplot() +
  geom_sf(data = huc8_poly, color = "black", alpha = 0) +
  geom_sf(data = huc10_polys, fill = "white", color = "black", alpha = 0) +
  geom_sf(data = lines_kpoo %>% filter(stream_order > 1), color = "blue") +
  # add usgs sites
  geom_sf(data = df_usgs_sites, 
          aes(fill = as_factor(site_no)),
          shape = 21, size = 5, color = "black") +
  # label sites
  ggrepel::geom_label_repel(
    data = df_usgs_sites %>% select(site_no, geometry),
    aes(label = site_no, geometry = geometry, color = site_no),
    stat = "sf_coordinates",
    min.segment.length = 0,  
    max.overlaps = Inf, 
    box.padding = 0.5, 
    segment.color = 'black',
    segment.size = 1,
    arrow = arrow(length = unit(0.01, 'npc')),
    ) + 
  scale_fill_viridis_d(direction = -1) +
  scale_color_viridis_d(direction = -1) +
  scale_x_continuous(breaks = seq(-91.0, -90.4, .2)) + 
  labs(title = "USGS gage sites", x ="", y = "", fill = "Site ID") + 
  guides(color = 'none')
p.usgs.sites

# pull the flow data --------------------------

# list of site numbers
usgs_sites <- df_usgs_sites %>% 
  pull(site_no)

df_usgs_flow <- 
  readNWISdv(usgs_sites, "00060") %>% 
  renameNWISColumns() %>% 
  clean_names() %>% 
  select(-ends_with("_cd")) %>% 
  as_tibble()

# Plot flow over time
p.usgs.flows <- df_usgs_flow %>% 
  ggplot(aes(x = date, y = flow, color = site_no)) +
  facet_wrap(vars(site_no), scales = "free") + 
  geom_line(size=1.5) + 
  scale_color_viridis_d(direction = -1) +
  labs(x = "Year", y = "Flow (cfs)",
       title = "Daily streamflow over time") 
p.usgs.flows


# Put em toegether?
library(patchwork)

p.usgs.sites | p.usgs.flows + guides(color = 'none')
