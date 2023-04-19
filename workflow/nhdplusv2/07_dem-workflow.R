# DEM config
library(hydrofabric)
source('workflow/config.R')
base        = '/Volumes/Transcend/ngen/DEM-products/'
dem_dir     =  glue('{base}DEM/')
dem_cor_dir =  glue('{base}breeched/')
d8_dir      =  glue('{base}d8/')
sca_dir     =  glue('{base}sca/')
slope_dir   =  glue('{base}slope/')
twi_dir     =  glue('{base}twi/')
wb_rast_dir =  glue('{base}reference_waterbodies/')
fl_rast_dir =  glue('{base}reference_flowlines/')
rip_dir     =  glue('{base}50yr/')
fema_dir    =  glue('{base}100yr/')

dir.create(dem_dir,  showWarnings = FALSE)

dir.create(dem_cor_dir, showWarnings = FALSE)
dir.create(d8_dir    , showWarnings = FALSE)
dir.create(sca_dir   , showWarnings = FALSE)
dir.create(slope_dir , showWarnings = FALSE)
dir.create(twi_dir   , showWarnings = FALSE)

dir.create(wb_rast_dir, showWarnings = FALSE)
dir.create(glue("{wb_rast_dir}10m/"), showWarnings = FALSE)
dir.create(glue("{wb_rast_dir}30m/"), showWarnings = FALSE)

dir.create(fl_rast_dir, showWarnings = FALSE)
dir.create(glue("{fl_rast_dir}10m/"), showWarnings = FALSE)
dir.create(glue("{fl_rast_dir}30m/"), showWarnings = FALSE)

dir.create(rip_dir   , showWarnings = FALSE)
dir.create(fema_dir  , showWarnings = FALSE)

#####

RPU = nhdplusTools::rpu_boundaries[40,]

wb_here_10   = glue('{wb_rast_dir}10m/nhdplus_wb_{r$RPUID}.tif')
wb_here_30   = glue('{wb_rast_dir}30m/nhdplus_wb_{r$RPUID}.tif')
fl_here   = glue('{fl_rast_dir}nhdplus_fl_{r$RPUID}.tif')
fema_here = glue('{fema_dir}100yr_{r$RPUID}.tif')
rip_here  = glue('{rip_dir}50yr_{r$RPUID}.tif')

dem_here = glue('{dem_dir}Ned{r$RPUID}.tif')
breeched_here = glue('{dem_cor_dir}breeched_{r$RPUID}.tif')
d8_here = glue('{d8_dir}d8_{r$RPUID}.tif')
sca_here = glue('{sca_dir}sca_{r$RPUID}.tif')
slope_here = glue('{slope_dir}slope_{r$RPUID}.tif')
twi_here = glue('{twi_dir}twi_{r$RPUID}.tif')


r = sf::st_buffer(st_transform(RPU, 5070), 100) %>%
  st_transform(st_crs(RPU))

# Riparian  ---------------------------------------------------------------
# Choosing 10m target
if(!file.exists(rip_here)){
  tmp  = dap("/Volumes/Transcend/ngen/USFS_CONUS/usfs_riparian.tif", AOI =r)[[1]]
  xx   = project(t,tmp)
  tmp2 = mask(tmp, xx, filename = rip_here)
  writeRaster(tmp2, rip_here)
}

target_grid_10m = rast(rip_here)
target_grid_30m = rast(dem_here)

# Waterbodies -------------------------------------------------------------
# Need 10meter target and 30m target
# No buffer needed
if(!file.exists(wb_here)){

  wbs = read_sf(glue('{wb_dir}/NHDPlus{r$VPUID}.gpkg')) %>%
    filter(FTYPE %in% c('LakePond', 'Reservoir')) %>%
    vect() %>%
    project(crs(target_grid))

  rasterize(wbs,
            target_grid_10m,
            background = NA,
            feild = 1,
            filename = wb_here_10,
            overwrite = TRUE)

  rasterize(wbs,
            rast("data/dem.tif"),
            background = NA,
            feild = 1,
            filename = wb_here_30,
            overwrite = TRUE)
}


# FEMA 100 yr -------------------------------------------------------------
# Choosing 10m target
# No buffer needed
if(!file.exists(fema_here)){

  states =      st_filter(st_transform(AOI::aoi_get(state = "conus"), st_crs(r)), r)

  fema_dirs = glue("/Volumes/GIS_Johnson/NFHL/USA/{states$state_name}-100yr-flood_valid.shp")

  fema = bind_rows(lapply(fema_dirs, read_sf))

  rasterize(project(vect(fema), crs(target_grid)),
            target_grid,
            background = NA,
            feild = 1,
            filename = fema_here,
            overwrite = TRUE)
}



# Flowlines -------------------------------------------------------------
# Choosing 30m target
# No buffer needed
if(!file.exists(fl_here)){
  rasterize(project(vect(glue('{fl_dir}/NHDPlus{r$VPUID}.gpkg')), crs(t)),
            t,
            background = NA,
            feild = 1,
            filename = fl_here,
            overwrite = TRUE)
}


# Process -----------------------------------------------------------------

o = climateR::dap('/Volumes/Transcend/ngen/DEM-products/dem.vrt', AOI = r)

fs::file_copy(dem_here, "data/dem.tif", overwrite = TRUE)

if(!file.exists(dem_here)){
  stop("fail")
}

if(!file.exists(mask_here)){
  mask = rast(dem_here)
  mask[!is.na(mask)] = 1
  mask[mask != 1] = NA
}



if(!file.exists(breeched_here)){
  whitebox::wbt_breach_depressions("data/dem.tif", breeched_here)
}

if(!file.exists(slope_here)){
  whitebox::wbt_slope("data/dem.tif", output = slope_here, units = "degrees")
  #tmp = rast(slope_here)
  #tmp[tmp <= 0] = 1
  #writeRaster(tmp, filename = slope_here, overwrite = TRUE)

}

if(!file.exists(d8_here)){
  whitebox::wbt_d8_pointer(breeched_here, d8_here)
}

if(!file.exists(sca_here)){
  whitebox::wbt_d8_flow_accumulation(d8_here, sca_here,
                                     out_type = "Specific Contributing Area",
                                     pntr = TRUE)
}

if(!file.exists(twi_here)){
  whitebox::wbt_wetness_index(sca = sca_here,
                              slope = slope_here,
                              output = twi_here)

  tmp  = rast(twi_here)
  wb30 =rast(wb_here_30)

  tmp[wb30 == 1] = 30

  writeRaster(tmp, twi_here, overwrite = TRUE)

}


tmp = rast(twi_here)
wb30 =rast(wb_here_30)

tmp[wb30 == 1] = 30

writeRaster(tmp, twi_here, overwrite = TRUE)

whitebox::wbt_wetness_index(sca = "data/flowAccum.tif", slope = "data/slope_percent.tif", output = "data/twi.tif")





rasterized_shapefile <- rasterizeGeom(vect(river), sca, fun="length")
writeRaster(rasterized_shapefile, glue("{directory}/river.tif"), overwrite = TRUE)
x <- ifel(sca <= gully_threshold ,vel_gully,vel_overland)
x <- ifel(rasterized_shapefile == 1 ,vel_channel,x)
# This x matrix represents how many minutes it takes to travel 1 meter
x <- 1.0/(x*60)
names(x)  = "travel_time"

writeRaster(x, glue("{directory}/travel_time.tif") ,overwrite=TRUE)


whitebox::wbt_breach_depressions("data/dem.tif", "data/dem_corr.tif")
whitebox::wbt_d8_pointer("data/dem_corr.tif", "data/d8.tif")
whitebox::wbt_d8_flow_accumulation("data/d8.tif",
                                   "data/flowAccum.tif",
                                   out_type = "Specific Contributing Area",
                                   pntr = TRUE)
whitebox::wbt_slope(dem = "data/dem.tif", output = "data/slope_percent.tif", units = "degrees")
whitebox::wbt_wetness_index(sca = "data/flowAccum.tif", slope = "data/slope_percent.tif", output = "data/twi.tif")

mapview::mapview(xx2[1,], burst = TRUE)
