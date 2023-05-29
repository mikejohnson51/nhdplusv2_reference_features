
# The purpose of this script is to process flowline data and improve network connectivity by using Burn Line Events flowlines and nhdPlusTools 

# Summary:
#   
#   1. Read and prepare the flowline data:
  # - Read the flowline data from a file path specified by `fl_path`.
  # - Perform necessary transformations: convert the data to an sf object, add Z and M dimensions (st_zm), and align the names (align_nhdplus_names).
  # - Join the flowline data with two other datasets (`vaa` and `new_atts`) based on the common identifier `COMID`.
  # - Select specific columns of interest from the joined data (COMID, fromnode, tonode, startflag, streamcalc, divergence, dnminorhyd).
  # - Perform another alignment of names.
  # - Compute a new column called LENGTHKM using the `add_lengthkm()` function.
# 
# 2. Filter and match COMIDs:
#   - Filter the `b` burn line events dataset to include only matching COMIDs found in the `nhd` data, creating the `ble` object.
#   - Find the matching indices of COMIDs between `ble` and `nhd`, storing the result in the `matcher` variable.
# 
# 3. Update geometry using matching indices:
#   - Replace the geometry in the `nhd` object with the geometry from `ble` at the matching indices, ensuring connectivity is corrected.
# 
# 4. Generate a custom network and update `override_tocomid`:
#   - Create a custom network (`custom_net`) based on the `nhd` data by selecting relevant columns (COMID, FromNode, ToNode, Divergence).
#   - Use the `get_tocomid()` function from the `nhdplusTools` package to generate a corrected `override_tocomid` column, considering node connections.
#   - Update the `nhd` data by performing a left join with `custom_net` based on matching COMIDs.
#   - Update the `override_tocomid` values in `nhd`, replacing them with values from the `toCOMID` column if `toCOMID` is not equal to 0. Otherwise, keep the original `override_tocomid` value.
# 
# CONCLUSION: read and processes flowline data
# --> aligns and joins relevant datasets
# --> corrects network connectivity issues by updating the geometry and `override_tocomid` values
# --> generates a custom network based on the updated data. 
# --> The goal is to ensure accurate representation and connectivity of river segments in the `nhd` object.

source('workflow/nhdplusv2/config.R')

fl_paths  = list.files(fl_dir,  full.names = TRUE)
ble_paths = list.files(ble_dir, full.names = TRUE)

new_atts = read_parquet(glue("{base_dir}/enhd_nhdplusatts.parquet"))

tryCatch({
  
  vaa <- nhdplusTools::get_vaa()
  
}, error = function(e) {
  
  message("Error caught: ", conditionMessage(e))
  
  # Attempt to unlink the file
  unlink(nhdplusTools::get_vaa_path())
  
  # Retry getting VAA
  vaa <- nhdplusTools::get_vaa()
})

par = 3

# BLE geopackage file path
output_file <- glue("{base_dir}/ble_events.gpkg")

# create Burn Line Events geopackage if it doesn't exist
if (!file.exists(output_file)) {
  
  # read in BurnLineEvents from NHD geodatabase
  ble <- read_sf(nhdplus_ble_path, "BurnLineEvent")
  # ble <- read_sf('/Volumes/Transcend/ngen/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb', "BurnLineEvent")

  # filter out empty geometries
  ble <- 
    ble %>%
    st_zm() %>%
    filter(!st_is_empty(.))

  # getting starting and divergence flags from NHD VAAs
  nhdflag <- 
    vaa %>%
    filter(startflag == 1 | divergence == 2) %>%
    select(COMID = comid, vpuid)
  
  # join start/divergence flags with BLE by COMID
  ble <- 
    nhdflag %>% 
    left_join(
      ble, 
      by = "COMID"
      ) %>%
    st_as_sf() %>%
    filter(!st_is_empty(.))
  
  # select COMID and VPU from BLE and cast to LINESTRING
  ble <- 
    ble %>% 
    select( COMID, vpuid) %>%
    st_cast("LINESTRING")
  
  # save out BLE geopackage
  write_sf(ble, output_file)
  
  rm(ble)
  
}

# read in Burn Line Events geopackage
system.time({
  b <- read_sf(output_file)
})

# i = 8

# Loop through each file path in fl_paths
for(i in 1:length(fl_paths)){

  # Get the current file path
  fl_path   = fl_paths[i]
  
  # Extract the VPU identifier from the file path
  which_VPU = gsub(".gpkg", "", gsub("NHDPlus", "", basename(fl_path)))
  #ble_path  = ble_paths[grep(which_VPU, basename(ble_paths))]
  
  # Define the output file path for the processed flowlines
  outfile   = glue("{reference_dir}flowlines_{which_VPU}.gpkg")

  # Check if the output file does not exist
  if(!file.exists(outfile)){
    
    
    # Read flowlines from file, join with VAA and new_atts
    nhd <- 
      fl_path %>%
      read_sf() %>% 
      st_zm() %>%
      align_nhdplus_names() %>%
      left_join(
        vaa, 
        by = c("COMID" = "comid")
        ) %>%
      select(COMID, fromnode, tonode, startflag, streamcalc, divergence, dnminorhyd) %>%
      left_join(
        new_atts, 
        by = c("COMID" = "comid")
        ) %>%
      align_nhdplus_names() %>%
      mutate(LENGTHKM  = add_lengthkm(.))
    
    # goi <- "1285672"
    # nhd %>% 
    #   dplyr::filter(gnis_id == goi) %>% 
    #   # .$geom %>% 
    #   plot()
    
    # Filter the b burn line events dataset on matching COMIDs in 'nhd'
    ble = filter(b, COMID %in% nhd$COMID)

    # Find the matching indices of COMIDs between 'ble' and 'nhd'
    matcher = match(ble$COMID, nhd$COMID)

    # Replace the geometry in nhd geometry with ble geometries at the matching indices
    st_geometry(nhd)[matcher] <- st_geometry(ble)
    
    # Generate a custom network based on 'nhd' data, and create a to override_tocomid column from get_tocomid() function
    custom_net <- 
      nhd %>% 
      st_drop_geometry() %>%
      select(COMID, FromNode, ToNode, Divergence) %>%
      nhdplusTools::get_tocomid(remove_coastal = FALSE) %>%
      select(comid, override_tocomid = tocomid)
    
    # left join nhd w/ custom_net on matching COMIDs, updating override_tocomid
    nhd <-
      nhd %>% 
      left_join(
        custom_net,
        by = c("COMID" = "comid")
        ) %>%
      mutate(override_tocomid = ifelse(toCOMID == 0, override_tocomid, toCOMID))

    # is a headwater and for sure flows to something,  where COMID is not in override_tocomid
    check <- !nhd$COMID %in% nhd$override_tocomid &
      !(nhd$override_tocomid == 0 | is.na(nhd$override_tocomid) |
          !nhd$override_tocomid %in% nhd$COMID)
    
    # filter nhd based on the check condition above
    check_direction <- filter(nhd, check)
    
    # Check not all of non 0 override_tocomid appear in nhd COMIDs, throw an error
    if(!all(check_direction$override_tocomid[check_direction$override_tocomid != 0] %in% nhd$COMID)){
      stop("this won't work")
    }
    
    # Find matching indices of override_tocomid in nhd and check_direction
    matcher <- match(check_direction$override_tocomid, nhd$COMID)
    
    # Get the matched rows from nhd
    matcher <- nhd[matcher, ]
    
    # Create a list combining check_direction and matcher
    fn_list <- Map(list,
                   split(check_direction, seq_len(nrow(check_direction))),
                   split(matcher, seq_len(nrow(matcher))))
    
    # make cluster of parallel workers
    cl <- makeCluster(par)

    # check and fix these, applies fix_flowdir() in parallel to each element of fn_list 
    new_geom <- pblapply(
      cl = cl,
      X = fn_list,
      FUN = function(x) {
        nhdplusTools::fix_flowdir(x[[1]]$COMID,
                                  fn_list = list(
                                    flowline  = x[[1]],
                                    network   = x[[2]],
                                    check_end = "end"
                                  ))
      }
    )
    
    # stop parallel cluster     
    stopCluster(cl)
    
    # check for errors based on try-error conditions then update check as needed
    error_index <- sapply(new_geom, inherits, what = "try-error")
    errors <- filter(nhd, COMID %in% check_direction$COMID[error_index])
    
    # update check as needed
    check[which(nhd$COMID %in% errors$COMID)] <- FALSE
    
    # Check for errors other than empty geometry errors
    if(!all(sapply(st_geometry(errors), st_is_empty))) {
      stop("Errors other than empty geometry from fix flowdir")
    }
    
    # Replace the geometry in 'nhd' with the corrected geometries
    st_geometry(nhd)[check] <- do.call(c, new_geom[!error_index])
    
    fn_list[[1]][[1]]$geom%>% 
      plot()
    fn_list[[1]][[2]]$geom %>% 
      plot()
    
    chk <- fn_list[[1]][[1]]
    mtch <- fn_list[[1]][[2]]
    
    df <- dplyr::bind_rows(chk, mtch)
    
    df %>% 
      names()
    df <- df %>% 
      dplyr::select(COMID, override_tocomid, FromNode, ToNode, StartFlag, Divergence)
    
    fixed_df <- 
      nhd %>% 
      dplyr::filter(COMID %in% df$COMID) %>% 
      dplyr::select(COMID, override_tocomid, FromNode, ToNode, StartFlag, Divergence)
    mapview::mapview(fixed_df, color = "red") + df
    mapview::mapview(fn_list[[1]][[1]]$geom) + fn_list[[1]][[2]]$geom
    
    # Filter nhd on COMIDs in new_atts and save out result to output file
    nhd %>% 
      filter(COMID %in% new_atts$comid) %>%
      select( -override_tocomid) %>%
      write_sf(outfile, "flowlines")
  }

  logger::log_info("Finished VPU ", which_VPU, " flowlines")
}
