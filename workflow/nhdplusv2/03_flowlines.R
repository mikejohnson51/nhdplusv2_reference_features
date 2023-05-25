source('workflow/nhdplusv2/config.R')

fl_paths  = list.files(fl_dir,  full.names = TRUE)
ble_paths = list.files(ble_dir, full.names = TRUE)

new_atts = read_parquet(glue("{base_dir}/enhd_nhdplusatts.parquet"))
vaa = get_vaa()
par = 3

# ble = read_sf('/Volumes/Transcend/ngen/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb',
#               "BurnLineEvent")
# 
# ble2 = ble %>%
#   st_zm() %>%
#   filter(!st_is_empty(.))
# 
# nhdflag = vaa %>% filter(startflag == 1 | divergence == 2) %>%
#   select(COMID = comid, vpuid)
# 
# ble3 = left_join(nhdflag, ble2, by = "COMID") %>%
#   st_as_sf() %>% 
#   filter(!st_is_empty(.))
# 
# ble4 = select(ble3, COMID, vpuid) %>% 
#   st_cast("LINESTRING")
# 
# write_sf(ble4, "/Volumes/Transcend/reference_geometries/ble_events.gpkg")

system.time({
  b = read_sf('/Volumes/Transcend/reference_geometries/ble_events.gpkg')
})



for(i in 1:length(fl_paths)){

  fl_path   = fl_paths[i]
  which_VPU = gsub(".gpkg", "", gsub("NHDPlus", "", basename(fl_path)))
  #ble_path  = ble_paths[grep(which_VPU, basename(ble_paths))]
  outfile   = glue("{reference_dir}flowlines_{which_VPU}.gpkg")

  if(!file.exists(outfile)){
    
    nhd <- st_zm(read_sf(fl_path)) %>%
      align_nhdplus_names() %>%
      left_join(vaa, by = c("COMID" = "comid")) %>%
      select(COMID, fromnode, tonode, startflag, streamcalc, divergence, dnminorhyd) %>%
      left_join(new_atts, by = c("COMID" = "comid")) %>%
      align_nhdplus_names() %>%
      mutate(LENGTHKM  = add_lengthkm(.))

    # ble <- left_join(select(st_drop_geometry(nhd), COMID), ble, by = "COMID") %>%
    #   st_as_sf() %>%
    #   st_zm()
    
    #sum(!st_is_empty(st_geometry(ble)))
    
    ble = filter(b, COMID %in% nhd$COMID)
    
    matcher = match(ble$COMID, nhd$COMID)
    
    st_geometry(nhd)[matcher] <- st_geometry(ble)

    custom_net <- st_drop_geometry(nhd) %>%
      select(COMID, FromNode, ToNode, Divergence) %>%
      nhdplusTools::get_tocomid(remove_coastal = FALSE) %>%
      select(comid, override_tocomid = tocomid)

    nhd <- left_join(nhd, custom_net, by = c("COMID" = "comid")) %>%
      mutate(override_tocomid = ifelse(toCOMID == 0, override_tocomid, toCOMID))

    # is a headwater and for sure flows to something.
    check <- !nhd$COMID %in% nhd$override_tocomid &
      !(nhd$override_tocomid == 0 | is.na(nhd$override_tocomid) |
          !nhd$override_tocomid %in% nhd$COMID)

    check_direction <- filter(nhd, check)

    if(!all(check_direction$override_tocomid[check_direction$override_tocomid != 0] %in% nhd$COMID)){
      stop("this won't work")
    }

    matcher <- match(check_direction$override_tocomid, nhd$COMID)

    matcher <- nhd[matcher, ]

    fn_list <- Map(list,
                   split(check_direction, seq_len(nrow(check_direction))),
                   split(matcher, seq_len(nrow(matcher))))

    cl <- makeCluster(par)

    # check and fix these.
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

    stopCluster(cl)

    error_index <- sapply(new_geom, inherits, what = "try-error")
    errors <- filter(nhd, COMID %in% check_direction$COMID[error_index])
    check[which(nhd$COMID %in% errors$COMID)] <- FALSE

    if(!all(sapply(st_geometry(errors), st_is_empty))) {
      stop("Errors other than empty geometry from fix flowdir")
    }

    st_geometry(nhd)[check] <- do.call(c, new_geom[!error_index])

    filter(nhd, COMID %in% new_atts$comid) %>%
      select( -override_tocomid) %>%
      write_sf(outfile, "flowlines")
  }

  logger::log_info("Finished VPU ", which_VPU, " flowlines")
}
