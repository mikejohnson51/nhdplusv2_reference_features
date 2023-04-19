all       = list.files(wb_dir, full.names = TRUE, pattern = ".gpkg$")

# WB_COMID == 166410600
# VPU13

# Waterbodies -------------------------------------------------------------

for(i in 1:length(all)){

  path = all[i]

  which_VPU = gsub(".gpkg", "", gsub("NHDPlus", "", basename(path)))

  outfile = glue("{reference_dir}waterbodies_{which_VPU}.gpkg")

  if(!file.exists(outfile)){

    wb  = read_sf(path)

    tmp = st_drop_geometry(wb)

    names(tmp) <- toupper(names(tmp))

    wb = st_zm(st_as_sf(cbind(tmp, st_geometry(wb))))

    xx = union_polygons(wb, "COMID") %>%
      st_transform(5070) %>%
      sf_remove_holes() %>%
      left_join(st_drop_geometry(wb), by = "COMID") %>%
      select(GNIS_ID, GNIS_NAME, COMID, FTYPE)

    xx1 = filter(xx, is.na(GNIS_NAME)) %>%
      mutate(member_comid = as.character(COMID))

    xx2 = filter(xx, !is.na(GNIS_NAME)) %>%
      mutate(area_sqkm = add_areasqkm(.))

    out = union_polygons(xx2, "GNIS_NAME") %>%
      left_join(st_drop_geometry(xx2), by = "GNIS_NAME") %>%
      group_by(GNIS_NAME) %>%
      mutate(member_comid = paste(COMID, collapse = ",")) %>%
      slice_max(area_sqkm) %>%
      ungroup() %>%
      bind_rows(xx1) %>%
      select(GNIS_ID, GNIS_NAME, COMID, FTYPE, member_comid) %>%
      mutate(area_sqkm = add_areasqkm(.))

    write_sf(out, outfile, "waterbodies")


  }

  logger::log_info("Finished VPU ", which_VPU, " waterbodies")

}

