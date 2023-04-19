source('workflow/config.R')

files  = list.files(catchments_dir, full.names = TRUE, pattern = ".gpkg$")
out_geojson = gsub('gpkg', 'geojson', glue('{cleaned_dir}{gsub("NHDPlus", "cleaned_", basename(files))}'))
flowpath_files = list.files(reference_dir, pattern = "flowlines", full.names = TRUE)
out_geojson = gsub('gpkg', 'geojson', glue('{cleaned_dir}{gsub("NHDPlus", "cleaned_", basename(files))}'))
out_tmp  = glue("{simplified_dir}{gsub('.geojson', '', gsub('cleaned', 'catchments',basename(out_geojson)))}.geojson")
out_gpkg = gsub('geojson', "gpkg", out_tmp)

unlink(out_geojson); unlink(out_tmp); unlink(out_gpkg)

# Clean files -------------------------------------------------------------

for(i in 1:length(files)){

  if(!file.exists(out_gpkg[i])){
    catchments        = read_sf(files[i])
    names(catchments) = tolower(names(catchments))

    catchments = catchments %>%
      st_transform(4326) %>%
      clean_geometry(sys = TRUE, 'featureid', keep = NULL) %>%
      st_make_valid() %>%
      mutate(areasqkm = add_areasqkm(.)) %>%
      st_transform(5070) %>% 
      fix_mp_issues(
        flowlines = st_transform(read_sf(flowpath_files[i]), 5070),
        "featureid"
       ) %>%
         st_transform(4326)
    
    
    if(all(st_geometry_type(catchments) == "POLYGON") & all(st_is_valid(catchments))){
      
      unlink(out_geojson[i])
      write_sf(catchments, out_geojson[i])
      
      system(paste0('node  --max-old-space-size=16000 `which mapshaper` ',
                    out_geojson[i], ' -simplify ',
                    num, '% keep-shapes -o ',
                    out_tmp[i]))
      
      catchments = read_sf(out_tmp[i])
      
      if(all(st_geometry_type(catchments) == "POLYGON") & all(st_is_valid(catchments))){
        write_sf(catchments, out_gpkg[i])
      } else {
        stop("Invalids Created in Step 2")
      }
    } else {
      stop("Invalids Created in Step 1")
    }
  }
}


# Rectify borders ---------------------------------------------------------

for(i in 30:nrow(topos)){

  VPU1 = topos$VPU1[i]
  VPU2 = topos$VPU2[i]

  v_path_1 = find_file_path(VPU1, out_gpkg, reference_dir)
  v_path_2 = find_file_path(VPU2, out_gpkg, reference_dir)

  log_info('\tRead in touching pairs')

  v1 = read_sf(v_path_1) |>
    st_transform(5070) |>
    #st_make_valid() |>
    rename_geometry('geometry')

  v2 = read_sf(v_path_2) |>
    st_transform(5070) |>
    #st_make_valid() |>
    rename_geometry('geometry')

  log_info('\tIsolate "edge" catchments')
  old_1 = st_filter(v1, v2)
  old_2 = st_filter(v2, v1)

  log_info('\tErase fragments of OVERLAP')
  new_1 = st_difference(old_1, st_union(st_combine(old_2)))
  new_1 = st_filter(v1, new_1)
  new_2 = st_difference(old_2, st_union(st_combine(old_1)))
  new_2 = st_filter(v2, new_2)


  u1 = sfheaders::sf_remove_holes(st_union(st_make_valid(new_1))) %>% 
    nngeo::st_remove_holes()
  u2 = sfheaders::sf_remove_holes(st_union(st_make_valid(new_2)))%>% 
    nngeo::st_remove_holes()

  log_info('\tBuild Fragments')

  base_cats = bind_rows(new_1, new_2) %>%
    st_make_valid()

  base_union = sfheaders::sf_remove_holes(st_union(c(u1,u2))) %>%
    st_make_valid()

  frags = st_difference(base_union,
                        st_make_valid(st_union(st_combine(base_cats)))) |>
    st_cast("MULTIPOLYGON") |>
    st_cast("POLYGON") |>
    st_as_sf() |>
    mutate(id = 1:n()) %>%
    rename_geometry('geometry') |>
    st_buffer(.0001)

  out = tryCatch({
    suppressWarnings({
      st_intersection(frags, base_cats) %>%
        st_collection_extract("POLYGON")
    })
  }, error = function(e) { NULL })


  ints = out %>%
    mutate(l = st_area(.)) %>%
    group_by(id) %>%
    slice_max(l, with_ties = FALSE) %>%
    ungroup() %>%
    select(featureid, id, l) %>%
    st_drop_geometry()

  tj = right_join(frags,
                  ints,
                  by = "id") %>%
    bind_rows(base_cats) %>%
    dplyr::select(-.data$id) %>%
    group_by(featureid) %>%
    mutate(n = n()) %>%
    ungroup()

  in_cat = suppressWarnings({
    hydrofab::union_polygons(filter(tj, n > 1) , 'featureid') %>%
      bind_rows(dplyr::select(filter(tj, n == 1), featureid)) %>%
      mutate(tmpID = 1:n()) |>
      st_make_valid()
  })

  log_info('\tReassemble VPUS')

  inds = in_cat$featureid[in_cat$featureid %in% v1$featureid]

  to_keep_1 = bind_rows( filter(v1, !featureid %in% inds),
                         filter(in_cat, featureid %in% inds)) |>
    select(names(v1)) %>%
    mutate(areasqkm = hydrofab::add_areasqkm(.)) %>%
    nngeo::st_remove_holes()

  inds2 = in_cat$featureid[in_cat$featureid %in% v2$featureid]

  to_keep_2 = bind_rows( filter(v2, !featureid %in% inds2),
                         filter(in_cat, featureid %in% inds2)) |>
    select(names(v1)) %>%
    mutate(areasqkm = hydrofab::add_areasqkm(.)) %>%
    nngeo::st_remove_holes()

  log_info('\tWrite VPUS')
  write_sf(to_keep_1, v_path_1, "catchments", overwrite = TRUE)
  write_sf(to_keep_2, v_path_2, "catchments", overwrite = TRUE)

  log_info('Finished: ', i , " of ", nrow(topos))

}








