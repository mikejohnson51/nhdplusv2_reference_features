source('workflow/nhdplusv2/config.R')

files  = list.files(catchments_dir, full.names = TRUE, pattern = ".gpkg$")
out_geojson = gsub('gpkg', 'geojson', glue('{cleaned_dir}{gsub("NHDPlus", "cleaned_", basename(files))}'))
flowpath_files = list.files(reference_dir, pattern = "flowlines", full.names = TRUE)
out_geojson = gsub('gpkg', 'geojson', glue('{cleaned_dir}{gsub("NHDPlus", "cleaned_", basename(files))}'))
out_tmp  = glue("{simplified_dir}{gsub('.geojson', '', gsub('cleaned', 'catchments',basename(out_geojson)))}.geojson")
out_gpkg = gsub('geojson', "gpkg", out_tmp)

unlink(out_geojson); unlink(out_tmp); unlink(out_gpkg)
# i = 8

# Clean files -------------------------------------------------------------

for(i in 1:length(files)){

  if(!file.exists(out_gpkg[i])){
    
      # read in catchments
     catchments        = read_sf(files[i])
    
     # make names lowercase
     names(catchments) = tolower(names(catchments))
     
     # clean geometries, dissolve internal bounds
     out = hydrofab::clean_geometry(
                         catchments = catchments,
                         ID         = "featureid",
                         keep       = num,
                         sys        = TRUE
                         )
     
      message("\t--- Writing Catchments")
      write_sf(out, out_gpkg[i])
   }
  
      message("Finished ", i, " of ", length(files))
}

# Rectify borders ---------------------------------------------------------

for(i in 1:nrow(topos)){

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
    mutate(areasqkm = add_areasqkm(.)) %>%
    st_remove_holes()

  inds2 = in_cat$featureid[in_cat$featureid %in% v2$featureid]

  to_keep_2 = bind_rows( filter(v2, !featureid %in% inds2),
                         filter(in_cat, featureid %in% inds2)) |>
    select(names(v1)) %>%
    mutate(areasqkm = add_areasqkm(.)) %>%
    nngeo::st_remove_holes()

  log_info('\tWrite VPUS')
  write_sf(to_keep_1, v_path_1, "catchments", overwrite = TRUE)
  write_sf(to_keep_2, v_path_2, "catchments", overwrite = TRUE)

  log_info('Finished: ', i , " of ", nrow(topos))

}

#######################################################################
#######################################################################
#######################################################################
# 
# clean_geometry <- function(catchments,
#                            ID = "ID",
#                            keep = NULL,
#                            crs = 5070,
#                            grid = .0009,
#                            sys = NULL
#                            ) {
#   
#   # Check to make sure keep is valid 
#   if (keep < 0 || keep > 1) {
#     
#     stop("Invalid value for 'keep'. 'keep' must be > 0 and <= 1")
#     
#   } 
# 
#   # keep an original count of # rows in catchments
#   MASTER_COUNT = nrow(catchments)
#   
#   # use system mapshaper or not
#   if(Sys.getenv("TURN_OFF_SYS_MAPSHAPER") == "YUP")  { sys <- FALSE }
#   
#   if(is.null(sys)) {
#     sys <- FALSE
#     try(sys <- is.character(check_sys_mapshaper(verbose = FALSE)))
#   }
#   
#   # set crs variable to crs of catchments
#   if (!is.null(crs)) {  
#     crs = st_crs(catchments)  
#     }
#   
#   catchments = lwgeom::st_snap_to_grid(
#     st_transform(catchments, 5070), 
#     size = grid
#     )
#   
#   # cast MPs to POLYGONS and add featureid count column
#   polygons = suppressWarnings({
#     catchments %>% 
#       st_cast("POLYGON") %>% 
#       fast_validity_check() %>% 
#       add_count(!!sym(ID)) %>% 
#       mutate(areasqkm = add_areasqkm(.), tmpID = 1:n()) %>% 
#       rename_geometry("geometry") 
#   })
#   
#   
#   if(any(st_geometry_type(polygons) != "POLYGON") | nrow(polygons) != MASTER_COUNT){
#     
#     # separate polygons with more than 1 feature counts
#     extra_parts = filter(polygons, n != 1) 
#     
#     # dissolve, and explode if necessary
#     try(
#       extra_parts <- ms_explode(ms_dissolve(extra_parts, ID, copy_fields = names(extra_parts))
#       ), 
#       silent = TRUE
#     )
#     
#     # recalculate area
#     extra_parts <- mutate(
#       extra_parts, 
#       areasqkm = add_areasqkm(extra_parts), 
#       newID    = row_number(desc(areasqkm))
#       ) 
#     
#     # get the biggest parts by area in each catchment and bind with rest of good_to_go catchments
#     main_parts <- 
#       extra_parts %>% 
#       group_by(.data[[ID]]) %>% 
#       slice_max(areasqkm, with_ties = FALSE) %>% 
#       ungroup() 
#     
#     # current filter uses "!=" 
#     small_parts <- 
#       extra_parts %>% 
#       filter(newID != main_parts$newID)
#     
#     # suggested change to filter using ! and %in%
#     small_parts <- 
#       extra_parts %>% 
#       filter(!newID %in% main_parts$newID)
#     
#     if(!sum(nrow(main_parts)) + nrow(filter(polygons, n == 1)) == MASTER_COUNT){ 
#       stop() 
#       }
#     
#     main_parts = bind_rows(main_parts, filter(polygons, n == 1))
#     
#     if(nrow(small_parts) > 0) {
#       # dissolve, and explode if necessary
#       small_parts <- tryCatch(
#         ms_explode(
#           ms_dissolve(
#             small_parts, ID, copy_fields = names(small_parts)
#             )
#         ), error = function(e){ NULL}, warning = function(w){
#           NULL 
#           }
#       )
#       
#       # add area
#       small_parts = mutate(
#         small_parts, 
#         areasqkm = add_areasqkm(small_parts),
#         newID    = 1:n()
#       ) %>% 
#         select(newID)
#       
#       # get the intersection between big parts and small parts and pull out the LINESTRINGs
#       out = tryCatch({
#         suppressWarnings({
#           st_intersection(small_parts, st_make_valid(main_parts)) %>%
#             st_collection_extract("LINESTRING")
#         })
#       }, error = function(e) {
#         NULL
#       })
#       
#       ints <-
#         out %>%
#         mutate(l = st_length(.)) %>%
#         group_by(newID) %>%
#         slice_max(l, with_ties = FALSE) %>%
#         ungroup()
#       
#       tj = right_join(
#         small_parts,
#         select(st_drop_geometry(ints), !!ID, newID),
#         by = "newID"
#       ) %>%
#         bind_rows(main_parts) %>%
#         select(-areasqkm, -tmpID, -newID) %>%
#         group_by(.data[[ID]]) %>%
#         mutate(n = n()) %>%
#         ungroup() %>%
#         rename_geometry('geometry')
#       
#       in_cat <- 
#         union_polygons(
#           filter(tj, .data$n > 1),
#           ID
#         ) %>% 
#         bind_rows(
#           select(
#             filter(tj, .data$n == 1), 
#             !!ID)
#         ) %>%
#         mutate(tmpID = 1:n()) %>% 
#         fast_validity_check()
#       
#     } else {
#       in_cat = fast_validity_check(main_parts)
#     }
#     
#     
#     if(all(st_is_valid(in_cat)) & all(st_geometry_type(in_cat) == "POLYGON")){
#       
#       if(!is.null(keep)){ 
#         in_cat2 = simplify_process(in_cat, keep, sys)
#         } 
#       
#     } else {
#       warning ("Invalid geometries found.", call. = FALSE)
#     }
#     
#     return(
#       mutate(in_cat, areasqkm = add_areasqkm(in_cat)) %>%
#         st_transform(crs) %>%
#         select("{ID}" := ID, areasqkm)  %>%
#         left_join(st_drop_geometry(select(catchments, -any_of('areasqkm'))), by = ID)
#     )
#     
#   } else {
#     return(
#       select(polygons, -n, -tmpID)
#     )
#   }
# }
# 
# # quickly check and validate invalid geometries only
# fast_validity_check <- function(x){
#   
#   bool    = st_is_valid(x)
#   valid   = filter(x, bool)
#   invalid = st_make_valid(filter(x, !bool)) %>% 
#     st_cast("POLYGON")
#   
#   return(bind_rows(valid, invalid))
#   
# }
# 
# simplify_process = function(catchments, keep, sys){
#   # catchments2 <- in_cat
#   
#   # simplify catchments
#   catchments2 =  ms_simplify(catchments2, keep = keep, keep_shapes = TRUE, sys = sys)
#   
#   # mark valid/invalid geoms
#   bool = st_is_valid(catchments2)
#   
#   # make invalid geoms valid
#   invalids = st_make_valid(filter(catchments2, !bool))
#   
#   # if not all polygons get returned, try different simplification keep value
#   if(nrow(filter(invalids, st_geometry_type(invalids) != "POLYGON")) > 0){
#     
#     warning("Invalid geometries found. Trying new keep of:",  keep + ((1-keep) / 2) , call. = FALSE)
#     
#     # try simplification again
#     catchments2 = ms_simplify(catchments2, keep =  keep + ((1-keep) / 2), keep_shapes = TRUE, sys = sys)
#     
#     # mark valid/invalid geoms
#     bool = st_is_valid(catchments2)
#     
#     # make invalid geoms valid
#     invalids = st_make_valid(filter(catchments2, !bool))
#     
#     # if catchments2 still containing non POLYGON geometries, return original data
#     if(nrow(filter(invalids, st_geometry_type(invalids) != "POLYGON")) > 0){ 
#       
#       warning("Invalid geometries found. Original catchments2 returned." , call. = FALSE) 
#       
#       return(catchments2)
#       
#     } else {
#       
#       # combine corrected invalids with valids and recalc area
#       return(
#         mutate(
#           bind_rows(invalids, filter(catchments2, bool)), 
#           areasqkm = add_areasqkm(.)
#         ) 
#       )
#     }
#     
#   } else {
#     
#     # combine corrected invalids with valids and recalc area
#     return(
#       mutate(
#         bind_rows(invalids, filter(catchments2, bool)), 
#         areasqkm = add_areasqkm(.)
#       ) 
#     )
#   }
# }
# 
# 
# 
# 
# 
# 
# 
# 
