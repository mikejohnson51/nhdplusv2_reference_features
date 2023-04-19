find_file_path = function(VPU, files, new_dir){
  f = grep(VPU, basename(files))
  tmp01 = files[f]
  tmp02 = paste0(new_dir,  basename(files[f]))

  if(!file.exists(tmp02)){
    log_info('\tCopying ', VPU, " to reference.")
    file.copy(grep(tmp01, files, value = TRUE), tmp02)
  }

  tmp02
}

# test

# get_nhdplus(comid = 3645, realization = "all") %>%
#   mapview::mapview()

# get_nhdplus(comid = 3981, realization = "all") %>%
#   mapview::mapview()

fix_mp_issues = function(catchments, flowlines, ID = "ID"){

  mps = filter(catchments, st_geometry_type(catchments) == "MULTIPOLYGON")
  mps =  suppressWarnings({ st_cast(mps, "POLYGON") }) %>%
    filter(!duplicated(.))

  good_to_go = filter(catchments, st_geometry_type(catchments) == "POLYGON") %>%
    nhdplusTools::rename_geometry("geometry")

  new = list()

  if(nrow(mps) > 1){

    tmp  = mutate(mps, areasqkm = add_areasqkm(mps))

    uids = unique(tmp[[ID]])
    message("Exploding ", length(uids), " unique IDs.")

    if (length(uids) > 0) {

      for (i in 1:length(uids)) {

        here = filter(tmp, get(ID) == uids[i])  %>%
          mutate(newID = 1:n())

        imap = st_intersects(here, filter(flowlines, COMID == here[[ID]][1])) %>%
          lengths()

        if(sum(imap == 1) > 1){

          ll = vector()

          suppressWarnings({
            for(z in 1:nrow(here)){
              tmp_l = st_intersection(here[z,], filter(flowlines, COMID == here[[ID]][1])) %>%
                st_length()

              if(length(tmp_l) == 0){
                ll[z] =  0
              } else {
                ll[z] =  tmp_l
              }
            }
          })

          imap = rep(0, length(ll))
          imap[which.max(ll)] = 1
        } else if(sum(imap == 1) < 1){

          imap = rep(0, nrow(here))
          imap[1] = 1
        }

        new[[i]] =  filter(here, imap == 1)

        to_dissolve = filter(here, imap != 1)


        for(j in 1:nrow(to_dissolve)){

          tmap = st_filter(good_to_go, to_dissolve[j,], .predicate = st_touches)

          suppressWarnings({
            opt <- st_intersection(to_dissolve[j,], tmap)
            gt  <- sf::st_geometry_type(opt)
            if (length(gt) == 1 && grepl("POINT", gt)) {
              opt <- sf::st_cast(opt, "LINESTRING")
            }
          })

          opt <- opt %>%
            st_collection_extract("LINESTRING") %>%
            mutate(l = st_length(.)) %>%
            slice_max(.data$l, with_ties = FALSE)

          ind = which(good_to_go[[ID]] == opt[[paste0(ID, ".1")]])
          good_to_go$geometry[ind] = st_union(st_union(st_geometry(to_dissolve[j,])),
                                              st_geometry(good_to_go[ind,]))
        }

        message(i)
      }
    }

    new_adj = bind_rows(new) %>%
      mutate(newID = NULL)
    xx = bind_rows(good_to_go, new_adj)
  } else {
    xx = good_to_go
  }

  if(nrow(xx) == length(unique(catchments[[ID]])) &
     sum(st_geometry_type(xx) == "POLYGON") == length(unique(catchments[[ID]]))){
    return(xx)
  } else {
    stop()
  }
}

