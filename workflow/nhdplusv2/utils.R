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

