
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reference.catchments

<!-- badges: start -->
<!-- badges: end -->

# Referenece Geometries

## NHDPlusV2 Reference Fabric (CONUS)

1.  `root_dir.R` –\> Use this to define your root directory
2.  `config.R` –\> these are workflow paramters and file structure
    defintions. Only update if you know what they are for!
3.  VPU topology (`workflow/nhdplusv2/00_vpu_topo.R`) –\> This file
    defines the touch VPU subsets
4.  Data downloads (`workflow/nhdplusv2/01_nhdplus_data.R`) –\> This
    file downloads all needed data in the root directory structure
    defined by `root_dir` and `config.R`
5.  Reference Flow lines (`workflow/nhdplusv2/02_waterbodies.R`) –\>
    This file processes all waterbodies
6.  Reference Catchments (`workflow/nhdplusv2/03_flowlines.R`) –\> This
    file processes all flowlines
7.  Reference Water bodies (`workflow/nhdplusv2/04_catchments.R`) –\>
    This file processes all catchments
8.  Merge gpkgs `(workflow/nhdplusv2/05_merge_gpkgs.R`) –\> The files
    merges all layers into VPU level and CONUS wide gpkgs
9.  Uplaoded (`workflow/nhdplusv2/06_uploaders.R`) –\> This file
    uploades data to AWS and ScienceBase (only Mike can run due to
    permissions)
10. DEM-Products (see below, `workflow/nhdplusv2/07_dem-workflow.R`);
    WIP

# DEM-Products

1.  30m DEM
2.  Breeched
3.  SCA
4.  D8
5.  TWI
6.  Flowlines
    - 30m
    - 10m
7.  Waterbodies
    - 30m
    - 10m
8.  Slope
9.  GIUH  
10. Travel Time
11. FEMA 100yr map
    - 10m
12. Riparian
    - 10m
13. MERIT landscape
14. Occurrence  
15. Extent floodwater
