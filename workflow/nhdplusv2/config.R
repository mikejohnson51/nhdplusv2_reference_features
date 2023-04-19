pacman::p_load(hydrofabric, logger,   archive, 
               aws.s3,      sbtools,  sfheaders,   
               parallel, pbapply,  arrow,       nngeo)

source("workflow/nhdplusv2/utils.R")
source("workflow/nhdplusv2/root_dir.R")

sf_use_s2(FALSE)

topos = read.csv("data/vpu_topology.csv")

facfdr_crs = '+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'
num        = 20

epa_bucket     = 'edap-ow-data-commons'

epa_download   = glue('{base_dir}/01_EPA_downloads/')

catchments_dir = glue('{base_dir}/02_Catchments/')
fl_dir         = glue('{base_dir}/02_Flowlines/')
wb_dir         = glue('{base_dir}/02_Waterbodies/')
ble_dir        = glue('{base_dir}/02_BLE/')

cleaned_dir    = glue('{base_dir}/03_cleaned_catchments/')
simplified_dir = glue('{base_dir}/03_simplified_catchments/')

reference_dir  = glue('{base_dir}/04_reference_geometries/')

final_dir  = glue('{base_dir}/reference_features/')

dir.create(epa_download,   showWarnings = FALSE)
dir.create(catchments_dir, showWarnings = FALSE)
dir.create(cleaned_dir,    showWarnings = FALSE)
dir.create(fl_dir,         showWarnings = FALSE)
dir.create(wb_dir,         showWarnings = FALSE)
dir.create(ble_dir,        showWarnings = FALSE)
dir.create(simplified_dir, showWarnings = FALSE)
dir.create(reference_dir,  showWarnings = FALSE)
dir.create(final_dir,      showWarnings = FALSE)

