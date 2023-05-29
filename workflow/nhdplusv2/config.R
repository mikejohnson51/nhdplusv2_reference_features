pacman::p_load(
  hydrofabric,
  logger,
  archive,
  rmapshaper,
  aws.s3,
  sbtools,
  sfheaders,
  parallel,
  pbapply,
  arrow,
  nngeo
)

source("workflow/nhdplusv2/utils.R")
source("workflow/nhdplusv2/root_dir.R")

sf_use_s2(FALSE)

topos = read.csv("data/vpu_topology.csv")

facfdr_crs = '+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'
num        = 50

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

# path to NHDPlus BLE events folder
nhdplus_ble_path <- "D:/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb"
# nhdplus_ble_path <- '/Volumes/Transcend/ngen/NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb'

# create base dir if it doesn't already exist
if(!dir.exists(base_dir)) {
  message(glue('Base directory does not exist...\nCreating directory: {base_dir}'))
  dir.create(base_dir, showWarnings = FALSE)
}

dir.create(epa_download,   showWarnings = FALSE)
dir.create(catchments_dir, showWarnings = FALSE)
dir.create(cleaned_dir,    showWarnings = FALSE)
dir.create(fl_dir,         showWarnings = FALSE)
dir.create(wb_dir,         showWarnings = FALSE)
dir.create(ble_dir,        showWarnings = FALSE)
dir.create(simplified_dir, showWarnings = FALSE)
dir.create(reference_dir,  showWarnings = FALSE)
dir.create(final_dir,      showWarnings = FALSE)

