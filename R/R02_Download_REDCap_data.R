# Download REDCap data
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you 
# have any questions or comments
# Last updated 2020-01-23

# Table of contents
# 1) Initial setup
# 2) Download REDCap data

###
### 1) Initial setup
###

message( 'Loading in REDCap data' )

# Read in directory pathways
source( 'R01_Directory_structure.R' )

# Useful functions
load_package( 'utilityf', from = 'Github' )

# Package for manipulating data frames
load_package( 'dplyr' )

# Package for manipulating strings
load_package( 'stringr' )

# Package with convenience functions 
# for downloading data from REDCap

# *WARNING*
# The most recent version of REDCapR does not 
# work with the specified projects - version 
# 0.9.8 works instead
# *WARNING*

go_to( 'REDCapR' );
location = getwd()
go_to( 'R scripts' )
library( REDCapR, lib.loc = location )

# Code to install previous version of REDCapR
if ( FALSE ) {
  
  package_url = paste0(
    "https://cran.r-project.org/",
    "src/contrib/Archive/REDCapR/",
    "REDCapR_0.9.8.tar.gz"
  )
  
  go_to( 'REDCapR' )
  destination = getwd()
  go_to( 'R scripts' )
  
  install.packages(
    package_url,
    repos = NULL,
    type = 'source',
    lib = destination
  )
  
  # Clean up workspace
  rm( package_url,
      destination )
  
}

# Logical; if TRUE, re-downloads data 
# from REDCap
if ( !exists( 'update_data' ) ) {
  update_data = F
}

###
### 2) Read in Redcap data
###

# Location for data
go_to( 'Data' )

# Check if .RData file already exists
tag = "ARCHES_REDCap_data"
redcap_file_exists = file_present( tag, output = 'logical' )

if ( update_data | !redcap_file_exists ) {
  # Download data from REDCap
  
  # Remove previous file
  if ( redcap_file_exists ) {
    file.remove( file_present( tag, output = 'name' ) )
  }
  
  # Read in API token
  go_to( 'Raw data' )
  token = read.csv( 
    file = "API_token.csv", 
    header = T,
    stringsAsFactors = F
  )
  
  # Loop over the pilot and active studies
  both_studies = list(
    Active = list(),
    Pilot = list()
  )
  for ( j in 1:nrow( token ) ) {
    
    # Use the REDCapR package to download full data set as data frame
    
    dwnld = redcap_read( 
      redcap_uri = "https://redcap.partners.org/redcap/api/", 
      token = token$Token[j],
      raw_or_label = 'label'
    )
    # For robustness all values will be read in as 
    # character strings
    
    # Save data
    raw_dat = dwnld$data
    
    # Column names
    cn = colnames( raw_dat )
    
    both_studies[[j]] = list(
      data = raw_dat,
      column = cn
    )
    
  }
  
  # Save file with data of extraction
  val = Sys.Date()
  val = gsub( '-', '_', val, fixed = T )
  
  go_to( 'Data' )
  save( both_studies, 
        file = paste( tag, '_', val, '.RData', sep = '' ) )
  
  # Clean up workspace
  rm( dwnld, token, val, raw_dat, cn, j )
  
} else {
  load( file = file_present( tag, output = 'name' ) )
}

# Clean up workspace
rm( tag, redcap_file_exists, update_data, location )

# Return to R scripts directory
go_to( 'R' )


