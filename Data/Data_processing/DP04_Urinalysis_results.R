# Urinalysis results
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you 
# have any questions or comments
# Last updated 2019-12-17

# Table of contents
# 1) Initial extraction of variables
#   1.1) Active study
#   1.2) Pilot study
# 2) Adjust/rename variables

message( 'Processing for urinalysis results' )

###
### 1) Initial extraction of variables
###

# 1.1) Active study

raw_dat = both_studies$Active$data
cn = both_studies$Active$column

col_to_add = raw_dat %>% 
  select(
    ua_thc_flag,
    ua_conf_result,
    ua_thc_dilut,
    ua_thc_ph,
    ua_thc_grav,
    ua_thclcmsms_crtadj
  ) %>% 
  rename(
    URN.CHR.THC_detected = ua_thc_flag,
    URN.DBL.THC_levels = ua_conf_result,
    URN.CHR.THC_results_by_dilution = ua_thc_dilut,
    URN.DBL.THC_pH = ua_thc_ph,
    URN.DBL.THC_gravity = ua_thc_grav,
    URN.DBL.THCCOOH_levels = ua_thclcmsms_crtadj
  )

# 1.2) Pilot study

raw_dat = both_studies$Pilot$data
cn = both_studies$Pilot$column

plt_dat = raw_dat %>% 
  select(
    ua_thc_flag,
    ua_conf_result,
    ua_thc_dilut,
    ua_thc_ph,
    ua_thc_grav,
    ua_thclcmsms_crtadj
  ) %>% 
  rename(
    URN.CHR.THC_detected = ua_thc_flag,
    URN.DBL.THC_levels = ua_conf_result,
    URN.CHR.THC_results_by_dilution = ua_thc_dilut,
    URN.DBL.THC_pH = ua_thc_ph,
    URN.DBL.THC_gravity = ua_thc_grav,
    URN.DBL.THCCOOH_levels = ua_thclcmsms_crtadj
  )

###
### 2) Adjust/rename variables
###

# Convert THC levels in active study to 
# character string to match with pilot
col_to_add$URN.DBL.THC_levels = 
  as.character( col_to_add$URN.DBL.THC_levels )

# Combine studies
col_to_add = rbind(
  col_to_add, plt_dat )

# Correct mistaken character string in THC levels
col_to_add$URN.DBL.THC_levels = 
  gsub( "Not detected", "0", col_to_add$URN.DBL.THC_levels, fixed = T )

# Convert THC levels back to numeric values
sel = col_to_add$URN.DBL.THC_levels == ""
col_to_add$URN.DBL.THC_levels[sel] = NA
col_to_add$URN.DBL.THC_levels = 
  as.numeric( col_to_add$URN.DBL.THC_levels )

# Add values to main data frame
clm = colnames( col_to_add )
for ( k in 1:ncol( col_to_add ) ) {
  
  if ( k == 1 ) {
    
    if ( is.null( dat[[ clm[k] ]] ) ) {
      dat = cbind( dat, col_to_add )
    }
    
  }
  
  dat[[ clm[k] ]] = col_to_add[[ clm[k] ]]
}

# Clean up workspace
rm( plt_dat, raw_dat, cn,
    sel, k, clm, col_to_add )

message( 'Processing finished' )

