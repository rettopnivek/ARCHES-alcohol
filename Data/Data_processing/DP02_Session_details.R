# Session info
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you 
# have any questions or comments
# Last updated 2020-01-26

# Table of contents
# 1) Active study
#   1.1) Extract raw variables
#   1.2) Define/adjust variables
#     1.2.1) Time points and visits
#     1.2.2) Propogate values
#     1.2.3) Data quality checks
#     1.2.4) Final adjustments
# 2) Pilot study
#   2.1) Extract raw variables
#   2.2) Define/adjust variables
#     2.2.1) Adjust screen ID
#     2.2.2) Time points and visits
#     2.2.3) Propogate values
#     2.2.4) Data quality checks
#     2.2.5) Final adjustments

message( 'Processing for session info' )

###
### 1) Active study
###

raw_dat = both_studies$Active$data
cn = both_studies$Active$column

# 1.1) Extract raw variables

dat = raw_dat %>% 
  select(
    sid,
    id,
    redcap_event_name,
    school_sex,
    sex,
    age_exact,
    date,
    phone_screen_date,
    school_race,
    school_ethnicity,
    rand_group,
    user_rand,
    ua_thc_flag,
    data_quality
  ) %>% 
  rename(
    IDS.INT.Screen = sid,
    IDS.CHR.Subject = id,
    SSS.CHR.Event_name = redcap_event_name,
    DMG.Sex_1 = school_sex,
    DMG.Sex_2 = sex,
    DMG.Age_1 = age_exact,
    SSS.Date_1 = date,
    SSS.Date_2 = phone_screen_date,
    DMG.Race_1 = school_race,
    DMG.Ethnicity_1 = school_ethnicity,
    SSS.Randomization_1 = rand_group,
    SSS.Cannabis_user_1 = user_rand,
    SSS.Flag_resumed_thc = ua_thc_flag,
    SSS.Flag_data_quality = data_quality
  )

# 1.2) Define/adjust variables

# unique( dat$SSS.CHR.Event_name )

# 1.2.1) Time points and visits

dat$SSS.CHR.Time_point = ''

lst = list(
  match = list(
    'Screening',
    'V1 (BL 1)',
    'V2 (BL 2)',
    'V3 (Day 1-3)',
    'V4 (Day 2-4)',
    'V5 (Day 5-9)',
    'V6 (Day 12-16)',
    'V7 (Day 19-23)',
    'V8 (Day 26-30)',
    'V9 (Day 54-58)'
  ),
  value = list(
    'Screening',
    'Baseline 1', 
    'Baseline 2',
    'Day 1 - 3',
    'Day 2 - 4',
    'Day 5 - 9',
    'Day 12 - 16', 
    'Day 19 - 23', 
    'Day 26 - 30', 
    'Day 54 - 58'
  )
)

dat$SSS.CHR.Time_point = process_assign_value(
  dat$SSS.CHR.Event_name,
  lst, init = ''
)

lst$value = list(
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9
)

dat$SSS.INT.Visit = process_assign_value(
  dat$SSS.CHR.Event_name,
  lst, init = NA
)

# Fix specific issues with missing data for 
# visits 7 - 8 for subject 074_COMM
entries = 
  dat$IDS.INT.Screen == 244 & 
  dat$SSS.CHR.Event_name %in% c(
    'V7 (Day 19-23)',
    'V8 (Day 26-30)'
  )
dat$SSS.Date_1[ entries ] = 
  '1970-01-01'

# 1.2.2) Propogate values

# Initialize variables

dat$DMG.DBL.Age_initial = NA
dat$DMG.CHR.Sex = ""
dat$DMG.CHR.Race_original = ""
dat$DMG.CHR.Ethnicity = ""
dat$SSS.CHR.Group = ""
dat$SSS.CHR.Cannabis_user
dat$SSS.INT.Days = NA
dat$SSS.INT.Days_inbetween = NA

date_vrb = c( 'SSS.Date_1', 'SSS.Date_2' )
phd = as.Date( '1970-01-01', format = '%Y-%m-%d' )
dat$SSS.DAT.Visit_date = phd

# Extract screen IDs
screen_ids = unique( dat$IDS.INT.Screen )
n_ids = length( screen_ids )

# Create a progress bar using a base R function
pb = txtProgressBar( min = 1, max = n_ids, style = 3 )

# Convert to list for speed
dat = as.list( dat )

message( 'Active study' )

# Loop over screen IDs and...
for ( s in 1:n_ids ) {
  
  # Determine all entries for a subject 
  # using screen ID
  entries = dat$IDS.INT.Screen == screen_ids[s]
  
  # Propogate ID
  
  val = process_find_unique_val(
    dat, 
    c( 'IDS.CHR.Subject' ),
    entries,
    default = ""
  )
  dat$IDS.CHR.Subject[entries] = val
  
  # Propogate session details
  
  # - Date of visit
  
  dat$SSS.DAT.Visit_date[entries] = process_date(
    dat[date_vrb],
    entries,
    phd = phd
  )
  
  # - Change in days
  
  sel = 
    entries & 
    !(dat$SSS.INT.Visit %in% c( 0, 2 ) ) & 
    dat$SSS.DAT.Visit_date != phd
  
  if ( any( sel ) ) {
    dat$SSS.INT.Days[sel] = process_days(
      dat$SSS.DAT.Visit_date,
      sel, 
      type = 1
    )
    
    dat$SSS.INT.Days_inbetween[sel] = process_days(
      dat$SSS.DAT.Visit_date,
      sel, 
      type = 2
    )
    
  }
  
  # - Randomization group
  
  val = process_find_unique_val(
    dat, 
    c( 'SSS.Randomization_1' ),
    entries,
    default = ''
  )
  dat$SSS.CHR.Group[entries] = val
  
  # - Cannabis user
  
  val = process_find_unique_val(
    dat, 
    c( 'SSS.Cannabis_user_1' ),
    entries,
    default = ""
  )
  
  dat$SSS.CHR.Cannabis_user[entries] = val
  
  if ( val == 'Non-User' ) {
    dat$SSS.CHR.Group[entries] = val
  }
  
  # Propogate demographic variables
  
  # - Age
  
  val = process_find_unique_val(
    dat, 
    c( 'DMG.Age_1' ),
    entries,
    default = NA
  )
  dat$DMG.DBL.Age_initial[entries] = val
  
  # - Biological sex at birth
  
  val = process_find_unique_val(
    dat, 
    c( 'DMG.Sex_1',
       'DMG.Sex_2' ),
    entries,
    default = ""
  )
  dat$DMG.CHR.Sex[entries] = val
  
  # - Race
  
  val = process_find_unique_val(
    dat, 
    c( 'DMG.Race_1' ),
    entries,
    default = ""
  )
  dat$DMG.CHR.Race_original[entries] = val
  
  # - Ethnicity
  
  val = process_find_unique_val(
    dat, 
    c( 'DMG.Ethnicity_1' ),
    entries,
    default = ""
  )
  dat$DMG.CHR.Ethnicity[entries] = val
  
  # Update the progress bar
  setTxtProgressBar(pb,s)
}
close( pb )
rm( pb )

# Convert back to data frame
dat = data.frame( dat, stringsAsFactors = F )

# 1.2.3) Data quality checks

# Resumed use of cannabis
dat$DQC.LGC.Resumed_THC = FALSE
sel = 
  dat$SSS.CHR.Group == "Contingency Management (Abstinence)" & 
  dat$SSS.INT.Visit > 2 & 
  ( raw_dat$abst_95_pi == "Yes" | 
    raw_dat$tlfb_mj_fu_use == "Yes" )
dat$DQC.LGC.Resumed_THC[sel] = TRUE

# 1.2.4) Final adjustments

dat = dat %>% 
  select(
    -contains( 'DMG.Sex' ),
    -DMG.Age_1,
    -DMG.Race_1,
    -DMG.Ethnicity_1,
    -SSS.Randomization_1,
    -SSS.Cannabis_user_1,
    -contains( 'SSS.Date_' ),
    -SSS.Flag_resumed_thc, 
    -SSS.Flag_data_quality,
    -SSS.CHR.Cannabis_user
  )

# Replace fake date with missing value
dat$SSS.DAT.Visit_date[ dat$SSS.DAT.Visit_date == phd ] = NA

# Indicator for pilot study
dat$SSS.LGC.Pilot = FALSE

# Rows
dat$SSS.INT.Row = 1:nrow( dat )

# Re-order columns
dat = dat %>% 
  select(
    contains( 'IDS' ),
    contains( 'SSS' ),
    contains( 'DQC' ),
    contains( 'DMG' )
  )

# Clean up workspace
rm( lst, date_vrb, entries,
    phd, n_ids, screen_ids,
    sel, val, s )

###
### 2) Pilot study
###

raw_dat = both_studies$Pilot$data
cn = both_studies$Pilot$column

# 2.1) Extract raw variables

plt_dat = raw_dat %>% 
  select(
    sid,
    id,
    redcap_event_name,
    date,
    age_exact,
    sex_conf,
    race,
    school_race,
    ethnicity,
    school_ethnicity,
    group,
    school_randomized,
    ua_thc_flag
  ) %>% 
  rename(
    IDS.INT.Screen = sid,
    IDS.CHR.Subject = id,
    SSS.CHR.Event_name = redcap_event_name,
    SSS.Date_1 = date,
    DMG.Age_1 = age_exact,
    DMG.Sex_1 = sex_conf,
    DMG.Race_1 = race,
    DMG.Race_2 = school_race,
    DMG.Ethnicity_1 = ethnicity,
    DMG.Ethnicity_2 = school_ethnicity,
    SSS.Randomization_1 = group,
    SSS.Randomization_2 = school_randomized,
    SSS.Flag_resumed_thc = ua_thc_flag
  )

# 2.2) Define/adjust variables

# unique( plt_dat$SSS.CHR.Event_name )

# 2.2.1) Adjust screen ID

plt_dat$IDS.INT.Screen = 
  plt_dat$IDS.INT.Screen + 
  max( dat$IDS.INT.Screen, na.rm = T )

# 2.2.2) Time points and visits

plt_dat$SSS.CHR.Time_point = ''

lst = list(
  match = list(
    'Screening',
    'V1 (Baseline)',
    'V2 (Day 1-3)',
    'V3 (Day 2-4)',
    'V4 (Day 5-9)',
    'V5 (Day 12-16)',
    'V6 (Day 19-23)',
    'V7 (Day 26-30)',
    'V8 (Day 40-44)',
    'ET Visit'
  ),
  value = list(
    'Screening',
    'Baseline 1', 
    'Day 1 - 3',
    'Day 2 - 4',
    'Day 5 - 9',
    'Day 12 - 16', 
    'Day 19 - 23', 
    'Day 26 - 30', 
    'Day 40 - 44',
    'ET Visit'
  )
)

plt_dat$SSS.CHR.Time_point = process_assign_value(
  plt_dat$SSS.CHR.Event_name,
  lst, init = ''
)

lst$value = list(
  0, 1, 3, 4, 5, 6, 7, 8, 9, 0
)

plt_dat$SSS.INT.Visit = process_assign_value(
  plt_dat$SSS.CHR.Event_name,
  lst, init = NA
)

# 2.2.3) Propogate values

# Initialize variables

plt_dat$DMG.DBL.Age_initial = NA
plt_dat$DMG.CHR.Sex = ""
plt_dat$DMG.CHR.Race_original = ""
plt_dat$DMG.CHR.Ethnicity = ""
plt_dat$SSS.CHR.Group = ""
plt_dat$SSS.CHR.Cannabis_user
plt_dat$SSS.INT.Days = NA
plt_dat$SSS.INT.Days_inbetween = NA

date_vrb = c( 'SSS.Date_1' )
phd = as.Date( '1970-01-01', format = '%Y-%m-%d' )
plt_dat$SSS.DAT.Visit_date = phd

# Extract screen IDs
screen_ids = unique( plt_dat$IDS.INT.Screen )
n_ids = length( screen_ids )

# Create a progress bar using a base R function
pb = txtProgressBar( min = 1, max = n_ids, style = 3 )

# Convert to list for speed
plt_dat = as.list( plt_dat )

message( 'Pilot study' )

# Loop over screen IDs and...
for ( s in 1:n_ids ) {
  
  # Determine all entries for a subject 
  # using screen ID
  entries = plt_dat$IDS.INT.Screen == screen_ids[s]
  
  # Propogate ID
  
  val = process_find_unique_val(
    plt_dat, 
    c( 'IDS.CHR.Subject' ),
    entries,
    default = ""
  )
  plt_dat$IDS.CHR.Subject[entries] = val
  
  # Propogate session details
  
  # - Date of visit
  
  plt_dat$SSS.DAT.Visit_date[entries] = process_date(
    plt_dat[date_vrb],
    entries,
    phd = phd
  )
  
  # - Change in days
  
  sel = 
    entries & 
    !(plt_dat$SSS.INT.Visit %in% 0) & 
    plt_dat$SSS.DAT.Visit_date != phd
  
  if ( any( sel ) ) {
    
    plt_dat$SSS.INT.Days[sel] = process_days(
      plt_dat$SSS.DAT.Visit_date,
      sel, 
      type = 1
    )
    
    plt_dat$SSS.INT.Days_inbetween[sel] = process_days(
      plt_dat$SSS.DAT.Visit_date,
      sel, 
      type = 2
    )
    
  }
  
  # - Randomization group
  
  val = process_find_unique_val(
    plt_dat, 
    c( 'SSS.Randomization_1',
       'SSS.Randomization_2' ),
    entries,
    default = ''
  )
  plt_dat$SSS.CHR.Group[entries] = val
  
  # Propogate demographic variables
  
  # - Age
  
  val = process_find_unique_val(
    plt_dat, 
    c( 'DMG.Age_1' ),
    entries,
    default = NA
  )
  plt_dat$DMG.DBL.Age_initial[entries] = val/365
  
  # - Biological sex at birth
  
  val = process_find_unique_val(
    plt_dat, 
    c( 'DMG.Sex_1' ),
    entries,
    default = 0
  )
  if ( val == 1 ) {
    plt_dat$DMG.CHR.Sex[entries] = "Male"
  }
  if ( val == 2 ) {
    plt_dat$DMG.CHR.Sex[entries] = "Female"
  }
  
  # - Race
  
  val = process_find_unique_val(
    plt_dat, 
    c( 'DMG.Race_1',
       'DMG.Race_2' ),
    entries,
    default = ""
  )
  plt_dat$DMG.CHR.Race_original[entries] = val
  
  # - Ethnicity
  
  val = process_find_unique_val(
    plt_dat, 
    c( 'DMG.Ethnicity_1',
       'DMG.Ethnicity_2' ),
    entries,
    default = ""
  )
  plt_dat$DMG.CHR.Ethnicity[entries] = val
  
  # Update the progress bar
  setTxtProgressBar(pb,s)
}
close( pb )
rm( pb )

# Convert back to data frame
plt_dat = data.frame( plt_dat, stringsAsFactors = F )

# 2.2.4) Data quality checks

# Resumed use of cannabis
plt_dat$DQC.LGC.Resumed_THC = FALSE
sel = 
  plt_dat$SSS.CHR.Group == "Cannabis User (Contingency Management)" & 
  plt_dat$SSS.INT.Visit > 2 & 
  ( raw_dat$abst_95_pi == "Yes" | 
    raw_dat$tlfb_mj_fu_use == "Yes" )
plt_dat$DQC.LGC.Resumed_THC[sel] = TRUE

# 2.2.5) Final adjustments

plt_dat = plt_dat %>% 
  select(
    -DMG.Age_1, 
    -contains( 'DMG.Sex' ),
    -contains( 'DMG.Race_' ),
    -contains( 'DMG.Ethnicity_' ),
    -contains( 'SSS.Randomization_' ),
    -contains( 'SSS.Date_' ),
    -SSS.Flag_resumed_thc
  )

# Replace fake date with missing value
plt_dat$SSS.DAT.Visit_date[ plt_dat$SSS.DAT.Visit_date == phd ] = NA

# Indicator for pilot study
plt_dat$SSS.LGC.Pilot = TRUE

# Rows
plt_dat$SSS.INT.Row = 1:nrow( plt_dat ) + nrow( dat )

# Re-order columns
plt_dat = plt_dat %>% 
  select(
    contains( 'IDS' ),
    contains( 'SSS' ),
    contains( 'DQC' ),
    contains( 'DMG' )
  )

# Clean up workspace
rm( lst, date_vrb, entries,
    phd, n_ids, screen_ids,
    sel, val, s, raw_dat, cn )

###
### 3) Combine data sets
###

# 3.1) Relabel and match groups

# Active
lst = list(
  match = list(
    '', 
    'Contingency Management (Abstinence)',
    'Monitoring',
    'Non-User'
  ),
  value = list(
    '',
    'CB-Abst',
    'CB-Mon',
    'NU'
  )
)

dat$SSS.CHR.Group.Recoded = process_assign_value(
  dat$SSS.CHR.Group, lst, init = "", type = 2
)

# Pilot
lst = list(
  match = list(
    'Cannabis User (Contingency Management)',
    'Ineligible',
    'Screener not completed',
    'Cannabis user (Monitoring)',
    '',
    'Non-User'
  ),
  value = list(
    'CB-Abst',
    '',
    '',
    'CB-Mon',
    '',
    'NU'
  )
)
plt_dat$SSS.CHR.Group.Recoded = process_assign_value(
  plt_dat$SSS.CHR.Group, lst, init = "", type = 2
)

# 3.2) Combine data

if ( all( colnames( plt_dat ) == colnames( dat ) ) & 
     ncol( plt_dat ) == ncol( dat ) ) {
  
  dat = rbind(
    dat,
    plt_dat
  )
  
  rm( plt_dat )
} else {
  stop( 'Data-frames are not aligned' )
}

# 3.3) Final adjustments

# Adjust race categories
sel = dat$DMG.CHR.Race_original == 
  "Haitain, Black or African American"
dat$DMG.CHR.Race_original[sel] = 
  "Haitian, Black or African American"

# Create collapsed categories
dat$DMG.CHR.Race = dat$DMG.CHR.Race_original
sel = dat$DMG.CHR.Race_original %in% 
  c( 'Asian', 
     'Hawaiian or Other Pacific Islander',
     'More than one race',
     'Other' )
dat$DMG.CHR.Race[sel] = 'Other'

# Adjust sex to refer to biological sex 
# at birth
dat$DMG.CHR.Sex_original = dat$DMG.CHR.Sex
sel = dat$DMG.CHR.Sex_original == 
  "Transgender Male (female to male)"
dat$DMG.CHR.Sex[sel] = "Female"

# Rename groups
dat = dat %>% 
  rename(
    SSS.CHR.Group_original = SSS.CHR.Group
  )
dat = dat %>% 
  rename(
    SSS.CHR.Group = SSS.CHR.Group.Recoded
  )

# Re-order columns
dat = dat %>% 
  select(
    contains( 'IDS' ),
    contains( 'SSS' ),
    contains( 'DQC' ),
    contains( 'DMG' )
  )

# Clean up workspace
rm( lst, sel )

message( 'Processing finished' )
