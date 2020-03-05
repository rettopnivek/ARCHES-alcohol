# TLFB ETOH data
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you 
# have any questions or comments
# Last updated 2020-01-27

# Table of contents
# 1) Initial setup
# 2) Extract TLFB for active study
# 3) Extract TLFB for pilot study
# 4) Post-hoc processing

message( 'Processing for TLFB ETOH data' )

###
### 1) Initial setup
###

# Remember current working directory
cur_dir = getwd()

# Specify location of TLFB calendars on shared drive
network_drive = paste0(
  "\\\\cifs2.partners.org/ADDNMED2$/Grants/Schuster/",
  "Cognition and Adolescent Health/TLFB" )

accessed_shared_drive = tryCatch({
    setwd( network_drive )
    TRUE
  }, error = function(e) FALSE )

if ( !accessed_shared_drive ) {
  go_to( 'Raw data' )
  setwd( 'TLFB_Calendars' )
}

# Initialize variables of interest
dat$TLF.INT.ETOH.Total_days = NA
dat$TLF.INT.ETOH.Days_drinking = NA
dat$TLF.INT.ETOH.Number_of_drinks = NA
dat$TLF.INT.MJ.Days_used = NA
dat$TLF.INT.ETOH.Any_missing = NA

###
### 2) Extract TLFB for active study
###

# Extract raw data
raw_dat = both_studies$Active$data
cn = both_studies$Active$column

# Subject IDs
ids = unique( dat$IDS.CHR.Subject[ !dat$SSS.LGC.Pilot ] )
ids = ids[ ids != "" ]
n_ids = length( ids )

# Navigate to location of calendars for active study
setwd( "Active Study" )
message( 'Active study' )

# Create a progress bar using a base R function
pb = txtProgressBar( min = 1, max = n_ids, style = 3 )

# Loop over subjects
for ( s in 1:n_ids ) {
  
  # Isolate entries for current subject
  cur_dat = dat %>% 
    filter( IDS.CHR.Subject == ids[s] )
  
  # 2.1) TLFB Calendar
  
  # Extract visits
  vst = cur_dat$SSS.INT.Visit
  vst = vst[ !is.na( vst ) & vst != 0 ]
  
  # Specify sheet names for excel sheet
  sheet_names = paste0( "TLFB Interim V", vst )
  sheet_names[ sheet_names == "TLFB Interim V1" ] = 
    "TLFB Baseline"
  
  # File name for excel sheet
  fname = paste( ids[s], "TLFB.xlsx", sep = "_" )
  
  # Initialize output for calendar
  out = data.frame(
    ID = rep( ids[s], 365*3 ),
    Date = as.Date( '1980-01-01' ),
    Visit = NA,
    Days_drinking = NA,
    N_drinks = NA,
    Any_missing = 0, 
    stringsAsFactors = F
  )
  # Initialize indices
  st = 1
  en = 0
  
  # Convert to list for speed
  out = as.list( out )
  
  # Loop over visits
  for ( v in 1:length( vst ) ) {
    
    # Specify row to start pulling data
    if ( v == 1 ) {
      sr = 0
    } else {
      sr = 3
    }
    
    # Read in excel sheet
    tlfb_cal = tryCatch(
      suppressWarnings(
      suppressMessages(
      read_excel(
        path = fname,
        sheet = sheet_names[v],
        col_names = T,
        skip = sr
      ) %>% as.data.frame() ) ),
      error = function(e) return( NULL )
    )
    
    # If sheet exists, process data
    if ( !is.null( tlfb_cal ) ) {
      
      for ( i in 1:ncol( tlfb_cal ) ) {
        if ( is.factor( tlfb_cal[[i]] ) ) {
          tlfb_cal[[i]] = as.character( tlfb_cal[[i]] )
        }
      }
      
      no_na = !is.na( tlfb_cal[[2]] )
      tlfb_cal = tlfb_cal[no_na,]
      
      st = 
        length( out$Visit ) - 
        sum( is.na( out$Visit ) ) + 1
      en = 
        length( out$Visit ) - 
        sum( is.na( out$Visit ) ) + 
        length( tlfb_cal[[2]] )
      
      out$Date[ st:en ] = tlfb_cal[[2]]
      out$Visit[ st:en ] = v
      out$Days_drinking[ st:en ] = tlfb_cal[[3]]
      out$N_drinks[ st:en ] = tlfb_cal[[4]]
      
    }
    
  }
  
  # Convert back to data frame
  out = out %>% data.frame( stringsAsFactors = F )
  # Remove missing rows
  out = out %>% 
    filter( !is.na( Visit ) )
  
  # Check for missing data
  check = 
    ( out$Days_drinking != 0 & 
      out$N_drinks == 0 ) | 
    is.na( out$Days_drinking ) | 
    is.na( out$N_drinks )
  
  if ( any( check ) ) {
    out$Any_missing[ check ] = 1
  }
  rm( check )
  
  # Loop over rows
  cur_dat = as.list( cur_dat )
  for ( i in 1:length( cur_dat[[1]] ) ) {
    
    if ( i == 1 ) {
      sel = out$Date <= cur_dat$SSS.DAT.Visit_date[i]
    } else {
      sel = out$Date > cur_dat$SSS.DAT.Visit_date[i-1] & 
        out$Date <= cur_dat$SSS.DAT.Visit_date[i]
    }
    if ( any(sel & !is.na(sel)) ) {
      cur_dat$TLF.INT.ETOH.Days_drinking[i] = 
        sum( out$Days_drinking[sel] )
      cur_dat$TLF.INT.ETOH.Number_of_drinks[i] = 
        sum( out$N_drinks[sel] )
      cur_dat$TLF.INT.ETOH.Any_missing[i] = 
        sum( out$Any_missing[sel] )
      cur_dat$TLF.INT.ETOH.Total_days[i] = 
        difftime(
          max( out$Date[sel] ),
          min( out$Date[sel] ),
          units = 'days' ) + 1
    }
    
  }
  
  # 2.2) REDCap variables
  
  # Isolate raw data for current subject
  cur_sel = raw_dat$sid %in% cur_dat$IDS.INT.Screen
  for ( i in 1:length( cur_dat[[1]] ) ) {
    
    # Extract current date
    cur_date = cur_dat$SSS.DAT.Visit_date[i]
    
    # If date is not missing, assume visit has occurred
    if ( !is.na( cur_date ) ) {
      
      # Extract current visit
      cur_vst = cur_dat$SSS.INT.Visit[i]
      # Extract current values for outcomes
      outcomes = rep( NA, 4 )
      outcomes[1] = cur_dat$TLF.INT.ETOH.Total_days[i]
      outcomes[2] = cur_dat$TLF.INT.ETOH.Days_drinking[i]
      outcomes[3] = cur_dat$TLF.INT.ETOH.Number_of_drinks[i]
      outcomes[4] = cur_dat$TLF.INT.MJ.Days_used[i]
      
      if ( cur_vst > 0 ) {
        
        n_days = difftime( cur_date, 
                           prev_date,
                           units = 'days' )
        
        
        if ( cur_vst == 1 ) {
          
          # Extract follow-up values
          val = raw_dat$tlfb_etoh_12b[cur_sel][i]
          cur_dat$TLF.INT.ETOH.Days_drinking[i] = val
          val = raw_dat$tlfb_etoh_13b[cur_sel][i]
          cur_dat$TLF.INT.ETOH.Number_of_drinks[i] = val
          val = raw_dat$tlfb_mj_15b[cur_sel][i]
          cur_dat$TLF.INT.MJ.Days_used[i] = val
          cur_dat$TLF.INT.ETOH.Total_days[i] = 30
          
        } else {
          
          # ETOH
          
          # Check for missing data or a mismatch in the 
          # number of reported days between visits
          check = 
            is.na( outcomes[2] ) | 
            n_days != outcomes[1]
          if ( check ) {
            
            if ( cur_vst > 1 ) {
              
              # Extract follow-up values
              val = raw_dat$tlfb_etoh_fu1[cur_sel][i]
              if ( val == "No" ) {
                cur_dat$TLF.INT.ETOH.Days_drinking[i] = 0
                cur_dat$TLF.INT.ETOH.Number_of_drinks[i] = 0
              } else {
                val = raw_dat$tlfb_etoh_fu2[cur_sel][i]
                cur_dat$TLF.INT.ETOH.Days_drinking[i] = val
                val = raw_dat$tlfb_etoh_fu3[cur_sel][i]
                cur_dat$TLF.INT.ETOH.Number_of_drinks[i] = val
              }
              cur_dat$TLF.INT.ETOH.Total_days[i] = n_days
              
            }
            
          }
          
          # MJ
          check = 
            is.na( outcomes[4] )
          if ( check ) {
            
            if ( cur_vst > 1 ) {
              
              # MJ
              val = raw_dat$tlfb_mj_fu_use[cur_sel][i]
              if ( val == "No" ) {
                cur_dat$TLF.INT.MJ.Days_used[i] = 0
              } else {
                val = raw_dat$tlfb_mj_fu1[cur_sel][i]
                cur_dat$TLF.INT.MJ.Days_used[i] = val
              }
              
            }
            
          }
          
          # print( paste( n_days, '|', outcomes[1] ) )
          # print( n_days == outcomes[1] )
          
        }
        
      }
      
      prev_date = cur_date
    }
    
  }
  
  cur_sel = dat$IDS.CHR.Subject == ids[s]
  dat$TLF.INT.ETOH.Total_days[cur_sel] = 
    cur_dat$TLF.INT.ETOH.Total_days
  dat$TLF.INT.ETOH.Days_drinking[cur_sel] = 
    cur_dat$TLF.INT.ETOH.Days_drinking
  dat$TLF.INT.ETOH.Number_of_drinks[cur_sel] = 
    cur_dat$TLF.INT.ETOH.Number_of_drinks
  dat$TLF.INT.MJ.Days_used[cur_sel] = 
    cur_dat$TLF.INT.MJ.Days_used
  
  # Update the progress bar
  setTxtProgressBar(pb,s)
}
close( pb )
rm( pb )

setwd( '..' )

###
### 3) Extract TLFB for pilot study
###

# Extract raw data
raw_dat = both_studies$Pilot$data
cn = both_studies$Pilot$column

# Subject IDs
ids = unique( dat$IDS.CHR.Subject[ dat$SSS.LGC.Pilot ] )
ids = ids[ ids != "" ]
n_ids = length( ids )

# Navigate to location of calendars for active study
setwd( "Pilot Study" )
message( 'Pilot study' )

# Create a progress bar using a base R function
pb = txtProgressBar( min = 1, max = n_ids, style = 3 )

# Loop over subjects
for ( s in 1:n_ids ) {
  
  # Isolate entries for current subject
  cur_dat = dat %>% 
    filter( IDS.CHR.Subject == ids[s] )
  
  # 3.1) TLFB Calendar
  
  # Extract visits
  vst = cur_dat$SSS.INT.Visit
  vst = vst[ !is.na( vst ) & vst != 0 ]
  
  # Specify sheet names for excel sheet
  sheet_names = c(
    "TLFB Baseline",
    "TLFB Interim"
  )
  
  # File name for excel sheet
  fname = paste( ids[s], "TLFB.xlsx", sep = "_" )
  
  # Initialize output for calendar
  out = data.frame(
    ID = rep( ids[s], 365*2 ),
    Date = as.Date( '1980-01-01' ),
    Visit = NA,
    Days_drinking = NA,
    N_drinks = NA,
    Any_missing = 0, 
    stringsAsFactors = F
  )
  # Initialize indices
  st = 1
  en = 0
  
  # Convert to list for speed
  out = as.list( out )
  
  # Loop over sheets
  for ( v in 1:length( sheet_names ) ) {
    
    # Specify row to start pulling data
    sr = 0
    
    # Read in excel sheet
    tlfb_cal = tryCatch(
      suppressWarnings(
      suppressMessages(
        read_excel(
          path = fname,
          sheet = sheet_names[v],
          col_names = T,
          skip = sr
        ) %>% as.data.frame() ) ),
      error = function(e) return( NULL )
    )
    
    # If sheet exists, process data
    if ( !is.null( tlfb_cal ) ) {
      
      for ( i in 1:ncol( tlfb_cal ) ) {
        if ( is.factor( tlfb_cal[[i]] ) ) {
          tlfb_cal[[i]] = as.character( tlfb_cal[[i]] )
        }
      }
      
      no_na = !is.na( tlfb_cal[[2]] )
      tlfb_cal = tlfb_cal[no_na,]
      
      st = 
        length( out$Visit ) - 
        sum( is.na( out$Visit ) ) + 1
      en = 
        length( out$Visit ) - 
        sum( is.na( out$Visit ) ) + 
        length( tlfb_cal[[2]] )
      
      out$Date[ st:en ] = tlfb_cal[[2]]
      out$Visit[ st:en ] = v
      out$Days_drinking[ st:en ] = tlfb_cal[[3]]
      out$N_drinks[ st:en ] = tlfb_cal[[4]]
      
    }
    
  }
  
  # Convert back to data frame
  out = out %>% data.frame( stringsAsFactors = F )
  # Remove missing rows
  out = out %>% 
    filter( !is.na( Visit ) )
  
  # Check for missing data
  check = 
    ( out$Days_drinking != 0 & 
        out$N_drinks == 0 ) | 
    is.na( out$Days_drinking ) | 
    is.na( out$N_drinks )
  if ( any( check ) ) {
    out$Any_missing[ check ] = 1
  }
  rm( check )
  
  # Loop over rows
  cur_dat = as.list( cur_dat )
  for ( i in 1:length( cur_dat[[1]] ) ) {
    
    if ( i == 1 ) {
      sel = out$Date <= cur_dat$SSS.DAT.Visit_date[i]
    } else {
      sel = out$Date > cur_dat$SSS.DAT.Visit_date[i-1] & 
        out$Date <= cur_dat$SSS.DAT.Visit_date[i]
    }
    if ( any(sel & !is.na(sel)) ) {
      cur_dat$TLF.INT.ETOH.Days_drinking[i] = 
        sum( out$Days_drinking[sel] )
      cur_dat$TLF.INT.ETOH.Number_of_drinks[i] = 
        sum( out$N_drinks[sel] )
      cur_dat$TLF.INT.ETOH.Any_missing[i] = 
        sum( out$Any_missing[sel] )
      cur_dat$TLF.INT.ETOH.Total_days[i] = 
        difftime(
          max( out$Date[sel] ),
          min( out$Date[sel] ),
          units = 'days' ) + 1
    }
    
  }
  
  # 3.2) REDCap variables
  
  # Isolate raw data for current subject
  cur_sel = raw_dat$sid %in%
    ( cur_dat$IDS.INT.Screen - 
        max( dat$IDS.INT.Screen[ !dat$SSS.LGC.Pilot ] ) )
  for ( i in 1:length( cur_dat[[1]] ) ) {
    
    # Extract current date
    cur_date = cur_dat$SSS.DAT.Visit_date[i]
    
    # If date is not missing, assume visit has occurred
    if ( !is.na( cur_date ) ) {
      
      # Extract current visit
      cur_vst = cur_dat$SSS.INT.Visit[i]
      # Extract current values for outcomes
      outcomes = rep( NA, 4 )
      outcomes[1] = cur_dat$TLF.INT.ETOH.Total_days[i]
      outcomes[2] = cur_dat$TLF.INT.ETOH.Days_drinking[i]
      outcomes[3] = cur_dat$TLF.INT.ETOH.Number_of_drinks[i]
      outcomes[4] = cur_dat$TLF.INT.MJ.Days_used[i]
      
      if ( cur_vst > 0 ) {
        
        n_days = difftime( cur_date, 
                           prev_date,
                           units = 'days' )
        
        if ( cur_vst == 1 ) {
          
          
          # Extract follow-up values
          val = raw_dat$tlfb_etoh_12b_recalc[cur_sel][i]
          cur_dat$TLF.INT.ETOH.Days_drinking[i] = val
          val = raw_dat$tlfb_etoh_13b_recalc[cur_sel][i]
          cur_dat$TLF.INT.ETOH.Number_of_drinks[i] = val
          val = raw_dat$tlfb_mj_15b_recalc[cur_sel][i]
          cur_dat$TLF.INT.MJ.Days_used[i] = val
          cur_dat$TLF.INT.ETOH.Total_days[i] = 30
          cur_dat$TLF.INT.ETOH.Any_missing[i] = 0
          
        } else {
          
          
          # ETOH
          
          # Check for missing data or a mismatch in the 
          # number of reported days between visits
          check = 
            is.na( outcomes[2] ) | 
            n_days != outcomes[1]
          if ( check ) {
            
            if ( cur_vst > 1 ) {
              
              # ETOH
              
              # Extract follow-up values
              val = raw_dat$tlfb_etoh_fu1[cur_sel][i]
              if ( val == "No" ) {
                cur_dat$TLF.INT.ETOH.Days_drinking[i] = 0
                cur_dat$TLF.INT.ETOH.Number_of_drinks[i] = 0
              } else {
                val = raw_dat$tlfb_etoh_fu2[cur_sel][i]
                cur_dat$TLF.INT.ETOH.Days_drinking[i] = val
                val = raw_dat$tlfb_etoh_fu4[cur_sel][i]
                cur_dat$TLF.INT.ETOH.Number_of_drinks[i] = val
              }
              cur_dat$TLF.INT.ETOH.Total_days[i] = n_days
              cur_dat$TLF.INT.ETOH.Any_missing[i] = 0
            }
            
          }
          
          # MJ
          
          check = is.na( outcomes[4] )
          if ( check ) {
            
            if ( cur_vst > 1 ) {
              
              # Extract follow-up values
              val = raw_dat$tlfb_mj_fu_use[cur_sel][i]
              if ( val == "No" ) {
                cur_dat$TLF.INT.MJ.Days_used[i] = 0
              } else {
                val = raw_dat$tlfb_mj_fu1[cur_sel][i]
                cur_dat$TLF.INT.MJ.Days_used[i] = val
              }
              
            }
            
          }
          
          # print( paste( n_days, '|', outcomes[1] ) )
          # print( n_days == outcomes[1] )
          
        }
        
      }
      
      prev_date = cur_date
    }
    
  }
  
  cur_sel = dat$IDS.CHR.Subject == ids[s]
  dat$TLF.INT.ETOH.Total_days[cur_sel] = 
    cur_dat$TLF.INT.ETOH.Total_days
  dat$TLF.INT.ETOH.Days_drinking[cur_sel] = 
    cur_dat$TLF.INT.ETOH.Days_drinking
  dat$TLF.INT.ETOH.Number_of_drinks[cur_sel] = 
    cur_dat$TLF.INT.ETOH.Number_of_drinks
  dat$TLF.INT.MJ.Days_used[cur_sel] = 
    cur_dat$TLF.INT.MJ.Days_used
  dat$TLF.INT.ETOH.Any_missing[cur_sel] = 
    cur_dat$TLF.INT.ETOH.Any_missing
  
  # Update the progress bar
  setTxtProgressBar(pb,s)
}
close( pb )
rm( pb )

setwd( '..' )

###
### 4) Post-hoc processing
###

setwd( cur_dir )

# If subjects drank on every day between vists and 
# also drank on day of visit, then days between 
# visits (calculated by date) will be less than 
# number of days spent drinking by one day
# (because visit day is counted)

# Identify these specific cases and re-normalize 
# to only look at days between visits
check = 
  dat$SSS.INT.Days_inbetween < dat$TLF.INT.ETOH.Days_drinking & 
  dat$SSS.INT.Days != 0
check = !is.na( check ) & check
if ( any( check ) ) {
  
  dat$TLF.INT.ETOH.Number_of_drinks[check] = 
    dat$TLF.INT.ETOH.Number_of_drinks[check] * 
    ( dat$SSS.INT.Days_inbetween[check] / 
      dat$TLF.INT.ETOH.Days_drinking[check] )
  
  dat$TLF.INT.ETOH.Days_drinking[check] = 
    dat$TLF.INT.ETOH.Days_drinking[check] - 1
}

# Clean up workspace
rm( sel, network_drive, no_na, outcomes,
    prev_date, s, sheet_names, sr, st, 
    v, val, vst, i, en, ids, n_days, 
    n_ids, cur_vst, fname, cur_dir, cur_date, 
    cur_sel, check, cur_dat, out,
    raw_dat, cn, tlfb_cal, accessed_shared_drive )

message( 'Processing finished' )

