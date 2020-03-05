# Data processing functions
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you 
# have any questions or comments
# Last updated 2020-02-13

# Table of contents
# 1) Packages
# 2) Functions
#   2.1) process_assign_value
#   2.2) process_find_unique_val
#   2.3) process_date
#   2.4) process_days
#   2.5) process_merge
#   2.6) process_find_closest
#   2.7) process_usage

###
### 1) Packages
###

# Useful functions
load_package( 'utilityf', from = 'Github' )

# Package for manipulating data frames
load_package( 'dplyr' )

# Package for manipulating strings
load_package( 'stringr' )

# Package for reading in Excel files
load_package( 'readxl' )

###
### 2) Functions
###

# Checks if functions exist in workspace
function_names = c(
  'process_assign_value',
  'process_find_unique_val',
  'process_date', 
  'process_days', 
  'process_merge',
  'process_find_closest',
  'process_usage'
)
check = any( sapply( function_names, exists ) )

if ( !check ) {
  
  message( 'Loading in processing functions' )
  
  # 2.1) 
  process_assign_value = function( input, lst, 
                                   init = NA,
                                   type = 1 ) {
    # Purpose:
    # Assigns values based on matches with an input vector,
    # producing a new vector of equivalent length to the input.
    # Arguments:
    # input - A vector of original values
    # lst   - A list with two named elements...
    #         match = The elements to match over
    #         value = The value to assign with successful matches
    # init  - The default value for the output
    # type  - The type of matching, either...
    #         1 = incomplete pattern matching
    #         2 = complete pattern matching
    # Returns:
    # A vector of new values.
    
    # Number of observations
    No = length( input )
    # Initialize output
    output = rep( init, No )
    
    # Number of values/elements to match over
    Nm = length( lst$value )
    
    # Loop over values and matches
    for ( i in 1:Nm ) {
      
      # Match over incomplete patterns
      if ( type == 1 ) {
        sel = grepl( lst$match[[i]], input, fixed = T )
      }
      # Match over complete patterns
      if ( type == 2 ) {
        sel = input %in% lst$match[[i]]
      }
      
      # Assign value
      output[sel] = lst$value[[i]]
      
    }
    
    return( output )
  }
  
  # 2.2) 
  process_find_unique_val = function( dat,
                                      vrb_names, 
                                      entries, 
                                      missing = "", 
                                      default = NA ) {
    # Purpose:
    # Extracts the unique value to propogate over all 
    # entries for a set of variables.
    # Arguments:
    # dat        - A list of variables of matching length 
    #              to 'entries'
    # vrb_names  - A vector of variable names
    # entries    - A logical vector equal to TRUE for 
    #              the entries belonging to a specific 
    #              subject
    # default    - The default output to return in the case 
    #              of missing values
    # Returns:
    # A unitary value.
    
    # Number of variables
    K = length( vrb_names )
    
    # Initialize output
    out = default
    cur_val = default
    
    # Loop over variables
    for ( k in 1:K ) {
      
      # Check for missing data over range of entries for subject
      no_missing = 
        entries & 
        !is.na( dat[[ vrb_names[k] ]] ) & 
        dat[[ vrb_names[k] ]] != missing
      
      # If entry provided, set output to unique value
      if ( any( no_missing ) ) {
        cur_val = unique( dat[[ vrb_names[k] ]][no_missing] )
        
        # Print a warning if multiple values are detected
        if ( length( cur_val ) > 1 ) {
          print( 'Multiple values detected' )
          print( unique( dat$IDS.CHR.Screen[entries] ) )
          print( cur_val )
          print( '' )
        }
        cur_val = cur_val[1]
        
        # Stop loop
        break()
      }
      
    }
    # Update output
    out = cur_val
    
    # Return output
    return( out )
  }
  
  # 2.3) 
  process_date = function( date_vrb, entries, 
                           visits = NULL,
                           phd = "1970-01-01" ) {
    # Purpose:
    # Function to convert a set of variables with potential 
    # dates (as string characters) into vector of R date objects.
    # Arguments:
    # date_vrb   - A list with variables with potential dates (as 
    #              character strings)
    # entries    - A logical vector equal to TRUE for 
    #              the entries belonging to a specific 
    #              subject
    # visits     - An optional vector of matching length to 
    #              'entries' indexing the separate visits
    #              to propogate dates over
    # phd        - A character string with a placeholder date 
    #              in the format 'YYYY-MM-DD'
    # Returns:
    # A vector of R date objects.
    
    # Plaecholder date
    phd = as.Date( phd, format = '%Y-%m-%d' )
    
    # Number of variables to process
    K = length( date_vrb )
    # Number of observations
    n = sum( entries )
    
    # Extract character strings with possible dates
    mat = matrix( "", n, K )
    for ( k in 1:K ) {
      mat[,k] = as.character( date_vrb[[k]][entries] )
    }
    mat[ is.na( mat ) ] = ""
    # Initialize with placeholder date
    out = rep( phd, n )
    
    # Loop over variables and extract possible dates
    for ( i in 1:n ) {
      sel = mat[i,] != ""
      if ( any( sel ) ) {
        cur_date = 
          strsplit( mat[i,sel][1], split = ":" )[[1]][1]
        
        # Convert to R date object
        val = as.Date( cur_date, format = '%Y-%m-%d' )
        
        # Check that date has been inputted correctly
        # (i.e., if it was day-month-year instead)
        check_date = as.Date( "99-01-01", format = "%Y-%m-%d" )
        if ( val < check_date ) {
          val = as.Date( cur_date,
                         format = "%d-%m-%Y" )
        }
        
        if ( !is.na( val ) ) out[i] = val
      }
    }
    
    # If 'visits' is provided, propogate dates over 
    # each unique visit
    if ( !is.null( visits ) ) {
      
      if ( any( !is.na( visits ) ) ) {
        
        unq = sort( unique( visits ) )
        unq = unq[ !is.na( unq ) ]
        
        for ( k in 1:length( unq ) ) {
          
          sel = 
            !is.na( visits ) & 
            visits == unq[k] & 
            out != phd
          
          if ( any( sel ) ) {
            if ( sum(sel) != sum( visits == unq[k] ) ) {
              cur_date = unique( out[sel] )[1]
              out[ visits == unq[k] ] = cur_date
            }
          }
          
        }
        
      }
      
    }
    
    return( out )
  }
  
  # 2.4) 
  process_days = function( x, 
                           entries, 
                           type = 1, 
                           time_points = NULL,
                           phd = '1970-01-01',
                           init = NA ) {
    # Purpose:
    # Computes the number of days since the first 
    # visit (or the increment in days between 
    # visits) given a set of dates.
    # Arguments:
    # x           - A vector of dates
    # entries     - A logical vector for the entries 
    #               to consider
    # type        - If type equals...
    #               1 = computes days since first visit
    #               2 = computes days between visits
    # time_points - An optional integer vector ranking 
    #               the dates from first to last
    #               (for breaking ties)
    # phd         - The placeholder date, a character 
    #               string formatted as 'YYYY-MM-DD'
    # init        - The default value for the output
    # Returns:
    # A vector of integers.
    
    # Initialize output
    out = rep( init, sum( entries ) )
    
    # Placeholder date
    phd = as.Date( phd, format = '%Y-%m-%d' )
    
    # Compute number of days relative 
    # to placeholder date
    dat_int = difftime( x[entries],
                        phd,
                        units = 'days' )
    dat_int = as.integer( dat_int )
    
    # Remove instances of placeholder date
    no_zeros = dat_int != 0
    
    if ( any( no_zeros ) ) {
      
      # Earliest date
      min_date = min( dat_int[ no_zeros ] )
      
      # Number of days since earliest
      out[no_zeros] = dat_int[no_zeros] - min_date
      
      # Increments in days between time points
      if ( type == 2 ) {
        
        # Viable time points
        val = out[no_zeros]
        new_out = rep( 0, length( val ) )
        check = rep( F, length( val ) )
        if ( !is.null( time_points ) ) {
          time_points = time_points[entries][no_zeros]
        }
        
        # If increments between days can be computed
        if ( sum( no_zeros ) > 1 ) {
          
          # Loop over time points
          for ( j in 1:length( val ) ) {
            
            # Check if any earlier days
            sel = ( val[-j] - val[j] ) < 0
            # Check if any repeat dates
            check[j] = any( val[-j] %in% val[j] )
            
            # Compute increment
            if ( any( sel ) ) {
              
              new_out[j] = val[j] - max( val[-j][sel] )
              if ( check[j] ) {
                
                if ( is.null( time_points ) ) {
                  
                  if ( any( which( val == val[j] ) < j ) ) {
                    new_out[j] = 0
                  }
                  
                } else {
                  
                  if ( any( time_points[ val == val[j] ] < j ) ) {
                    new_out[j] = 0
                  }
                  
                }
                
              }
              
            }
          }
        } else {
          new_out = 0
        }
        
        out[no_zeros] = new_out
      }
      
    }
    
    return( out )
  }
  
  # 2.5) 
  process_merge = function( vrb_to_merge, init = NA, missing = "" ) {
    # Purpose:
    # A function to merge together multiple variables, 
    # combining non-overlapping data into a single vector.
    # Arguments:
    # vrb_to_merge - A list with the vectors (of matching
    #                length) to merge
    # default      - The default output value
    # missing      - A vector with additional values to 
    #                treat as missing
    # Returns:
    # A single vector of combined values.
    
    K = length( vrb_to_merge )
    n = length( vrb_to_merge[[1]] )
    out = rep( init, n )
    
    for ( k in 1:K ) {
      
      sel = 
        is.na( out ) | 
        out %in% missing
      out[sel] = vrb_to_merge[[k]][sel]
      
    }
    
    return( out )
  }
  
  # 2.6) 
  process_find_closest = function( x, 
                                   time_point, 
                                   entries, 
                                   init = NA, 
                                   ref = 1,
                                   type = 'earliest' ) {
    # Purpose:
    # Attempts to find the value at the closest 
    # time point compared to a reference time point.
    # Arguments:
    # x          - A vector of values
    # time_point - A vector of time points
    # entries    - A logical vector indicating the 
    #              subset of cases to consider
    # init       - The default output
    # ref        - The reference time point
    # type       - The time point to use in case of 
    #              tied values.
    # Returns:
    # A single value
    
    out = init
    
    cur_x = x[entries]
    cur_tp = time_point[entries]
    no_na = !is.na( cur_x )
    distance = cur_tp - ref
    
    if ( any( no_na ) ) {
      
      if ( sum( no_na ) > 1 ) {
        
        if ( 0 %in% distance[ no_na ] ) {
          
          out = cur_x[ distance == 0 ]
          
        } else {
          
          
          final_x = cur_x[no_na]
          final_dis = abs( distance[no_na] )
          
          if ( type == 'earliest' ) {
            
            sel = min( which(
              min( final_dis ) == final_dis
            ) )
            
            out = final_x[sel]
            
          }
          
          
          if ( type == 'latest' ) {
            
            sel = max( which(
              min( final_dis ) == final_dis
            ) )
            
            out = final_x[sel]
            
          }
          
        }
        
      } else {
        out = cur_x[no_na]
      }
    }
    
    return( out )
  }
  
  # 2.7) 
  process_usage = function( df = NULL, lst = NULL ) {
    # Purpose:
    # Recodes lifetime and current frequency of use 
    # for a substance into a set of discrete 
    # categories (i.e., 'Never tried', 'Previous', 
    # 'Current (1 - 3 days)', 'Current (4 - 7 days)' )
    # Arguments:
    # df   - A data frame with the observations and 
    #        a variable 'IDS.INT.Screen' for the 
    #        screening ID
    # lst  - A named list with the label and identifiers 
    #        for 'lifetime' and 'current' variables
    # Returns:
    # Either a template for the 'lst' variable, or a 
    # a character string with the categories.
    
    # Categories
    
    #          | Past 3 months | 
    #          | or            | 
    # Lifetime | Past 30 days  | Category
    # None     | NA/0          | Never tried
    # Any      | < 1 per Wk    | Previous
    # Any      | 1 - 3 per Wk  | Current (1 - 3 days a week)
    # Any      | 4 - 7 per Wk  | Current (4 - 7 days a week)
    # Any NA   | Any NA        | Missing
    # None     | > 0           | Uncategorized
    
    out = NULL
    
    if ( !is.null( df ) & !is.null( lst ) ) {
      
      df$Lifetime = df[[ lst$lifetime$label ]]
      df$Current = df[[ lst$current$label ]]
      
      tmp = df %>% 
        group_by( 
          Lifetime, 
          Current
        ) %>% 
        summarise(
          N = length( IDS.INT.Screen )
        ) %>% 
        data.frame( stringsAsFactors = F )
      
      # Initialize column with categories
      tmp$Usage = 'Uncategorized'
      
      sel = 
        !is.na( tmp$Lifetime ) & 
        tmp$Lifetime %in% lst$lifetime$never & 
        ( is.na( tmp$Current ) | 
          tmp$Current %in% lst$current$never )
      tmp$Usage[sel] = 'Never tried'
      
      sel = 
        !is.na( tmp$Lifetime ) & 
        tmp$Lifetime %in% lst$lifetime$previous & 
        !is.na( tmp$Current ) & 
        tmp$Current %in% lst$current$previous
      tmp$Usage[sel] = 'Previous'
      
      sel = 
        !is.na( tmp$Lifetime ) & 
        tmp$Lifetime %in% lst$lifetime$week_1_3 & 
        !is.na( tmp$Current ) & 
        tmp$Current %in% lst$current$week_1_3
      tmp$Usage[sel] = 'Current (1 - 3 days)'
      
      sel = 
        !is.na( tmp$Lifetime ) & 
        tmp$Lifetime %in% lst$lifetime$week_4_7 & 
        !is.na( tmp$Current ) & 
        tmp$Current %in% lst$current$week_4_7
      tmp$Usage[sel] = 'Current (4 - 7 days)'
      
      # Missing data
      sel = findNA( tmp ) & tmp$Usage != 'Never tried'
      tmp$Usage[sel] = 'Missing'
      
      # Initialize categories
      out = rep( 'Missing', nrow( df ) )
      
      # NA values ill-suited for conditional logic 
      # so recode
      df$Lifetime[ is.na( df$Lifetime ) ] = 999
      tmp$Lifetime[ is.na( tmp$Lifetime ) ] = 999
      df$Current[ is.na( df$Current ) ] = 999
      tmp$Current[ is.na( tmp$Current ) ] = 999
      
      # Number of entries
      n_cases = nrow( tmp )
      
      # Convert to lists for speed
      tmp = as.list( tmp )
      df = as.list( df )
      
      # Loop over cases
      for ( i in 1:n_cases ) {
        
        # Identify matching cases
        entries = 
          df$Lifetime == tmp$Lifetime[i] & 
          df$Current == tmp$Current[i]
        
        out[entries] = tmp$Usage[i]
        
      }
      
      # Convert back to data frames
      df = data.frame( df, stringsAsFactors = F )
      
    } else {
      
      out = list(
        lifetime = list(
          label = '',
          never = 0,
          previous = c( 1:5 ),
          week_1_3 = c( 1:5 ),
          week_4_7 = c( 1:5 )
        ),
        current = list(
          label = '',
          never = 0,
          previous = c( 0:2 ),
          week_1_3 = c( 3:4 ),
          week_4_7 = c( 5:6 )
        )
      )
      
    }
    
    return( out )
  }
  
} else {
  
  # Removes functions from workspace
  rm( list = function_names )
  
  message( 'Unloading processing functions' )
  
}

# Clean up workspace
rm( function_names, check )
