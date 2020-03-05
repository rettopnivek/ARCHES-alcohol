# Inventory measures
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you 
# have any questions or comments
# Last updated 2020-02-10

# Table of contents
# 1) Initial extraction of variables
#   1.1) AUDIT
#   1.2) CUDIT
#   1.3) MASQ
#   1.4) UPPS
#   1.5) MMM
# 2) Final adjustments
#   2.1) Propogate baseline values

message( 'Processing for inventory measures' )

###
### 1) Initial extraction of variables
###

# 1.1) AUDIT

dat$INV.INT.AUDIT.TOT = c(
  both_studies$Active$data$audit,
  both_studies$Pilot$data$audit
)
attributes( dat$INV.INT.AUDIT.TOT ) = list(
  measure = "Alcohol Use Disorder Identification Test (AUDIT)",
  max = 40,
  min = 0,
  items = 10
)

# 1.2) CUDIT

dat$INV.INT.CUDIT.TOT = c(
  both_studies$Active$data$cudit_total,
  both_studies$Pilot$data$cudit_total
)
attributes( dat$INV.INT.CUDIT.TOT ) = list(
  measure = "Cannabis Use Disorder Identification Test (CUDIT)",
  max = 32,
  min = 0,
  items = 8
)

# 1.3) MASQ

dat$INV.INT.MASQ.GDA = c(
  both_studies$Active$data$masq_score_gad,
  both_studies$Pilot$data$masq_score_gad
)
attributes( dat$INV.INT.MASQ.GDA ) = list(
  measure = "Mood and Anxiety Symptoms Questionnaire",
  subscale = "General distress - anxiety",
  max = 55,
  min = 11,
  items = 11
)

dat$INV.INT.MASQ.GDD = c(
  both_studies$Active$data$masq_score_gdd,
  both_studies$Pilot$data$masq_score_gdd
)
attributes( dat$INV.INT.MASQ.GDD ) = list(
  measure = "Mood and Anxiety Symptoms Questionnaire",
  subscale = "General distress - depression",
  max = 60,
  min = 12,
  items = 12
)

dat$INV.INT.MASQ.AA = c(
  both_studies$Active$data$masq_score_aa,
  both_studies$Pilot$data$masq_score_aa
)
attributes( dat$INV.INT.MASQ.AA ) = list(
  measure = "Mood and Anxiety Symptoms Questionnaire",
  subscale = "Anxious arousal",
  max = 85,
  min = 17,
  items = 17
)

dat$INV.INT.MASQ.AD = c(
  both_studies$Active$data$masq_score_ad,
  both_studies$Pilot$data$masq_score_ad
)
attributes( dat$INV.INT.MASQ.AD ) = list(
  measure = "Mood and Anxiety Symptoms Questionnaire",
  subscale = "Anhedonic depression",
  max = 110,
  min = 22,
  items = 22
)

# 1.4) UPPS

dat$INV.INT.UPPS.URG = c(
  both_studies$Active$data$upps_urgency,
  both_studies$Pilot$data$upps_urgency
)
attributes( dat$INV.INT.UPPS.URG ) = list(
  measure = "UPPS Impulsive Behavior Scale",
  subscale = "Negative urgency",
  max = 48,
  min = 12
)

dat$INV.INT.UPPS.PRE = c(
  both_studies$Active$data$upps_premeditation,
  both_studies$Pilot$data$upps_premeditation
)
attributes( dat$INV.INT.UPPS.PRE ) = list(
  measure = "UPPS Impulsive Behavior Scale",
  subscale = "Premediation (lack of)",
  max = 44,
  min = 11,
  items = 11
)


dat$INV.INT.UPPS.PER = c(
  both_studies$Active$data$upps_perseverance,
  both_studies$Pilot$data$upps_perseverance
)
attributes( dat$INV.INT.UPPS.PER ) = list(
  measure = "UPPS Impulsive Behavior Scale",
  subscale = "Perserverance (lack of)",
  max = 40,
  min = 10,
  items = 10
)

dat$INV.INT.UPPS.SS = c(
  both_studies$Active$data$upps_ss,
  both_studies$Pilot$data$upps_ss
)
attributes( dat$INV.INT.UPPS.SS ) = list(
  measure = "UPPS Impulsive Behavior Scale",
  subscale = "Sensation seeking",
  max = 48,
  min = 12,
  items = 12
)

dat$INV.INT.UPPS.POS = c(
  both_studies$Active$data$upps_posurgency,
  both_studies$Pilot$data$upps_posurgency
)
attributes( dat$INV.INT.UPPS.POS ) = list(
  measure = "UPPS Impulsive Behavior Scale",
  subscale = "Positive urgency",
  max = 56,
  min = 14,
  items = 14
)

# 1.5) MMM

dat$INV.DBL.MMM.COP = c(
  both_studies$Active$data$mmm_coping,
  both_studies$Pilot$data$mmm_coping
)
attributes( dat$INV.DBL.MMM.COP ) = list(
  measure = "Marijuana Motives Measure (MMM)",
  subscale = "Coping",
  max = 5,
  min = 1,
  items = 5
)

dat$INV.DBL.MMM.CON = c(
  both_studies$Active$data$mmm_conformity,
  both_studies$Pilot$data$mmm_conformity
)
attributes( dat$INV.DBL.MMM.CON ) = list(
  measure = "Marijuana Motives Measure (MMM)",
  subscale = "Conformity",
  max = 5,
  min = 1,
  items = 5
)

dat$INV.DBL.MMM.SOC = c(
  both_studies$Active$data$mmm_social,
  both_studies$Pilot$data$mmm_social
)
attributes( dat$INV.DBL.MMM.SOC ) = list(
  measure = "Marijuana Motives Measure (MMM)",
  subscale = "Social",
  max = 5,
  min = 1,
  items = 5
)

dat$INV.DBL.MMM.ENH = c(
  both_studies$Active$data$mmm_enhancement,
  both_studies$Pilot$data$mmm_enhancement
)
attributes( dat$INV.DBL.MMM.ENH ) = list(
  measure = "Marijuana Motives Measure (MMM)",
  subscale = "Enhancement",
  max = 5,
  min = 1,
  items = 5
)

dat$INV.DBL.MMM.EXP = c(
  both_studies$Active$data$mmm_expansion,
  both_studies$Pilot$data$mmm_expansion
)
attributes( dat$INV.DBL.MMM.EXP ) = list(
  measure = "Marijuana Motives Measure (MMM)",
  subscale = "Expansion",
  max = 5,
  min = 1,
  items = 5
)

###
### 2) Final adjustments
###

# 2.1) Propogate baseline values

# Extract subject IDs
ids = unique( dat$IDS.CHR.Subject )
ids = ids[ ids != "" ]
n_ids = length( ids )

# Inventory subscales
vrb = list(
  UPPS = dat %>% select( contains( 'UPPS.' ) ) %>% colnames,
  MMM = dat %>% select( contains( 'MMM.' ) ) %>% colnames
)

# Save version of AUDIT that respects original time points
dat$INV.INT.AUDIT.Time_points = 
  dat$INV.INT.AUDIT.TOT

# Create a progress bar using a base R function
pb = txtProgressBar( min = 1, max = n_ids, style = 3 )

# Convert to list for speed
dat = as.list( dat )

# Loop over IDs
for ( s in 1:n_ids ) {
  
  # Determine all entries for a subject 
  # using ID
  entries = dat$IDS.CHR.Subject == ids[s]
  
  # AUDIT
  
  # Check if data is from pilot study
  if ( any( dat$SSS.LGC.Pilot[entries] ) ) {
    # Take baseline value only
    
    val = process_find_closest(
      dat$INV.INT.AUDIT.TOT,
      dat$SSS.INT.Visit,
      entries,
      ref = 1
    )
    dat$INV.INT.AUDIT.TOT[entries] = val
    
  } else {
    # Propogate value
    
    val = process_find_unique_val(
      dat,
      'INV.INT.AUDIT.TOT',
      entries,
      default = NA
    )
    dat$INV.INT.AUDIT.TOT[entries] = val
    
  }
  
  # CUDIT
  
  val = process_find_unique_val(
    dat,
    'INV.INT.CUDIT.TOT',
    entries,
    default = NA
  )
  dat$INV.INT.CUDIT.TOT[entries] = val
  
  # MASQ
  
  # UPPS
  for ( k in 1:length( vrb$UPPS ) ) {
    val = process_find_unique_val(
      dat,
      vrb$UPPS[k],
      entries,
      default = NA
    )
    dat[[ vrb$UPPS[k] ]][entries] = val
  }
  
  # MMM
  for ( k in 1:length( vrb$MMM ) ) {
    val = process_find_unique_val(
      dat,
      vrb$MMM[k],
      entries,
      default = NA
    )
    dat[[ vrb$MMM[k] ]][entries] = val
  }
  
  # Update the progress bar
  setTxtProgressBar(pb,s)
}
close( pb )
rm( pb )

# Convert back to data frame
dat = data.frame( dat, stringsAsFactors = F )

# Non-users will not have values for 
# CUDIT and MMM
sel = 
  dat$LBL.Group == "Non-User" & 
  dat$ID.Subject != ""
dat$INV.DBL.MMM.CON[sel] = 0
dat$INV.DBL.MMM.COP[sel] = 0
dat$INV.DBL.MMM.ENH[sel] = 0
dat$INV.DBL.MMM.EXP[sel] = 0
dat$INV.DBL.MMM.SOC[sel] = 0
dat$INV.INT.CUDIT.TOT[sel] = 0

if ( FALSE ) {
  
  f = function(x) sum( !is.na( x ) )
  
  tmp = dat %>% 
    filter(
      IDS.CHR.Subject != ""
    ) %>% 
    group_by(
      SSS.LGC.Pilot,
      IDS.CHR.Subject
    ) %>% 
    select(
      contains( 'INV.' )
    ) %>%
    summarise_all(
      f
    ) %>% 
    as.data.frame
  
  rm( f, tmp )
}

# Clean up workspace
rm( vrb, entries, ids, n_ids, s, val, k )

message( 'Processing finished' )

