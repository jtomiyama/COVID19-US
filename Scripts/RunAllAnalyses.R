#####################################
## Script to trigger all analyses. ##
#####################################
library(digest)
library(dplyr)
library(openxlsx)

## Declare simulation type and scope ##
# Declare argon opts
argon_opts <- "-q UI,all.q"
# Running on HPC?
is.argon <- TRUE
# Is this a debug run?
isDebug <- FALSE
# Determine the simulation grid
# refer to Data/state_key.xlsx
 states_to_run <- c(14, 15, 16, 17, 23, 24, 
                   26, 28, 35, 36, 42, 50)
intervention_types_to_run <- c(1,2,3,4,5)
mandate_type <- "SAHO"
templates_to_use <- c("default.template.mortality.R")
## Constrain ourselves to first part of the epidemic
analysis_date <- as.Date("2020-06-01")
if (!dir.exists(paste0("../Results/", analysis_date))){
  dir.create(paste0("../Results/", analysis_date))
}

## Update the datasets ##
system("Rscript UpdateData.R")

## Get intervention data ##
stateData <- read.csv("../Data/covid-19-data/us-states.csv", stringsAsFactors = FALSE)
censusData <- read.xlsx("../Data/nst-est2019-01.xlsx")
uqStates <- sort(unique(intersect(gsub(".", "", censusData$State, fixed = TRUE), stateData$state)))
states_to_run <- data.frame(state_idx=states_to_run,state=uqStates[states_to_run]) 

interv <- read.csv("../Data/intervention_info.csv") %>%
  filter(Type == mandate_type, curated == 1) %>%
  inner_join(states_to_run, by="state")

if(nrow(interv) > 0){
  grid <- expand.grid(template=templates_to_use,
                      intervention=intervention_types_to_run, 
                      state=states_to_run$state, stringsAsFactors = FALSE) %>% inner_join(interv, by = "state") %>%
    select(-fips, -curated)
  
  # short unique identifiers for templates
  grid$template_short <- vapply(as.character(grid$template), function(x){
    substr(digest::digest(x, algo = "md5"),1,6)},"STRING")
  
  cleanDt <- function(x){
    gsub("/", ".", x, fixed = TRUE)
  }
  
  grid$outputfile <- apply(grid, 1, function(x){
    dir_header <- paste0("../Results/",analysis_date, "/")
    
    clean_idx <- ifelse(x[["state_idx"]] < 10, 
                        paste0("0", trimws(x[["state_idx"]])), 
                        x[["state_idx"]])
    
    paste0(dir_header, 
           paste0(
             paste0(c(clean_idx, 
                      x[["intervention"]], 
                      cleanDt(x[["intervDate"]]), 
                      cleanDt(x[["reopenDate"]]), 
                      x[["template_short"]]), 
                    collapse = "_"), ".rda"))
    
  })
  
  submission_strings <- vapply(1:nrow(grid), FUN.VALUE = "string", FUN = function(i){
    args <- list(s = as.character(grid$state_idx[i]),
                 m = as.character(grid$template[i]),
                 d = as.character(grid$intervDate[i]),
                 r = as.character(grid$reopenDate[i]),
                 t = as.character(grid$intervention[i]),
                 b = 1*isDebug,
                 o = as.character(grid$outputfile[i]))
    
    if (is.argon){
      return(paste0("qsub -pe smp 16 -cwd ", argon_opts, " submitJob.sh ", paste0(args, collapse = " ")))
    } else {
      return(
        paste0("Rscript AnalyzeNYT.R ", 
               paste0("-", paste(names(args), unlist(args), sep =" "), collapse = " "))
        
      )
    }
  })
  
  
  
  
  for (i in 1:length(submission_strings)){
    print(submission_strings[i])
    system(submission_strings[i])
  } 
}

### If no intervention ###

no_interv <- anti_join(states_to_run, interv, by = "state")
if(nrow(no_interv) > 0){
  no_interv$template <- templates_to_use
  no_interv$intervention <- 0
  no_interv$template_short <- vapply(as.character(no_interv$template), function(x){
    substr(digest::digest(x, algo = "md5"),1,6)},"STRING")
  no_interv$outputfile <- apply(no_interv, 1, function(x){
    dir_header <- paste0("../Results/",analysis_date, "/")
    
    clean_idx <- ifelse(x[["state_idx"]] < 10, 
                        paste0("0", trimws(x[["state_idx"]])), 
                        x[["state_idx"]])
    
    paste0(dir_header, 
           paste0(
             paste0(c(clean_idx, 
                      x[["intervention"]], 
                      x[["template_short"]]), 
                    collapse = "_"), ".rda"))
    
  })
  
  ### Note: dates are not for the template to run but are necessary for 
  ### Argon script
  
  submission_strings2 <- vapply(1:nrow(no_interv), FUN.VALUE = "string", FUN = function(i){
    args <- list(s = as.character(no_interv$state_idx[i]),
                 m = as.character(no_interv$template[i]),
                 d = "2020-03-01",
                 r = "2020-05-01",
                 t = as.character(no_interv$intervention[i]),
                 b = 1*isDebug,
                 o = as.character(no_interv$outputfile[i]))
    
    if (is.argon){
      return(paste0("qsub -pe smp 16 -cwd ", argon_opts, " submitJob.sh ", paste0(args, collapse = " ")))
    } else {
      return(
        paste0("Rscript AnalyzeNYT.R ", 
               paste0("-", paste(names(args), unlist(args), sep =" "), collapse = " "))
        
      )
    }
  })
  
  for (i in 1:length(submission_strings2)){
    print(submission_strings2[i])
    system(submission_strings2[i])
  }
}
