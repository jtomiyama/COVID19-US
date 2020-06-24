#####################################
## Script to trigger all analyses. ##
#####################################
library(digest)
library(dplyr)
library(openxlsx)

## Declare simulation type and scope ##
# Declare argon opts
argon_opts <- "-q UI,all.q"
# Running gon HPC?
is.argon <- TRUE
# Is this a debug run?
isDebug <- FALSE
# Determine the simulation grid
states_to_run <- c(16)
intervention_types_to_run <- c(1,2,3,4,5)
templates_to_use <- c("default.template.mortality.R")
analysis_date <- Sys.Date()
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
  inner_join(states_to_run, by="state")

grid <- expand.grid(template=templates_to_use,
            intervention=intervention_types_to_run, 
            state=states_to_run$state, stringsAsFactors = FALSE) %>% full_join(interv, by = "state") %>%
  select(-fips, -curated)

# short unique identifiers for templates
grid$template_short <- vapply(as.character(grid$template), function(x){
  substr(digest::digest(x, algo = "md5"),1,6)},"STRING")

cleanDt <- function(x){
  gsub("/", ".", x, fixed = TRUE)
}
grid$outputfile <- apply(grid, 1, function(x){
  paste0(paste0("../Results/", analysis_date, "/"), 
         paste0(paste0(c(x[["state_idx"]], 
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
