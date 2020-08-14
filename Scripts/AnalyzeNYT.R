library(ABSEIR)
library(dplyr)
library(optparse)
library(splines)
library(openxlsx)
library(digest)

option_list <- list(
make_option(c("-s", "--state"), type = "character", 
            default = "16", help = "State to Analyze - index",
            metavar="character"),
make_option(c("-m", "--template"), type = "character", 
            default = "default.template.mortality.R", help = "Analysis Template File",
            metavar="character"),
make_option(c("-a", "--adate"), type = "character", 
            default = "2020-06-01", help = "Analysis Date (change from unchecked spread)",
            metavar="character"),
make_option(c("-d", "--date"), type = "character", 
            default = "2020-03-01", help = "Intervention Date (change from unchecked spread)",
            metavar="character"),
make_option(c("-r", "--reopen"), type = "character", 
            default = "2020-05-01", help = "Reopen Date (or closest thing thereto)",
            metavar="character"),
make_option(c("-t", "--type"), type = "character", 
            default = "3", help = "Intervention Type (0/1/2/3)",
            metavar="character"),
make_option(c("-b", "--isdebug"), type = "character", 
            default = "0", help = "Debug?",
            metavar="character"),
make_option(c("-o", "--outfile"), type = "character", 
            default = "tmp.rda", help = "output file",
            metavar="character"),
make_option(c("-z", "--cores"), type = "character", 
            default = "8", help = "cores",
            metavar="character")
)

opt_parser <- OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

#save("opt", file = paste0(substr(digest(opt, algo = "md5"),1,5), ".rda"))
state <- opt$state
interventionDate <- as.Date(opt$date, format="%m/%d/%Y")
reopenDate <- as.Date(opt$reopen, format="%m/%d/%Y")
analysisDate <- as.Date(opt$adate)
resultsFileName <- opt$outfile
isdebug <- as.numeric(opt$isdebug) > 0
cores <- as.integer(opt$cores)
intervType <- as.integer(opt$type)

templateFile <- paste0("../Templates/", opt$template)
NPredDays <- 130 

if(!file.exists(templateFile)){
  stop(paste0("Template doesn't exist: ", templateFile))
}

stateData <- read.csv("../Data/covid-19-data/us-states.csv", stringsAsFactors = FALSE)
censusData <- read.xlsx("../Data/nst-est2019-01.xlsx")
uqStates <- sort(unique(intersect(gsub(".", "", censusData$State, fixed = TRUE), stateData$state)))
instate <- uqStates[as.numeric(opt$state)]
stateCenIdx <- which(gsub(".", "", censusData$State, fixed = TRUE) == instate)

if (length(stateCenIdx) == 0){
  stop("State not found in Census Data: ", instate)  
}

statePop <- censusData[stateCenIdx,]$`2019`

if (file.exists(resultsFileName)){
  print("Results file already exists, skipping.")
} else { 
  
  stateDataFiltered <- filter(stateData, state == instate) %>% arrange(date) %>%
    mutate(date = as.Date(date,format = "%Y-%m-%d")) %>% 
    mutate(idxDate = as.numeric(date - min(date))) %>%
    mutate(deathsNonCum = c(deaths[1], diff(deaths))) %>% 
    mutate(casesNonCum = c(cases[1], diff(cases))) %>%
    select(date, cases, deaths, idxDate, deathsNonCum, casesNonCum) 
  
  ### Differentiate between the observed data and the data for analysis
  ### In the event analysis date is in the past
  stateDataObserved <- stateDataFiltered
  stateDataFiltered <- statDataFiltered %>% filter(date <= analysisDate)
  
  minIdx <- max(1, min(which(stateDataFiltered$casesNonCum > 0))-7)
  stateDataFiltered <- stateDataFiltered[minIdx:nrow(stateDataFiltered),]
  
  #padWeeks
  padData <- data.frame(date = max(stateDataFiltered$date) + 1:NPredDays)
  stateDataFiltered <<- full_join(stateDataFiltered, padData, by = "date")
  stateDataFiltered$idxDate <- as.numeric(stateDataFiltered$date - min(stateDataFiltered$date))
  
  # Fill in any zero counts
  nullData <- data.frame(idxDate = min(stateDataFiltered$idxDate):max(stateDataFiltered$idxDate), 
                         cases = NA, deaths = NA, deathsNonCum = 0, casesNonCum = 0)
  nullData <- nullData[!(nullData$idxDate %in% stateDataFiltered$idxDate),]
  if (nrow(nullData) > 0){
    stateDataFiltered <- full_join(stateDataFiltered, nullData, by = "idxDate")
  }
  
  
  source(templateFile)
  result <- SpatialSEIRModel(data_model_1,
                            exposure_model,
                            reinfection_model,
                            distance_model,
                            transition_priors = Transition_priors,
                            initial_value_container,
                            sampling_control,
                            samples = nSamples,
                            verbose = 2)
  
  simulated = epidemic.simulations(result, replicates = 2)
  tmp <- structure(list(modelObject = result, simulationResults = result$simulationResults, 
                        params = result$param.samples), class = "PosteriorSimulation")
  R0 <- ComputeR0(SimObject = simulated, cores = 1)
  R0post <- ComputeR0(SimObject = tmp, cores = 1)
  
  single_state_results <- list(data = stateDataFiltered, 
                               result = R0post, 
                               sims = R0,
                               location = state)
  save("single_state_results", "stateDataObserved", "opt", file = resultsFileName, compress = "bzip2")
} 


