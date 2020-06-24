################################################
##  Template for statewide mortality analyses ##
################################################

####################
## Globals/config ##
####################

# Fatality proportion - currently not able to be stratified by age
reportFraction <- 0.02
# Number of samples 
nSamples <- 100


#####################
## Required inputs ##
#####################

required_globals <- c("state",
                      "cores",
                      "isdebug",
                      "interventionDate", 
                      "reopenDate",
                      "intervType",
                      "stateDataFiltered",
                      "instate")

tmp <- sapply(required_globals, function(x){if (!exists(x)){stop("Missing: ", x)}})


# Data Model: new deaths, modeled as fraction of new "removed" cases
data_model_1 = DataModel(stateDataFiltered$deathsNonCum,
                         type = "fractional",
                         compartment="R_star",
                         cumulative=FALSE, 
                         params = list(report_fraction = reportFraction, report_fraction_ess = 200))

# Determine Exposure Model based on intervType

# Exposure model: spline basis beginning on epidemic response date 

# Check for valid intervention type
if (!intervType %in% 1:5){
  stop("Expecting Intervention of type 1 to 5")
}

if (intervType == 1){
  # Shift @ start and stop
  X <- cbind(1, 
             1*(stateDataFiltered$date >= interventionDate), 
             1*(stateDataFiltered$date >= reopenDate))
} else if (intervType == 2){
  # Piecewise linear @ start and stop
  X <- cbind(1, cumsum(stateDataFiltered$date >= interventionDate)/7, 
             cumsum((stateDataFiltered$date >= reopenDate))/7) # Weekly scale
} else if (intervType == 3){
  # Spline basis after intervention
  c1 <- cumsum(1*((stateDataFiltered$date >= interventionDate)))
  sharedBasis <- bs(0:200, degree = 4) 
  X <- as.matrix(cbind(1,predict(sharedBasis,c1)))
} else if (intervType == 4){
  # Base epidemic intensity on Google mobility - workplace
  
  #download.file(url = "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=6d352e35dcffafce",
  #              destfile = "mobility.csv")
  
  mobility <- read.csv("../Data/mobility.csv", stringsAsFactors = FALSE)
  # Figure out sub-region
  mobility <- filter(mobility, country_region == "United States" & sub_region_1 == instate)
  if (nrow(mobility) == 0){
    stop("Mobility data not found for: ", instate)
  }
  mobility <- mobility %>% 
    group_by(date) %>%
    summarize(parks_mean = mean(parks_percent_change_from_baseline, na.rm=TRUE),
              retail_mean = mean(retail_and_recreation_percent_change_from_baseline, na.rm=TRUE), 
              transit_mean = mean(transit_stations_percent_change_from_baseline, na.rm=TRUE),
              residential_mean = mean(residential_percent_change_from_baseline, na.rm=TRUE),
              grocery_mean = mean(grocery_and_pharmacy_percent_change_from_baseline, na.rm=TRUE),
              workplace_mean = mean(workplaces_percent_change_from_baseline, na.rm=TRUE))
  
  # Throw away last few days of mobility
  mobility <- filter(mobility, as.Date(date) <= as.Date(max(date))-3)
  
  # Naiive assumption - assume that, moving forward, mobility stays at the previous week average. 
  X <- cbind(1, rep(NA, nrow(stateDataFiltered)))
  lastWkAvg <- mean(mobility$workplace_mean[order(mobility$date,decreasing = TRUE)][1:7])
  for (i in 1:nrow(X)){
    if (stateDataFiltered$date[i] %in% as.Date(mobility$date)){
      idx <- which(as.Date(mobility$date)==stateDataFiltered$date[i]) 
      X[i,2] <- mobility$workplace_mean[idx]
    } else{
      X[i,2] <- lastWkAvg
    }
  }
  # Normalize
  X[,2] <- X[,2]/max(abs(X[,2]))
} else if (intervType == 5){
  # Rec mobility
  
  mobility <- read.csv("../Data/mobility.csv", stringsAsFactors = FALSE)
  # Figure out sub-region
  mobility <- filter(mobility, country_region == "United States" & sub_region_1 == instate)
  if (nrow(mobility) == 0){
    stop("Mobility data not found for: ", instate)
  }
  mobility <- mobility %>% 
    group_by(date) %>%
    summarize(parks_mean = mean(parks_percent_change_from_baseline, na.rm=TRUE),
              retail_mean = mean(retail_and_recreation_percent_change_from_baseline, na.rm=TRUE), 
              transit_mean = mean(transit_stations_percent_change_from_baseline, na.rm=TRUE),
              residential_mean = mean(residential_percent_change_from_baseline, na.rm=TRUE),
              grocery_mean = mean(grocery_and_pharmacy_percent_change_from_baseline, na.rm=TRUE),
              workplace_mean = mean(workplaces_percent_change_from_baseline, na.rm=TRUE))
  
  # Throw away last few days of mobility
  mobility <- filter(mobility, as.Date(date) <= as.Date(max(date))-3)
  
  
  # Naiive assumption - assume that, moving forward, mobility stays at the previous week average. 
  X <- cbind(1, rep(NA, nrow(stateDataFiltered)))
  lastWkAvg <- mean(mobility$retail_mean[order(mobility$date,decreasing = TRUE)][1:7])
  for (i in 1:nrow(X)){
    if (stateDataFiltered$date[i] %in% as.Date(mobility$date)){
      idx <- which(as.Date(mobility$date)==stateDataFiltered$date[i]) 
      X[i,2] <- mobility$retail_mean[idx]
    } else{
      X[i,2] <- lastWkAvg
    }
  }
  X[,2] <- X[,2]/max(abs(X[,2]))
}

exposure_model = ExposureModel(X,
                               nTpt = nrow(stateDataFiltered),
                               nLoc = 1,
                               betaPriorPrecision = 0.5,
                               betaPriorMean = 0)

# There's no reinfection in this case, so we just use a "SEIR" model. 
reinfection_model = ReinfectionModel("SEIR")

# we have no distance model, because it's a non-spatial analysis
distance_model = DistanceModel(list(matrix(0)))

# Set initial population sizes
initial_value_container = InitialValueContainer(S0=statePop,
                                                E0=10,
                                                I0=10,
                                                R0=0, type = "uniform", 
                                                params = list(max_S0 = statePop + 100,
                                                              max_E0 = 300,
                                                              max_I0 = 300,
                                                              max_R0 = 2))


# Model to describe E to I and I to R transition probabilities.

# Latent period: 2-14 days with median 5


pickWeibullPars <- function(qdf){
  rslt <- optim(par = c(1,1), fn = function(par){
    sum((qweibull(p = qdf$q, shape = par[1], scale = par[2]) - qdf$x)^2)
  })
  rslt$par
}

pickGammaHyperPars <- function(mean, ESS){
  b <- ESS/(mean+1)
  a <- ESS - b
  c(a,b)
}

latent_par_means <- pickWeibullPars(qdf=data.frame(q=c(0.025,0.5,0.975),
                                                   x=c(2,5,14)))
infectious_par_means <- pickWeibullPars(qdf = data.frame(q=c(0.025,0.5,0.975),
                                                         x = c(10,14,32)))

Transition_priors <- WeibullTransitionPriors(latent_shape_prior_alpha = pickGammaHyperPars(latent_par_means[1], 1000)[1],
                                            latent_shape_prior_beta = pickGammaHyperPars(latent_par_means[1], 1000)[2],
                                            latent_scale_prior_alpha = pickGammaHyperPars(latent_par_means[2], 1000)[1],
                                            latent_scale_prior_beta = pickGammaHyperPars(latent_par_means[2], 1000)[2],
                                            infectious_shape_prior_alpha = pickGammaHyperPars(infectious_par_means[1], 100)[1],
                                            infectious_shape_prior_beta = pickGammaHyperPars(infectious_par_means[1], 100)[2],
                                            infectious_scale_prior_alpha = pickGammaHyperPars(infectious_par_means[2], 100)[1],
                                            infectious_scale_prior_beta = pickGammaHyperPars(infectious_par_means[2], 100)[2]) 


if (isdebug){
  sampling_control <- SamplingControl(seed = 123124, 
                                     n_cores = cores,
                                     algorithm="Beaumont2009",
                                     list(init_batch_size = 10000,
                                          batch_size = 1000,
                                          epochs = 2,
                                          max_batches = 2,
                                          shrinkage = 0.85,
                                          keep_compartments =TRUE
                                     ))
} else{
  sampling_control <- SamplingControl(seed = 123124, 
                                      n_cores = cores,
                                      algorithm="Beaumont2009",
                                      list(init_batch_size = 1000000,
                                           batch_size = 1000,
                                           epochs = 2,
                                           max_batches = 2,
                                           shrinkage = 0.85,
                                           keep_compartments =TRUE
                                      ))
}
