library(dplyr)
library(openxlsx)
library(rmarkdown)

### Note this is different from RunAllAnalyses
cleanDt <- function(x){
  gsub("/", "_", x, fixed = TRUE)
}

cleanIdx <- function(x){
  ifelse(x < 10, paste0("0", trimws(x)), x)
}

createTemplateConfigEnv <- function(...){
  args <- list(...)
  paramEnv <- new.env()
  for (n in names(args)){
    paramEnv[[n]] <- args[[n]]
  }
  return(paramEnv)
}


##########################################################
date_run <- format(Sys.Date(), "%m/%d/%Y")
date_run <- cleanDt(date_run)
states_to_run <- c(14, 15, 16, 17, 23, 24, 
                   26, 28, 35, 36, 42, 50)
intervention_types <- 1:3
state_key <- read.xlsx("../Data/state_key.xlsx") %>% 
  filter(state_idx %in% states_to_run)

apply(state_key, 1, function(x){
  te <- createTemplateConfigEnv(TEMPLATE_STATE_NAME = x[["state"]],
                                TEMPLATE_STATE_IDX = x[["state_idx"]],
                                TEMPLATE_INTERVENTION_TYPES = intervention_types)
  report_file_name <- paste0("../Reports/", Sys.Date(),
                             "/", te$TEMPLATE_STATE_IDX,"_", date_run)
  
  render("../Templates/R0_State_Template.Rmd", 
         output_file = report_file_name, 
         envir = te)
})