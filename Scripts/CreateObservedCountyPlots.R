library(rmarkdown)
createTemplateConfigEnv <- function(...){
  args <- list(...)
  paramEnv <- new.env()
  for (n in names(args)){
    paramEnv[[n]] <- args[[n]]
  }
  return(paramEnv)
}


###########################################################

## config
date_run <- Sys.Date()
states <- c("iowa",
            "indiana",
            "kansas",
            "illinois",
            "michigan",
            "minnesota",
            "missouri",
            "nebraska",
            "north dakota",
            "ohio",
            "south dakota",
            "wisconsin")
# states <- "iowa"
START_DATE <- "2020-01-01" ## Assumed to be a single value
END_DATE <- "2020-06-01"

rprt_dir <- paste0("../Reports/ObservedCounty/", date_run)

if(!dir.exists(rprt_dir)){
  dir.create(rprt_dir)
}

file.copy("../Templates/ExploratoryCountyTemplate.Rmd", 
          paste0(rprt_dir, "/ExploratoryCountyTemplate_cp.Rmd"),
          overwrite = TRUE)

for(s in states){
  te <- createTemplateConfigEnv(TEMPLATE_STATE = s,
                                START_DATE = START_DATE,
                                END_DATE = END_DATE)
  render(paste0(rprt_dir, "/ExploratoryCountyTemplate_cp.Rmd"),
         output_file = s,
         envir = te)
  # run(paste0(rprt_dir, "/ExploratoryCountyTemplate_cp.Rmd"),
  #     render_args = list("envir" = te, "output_file" = s))
  # deployApp(appDir = rprt_dir, appTitle = s)
}