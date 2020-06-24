print("Updating Mobility Data")
download.file(url = "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=6d352e35dcffafce",
              destfile = "../Data/mobility.csv")
print("Updating NYT Data")
setwd("../Data/covid-19-data/")
system("git pull")
print("Updating JH Data")
setwd("../COVID-19/")
system("git pull")