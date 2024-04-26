# Function that read the fleet register 
# zip file downloaded by the EU site 

read.fleet <- function(file){
  fleet <- read.table(unz(file, "vesselRegistryListResults.csv"), 
                      header=T,  sep=";", quote = "")
  fleet <- mutate(fleet, 
                  StartYear = substr(Event.Start.Date, 1, 4),
                  EndYear = substr(Event.End.Date, 1, 4))
  return(fleet)
}