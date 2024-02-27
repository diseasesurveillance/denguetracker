library(denguetracker)
library(lubridate)

#setwd(yourwd)

brazil_ufs <- c(
  "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", 
  "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", 
  "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"
)

last_ew_start <- Sys.Date() - wday(today) + 1

for (uf in brazil_ufs) {
  
  infodengue_data <- denguetracker::fetch_data_from_state(uf,
                                                          ey_start=2018,
                                                          ey_end=2024)
  filename <- sprintf("%s_%s_infodengue.csv", uf, last_ew_start)
  file_path <- paste0("weekly_data/infodengue/",filename)
  write.csv(infodengue_data, file_path, row.names = F)
  cat("\nSuccessfully saved ", filename, "\n")
  
}
