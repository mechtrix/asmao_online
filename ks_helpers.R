ks_drive_shaft <- function(dataset) {
  
  ks_data <- dataset %>% pull("diameter")
  
  ks_res <- ks.test(ks_data,"pnorm",mean = mean(ks_data), sd = sd(ks_data))
  
  out <- ks_res
  
  return(out)
  
  }