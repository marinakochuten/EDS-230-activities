#' Compute energy produced from a photovoltaic system given average annual solar radiation
#'
#' @param A Solar panel area (m2)
#' @param H Annual average solar radiation (kWh)
#' @param r Panel yield (0-1) (manufacture efficiency - usually around 0.2)
#' @param PR Performance ratio (0-1) (accounting for site factors that impact efficiency usually around 0.75)
#'
#' @return Energy (kWh)
#'
#' @examples
#' # Example with default r and PR values
#' energy <- pv_energy_production(A = 20, H = 1000)
#' 
#' # Example with custom r and PR values
#' energy_custom <- pv_energy_production(A = 20, H = 1000, r = 0.1, PR = 0.5)

pv_energy_production <- function(A, H, r = 0.2, PR = 0.75) {
  
  # Error messages
  if(!is.numeric(A) | A < 0){
    stop("'A' must be a positive number")
  }
  
  if(!is.numeric(H) | H < 0){
    stop("'H' must be a positive number")
  }
  
  if(!is.numeric(r) | r < 0 | r > 1){
    stop("'r' must be a number between 0 and 1")
  }
  
  if(!is.numeric(PR) | PR < 0 | PR > 1){
    stop("'PR' must be a number between 0 and 1")
  }
  
  # Calculate energy produced (E) and return E
  E <- A * r * H * PR
  return(E)
  
}
