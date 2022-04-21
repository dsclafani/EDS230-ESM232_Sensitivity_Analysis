
#' Atmospheric Conductance calculation
#' This function computes atmospheric conductance based wind speed and vegetation characterisitics. 
#' @param h vegetation height (cm)
#' @param v wind speed (cm/s)
#' @param k_d constant parameter(default is 0.7)
#' @param k_0 constant parameter(default is 0.7)
#' @param z_m  height at which wind speed is measured - must be higher than the vegetation (cm), if NULL assumed to be 200 cm above the vegetation
#'
#' @return atmospheric_conductance(cm/s) 
#' @export
#'
#' @examples
#' atmos_conduct(1000, 250)
#' atmos_conduct(h = 1000, v = 250, k_d = 0.8, k_0 = 0.15, z_m = 300)
atmos_conduct <- function(h, v, k_d = 0.7, k_0 = 0.1, z_m = NULL){
        z_d = k_d *h
        z_0 = k_0 *h
        if(is.null(z_m)){
          z_m = 200 + h
        }
        c_at = v / (6.25 * log((z_m-z_d)/z_0)^2)
        return(list(atmospheric_conductance = c_at))
}