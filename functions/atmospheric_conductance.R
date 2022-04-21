
atmos_conduct <- function(h, v, k_d = 0.7, k_0 = 0.1, z_m = NULL){
        z_d = k_d *h
        z_0 = k_0 *h
        if(is.null(z_m)){
          z_m = 200 + h
        }
        c_at = v / (6.25 * log((z_m-z_d)/z_0)^2)
        return(list(atmospheric_conductance = c_at))
}