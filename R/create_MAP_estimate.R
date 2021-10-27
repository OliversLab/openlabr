#' Create MAP Estimates for an individual using an mrgsolve model
#' @param x mrgsim model to be used in the simulation (must contain parameters called ETA1 ... ETAX)
#' @param tdm_data contains covariates, dosing events and observed concentration
#' @param ycol_name column name of the observed concentration in tdm_data
#' @param dvcol_name name of simulation result
#' @param init_etas named vector of initial values for MAP bayes estimation (number and name of ETAS must conform with the model, names: ETA1 ... ETAX)
#' @import tidyverse
#' @import mrgsolve
#' @export

create_MAP_estimate <- function (x, tdm_data, ycol_name="CONC", dvcol_name="IPRED", init_etas){

  mapbayes <- function(eta,d,ycol,mod,dvcol) {
    sigma_m <- smat (mod)
    as.matrix (sigma_m) -> sigma_m
    sigma_m [1,1]-> prop
    sigma_m [2,2]-> add

    omega_m <- omat (mod)
    as.matrix (omega_m) -> omega_m
    solve (omega_m) -> omega_m_inv

    eta_m <- eta %>% matrix(nrow=1)
    mod <-  param(mod,eta)
    out <- mod %>% zero_re() %>% ev(d) %>%  carry_out(evid) %>%  mrgsim(output="df", end=-1, add=tdm_data$time) %>% filter(evid==0)
    # http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3339294/

    sig2j <- out[[dvcol]]^2*prop + add
    sqwres <- log(sig2j) + (1/sig2j)*(d[[ycol]] - out[[dvcol]])^2
    nOn <- diag(eta_m %*% omega_m_inv %*% t(eta_m))
    return(sum(sqwres,na.rm=TRUE) + nOn)
  }

  map_estimate <-optim (par= init_etas, fn=mapbayes, ycol=ycol_name, mod=x, d=tdm_data, dvcol=dvcol_name)

  map_estimate$par %>% as.data.frame() %>% t() -> map_estimate

  map_data <- cbind(tdm_data, map_estimate)

  return(map_data)

}

