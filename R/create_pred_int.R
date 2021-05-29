#' Create predictions intervals from mrgsim result
#' @param x mrgsim result (must contain an event-ID column)
#' @param dv_column name of the column containing the dependent variable
#' @param time name of the column containing the longitudinal (independent) variable
#' @param id_column name of the column containing ID
#' @param evid_column name of the column containing event-ID
#' @returns a data.frame containing the independent variable, the 2.5th, 5th, 25th, 50th, 75th, 95th and 97.5th percentile of the dependent variable
#' @examples
#' data(ruxSim)
#' create_pred_int(ruxSim)
#' @import tidyverse
#' @import mrgsolve
#' @export

create_pred_int <- function(x,
                            dv_column="IPRED",
                            time_column="time",
                            id_column="ID",
                            evid_column="evid") {

  x %>%  as_tibble -> x_tibble

  x_tibble[x_tibble[evid_column]==0,] -> x_tibble_obs_only

  ids <- x_tibble_obs_only[id_column] %>% unique() %>% simplify() %>% unname()
  max_ids <- length(ids)

  n_observations <- x_tibble_obs_only[ x_tibble_obs_only[id_column] == 1,] %>% nrow()
  temp_time <-  x_tibble_obs_only[ x_tibble_obs_only[id_column] == 1,][time_column] %>% simplify() %>% unname()

  ## reserve memory for the matrix
  empty_data <- matrix(nrow=n_observations, ncol=max_ids, byrow = T)

  for (k in 1:max_ids) {
    empty_data[,k] <-x_tibble_obs_only[ x_tibble_obs_only[id_column] == ids[k],] [[dv_column]]

  }

  ## Retrieve quantiles of MC simulation
  sim_quantiles <- apply(t(empty_data),2,function(x) quantile(x,probs=c(0.025,
                                                                        0.975,
                                                                        0.5,
                                                                        0.05,
                                                                        0.95,
                                                                        0.25,
                                                                        0.75)
  )
  )

  result <- data.frame(time=temp_time, perc_2.5=sim_quantiles[1,],perc_97.5=sim_quantiles[2,], median=sim_quantiles[3,],
                       perc_5=sim_quantiles[4,],perc_95=sim_quantiles[5,],
                       perc_25=sim_quantiles[6,],perc_75=sim_quantiles[7,])

  class(result) <- c("data.frame", "open_sim")

  return(result)

}
