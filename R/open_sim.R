
# x = mrgsim object
create_pred_int <- function(x, dv_column="IPRED", time_column="time", id_column="ID", evid_column="evid") {

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


create_pred_plot <- function (x, intervall_colour ="red",
                              median_colour="darkblue",
                              y_lab = "Plasma Concentration [ng/mL]",
                              x_lab = "Time [h]",
                              plot_title = "PK-Curve",
                              plot_subtitle = "oral",
                              text_size = 14) {
  create_pred_int(x) %>%
  ggplot(mapping = aes(x=time)) +  theme_bw() +
  geom_ribbon(aes(ymin=perc_2.5, ymax=perc_5, alpha="95 %"), fill=intervall_colour) +
  geom_ribbon(aes(ymin=perc_5, ymax=perc_25, alpha="90 %"), fill=intervall_colour) +
  geom_ribbon(aes(ymin=perc_25, ymax=median, alpha="75 %"), fill=intervall_colour) +
  geom_ribbon(aes(ymin=median, ymax=perc_75, alpha="75 %"), fill=intervall_colour) +
  geom_ribbon(aes(ymin=perc_75, ymax=perc_95, alpha="90 %"), fill=intervall_colour) +
  geom_ribbon(aes(ymin=perc_95, ymax=perc_97.5, alpha="95 %"), fill=intervall_colour) +
  geom_line(aes(y=median, colour="median"), lwd=1) +
  scale_alpha_manual(values = c("75 %"=.4,
                                "90 %"=.3,
                                "95 %"=.2)) +
  scale_colour_manual(values=c("median"=median_colour)) +
  guides(alpha = guide_legend(override.aes = list(alpha = c(0.12,0.08,0.04) ))) +
  labs(alpha="Prediction Intervall", colour="", x=x_lab, y=y_lab) +
  ggtitle(plot_title, plot_subtitle) +
  theme(legend.position = "bottom", axis.text = element_text(size=text_size),
        axis.title = element_text(size=text_size+1),
        legend.text = element_text(size = text_size-1),
        plot.title = element_text(size=text_size+1, face = "bold"),
        plot.subtitle = element_text(size=text_size))
}

create_pred_plot(mrgsim_object_example)



