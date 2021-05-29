#' Create a plot with prediction intervals from mrgsim result and optional observed data
#' @param x mrgsim result (must contain an event-ID column)
#' @param median_colour specify colour of the median line
#' @param obs_colour specify colour of the observations
#' @param obs_shape specify shape of the observations
#' @param y_lab specify label of the y-axis
#' @param x_lab specify label of x-axis
#' @param plot_title specify plot title
#' @param plot_subtitle specify plot subtitle
#' @param text_size specify text size for labels and title
#' @param obs a data.frame containing two columns called \code{x} and \code{y} with independent and dependent variable of the observations
#' @returns The result is a ggplot object
#' @examples
#' data(ruxSim)
#' create_pred_plot(ruxSim)
#' @import tidyverse
#' @import mrgsolve
#' @import ggplot2
#' @export

create_pred_plot <- function (x, intervall_colour ="red",
                              median_colour="darkblue",
                              obs_colour="black",
                              obs_shape = 1,
                              obs_size=3,
                              y_lab = "Plasma Concentration [ng/mL]",
                              x_lab = "Time [h]",
                              plot_title = "PK-Curve",
                              plot_subtitle = "oral",
                              text_size = 14,
                              obs = NULL) {
  if(is.null(obs) ) {
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
            plot.subtitle = element_text(size=text_size)) -> result_plot

  } else {
    create_pred_int(x) %>%
      ggplot(mapping = aes(x=time)) +  theme_bw() +
      geom_ribbon(aes(ymin=perc_2.5, ymax=perc_5, alpha="95 %"), fill=intervall_colour) +
      geom_ribbon(aes(ymin=perc_5, ymax=perc_25, alpha="90 %"), fill=intervall_colour) +
      geom_ribbon(aes(ymin=perc_25, ymax=median, alpha="50 %"), fill=intervall_colour) +
      geom_ribbon(aes(ymin=median, ymax=perc_75, alpha="50 %"), fill=intervall_colour) +
      geom_ribbon(aes(ymin=perc_75, ymax=perc_95, alpha="90 %"), fill=intervall_colour) +
      geom_ribbon(aes(ymin=perc_95, ymax=perc_97.5, alpha="95 %"), fill=intervall_colour) +
      geom_line(aes(y=median, colour="median"), lwd=1) +
      geom_point(data=obs, aes(x=x, y=y, colour="observation"), shape=obs_shape, size=obs_size) +
      scale_alpha_manual(values = c("50 %"=.4,
                                    "90 %"=.3,
                                    "95 %"=.2)) +
      scale_colour_manual(values=c("median"=median_colour,
                                   "observation"=obs_colour)) +
      guides(alpha = guide_legend(override.aes = list(alpha = c(0.12,0.08,0.04) )),
             colour = guide_legend(override.aes = list(linetype=c(1,NA),
                                                       shape=c(NA,obs_shape)))
      ) +
      labs(alpha="Prediction Intervall", colour="", x=x_lab, y=y_lab) +
      ggtitle(plot_title, plot_subtitle) +
      theme(legend.position = "bottom", axis.text = element_text(size=text_size),
            axis.title = element_text(size=text_size+1),
            legend.text = element_text(size = text_size-1),
            plot.title = element_text(size=text_size+1, face = "bold"),
            plot.subtitle = element_text(size=text_size)) -> result_plot
  }
  return(result_plot)

}
