#' This an open source format for dealing with chromatograms in R
#' @title OpenChromatogram class
#' @docType class
#' @examples
#' data(carbChrom)
#' OpenChromatogram$new("UV210nm", carbChrom$mAU, carbChrom$Time,
#'                     "min", "mAU") -> chrom
#' plot(chrom)
#' @import tidyverse
#' @import ggplot2
#' @export


OpenChromatogram <- R6::R6Class(
  "OpenChromatogram",
  public = list(
    #' @field detector String indicating the detector type
    detector = NULL,

    #' @field time_trace String indicating the name of the time domain trace
    time_trace = NULL,

    #' @field signal_trace String indicating the name of signal trace
    signal_trace = NULL,

    #' @field unit_time String indicating the time unit
    unit_time = NA,

    #' @field unit_signal String indicating the detector signal unit
    unit_signal = NA,

    #' @description OpenChromatogram is used for plotting chromatogram data
    #' @param detector String object  indicating the detector type
    #' @param signal_trace String object indicating the name of the time domain trace
    #' @param time_trace String object indicating the name of signal trace
    #' @param unit_time String object indicating the time unit
    #' @param unit_signal String object indicating the detector signal unit
    #' @returns an R6 class object of OpenChromatogram
    initialize = function(detector=NULL, signal_trace=NULL, time_trace=NULL,
                          unit_time, unit_signal) {
      self$detector <- detector
      self$time_trace <- time_trace
      self$signal_trace <- signal_trace
      self$unit_time <- unit_time
      self$unit_signal <- unit_signal
    },

    #' @description Creates a ggplot2 object from the class
    #' @returns a ggplot2 object
    plot = function() {

      plot_data <- data.frame(time_trace=self$time_trace, signal_trace=self$signal_trace)

      derivatives <- self$peak_detect()

      plot_data <- cbind(plot_data, derivatives)


      head(plot_data)

      ggplot(plot_data, aes(x=time_trace, y=signal_trace)) +
        geom_line() + ggtitle(self$detector) +
        geom_line(aes(y=first_derivative*10, colour="first")) +
        geom_line(aes(y=second_derivative*10, colour="second")) +
        geom_ribbon(aes(ymin=0,ymax=as.numeric(peak_detected), fill="peak detected"), alpha=.2) +
        labs(x=paste0("Time [", self$unit_time, "]"),
             y=paste0("Signal [", self$unit_signal, "]")) -> pl

      return(pl)
    },

    #' @description Integrates the chromatogram using a given threshold
    #' @param threshold threshold for peak detection
    #' @returns a data.frame with three columns, first and second derivative and peak detection
    peak_detect = function(threshold=0.001) {

      peak_detected = vector()
      first_derivative = vector()
      for(i in 1:(length(self$signal_trace)-1) ) {

        first_derivative[i] <- (self$signal_trace[i+1]-self$signal_trace[i])



      }
      first_derivative[i+1] <-0


      second_derivative = vector()
      for(i in 1:(length(first_derivative)-1) ) {

        second_derivative[i] <- (first_derivative[i+1]-first_derivative[i])


        peak_detected[i] <- abs(second_derivative[i]) > threshold



      }
      second_derivative[i+1] <-0
      peak_detected[i+1] <- FALSE

      return(data.frame(first_derivative, second_derivative, peak_detected))

    }


  )
)

