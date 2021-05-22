


chromatogram <- R6::R6Class(
  "OpenChromatogram",
  public = list(
    detector = NULL,
    time_trace = NULL,
    signal_trace = NULL,
    unit_time = NA,
    unit_signal = NA,
    initialize = function(detector=NULL, signal_trace=NULL, time_trace=NULL,
                          unit_time, unit_signal) {
      self$detector <- detector
      self$time_trace <- time_trace
      self$signal_trace <- signal_trace
      self$unit_time <- unit_time
      self$unit_signal <- unit_signal
    },

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
             y=paste0("Signal [", self$unit_signal, "]"))
    },

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



chromatogram$new("UV210nm", Carbocisteine$mAU, Carbocisteine$Time,
                 "min", "mAU") -> chrom


plot(chrom) -> pl

pl+coord_cartesian(ylim=c(-5,5), xlim=c(2,3))

pl+coord_cartesian(ylim=c(-5,5), xlim=c(4,6))
pl+coord_cartesian(ylim=c(-.05,.05), xlim=c(4,6))

