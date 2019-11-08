#Plot Functions
#All the plot do not have xlabel, ylabel, title, please specifiy when call the function using plotly layout
plot_line <- function(df, xvalues, yvalue, group){
  
  p = ggplot(df, aes_string(x = xvalues, y = yvalue, color = group)) 
  p = p + geom_line()
  p = p + theme()
  p = p + xlab("") + ylab("")
  p = p + theme_bw()
  p = p + guides(fill=guide_legend(""))
  
  #Comment below if want to have a static plot
  return (p)
}

plotly_donut<- function(df, xvalue, yvalue, hole, ...){
  p <- df %>%
    plot_ly(labels = df[[xvalue]], values = df[[yvalue]]) %>%
    add_pie(hole = hole) %>%
    layout(showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  p
}

#plot trend and lod trend with first order difference
ts_plot_day <- function(total_activity) {
  log.total_activity=log(total_activity)
  diff.total_activity = diff(total_activity)
  difflog.total_activity = diff(log.total_activity)
  par(mfrow=c(2,2))
  plot(total_activity, main="Original Activity")
  plot(log.total_activity, main="Log Activity")
  plot(diff.total_activity, main="Difference Activity")
  plot(difflog.total_activity, main="Difference Log Activity")
}

plot_time_series <- function(ts_object, ts_object_name){
  #' Plot Time Series Object
  #'
  #' Creates time series plot utilizing \code{ggplot2} utlizing
  #' custom themes to ensure plots are
  #' consistent. Utlizes \code{autoplot} function for plots.
  #'
  #' @param ts_object time series object used to create plot
  #' @param ts_object_name preferred title of plot
  #' @examples
  #' data(AirPassengers)
  #'
  #' air_pass_ts <- as.ts(AirPassengers)
  #'
  #' plot_time_series(air_pass_ts, 'Air Passengers')
  if (is.ts(ts_object) == TRUE){
    if(missing(ts_object_name)) {
      warning('Title for plot not entered!')
    } else {
      startYear <- start(ts_object) # Grabs start date
      endYear <- end(ts_object) # Grabs end date
      tsPlot <- autoplot(ts_object,
                         ts.colour = 'turquoise4',
                         size = 1,
                         main = sprintf("Plot of %s Time Series",
                                        ts_object_name)) +
        theme(axis.text.x = element_text(angle = 35, hjust = 1),
              panel.background = element_rect(fill = "gray98"),
              axis.line.x = element_line(colour="gray"),
              axis.line.y = element_line(colour="gray")) #+
        #labs(x = "Year", y = "Closing Values") 
      return(tsPlot)
    }
  }
  else {
    warning('Make sure object entered is time-series object!')
  }
}

# FUNCTION FOR ACF AND PACF PLOTS
plot_acf_pacf <- function(ts_object, ts_object_name,ncol){
  #' Plot ACF and PACF for Time Series Object
  #'
  #' Creates \emph{Autocorrelation} and \emph{Partial Autocorrelation} plot
  #' utilizing \code{ggplot2} with custom themes to ensure plots are
  #' consistent. Utlizes \code{autoplot} function for plots.
  #'
  #' @param ts_object time series object used to create plot
  #' @param ts_object_name preferred title of plot
  #' @examples
  #' data(AirPassengers)
  #'
  #' air_pass_ts <- as.ts(AirPassengers)
  #'
  #' plot_acf_pacf(air_pass_ts, 'Air Passengers Data Set')
  if (is.ts(ts_object) == TRUE){
    if(missing(ts_object_name)) {
      warning('Title for plot not entered!')
    } else {
      a <- autoplot(acf(ts_object, plot = FALSE),
                    colour = 'turquoise4',
                    conf.int.fill = '#4C4CFF',
                    conf.int.value = 0.95, conf.int.type = 'ma') +
        theme(panel.background = element_rect(fill = "gray98"),
              axis.line.y   = element_line(colour="gray"),
              axis.line.x = element_line(colour="gray")) +
        ggtitle(sprintf("ACF plot of %s", ts_object_name))
      
      b <- autoplot(pacf(ts_object, plot = FALSE),
                    colour = 'turquoise4',
                    conf.int.fill = '#4C4CFF',
                    conf.int.value = 0.95, conf.int.type = 'ma') +
        theme(panel.background = element_rect(fill = "gray98"),
              axis.line.y   = element_line(colour="gray"),
              axis.line.x = element_line(colour="gray")) + labs(y="PACF") +
        ggtitle(sprintf("PACF plot of %s", ts_object_name))
      
      grid.arrange(a, b, ncol =ncol)
    }
  } else {
    warning('Make sure object entered is time-series object!')
  }
}

# Decomposed Plot
plot_decomp <- function(ts_object, ts_object_name){
  #' Plots Seasonal Decomposition for Time Series Object
  #'
  #' Decomposes time series object to \emph{Seasonal},
  #' \emph{Remainder}, and \emph{Trend}.
  #' Utilizing \code{ggplot2} with custom themes to ensure plots are
  #' consistent. Utlizes \code{autoplot} function for plots.
  #'
  #' @param ts_object time series object used to create plot
  #' @param ts_object_name preferred title of plot
  #' @examples
  #' data(AirPassengers)
  #'
  #' air_pass_ts <- as.ts(AirPassengers)
  #'
  #' plot_decomp(air_pass_ts, 'Air Passengers Data Set')
  if (is.ts(ts_object) == TRUE){
    autoplot(stl(ts_object, s.window = "periodic"),
             main = sprintf("Decomposition Plot of %s", ts_object_name),
             ts.colour = "turquoise4") +
      theme(panel.background = element_rect(fill = "gray98"),
            axis.line.y   = element_line(colour="gray"),
            axis.line.x = element_line(colour="gray"))
  } else {
    warning('Make sure object entered is time-series object!')
  }
}


ggtsdiag_custom <- function(object, ts_object_name, gof.lag = 10,
                            conf.int = TRUE,
                            conf.int.colour = '#0000FF', conf.int.linetype = 'dashed',
                            conf.int.fill = NULL, conf.int.alpha = 0.3,
                            ad.colour = '#888888', ad.linetype = 'dashed', ad.size = .2,
                            nrow = NULL, ncol = 1, ...) {
  rs <- stats::residuals(object)
  if (is.null(rs)) {
    rs <- object$residuals
  }
  if (is.null(rs)) {
    rs <- object$resid
  }
  
  p.std <- ggplot2::autoplot(rs, na.action = stats::na.pass,
                             ts.colour = 'turquoise4', size = 1.05) +
    ggplot2::geom_hline(yintercept = 0,
                        linetype = ad.linetype, size = ad.size,
                        colour = ad.colour) +
    labs(subtitle = '') +
    ggplot2::ggtitle(sprintf("Residual Diagnostics for %s \nNon-Standardized Residuals",
                             ts_object_name))
  
  acfobj <- stats::acf(rs, plot = FALSE, na.action = stats::na.pass)
  p.acf <- autoplot(acfobj, conf.int = conf.int,
                    conf.int.colour = conf.int.colour,
                    conf.int.linetype = conf.int.linetype,
                    conf.int.fill = conf.int.fill,
                    conf.int.alpha = conf.int.alpha,
                    colour = 'turquoise4', size = 1.25)
  p.acf <- p.acf + ggplot2::ggtitle('ACF of Residuals')
  
  nlag <- gof.lag
  pval <- numeric(nlag)
  for (i in 1L:nlag) pval[i] <- stats::Box.test(rs, i, type = "Ljung-Box")$p.value
  lb.df <- data.frame(Lag = 1L:nlag, `p value` = pval,
                      lower = -0.05, upper = 0.05)
  # Unnable to create column with space by above expression
  colnames(lb.df) <- c('Lag', 'p value', 'lower', 'upper')
  p.lb <- ggplot2::ggplot(data = lb.df, mapping = ggplot2::aes_string(x = 'Lag')) +
    ggplot2::geom_point(mapping = ggplot2::aes_string(y = '`p value`'), na.rm = TRUE,
                        colour = 'turquoise4') +
    ggplot2::scale_y_continuous(limits=c(-0.1, 1)) +
    ggplot2::ggtitle('p values for Ljung-Box statistic')
  
  p.lb <- ggfortify:::plot_confint(p = p.lb, data = lb.df, conf.int = conf.int,
                                   conf.int.colour = conf.int.colour,
                                   conf.int.linetype = conf.int.linetype,
                                   conf.int.fill = conf.int.fill, conf.int.alpha = conf.int.alpha)
  
  if (is.null(ncol)) { ncol <- 0 }
  if (is.null(nrow)) { nrow <- 0 }
  new('ggmultiplot', plots = list(p.std, p.acf, p.lb), nrow = nrow, ncol = ncol)
}


###########################################################################################
# HERE FOUND AT http://librestats.com/2012/06/11/autoplot-graphical-methods-with-ggplot2/ #
# BY DREW SCHMIDT WITH SLIGHT MODIFICATIONS TO FIT OUR PLOTS                              #
###########################################################################################

plot_ts_forecast <- function(df){
  #setDT(df)
  df = df[, Lower:= ifelse(Lower < 0, 0, Lower)]
  gg = ggplot(df, aes(x = Index, y = Data))
  gg = gg + geom_line(na.rm = TRUE) + geom_point(aes(x = Index, y = Fitted), color = "#0072B2", na.rm = TRUE)
  gg = gg + geom_point(aes(x = Index, y = Point), color = "red", na.rm = TRUE)
  gg = gg + geom_ribbon(aes(ymin = Lower, ymax = Higher), alpha = 0.2, fill = '#0072B2', na.rm = TRUE) 
  #gg = gg + theme_bw()
  
  
  #Comment below if want to have a static plot
  return (gg)
}
