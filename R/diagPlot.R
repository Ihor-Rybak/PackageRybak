#' @title Checking the Gauss-Markov conditions
#'
#' @description This function graphically checks the Gauss-Markov conditions for evaluation
#'  parameters of the linear model of multiple regression.
#'
#' @param model
#'
#' @examples diagPlot(model)
#'
#' @export diagPlot
#'

require(ggplot2)
require(cowplot)
diagPlot<-function(model){
  library("ggplot2")
  p1<-ggplot(model, aes(.fitted, .resid))+geom_point(colour = "green")
  p1<-p1+stat_smooth(method="loess",formula = y ~ x)+geom_hline(yintercept=0, col="red", linetype="dashed")
  p1<-p1+xlab("Fitted values")+ylab("Residuals")
  p1<-p1+ggtitle("Residual vs Fitted Plot")+theme_bw()

  p2 <- ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm = TRUE, colour = "green")
  p2 <- p2+geom_abline(col = "red")+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
  p2 <- p2+ggtitle("Normal Q-Q")+theme_bw()

  p3<-ggplot(model, aes(.fitted, sqrt(abs(.stdresid))))+geom_point(na.rm=TRUE, colour = "green")
  p3<-p3+stat_smooth(method="loess",formula = y ~ x, na.rm = TRUE)+xlab("Fitted Value")
  p3<-p3+ylab(expression(sqrt("|Standardized residuals|")))
  p3<-p3+ggtitle("Scale-Location")+theme_bw()

  p4<-ggplot(model, aes(seq_along(.cooksd), .cooksd))+geom_bar(stat="identity", position="identity", colour = "green")
  p4<-p4+xlab("Obs. Number")+ylab("Cook's distance")
  p4<-p4+ggtitle("Cook's distance")+theme_bw()

  p5<-ggplot(model, aes(.hat, .stdresid))+geom_point(aes(size=.cooksd), na.rm=TRUE, colour = "green")
  p5<-p5+stat_smooth(method="loess",formula = y ~ x, na.rm=TRUE)
  p5<-p5+xlab("Leverage")+ylab("Standardized Residuals")
  p5<-p5+ggtitle("Residual vs Leverage Plot")
  p5<-p5+scale_size_continuous("Cook's Distance", range=c(1,5))
  p5<-p5+theme_bw()+theme(legend.position="bottom")

  p6<-ggplot(model, aes(.hat, .cooksd))+geom_point(na.rm=TRUE, colour = "green")+stat_smooth(method="loess",formula = y ~ x, na.rm=TRUE)
  p6<-p6+xlab("Leverage hii")+ylab("Cook's Distance")
  p6<-p6+ggtitle("Cook's dist vs Leverage hii/(1-hii)")
  p6<-p6+geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed")
  p6<-p6+theme_bw()

  library("cowplot")
  theme_set(theme_cowplot())
  plot_grid(p1, p2, p3, p4, p5, p6,
                   ncol = 3, nrow = 2)

}
