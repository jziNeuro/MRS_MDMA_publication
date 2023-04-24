# define function to plot boxplots
boxplot_groups <- function(var, pred, data, ylab_text, xlab_text, p_text){
  
  # var = outcome
  # pred = grouping variable
  
  
  # get color
  colors_group <- pal_uchicago(palette = c("default"), alpha = 1)(8)[c(3,5)]
  plt_margin <- 0.5
  
  # get p-value as text
  p_text <- paste0("p=", p_text)
  textsize <- 6
  
  # get range of x and y (to print p-value anddraw significance line)
  range_y <- range(data[,var], na.rm = T)
  ymax <- range_y[2]
  
  p_text_ypos <- ymax + (range_y[2]-range_y[1])*(textsize/60)
  sign_line_ypos <- ymax + (range_y[2]-range_y[1])*((textsize-1)/100)
  tip_signline_ypos <- ymax + (range_y[2]-range_y[1])*((textsize-3)/100)
  
  # get boxplot to display group difference
  boxplot_out <- ggplot(data, aes(x = data[,pred], y = data[,var], fill = data[,pred])) + 
    theme_void() +
    geom_boxplot(width=0.3, alpha=0.6, outlier.colour = NA) +
    scale_fill_manual(values=colors_group) +
    geom_point(position=position_jitterdodge(jitter.width = 0.15, jitter.height = 0), shape=16, size=1.75, alpha=0.8) +
    ylab(ylab_text) +
    scale_x_discrete(labels = xlab_text) +
    annotate("text", x = 1.5, y = p_text_ypos, label = p_text, size=textsize) +
    geom_segment(aes(x=1, xend=2, y=sign_line_ypos, yend=sign_line_ypos)) +
    geom_segment(aes(x=1, xend=1, y=sign_line_ypos, yend=tip_signline_ypos)) +
    geom_segment(aes(x=2, xend=2, y=sign_line_ypos, yend=tip_signline_ypos)) +
    theme(legend.position = "none",
          strip.text=element_text(vjust=1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line = element_line(colour = 'black', size = 0.3),
          plot.title = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_text(size=15, angle=90, vjust=4),
          axis.ticks = element_line(size=0.15),
          axis.ticks.length=unit(0.1,"cm"),
          axis.text.x = element_text(size=12, vjust=-0.5),
          axis.text.y = element_text(size=12, margin = unit(c(0,0.075,0,0), "cm")),
          plot.margin = unit(c(plt_margin,plt_margin,plt_margin,plt_margin), "cm"),
    )
}




# define formula to get standardized coefficient (~robust correlation coefficient)
get_std_coeffs <- function(model_in){coef(model_in)*apply(as.matrix(model.matrix(model_in)),2,function(x) sqrt(sum((x-mean(x,na.rm=T)*attr(attr(model_in$model,"terms"),"intercept"))^2,na.rm=T)))/apply(as.matrix(model.frame(model_in)[,1]),2,function(x) sqrt(sum((x-mean(x,na.rm=T)*attr(attr(model_in$model,"terms"),"intercept"))^2,na.rm=T)))
}



# define function to plot robust correlation
plot_robust_correlation <- function(var_outc, var_corr, data, ylab_text, xlab_text, log_transf, col_index){
  
  # var_outc = outcome
  # var_corr = variable to correlate with outcome
  # log_transf = TRUE if you want to log transform variable to correlate
  
  plt_margin <- 0.5
  
  # get variable to correlate with outcome (log or non-log)
  if (log_transf){predi <- log(data[,var_corr])} else {predi <- data[,var_corr]}
  
  # get robust linear regression coefficient and extract coefficient as well as p-value
  lmrob_obj <- lmrob(data[,var_outc] ~ predi)
  rob_corr_coeff <- round(get_std_coeffs(lmrob_obj)[2],2)
  
  pval_raw <- summary(lmrob_obj)$coefficients[2,4]
  pval <- ifelse(pval_raw>0.1, paste0("p=",format(round(pval_raw,2), nsmall=2)), paste0("p=",round(pval_raw,3)))
  pval <- ifelse(pval_raw>0.0001, pval, "p<0.0001")
  cor_text <- paste0("R=", format(rob_corr_coeff, nsmall = 2), ", ", pval)
  
  # get range of x and y (to print correlation coefficient and p-value)
  xrange <- range(predi, na.rm=T)
  xtext_pos <- xrange[1] + (xrange[2]-xrange[1])*0.1
  yrange <- range(data[,var_outc], na.rm=T)
  ymax <- yrange[2]
  ytext_pos <- ymax + (yrange[2]-yrange[1])*0.1
  
  # get color of points
  col_points <- pal_uchicago(palette = c("default"), alpha = 1)(8)[col_index]
  
  # get robust correlation plot
  robust_correlation_plot <- ggplot(data, aes(x = predi, y = data[,var_outc])) +
    theme_void() +
    geom_smooth(method = robustbase::lmrob, se=FALSE, color="black", formula = y ~ x, size=1) +
    geom_point(color=col_points, size=2.5, alpha = 0.8, shape=16) +
    annotate("text", x = xtext_pos, y = ytext_pos, label = cor_text, size=5, hjust = 0) + 
    xlab(xlab_text) +
    ylab(ylab_text) +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.line = element_line(colour = 'black', size = 0.3),
          plot.title = element_blank(),
          axis.title.x=element_text(size=13, vjust=-2),
          axis.title.y=element_text(size=13, angle=90, vjust=4),
          axis.ticks = element_line(size=0.15),
          axis.ticks.length=unit(0.1,"cm"),
          axis.text.x = element_text(size=12, vjust=-0.25),
          axis.text.y = element_text(size=12, margin = unit(c(0,0.075,0,0), "cm")),
          plot.margin = unit(c(plt_margin,plt_margin,plt_margin,plt_margin), "cm"),
    )
}


