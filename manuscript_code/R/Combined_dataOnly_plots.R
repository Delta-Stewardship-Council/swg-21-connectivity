################################################################################
#~~ Script created by Jereme W Gaeta, CDFW
#~~ Last modified by Jereme W Gaeta on 04/17/2025
#~~ Description: Supplementary figure of Raw data across variable space
################################################################################

rm(list = ls())
graphics.off()
cat("\f")
gc(TRUE)
gc(reset = TRUE)
gc()


library(emmeans)
library(alphahull)
library(dplyr)
library(tidyr)
library(mgcv)
library(viridis)
library(here)

op = function(x, d=2) sprintf(paste0("%1.",d,"f"), x)
plot_limit = function(adj=0.015, usr=par('usr')){
  y_adj_low <<- usr[3]+(usr[4]-usr[3])*adj
  y_adj_high <<- usr[4]-(usr[4]-usr[3])*adj
}
color.bar <- function(lut, min, max=-min, nticks=11,
                      ticks=seq(min, max, len=nticks),
                      title='', horiz=TRUE) {
  scale = (length(lut))/(max-min)
  par(new=TRUE)
  plot(c(min,max), c(0,1), type='n', bty='n',
       xaxt='n', xlab='', yaxt='n', ylab='',
       main=title, xaxs="i", yaxs="i")
  for (i in 1:(length(lut))) {
    x = (i-1)/scale + min
    if(horiz){
      rect(x,0, x+1/scale, 1, col=lut[i], border=lut[i], lwd=0.1)
    } else {
      rect(0, x, 1, x+1/scale, col=lut[i], border=lut[i], lwd=0.1)
    }

  }
}
plot_label = function(lab="(a)", x_prop=0.08, y_prop=0.92,
                      font_type=2, fcex=1.15, usr=par('usr')){
  x_val = usr[1]+(usr[2]-usr[1])*x_prop
  y_val = usr[3]+(usr[4]-usr[3])*y_prop
  text(x = x_val, y = y_val, labels = lab, font = font_type, cex=fcex, xpd=NA)
}
extract_polygons <- function(alpha_obj)
{
  # https://stackoverflow.com/questions/61973754/extract-multiple-polygons-from-alphahull-in-r
  if(class(alpha_obj) != "ashape") stop("extract_polygons requires an ashape")
  edge.df <- as.data.frame(alpha_obj$edges)
  groups <- ns <- xs <- ys <- numeric(nrow(edge.df))
  m <- cbind(edge.df[[1]], edge.df[[2]])
  group <- 1
  repeat {
    i <- which(groups == 0)[1]
    if (length(i) == 0 | is.na(i)) break()
    j <- n <- 1
    repeat {
      groups[i] <- group
      ns[i] <- n
      if(j == 1) xs[i] <- edge.df$x1[i] else xs[i] <- edge.df$x2[i]
      if(j == 1) ys[i] <- edge.df$y1[i] else ys[i] <- edge.df$y2[i]
      next_ind <- which((m[, j] == m[i, j] | m[, (j %% 2 + 1)] == m[i, j]) & groups == 0)
      if (length(next_ind) == 0) break()
      j <- which(m[next_ind,] == m[i, j]) %% 2 + 1
      i <- next_ind
      n <- n + 1
    }
    group <- group + 1
  }
  data.frame(x = xs, y = ys, group = as.factor(groups))[order(groups, ns), ]
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# save(yolo.none_arr_red , yolo.shrt_arr_red, yolo.long_arr_red, up.none_arr_red,
#      up.shrt_arr_red, up.long_arr_red, down.none_arr_red,
#      down.shrt_arr_red, down.long_arr_red, up.log_q_range,
#      up.temp_range, log_q_range, temp_range, do.log_q_range,
#      do.temp_range, file="all_GAM_outputs.RData")
# load("gams_origdata.Rdata")
load(here("manuscript_code/data_clean/data_gam_results.Rdata"))
load(file = here("manuscript_code/data_clean/all_GAM_outputs.RData"))
# load(file="point_comparisons.RData")


# Raw Data ----------------------------------------------------------------

upstream <- alldata %>% filter(region == "upstream")
# subset the upstream data frame each inundation factor
up.none = upstream %>% filter(inund_factor == "none")
up.shrt = upstream %>% filter(inund_factor == "short")
up.long = upstream %>% filter(inund_factor == "long")
up.none_points = data.frame(log_qsdy=up.none$log_qsdy, WTmwk=up.none$WTmwk,
                            log_chla=up.none$log_chla)
# remove duplicate observations; average log_chla
up.none_points_red = aggregate(x=up.none_points$log_chla,by=list(
  log_qsdy=up.none_points$log_qsdy,WTmwk=up.none_points$WTmwk),
  data=up.none_points,FUN=mean)
colnames(up.none_points_red)[3] = "log_chla"

up.shrt_points = data.frame(log_qsdy=up.shrt$log_qsdy, WTmwk=up.shrt$WTmwk,
                            log_chla=up.shrt$log_chla)
up.shrt_points_red = aggregate(x=up.shrt_points$log_chla,by=list(
  log_qsdy=up.shrt_points$log_qsdy,WTmwk=up.shrt_points$WTmwk),
  data=up.shrt_points,FUN=mean)
colnames(up.shrt_points_red)[3] = "log_chla"

up.long_points = data.frame(log_qsdy=up.long$log_qsdy, WTmwk=up.long$WTmwk,
                            log_chla=up.long$log_chla)
up.long_points_red = aggregate(x=up.long_points$log_chla,by=list(
  log_qsdy=up.long_points$log_qsdy,WTmwk=up.long_points$WTmwk),
  data=up.long_points,FUN=mean)
colnames(up.long_points_red)[3] = "log_chla"

#~~~~~

yolo <- alldata %>% filter(region == "yolo")
# subset the yolo data frame each inundation factor
yo.none = yolo %>% filter(inund_factor == "none")
yo.shrt = yolo %>% filter(inund_factor == "short")
yo.long = yolo %>% filter(inund_factor == "long")
yo.none_points = data.frame(log_qsdy=yo.none$log_qsdy, WTmwk=yo.none$WTmwk,
                            log_chla=yo.none$log_chla)
# remove duplicate observations; average log_chla
yo.none_points_red = aggregate(x=yo.none_points$log_chla,by=list(
  log_qsdy=yo.none_points$log_qsdy,WTmwk=yo.none_points$WTmwk),
  data=yo.none_points,FUN=mean)
colnames(yo.none_points_red)[3] = "log_chla"

yo.shrt_points = data.frame(log_qsdy=yo.shrt$log_qsdy, WTmwk=yo.shrt$WTmwk,
                            log_chla=yo.shrt$log_chla)
yo.shrt_points_red = aggregate(x=yo.shrt_points$log_chla,by=list(
  log_qsdy=yo.shrt_points$log_qsdy,WTmwk=yo.shrt_points$WTmwk),
  data=yo.shrt_points,FUN=mean)
colnames(yo.shrt_points_red)[3] = "log_chla"

yo.long_points = data.frame(log_qsdy=yo.long$log_qsdy, WTmwk=yo.long$WTmwk,
                            log_chla=yo.long$log_chla)
yo.long_points_red = aggregate(x=yo.long_points$log_chla,by=list(
  log_qsdy=yo.long_points$log_qsdy,WTmwk=yo.long_points$WTmwk),
  data=yo.long_points,FUN=mean)
colnames(yo.long_points_red)[3] = "log_chla"

#~~~~~

downstream <- alldata %>% filter(region == "downstream")
# subset the downstream data frame each inundation factor
do.none = downstream %>% filter(inund_factor == "none")
do.shrt = downstream %>% filter(inund_factor == "short")
do.long = downstream %>% filter(inund_factor == "long")
do.none_points = data.frame(log_qsdy=do.none$log_qsdy, WTmwk=do.none$WTmwk,
                            log_chla=do.none$log_chla)
# remove duplicate observations; average log_chla
do.none_points_red = aggregate(x=do.none_points$log_chla,by=list(
  log_qsdy=do.none_points$log_qsdy,WTmwk=do.none_points$WTmwk),
  data=do.none_points,FUN=mean)
colnames(do.none_points_red)[3] = "log_chla"

do.shrt_points = data.frame(log_qsdy=do.shrt$log_qsdy, WTmwk=do.shrt$WTmwk,
                            log_chla=do.shrt$log_chla)
do.shrt_points_red = aggregate(x=do.shrt_points$log_chla,by=list(
  log_qsdy=do.shrt_points$log_qsdy,WTmwk=do.shrt_points$WTmwk),
  data=do.shrt_points,FUN=mean)
colnames(do.shrt_points_red)[3] = "log_chla"

do.long_points = data.frame(log_qsdy=do.long$log_qsdy, WTmwk=do.long$WTmwk,
                            log_chla=do.long$log_chla)
do.long_points_red = aggregate(x=do.long_points$log_chla,by=list(
  log_qsdy=do.long_points$log_qsdy,WTmwk=do.long_points$WTmwk),
  data=do.long_points,FUN=mean)
colnames(do.long_points_red)[3] = "log_chla"


hist(c(
  up.none_points_red$log_chla, up.shrt_points_red$log_chla,
  up.long_points_red$log_chla, yo.none_points_red$log_chla,
  yo.shrt_points_red$log_chla, yo.long_points_red$log_chla,
  do.none_points_red$log_chla, do.shrt_points_red$log_chla,
  do.long_points_red$log_chla))

# Convex Hulls ------------------------------------------------------------

alpha_parameter = 5
yolo.none_con_hull = ahull(
  x=yo.none_points_red, alpha=alpha_parameter)
polygon.yo.none.df <- extract_polygons(yolo.none_con_hull$ashape.obj)
alpha_parameter = 4
yolo.shrt_con_hull = ahull(
  x=yo.shrt_points_red, alpha=alpha_parameter)
polygon.yo.shrt.df <- extract_polygons(yolo.shrt_con_hull$ashape.obj)
alpha_parameter = 3
yolo.long_con_hull = ahull(
  x=yo.long_points_red, alpha=alpha_parameter)
polygon.yo.long.df <- extract_polygons(yolo.long_con_hull$ashape.obj)

alpha_parameter = 5
up.none_con_hull = ahull(
  x=up.none_points_red, alpha=alpha_parameter)
polygon.up.none.df <- extract_polygons(up.none_con_hull$ashape.obj)
alpha_parameter = 4
up.shrt_con_hull = ahull(
  x=up.shrt_points_red, alpha=alpha_parameter)
polygon.up.shrt.df <- extract_polygons(up.shrt_con_hull$ashape.obj)
alpha_parameter = 6
up.long_con_hull = ahull(
  x=up.long_points_red, alpha=alpha_parameter)
polygon.up.long.df <- extract_polygons(up.long_con_hull$ashape.obj)

alpha_parameter = 5
down.none_con_hull = ahull(
  x=do.none_points_red, alpha=alpha_parameter)
polygon.do.none.df <- extract_polygons(down.none_con_hull$ashape.obj)
alpha_parameter = 4
down.shrt_con_hull = ahull(
  x=do.shrt_points_red, alpha=alpha_parameter)
polygon.do.shrt.df <- extract_polygons(down.shrt_con_hull$ashape.obj)
alpha_parameter = 3
down.long_con_hull = ahull(
  x=do.long_points_red, alpha=alpha_parameter)
polygon.do.long.df <- extract_polygons(down.long_con_hull$ashape.obj)


# Plot set-up -------------------------------------------------------------

col_n = 500

col_pal=turbo(col_n, alpha = 1, begin = 0, end = 1, direction = 1)

log_chla_range = range(
  up.none_points_red$log_chla, up.shrt_points_red$log_chla,
  up.long_points_red$log_chla, yo.none_points_red$log_chla,
  yo.shrt_points_red$log_chla, yo.long_points_red$log_chla,
  do.none_points_red$log_chla, do.shrt_points_red$log_chla,
  do.long_points_red$log_chla)

log_chla_seq = seq(from=log_chla_range[1],to=log_chla_range[2],length.out=col_n)


up.none_points_red$col=apply(
  X = up.none_points_red, MARGIN = 1, FUN = function(x){
    col_pal[which(abs(x["log_chla"]-log_chla_seq) ==
                    min(abs(x["log_chla"]-log_chla_seq)))] })
up.shrt_points_red$col=apply(
  X = up.shrt_points_red, MARGIN = 1, FUN = function(x){
    col_pal[which(abs(x["log_chla"]-log_chla_seq) ==
                    min(abs(x["log_chla"]-log_chla_seq)))] })
up.long_points_red$col=apply(
  X = up.long_points_red, MARGIN = 1, FUN = function(x){
    col_pal[which(abs(x["log_chla"]-log_chla_seq) ==
                    min(abs(x["log_chla"]-log_chla_seq)))] })

yo.none_points_red$col=apply(
  X = yo.none_points_red, MARGIN = 1, FUN = function(x){
    col_pal[which(abs(x["log_chla"]-log_chla_seq) ==
                    min(abs(x["log_chla"]-log_chla_seq)))] })
yo.shrt_points_red$col=apply(
  X = yo.shrt_points_red, MARGIN = 1, FUN = function(x){
    col_pal[which(abs(x["log_chla"]-log_chla_seq) ==
                    min(abs(x["log_chla"]-log_chla_seq)))] })
yo.long_points_red$col=apply(
  X = yo.long_points_red, MARGIN = 1, FUN = function(x){
    col_pal[which(abs(x["log_chla"]-log_chla_seq) ==
                    min(abs(x["log_chla"]-log_chla_seq)))] })

do.none_points_red$col=apply(
  X = do.none_points_red, MARGIN = 1, FUN = function(x){
    col_pal[which(abs(x["log_chla"]-log_chla_seq) ==
                    min(abs(x["log_chla"]-log_chla_seq)))] })
do.shrt_points_red$col=apply(
  X = do.shrt_points_red, MARGIN = 1, FUN = function(x){
    col_pal[which(abs(x["log_chla"]-log_chla_seq) ==
                    min(abs(x["log_chla"]-log_chla_seq)))] })
do.long_points_red$col=apply(
  X = do.long_points_red, MARGIN = 1, FUN = function(x){
    col_pal[which(abs(x["log_chla"]-log_chla_seq) ==
                    min(abs(x["log_chla"]-log_chla_seq)))] })


main_x2_labs=c(4900, 8100, 13400, 22000, 36000,60000, 99000)
main_x2_ats = log(main_x2_labs)

flood_x2_labs=c(60, 400, 3000,22000,163000)
flood_x2_ats = log(flood_x2_labs)

down_x2_labs=c(3000,8000,22000,60000,163000)
down_x2_ats = log(down_x2_labs)

# PLOT IT! ----------------------------------------------------------------


# quartz(height=7*1.33, width=8.5)
# windows(height=7*1.33, width=8.5)
# pdf("2024_revised_NCEAS_image_plots_dataonly.pdf", height=7*1.4, width=8.5)
par(mfrow=c(3,2), mar=c(2.5,3,2.75,1), oma=c(11,3.5,4.5,0))
image(x = up.log_q_range, y = up.temp_range, z = up.none_arr_red[,,"pred"],
      xlab="", ylab="", col = NA,
      useRaster=TRUE, las=1, cex.axis=1.2)
axis(side = 3, at = main_x2_ats, labels = main_x2_labs/1000, cex.axis=1.2)
mtext(text = "Mainstem", side = 2, line = 5, font=2, cex=1.15)
mtext(text = "Flow (Thousands of cfs)", side = 3, line = 2.5, cex=1)
mtext(text = bquote(Water~Temperature~"("*degree*C*")"), side = 2, line = 2.5)
plot_label("(a)",  x_prop = 0.96, y_prop=0.94, fcex = 1.35)
points(up.none_points_red$log_qsdy, up.none_points_red$WTmwk, cex=0.75,
       col=up.none_points_red$col, pch=16)
polygon(polygon.up.none.df$x, polygon.up.none.df$y, lty=1, lwd=1,
        border=gray(0.1,0.2))

image(x = up.log_q_range, y = up.temp_range, z = up.shrt_arr_red[,,"pred"],
      xlab="", ylab="", add=TRUE, col = NA,
      useRaster=TRUE, las=1, cex.axis=1.2)
points(up.shrt_points_red$log_qsdy, up.shrt_points_red$WTmwk, cex=0.75,
       col=up.shrt_points_red$col, pch=15)
polygon(polygon.up.shrt.df$x, polygon.up.shrt.df$y, lty=5, lwd=1,
        border=gray(0.1,0.2))


image(x = up.log_q_range, y = up.temp_range, z = up.none_arr_red[,,"pred"],
      xlab="", ylab="", col = NA,
      useRaster=TRUE, las=1, cex.axis=1.2)
mtext(text = "Flow (Thousands of cfs)", side = 3, line = 2.5, cex=1)
plot_label("(b)",  x_prop = 0.96, y_prop=0.94, fcex = 1.35)
axis(side = 3, at = main_x2_ats, labels = main_x2_labs/1000, cex.axis=1.2)
points(up.none_points_red$log_qsdy, up.none_points_red$WTmwk, cex=0.75,
       col=up.none_points_red$col, pch=16)
polygon(polygon.up.none.df$x, polygon.up.none.df$y, lty=1, lwd=1,
        border=gray(0.1,0.2))

image(x = up.log_q_range, y = up.temp_range, z = up.long_arr_red[,,"pred"],
      xlab="", ylab="", add=TRUE, col = NA, useRaster=TRUE, las=1)
points(up.long_points_red$log_qsdy, up.long_points_red$WTmwk, cex=0.75,
       col=up.long_points_red$col, pch=17)
polygon(polygon.up.long.df$x, polygon.up.long.df$y, lty=3, lwd=1,
        border=gray(0.1,0.2))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
image(x = log_q_range, y = temp_range, z = yolo.none_arr_red[,,"pred"],
      xlab="", ylab="", col = NA,useRaster=TRUE, las=1, cex.axis=1.2)
mtext(text = "Floodplain", side = 2, line = 5, font=2, cex=1.15)
axis(side = 3, at = flood_x2_ats, labels = flood_x2_labs/1000, cex.axis=1.2)
mtext(text = bquote(Water~Temperature~"("*degree*C*")"), side = 2, line = 2.5)
plot_label("(c)", x_prop = 0.96, y_prop=0.94, fcex = 1.35)
points(yo.none_points_red$log_qsdy, yo.none_points_red$WTmwk, cex=0.75,
       col=yo.none_points_red$col, pch=16)
polygon(polygon.yo.none.df$x, polygon.yo.none.df$y, lty=1, lwd=1,
        border=gray(0.1,0.2))

image(x = log_q_range, y = temp_range, z = yolo.shrt_arr_red[,,"pred"],
      xlab="", ylab="", add=TRUE, col=NA, useRaster=TRUE, las=1)
points(yo.shrt_points_red$log_qsdy, yo.shrt_points_red$WTmwk, cex=0.75,
       col=yo.shrt_points_red$col, pch=15)
polygon(polygon.yo.shrt.df$x, polygon.yo.shrt.df$y,lty=5, lwd=1,
        border=gray(0.1,0.2))


image(x = log_q_range, y = temp_range, z = yolo.none_arr_red[,,"pred"],
      xlab="", ylab="", col = NA, useRaster=TRUE, las=1, cex.axis=1.2)
axis(side = 3, at = flood_x2_ats, labels = flood_x2_labs/1000, cex.axis=1.2)
plot_label("(d)", x_prop = 0.96, y_prop=0.94, fcex = 1.35)
points(yo.none_points_red$log_qsdy, yo.none_points_red$WTmwk, cex=0.75,
       col=yo.none_points_red$col, pch=16)
polygon(polygon.yo.none.df$x, polygon.yo.none.df$y, lty=1, lwd=1,
        border=gray(0.1,0.2))

image(x = log_q_range, y = temp_range, z = yolo.long_arr_red[,,"pred"],
      xlab="", ylab="", add=TRUE, col = NA, useRaster=TRUE, las=1)
points(yo.long_points_red$log_qsdy, yo.long_points_red$WTmwk, cex=0.75,
       col=yo.long_points_red$col, pch=17)
polygon(polygon.yo.long.df$x, polygon.yo.long.df$y, lty=3, lwd=1,
        border=gray(0.1,0.2))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
image(x = do.log_q_range, y = do.temp_range, z = down.none_arr_red[,,"pred"],
      xlab="", ylab="", col = NA, useRaster=TRUE, las=1, cex.axis=1.2)
axis(side = 3, at = down_x2_ats, labels = down_x2_labs/1000, cex.axis=1.2)
mtext(text = "Downstream", side = 2, line = 5, font=2, cex=1.15)
mtext(text = bquote(log[e](Flow~(cfs))), side = 1, line = 3)
mtext(text = bquote(Water~Temperature~"("*degree*C*")"), side = 2, line = 2.5)
plot_label("(e)",  x_prop = 0.96, y_prop=0.94, fcex = 1.35)
points(do.none_points_red$log_qsdy, do.none_points_red$WTmwk, cex=0.75,
       col=do.none_points_red$col, pch=16)
polygon(polygon.do.none.df$x, polygon.do.none.df$y, lty=1, lwd=1,
        border=gray(0.1,0.2))

image(x = do.log_q_range, y = do.temp_range, z = down.shrt_arr_red[,,"pred"],
      xlab="", ylab="", add=TRUE, col = NA, useRaster=TRUE, las=1)
points(do.shrt_points_red$log_qsdy, do.shrt_points_red$WTmwk, cex=0.75,
       col=do.shrt_points_red$col, pch=15)
polygon(polygon.do.shrt.df$x, polygon.do.shrt.df$y, lty=5, lwd=1,
        border=gray(0.1,0.2))

image(x = do.log_q_range, y = do.temp_range, z = down.none_arr_red[,,"pred"],
      xlab="", ylab="", col = NA, useRaster=TRUE, las=1, cex.axis=1.2)
axis(side = 3, at = down_x2_ats, labels = down_x2_labs/1000, cex.axis=1.2)
plot_label("(f)",  x_prop = 0.96, y_prop=0.94, fcex = 1.35)
mtext(text = bquote(log[e](Flow~(cfs))), side = 1, line = 3)
points(do.none_points_red$log_qsdy, do.none_points_red$WTmwk, cex=0.75,
       col=do.none_points_red$col, pch=16)
polygon(polygon.do.none.df$x, polygon.do.none.df$y, lty=1, lwd=1,
        border=gray(0.1,0.2))

image(x = do.log_q_range, y = do.temp_range, z = down.long_arr_red[,,"pred"],
      xlab="", ylab="", add=TRUE, col = NA, useRaster=TRUE, las=1)
points(do.long_points_red$log_qsdy, do.long_points_red$WTmwk, cex=0.75,
       col=do.long_points_red$col, pch=17)
polygon(polygon.do.long.df$x, polygon.do.long.df$y, lty=3, lwd=1,
        border=gray(0.1,0.2))


#~~~~~~~  Scale Bar
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 2, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
bar_exp_lab = c(0.2,0.4,0.6,1,2,3,5,8,12,20,35,55)
bar_exp_ax=(log(bar_exp_lab)-min(log_chla_range))/
  max((log_chla_range-min(log_chla_range)))
bar_lab = seq(from=-1.5, to=4, by=0.5)
bar_ax=(bar_lab-min(log_chla_range))/max((log_chla_range-min(log_chla_range)))
par(mar=c(4.35,4,68.65,1))
color.bar(col_pal, min=0, max = 1, ticks = FALSE)
axis(side = 1, at = bar_ax, labels=format(bar_lab, digits=3), las=1, cex.axis=1.2)
axis(side = 3, at = bar_exp_ax, labels=format(bar_exp_lab, digits=3), las=1, cex.axis=1.2)
mtext(text = bquote(log[e](Chlorophyll-a)~Color~Scale), side = 1, line = 2.75, xpd=NA)
mtext(text = bquote(Chlorophyll-a~Color~Scale), side = 3, line = 2.1, xpd=NA)
box(which="plot")

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("top", legend = c("No Inundation", "Short Inundation", "Long Inundation"),
       lty=c(1,5,3), horiz = TRUE, cex=1.5, bty='n', lwd=1.5)

dev.off()


#####################################################################
