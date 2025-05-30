################################################################################
#~~ Script created by Jereme W Gaeta, CDFW
#~~ Last modified by Jereme W Gaeta on 05/27/2025
#~~ Description: Image plots of model prediction across variable space
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
# Load data ---------

## From earlier files --------------------
wd <- here::here("manuscript_code/")
load(file.path(wd, "data_clean", "data_gam_results.Rdata"))
load(file.path(wd, "data_clean", "all_GAM_outputs.RData"))
load(file.path(wd, "data_clean", "point_comparisons.RData"))
## Raw Data ----------------------------------------------------------------

upstream <- alldata %>% filter(region == "upstream")
# subset the upstream data frame each inundation factor
up.none = upstream %>% filter(inund_factor == "none")
up.shrt = upstream %>% filter(inund_factor == "short")
up.long = upstream %>% filter(inund_factor == "long")
up.none_points = data.frame(log_qsdy=up.none$log_qsdy, WTmwk=up.none$WTmwk)
# remove duplicate observations
up.none_points_red = up.none_points[!duplicated(up.none_points),]
up.shrt_points = data.frame(log_qsdy=up.shrt$log_qsdy, WTmwk=up.shrt$WTmwk)
up.shrt_points_red = up.shrt_points[!duplicated(up.shrt_points),]
up.long_points = data.frame(log_qsdy=up.long$log_qsdy, WTmwk=up.long$WTmwk)
up.long_points_red = up.long_points[!duplicated(up.long_points),]
#~~~~~

yolo <- alldata %>% filter(region == "yolo")
# subset the yolo data frame each inundation factor
yo.none = yolo %>% filter(inund_factor == "none")
yo.shrt = yolo %>% filter(inund_factor == "short")
yo.long = yolo %>% filter(inund_factor == "long")
yo.none_points = data.frame(log_qsdy=yo.none$log_qsdy, WTmwk=yo.none$WTmwk)
# remove duplicate observations
yo.none_points_red = yo.none_points[!duplicated(yo.none_points),]
yo.shrt_points = data.frame(log_qsdy=yo.shrt$log_qsdy, WTmwk=yo.shrt$WTmwk)
yo.shrt_points_red = yo.shrt_points[!duplicated(yo.shrt_points),]
yo.long_points = data.frame(log_qsdy=yo.long$log_qsdy, WTmwk=yo.long$WTmwk)
yo.long_points_red = yo.long_points[!duplicated(yo.long_points),]
#~~~~~

downstream <- alldata %>% filter(region == "downstream")
# subset the downstream data frame each inundation factor
do.none = downstream %>% filter(inund_factor == "none")
do.shrt = downstream %>% filter(inund_factor == "short")
do.long = downstream %>% filter(inund_factor == "long")
do.none_points = data.frame(log_qsdy=do.none$log_qsdy, WTmwk=do.none$WTmwk)
# remove duplicate observations
do.none_points_red = do.none_points[!duplicated(do.none_points),]
do.shrt_points = data.frame(log_qsdy=do.shrt$log_qsdy, WTmwk=do.shrt$WTmwk)
do.shrt_points_red = do.shrt_points[!duplicated(do.shrt_points),]
do.long_points = data.frame(log_qsdy=do.long$log_qsdy, WTmwk=do.long$WTmwk)
do.long_points_red = do.long_points[!duplicated(do.long_points),]


# Prep Convex Hulls (Fig3)  ------------------------------------------------------------

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


# Convex Hull Plot
## set-up -------------------------------------------------------------

col_n = 500

col_pal=turbo(col_n, alpha = 1, begin = 0, end = 1, direction = 1)

# col_hex = c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','gray70',
#             '#c7eae5','#80cdc1','#35978f','#810f7c','#88419d','#8c6bb1')
# col_pal = colorRampPalette(col_hex)(col_n)
yo.all_range = range(range(na.omit(c(yolo.none_arr_red[,,"pred"]))),
                     range(na.omit(c(yolo.shrt_arr_red[,,"pred"]))),
                     range(na.omit(c(yolo.long_arr_red[,,"pred"]))))
yo.all_seq = seq(from=yo.all_range[1], to=yo.all_range[2], length.out=col_n)

#~ Clumsy but effective code
yo.none_col_min_index = which(abs(yo.all_seq - range(na.omit(c(yolo.none_arr_red[,,"pred"])))[1]) == min(abs(yo.all_seq - range(na.omit(c(yolo.none_arr_red[,,"pred"])))[1])))
yo.none_col_max_index = which(abs(yo.all_seq - range(na.omit(c(yolo.none_arr_red[,,"pred"])))[2]) == min(abs(yo.all_seq - range(na.omit(c(yolo.none_arr_red[,,"pred"])))[2])))
yo.shrt_col_min_index = which(abs(yo.all_seq - range(na.omit(c(yolo.shrt_arr_red[,,"pred"])))[1]) == min(abs(yo.all_seq - range(na.omit(c(yolo.shrt_arr_red[,,"pred"])))[1])))
yo.shrt_col_max_index = which(abs(yo.all_seq - range(na.omit(c(yolo.shrt_arr_red[,,"pred"])))[2]) == min(abs(yo.all_seq - range(na.omit(c(yolo.shrt_arr_red[,,"pred"])))[2])))
yo.long_col_min_index = which(abs(yo.all_seq - range(na.omit(c(yolo.long_arr_red[,,"pred"])))[1]) == min(abs(yo.all_seq - range(na.omit(c(yolo.long_arr_red[,,"pred"])))[1])))
yo.long_col_max_index = which(abs(yo.all_seq - range(na.omit(c(yolo.long_arr_red[,,"pred"])))[2]) == min(abs(yo.all_seq - range(na.omit(c(yolo.long_arr_red[,,"pred"])))[2])))

yo.none_col_pal = col_pal[yo.none_col_min_index:yo.none_col_max_index]
yo.shrt_col_pal = col_pal[yo.shrt_col_min_index:yo.shrt_col_max_index]
yo.long_col_pal = col_pal[yo.long_col_min_index:yo.long_col_max_index]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
up.all_range = range(range(na.omit(c(up.none_arr_red[,,"pred"]))),
                     range(na.omit(c(up.shrt_arr_red[,,"pred"]))),
                     range(na.omit(c(up.long_arr_red[,,"pred"]))))

# Determine which values in the color sequence overlap with the Yolo color plate
up_col_min = which(abs(yo.all_seq-up.all_range[1])==min(abs(yo.all_seq-up.all_range[1])))
up_col_max = which(abs(yo.all_seq-up.all_range[2])==min(abs(yo.all_seq-up.all_range[2])))
up_col_pal = colorRampPalette(col_pal[up_col_min:up_col_max])(col_n)
up.all_seq = seq(from=up.all_range[1], to=up.all_range[2], length.out=col_n)

#~ Clumsy but effective code
up.none_col_min_index = which(abs(up.all_seq - range(na.omit(c(up.none_arr_red[,,"pred"])))[1]) == min(abs(up.all_seq - range(na.omit(c(up.none_arr_red[,,"pred"])))[1])))
up.none_col_max_index = which(abs(up.all_seq - range(na.omit(c(up.none_arr_red[,,"pred"])))[2]) == min(abs(up.all_seq - range(na.omit(c(up.none_arr_red[,,"pred"])))[2])))
up.shrt_col_min_index = which(abs(up.all_seq - range(na.omit(c(up.shrt_arr_red[,,"pred"])))[1]) == min(abs(up.all_seq - range(na.omit(c(up.shrt_arr_red[,,"pred"])))[1])))
up.shrt_col_max_index = which(abs(up.all_seq - range(na.omit(c(up.shrt_arr_red[,,"pred"])))[2]) == min(abs(up.all_seq - range(na.omit(c(up.shrt_arr_red[,,"pred"])))[2])))
up.long_col_min_index = which(abs(up.all_seq - range(na.omit(c(up.long_arr_red[,,"pred"])))[1]) == min(abs(up.all_seq - range(na.omit(c(up.long_arr_red[,,"pred"])))[1])))
up.long_col_max_index = which(abs(up.all_seq - range(na.omit(c(up.long_arr_red[,,"pred"])))[2]) == min(abs(up.all_seq - range(na.omit(c(up.long_arr_red[,,"pred"])))[2])))

up.none_col_pal = up_col_pal[up.none_col_min_index:up.none_col_max_index]
up.shrt_col_pal = up_col_pal[up.shrt_col_min_index:up.shrt_col_max_index]
up.long_col_pal = up_col_pal[up.long_col_min_index:up.long_col_max_index]



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
do.all_range = range(range(na.omit(c(down.none_arr_red[,,"pred"]))),
                     range(na.omit(c(down.shrt_arr_red[,,"pred"]))),
                     range(na.omit(c(down.long_arr_red[,,"pred"]))))

# Determine which values in the color sequence overlap with the Yolo color plate
do_col_min = which(abs(yo.all_seq-do.all_range[1])==min(abs(yo.all_seq-do.all_range[1])))
do_col_max = which(abs(yo.all_seq-do.all_range[2])==min(abs(yo.all_seq-do.all_range[2])))
do_col_pal = colorRampPalette(col_pal[do_col_min:do_col_max])(col_n)
do.all_seq = seq(from=do.all_range[1], to=do.all_range[2], length.out=col_n)

#~ Clumsy but effective code
do.none_col_min_index = which(abs(do.all_seq - range(na.omit(c(down.none_arr_red[,,"pred"])))[1]) == min(abs(do.all_seq - range(na.omit(c(down.none_arr_red[,,"pred"])))[1])))
do.none_col_max_index = which(abs(do.all_seq - range(na.omit(c(down.none_arr_red[,,"pred"])))[2]) == min(abs(do.all_seq - range(na.omit(c(down.none_arr_red[,,"pred"])))[2])))
do.shrt_col_min_index = which(abs(do.all_seq - range(na.omit(c(down.shrt_arr_red[,,"pred"])))[1]) == min(abs(do.all_seq - range(na.omit(c(down.shrt_arr_red[,,"pred"])))[1])))
do.shrt_col_max_index = which(abs(do.all_seq - range(na.omit(c(down.shrt_arr_red[,,"pred"])))[2]) == min(abs(do.all_seq - range(na.omit(c(down.shrt_arr_red[,,"pred"])))[2])))
do.long_col_min_index = which(abs(do.all_seq - range(na.omit(c(down.long_arr_red[,,"pred"])))[1]) == min(abs(do.all_seq - range(na.omit(c(down.long_arr_red[,,"pred"])))[1])))
do.long_col_max_index = which(abs(do.all_seq - range(na.omit(c(down.long_arr_red[,,"pred"])))[2]) == min(abs(do.all_seq - range(na.omit(c(down.long_arr_red[,,"pred"])))[2])))

do.none_col_pal = do_col_pal[do.none_col_min_index:do.none_col_max_index]
do.shrt_col_pal = do_col_pal[do.shrt_col_min_index:do.shrt_col_max_index]
do.long_col_pal = do_col_pal[do.long_col_min_index:do.long_col_max_index]

main_x2_labs=c(4900, 8100, 13400, 22000, 36000,60000, 99000)
main_x2_ats = log(main_x2_labs)

flood_x2_labs=c(60, 400, 3000,22000,163000)
flood_x2_ats = log(flood_x2_labs)

down_x2_labs=c(3000,8000,22000,60000,163000)
down_x2_ats = log(down_x2_labs)

## PLOT IT! ----------------------------------------------------------------

# Need to run the pdf line in order to create the image plus:
# quartz line for mac; windows line for windows

# quartz(height=7*1.33, width=8.5)
# windows(height=7*1.6, width=8.5)
# pdf("Figure_3_Khanna.pdf", height=7*1.6, width=8.5)
par(mfrow=c(3,2), mar=c(2.5,3,2.75,1), oma=c(11,3.5,6.5,0))
image(x = up.log_q_range, y = up.temp_range, z = up.none_arr_red[,,"pred"],
      xlab="", ylab="", col = up.none_col_pal,
      useRaster=TRUE, las=1, cex.axis=1.2)
axis(side = 3, at = main_x2_ats, labels = main_x2_labs/1000, cex.axis=1.2)
mtext(text = "Mainstem", side = 2, line = 5, font=2, cex=1.15)
mtext(text = "Flow (Thousands of cfs)", side = 3, line = 2.5, cex=1)
text(x = 8, y= 27.5, labels = "Figure 3", font=2,  adj = 0, xpd=NA, cex=2)
mtext(text = bquote(Water~Temperature~"("*degree*C*")"), side = 2, line = 2.5)
plot_label("(a)",  x_prop = 0.96, y_prop=0.94, fcex = 1.35)
points(up.none_points_red$log_qsdy, up.none_points_red$WTmwk, cex=0.75, col=gray(0.1,0.5))
polygon(polygon.up.none.df$x, polygon.up.none.df$y, lty=1, lwd=1)

image(x = up.log_q_range, y = up.temp_range, z = up.shrt_arr_red[,,"pred"],
      xlab="", ylab="", add=TRUE, col = up.shrt_col_pal,
      useRaster=TRUE, las=1, cex.axis=1.2)
points(up.shrt_points_red$log_qsdy, up.shrt_points_red$WTmwk, cex=0.75, col=gray(0.1,0.5))
polygon(polygon.up.shrt.df$x, polygon.up.shrt.df$y, lty=5, lwd=1)
points(up_points$log_q , up_points$temp, pch=23, bg="white", lwd=0.75)
for(i in 1:5){
  text(x = up_points$log_q[i]+c(-0.033,0,0,0,0)[i],
       y = up_points$temp[i]+c(0,-1.1,-1,1.35,-1.5)[i],
       font=2,labels = paste0("a.", 1:5)[i], cex=1.15,
       adj = c(c(1,0.5,0.5,0.25,0.5)[i],c(0.35,0,0.35,1,0)[i]))
}


image(x = up.log_q_range, y = up.temp_range, z = up.none_arr_red[,,"pred"],
      xlab="", ylab="", col = up.none_col_pal,
      useRaster=TRUE, las=1, cex.axis=1.2)
mtext(text = "Flow (Thousands of cfs)", side = 3, line = 2.5, cex=1)
plot_label("(b)",  x_prop = 0.96, y_prop=0.94, fcex = 1.35)
axis(side = 3, at = main_x2_ats, labels = main_x2_labs/1000, cex.axis=1.2)
points(up.none_points_red$log_qsdy, up.none_points_red$WTmwk, cex=0.75, col=gray(0.1,0.5))
polygon(polygon.up.none.df$x, polygon.up.none.df$y, lty=1, lwd=1)

image(x = up.log_q_range, y = up.temp_range, z = up.long_arr_red[,,"pred"],
      xlab="", ylab="", add=TRUE, col = up.long_col_pal,
      useRaster=TRUE, las=1)
points(up.long_points_red$log_qsdy, up.long_points_red$WTmwk, cex=0.75, col=gray(0.1,0.5))
polygon(polygon.up.long.df$x, polygon.up.long.df$y, lty=3, lwd=1)
points(up_points$log_q , up_points$temp, pch=23, bg="white", lwd=0.75)
for(i in 1:5){
  text(x = up_points$log_q[i]+c(-0.033,0,0,0,0)[i],
       y = up_points$temp[i]+c(0,-1.1,-1.1,1.35,-1.45)[i],
       font=2,labels = paste0("a.", 1:5)[i], cex=1.15,
       adj = c(c(1,0.5,0.25,0.25,0.5)[i],c(0.35,0,0.35,1,0)[i]))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
image(x = log_q_range, y = temp_range, z = yolo.none_arr_red[,,"pred"],
      xlab="", ylab="", col = yo.none_col_pal,
      useRaster=TRUE, las=1, cex.axis=1.2)
mtext(text = "Floodplain", side = 2, line = 5, font=2, cex=1.15)
axis(side = 3, at = flood_x2_ats, labels = flood_x2_labs/1000, cex.axis=1.2)
mtext(text = bquote(Water~Temperature~"("*degree*C*")"), side = 2, line = 2.5)
plot_label("(c)", x_prop = 0.96, y_prop=0.94, fcex = 1.35)
points(yo.none_points_red$log_qsdy, yo.none_points_red$WTmwk, cex=0.75, col=gray(0.1,0.5))
polygon(polygon.yo.none.df$x, polygon.yo.none.df$y, lty=1, lwd=1)

image(x = log_q_range, y = temp_range, z = yolo.shrt_arr_red[,,"pred"],
      xlab="", ylab="", add=TRUE, col = yo.shrt_col_pal,
      useRaster=TRUE, las=1)
points(yo.shrt_points_red$log_qsdy, yo.shrt_points_red$WTmwk, cex=0.75, col=gray(0.1,0.5))
polygon(polygon.yo.shrt.df$x, polygon.yo.shrt.df$y,lty=5, lwd=1)
points(yo_points$log_q , yo_points$temp, pch=23, bg="white", lwd=0.75)
for(i in 1:5){
  text(x = yo_points$log_q[i]+c(0,0.15,-0.2,0,0)[i],
       y = yo_points$temp[i]+c(-1.15,0,0,2,-2)[i],
       font=2,labels = paste0("b.", 1:5)[i], cex=1.15,
       adj = c(c(0.4,0,1,0,0.25)[i],c(0,0.35,0.35,1,0)[i]))
}

image(x = log_q_range, y = temp_range, z = yolo.none_arr_red[,,"pred"],
      xlab="", ylab="", col = yo.none_col_pal,
      useRaster=TRUE, las=1, cex.axis=1.2)
axis(side = 3, at = flood_x2_ats, labels = flood_x2_labs/1000, cex.axis=1.2)
plot_label("(d)", x_prop = 0.96, y_prop=0.94, fcex = 1.35)
points(yo.none_points_red$log_qsdy, yo.none_points_red$WTmwk, cex=0.75, col=gray(0.1,0.5))
polygon(polygon.yo.none.df$x, polygon.yo.none.df$y, lty=1, lwd=1)

image(x = log_q_range, y = temp_range, z = yolo.long_arr_red[,,"pred"],
      xlab="", ylab="", add=TRUE, col = yo.long_col_pal,
      useRaster=TRUE, las=1)
points(yo.long_points_red$log_qsdy, yo.long_points_red$WTmwk, cex=0.75, col=gray(0.1,0.5))
polygon(polygon.yo.long.df$x, polygon.yo.long.df$y, lty=3, lwd=1)
points(yo_points$log_q , yo_points$temp, pch=23, bg="white", lwd=0.75)
for(i in 1:5){
  text(x = yo_points$log_q[i]+c(0,0.15,-0.2,0,0)[i],
       y = yo_points$temp[i]+c(-1.15,0,0,2,-1.5)[i],
       font=2,labels = paste0("b.", 1:5)[i], cex=1.15,
       adj = c(c(0.4,0,1,0.25,0.25)[i],c(0,0.35,0.35,1,0)[i]))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
image(x = do.log_q_range, y = do.temp_range, z = down.none_arr_red[,,"pred"],
      xlab="", ylab="", col = do.none_col_pal,
      useRaster=TRUE, las=1, cex.axis=1.2)
axis(side = 3, at = down_x2_ats, labels = down_x2_labs/1000, cex.axis=1.2)
mtext(text = "Downstream", side = 2, line = 5, font=2, cex=1.15)
mtext(text = bquote(log[e](Flow~(cfs))), side = 1, line = 3)
mtext(text = bquote(Water~Temperature~"("*degree*C*")"), side = 2, line = 2.5)
plot_label("(e)",  x_prop = 0.96, y_prop=0.94, fcex = 1.35)
points(do.none_points_red$log_qsdy, do.none_points_red$WTmwk, cex=0.75, col=gray(0.1,0.5))
polygon(polygon.do.none.df$x, polygon.do.none.df$y, lty=1, lwd=1)

image(x = do.log_q_range, y = do.temp_range, z = down.shrt_arr_red[,,"pred"],
      xlab="", ylab="", add=TRUE, col = do.shrt_col_pal,
      useRaster=TRUE, las=1)
points(do.shrt_points_red$log_qsdy, do.shrt_points_red$WTmwk, cex=0.75, col=gray(0.1,0.5))
polygon(polygon.do.shrt.df$x, polygon.do.shrt.df$y, lty=5, lwd=1)
points(do_points$log_q , do_points$temp, pch=23, bg="white", lwd=0.75)
for(i in 1:4){
  text(x = do_points$log_q[i]+c(-0.2,-0.25,-0.175,0)[i],
       y = do_points$temp[i]+c(-0.25,0.25,0.75,-0.35)[i],
       font=2,labels = paste0("c.", 1:4)[i], cex=1.15,
       adj = c(c(0,0,0,0.25)[i],c(1,0,0,1)[i]))
}

image(x = do.log_q_range, y = do.temp_range, z = down.none_arr_red[,,"pred"],
      xlab="", ylab="", col = do.none_col_pal,
      useRaster=TRUE, las=1, cex.axis=1.2)
axis(side = 3, at = down_x2_ats, labels = down_x2_labs/1000, cex.axis=1.2)
plot_label("(f)",  x_prop = 0.96, y_prop=0.94, fcex = 1.35)
mtext(text = bquote(log[e](Flow~(cfs))), side = 1, line = 3)
# mtext(text = bquote(Water~Temperature~"("*degree*C*")"), side = 2, line = 2.5)
points(do.none_points_red$log_qsdy, do.none_points_red$WTmwk, cex=0.75, col=gray(0.1,0.5))
polygon(polygon.do.none.df$x, polygon.do.none.df$y, lty=1, lwd=1)

image(x = do.log_q_range, y = do.temp_range, z = down.long_arr_red[,,"pred"],
      xlab="", ylab="", add=TRUE, col = do.long_col_pal,
      useRaster=TRUE, las=1)
points(do.long_points_red$log_qsdy, do.long_points_red$WTmwk, cex=0.75, col=gray(0.1,0.5))
polygon(polygon.do.long.df$x, polygon.do.long.df$y, lty=3, lwd=1)
points(do_points$log_q , do_points$temp, pch=23, bg="white", lwd=0.75)
for(i in 1:4){
  text(x = do_points$log_q[i]+c(-0.2,-0.25,-0.175,0)[i],
       y = do_points$temp[i]+c(-0.25,0.25,0.75,-0.35)[i],
       font=2,labels = paste0("c.", 1:4)[i], cex=1.15,
       adj = c(c(0,0,0,0.25)[i],c(1,0,0,1)[i]))
}

#~~~~~~~  Scale Bar
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 2, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
bar_exp_lab = c(0.6,1,2,3,5,8,12,20,35,55)
bar_exp_ax=(log(bar_exp_lab)-min(yo.all_range))/max((yo.all_range-min(yo.all_range)))
bar_lab = seq(from=-0.5, to=4, by=0.5)
bar_ax=(bar_lab-min(yo.all_range))/max((yo.all_range-min(yo.all_range)))
par(mar=c(4.35,4,79.15,1))
color.bar(col_pal, min=0, max = 1, ticks = FALSE)
axis(side = 1, at = bar_ax, labels=format(bar_lab, digits=3), las=1, cex.axis=1.2)
axis(side = 3, at = bar_exp_ax, labels=format(bar_exp_lab, digits=3), las=1, cex.axis=1.2)
mtext(text = bquote(log[e](Chlorophyll-a)~Color~Scale), side = 1, line = 2.75, xpd=NA)
mtext(text = bquote(Chlorophyll-a~Color~Scale), side = 3, line = 2.1, xpd=NA)
box(which="plot")

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 2.5, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("top", legend = c("No Inundation", "Short Inundation", "Long Inundation"),
       lty=c(1,5,3), horiz = TRUE, cex=1.5, bty='n', lwd=1.5)

dev.off()


# Pointwise comparison estimates (Fig4)---------

## set-up -----------------------

up_points_bc = up_points
yo_points_bc = yo_points
do_points_bc = do_points

up_points_bc[,6:14] = exp(up_points_bc[,6:14])
yo_points_bc[,6:14] = exp(yo_points_bc[,6:14])
do_points_bc[,6:14] = exp(do_points_bc[,6:14])

up.plot_multComp_letters$plot_y = exp(up.plot_multComp_letters$plot_y)
yo_plot_multComp_letters$plot_y = exp(yo_plot_multComp_letters$plot_y)
do.plot_multComp_letters$plot_y = exp(do.plot_multComp_letters$plot_y)

up_y_range = range( na.omit(unlist(up_points_bc[,c(
  "none_lo", "none_up", "shrt_lo", "shrt_up", "long_lo", "long_up")])))
yo_y_range = range(na.omit(unlist(yo_points_bc[,c(
  "none_lo", "none_up", "shrt_lo", "shrt_up", "long_lo", "long_up")])))
do_y_range = range(na.omit(unlist(do_points_bc[,c(
  "none_lo", "none_up", "shrt_lo", "shrt_up", "long_lo", "long_up")])))


## Plot ---------------------------
# Need to run the pdf line in order to create the image plus:
# quartz line for mac; windows line for windows

# quartz(height=7*1.33, width=8.5)
# windows(height=7*1.4, width=8.5)
# pdf("Figure_4_Khanna.pdf", height=7*1.4, width=8.5)
par(mfrow=c(3,1), mar=c(2.5,3,2.5,1), oma=c(5.5,3.5,0,0.5))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~ Mainstem plot
plot(c(up_y_range[1], up_y_range[2]*1.05)~c(1,12), type="n", las=1, xlim=c(0.25,15.75),
     xaxs="i", xaxt="n", xlab="", ylab="", cex.axis=1.2)
mtext(text = "Mainstem", side = 2, line = 5, font=2, cex=1.15)
text(x = -1, y= 7.35, labels = "Figure #4", font=2,  adj = 0, xpd=NA, cex=2)
polygon(x = c(0,3.5,3.5,0,0), y=c(-10,-10,100,100,-10), border=NA, col="gray85")
polygon(x = c(6.5,9.5,9.5,6.5,6.5), y=c(-10,-10,100,100,-10), border=NA, col="gray85")
polygon(x = c(12.5,15.75,15.75,12.5,12.5), y=c(-10,-10,100,100,-10), border=NA, col="gray85")
#polygon(x = c(18.5,21.75,21.75,18.5,18.5), y=c(-10,-10,10,10,-10), border=NA, col="gray85")
abline(v=c(3.5,6.5,9.5, 12.5, 15.75), lty=3, col="gray70", xlab="")
# mtext(text = bquote(log[e](Chl)), side = 2, line = 2.5)
mtext(text = bquote(Chlorophyll-a), side = 2, line = 2.5)
plot_label("(a)", x_prop = 0.02, y_prop=0.94)
text(y = up.plot_multComp_letters$plot_y + (up_y_range[2]*1.05-up_y_range[2]),
     x = up.plot_multComp_letters$plot_x, cex=1.15,
     labels = up.plot_multComp_letters$plot_letters, adj = c(0.5,0.25))
up_sub_panel_lab_y = par('usr')[3]+(par('usr')[4]-par('usr')[3])*0.05
text(y = up_sub_panel_lab_y,
     x =c(3.5,6.5,9.5,12.5, 15.5)-2.85, cex=1.15, font=2,
     labels = paste0("(a.", 1:5,")"), adj = c(0,0.25))

arrows(x0=c(1:3), x1=c(1:3), y0=unlist(up_points_bc[1,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(up_points_bc[1,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(1:3), y=unlist(up_points_bc[1,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple", "green", "tomato3"), cex=1.5)

arrows(x0=c(4:6), x1=c(4:6), y0=unlist(up_points_bc[2,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(up_points_bc[2,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(4:6), y=unlist(up_points_bc[2,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple", "green", "tomato3"), cex=1.5)

arrows(x0=c(7:9), x1=c(7:9), y0=unlist(up_points_bc[3,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(up_points_bc[3,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(7:9), y=unlist(up_points_bc[3,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple", "green", "tomato3"), cex=1.5)

arrows(x0=c(10:12), x1=c(10:12), y0=unlist(up_points_bc[4,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(up_points_bc[4,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(10:12), y=unlist(up_points_bc[4,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple", "green", "tomato3"), cex=1.5)

arrows(x0=c(13:15), x1=c(13:15), y0=unlist(up_points_bc[5,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(up_points_bc[5,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(13:15), y=unlist(up_points_bc[5,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple", "green", "tomato3"), cex=1.5)

for(i in 1:5){
  text(x=c(2,5,8,11,14)[i], y=up_y_range[1]-diff(up_y_range)*0.075, adj=c(0.5,1),
       labels=bquote(Flow~(cfs)*"="*.(op(round(exp(up_points_bc[i,"log_q"]),digits=0),d = 0))),
       xpd=NA, cex=1.3)
  text(x=c(2,5,8,11,14)[i], y=up_y_range[1]-diff(up_y_range)*0.175, adj=c(0.5,1),
       labels=bquote(W.~Temp.~"("*degree*C*")"*"="*.(
         op(round(up_points_bc[i,"temp"],digits=1),d=1))),
       xpd=NA, cex=1.3)
}
box(which="plot")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~ Yolo plot
plot(c(yo_y_range[1], yo_y_range[2]*1.05)~c(1,12), type="n", las=1, xlim=c(0.5,15.5),
     xaxs="i", xaxt="n", xlab="", ylab="", cex.axis=1.2)
mtext(text = "Floodplain", side = 2, line = 5, font=2, cex=1.15)
polygon(x = c(0,3.5,3.5,0,0), y=c(-100,-100,100,100,-10), border=NA, col="gray85")
polygon(x = c(6.5,9.5,9.5,6.5,6.5), y=c(-10,-10,100,100,-10), border=NA, col="gray85")
polygon(x = c(12.5,15.5,15.5,12.5,12.5), y=c(-10,-10,100,100,-10), border=NA, col="gray85")
abline(v=c(3.5,6.5,9.5, 12.5, 15.5, 18.5), lty=3, col="gray70", xlab="")
# mtext(text = bquote(log[e](Chl)), side = 2, line = 2.5)
mtext(text = bquote(Chlorophyll-a), side = 2, line = 2.5)
plot_label("(b)", x_prop = 0.025)
text(y = yo_plot_multComp_letters$plot_y + (yo_y_range[2]*1.05-yo_y_range[2]),
     x = yo_plot_multComp_letters$plot_x, cex=1.15,
     labels = yo_plot_multComp_letters$plot_letters, adj = c(0.5,0.25))
yo_sub_panel_lab_y = par('usr')[3]+(par('usr')[4]-par('usr')[3])*0.05
text(y = yo_sub_panel_lab_y,
     x =c(3.5,6.5,9.5,12.5, 15.5)-2.85, cex=1.15, font=2,
     labels = paste0("(b.", 1:5,")"), adj = c(0,0.25))

arrows(x0=c(1:3), x1=c(1:3), y0=unlist(yo_points_bc[1,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(yo_points_bc[1,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(1:3), y=unlist(yo_points_bc[1,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple", "green", "tomato3"), cex=1.5)

arrows(x0=c(4:6), x1=c(4:6), y0=unlist(yo_points_bc[2,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(yo_points_bc[2,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(4:6), y=unlist(yo_points_bc[2,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple", "green", "tomato3"), cex=1.5)

arrows(x0=c(7:9), x1=c(7:9), y0=unlist(yo_points_bc[3,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(yo_points_bc[3,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(7:9), y=unlist(yo_points_bc[3,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple", "green", "tomato3"), cex=1.5)

arrows(x0=c(10:12), x1=c(10:12), y0=unlist(yo_points_bc[4,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(yo_points_bc[4,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(10:12), y=unlist(yo_points_bc[4,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple", "green", "tomato3"), cex=1.5)

arrows(x0=c(13:15), x1=c(13:15), y0=unlist(yo_points_bc[5,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(yo_points_bc[5,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(13:15), y=unlist(yo_points_bc[5,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple", "green", "tomato3"), cex=1.5)

for(i in 1:5){
  text(x=c(2,5,8,11,14)[i], y=yo_y_range[1]-diff(yo_y_range)*0.075, adj=c(0.5,1),
       labels=bquote(Flow~(cfs)*"="*.(op(round(exp(yo_points_bc[i,"log_q"]),digits=0),d = 0))),
       xpd=NA, cex=1.3)
  text(x=c(2,5,8,11,14)[i], y=yo_y_range[1]-diff(yo_y_range)*0.175, adj=c(0.5,1),
       labels=bquote(W.~Temp.~"("*degree*C*")"*"="*.(
         op(round(yo_points_bc[i,"temp"],digits=1),d=1))),
       xpd=NA, cex=1.3)
}
box(which="plot")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~ Downstream plot
plot(c(do_y_range[1], do_y_range[2]*1.05)~c(1,12), type="n", las=1, xlim=c(0.25,12.5),
     xaxs="i", xaxt="n", xlab="", ylab="", cex.axis=1.2)
mtext(text = "Downstream", side = 2, line = 5, font=2, cex=1.15)
polygon(x = c(0,3.5,3.5,0,0), y=c(-10,-10,10,10,-10), border=NA, col="gray85")
polygon(x = c(6.5,9.5,9.5,6.5,6.5), y=c(-10,-10,10,10,-10), border=NA, col="gray85")
abline(v=c(3.5,6.5,9.5, 12.5, 15.75), lty=3, col="gray70", xlab="")
# mtext(text = bquote(log[e](Chl)), side = 2, line = 2.5)
mtext(text = bquote(Chlorophyll-a), side = 2, line = 2.5)
plot_label("(c)", x_prop = 0.02, y_prop=0.94)
text(y = do.plot_multComp_letters$plot_y + (do_y_range[2]*1.05-do_y_range[2]),
     x = do.plot_multComp_letters$plot_x, cex=1.15,
     labels = do.plot_multComp_letters$plot_letters, adj = c(0.5,0.25))

do_sub_panel_lab_y = par('usr')[3]+(par('usr')[4]-par('usr')[3])*0.05
text(y = do_sub_panel_lab_y,
     x =c(3.5,6.5,9.5,12.5)-0.15, cex=1.15, font=2,
     labels = paste0("(c.", 1:4,")"), adj = c(1,0.25))

arrows(x0=c(1:3), x1=c(1:3), y0=unlist(do_points_bc[1,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(do_points_bc[1,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(1:3), y=unlist(do_points_bc[1,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple", "green", "tomato3"), cex=1.5)

arrows(x0=c(4:6), x1=c(4:6), y0=unlist(do_points_bc[2,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(do_points_bc[2,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(4:6), y=unlist(do_points_bc[2,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple", "green", "tomato3"), cex=1.5)

arrows(x0=c(7:9), x1=c(7:9), y0=unlist(do_points_bc[3,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(do_points_bc[3,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(7:9), y=unlist(do_points_bc[3,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple", "green", "tomato3"), cex=1.5)

arrows(x0=c(10:12), x1=c(10:12), y0=unlist(do_points_bc[4,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(do_points_bc[4,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(10:12), y=unlist(do_points_bc[4,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple", "green", "tomato3"), cex=1.5)

for(i in 1:7){
  text(x=c(2,5,8,11)[i], y=do_y_range[1]-diff(do_y_range)*0.075, adj=c(0.5,1),
       labels=bquote(Flow~(cfs)*"="*.(op(round(exp(do_points_bc[i,"log_q"]),digits=0),
                                         d = 0))),
       xpd=NA, cex=1.3)
  text(x=c(2,5,8,11)[i], y=do_y_range[1]-diff(do_y_range)*0.175, adj=c(0.5,1),
       labels=bquote(W.~Temp.~"("*degree*C*")"*"="*.
                     (op(round(do_points_bc[i,"temp"],digits=1),d=1))),
       xpd=NA, cex=1.3)
}
box(which="plot")

par(mfrow=c(3,1), mar=c(2.5,3,2.5,1), oma=c(5.5,3.5,0,0.5))
par(fig = c(0, 1, 0, 1), oma = c(0, 3.5, 0, 1), mar = c(0, 3, 0, 1), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", legend = c("No Inundation", "Short Inundation", "Long Inundation"),
       pch=21, pt.bg=c("purple", "green", "tomato3"), inset=0.01, pt.cex=2,
       bg = NA, horiz=TRUE, cex=1.5, bty='n')

dev.off()
