
#########################################################################
#########################################################################
#~~   NCEAS Primary Production Inundation Project
#~~   Code Created by C Pien on October 5, 2022
#~~   Last Modified by JW Gaeta  on May 28, 2025
#~~   Purpose: generate all data for subsequent figures
#########################################################################
#########################################################################

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

wd <- here::here("manuscript_code")
load(file.path(wd,"data_clean", "data_gam_results.Rdata"))


#########################################################################
#~~
#~~   Yolo code
#~~
#########################################################################

## Filter to Yolo Region
yolo <- alldata %>% filter(region == "yolo")

## gamyo6d - Yolo final model
yolo_mod = gam(log_chla ~ te(log_qsdy, WTmwk, by = inund_factor) + inund_factor + s(station, bs = "re"), method = "REML", data = yolo, family = "gaussian")


# subset the yolo data frame each inundation factor
yo.none = yolo %>% filter(inund_factor == "none")
yo.shrt = yolo %>% filter(inund_factor == "short")
yo.long = yolo %>% filter(inund_factor == "long")


# Get values
y_table <- yolo %>%
  summarize(minQ = quantile(log_qsdy, 0.05),
            maxQ = quantile(log_qsdy, 0.95),
            minWT = quantile(WTmwk, 0.05),
            maxWT = quantile(WTmwk, 0.95)) %>%
  as.data.frame() %>% unlist()


######################################################################
#~~   Develop model predictions grid
######################################################################

sim_res = 500

log_q_range = seq(
  from = min(yolo$log_qsdy), to=max(yolo$log_qsdy), length.out=sim_res)
temp_range = seq(
  from = min(yolo$WTmwk), to=max(yolo$WTmwk), length.out=sim_res)

yolo.none_arr = array(data = NA, dim = c(sim_res,sim_res,3), dimnames =
                        list(log_q = paste0("log_q_",round(log_q_range,digits=2)),
                             temp = paste0("temp_",round(temp_range,digits=2)),
                             est = c("lower", "pred", "upper")))
yolo.shrt_arr = array(data = NA, dim = c(sim_res,sim_res,3), dimnames =
                        list(log_q = paste0("log_q_",round(log_q_range,digits=2)),
                             temp = paste0("temp_",round(temp_range,digits=2)),
                             est = c("lower", "pred", "upper")))
yolo.long_arr = array(data = NA, dim = c(sim_res,sim_res,3), dimnames =
                        list(log_q = paste0("log_q_",round(log_q_range,digits=2)),
                             temp = paste0("temp_",round(temp_range,digits=2)),
                             est = c("lower", "pred", "upper")))


#~~ Takes approximately one minute each
for(i in 1:sim_res){
  emms = emmeans(object = yolo_mod, specs = c("inund_factor", "WTmwk", "log_qsdy"),
                 at = list(inund_factor = "none", WTmwk = temp_range[i],
                           log_qsdy = log_q_range))
  yolo.none_arr[,i,"lower"] = c(data.frame(emms)[,c("lower.CL")])
  yolo.none_arr[,i,"pred"] = c(data.frame(emms)[,c("emmean")])
  yolo.none_arr[,i,"upper"] = c(data.frame(emms)[,c("upper.CL")])
}
for(i in 1:sim_res){
  emms = emmeans(object = yolo_mod, specs = c("inund_factor", "WTmwk", "log_qsdy"),
                 at = list(inund_factor = "short", WTmwk = temp_range[i],
                           log_qsdy = log_q_range))
  yolo.shrt_arr[,i,"lower"] = c(data.frame(emms)[,c("lower.CL")])
  yolo.shrt_arr[,i,"pred"] = c(data.frame(emms)[,c("emmean")])
  yolo.shrt_arr[,i,"upper"] = c(data.frame(emms)[,c("upper.CL")])
}
for(i in 1:sim_res){
  emms = emmeans(object = yolo_mod, specs = c("inund_factor", "WTmwk", "log_qsdy"),
                 at = list(inund_factor = "long", WTmwk = temp_range[i],
                           log_qsdy = log_q_range))
  yolo.long_arr[,i,"lower"] = c(data.frame(emms)[,c("lower.CL")])
  yolo.long_arr[,i,"pred"] = c(data.frame(emms)[,c("emmean")])
  yolo.long_arr[,i,"upper"] = c(data.frame(emms)[,c("upper.CL")])
}

# windows(width=10.5*0.65, height=9.5*0.9)
par(mfrow=c(3,1), mar=c(4.5,4.5,1.5,1.5))
image(x = log_q_range, y = temp_range, z = yolo.none_arr[,,"pred"],
      xlab="log Q", ylab="Temp")
points(yo.none$log_qsdy, yo.none$WTmwk)

image(x = log_q_range, y = temp_range, z = yolo.shrt_arr[,,"pred"],
      xlab="log Q", ylab="Temp")
points(yo.shrt$log_qsdy, yo.shrt$WTmwk)

image(x = log_q_range, y = temp_range, z = yolo.long_arr[,,"pred"],
      xlab="log Q", ylab="Temp")
points(yo.long$log_qsdy, yo.long$WTmwk)


#############################################################
#~~   Develop convex hulls:
#############################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~   No inundation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
yo.none_points = data.frame(log_qsdy=yo.none$log_qsdy, WTmwk=yo.none$WTmwk)

# remove duplicate observations
yo.none_points_red = yo.none_points[!duplicated(yo.none_points),]

# Alpha controls: the higher the alpha the more inclusive the hull
#   The greater the sample size the lower the alpha (<1 is normal)
alpha_parameter = 5

#~~~~ Build convex hull
yolo.none_con_hull = ahull(
  x=yo.none_points_red, alpha=alpha_parameter)
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
polygon.yo.none.df <- extract_polygons(yolo.none_con_hull$ashape.obj)

# plot(yolo.none_con_hull, lwd=1)
# polygon(polygon.yo.none.df$x, polygon.yo.none.df$y, border="darkred", lwd=2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~   Short inundation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
yo.shrt_points = data.frame(log_qsdy=yo.shrt$log_qsdy, WTmwk=yo.shrt$WTmwk)

# remove duplicate observations
yo.shrt_points_red = yo.shrt_points[!duplicated(yo.shrt_points),]

# Alpha controls: the higher the alpha the more inclusive the hull
#   The greater the sample size the lower the alpha (<1 is normal)
alpha_parameter = 4

#~~~~ Build convex hull
yolo.shrt_con_hull = ahull(
  x=yo.shrt_points_red, alpha=alpha_parameter)
polygon.yo.shrt.df <- extract_polygons(yolo.shrt_con_hull$ashape.obj)

#par(mfrow=c(1,1))
# plot(yolo.shrt_con_hull)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~   Long inundation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
yo.long_points = data.frame(log_qsdy=yo.long$log_qsdy, WTmwk=yo.long$WTmwk)

# remove duplicate observations
yo.long_points_red = yo.long_points[!duplicated(yo.long_points),]

# Alpha controls: the higher the alpha the more inclusive the hull
#   The greater the sample size the lower the alpha (<1 is normal)
alpha_parameter = 3

#~~~~ Build convex hull
yolo.long_con_hull = ahull(
  x=yo.long_points_red, alpha=alpha_parameter)
polygon.yo.long.df <- extract_polygons(yolo.long_con_hull$ashape.obj)

# par(mfrow=c(1,1))
# plot(yolo.long_con_hull)


#############################################################
#~~   Replace matrix values outside of hull with NAs:
#############################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~   No inundation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~ remove cells not in the hull
yolo.none_arr_red = yolo.none_arr
by_row_names = log_q_range
by_col_names = temp_range
for(i in 1:length(by_row_names)){
  in_hull_vec = inahull(
    yolo.none_con_hull, cbind(
      rep(by_row_names[i], times=length(by_col_names)),
      by_col_names))
  yolo.none_arr_red[i,!in_hull_vec,] = NA
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~   Short inundation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~ remove cells not in the hull
yolo.shrt_arr_red = yolo.shrt_arr
by_row_names = log_q_range
by_col_names = temp_range
for(i in 1:length(by_row_names)){
  in_hull_vec = inahull(
    yolo.shrt_con_hull, cbind(
      rep(by_row_names[i], times=length(by_col_names)),
      by_col_names))
  yolo.shrt_arr_red[i,!in_hull_vec,] = NA
}

#~~~~ Manually remove values from hull that appear to be extrapolation!!!
#~~~~ Removed based on log chl = "manual_replace_log_chl_value"

manual_replace_log_chl_value = 0.8


yo.shrt_temp_replace_index = which(temp_range >=11)
yo.shrt_q_replace_index = which(log_q_range >=8.65 & log_q_range <=9.8 )
for(i in yo.shrt_q_replace_index){
  NA_index = as.integer(which(
    yolo.shrt_arr_red[i,yo.shrt_temp_replace_index,"pred"] <
      manual_replace_log_chl_value))
  if(length(NA_index)>0){
    yolo.shrt_arr_red[i,yo.shrt_temp_replace_index[NA_index],] = NA
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~   Long inundation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~ remove cells not in the hull
yolo.long_arr_red = yolo.long_arr
by_row_names = log_q_range
by_col_names = temp_range
for(i in 1:length(by_row_names)){
  in_hull_vec = inahull(
    yolo.long_con_hull, cbind(
      rep(by_row_names[i], times=length(by_col_names)),
      by_col_names))
  yolo.long_arr_red[i,!in_hull_vec,] = NA
}

#############################################################
#~~   Identify convex hull overlap for cherry picking
#############################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
# None & short overlap:
short_none_test = (1*!is.na(yolo.none_arr_red[,,"pred"])) +
  (1*!is.na(yolo.shrt_arr_red[,,"pred"]))
short_none_test_index = data.frame(which(short_none_test==2, arr.ind = TRUE))
short_none_test_index$log_q_val = log_q_range[short_none_test_index$log_q]
short_none_test_index$temp_val = temp_range[short_none_test_index$temp]

short_none_test_index = short_none_test_index[
  which(short_none_test_index$temp_val>=14.99 &
          short_none_test_index$temp_val<=15.03 &
          short_none_test_index$log_q_val>=7.97 &
          short_none_test_index$log_q_val<=8.03),]

yo_points = data.frame(
  point_name = "yo.none.short_overlap",
  mat_x = short_none_test_index$log_q[which(
    short_none_test_index$log_q_val == quantile(unique(
      short_none_test_index$log_q_val), probs=0.5,type=1))[1]],
  mat_y = short_none_test_index$temp[which(
    short_none_test_index$temp_val == quantile(unique(
      short_none_test_index$temp_val), probs=0.5,type=1))[1]],
  log_q = quantile(unique(short_none_test_index$log_q_val), probs=0.5,type=1),
  temp = quantile(unique(short_none_test_index$temp_val), probs=0.5,type=1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
# None & ONLY:
none_test = (1*!is.na(yolo.none_arr_red[,,"pred"]))
none_test_index = data.frame(which(short_none_test==1, arr.ind = TRUE))
none_test_index$log_q_val = log_q_range[none_test_index$log_q]
none_test_index$temp_val = temp_range[none_test_index$temp]

none_only_test_index1 = none_test_index[
  which(none_test_index$temp_val>=16.98 & none_test_index$temp_val<=17.03 &
          none_test_index$log_q_val>=5.97 & none_test_index$log_q_val<=6.03),]
none_only_test_index2 = none_test_index[
  which(none_test_index$temp_val>=9.56 & none_test_index$temp_val<=9.61 &
          none_test_index$log_q_val>=5.97 & none_test_index$log_q_val<=6.03),]

yo_points = rbind(
  yo_points, data.frame(
    point_name = "yo.none_lo_Q_lo_Temp", mat_x = none_only_test_index2$log_q[
      which(none_only_test_index2$log_q_val == quantile(unique(
        none_only_test_index2$log_q_val), probs=0.5,type=1))[1]],
    mat_y = none_only_test_index2$temp[which(
      none_only_test_index2$temp_val == quantile(unique(
        none_only_test_index2$temp_val), probs=0.5,type=1))[1]],
    log_q = quantile(unique(none_only_test_index2$log_q_val), probs=0.5,type=1),
    temp = quantile(unique(none_only_test_index2$temp_val), probs=0.5,type=1)))

yo_points = rbind(
  yo_points, data.frame(
    point_name = "yo.none_lo_Q_hi_Temp",mat_x = none_only_test_index1$log_q[
      which(none_only_test_index1$log_q_val == quantile(unique(
        none_only_test_index1$log_q_val), probs=0.5,type=1))[1]],
    mat_y = none_only_test_index1$temp[which(
      none_only_test_index1$temp_val == quantile(unique(
        none_only_test_index1$temp_val), probs=0.5,type=1))[1]],
    log_q = quantile(unique(none_only_test_index1$log_q_val), probs=0.5,type=1),
    temp = quantile(unique(none_only_test_index1$temp_val), probs=0.5,type=1)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
# Short & long overlap:
long_short_test = (1*!is.na(yolo.shrt_arr_red[,,"pred"])) +
  (1*!is.na(yolo.long_arr_red[,,"pred"]))
long_short_test_index = data.frame(which(long_short_test==2, arr.ind = TRUE))
long_short_test_index$log_q_val = log_q_range[long_short_test_index$log_q]
long_short_test_index$temp_val = temp_range[long_short_test_index$temp]

# Reduce to look at high flow values
long_short_test_index_hi_temp = long_short_test_index[which(
  long_short_test_index$log_q_val>=11.5),]
yo_points = rbind(
  yo_points, data.frame(
    point_name = "yo.short.long_overlap_hi_Q_lo_Temp",
    mat_x = long_short_test_index_hi_temp$log_q[which(
      long_short_test_index_hi_temp$log_q_val == quantile(unique(
        long_short_test_index_hi_temp$log_q_val), probs=0.5,type=1))[1]],
    mat_y = long_short_test_index_hi_temp$temp[which(
      long_short_test_index_hi_temp$temp_val == quantile(unique(
        long_short_test_index_hi_temp$temp_val), probs=0.5,type=1))[1]],
    log_q = quantile(unique(long_short_test_index_hi_temp$log_q_val),
                     probs=0.5,type=1),
    temp = quantile(unique(long_short_test_index_hi_temp$temp_val),
                    probs=0.5,type=1)))

# Reduce to look at low temps, moderate flow values
long_short_test_index_mod_Q_mod_Temp = long_short_test_index[which(
  long_short_test_index$log_q_val>=9.75 &
    long_short_test_index$log_q_val <=10.25 &
    long_short_test_index$temp_val>=11 & long_short_test_index$temp_val <=12),]

yo_points = rbind(
  yo_points, data.frame(
    point_name = "yo.short.long_overlap_mod_Q_mod_Temp",
    mat_x = long_short_test_index_mod_Q_mod_Temp$log_q[which(
      long_short_test_index_mod_Q_mod_Temp$log_q_val == quantile(unique(
        long_short_test_index_mod_Q_mod_Temp$log_q_val), probs=0.5,type=1))[1]],
    mat_y = long_short_test_index_mod_Q_mod_Temp$temp[which(
      long_short_test_index_mod_Q_mod_Temp$temp_val == quantile(unique(
        long_short_test_index_mod_Q_mod_Temp$temp_val), probs=0.5,type=1))[1]],
    log_q = quantile(unique(long_short_test_index_mod_Q_mod_Temp$log_q_val),
                     probs=0.5,type=1),
    temp = quantile(unique(long_short_test_index_mod_Q_mod_Temp$temp_val),
                    probs=0.5,type=1)))

# Reduce to look at high temps, moderate flow values
long_short_test_index_mod_Q_hi_Temp = long_short_test_index[which(
  long_short_test_index$log_q_val>=8 & long_short_test_index$log_q_val <=9.25 &
    long_short_test_index$temp_val>=14),]

yo_points = rbind(
  yo_points, data.frame(
    point_name = "yo.short.long_overlap_mod_Q_hi_Temp",
    mat_x = long_short_test_index_mod_Q_hi_Temp$log_q[
      which(long_short_test_index_mod_Q_hi_Temp$log_q_val == quantile(
        unique(long_short_test_index_mod_Q_hi_Temp$log_q_val),
        probs=0.5,type=1))[1]],
    mat_y = long_short_test_index_mod_Q_hi_Temp$temp[
      which(long_short_test_index_mod_Q_hi_Temp$temp_val == quantile(
        unique(long_short_test_index_mod_Q_hi_Temp$temp_val),
        probs=0.5,type=1))[1]],
    log_q = quantile(unique(long_short_test_index_mod_Q_hi_Temp$log_q_val),
                     probs=0.5,type=1),
    temp = quantile(unique(long_short_test_index_mod_Q_hi_Temp$temp_val),
                    probs=0.5,type=1)))

# Reduce to look at high temps, highish flow values
long_short_test_index_hi_Q_hi_Temp = long_short_test_index[
  which(long_short_test_index$log_q_val>=9.98 &
          long_short_test_index$log_q_val <=10.02 &
          long_short_test_index$temp_val>=14.98 &
          long_short_test_index$temp_val<=15.02),]

yo_points = rbind(
  yo_points, data.frame(
    point_name = "yo.short.long_overlap_hi_Q_hi_Temp",
    mat_x = long_short_test_index_hi_Q_hi_Temp$log_q[
      which(long_short_test_index_hi_Q_hi_Temp$log_q_val == quantile(
        unique(long_short_test_index_hi_Q_hi_Temp$log_q_val),
        probs=0.5,type=1))[1]],
    mat_y = long_short_test_index_hi_Q_hi_Temp$temp[
      which(long_short_test_index_hi_Q_hi_Temp$temp_val == quantile(
        unique(long_short_test_index_hi_Q_hi_Temp$temp_val),
        probs=0.5,type=1))[1]], log_q = quantile(
          unique(long_short_test_index_hi_Q_hi_Temp$log_q_val), probs=0.5,type=1),
    temp = quantile(unique(long_short_test_index_hi_Q_hi_Temp$temp_val),
                    probs=0.5,type=1)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
#compile points
point_n = dim(yo_points)[1]
point_mat = matrix(
  data=NA, ncol=9, nrow=point_n, dimnames=list(NULL, col=c(
    "none_pred", "none_lo", "none_up", "shrt_pred", "shrt_lo", "shrt_up",
    "long_pred", "long_lo", "long_up")))

for(i in 1:point_n){
  point_mat[i,"none_pred"] = yolo.none_arr_red[
    yo_points$mat_x[i],yo_points$mat_y[i],"pred"]
  point_mat[i,"none_lo"] = yolo.none_arr_red[
    yo_points$mat_x[i],yo_points$mat_y[i],"lower"]
  point_mat[i,"none_up"] = yolo.none_arr_red[
    yo_points$mat_x[i],yo_points$mat_y[i],"upper"]

  point_mat[i,"shrt_pred"] = yolo.shrt_arr_red[
    yo_points$mat_x[i],yo_points$mat_y[i],"pred"]
  point_mat[i,"shrt_lo"] = yolo.shrt_arr_red[
    yo_points$mat_x[i],yo_points$mat_y[i],"lower"]
  point_mat[i,"shrt_up"] = yolo.shrt_arr_red[
    yo_points$mat_x[i],yo_points$mat_y[i],"upper"]

  point_mat[i,"long_pred"] = yolo.long_arr_red[
    yo_points$mat_x[i],yo_points$mat_y[i],"pred"]
  point_mat[i,"long_lo"] = yolo.long_arr_red[
    yo_points$mat_x[i],yo_points$mat_y[i],"lower"]
  point_mat[i,"long_up"] = yolo.long_arr_red[
    yo_points$mat_x[i],yo_points$mat_y[i],"upper"]
}

point_dat = data.frame(point_mat)

yo_points = cbind(yo_points, point_dat)
yo_points = yo_points[order(yo_points$log_q),]


#########################################################################
#~~
#~~   Upstream code
#~~
#########################################################################

unique(alldata$region)
upstream <- alldata %>% filter(region == "upstream")

## gamu6d - Downstream final model
up_mod = gam(log_chla ~ te(log_qsdy, WTmwk, by = inund_factor) + inund_factor +
               s(station, bs = "re"), method = "REML", data = upstream,
             family = "gaussian")


# subset the upstream data frame each inundation factor
up.none = upstream %>% filter(inund_factor == "none")
up.shrt = upstream %>% filter(inund_factor == "short")
up.long = upstream %>% filter(inund_factor == "long")


# Get values
y_table <- upstream %>%
  summarize(minQ = quantile(log_qsdy, 0.05),
            maxQ = quantile(log_qsdy, 0.95),
            minWT = quantile(WTmwk, 0.05),
            maxWT = quantile(WTmwk, 0.95)) %>%
  as.data.frame() %>% unlist()


######################################################################
#~~   Develop model predictions grid
######################################################################

sim_res = 250

up.log_q_range = seq(
  from = min(upstream$log_qsdy), to=max(upstream$log_qsdy), length.out=sim_res)
up.temp_range = seq(
  from = min(upstream$WTmwk), to=max(upstream$WTmwk), length.out=sim_res)

up.none_arr = array(data = NA, dim = c(sim_res,sim_res,3), dimnames =
                      list(log_q = paste0("log_q_",round(up.log_q_range,digits=2)),
                           temp = paste0("temp_",round(up.temp_range,digits=2)),
                           est = c("lower", "pred", "upper")))
up.shrt_arr = array(data = NA, dim = c(sim_res,sim_res,3), dimnames =
                      list(log_q = paste0("log_q_",round(up.log_q_range,digits=2)),
                           temp = paste0("temp_",round(up.temp_range,digits=2)),
                           est = c("lower", "pred", "upper")))
up.long_arr = array(data = NA, dim = c(sim_res,sim_res,3), dimnames =
                      list(log_q = paste0("log_q_",round(up.log_q_range,digits=2)),
                           temp = paste0("temp_",round(up.temp_range,digits=2)),
                           est = c("lower", "pred", "upper")))

for(i in 1:sim_res){
  emms = emmeans(object = up_mod, specs = c("inund_factor", "WTmwk", "log_qsdy"),
                 at = list(inund_factor = "none", WTmwk = up.temp_range[i],
                           log_qsdy = up.log_q_range))
  up.none_arr[,i,"lower"] = c(data.frame(emms)[,c("lower.CL")])
  up.none_arr[,i,"pred"] = c(data.frame(emms)[,c("emmean")])
  up.none_arr[,i,"upper"] = c(data.frame(emms)[,c("upper.CL")])
}
for(i in 1:sim_res){
  emms = emmeans(object = up_mod, specs = c("inund_factor", "WTmwk", "log_qsdy"),
                 at = list(inund_factor = "short", WTmwk = up.temp_range[i],
                           log_qsdy = up.log_q_range))
  up.shrt_arr[,i,"lower"] = c(data.frame(emms)[,c("lower.CL")])
  up.shrt_arr[,i,"pred"] = c(data.frame(emms)[,c("emmean")])
  up.shrt_arr[,i,"upper"] = c(data.frame(emms)[,c("upper.CL")])
}
for(i in 1:sim_res){
  emms = emmeans(object = up_mod, specs = c("inund_factor", "WTmwk", "log_qsdy"),
                 at = list(inund_factor = "long", WTmwk = up.temp_range[i],
                           log_qsdy = up.log_q_range))
  up.long_arr[,i,"lower"] = c(data.frame(emms)[,c("lower.CL")])
  up.long_arr[,i,"pred"] = c(data.frame(emms)[,c("emmean")])
  up.long_arr[,i,"upper"] = c(data.frame(emms)[,c("upper.CL")])
}

par(mfrow=c(3,1), mar=c(4.5,4.5,1.5,1.5))
image(x = up.log_q_range, y = up.temp_range, z = up.none_arr[,,"pred"],
      xlab="log Q", ylab="Temp")
points(up.none$log_qsdy, up.none$WTmwk)

image(x = up.log_q_range, y = up.temp_range, z = up.shrt_arr[,,"pred"],
      xlab="log Q", ylab="Temp")
points(up.shrt$log_qsdy, up.shrt$WTmwk)

image(x = up.log_q_range, y = up.temp_range, z = up.long_arr[,,"pred"],
      xlab="log Q", ylab="Temp")
points(up.long$log_qsdy, up.long$WTmwk)


#############################################################
#~~   Develop convex hulls:
#############################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~   No inundation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
up.none_points = data.frame(log_qsdy=up.none$log_qsdy, WTmwk=up.none$WTmwk)

# remove duplicate observations
up.none_points_red = up.none_points[!duplicated(up.none_points),]

# Alpha controls: the higher the alpha the more inclusive the hull
#   The greater the sample size the lower the alpha (<1 is normal)
alpha_parameter = 5

#~~~~ Build convex hull
up.none_con_hull = ahull(
  x=up.none_points_red, alpha=alpha_parameter)
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
polygon.up.none.df <- extract_polygons(up.none_con_hull$ashape.obj)

# par(mfrow=c(1,1))
# plot(up.none_con_hull, lwd=1)
# polygon(polygon.up.none.df$x, polygon.up.none.df$y, border="darkred", lwd=2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~   Short inundation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
up.shrt_points = data.frame(log_qsdy=up.shrt$log_qsdy, WTmwk=up.shrt$WTmwk)

# remove duplicate observations
up.shrt_points_red = up.shrt_points[!duplicated(up.shrt_points),]

# Alpha controls: the higher the alpha the more inclusive the hull
#   The greater the sample size the lower the alpha (<1 is normal)
alpha_parameter = 4

#~~~~ Build convex hull
up.shrt_con_hull = ahull(
  x=up.shrt_points_red, alpha=alpha_parameter)
polygon.up.shrt.df <- extract_polygons(up.shrt_con_hull$ashape.obj)

# par(mfrow=c(1,1))
# plot(up.shrt_con_hull)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~   Long inundation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
up.long_points = data.frame(log_qsdy=up.long$log_qsdy, WTmwk=up.long$WTmwk)

# remove duplicate observations
up.long_points_red = up.long_points[!duplicated(up.long_points),]

# Alpha controls: the higher the alpha the more inclusive the hull
#   The greater the sample size the lower the alpha (<1 is normal)
alpha_parameter = 6

#~~~~ Build convex hull
up.long_con_hull = ahull(
  x=up.long_points_red, alpha=alpha_parameter)
polygon.up.long.df <- extract_polygons(up.long_con_hull$ashape.obj)

# par(mfrow=c(1,1))
# plot(up.long_con_hull)


#############################################################
#~~   Replace matrix values outside of hull with NAs:
#############################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~   No inundation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~ remove cells not in the hull
up.none_arr_red = up.none_arr
by_row_names = up.log_q_range
by_col_names = up.temp_range
for(i in 1:length(by_row_names)){
  in_hull_vec = inahull(
    up.none_con_hull, cbind(
      rep(by_row_names[i], times=length(by_col_names)),
      by_col_names))
  up.none_arr_red[i,!in_hull_vec,] = NA
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~   Short inundation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~ remove cells not in the hull
up.shrt_arr_red = up.shrt_arr
by_row_names = up.log_q_range
by_col_names = up.temp_range
for(i in 1:length(by_row_names)){
  in_hull_vec = inahull(
    up.shrt_con_hull, cbind(
      rep(by_row_names[i], times=length(by_col_names)),
      by_col_names))
  up.shrt_arr_red[i,!in_hull_vec,] = NA
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~   Long inundation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~ remove cells not in the hull
up.long_arr_red = up.long_arr
by_row_names = up.log_q_range
by_col_names = up.temp_range
for(i in 1:length(by_row_names)){
  in_hull_vec = inahull(
    up.long_con_hull, cbind(
      rep(by_row_names[i], times=length(by_col_names)),
      by_col_names))
  up.long_arr_red[i,!in_hull_vec,] = NA
}

#############################################################
#~~   Identify convex hull overlap for cherry picking
#############################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
# None & short overlap:
short_none_test = (1*!is.na(up.none_arr_red[,,"pred"])) + (1*!is.na(up.shrt_arr_red[,,"pred"]))
short_none_test_index = data.frame(which(short_none_test==2, arr.ind = TRUE))
short_none_test_index$log_q_val = up.log_q_range[short_none_test_index$log_q]
short_none_test_index$temp_val = up.temp_range[short_none_test_index$temp]

short_none_test_index = short_none_test_index[
  which(short_none_test_index$temp_val>=10.25 & short_none_test_index$temp_val<=10.6 &
          short_none_test_index$log_q_val>=10.25 & short_none_test_index$log_q_val<=10.75),]

up_points = data.frame(
  point_name = "up.none.short_overlap",
  mat_x = short_none_test_index$log_q[which(short_none_test_index$log_q_val == quantile(unique(short_none_test_index$log_q_val), probs=0.5,type=1))[1]],
  mat_y = short_none_test_index$temp[which(short_none_test_index$temp_val == quantile(unique(short_none_test_index$temp_val), probs=0.5,type=1))[1]],
  log_q = quantile(unique(short_none_test_index$log_q_val), probs=0.5,type=1),
  temp = quantile(unique(short_none_test_index$temp_val), probs=0.5,type=1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
# None ONLY:
none_test = (1*!is.na(up.none_arr_red[,,"pred"]))
none_test_index = data.frame(which(short_none_test==1, arr.ind = TRUE))
none_test_index$log_q_val = up.log_q_range[none_test_index$log_q]
none_test_index$temp_val = up.temp_range[none_test_index$temp]

none_only_test_index1 = none_test_index[
  which(none_test_index$temp_val>=10.45 & none_test_index$temp_val<=10.55 &
          none_test_index$log_q_val>=9 & none_test_index$log_q_val<=9.25),]
none_only_test_index2 = none_test_index[
  which(none_test_index$temp_val>=17.5 &
          none_test_index$log_q_val>=9 & none_test_index$log_q_val<=9.25),]

up_points = rbind(
  up_points,  data.frame(
    point_name = "up.none_lo_Q_lo_Temp", mat_x = none_only_test_index2$log_q[
      which(none_only_test_index2$log_q_val == quantile(unique(
        none_only_test_index2$log_q_val), probs=0.5,type=1))[1]],
    mat_y = none_only_test_index2$temp[which(
      none_only_test_index2$temp_val == quantile(unique(
        none_only_test_index2$temp_val), probs=0.5,type=1))[1]],
    log_q = quantile(unique(none_only_test_index2$log_q_val), probs=0.5,type=1),
    temp = quantile(unique(none_only_test_index2$temp_val), probs=0.5,type=1)))

up_points = rbind(
  up_points, data.frame(
    point_name = "up.none_lo_Q_hi_Temp", mat_x = none_only_test_index1$log_q[
      which(none_only_test_index1$log_q_val == quantile(unique(
        none_only_test_index1$log_q_val), probs=0.5,type=1))[1]],
    mat_y = none_only_test_index1$temp[which(
      none_only_test_index1$temp_val == quantile(unique(
        none_only_test_index1$temp_val), probs=0.5,type=1))[1]],
    log_q = quantile(unique(none_only_test_index1$log_q_val), probs=0.5,type=1),
    temp = quantile(unique(none_only_test_index1$temp_val), probs=0.5,type=1)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
# long Bottom Corner:
long_onlytest = (1*!is.na(up.long_arr_red[,,"pred"]))
long_onlytest_index = data.frame(which(long_onlytest==1, arr.ind = TRUE))
long_onlytest_index$log_q_val = up.log_q_range[long_onlytest_index$log_q]
long_onlytest_index$temp_val = up.temp_range[long_onlytest_index$temp]

# Reduce to look at high flow values
long_onlytest_index_hi_temp = long_onlytest_index[which(
  long_onlytest_index$log_q_val>=11.1 & long_onlytest_index$temp_val<9.5 ),]

up_points = rbind(up_points,
                  data.frame(
                    point_name = "up.long_only_hi_Q_lo_Temp",
                    mat_x = long_onlytest_index_hi_temp$log_q[which(long_onlytest_index_hi_temp$log_q_val == quantile(unique(long_onlytest_index_hi_temp$log_q_val), probs=0.5,type=1))[1]],
                    mat_y = long_onlytest_index_hi_temp$temp[which(long_onlytest_index_hi_temp$temp_val == quantile(unique(long_onlytest_index_hi_temp$temp_val), probs=0.5,type=1))[1]],
                    log_q = quantile(unique(long_onlytest_index_hi_temp$log_q_val), probs=0.5,type=1),
                    temp = quantile(unique(long_onlytest_index_hi_temp$temp_val), probs=0.5,type=1)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
# Short & long overlap:

# none_test = (1*!is.na(up.none_arr_red[,,"pred"]))
# none_test_index = data.frame(which(short_none_test==1, arr.ind = TRUE))
# none_test_index$log_q_val = up.log_q_range[none_test_index$log_q]
# none_test_index$temp_val = up.temp_range[none_test_index$temp]

long_short_test = (1*!is.na(up.shrt_arr_red[,,"pred"])) +
  (1*!is.na(up.long_arr_red[,,"pred"]))
long_short_test_index = data.frame(which(long_short_test==2, arr.ind = TRUE))
long_short_test_index$log_q_val = log_q_range[long_short_test_index$log_q]
long_short_test_index$temp_val = temp_range[long_short_test_index$temp]


# Reduce to look at low temps, moderate flow values
long_short_test_index_mod_Q_mod_Temp = long_short_test_index[which(
  long_short_test_index$log_q_val>=10.75 &
    long_short_test_index$log_q_val <=11.25 &
    long_short_test_index$temp_val>=12.75 &
    long_short_test_index$temp_val <=15),]

up_points = rbind(
  up_points, data.frame(
    point_name = "up.short.long_overlap_mod_Q_mod_Temp",
    mat_x = long_short_test_index_mod_Q_mod_Temp$log_q[which(
      long_short_test_index_mod_Q_mod_Temp$log_q_val == quantile(unique(
        long_short_test_index_mod_Q_mod_Temp$log_q_val), probs=0.5,type=1))[1]],
    mat_y = long_short_test_index_mod_Q_mod_Temp$temp[which(
      long_short_test_index_mod_Q_mod_Temp$temp_val == quantile(unique(
        long_short_test_index_mod_Q_mod_Temp$temp_val), probs=0.5,type=1))[1]],
    log_q = quantile(unique(long_short_test_index_mod_Q_mod_Temp$log_q_val),
                     probs=0.5,type=1),
    temp = quantile(unique(long_short_test_index_mod_Q_mod_Temp$temp_val),
                    probs=0.5,type=1)))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
#compile points
point_n = dim(up_points)[1]
point_mat = matrix(data=NA, ncol=9, nrow=point_n,
                   dimnames=list(NULL, col=c("none_pred", "none_lo", "none_up", "shrt_pred", "shrt_lo", "shrt_up", "long_pred", "long_lo", "long_up")))

for(i in 1:point_n){
  point_mat[i,"none_pred"] = up.none_arr_red[up_points$mat_x[i],up_points$mat_y[i],"pred"]
  point_mat[i,"none_lo"] = up.none_arr_red[up_points$mat_x[i],up_points$mat_y[i],"lower"]
  point_mat[i,"none_up"] = up.none_arr_red[up_points$mat_x[i],up_points$mat_y[i],"upper"]

  point_mat[i,"shrt_pred"] = up.shrt_arr_red[up_points$mat_x[i],up_points$mat_y[i],"pred"]
  point_mat[i,"shrt_lo"] = up.shrt_arr_red[up_points$mat_x[i],up_points$mat_y[i],"lower"]
  point_mat[i,"shrt_up"] = up.shrt_arr_red[up_points$mat_x[i],up_points$mat_y[i],"upper"]

  point_mat[i,"long_pred"] = up.long_arr_red[up_points$mat_x[i],up_points$mat_y[i],"pred"]
  point_mat[i,"long_lo"] = up.long_arr_red[up_points$mat_x[i],up_points$mat_y[i],"lower"]
  point_mat[i,"long_up"] = up.long_arr_red[up_points$mat_x[i],up_points$mat_y[i],"upper"]
}

point_dat = data.frame(point_mat)

up_points = cbind(up_points, point_dat)
up_points = up_points[order(up_points$log_q),]


#########################################################################
#~~
#~~   Downstream code
#~~
#########################################################################

unique(alldata$region)
downstream <- alldata %>% filter(region == "downstream")

## gamu6d - Downstream final model
down_mod = gam(log_chla ~ te(log_qsdy, WTmwk, by = inund_factor) + inund_factor + s(station, bs = "re"), method = "REML", data = downstream, family = "gaussian")


# subset the downstream data frame each inundation factor
do.none = downstream %>% filter(inund_factor == "none")
do.shrt = downstream %>% filter(inund_factor == "short")
do.long = downstream %>% filter(inund_factor == "long")


# Get values
y_table <- downstream %>%
  summarize(minQ = quantile(log_qsdy, 0.05),
            maxQ = quantile(log_qsdy, 0.95),
            minWT = quantile(WTmwk, 0.05),
            maxWT = quantile(WTmwk, 0.95)) %>%
  as.data.frame() %>% unlist()


######################################################################
#~~   Develop model predictions grid
######################################################################

sim_res = 250

do.log_q_range = seq(
  from = min(downstream$log_qsdy), to=max(downstream$log_qsdy), length.out=sim_res)
do.temp_range = seq(
  from = min(downstream$WTmwk), to=max(downstream$WTmwk), length.out=sim_res)

down.none_arr = array(data = NA, dim = c(sim_res,sim_res,3), dimnames =
                        list(log_q = paste0("log_q_",round(do.log_q_range,digits=2)),
                             temp = paste0("temp_",round(do.temp_range,digits=2)),
                             est = c("lower", "pred", "upper")))
down.shrt_arr = array(data = NA, dim = c(sim_res,sim_res,3), dimnames =
                        list(log_q = paste0("log_q_",round(do.log_q_range,digits=2)),
                             temp = paste0("temp_",round(do.temp_range,digits=2)),
                             est = c("lower", "pred", "upper")))
down.long_arr = array(data = NA, dim = c(sim_res,sim_res,3), dimnames =
                        list(log_q = paste0("log_q_",round(do.log_q_range,digits=2)),
                             temp = paste0("temp_",round(do.temp_range,digits=2)),
                             est = c("lower", "pred", "upper")))

for(i in 1:sim_res){
  emms = emmeans(object = down_mod, specs = c("inund_factor", "WTmwk", "log_qsdy"),
                 at = list(inund_factor = "none", WTmwk = do.temp_range[i],
                           log_qsdy = do.log_q_range))
  down.none_arr[,i,"lower"] = c(data.frame(emms)[,c("lower.CL")])
  down.none_arr[,i,"pred"] = c(data.frame(emms)[,c("emmean")])
  down.none_arr[,i,"upper"] = c(data.frame(emms)[,c("upper.CL")])
}
for(i in 1:sim_res){
  emms = emmeans(object = down_mod, specs = c("inund_factor", "WTmwk", "log_qsdy"),
                 at = list(inund_factor = "short", WTmwk = do.temp_range[i],
                           log_qsdy = do.log_q_range))
  down.shrt_arr[,i,"lower"] = c(data.frame(emms)[,c("lower.CL")])
  down.shrt_arr[,i,"pred"] = c(data.frame(emms)[,c("emmean")])
  down.shrt_arr[,i,"upper"] = c(data.frame(emms)[,c("upper.CL")])
}
for(i in 1:sim_res){
  emms = emmeans(object = down_mod, specs = c("inund_factor", "WTmwk", "log_qsdy"),
                 at = list(inund_factor = "long", WTmwk = do.temp_range[i],
                           log_qsdy = do.log_q_range))
  down.long_arr[,i,"lower"] = c(data.frame(emms)[,c("lower.CL")])
  down.long_arr[,i,"pred"] = c(data.frame(emms)[,c("emmean")])
  down.long_arr[,i,"upper"] = c(data.frame(emms)[,c("upper.CL")])
}

par(mfrow=c(3,1), mar=c(4.5,4.5,1.5,1.5))
image(x = do.log_q_range, y = do.temp_range, z = down.none_arr[,,"pred"],
      xlab="log Q", ylab="Temp")
points(do.none$log_qsdy, do.none$WTmwk)

image(x = do.log_q_range, y = do.temp_range, z = down.shrt_arr[,,"pred"],
      xlab="log Q", ylab="Temp")
points(do.shrt$log_qsdy, do.shrt$WTmwk)

image(x = do.log_q_range, y = do.temp_range, z = down.long_arr[,,"pred"],
      xlab="log Q", ylab="Temp")
points(do.long$log_qsdy, do.long$WTmwk)


#############################################################
#~~   Develop convex hulls:
#############################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~   No inundation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
do.none_points = data.frame(log_qsdy=do.none$log_qsdy, WTmwk=do.none$WTmwk)

# remove duplicate observations
do.none_points_red = do.none_points[!duplicated(do.none_points),]

# Alpha controls: the higher the alpha the more inclusive the hull
#   The greater the sample size the lower the alpha (<1 is normal)
alpha_parameter = 5

#~~~~ Build convex hull
down.none_con_hull = ahull(
  x=do.none_points_red, alpha=alpha_parameter)
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
polygon.do.none.df <- extract_polygons(down.none_con_hull$ashape.obj)

# plot(down.none_con_hull, lwd=1)
# polygon(polygon.do.none.df$x, polygon.do.none.df$y, border="darkred", lwd=2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~   Short inundation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
do.shrt_points = data.frame(log_qsdy=do.shrt$log_qsdy, WTmwk=do.shrt$WTmwk)

# remove duplicate observations
do.shrt_points_red = do.shrt_points[!duplicated(do.shrt_points),]

# Alpha controls: the higher the alpha the more inclusive the hull
#   The greater the sample size the lower the alpha (<1 is normal)
alpha_parameter = 4

#~~~~ Build convex hull
down.shrt_con_hull = ahull(
  x=do.shrt_points_red, alpha=alpha_parameter)
polygon.do.shrt.df <- extract_polygons(down.shrt_con_hull$ashape.obj)

#par(mfrow=c(1,1))
# plot(down.shrt_con_hull)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~   Long inundation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
do.long_points = data.frame(log_qsdy=do.long$log_qsdy, WTmwk=do.long$WTmwk)

# remove duplicate observations
do.long_points_red = do.long_points[!duplicated(do.long_points),]

# Alpha controls: the higher the alpha the more inclusive the hull
#   The greater the sample size the lower the alpha (<1 is normal)
alpha_parameter = 5.5

#~~~~ Build convex hull
down.long_con_hull = ahull(
  x=do.long_points_red, alpha=alpha_parameter)
polygon.do.long.df <- extract_polygons(down.long_con_hull$ashape.obj)

# par(mfrow=c(1,1))
# plot(down.long_con_hull)


#############################################################
#~~   Replace matrix values outside of hull with NAs:
#############################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~   No inundation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~ remove cells not in the hull
down.none_arr_red = down.none_arr
by_row_names = do.log_q_range
by_col_names = do.temp_range
for(i in 1:length(by_row_names)){
  in_hull_vec = inahull(
    down.none_con_hull, cbind(
      rep(by_row_names[i], times=length(by_col_names)),
      by_col_names))
  down.none_arr_red[i,!in_hull_vec,] = NA
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~   Short inundation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~ remove cells not in the hull
down.shrt_arr_red = down.shrt_arr
by_row_names = do.log_q_range
by_col_names = do.temp_range
for(i in 1:length(by_row_names)){
  in_hull_vec = inahull(
    down.shrt_con_hull, cbind(
      rep(by_row_names[i], times=length(by_col_names)),
      by_col_names))
  down.shrt_arr_red[i,!in_hull_vec,] = NA
}

#~~~~ Manually remove values from hull that appear to be extrapolation!!!
#~~~~ Removed based on log chl = "manual_replace_log_chl_value"

manual_replace_log_chl_value = 0.8


do.shrt_temp_replace_index = which(do.temp_range >=11)
do.shrt_q_replace_index = which(do.log_q_range >=8.65 & do.log_q_range <=9.8 )
for(i in do.shrt_q_replace_index){
  NA_index = as.integer(which(down.shrt_arr_red[i,do.shrt_temp_replace_index,"pred"] < manual_replace_log_chl_value))
  if(length(NA_index)>0){
    down.shrt_arr_red[i,do.shrt_temp_replace_index[NA_index],] = NA
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~   Long inundation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~ remove cells not in the hull
down.long_arr_red = down.long_arr
by_row_names = do.log_q_range
by_col_names = do.temp_range
for(i in 1:length(by_row_names)){
  in_hull_vec = inahull(
    down.long_con_hull, cbind(
      rep(by_row_names[i], times=length(by_col_names)),
      by_col_names))
  down.long_arr_red[i,!in_hull_vec,] = NA
}


#############################################################
#~~   Identify convex hull overlap for cherry picking
#############################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
# None & short overlap:
short_none_test = (1*!is.na(down.none_arr_red[,,"pred"])) + (1*!is.na(down.shrt_arr_red[,,"pred"]))
short_none_test_index = data.frame(which(short_none_test==2, arr.ind = TRUE))
short_none_test_index$log_q_val = do.log_q_range[short_none_test_index$log_q]
short_none_test_index$temp_val = do.temp_range[short_none_test_index$temp]

short_none_test_index = short_none_test_index[
  which(short_none_test_index$temp_val>=10.25 & short_none_test_index$temp_val<=10.6 &
          short_none_test_index$log_q_val>=10.25 & short_none_test_index$log_q_val<=10.75),]

do_points = data.frame(
  point_name = "do.none.short_overlap",
  mat_x = short_none_test_index$log_q[which(short_none_test_index$log_q_val == quantile(unique(short_none_test_index$log_q_val), probs=0.5,type=1))[1]],
  mat_y = short_none_test_index$temp[which(short_none_test_index$temp_val == quantile(unique(short_none_test_index$temp_val), probs=0.5,type=1))[1]],
  log_q = quantile(unique(short_none_test_index$log_q_val), probs=0.5,type=1),
  temp = quantile(unique(short_none_test_index$temp_val), probs=0.5,type=1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
# None ONLY:
none_test = (1*!is.na(down.none_arr_red[,,"pred"]))
none_test_index = data.frame(which(short_none_test==1, arr.ind = TRUE))
none_test_index$log_q_val = do.log_q_range[none_test_index$log_q]
none_test_index$temp_val = do.temp_range[none_test_index$temp]

none_only_test_index1 = none_test_index[
  which(none_test_index$temp_val>=10.45 & none_test_index$temp_val<=10.55 &
          none_test_index$log_q_val>=9 & none_test_index$log_q_val<=9.25),]
none_only_test_index2 = none_test_index[
  which(none_test_index$temp_val>=17.5 &
          none_test_index$log_q_val>=9 & none_test_index$log_q_val<=9.25),]

do_points = rbind(do_points,
                  data.frame(
                    point_name = "do.none_lo_Q_lo_Temp",
                    mat_x = none_only_test_index2$log_q[which(none_only_test_index2$log_q_val == quantile(unique(none_only_test_index2$log_q_val), probs=0.5,type=1))[1]],
                    mat_y = none_only_test_index2$temp[which(none_only_test_index2$temp_val == quantile(unique(none_only_test_index2$temp_val), probs=0.5,type=1))[1]],
                    log_q = quantile(unique(none_only_test_index2$log_q_val), probs=0.5,type=1),
                    temp = quantile(unique(none_only_test_index2$temp_val), probs=0.5,type=1)))

do_points = rbind(do_points,
                  data.frame(
                    point_name = "do.none_lo_Q_hi_Temp",
                    mat_x = none_only_test_index1$log_q[which(none_only_test_index1$log_q_val == quantile(unique(none_only_test_index1$log_q_val), probs=0.5,type=1))[1]],
                    mat_y = none_only_test_index1$temp[which(none_only_test_index1$temp_val == quantile(unique(none_only_test_index1$temp_val), probs=0.5,type=1))[1]],
                    log_q = quantile(unique(none_only_test_index1$log_q_val), probs=0.5,type=1),
                    temp = quantile(unique(none_only_test_index1$temp_val), probs=0.5,type=1)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
# Short & long overlap:
long_short_test = (1*!is.na(down.shrt_arr_red[,,"pred"])) + (1*!is.na(down.long_arr_red[,,"pred"]))
long_short_test_index = data.frame(which(long_short_test==2, arr.ind = TRUE))
long_short_test_index$log_q_val = do.log_q_range[long_short_test_index$log_q]
long_short_test_index$temp_val = do.temp_range[long_short_test_index$temp]

# Reduce to look at high flow values
long_short_test_index_hi_temp = long_short_test_index[which(
  long_short_test_index$log_q_val>=11.5 & long_short_test_index$temp_val>=10.45 &
    long_short_test_index$temp_val<=10.55),]
do_points = rbind(do_points,
                  data.frame(
                    point_name = "do.short.long_overlap_hi_Q_lo_Temp",
                    mat_x = long_short_test_index_hi_temp$log_q[which(long_short_test_index_hi_temp$log_q_val == quantile(unique(long_short_test_index_hi_temp$log_q_val), probs=0.5,type=1))[1]],
                    mat_y = long_short_test_index_hi_temp$temp[which(long_short_test_index_hi_temp$temp_val == quantile(unique(long_short_test_index_hi_temp$temp_val), probs=0.5,type=1))[1]],
                    log_q = quantile(unique(long_short_test_index_hi_temp$log_q_val), probs=0.5,type=1),
                    temp = quantile(unique(long_short_test_index_hi_temp$temp_val), probs=0.5,type=1)))

# Reduce to look at low temps, moderate flow values
long_short_test_index_mod_Q_mod_Temp = long_short_test_index[which(long_short_test_index$log_q_val>=10.75 & long_short_test_index$log_q_val <=11.25 & long_short_test_index$temp_val>=12.75 & long_short_test_index$temp_val <=15),]

do_points = rbind(do_points,
                  data.frame(
                    point_name = "do.short.long_overlap_mod_Q_mod_Temp",
                    mat_x = long_short_test_index_mod_Q_mod_Temp$log_q[which(long_short_test_index_mod_Q_mod_Temp$log_q_val == quantile(unique(long_short_test_index_mod_Q_mod_Temp$log_q_val), probs=0.5,type=1))[1]],
                    mat_y = long_short_test_index_mod_Q_mod_Temp$temp[which(long_short_test_index_mod_Q_mod_Temp$temp_val == quantile(unique(long_short_test_index_mod_Q_mod_Temp$temp_val), probs=0.5,type=1))[1]],
                    log_q = quantile(unique(long_short_test_index_mod_Q_mod_Temp$log_q_val), probs=0.5,type=1),
                    temp = quantile(unique(long_short_test_index_mod_Q_mod_Temp$temp_val), probs=0.5,type=1)))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
#compile points
point_n = dim(do_points)[1]
point_mat = matrix(data=NA, ncol=9, nrow=point_n,
                   dimnames=list(NULL, col=c("none_pred", "none_lo", "none_up", "shrt_pred", "shrt_lo", "shrt_up", "long_pred", "long_lo", "long_up")))

for(i in 1:point_n){
  point_mat[i,"none_pred"] = down.none_arr_red[do_points$mat_x[i],do_points$mat_y[i],"pred"]
  point_mat[i,"none_lo"] = down.none_arr_red[do_points$mat_x[i],do_points$mat_y[i],"lower"]
  point_mat[i,"none_up"] = down.none_arr_red[do_points$mat_x[i],do_points$mat_y[i],"upper"]

  point_mat[i,"shrt_pred"] = down.shrt_arr_red[do_points$mat_x[i],do_points$mat_y[i],"pred"]
  point_mat[i,"shrt_lo"] = down.shrt_arr_red[do_points$mat_x[i],do_points$mat_y[i],"lower"]
  point_mat[i,"shrt_up"] = down.shrt_arr_red[do_points$mat_x[i],do_points$mat_y[i],"upper"]

  point_mat[i,"long_pred"] = down.long_arr_red[do_points$mat_x[i],do_points$mat_y[i],"pred"]
  point_mat[i,"long_lo"] = down.long_arr_red[do_points$mat_x[i],do_points$mat_y[i],"lower"]
  point_mat[i,"long_up"] = down.long_arr_red[do_points$mat_x[i],do_points$mat_y[i],"upper"]
}

point_dat = data.frame(point_mat)

do_points = cbind(do_points, point_dat)
do_points = do_points[order(do_points$log_q),]


################################################################################
#~~~
#~~~  save the data for Combined_image_plots.R file
#~~~
################################################################################


save(yolo.none_arr_red , yolo.shrt_arr_red, yolo.long_arr_red, yo_points,
     up.none_arr_red, up.shrt_arr_red, up.long_arr_red, up_points,
     down.none_arr_red, down.shrt_arr_red, down.long_arr_red, do_points,
     up.log_q_range, up.temp_range, log_q_range, temp_range, do.log_q_range,
     do.temp_range, file=file.path(wd, "data_clean", "all_GAM_outputs.RData"))
