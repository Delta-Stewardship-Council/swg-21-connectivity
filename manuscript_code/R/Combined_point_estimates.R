################################################################################
#~~ Script created by Jereme W Gaeta, CDFW
#~~ Last modified by Jereme W Gaeta on 04/17/2025
#~~ Description: Point estimate showing uncertainty around model predictions
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


###############################################################################

smooth_diff <- function(model, newdata, r1, r2, alpha = 0.05, unconditional = FALSE, excluding = NA) {
  xp <- predict(model, newdata = newdata, type = 'lpmatrix', exclude = excluding)
  ## difference rows of xp for data from comparison
  X <- xp[r1, ] - xp[r2, ]
  dif <- X %*% coef(model)
  se <- sqrt(rowSums((X %*% vcov(model, unconditional = unconditional)) * X))
  crit <- qt(alpha/2, df.residual(model), lower.tail = TRUE)
  upr <- dif + (crit * se)
  lwr <- dif - (crit * se)
  data.frame(pair = paste(r1, r2, sep = '-'),
             diff = dif,
             se = se,
             upper = upr,
             lower = lwr)
}

###############################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~  Mainstem/Upstream
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###############################################################################


upstream <- alldata %>% filter(region == "upstream")

up_mod = gam(log_chla ~ te(log_qsdy, WTmwk, by = inund_factor) + inund_factor +
               s(station, bs = "re"), method = "REML", data = upstream,
             family = "gaussian")

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

up_points = rbind(up_points,
                  data.frame(
                    point_name = "up.none_lo_Q_lo_Temp",
                    mat_x = none_only_test_index2$log_q[which(none_only_test_index2$log_q_val == quantile(unique(none_only_test_index2$log_q_val), probs=0.5,type=1))[1]],
                    mat_y = none_only_test_index2$temp[which(none_only_test_index2$temp_val == quantile(unique(none_only_test_index2$temp_val), probs=0.5,type=1))[1]],
                    log_q = quantile(unique(none_only_test_index2$log_q_val), probs=0.5,type=1),
                    temp = quantile(unique(none_only_test_index2$temp_val), probs=0.5,type=1)))

up_points = rbind(up_points,
                  data.frame(
                    point_name = "up.none_lo_Q_hi_Temp",
                    mat_x = none_only_test_index1$log_q[which(none_only_test_index1$log_q_val == quantile(unique(none_only_test_index1$log_q_val), probs=0.5,type=1))[1]],
                    mat_y = none_only_test_index1$temp[which(none_only_test_index1$temp_val == quantile(unique(none_only_test_index1$temp_val), probs=0.5,type=1))[1]],
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
# Reduce to look at low temps, moderate flow values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
# Short & long overlap:
long_short_test = (1*!is.na(up.shrt_arr_red[,,"pred"])) + (1*!is.na(up.long_arr_red[,,"pred"]))
long_short_test_index = data.frame(which(long_short_test==2, arr.ind = TRUE))
long_short_test_index$log_q_val = up.log_q_range[long_short_test_index$log_q]
long_short_test_index$temp_val = up.temp_range[long_short_test_index$temp]



long_short_test_index_mod_Q_mod_Temp = long_short_test_index[which(long_short_test_index$log_q_val>=10.75 & long_short_test_index$log_q_val <=11.25 & long_short_test_index$temp_val>=12.75 & long_short_test_index$temp_val <=15),]

up_points = rbind(up_points,
                  data.frame(
                    point_name = "up.short.long_overlap_mod_Q_mod_Temp",
                    mat_x = long_short_test_index_mod_Q_mod_Temp$log_q[which(long_short_test_index_mod_Q_mod_Temp$log_q_val == quantile(unique(long_short_test_index_mod_Q_mod_Temp$log_q_val), probs=0.5,type=1))[1]],
                    mat_y = long_short_test_index_mod_Q_mod_Temp$temp[which(long_short_test_index_mod_Q_mod_Temp$temp_val == quantile(unique(long_short_test_index_mod_Q_mod_Temp$temp_val), probs=0.5,type=1))[1]],
                    log_q = quantile(unique(long_short_test_index_mod_Q_mod_Temp$log_q_val), probs=0.5,type=1),
                    temp = quantile(unique(long_short_test_index_mod_Q_mod_Temp$temp_val), probs=0.5,type=1)))


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


#~~  Change low temp, low flow scenario as per Shruti's request on 8/18/2023
up_points[which(up_points$point_name=="up.none_lo_Q_hi_Temp"),"temp"]=
  up_points[which(up_points$point_name=="up.none.short_overlap"),"temp"]


# do_points_new_data = do_points_new_data[
#   order(do_points_new_data$WTmwk, decreasing = TRUE),]

#############################################################
#~~   Gavin Simpson posterior distribution approach
#############################################################

# up_mod = gam(log_chla ~ te(log_qsdy, WTmwk, by = inund_factor) +
#                  inund_factor + s(station, bs = "re"), method = "REML",
#                data = yolo, family = "gaussian")

up_points_new_data_null = data.frame(
  log_qsdy = up_points$log_q, WTmwk = up_points$temp, inund_factor="none")
up_points_new_data = rbind(up_points_new_data_null,
                           data.frame(
                             log_qsdy = up_points$log_q, WTmwk = up_points$temp, inund_factor="short"))
up_points_new_data = rbind(up_points_new_data,
                           data.frame(
                             log_qsdy = up_points$log_q, WTmwk = up_points$temp, inund_factor="long"))
up_points_new_data$station = "SHR"

#~~   remove non-comparisons  ~~#
up_points_new_data = up_points_new_data[!is.na(unlist(c(up_points$none_pred, up_points$shrt_pred, up_points$long_pred))),]

compare_options = expand.grid(1:dim(up_points_new_data)[1],1:dim(up_points_new_data)[1])
compare_mat = matrix(data = paste0(compare_options[,1],"-",compare_options[,2]),
                     nrow = dim(up_points_new_data)[1],
                     ncol = dim(up_points_new_data)[1],
                     dimnames = list(rows = paste0("lnQ",round(up_points_new_data[,1],1),"_temp",
                                                   round(up_points_new_data[,1],1)),
                                     col = paste0("lnQ",round(up_points_new_data[,1],1),"_temp",
                                                  round(up_points_new_data[,1],1))))
first_obs_index = as.integer(unlist(strsplit(compare_mat[lower.tri(compare_mat)], split = "-"))[c(TRUE,FALSE)])
second_obs_index = as.integer(unlist(strsplit(compare_mat[lower.tri(compare_mat)], split = "-"))[c(FALSE,TRUE)])


up.mult_comp = smooth_diff(model = up_mod, newdata = up_points_new_data,
                           r1 = first_obs_index, r2 = second_obs_index,
                           excluding = "s(station)")

up.mult_comp$sig_dif = sign(up.mult_comp$upper)==sign(up.mult_comp$lower)
#up.mult_comp$

up.mult_comp_vec = up.mult_comp$sig_dif
names(up.mult_comp_vec) = up.mult_comp$pair

up.mult_levels = multcompView::multcompLetters(up.mult_comp_vec)

up.mult_comp_letters_df = data.frame(
  df_order = as.integer(names(up.mult_levels$Letters)),
  letter = up.mult_levels$Letters)
up.mult_comp_letters_df = up.mult_comp_letters_df[order(up.mult_comp_letters_df[,1]),]

up.mult_comp_letters_df = cbind(up_points_new_data,up.mult_comp_letters_df)

up.mult_comp_letters_df_wide=data.frame(pivot_wider(
  data = up.mult_comp_letters_df[,c("log_qsdy","WTmwk","inund_factor","letter")],
  id_cols = c("log_qsdy","WTmwk"), values_from = "letter",
  names_from = "inund_factor"))

up_points = left_join(up_points, up.mult_comp_letters_df_wide,
                      by=c("log_q" = "log_qsdy","temp" = "WTmwk"))
up.plot_multComp_letters = data.frame(
  plot_y=c(unlist(t(up_points[,c("none_up", "shrt_up", "long_up")]))),
  plot_letters = c(unlist(t(up_points[,c("none", "short", "long")])))
)
up.plot_multComp_letters$plot_x = as.integer(row.names(up.plot_multComp_letters))



###############################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~  Yolo
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###############################################################################

yolo <- alldata %>% filter(region == "yolo")

## gamyo6d - Yolo final model
yolo_mod = gam(log_chla ~ te(log_qsdy, WTmwk, by = inund_factor) + inund_factor + s(station, bs = "re"), method = "REML", data = yolo, family = "gaussian")


#############################################################
#~~   Identify convex hull overlap for cherry picking
#############################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
# None & short overlap:
short_none_test = (1*!is.na(yolo.none_arr_red[,,"pred"])) + (1*!is.na(yolo.shrt_arr_red[,,"pred"]))
short_none_test_index = data.frame(which(short_none_test==2, arr.ind = TRUE))
short_none_test_index$log_q_val = log_q_range[short_none_test_index$log_q]
short_none_test_index$temp_val = temp_range[short_none_test_index$temp]

short_none_test_index = short_none_test_index[
  which(short_none_test_index$temp_val>=14.99 & short_none_test_index$temp_val<=15.03 &
          short_none_test_index$log_q_val>=7.97 & short_none_test_index$log_q_val<=8.03),]

yo_points = data.frame(
  point_name = "yo.none.short_overlap",
  mat_x = short_none_test_index$log_q[which(short_none_test_index$log_q_val == quantile(unique(short_none_test_index$log_q_val), probs=0.5,type=1))[1]],
  mat_y = short_none_test_index$temp[which(short_none_test_index$temp_val == quantile(unique(short_none_test_index$temp_val), probs=0.5,type=1))[1]],
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

yo_points = rbind(yo_points,
                  data.frame(
                    point_name = "yo.none_lo_Q_lo_Temp",
                    mat_x = none_only_test_index2$log_q[which(none_only_test_index2$log_q_val == quantile(unique(none_only_test_index2$log_q_val), probs=0.5,type=1))[1]],
                    mat_y = none_only_test_index2$temp[which(none_only_test_index2$temp_val == quantile(unique(none_only_test_index2$temp_val), probs=0.5,type=1))[1]],
                    log_q = quantile(unique(none_only_test_index2$log_q_val), probs=0.5,type=1),
                    temp = quantile(unique(none_only_test_index2$temp_val), probs=0.5,type=1)))

yo_points = rbind(yo_points,
                  data.frame(
                    point_name = "yo.none_lo_Q_hi_Temp",
                    mat_x = none_only_test_index1$log_q[which(none_only_test_index1$log_q_val == quantile(unique(none_only_test_index1$log_q_val), probs=0.5,type=1))[1]],
                    mat_y = none_only_test_index1$temp[which(none_only_test_index1$temp_val == quantile(unique(none_only_test_index1$temp_val), probs=0.5,type=1))[1]],
                    log_q = quantile(unique(none_only_test_index1$log_q_val), probs=0.5,type=1),
                    temp = quantile(unique(none_only_test_index1$temp_val), probs=0.5,type=1)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
# Short & long overlap:
long_short_test = (1*!is.na(yolo.shrt_arr_red[,,"pred"])) + (1*!is.na(yolo.long_arr_red[,,"pred"]))
long_short_test_index = data.frame(which(long_short_test==2, arr.ind = TRUE))
long_short_test_index$log_q_val = log_q_range[long_short_test_index$log_q]
long_short_test_index$temp_val = temp_range[long_short_test_index$temp]

# Reduce to look at high flow values
long_short_test_index_hi_temp = long_short_test_index[which(long_short_test_index$log_q_val>=11.5),]
yo_points = rbind(yo_points,
                  data.frame(
                    point_name = "yo.short.long_overlap_hi_Q_lo_Temp",
                    mat_x = long_short_test_index_hi_temp$log_q[which(long_short_test_index_hi_temp$log_q_val == quantile(unique(long_short_test_index_hi_temp$log_q_val), probs=0.5,type=1))[1]],
                    mat_y = long_short_test_index_hi_temp$temp[which(long_short_test_index_hi_temp$temp_val == quantile(unique(long_short_test_index_hi_temp$temp_val), probs=0.5,type=1))[1]],
                    log_q = quantile(unique(long_short_test_index_hi_temp$log_q_val), probs=0.5,type=1),
                    temp = quantile(unique(long_short_test_index_hi_temp$temp_val), probs=0.5,type=1)))

# Reduce to look at low temps, moderate flow values
long_short_test_index_mod_Q_mod_Temp = long_short_test_index[which(long_short_test_index$log_q_val>=9.75 & long_short_test_index$log_q_val <=10.25 & long_short_test_index$temp_val>=11 & long_short_test_index$temp_val <=12),]

yo_points = rbind(yo_points,
                  data.frame(
                    point_name = "yo.short.long_overlap_mod_Q_mod_Temp",
                    mat_x = long_short_test_index_mod_Q_mod_Temp$log_q[which(long_short_test_index_mod_Q_mod_Temp$log_q_val == quantile(unique(long_short_test_index_mod_Q_mod_Temp$log_q_val), probs=0.5,type=1))[1]],
                    mat_y = long_short_test_index_mod_Q_mod_Temp$temp[which(long_short_test_index_mod_Q_mod_Temp$temp_val == quantile(unique(long_short_test_index_mod_Q_mod_Temp$temp_val), probs=0.5,type=1))[1]],
                    log_q = quantile(unique(long_short_test_index_mod_Q_mod_Temp$log_q_val), probs=0.5,type=1),
                    temp = quantile(unique(long_short_test_index_mod_Q_mod_Temp$temp_val), probs=0.5,type=1)))

# Reduce to look at high temps, moderate flow values
long_short_test_index_mod_Q_hi_Temp = long_short_test_index[which(long_short_test_index$log_q_val>=8 & long_short_test_index$log_q_val <=9.25 & long_short_test_index$temp_val>=14),]

yo_points = rbind(yo_points,
                  data.frame(
                    point_name = "yo.short.long_overlap_mod_Q_hi_Temp",
                    mat_x = long_short_test_index_mod_Q_hi_Temp$log_q[which(long_short_test_index_mod_Q_hi_Temp$log_q_val == quantile(unique(long_short_test_index_mod_Q_hi_Temp$log_q_val), probs=0.5,type=1))[1]],
                    mat_y = long_short_test_index_mod_Q_hi_Temp$temp[which(long_short_test_index_mod_Q_hi_Temp$temp_val == quantile(unique(long_short_test_index_mod_Q_hi_Temp$temp_val), probs=0.5,type=1))[1]],
                    log_q = quantile(unique(long_short_test_index_mod_Q_hi_Temp$log_q_val), probs=0.5,type=1),
                    temp = quantile(unique(long_short_test_index_mod_Q_hi_Temp$temp_val), probs=0.5,type=1)))

# Reduce to look at high temps, highish flow values
long_short_test_index_hi_Q_hi_Temp = long_short_test_index[which(long_short_test_index$log_q_val>=9.98 & long_short_test_index$log_q_val <=10.02 & long_short_test_index$temp_val>=14.98 & long_short_test_index$temp_val<=15.02),]

yo_points = rbind(yo_points,
                  data.frame(
                    point_name = "yo.short.long_overlap_hi_Q_hi_Temp",
                    mat_x = long_short_test_index_hi_Q_hi_Temp$log_q[which(long_short_test_index_hi_Q_hi_Temp$log_q_val == quantile(unique(long_short_test_index_hi_Q_hi_Temp$log_q_val), probs=0.5,type=1))[1]],
                    mat_y = long_short_test_index_hi_Q_hi_Temp$temp[which(long_short_test_index_hi_Q_hi_Temp$temp_val == quantile(unique(long_short_test_index_hi_Q_hi_Temp$temp_val), probs=0.5,type=1))[1]],
                    log_q = quantile(unique(long_short_test_index_hi_Q_hi_Temp$log_q_val), probs=0.5,type=1),
                    temp = quantile(unique(long_short_test_index_hi_Q_hi_Temp$temp_val), probs=0.5,type=1)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
#compile points
point_n = dim(yo_points)[1]
point_mat = matrix(data=NA, ncol=9, nrow=point_n,
                   dimnames=list(NULL, col=c("none_pred", "none_lo", "none_up", "shrt_pred", "shrt_lo", "shrt_up", "long_pred", "long_lo", "long_up")))

for(i in 1:point_n){
  point_mat[i,"none_pred"] = yolo.none_arr_red[yo_points$mat_x[i],yo_points$mat_y[i],"pred"]
  point_mat[i,"none_lo"] = yolo.none_arr_red[yo_points$mat_x[i],yo_points$mat_y[i],"lower"]
  point_mat[i,"none_up"] = yolo.none_arr_red[yo_points$mat_x[i],yo_points$mat_y[i],"upper"]

  point_mat[i,"shrt_pred"] = yolo.shrt_arr_red[yo_points$mat_x[i],yo_points$mat_y[i],"pred"]
  point_mat[i,"shrt_lo"] = yolo.shrt_arr_red[yo_points$mat_x[i],yo_points$mat_y[i],"lower"]
  point_mat[i,"shrt_up"] = yolo.shrt_arr_red[yo_points$mat_x[i],yo_points$mat_y[i],"upper"]

  point_mat[i,"long_pred"] = yolo.long_arr_red[yo_points$mat_x[i],yo_points$mat_y[i],"pred"]
  point_mat[i,"long_lo"] = yolo.long_arr_red[yo_points$mat_x[i],yo_points$mat_y[i],"lower"]
  point_mat[i,"long_up"] = yolo.long_arr_red[yo_points$mat_x[i],yo_points$mat_y[i],"upper"]
}

point_dat = data.frame(point_mat)

yo_points = cbind(yo_points, point_dat)
yo_points = yo_points[order(yo_points$log_q),]

#~~ remove comparisons Shruti did not want as of 8/18/2023 meeting

yo_index_rem = c(
  which(round(yo_points$log_q, digits = 2)==9.98 &
          round(yo_points$temp, digits = 2)==15.01),
  which(round(yo_points$log_q, digits = 2)==10 &
          round(yo_points$temp, digits = 2)==11.51))
yo_points = yo_points[-yo_index_rem,]

#~~  Change low temp, low flow scenario as per Shruti's request on 8/18/2023
yo_points[which(yo_points$point_name=="yo.none_lo_Q_hi_Temp"),"temp"]=
  yo_points[which(yo_points$point_name==
                    "yo.short.long_overlap_mod_Q_hi_Temp"),"temp"]

#############################################################
#~~   Gavin Simpson posterior distribution approach
#~ https://fromthebottomoftheheap.net/2017/10/10/difference-splines-i/
#############################################################

# yolo_mod = gam(log_chla ~ te(log_qsdy, WTmwk, by = inund_factor) +
#                  inund_factor + s(station, bs = "re"), method = "REML",
#                data = yolo, family = "gaussian")

yo_points_new_data_null = data.frame(
  log_qsdy = yo_points$log_q, WTmwk = yo_points$temp, inund_factor="none")
yo_points_new_data = rbind(yo_points_new_data_null,
                           data.frame(
                             log_qsdy = yo_points$log_q, WTmwk = yo_points$temp, inund_factor="short"))
yo_points_new_data = rbind(yo_points_new_data,
                           data.frame(
                             log_qsdy = yo_points$log_q, WTmwk = yo_points$temp, inund_factor="long"))
yo_points_new_data$station = "LIS"


#~~   remove non-comparisons  ~~#
yo_points_new_data = yo_points_new_data[!is.na(unlist(c(yo_points$none_pred, yo_points$shrt_pred, yo_points$long_pred))),]


compare_options = expand.grid(1:dim(yo_points_new_data)[1],1:dim(yo_points_new_data)[1])
compare_mat = matrix(data = paste0(compare_options[,1],"-",compare_options[,2]),
                     nrow = dim(yo_points_new_data)[1],
                     ncol = dim(yo_points_new_data)[1],
                     dimnames = list(rows = paste0("lnQ",round(yo_points_new_data[,1],1),"_temp",
                                                   round(yo_points_new_data[,1],1)),
                                     col = paste0("lnQ",round(yo_points_new_data[,1],1),"_temp",
                                                  round(yo_points_new_data[,1],1))))
first_obs_index = as.integer(unlist(strsplit(compare_mat[lower.tri(compare_mat)], split = "-"))[c(TRUE,FALSE)])
second_obs_index = as.integer(unlist(strsplit(compare_mat[lower.tri(compare_mat)], split = "-"))[c(FALSE,TRUE)])


mult_comp = smooth_diff(model = yolo_mod, newdata = yo_points_new_data,
                        r1 = first_obs_index, r2 = second_obs_index,
                        excluding = "s(station)")

mult_comp$sig_dif = sign(mult_comp$upper)==sign(mult_comp$lower)
#mult_comp$

mult_comp_vec = mult_comp$sig_dif
names(mult_comp_vec) = mult_comp$pair

mult_levels = multcompView::multcompLetters(mult_comp_vec)

mult_comp_letters_df = data.frame(
  df_order = as.integer(names(mult_levels$Letters)),
  letter = mult_levels$Letters)
mult_comp_letters_df = mult_comp_letters_df[order(mult_comp_letters_df[,1]),]

mult_comp_letters_df = cbind(yo_points_new_data,mult_comp_letters_df)

mult_comp_letters_df_wide=data.frame(pivot_wider(
  data = mult_comp_letters_df[,c("log_qsdy","WTmwk","inund_factor","letter")],
  id_cols = c("log_qsdy","WTmwk"), values_from = "letter",
  names_from = "inund_factor"))

yo_points = left_join(yo_points, mult_comp_letters_df_wide,
                      by=c("log_q" = "log_qsdy","temp" = "WTmwk"))
yo_plot_multComp_letters = data.frame(
  plot_y=c(unlist(t(yo_points[,c("none_up", "shrt_up", "long_up")]))),
  plot_letters = c(unlist(t(yo_points[,c("none", "short", "long")])))
)
yo_plot_multComp_letters$plot_x = as.integer(row.names(yo_plot_multComp_letters))




###############################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~  Downstream
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###############################################################################

downstream <- alldata %>% filter(region == "downstream")

down_mod = gam(log_chla ~ te(log_qsdy, WTmwk, by = inund_factor) + inund_factor + s(station, bs = "re"), method = "REML", data = downstream, family = "gaussian")

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

#~~ remove the original 3rd section as per Shruti's request

do_points = do_points[-which(do_points$point_name == "do.none.short_overlap"),]


# down_mod = gam(log_chla ~ te(log_qsdy, WTmwk, by = inund_factor) +
#                  inund_factor + s(station, bs = "re"), method = "REML",
#                data = yolo, family = "gaussian")

do_points_new_data_null = data.frame(
  log_qsdy = do_points$log_q, WTmwk = do_points$temp, inund_factor="none")
do_points_new_data = rbind(do_points_new_data_null,
                           data.frame(
                             log_qsdy = do_points$log_q, WTmwk = do_points$temp, inund_factor="short"))
do_points_new_data = rbind(do_points_new_data,
                           data.frame(
                             log_qsdy = do_points$log_q, WTmwk = do_points$temp, inund_factor="long"))
do_points_new_data$station = "D22"

#~~   remove non-comparisons  ~~#
do_points_new_data = do_points_new_data[!is.na(unlist(c(do_points$none_pred, do_points$shrt_pred, do_points$long_pred))),]


do_points_new_data = do_points_new_data[
  order(do_points_new_data$WTmwk, decreasing = TRUE),]

#############################################################
#~~   Gavin Simpson posterior distribution approach
#############################################################

compare_options = expand.grid(1:dim(do_points_new_data)[1],1:dim(do_points_new_data)[1])
compare_mat = matrix(data = paste0(compare_options[,1],"-",compare_options[,2]),
                     nrow = dim(do_points_new_data)[1],
                     ncol = dim(do_points_new_data)[1],
                     dimnames = list(rows = paste0("lnQ",round(do_points_new_data[,1],1),"_temp",
                                                   round(do_points_new_data[,1],1)),
                                     col = paste0("lnQ",round(do_points_new_data[,1],1),"_temp",
                                                  round(do_points_new_data[,1],1))))
first_obs_index = as.integer(unlist(strsplit(compare_mat[lower.tri(compare_mat)], split = "-"))[c(TRUE,FALSE)])
second_obs_index = as.integer(unlist(strsplit(compare_mat[lower.tri(compare_mat)], split = "-"))[c(FALSE,TRUE)])


do.mult_comp = smooth_diff(model = down_mod, newdata = do_points_new_data,
                           r1 = first_obs_index, r2 = second_obs_index,
                           excluding = "s(station)")

do.mult_comp$sig_dif = sign(do.mult_comp$upper)==sign(do.mult_comp$lower)
#do.mult_comp$

do.mult_comp_vec = do.mult_comp$sig_dif
names(do.mult_comp_vec) = do.mult_comp$pair

do.mult_levels = multcompView::multcompLetters(do.mult_comp_vec)

do.mult_comp_letters_df = data.frame(
  df_order = as.integer(names(do.mult_levels$Letters)),
  letter = do.mult_levels$Letters)
do.mult_comp_letters_df = do.mult_comp_letters_df[order(do.mult_comp_letters_df[,1]),]

do.mult_comp_letters_df = cbind(do_points_new_data,do.mult_comp_letters_df)

do.mult_comp_letters_df_wide=data.frame(pivot_wider(
  data = do.mult_comp_letters_df[,c("log_qsdy","WTmwk","inund_factor","letter")],
  id_cols = c("log_qsdy","WTmwk"), values_from = "letter",
  names_from = "inund_factor"))

do_points = left_join(do_points, do.mult_comp_letters_df_wide,
                      by=c("log_q" = "log_qsdy","temp" = "WTmwk"))
do.plot_multComp_letters = data.frame(
  plot_y=c(unlist(t(do_points[,c("none_up", "shrt_up", "long_up")]))),
  plot_letters = c(unlist(t(do_points[,c("none", "short", "long")])))
)
do.plot_multComp_letters$plot_x = as.integer(row.names(do.plot_multComp_letters))





###############################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~  Plot it!!!
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###############################################################################

 # save(up_points, yo_points, do_points, up.plot_multComp_letters,
 #      yo_plot_multComp_letters, do.plot_multComp_letters,
 #      file = here::here("manuscript_code/data_clean/point_comparisons.RData"))

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


# quartz(height=7*1.33, width=8.5)
# windows(height=7*1.33, width=8.5)
# pdf("revised_NCEAS_pointEst_plots2.pdf", height=7*1.4, width=8.5)
par(mfrow=c(3,1), mar=c(2.5,3,2.75,1), oma=c(11,3.5,4.5,0))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~ Mainstem plot
plot(c(up_y_range[1], up_y_range[2]*1.05)~c(1,12), type="n", las=1, xlim=c(0.25,15.75),
     xaxs="i", xaxt="n", xlab="", ylab="")
mtext(text = "Mainstem", side = 2, line = 5, font=2, cex=1.15)
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
     x =c(3.5,6.5,9.5,12.5, 15.5)-2.85, cex=1, font=2,
     labels = paste0("(a.", 1:5,")"), adj = c(0,0.25))

arrows(x0=c(1:3), x1=c(1:3), y0=unlist(up_points_bc[1,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(up_points_bc[1,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(1:3), y=unlist(up_points_bc[1,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple4", "green", "darkred"), cex=1.5)

arrows(x0=c(4:6), x1=c(4:6), y0=unlist(up_points_bc[2,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(up_points_bc[2,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(4:6), y=unlist(up_points_bc[2,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple4", "green", "darkred"), cex=1.5)

arrows(x0=c(7:9), x1=c(7:9), y0=unlist(up_points_bc[3,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(up_points_bc[3,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(7:9), y=unlist(up_points_bc[3,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple4", "green", "darkred"), cex=1.5)

arrows(x0=c(10:12), x1=c(10:12), y0=unlist(up_points_bc[4,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(up_points_bc[4,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(10:12), y=unlist(up_points_bc[4,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple4", "green", "darkred"), cex=1.5)

arrows(x0=c(13:15), x1=c(13:15), y0=unlist(up_points_bc[5,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(up_points_bc[5,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(13:15), y=unlist(up_points_bc[5,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple4", "green", "darkred"), cex=1.5)

for(i in 1:7){
  text(x=c(2,5,8,11,14)[i], y=up_y_range[1]-diff(up_y_range)*0.1, adj=c(0.5,1),
       labels=bquote(log[e](Flow~(cfs))*"="*.(op(round(up_points_bc[i,"log_q"],digits=2),d = 2))),
       xpd=NA)
  text(x=c(2,5,8,11,14)[i], y=up_y_range[1]-diff(up_y_range)*0.2, adj=c(0.5,1),
       labels=bquote(Water~Temp.*"="*.(op(round(up_points_bc[i,"temp"],digits=2)),d=2)),
       xpd=NA)
}
box(which="plot")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~ Yolo plot
plot(c(yo_y_range[1], yo_y_range[2]*1.05)~c(1,12), type="n", las=1, xlim=c(0.5,15.5),
     xaxs="i", xaxt="n", xlab="", ylab="")
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
     x =c(3.5,6.5,9.5,12.5, 15.5)-2.85, cex=1, font=2,
     labels = paste0("(b.", 1:5,")"), adj = c(0,0.25))

arrows(x0=c(1:3), x1=c(1:3), y0=unlist(yo_points_bc[1,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(yo_points_bc[1,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(1:3), y=unlist(yo_points_bc[1,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple4", "green", "darkred"), cex=1.5)

arrows(x0=c(4:6), x1=c(4:6), y0=unlist(yo_points_bc[2,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(yo_points_bc[2,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(4:6), y=unlist(yo_points_bc[2,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple4", "green", "darkred"), cex=1.5)

arrows(x0=c(7:9), x1=c(7:9), y0=unlist(yo_points_bc[3,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(yo_points_bc[3,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(7:9), y=unlist(yo_points_bc[3,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple4", "green", "darkred"), cex=1.5)

arrows(x0=c(10:12), x1=c(10:12), y0=unlist(yo_points_bc[4,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(yo_points_bc[4,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(10:12), y=unlist(yo_points_bc[4,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple4", "green", "darkred"), cex=1.5)

arrows(x0=c(13:15), x1=c(13:15), y0=unlist(yo_points_bc[5,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(yo_points_bc[5,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(13:15), y=unlist(yo_points_bc[5,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple4", "green", "darkred"), cex=1.5)

for(i in 1:5){
  text(x=c(2,5,8,11,14)[i], y=yo_y_range[1]-diff(yo_y_range)*0.1, adj=c(0.5,1),
       labels=bquote(log[e](Flow~(cfs))*"="*.(op(round(yo_points_bc[i,"log_q"],digits=2),d = 2))),
       xpd=NA)
  text(x=c(2,5,8,11,14)[i], y=yo_y_range[1]-diff(yo_y_range)*0.2, adj=c(0.5,1),
       labels=bquote(Water~Temp.*"="*.(op(round(yo_points_bc[i,"temp"],digits=2)),d=2)),
       xpd=NA)
}
box(which="plot")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~ Downstream plot
plot(c(do_y_range[1], do_y_range[2]*1.05)~c(1,12), type="n", las=1, xlim=c(0.25,12.5),
     xaxs="i", xaxt="n", xlab="", ylab="")
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
     x =c(3.5,6.5,9.5,12.5)-0.15, cex=1, font=2,
     labels = paste0("(c.", 1:4,")"), adj = c(1,0.25))

arrows(x0=c(1:3), x1=c(1:3), y0=unlist(do_points_bc[1,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(do_points_bc[1,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(1:3), y=unlist(do_points_bc[1,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple4", "green", "darkred"), cex=1.5)

arrows(x0=c(4:6), x1=c(4:6), y0=unlist(do_points_bc[2,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(do_points_bc[2,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(4:6), y=unlist(do_points_bc[2,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple4", "green", "darkred"), cex=1.5)

arrows(x0=c(7:9), x1=c(7:9), y0=unlist(do_points_bc[3,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(do_points_bc[3,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(7:9), y=unlist(do_points_bc[3,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple4", "green", "darkred"), cex=1.5)

arrows(x0=c(10:12), x1=c(10:12), y0=unlist(do_points_bc[4,c("none_lo", "shrt_lo", "long_lo")]),
       y1=unlist(do_points_bc[4,c("none_up", "shrt_up", "long_up")]), code=3, angle = 90, length = 0.1)
points(x=c(10:12), y=unlist(do_points_bc[4,c("none_pred", "shrt_pred", "long_pred")]),
       pch=21, bg=c("purple4", "green", "darkred"), cex=1.5)

for(i in 1:7){
  text(x=c(2,5,8,11)[i], y=do_y_range[1]-diff(do_y_range)*0.1, adj=c(0.5,1),
       labels=bquote(log[e](Flow~(cfs))*"="*.(op(round(do_points_bc[i,"log_q"],digits=2),d = 2))),
       xpd=NA)
  text(x=c(2,5,8,11)[i], y=do_y_range[1]-diff(do_y_range)*0.2, adj=c(0.5,1),
       labels=bquote(Water~Temp.*"="*.(op(round(do_points_bc[i,"temp"],digits=2)),d=2)),
       xpd=NA)
}
box(which="plot")

legend("bottomleft", legend = c("No Inundation", "Short Inundation", "Long Inundation"),
        pch=21, pt.bg=c("purple4", "green", "darkred"), inset=0.045, pt.cex=1.5,
       bg = "white")

dev.off()
