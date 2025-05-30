# make dag of repo workflow

library(nomnoml)
library(htmlwidgets)

## Make the text:
nom1 <-
  "
#stroke: #404040
#fontSize: 16
#font: Roboto Condensed
#gutter: 2
#padding: 12
#leading: 1.1
#spacing: 30
#.raw: fill=#D9D9D9 visual=roundrect bold
#.mod: fill=#3A5FCD visual=roundrect stroke=#FFFFFF bold
#.out: fill=#B9D3EE fontSize=12 bold
#.fnc: fill=#EEC591 visual=roundrect italic
#.enviro: fill=#FFEBCD visual=database bold
#direction: down
[<frame> CONNECTIVITY MANUSCRIPT WORKFLOW |

[<enviro> Raw Data] --> [<fnc> 1) f_get_flow_riovista.R
2) f_get_flow_verona.R]

[<fnc> 1) f_get_flow_riovista.R
2) f_get_flow_verona.R] :-> [<raw>1) raw_flow_usgs_11455420.csv
2) raw_flow_usgs_11425500.csv]

[<raw>1) raw_flow_usgs_11455420.csv
2) raw_flow_usgs_11425500.csv] --> [<fnc> 1) f_clean_flow_riovista.R
2) f_clean_flow_verona.R]

[<fnc> 1) f_clean_flow_riovista.R
2) f_clean_flow_verona.R] :-> [<raw> 1) clean_flow_usgs_11455420.csv
2) clean_flow_usgs_11425500.csv]

[<raw>1) clean_flow_usgs_11455420.csv
2) clean_flow_usgs_11425500.csv] --> [<fnc> f_integrate_model_data.R]

[<enviro>Raw Data] --> [<raw>1) discretewq package
2) Yolo bypass data (YBFMP)
doi.org/10.6073]

[1) discretewq package
2) Yolo bypass data (YBFMP):
doi.org/10.6073] --> [<fnc>1) f_clean_discretewq.R
2) f_clean_ybfmp.R]

[<raw>1) f_clean_discretewq.R
2) f_clean_ybfmp.R] :-> [<raw>1) clean_discretewq.csv
2) clean_ybfmp.csv]

[<raw>1) clean_discretewq.csv
2) clean_ybfmp.csv] --> [<fnc> f_load_chla.R]

[<raw> model_chla.csv] --> [<fnc> f_integrate_model_data.R]

[<fnc> f_load_chla.R] :-> [<raw> model_chla.csv]

[<fnc>f_integrate_model_data.R] :-> [<raw>model_chla_covars.csv]

[<raw>model_chla_covars.csv] --> [<fnc>run_gams.R]

[<fnc>run_gams.R] :-> [<out> data_gam_results.Rdata]

[<out> data_gam_results.Rdata] --> [<fnc>all_GAM_outputs.R]
[<fnc>all_GAM_outputs.R]:->[<out> all_GAM_outputs.Rdata]

[<out>all_GAM_outputs.Rdata]-->[<fnc>Combined_point_estimates.R]
[<out>data_gam_results.Rdata]-->[<fnc>Combined_point_estimates.R]

[<fnc>Combined_point_estimates.R]-->[<out>point_comparisons.Rdata]

[<out> data_gam_results.Rdata] --> [<fnc>predication_plot.R]


[<enviro>Raw Data] --> [<raw>1) Inundation package (inundation, dayflow)
2) water temperature data:
doi.org/10.6073]

[<raw>1) Inundation package (inundation, dayflow)
2) water temperature data:
doi.org/10.6073] --> [<fnc> f_clean_stations.R]

[<raw>1) Inundation package (inundation, dayflow)
2) water temperature data:
doi.org/10.6073] --> [<fnc>f_integrate_model_data.R]

[<fnc> f_clean_stations.R] :-> [<raw> stations.csv]



[<fnc> f_clean_stations.R] :-> [<raw> model_chla_covars.csv]

[<out>data_gam_results.Rdata] --> [<mod> plot_model_validation_re.R|  - Model Validation Plots (FigS5-S7)]
[<out>data_gam_results.Rdata] --> [<mod> plot_raw_data.R|  - Boxplots Raw Data (Fig2) \n - Correlation Matrix (FigS2) \n - Raw Data Histograms (FigS3)]
[<raw>stations.csv] --> [<mod> plot_locations_map.R| - Station Map (Fig1)]
[<raw> model_chla.csv] --> [<mod>2016_example.R| 2016 Inundation Plot (Fig6)]

[<raw>1) Inundation package (inundation, dayflow)
2) water temperature data:
doi.org/10.6073] --> [<mod>2016_example.R| 2016 Inundation Plot (Fig6)]

[<raw>1) Inundation package (inundation, dayflow)
2) water temperature data:
doi.org/10.6073] --> [<mod>inund_factor_plot.R| Inundation Events (FigS1)]

[<out>data_gam_results.Rdata] --> [<mod> predication_plot.R| Predicted Chlorophyll by Flow (Fig5)]
[<out>all_GAM_outputs.Rdata] --> [<mod> Combined_image_plots.R \n Combined_dataOnly_plots.R| - Predicted rasters (Fig3) \n - Predicted point estimates (Fig4) \n - Convex Hull points (FigS4)]
[<out>point_comparisons.Rdata] --> [<mod> Combined_image_plots.R \n Combined_dataOnly_plots.R| - Predicted rasters (Fig3) \n - Predicted point estimates (Fig4) \n - Convex Hull points (FigS4)]
[<out>data_gam_results.Rdata] --> [<mod> Combined_image_plots.R \n Combined_dataOnly_plots.R| - Predicted rasters (Fig3) \n - Predicted point estimates (Fig4) \n - Convex Hull points (FigS4)]

]
"

nomnoml(nom1) # view in Viewer

# save out
# The nomnoml png export wasn't working for me so see workaround below.
# nomnoml::nomnoml(nom1, png = "manuscript_code/figures/repo_workflow_dag_v3.png", width = 550, height = 450)

w <- nomnoml::nomnoml(nom1)
saveWidget(w, "nom_temp.html", selfcontained = TRUE)

webshot2::webshot("nom_temp.html",
                 file = "manuscript_code/figures/workflow_dag_v3.png",
                 vwidth = 650, vheight = 800, zoom = 3)
