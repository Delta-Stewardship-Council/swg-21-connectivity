# make dag of repo workflow

library(nomnoml)
#E0EEEE blue
#3A5FCD steelblue3
#FFEBCD light orange
#EEC591 darkorange
#D9D9D9 gray85
#2F4F4F darkslategrey

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
[<frame> CONNECTIVITY WORKFLOW |

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
<doi.org/10.6073> ]

[1) discretewq package
2) Yolo bypass data (YBFMP):
<doi.org/10.6073>] --> [<fnc>1) f_clean_discretewq.R
2) f_clean_ybfmp.R]

[<raw>1) f_clean_discretewq.R
2) f_clean_ybfmp.R] :-> [<raw>1) clean_discretewq.csv
2) clean_ybfmp.csv]

[<raw>1) clean_discretewq.csv
2) clean_ybfmp.csv] --> [<fnc> f_load_chla.R]

[<raw> model_chla.csv (n=26)] --> [<fnc> f_integrate_model_data.R]

[<fnc> f_load_chla.R] :-> [<raw> model_chla.csv (n=26)]

[<fnc>f_integrate_model_data.R] :-> [<raw>model_chla_covars.csv (n=12)]

[<enviro>Raw Data] --> [<raw>1) Inundation package (inundation, dayflow)
2) water temperature data:
<doi.org/10.6073>]

[<raw>1) Inundation package (inundation, dayflow)
2) water temperature data:
<doi.org/10.6073>] --> [<fnc> f_clean_stations.R]

[<raw>1) Inundation package (inundation, dayflow)
2) water temperature data:
<doi.org/10.6073>] --> [<fnc>f_integrate_model_data.R]

[<fnc> f_clean_stations.R] :-> [<raw> stations.csv]

[<fnc> f_clean_stations.R] :-> [<raw> model_chla_covars.csv (n=12)]

[<raw> model_chla_covars.csv (n=12)] --> [<mod> run_gams.R| - gam outputs \n - Vis_code.R]
[<raw> model_chla_covars.csv (n=12)] --> [<mod> plot_model_validation_re.R|  - Model Validation Plots]
[<raw> model_chla_covars.csv (n=12)] --> [<mod> plot_raw_data.R|  - Boxplots]
[<mod> run_gams.R] :-> [<out>data_gam_results.Rdata]
]
"

nomnoml(nom1) # view in Viewer

# save out
nomnoml(nom1, png = "figures/repo_workflow_dag_v2.png", width = 1200, height = 1600)
