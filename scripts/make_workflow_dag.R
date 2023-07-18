# make dag of repo workflow

library(nomnoml)
# #E0EEEE blue
# ##EEC591 darkorange
## Make the text:
nom1 <-
"
#stroke: #53868B
#.raw: fill=#FFEBCD visual=roundrect
#.dir: fill=#E0EEEE visual=roundrect bold
#.fnc: fill=#E0EEEE visual=roundrect bold
#.enviro: fill=#B4EEB4 visual=database
#direction: down
[<frame> Connectivity Workflow |

[Raw Data \n (from repository or local)] --> [<raw> 1) discretewq package
2) Yolo bypass data (YBFMP):
https://doi.org/10.6073/pasta/5791d7eaca09fb9471c5589c66f86863]

[Raw Data \n (from repository or local)] --> [<fnc> 1) f_get_flow_riovista.R
2) f_get_flow_verona.R]

[<fnc> 1) f_get_flow_riovista.R
2) f_get_flow_verona.R] :-> [<raw>1) raw_flow_usgs_11455420.csv
2) raw_flow_usgs_11425500.csv]

[<raw>1) raw_flow_usgs_11455420.csv
2) raw_flow_usgs_11425500.csv] --> [<fnc> 1) f_clean_flow_riovista.R
2) f_clean_flow_verona.R]

[<fnc> 1) f_clean_flow_riovista.R
2) f_clean_flow_verona.R] --> [<raw> 1) clean_flow_usgs_11455420.csv
2) clean_flow_usgs_11425500.csv]

[1) discretewq package
2) Yolo bypass data (YBFMP):
https://doi.org/10.6073/pasta/5791d7eaca09fb9471c5589c66f86863] --> [<fnc>1) f_clean_discretewq.R
2) f_clean_ybfmp.R]

[<raw>1) f_clean_discretewq.R
2) f_clean_ybfmp.R] --> [<raw>1) clean_discretewq.csv
2) clean_ybfmp.csv]

[<raw>1) clean_discretewq.csv
2) clean_ybfmp.csv] --> [<fnc> f_load_chla.R]

[<fnc> f_load_chla.R] --> [<raw> model_chla.csv (n=26)]

[Raw Data \n (from repository or local)] --> [<raw>1) inundation package (inundation, yolo dayflow)
2) water temperature data: https://doi.org/10.6073/pasta/5d84e5b8ea74dd0854d4aba1e4a6122d]

[<raw>1) inundation package (inundation, yolo dayflow)
2) water temperature data: https://doi.org/10.6073/pasta/5d84e5b8ea74dd0854d4aba1e4a6122d] --> [<fnc> f_clean_stations.R]

[<fnc> f_clean_stations.R] --> [<raw> stations.csv]

[<raw>1) clean_flow_usgs_11455420.csv
2) clean_flow_usgs_11425500.csv] --> [<raw> f_load functions.R]


]
"

nomnoml(nom1) # view in Viewer
nomnoml(nom1, png = "figures/repo_workflow_dag.png", width = 4, height = 6)


#[<raw> f_load functions] -:> [<enviro>Loads Clean Datasets \n to environment]

#[<enviro>Loads Clean Datasets \n to environment (one or all)] -:> [Data Integration]

#[Data Integration] --> [<raw>f_make_model functions]

#[<raw> f_make_model functions] :-> [<enviro>Model Inputs]
