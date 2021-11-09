# make dag of repo workflow

library(nomnoml)

## Make the text:
nom1 <-
"
#stroke: #53868B
#.raw: fill=#FFEBCD visual=ellipse
#.dir: fill=#E0EEEE visual=roundrect bold
#.clean: fill=#EEC591 visual=ellipse
#.enviro: fill=#B4EEB4 visual=database
#direction: down
[<frame> Connectivity Workflow |
  [Raw Data Files \n (from repository or local)] --> [<raw> f_get functions]
  [<raw> f_get functions] :-> [<dir>data_raw]
  [<dir>data_raw] --> [<raw> f_clean functions]
  [Repository (e.g., EDI)] --> [<raw> f_clean functions]
  [<raw>f_clean functions] :-> [<dir>data_clean]
  [<dir>data_clean] --> [<raw>f_load functions]
  [<raw> f_load functions] -:> [<enviro>Loads Clean Datasets \n to environment]
  [<enviro>Loads Clean Datasets \n to environment (one or all)] -:> [Data Integration]
  [Data Integration] --> [<raw>f_make_model functions]
  [<raw> f_make_model functions] :-> [<enviro>Model Inputs]
]
"

nomnoml(nom1) # view in Viewer
nomnoml(nom1, png = "figures/repo_workflow_dag.png", width = 4, height = 6)
