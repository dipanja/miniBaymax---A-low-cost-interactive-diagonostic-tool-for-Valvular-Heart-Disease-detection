# miniBaymax--A-low-cost-interactive-diagonostic-tool-for-Valvular-Heart-Disease-detection

## This interactive web application is motivated by Baymax of the animated movie Big Hero 6. It detects Valvular Heart disorder i.e. damage or defect in one of the four heart valves. On the Machine Learning side, I have used AutoML from the deep learning platform H2O. And the interactive application is build in RShiny.

### There are three main files in this application:
+ __h2o_model.R__ consists of the pretrained model. I have used the AutoML function from h2o to find the best model. The best model is then saved and is used in the RShiny application.
+ The other two files are __ui.R__ and __server.R__ used in the RShiny application.

### The other important files are
+ The initial data used for model building is in __Z_Alizadeh_Sani.csv__
+ __modeling_data.csv__ is the dataset obtained after feature subset selection (using Boruta).
