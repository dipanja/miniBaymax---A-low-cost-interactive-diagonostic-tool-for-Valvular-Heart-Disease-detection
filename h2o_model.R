closeAllConnections()
rm(list =ls())
cat("\f")

master_data <- read.csv("Z_Alizadeh_Sani.csv",
                        header = T,
                        sep = ",",
                        na.string = c("NA",
                                      "n/a",
                                      ""),
                        stringsAsFactors = T)

master_data <- master_data[complete.cases(master_data),]

dim(master_data)

print(require(dplyr))

#Dropping Cath variable
master_data <- master_data %>% select(-c(Cath))

#Change certain variable names
originals <- c("Length",
               "BMI",
               "DM",
               "HTN",
               "FH",
               "CRF",
               "CVA",
               "CHF",
               "DLP",
               "BP",
               "PR",
               "Nonanginal",
               "Exertional.CP",
               "LowTH.Ang",
               "LVH",
               "Poor.R.Progression",
               "FBS",
               "CR",
               "TG",
               "LDL",
               "HDL",
               "BUN",
               "ESR",
               "HB",
               "K",
               "Na",
               "WBC",
               "Lymph",
               "Neut",
               "PLT",
               "Region.RWMA",
               "VHD")

change <- c("Height",
            "Body.mass.index",
            "Diabetes.mellitus",
            "Hypertension",
            "Family.history",
            "Chronic.renal.failure",
            "Cerebrovascular.accident",
            "Congestive.heart.failure",
            "Dyslipidemia",
            "Blood.pressure",
            "Pulse.rate",
            "Nonanginal.chest.pain",
            "Exertional.chest.pain",
            "Low.threshold.angina",
            "Left.ventricular.hypertrophy",
            "Poor.R.Wave.progession",
            "Fasting.blood.sugar",
            "Creatine",
            "Triglyceride",
            "Low.density.lipoprotein",
            "High.density.lipoprotein",
            "Blood.urea.nitrogen",
            "Erythrocyte.sedimentation.rate",
            "Hemoglobin",
            "Potassium",
            "Sodium",
            "White.blood.cell",
            "Lymphocyte",
            "Neutrophil",
            "Platelet",
            "Regional.wall.motion.abnormality",
            "Valvular.heart.disease")

print(require(data.table))

setnames(master_data,
         originals,
         change)

cols.factor <- c("Diabetes.mellitus",
                 "Hypertension",
                 "Current.Smoker",
                 "EX.Smoker",
                 "Family.history",
                 "Edema",
                 "Typical.Chest.Pain",
                 "Function.Class",
                 "Q.Wave",
                 "St.Elevation",
                 "St.Depression",
                 "Tinversion")

master_data[cols.factor] <- lapply(master_data[cols.factor],
                                   as.factor)

master_data$Creatine <- as.numeric(as.character(master_data$Creatine))

master_data$Valvular.heart.disease<- ifelse(master_data$Valvular.heart.disease == "Severe" |
                                              master_data$Valvular.heart.disease == "Moderate",
                                            "CONCERN",
                                            "NO CONCERN")

master_data$Valvular.heart.disease <- as.factor(master_data$Valvular.heart.disease)

print(require(Boruta))

set.seed(1234)

boruta.train <- Boruta(Valvular.heart.disease ~.,
                       data = master_data,
                       doTrace = 2,
                       maxRuns = 500)

final.boruta <- TentativeRoughFix(boruta.train)

final.selected.features <- getSelectedAttributes(final.boruta)


imp.xf = NULL

for (i in 1:length(final.selected.features)){
  
  imp.xf[i] = paste0(final.selected.features[i])
  
}

imp.f <- c(imp.xf,
           "Valvular.heart.disease")

imp.xy <- subset(master_data,
                 select = imp.f)

modeling_data <- data.frame(imp.xy)

modeling_data$Lung.rales <- as.factor(ifelse(modeling_data$Lung.rales == "N",
                                             "No",
                                             "Yes"))

modeling_data$Systolic.Murmur <- as.factor(ifelse(modeling_data$Systolic.Murmur == "N",
                                                  "No",
                                                  "Yes"))

modeling_data$Diastolic.Murmur <- as.factor(ifelse(modeling_data$Diastolic.Murmur == "N",
                                                   "No",
                                                   "Yes"))

modeling_data$Dyspnea <- as.factor(ifelse(modeling_data$Dyspnea == "N",
                                          "No",
                                          "Yes"))

write.table(modeling_data,
            file = "modeling_data.csv",
            row.names = F,
            col.names = T,
            sep = ",",
            append = F)

#==========================================================
str(modeling_data)

print(require(h2o))
h2o.init(nthreads = -1,
         min_mem_size = "8g",
         max_mem_size = "10g")

final.df <- as.h2o(modeling_data)

split_h2o <- h2o.splitFrame(final.df,
                            c(0.7,
                              0.1),
                            seed = 1234 )


train_h2o <- h2o.assign(split_h2o[[1]],
                        "train" )
h2o.dim(train_h2o)

valid_h2o <- h2o.assign(split_h2o[[2]],
                        "valid" )
h2o.dim(valid_h2o)

test_h2o  <- h2o.assign(split_h2o[[3]],
                        "test" )
h2o.dim(test_h2o)

target <- "Valvular.heart.disease"

predictors <- setdiff(names(train_h2o),
                      target)

predictors

start <- print(Sys.time())

automl_h2o_models <- h2o.automl(x = predictors,
                                y = target,
                                training_frame    = train_h2o,
                                leaderboard_frame = valid_h2o,
                                balance_classes = T,
                                max_runtime_secs = 60*3,
                                project_name = "six",
                                max_after_balance_size = 10,
                                sort_metric = "AUC",
                                stopping_metric = "AUC")
end.time <- Sys.time()

time.taken <- print(end.time - start)



leadership_board <- automl_h2o_models@leaderboard

print(leadership_board)

automl_leader <- automl_h2o_models@leader

automl_leader

saved.model <- h2o.saveModel(object = automl_leader,
                             path = getwd(),
                             force = T)

fetched.model <- h2o.loadModel(saved.model)

h2o.varimp_plot(automl_leader)


prediction_on_test <- h2o.predict(object = fetched.model,
                                  newdata = test_h2o)

model_performance_on_test <- h2o.performance(automl_leader,
                                             test_h2o)

print(model_performance_on_test)

new.data <- data.frame(Family.history = as.factor(1),
                       Pulse.rate = as.numeric(100),
                       Lung.rales = as.factor("No"),
                       Systolic.Murmur = as.factor("Yes"),
                       Diastolic.Murmur = as.factor("Yes"),
                       Dyspnea = as.factor("Yes"),
                       Function.Class = as.factor(3),
                       EF.TTE = as.numeric(55))

new.data <- as.h2o(new.data)

new.pred <- h2o.predict(object = fetched.model,
                        newdata = new.data)

new.pred

print(require(lime))

new.data2 <- data.frame(Family.history = as.factor(1),
                        Pulse.rate = as.numeric(100),
                        Lung.rales = as.factor("No"),
                        Systolic.Murmur = as.factor("Yes"),
                        Diastolic.Murmur = as.factor("Yes"),
                        Dyspnea = as.factor("Yes"),
                        Function.Class = as.factor(3),
                        EF.TTE = as.numeric(55))

explainer <- lime::lime(x = as.data.frame(train_h2o),
                        model = fetched.model)

explanations <- lime::explain(x = as.data.frame(new.data2),
                              explainer = explainer,
                              n_permutations = 500,
                              feature_select = "auto",
                              n_labels = 1,
                              n_features = 3)

lime::plot_features(explanations, ncol = 2)

h2o.shutdown()
