# WHO: LifeExpectancy regression: WHO2
bdanalytics  

**  **    
**Date: (Thu) Jun 04, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/WHO.csv  
    New:        <newdt_url>  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

### ![](<filename>.png)

## Potential next steps include:
- Organization:
    - Categorize by chunk
    - Priority criteria:
        0. Ease of change
        1. Impacts report
        2. Cleans innards
        3. Bug report
        
- all chunks:
    - at chunk-end rm(!glb_<var>)
    
- manage.missing.data chunk:
    - cleaner way to manage re-splitting of training vs. new entity

- extract.features chunk:
    - Add n-grams for glb_txt_vars
        - "RTextTools", "tau", "RWeka", and "textcat" packages
    - Convert user-specified mutate code to config specs
    
- fit.models chunk:
    - Prediction accuracy scatter graph:
    -   Add tiles (raw vs. PCA)
    -   Use shiny for drop-down of "important" features
    -   Use plot.ly for interactive plots ?
    
    - Change .fit suffix of model metrics to .mdl if it's data independent (e.g. AIC, Adj.R.Squared - is it truly data independent ?, etc.)
    - move model_type parameter to myfit_mdl before indep_vars_vctr (keep all model_* together)
    - create a custom model for rpart that has minbucket as a tuning parameter
    - varImp for randomForest crashes in caret version:6.0.41 -> submit bug report

- Probability handling for multinomials vs. desired binomial outcome
-   ROCR currently supports only evaluation of binary classification tasks (version 1.0.7)
-   extensions toward multiclass classification are scheduled for the next release

- Skip trControl.method="cv" for dummy classifier ?
- Add custom model to caret for a dummy (baseline) classifier (binomial & multinomial) that generates proba/outcomes which mimics the freq distribution of glb_rsp_var values; Right now glb_dmy_glm_mdl always generates most frequent outcome in training data
- glm_dmy_mdl should use the same method as glm_sel_mdl until custom dummy classifer is implemented

- fit.all.training chunk:
    - myplot_prediction_classification: displays 'x' instead of '+' when there are no prediction errors 
- Compare glb_sel_mdl vs. glb_fin_mdl:
    - varImp
    - Prediction differences (shd be minimal ?)

- Move glb_analytics_diag_plots to mydsutils.R: (+) Easier to debug (-) Too many glb vars used
- Add print(ggplot.petrinet(glb_analytics_pn) + coord_flip()) at the end of every major chunk
- Parameterize glb_analytics_pn
- Move glb_impute_missing_data to mydsutils.R: (-) Too many glb vars used; glb_<>_df reassigned
- Replicate myfit_mdl_classification features in myfit_mdl_regression
- Do non-glm methods handle interaction terms ?
- f-score computation for classifiers should be summation across outcomes (not just the desired one ?)
- Add accuracy computation to glb_dmy_mdl in predict.data.new chunk
- Why does splitting fit.data.training.all chunk into separate chunks add an overhead of ~30 secs ? It's not rbind b/c other chunks have lower elapsed time. Is it the number of plots ?
- Incorporate code chunks in print_sessionInfo
- Test against 
    - projects in github.com/bdanalytics
    - lectures in jhu-datascience track

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/myscript.R")
source("~/Dropbox/datascience/R/mydsutils.R")
```

```
## Loading required package: caret
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
source("~/Dropbox/datascience/R/myplclust.R")
# Gather all package requirements here
suppressPackageStartupMessages(require(doMC))
registerDoMC(4) # max(length(glb_txt_vars), glb_n_cv_folds) + 1
#packageVersion("snow")

#require(sos); findFn("pinv", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_trnng_url <- "https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/WHO.csv"
glb_newdt_url <- "<newdt_url>"
glb_out_pfx <- "WHO2_"
glb_save_envir <- FALSE # or TRUE

glb_is_separate_newent_dataset <- FALSE    # or TRUE
glb_split_entity_newent_datasets <- TRUE   # or FALSE
glb_split_newdata_method <- "sample"          # "condition" or "sample" or "copy"
glb_split_newdata_condition <- "<col_name> <condition_operator> <value>"    # or NULL
glb_split_newdata_size_ratio <- 0.3               # > 0 & < 1
glb_split_sample.seed <- 123               # or any integer
glb_drop_vars <- c(NULL) # or c("<col_name>")

glb_max_fitent_obs <- NULL # or any integer                         
glb_is_regression <- TRUE; glb_is_classification <- FALSE; glb_is_binomial <- TRUE

glb_rsp_var_raw <- "LifeExpectancy"

# for classification, the response variable has to be a factor
glb_rsp_var <- glb_rsp_var_raw # or "LifeExpectancy.fctr"

# if the response factor is based on numbers e.g (0/1 vs. "A"/"B"), 
#   caret predict(..., type="prob") crashes
glb_map_rsp_raw_to_var <- NULL # or function(raw) {
    #relevel(factor(ifelse(raw == 1, "Y", "N")), as.factor(c("Y", "N")), ref="N")
    #as.factor(paste0("B", raw))
    #as.factor(raw)    
#}
#glb_map_rsp_raw_to_var(c(1, 1, 0, 0, 0))

glb_map_rsp_var_to_raw <- NULL # or function(var) {
    #as.numeric(var) - 1
    #as.numeric(var)
    #levels(var)[as.numeric(var)]
    #c(" <=50K", " >50K")[as.numeric(var)]
#}
#glb_map_rsp_var_to_raw(glb_map_rsp_raw_to_var(c(1, 1, 0, 0, 0)))

if ((glb_rsp_var != glb_rsp_var_raw) & is.null(glb_map_rsp_raw_to_var))
    stop("glb_map_rsp_raw_to_var function expected")
glb_rsp_var_out <- paste0(glb_rsp_var, ".predict.") # model_id is appended later

# List info gathered for various columns
# <col_name>:   <description>; <notes>

glb_id_vars <- c("Country")
glb_category_vars <- NULL # or c("<var1>", "<var2>")
glb_date_vars <- NULL # or c("<date_var>")

glb_txt_vars <- NULL # or c("<txt_var1>", "<txt_var2>")   
#Sys.setlocale("LC_ALL", "C") # For english

glb_append_stop_words <- list()
# Remember to use unstemmed words
#orderBy(~ -cor.y.abs, subset(glb_feats_df, grepl("[HSA]\\.T\\.", id) & !is.na(cor.high.X)))
#dsp_obs(Headline.contains="polit")
#subset(glb_allobs_df, H.T.compani > 0)[, c("UniqueID", "Headline", "H.T.compani")]
# glb_append_stop_words[["<txt_var1>"]] <- c(NULL
# #                             ,"<word1>" # <reason1>
#                             )
#subset(glb_allobs_df, S.T.newyorktim > 0)[, c("UniqueID", "Snippet", "S.T.newyorktim")]
#glb_txt_lst[["Snippet"]][which(glb_allobs_df$UniqueID %in% c(8394, 8317, 8339, 8350, 8307))]

glb_important_terms <- list()
# Remember to use stemmed terms

glb_sprs_thresholds <- NULL # or c(0.988, 0.970, 0.970) # Generates 29, 22, 22 terms
# Properties:
#   numrows(glb_feats_df) << numrows(glb_fitobs_df)
#   Select terms that appear in at least 0.2 * O(FP/FN(glb_OOBobs_df))
#       numrows(glb_OOBobs_df) = 1.1 * numrows(glb_newobs_df)
names(glb_sprs_thresholds) <- glb_txt_vars

glb_log_vars <- NULL # or c("<numeric_var1>", "<numeric_var2>")

# List transformed vars  
glb_exclude_vars_as_features <- c(NULL) # or c("<var_name>") 
if (glb_rsp_var_raw != glb_rsp_var)
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                            glb_rsp_var_raw)
if (!is.null(glb_txt_vars))
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                          glb_txt_vars)

# List feats that shd be excluded due to known causation by prediction variable
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c(NULL)) # or c("<col_name>")

glb_interaction_only_features <- NULL # or ???

glb_force_0_to_NA_vars <- NULL # or c("<numeric_var1>", "<numeric_var2>")
glb_impute_na_data <- TRUE # or FALSE
glb_mice_complete.seed <- 144 # or any integer

glb_cluster <- FALSE # or TRUE

glb_models_lst <- list(); glb_models_df <- data.frame()
# Regression
if (glb_is_regression)
    glb_models_method_vctr <- c("lm", "glm", "rpart", "rf") else
# Classification
    if (glb_is_binomial)
        glb_models_method_vctr <- c("glm", "bayesglm", "rpart", "rf") else  
        glb_models_method_vctr <- c("rpart", "rf")

# Baseline prediction model feature(s)
glb_Baseline_mdl_var <- NULL # or c("<col_name>")

glb_model_metric_terms <- NULL # or matrix(c(
#                               0,1,2,3,4,
#                               2,0,1,2,3,
#                               4,2,0,1,2,
#                               6,4,2,0,1,
#                               8,6,4,2,0
#                           ), byrow=TRUE, nrow=5)
glb_model_metric <- NULL # or "<metric_name>"
glb_model_metric_maximize <- NULL # or FALSE (TRUE is not the default for both classification & regression) 
glb_model_metric_smmry <- NULL # or function(data, lev=NULL, model=NULL) {
#     confusion_mtrx <- t(as.matrix(confusionMatrix(data$pred, data$obs)))
#     #print(confusion_mtrx)
#     #print(confusion_mtrx * glb_model_metric_terms)
#     metric <- sum(confusion_mtrx * glb_model_metric_terms) / nrow(data)
#     names(metric) <- glb_model_metric
#     return(metric)
# }

glb_tune_models_df <- 
   rbind(
    #data.frame(parameter="cp", min=0.00005, max=0.00005, by=0.000005),
                            #seq(from=0.01,  to=0.01, by=0.01)
    #data.frame(parameter="mtry",  min=080, max=100, by=10),
    #data.frame(parameter="mtry",  min=08, max=10, by=1),    
    data.frame(parameter="dummy", min=2, max=4, by=1)
        ) 
# or NULL
glb_n_cv_folds <- 3 # or NULL

glb_clf_proba_threshold <- NULL # 0.5

# Model selection criteria
if (glb_is_regression)
    glb_model_evl_criteria <- c("min.RMSE.OOB", "max.R.sq.OOB", "max.Adj.R.sq.fit")
if (glb_is_classification) {
    if (glb_is_binomial)
        glb_model_evl_criteria <- 
            c("max.Accuracy.OOB", "max.auc.OOB", "max.Kappa.OOB", "min.aic.fit") else
        glb_model_evl_criteria <- c("max.Accuracy.OOB", "max.Kappa.OOB")
}

glb_sel_mdl_id <- NULL # or "<model_id_prefix>.<model_method>"
glb_fin_mdl_id <- glb_sel_mdl_id # or "Final"

# Depict process
glb_analytics_pn <- petrinet(name="glb_analytics_pn",
                        trans_df=data.frame(id=1:6,
    name=c("data.training.all","data.new",
           "model.selected","model.final",
           "data.training.all.prediction","data.new.prediction"),
    x=c(   -5,-5,-15,-25,-25,-35),
    y=c(   -5, 5,  0,  0, -5,  5)
                        ),
                        places_df=data.frame(id=1:4,
    name=c("bgn","fit.data.training.all","predict.data.new","end"),
    x=c(   -0,   -20,                    -30,               -40),
    y=c(    0,     0,                      0,                 0),
    M0=c(   3,     0,                      0,                 0)
                        ),
                        arcs_df=data.frame(
    begin=c("bgn","bgn","bgn",        
            "data.training.all","model.selected","fit.data.training.all",
            "fit.data.training.all","model.final",    
            "data.new","predict.data.new",
            "data.training.all.prediction","data.new.prediction"),
    end  =c("data.training.all","data.new","model.selected",
            "fit.data.training.all","fit.data.training.all","model.final",
            "data.training.all.prediction","predict.data.new",
            "predict.data.new","data.new.prediction",
            "end","end")
                        ))
#print(ggplot.petrinet(glb_analytics_pn))
print(ggplot.petrinet(glb_analytics_pn) + coord_flip())
```

```
## Loading required package: grid
```

![](WHO2_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor   bgn end elapsed
## 1 import.data          1          0 7.716  NA      NA
```

## Step `1.0: import data`
#### chunk option: eval=<r condition>

```r
glb_trnobs_df <- myimport_data(url=glb_trnng_url, comment="glb_trnobs_df", 
                                force_header=TRUE)
```

```
## [1] "Reading file ./data/WHO.csv..."
## [1] "dimensions of data in ./data/WHO.csv: 194 rows x 13 cols"
##               Country                Region Population Under15 Over60
## 1         Afghanistan Eastern Mediterranean      29825   47.42   3.82
## 2             Albania                Europe       3162   21.33  14.93
## 3             Algeria                Africa      38482   27.42   7.17
## 4             Andorra                Europe         78   15.20  22.86
## 5              Angola                Africa      20821   47.58   3.84
## 6 Antigua and Barbuda              Americas         89   25.96  12.35
##   FertilityRate LifeExpectancy ChildMortality CellularSubscribers
## 1          5.40             60           98.5               54.26
## 2          1.75             74           16.7               96.39
## 3          2.83             73           20.0               98.99
## 4            NA             82            3.2               75.49
## 5          6.10             51          163.5               48.38
## 6          2.12             75            9.9              196.41
##   LiteracyRate   GNI PrimarySchoolEnrollmentMale
## 1           NA  1140                          NA
## 2           NA  8820                          NA
## 3           NA  8310                        98.2
## 4           NA    NA                        78.4
## 5         70.1  5230                        93.1
## 6         99.0 17900                        91.1
##   PrimarySchoolEnrollmentFemale
## 1                            NA
## 2                            NA
## 3                          96.4
## 4                          79.4
## 5                          78.2
## 6                          84.5
##                 Country          Region Population Under15 Over60
## 7             Argentina        Americas      41087   24.42  14.97
## 29             Cambodia Western Pacific      14865   31.23   7.67
## 99            Lithuania          Europe       3028   15.13  20.57
## 140 Republic of Moldova          Europe       3514   16.52  16.72
## 141             Romania          Europe      21755   15.05  20.66
## 191            Viet Nam Western Pacific      90796   22.87   9.32
##     FertilityRate LifeExpectancy ChildMortality CellularSubscribers
## 7            2.20             76           14.2              134.92
## 29           2.93             65           39.7               96.17
## 99           1.49             74            5.4              151.30
## 140          1.47             71           17.6              104.80
## 141          1.39             74           12.2              109.16
## 191          1.79             75           23.0              143.39
##     LiteracyRate   GNI PrimarySchoolEnrollmentMale
## 7           97.8 17130                          NA
## 29            NA  2230                        96.4
## 99          99.7 19640                        95.6
## 140         98.5  3640                        90.1
## 141         97.7 15120                        87.9
## 191         93.2  3250                          NA
##     PrimarySchoolEnrollmentFemale
## 7                              NA
## 29                           95.4
## 99                           95.8
## 140                          90.1
## 141                          87.3
## 191                            NA
##                                Country                Region Population
## 189                            Vanuatu       Western Pacific        247
## 190 Venezuela (Bolivarian Republic of)              Americas      29955
## 191                           Viet Nam       Western Pacific      90796
## 192                              Yemen Eastern Mediterranean      23852
## 193                             Zambia                Africa      14075
## 194                           Zimbabwe                Africa      13724
##     Under15 Over60 FertilityRate LifeExpectancy ChildMortality
## 189   37.37   6.02          3.46             72           17.9
## 190   28.84   9.17          2.44             75           15.3
## 191   22.87   9.32          1.79             75           23.0
## 192   40.72   4.54          4.35             64           60.0
## 193   46.73   3.95          5.77             55           88.5
## 194   40.24   5.68          3.64             54           89.8
##     CellularSubscribers LiteracyRate   GNI PrimarySchoolEnrollmentMale
## 189               55.76         82.6  4330                          NA
## 190               97.78           NA 12430                        94.7
## 191              143.39         93.2  3250                          NA
## 192               47.05         63.9  2170                        85.5
## 193               60.59         71.2  1490                        91.4
## 194               72.13         92.2    NA                          NA
##     PrimarySchoolEnrollmentFemale
## 189                            NA
## 190                          95.1
## 191                            NA
## 192                          70.5
## 193                          93.9
## 194                            NA
## 'data.frame':	194 obs. of  13 variables:
##  $ Country                      : chr  "Afghanistan" "Albania" "Algeria" "Andorra" ...
##  $ Region                       : chr  "Eastern Mediterranean" "Europe" "Africa" "Europe" ...
##  $ Population                   : int  29825 3162 38482 78 20821 89 41087 2969 23050 8464 ...
##  $ Under15                      : num  47.4 21.3 27.4 15.2 47.6 ...
##  $ Over60                       : num  3.82 14.93 7.17 22.86 3.84 ...
##  $ FertilityRate                : num  5.4 1.75 2.83 NA 6.1 2.12 2.2 1.74 1.89 1.44 ...
##  $ LifeExpectancy               : int  60 74 73 82 51 75 76 71 82 81 ...
##  $ ChildMortality               : num  98.5 16.7 20 3.2 163.5 ...
##  $ CellularSubscribers          : num  54.3 96.4 99 75.5 48.4 ...
##  $ LiteracyRate                 : num  NA NA NA NA 70.1 99 97.8 99.6 NA NA ...
##  $ GNI                          : num  1140 8820 8310 NA 5230 ...
##  $ PrimarySchoolEnrollmentMale  : num  NA NA 98.2 78.4 93.1 91.1 NA NA 96.9 NA ...
##  $ PrimarySchoolEnrollmentFemale: num  NA NA 96.4 79.4 78.2 84.5 NA NA 97.5 NA ...
##  - attr(*, "comment")= chr "glb_trnobs_df"
## NULL
```

```r
if (glb_is_separate_newent_dataset) {
    glb_newobs_df <- myimport_data(url=glb_newdt_url, comment="glb_newobs_df", 
                                   force_header=TRUE)
    
    # To make plots / stats / checks easier in chunk:inspectORexplore.data
    glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df); 
    comment(glb_allobs_df) <- "glb_allobs_df"
} else {
    glb_allobs_df <- glb_trnobs_df; comment(glb_allobs_df) <- "glb_allobs_df"
    if (!glb_split_entity_newent_datasets) {
        stop("Not implemented yet") 
        glb_newobs_df <- glb_trnobs_df[sample(1:nrow(glb_trnobs_df),
                                          max(2, nrow(glb_trnobs_df) / 1000)),]                    
    } else      if (glb_split_newdata_method == "condition") {
            glb_newobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=glb_split_newdata_condition)))
            glb_trnobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=paste0("!(", 
                                                      glb_split_newdata_condition,
                                                      ")"))))
        } else if (glb_split_newdata_method == "sample") {
                require(caTools)
                
                set.seed(glb_split_sample.seed)
                split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
                                      SplitRatio=(1-glb_split_newdata_size_ratio))
                glb_newobs_df <- glb_trnobs_df[!split, ] 
                glb_trnobs_df <- glb_trnobs_df[split ,]
        } else if (glb_split_newdata_method == "copy") {  
            glb_trnobs_df <- glb_allobs_df
            comment(glb_trnobs_df) <- "glb_trnobs_df"
            glb_newobs_df <- glb_allobs_df
            comment(glb_newobs_df) <- "glb_newobs_df"
        } else stop("glb_split_newdata_method should be %in% c('condition', 'sample', 'copy')")   

    comment(glb_newobs_df) <- "glb_newobs_df"
    myprint_df(glb_newobs_df)
    str(glb_newobs_df)

    if (glb_split_entity_newent_datasets) {
        myprint_df(glb_trnobs_df)
        str(glb_trnobs_df)        
    }
}         
```

```
## Loading required package: caTools
```

```
##     Country          Region Population Under15 Over60 FertilityRate
## 3   Algeria          Africa      38482   27.42   7.17          2.83
## 4   Andorra          Europe         78   15.20  22.86            NA
## 5    Angola          Africa      20821   47.58   3.84          6.10
## 7 Argentina        Americas      41087   24.42  14.97          2.20
## 8   Armenia          Europe       2969   20.34  14.06          1.74
## 9 Australia Western Pacific      23050   18.95  19.46          1.89
##   LifeExpectancy ChildMortality CellularSubscribers LiteracyRate   GNI
## 3             73           20.0               98.99           NA  8310
## 4             82            3.2               75.49           NA    NA
## 5             51          163.5               48.38         70.1  5230
## 7             76           14.2              134.92         97.8 17130
## 8             71           16.4              103.57         99.6  6100
## 9             82            4.9              108.34           NA 38110
##   PrimarySchoolEnrollmentMale PrimarySchoolEnrollmentFemale
## 3                        98.2                          96.4
## 4                        78.4                          79.4
## 5                        93.1                          78.2
## 7                          NA                            NA
## 8                          NA                            NA
## 9                        96.9                          97.5
##                         Country                Region Population Under15
## 16                      Belarus                Europe       9405   15.10
## 68                       Greece                Europe      11125   14.60
## 81                         Iraq Eastern Mediterranean      32778   40.51
## 86                        Japan       Western Pacific     127000   13.12
## 126                        Niue       Western Pacific          1   30.61
## 185 United Republic of Tanzania                Africa      47783   44.85
##     Over60 FertilityRate LifeExpectancy ChildMortality CellularSubscribers
## 16   19.31          1.47             71            5.2              111.88
## 68   25.41          1.51             81            4.8              106.48
## 81    4.95          4.15             69           34.4               78.12
## 86   31.92          1.39             83            3.0              104.95
## 126   9.07            NA             72           25.1                  NA
## 185   4.89          5.36             59           54.0               55.53
##     LiteracyRate   GNI PrimarySchoolEnrollmentMale
## 16            NA 14460                          NA
## 68          97.2 25100                        98.8
## 81          78.2  3750                          NA
## 86            NA 35330                          NA
## 126           NA    NA                          NA
## 185         73.2  1500                          NA
##     PrimarySchoolEnrollmentFemale
## 16                             NA
## 68                           99.3
## 81                             NA
## 86                             NA
## 126                            NA
## 185                            NA
##                         Country                Region Population Under15
## 171                    Thailand       South-East Asia      66785   18.47
## 177                     Tunisia Eastern Mediterranean      10875   23.22
## 185 United Republic of Tanzania                Africa      47783   44.85
## 191                    Viet Nam       Western Pacific      90796   22.87
## 192                       Yemen Eastern Mediterranean      23852   40.72
## 193                      Zambia                Africa      14075   46.73
##     Over60 FertilityRate LifeExpectancy ChildMortality CellularSubscribers
## 171  13.96          1.43             74           13.2              111.63
## 177  10.49          2.04             76           16.1              116.93
## 185   4.89          5.36             59           54.0               55.53
## 191   9.32          1.79             75           23.0              143.39
## 192   4.54          4.35             64           60.0               47.05
## 193   3.95          5.77             55           88.5               60.59
##     LiteracyRate  GNI PrimarySchoolEnrollmentMale
## 171           NA 8360                          NA
## 177           NA 9030                          NA
## 185         73.2 1500                          NA
## 191         93.2 3250                          NA
## 192         63.9 2170                        85.5
## 193         71.2 1490                        91.4
##     PrimarySchoolEnrollmentFemale
## 171                            NA
## 177                            NA
## 185                            NA
## 191                            NA
## 192                          70.5
## 193                          93.9
## 'data.frame':	55 obs. of  13 variables:
##  $ Country                      : chr  "Algeria" "Andorra" "Angola" "Argentina" ...
##  $ Region                       : chr  "Africa" "Europe" "Africa" "Americas" ...
##  $ Population                   : int  38482 78 20821 41087 2969 23050 1318 9405 2004 199000 ...
##  $ Under15                      : num  27.4 15.2 47.6 24.4 20.3 ...
##  $ Over60                       : num  7.17 22.86 3.84 14.97 14.06 ...
##  $ FertilityRate                : num  2.83 NA 6.1 2.2 1.74 1.89 2.12 1.47 2.71 1.82 ...
##  $ LifeExpectancy               : int  73 82 51 76 71 82 79 71 66 74 ...
##  $ ChildMortality               : num  20 3.2 163.5 14.2 16.4 ...
##  $ CellularSubscribers          : num  99 75.5 48.4 134.9 103.6 ...
##  $ LiteracyRate                 : num  NA NA 70.1 97.8 99.6 NA 91.9 NA 84.5 NA ...
##  $ GNI                          : num  8310 NA 5230 17130 6100 ...
##  $ PrimarySchoolEnrollmentMale  : num  98.2 78.4 93.1 NA NA 96.9 NA NA NA NA ...
##  $ PrimarySchoolEnrollmentFemale: num  96.4 79.4 78.2 NA NA 97.5 NA NA NA NA ...
##  - attr(*, "comment")= chr "glb_newobs_df"
##                Country                Region Population Under15 Over60
## 1          Afghanistan Eastern Mediterranean      29825   47.42   3.82
## 2              Albania                Europe       3162   21.33  14.93
## 6  Antigua and Barbuda              Americas         89   25.96  12.35
## 10             Austria                Europe       8464   14.51  23.52
## 11          Azerbaijan                Europe       9309   22.25   8.24
## 12             Bahamas              Americas        372   21.62  11.24
##    FertilityRate LifeExpectancy ChildMortality CellularSubscribers
## 1           5.40             60           98.5               54.26
## 2           1.75             74           16.7               96.39
## 6           2.12             75            9.9              196.41
## 10          1.44             81            4.0              154.78
## 11          1.96             71           35.2              108.75
## 12          1.90             75           16.9               86.06
##    LiteracyRate   GNI PrimarySchoolEnrollmentMale
## 1            NA  1140                          NA
## 2            NA  8820                          NA
## 6            99 17900                        91.1
## 10           NA 42050                          NA
## 11           NA  8960                        85.3
## 12           NA    NA                          NA
##    PrimarySchoolEnrollmentFemale
## 1                             NA
## 2                             NA
## 6                           84.5
## 10                            NA
## 11                          84.1
## 12                            NA
##                              Country                Region Population
## 48  Democratic Republic of the Congo                Africa      65705
## 80        Iran (Islamic Republic of) Eastern Mediterranean      76424
## 103                         Malaysia       Western Pacific      29240
## 116                       Mozambique                Africa      25203
## 165                         Suriname              Americas        535
## 184                   United Kingdom                Europe      62783
##     Under15 Over60 FertilityRate LifeExpectancy ChildMortality
## 48    45.11   4.51          6.15             49          145.7
## 80    23.68   7.82          1.91             73           17.6
## 103   26.65   8.21          1.99             74            8.5
## 116   45.38   5.01          5.34             53           89.7
## 165   27.83   9.55          2.32             72           20.8
## 184   17.54  23.06          1.90             80            4.8
##     CellularSubscribers LiteracyRate   GNI PrimarySchoolEnrollmentMale
## 48                23.09         66.8   340                          NA
## 80                74.93           NA    NA                          NA
## 103              127.04         93.1 15650                          NA
## 116               32.83         56.1   970                        94.6
## 165              178.88         94.7    NA                          NA
## 184              130.75           NA 36010                        99.8
##     PrimarySchoolEnrollmentFemale
## 48                             NA
## 80                             NA
## 103                            NA
## 116                          89.4
## 165                            NA
## 184                          99.6
##                                Country          Region Population Under15
## 186           United States of America        Americas     318000   19.63
## 187                            Uruguay        Americas       3395   22.05
## 188                         Uzbekistan          Europe      28541   28.90
## 189                            Vanuatu Western Pacific        247   37.37
## 190 Venezuela (Bolivarian Republic of)        Americas      29955   28.84
## 194                           Zimbabwe          Africa      13724   40.24
##     Over60 FertilityRate LifeExpectancy ChildMortality CellularSubscribers
## 186  19.31          2.00             79            7.1               92.72
## 187  18.59          2.07             77            7.2              140.75
## 188   6.38          2.38             68           39.6               91.65
## 189   6.02          3.46             72           17.9               55.76
## 190   9.17          2.44             75           15.3               97.78
## 194   5.68          3.64             54           89.8               72.13
##     LiteracyRate   GNI PrimarySchoolEnrollmentMale
## 186           NA 48820                        95.4
## 187         98.1 14640                          NA
## 188         99.4  3420                        93.3
## 189         82.6  4330                          NA
## 190           NA 12430                        94.7
## 194         92.2    NA                          NA
##     PrimarySchoolEnrollmentFemale
## 186                          96.1
## 187                            NA
## 188                          91.0
## 189                            NA
## 190                          95.1
## 194                            NA
## 'data.frame':	139 obs. of  13 variables:
##  $ Country                      : chr  "Afghanistan" "Albania" "Antigua and Barbuda" "Austria" ...
##  $ Region                       : chr  "Eastern Mediterranean" "Europe" "Americas" "Europe" ...
##  $ Population                   : int  29825 3162 89 8464 9309 372 155000 283 11060 324 ...
##  $ Under15                      : num  47.4 21.3 26 14.5 22.2 ...
##  $ Over60                       : num  3.82 14.93 12.35 23.52 8.24 ...
##  $ FertilityRate                : num  5.4 1.75 2.12 1.44 1.96 1.9 2.24 1.84 1.85 2.76 ...
##  $ LifeExpectancy               : int  60 74 75 81 71 75 70 78 80 74 ...
##  $ ChildMortality               : num  98.5 16.7 9.9 4 35.2 16.9 40.9 18.4 4.2 18.3 ...
##  $ CellularSubscribers          : num  54.3 96.4 196.4 154.8 108.8 ...
##  $ LiteracyRate                 : num  NA NA 99 NA NA NA 56.8 NA NA NA ...
##  $ GNI                          : num  1140 8820 17900 42050 8960 ...
##  $ PrimarySchoolEnrollmentMale  : num  NA NA 91.1 NA 85.3 NA NA NA 98.9 NA ...
##  $ PrimarySchoolEnrollmentFemale: num  NA NA 84.5 NA 84.1 NA NA NA 99.2 NA ...
##  - attr(*, "comment")= chr "glb_trnobs_df"
```

```r
if (nrow(glb_trnobs_df) == nrow(glb_allobs_df))
    warning("glb_trnobs_df same as glb_allobs_df")
if (nrow(glb_newobs_df) == nrow(glb_allobs_df))
    warning("glb_newobs_df same as glb_allobs_df")

if (length(glb_drop_vars) > 0) {
    warning("dropping vars: ", paste0(glb_drop_vars, collapse=", "))
    glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), glb_drop_vars)]
    glb_trnobs_df <- glb_trnobs_df[, setdiff(names(glb_trnobs_df), glb_drop_vars)]    
    glb_newobs_df <- glb_newobs_df[, setdiff(names(glb_newobs_df), glb_drop_vars)]    
}

# Check for duplicates in glb_id_vars
if (length(glb_id_vars) == 0) {
    warning("using .rownames as identifiers for observations")
    glb_allobs_df$.rownames <- rownames(glb_allobs_df)
    glb_id_vars <- ".rownames"
}
if (sum(duplicated(glb_allobs_df[, glb_id_vars, FALSE])) > 0)
    stop(glb_id_vars, " duplicated in glb_allobs_df")
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_id_vars)

# Combine trnent & newent into glb_allobs_df for easier manipulation
glb_trnobs_df$.src <- "Train"; glb_newobs_df$.src <- "Test"; 
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, ".src")
glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df)
comment(glb_allobs_df) <- "glb_allobs_df"
glb_trnobs_df <- glb_newobs_df <- NULL

glb_chunks_df <- myadd_chunk(glb_chunks_df, "inspect.data", major.inc=TRUE)
```

```
##          label step_major step_minor   bgn   end elapsed
## 1  import.data          1          0 7.716 8.259   0.544
## 2 inspect.data          2          0 8.260    NA      NA
```

## Step `2.0: inspect data`

```r
#print(str(glb_allobs_df))
#View(glb_allobs_df)

dsp_class_dstrb <- function(var) {
    xtab_df <- mycreate_xtab_df(glb_allobs_df, c(".src", var))
    rownames(xtab_df) <- xtab_df$.src
    xtab_df <- subset(xtab_df, select=-.src)
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

dsp_problem_data <- function(df, terminate=FALSE) {
    print(sprintf("numeric data missing in %s: ", 
                  ifelse(!is.null(df_name <- comment(df)), df_name, "")))
    numeric_missing <- sapply(setdiff(names(df), myfind_chr_cols_df(df)), 
                                function(col) sum(is.na(df[, col])))
    numeric_missing <- numeric_missing[numeric_missing > 0]
    print(numeric_missing)
    numeric_feats_missing <- setdiff(names(numeric_missing), glb_exclude_vars_as_features)
    if ((length(numeric_feats_missing) > 0) && terminate)
        stop("terminating due to missing values")
    
    print(sprintf("numeric data w/ 0s in %s: ", 
                  ifelse(!is.null(df_name <- comment(df)), df_name, "")))
    print(sapply(setdiff(names(df), myfind_chr_cols_df(df)), 
                 function(col) sum(df[, col] == 0, na.rm=TRUE)))
    
    print(sprintf("numeric data w/ Infs in %s: ", 
                  ifelse(!is.null(df_name <- comment(df)), df_name, "")))
    print(sapply(setdiff(names(df), myfind_chr_cols_df(df)), 
                 function(col) sum(df[, col] == Inf, na.rm=TRUE)))
    
    print(sprintf("numeric data w/ NaNs in %s: ", 
                  ifelse(!is.null(df_name <- comment(df)), df_name, "")))
    print(sapply(setdiff(names(df), myfind_chr_cols_df(df)), 
                 function(col) sum(df[, col] == NaN, na.rm=TRUE)))
    
    print(sprintf("string data missing in %s: ", 
                  ifelse(!is.null(df_name <- comment(df)), df_name, "")))
    print(sapply(setdiff(myfind_chr_cols_df(df), ".src"), 
                        function(col) sum(df[, col] == "")))
}

# Performed repeatedly in other chunks
glb_chk_data <- function() {
    # Histogram of predictor in glb_trnobs_df & glb_newobs_df
    print(myplot_histogram(glb_allobs_df, glb_rsp_var_raw) + facet_wrap(~ .src))
    
    if (glb_is_classification) 
        dsp_class_dstrb(var=ifelse(glb_rsp_var %in% names(glb_allobs_df), 
                                   glb_rsp_var, glb_rsp_var_raw))
    dsp_problem_data(glb_allobs_df)
}
glb_chk_data()
```

```
## Warning in loop_apply(n, do.ply): position_stack requires constant width:
## output may be incorrect
```

```
## Warning in loop_apply(n, do.ply): position_stack requires constant width:
## output may be incorrect
```

![](WHO2_files/figure-html/inspect.data-1.png) 

```
## [1] "numeric data missing in glb_allobs_df: "
##                 FertilityRate           CellularSubscribers 
##                            11                            10 
##                  LiteracyRate                           GNI 
##                            91                            32 
##   PrimarySchoolEnrollmentMale PrimarySchoolEnrollmentFemale 
##                            93                            93 
## [1] "numeric data w/ 0s in glb_allobs_df: "
##                    Population                       Under15 
##                             0                             0 
##                        Over60                 FertilityRate 
##                             0                             0 
##                LifeExpectancy                ChildMortality 
##                             0                             0 
##           CellularSubscribers                  LiteracyRate 
##                             0                             0 
##                           GNI   PrimarySchoolEnrollmentMale 
##                             0                             0 
## PrimarySchoolEnrollmentFemale 
##                             0 
## [1] "numeric data w/ Infs in glb_allobs_df: "
##                    Population                       Under15 
##                             0                             0 
##                        Over60                 FertilityRate 
##                             0                             0 
##                LifeExpectancy                ChildMortality 
##                             0                             0 
##           CellularSubscribers                  LiteracyRate 
##                             0                             0 
##                           GNI   PrimarySchoolEnrollmentMale 
##                             0                             0 
## PrimarySchoolEnrollmentFemale 
##                             0 
## [1] "numeric data w/ NaNs in glb_allobs_df: "
##                    Population                       Under15 
##                             0                             0 
##                        Over60                 FertilityRate 
##                             0                             0 
##                LifeExpectancy                ChildMortality 
##                             0                             0 
##           CellularSubscribers                  LiteracyRate 
##                             0                             0 
##                           GNI   PrimarySchoolEnrollmentMale 
##                             0                             0 
## PrimarySchoolEnrollmentFemale 
##                             0 
## [1] "string data missing in glb_allobs_df: "
## Country  Region 
##       0       0
```

```r
# Create new features that help diagnostics
if (!is.null(glb_map_rsp_raw_to_var)) {
    glb_allobs_df[, glb_rsp_var] <- 
        glb_map_rsp_raw_to_var(glb_allobs_df[, glb_rsp_var_raw])
    mycheck_map_results(mapd_df=glb_allobs_df, 
                        from_col_name=glb_rsp_var_raw, to_col_name=glb_rsp_var)
        
    if (glb_is_classification) dsp_class_dstrb(glb_rsp_var)
}

#   Convert dates to numbers 
#       typically, dates come in as chars; 
#           so this must be done before converting chars to factors

myextract_dates_df <- function(df, vars, rsp_var) {
    keep_feats <- c(NULL)
    for (var in vars) {
        dates_df <- data.frame(.date=strptime(df[, var], "%Y-%m-%d %H:%M:%S"))
        dates_df[, rsp_var] <- df[, rsp_var]
        dates_df[, paste0(var, ".POSIX")] <- dates_df$.date
        dates_df[, paste0(var, ".year")] <- as.numeric(format(dates_df$.date, "%Y"))
        dates_df[, paste0(var, ".year.fctr")] <- as.factor(format(dates_df$.date, "%Y")) 
        dates_df[, paste0(var, ".month")] <- as.numeric(format(dates_df$.date, "%m"))
        dates_df[, paste0(var, ".month.fctr")] <- as.factor(format(dates_df$.date, "%m"))
        dates_df[, paste0(var, ".date")] <- as.numeric(format(dates_df$.date, "%d"))
        dates_df[, paste0(var, ".date.fctr")] <- 
            cut(as.numeric(format(dates_df$.date, "%d")), 5) # by month week  
        dates_df[, paste0(var, ".juliandate")] <- as.numeric(format(dates_df$.date, "%j"))        
        
        # wkday Sun=0; Mon=1; ...; Sat=6
        dates_df[, paste0(var, ".wkday")] <- as.numeric(format(dates_df$.date, "%w"))
        dates_df[, paste0(var, ".wkday.fctr")] <- as.factor(format(dates_df$.date, "%w"))
        
        # Federal holidays 1.9., 13.10.,         27.11., 25.12.
        # NYState holidays 1.9., 13.10., 11.11., 27.11., 25.12.
        months <- dates_df[, paste0(var, ".month")]
        dates  <- dates_df[, paste0(var, ".date")]
        dates_df[, paste0(var, ".hlday")] <- 
            ifelse( ((months == 09) & (dates  == 01)) |
                    ((months == 10) & (dates  == 13)) |  
                    ((months == 11) & (dates  == 27)) |         
                    ((months == 12) & (dates  == 25)) ,                                 
                    1, 0)
        dates_df[, paste0(var, ".wkend")] <- as.numeric(
            (dates_df[, paste0(var, ".wkday")] %in% c(0, 6)) | 
            dates_df[, paste0(var, ".hlday")] )
        dates_df[, paste0(var, ".hour")] <- as.numeric(format(dates_df$.date, "%H"))
        dates_df[, paste0(var, ".hour.fctr")] <- 
            cut(as.numeric(format(dates_df$.date, "%H")), 3) # by work-shift    
        dates_df[, paste0(var, ".minute")] <- as.numeric(format(dates_df$.date, "%M")) 
        dates_df[, paste0(var, ".minute.fctr")] <- 
            cut(as.numeric(format(dates_df$.date, "%M")), 4) # by quarter-hours    
        dates_df[, paste0(var, ".second")] <- as.numeric(format(dates_df$.date, "%S")) 
        dates_df[, paste0(var, ".second.fctr")] <- 
            cut(as.numeric(format(dates_df$.date, "%S")), 4) # by quarter-hours    
        dates_df[, paste0(var, ".day.minutes")] <- 60 * dates_df[, paste0(var, ".hour")] + 
                                                    dates_df[, paste0(var, ".minute")]
        
        dates_df[, paste0(var, ".day.minutes.poly.", 1:5)] <- 
            as.matrix(poly(dates_df[, paste0(var, ".day.minutes")], 5))
        
#         print(gp <- myplot_box(df=dates_df, ycol_names="PubDate.day.minutes", 
#                                xcol_name=rsp_var))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name=".rownames", 
#                         ycol_name="PubDate.day.minutes", colorcol_name=rsp_var))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate", 
#                         ycol_name="PubDate.day.minutes.poly.1", colorcol_name=rsp_var))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.day.minutes", 
#                         ycol_name="PubDate.day.minutes.poly.4", colorcol_name=rsp_var))
# 
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate", 
#                         ycol_name="PubDate.day.minutes", colorcol_name=rsp_var, smooth=TRUE))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate", 
#                         ycol_name="PubDate.day.minutes.poly.4", colorcol_name=rsp_var, smooth=TRUE))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate", 
#                         ycol_name=c("PubDate.day.minutes", "PubDate.day.minutes.poly.4"), 
#                         colorcol_name=rsp_var))
        
        print(gp <- myplot_scatter(df=subset(dates_df, Popular.fctr=="Y"), 
                                   xcol_name="PubDate.juliandate", 
                        ycol_name="PubDate.day.minutes", colorcol_name=rsp_var))
        print(gp <- myplot_box(df=dates_df, ycol_names="PubDate.second", 
                               xcol_name=rsp_var))
        print(gp <- myplot_bar(df=dates_df, ycol_names="PubDate.second.fctr", 
                               xcol_name=rsp_var, colorcol_name="PubDate.second.fctr"))                
        keep_feats <- union(keep_feats, paste(var, 
            c(".POSIX", ".year.fctr", ".month.fctr", ".date.fctr", ".wkday.fctr", 
              ".wkend", ".hour.fctr", ".minute.fctr", ".second.fctr", 
              paste0(".day.minutes.poly.", 1:5)), sep=""))        
    }
    #myprint_df(dates_df)
    return(dates_df[, keep_feats])
}

if (!is.null(glb_date_vars)) {
    glb_allobs_df <- cbind(glb_allobs_df, 
        myextract_dates_df(df=glb_allobs_df, vars=glb_date_vars, rsp_var=glb_rsp_var))
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                          paste(glb_date_vars, c("", ".POSIX"), sep=""))
}

if (!is.null(glb_date_vars)) {
    srt_allobs_df <- orderBy(~PubDate.POSIX, glb_allobs_df)
    print(myplot_scatter(subset(srt_allobs_df, 
                                PubDate.POSIX < strptime("2014-09-02", "%Y-%m-%d")), 
                                xcol_name="PubDate.POSIX", ycol_name=glb_rsp_var,
                               colorcol_name=glb_rsp_var
                         ))
    # Create features that measure the gap between previous timestamp in the data
    require(zoo)
    pd = as.POSIXlt(srt_allobs_df$PubDate)
    z = zoo(as.numeric(pd))
    srt_allobs_df[, "PubDate.zoo"] <- z
    print(head(srt_allobs_df))
    print(myplot_scatter(subset(srt_allobs_df, 
                                PubDate.POSIX < strptime("2014-09-02", "%Y-%m-%d")), 
                                xcol_name="PubDate.zoo", ycol_name=glb_rsp_var,
                               colorcol_name=glb_rsp_var
                         ))
    n = nrow(srt_allobs_df)
    b = zoo(, seq(n))
    
    last1 = as.numeric(merge(z-lag(z, -1), b, all = TRUE))
    srt_allobs_df[, "PubDate.last1"] <- last1
    srt_allobs_df[is.na(srt_allobs_df$PubDate.last1), "PubDate.last1"] <- 0
    srt_allobs_df[, "PubDate.last1.log"] <- log(1 + srt_allobs_df[, "PubDate.last1"])
    print(gp <- myplot_box(df=subset(srt_allobs_df, PubDate.last1.log > 0), 
                           ycol_names="PubDate.last1.log", 
                           xcol_name=glb_rsp_var))
    
    last10 = as.numeric(merge(z-lag(z, -10), b, all = TRUE))
    srt_allobs_df[, "PubDate.last10"] <- last10
    srt_allobs_df[is.na(srt_allobs_df$PubDate.last10), "PubDate.last10"] <- 0
    srt_allobs_df[, "PubDate.last10.log"] <- log(1 + srt_allobs_df[, "PubDate.last10"])
    print(gp <- myplot_box(df=subset(srt_allobs_df, PubDate.last10.log > 0), 
                           ycol_names="PubDate.last10.log", 
                           xcol_name=glb_rsp_var))
    
    last100 = as.numeric(merge(z-lag(z, -100), b, all = TRUE))
    srt_allobs_df[, "PubDate.last100"] <- last100
    srt_allobs_df[is.na(srt_allobs_df$PubDate.last100), "PubDate.last100"] <- 0
    srt_allobs_df[, "PubDate.last100.log"] <- log(1 + srt_allobs_df[, "PubDate.last100"])
    print(gp <- myplot_box(df=subset(srt_allobs_df, PubDate.last100.log > 0), 
                           ycol_names="PubDate.last100.log", 
                           xcol_name=glb_rsp_var))
    
    glb_allobs_df <- srt_allobs_df
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
        c("PubDate.zoo", "PubDate.last1", "PubDate.last10", "PubDate.last100"))
    # all2$last3 = as.numeric(merge(z-lag(z, -3), b, all = TRUE))
    # all2$last5 = as.numeric(merge(z-lag(z, -5), b, all = TRUE))
    # all2$last10 = as.numeric(merge(z-lag(z, -10), b, all = TRUE))
    # all2$last20 = as.numeric(merge(z-lag(z, -20), b, all = TRUE))
    # all2$last50 = as.numeric(merge(z-lag(z, -50), b, all = TRUE))
    # 
    # 
    # # order table
    # all2 = all2[order(all2$id),]
    # 
    # ## fill in NAs
    # # count averages
    # na.avg = all2 %>% group_by(weekend, hour) %>% dplyr::summarise(
    #     last1=mean(last1, na.rm=TRUE),
    #     last3=mean(last3, na.rm=TRUE),
    #     last5=mean(last5, na.rm=TRUE),
    #     last10=mean(last10, na.rm=TRUE),
    #     last20=mean(last20, na.rm=TRUE),
    #     last50=mean(last50, na.rm=TRUE)
    # )
    # 
    # # fill in averages
    # na.merge = merge(all2, na.avg, by=c("weekend","hour"))
    # na.merge = na.merge[order(na.merge$id),]
    # for(i in c("last1", "last3", "last5", "last10", "last20", "last50")) {
    #     y = paste0(i, ".y")
    #     idx = is.na(all2[[i]])
    #     all2[idx,][[i]] <- na.merge[idx,][[y]]
    # }
    # rm(na.avg, na.merge, b, i, idx, n, pd, sec, sh, y, z)
}

# check distribution of all numeric data
dsp_numeric_vars_dstrb <- function(vars_lst) {
    for (var in vars_lst) {
        print(sprintf("var: %s", var))
        gp <- myplot_box(df=glb_allobs_df, ycol_names=var, xcol_name=glb_rsp_var)
        if (inherits(glb_allobs_df[, var], "factor"))
            gp <- gp + facet_wrap(reformulate(var))
        print(gp)
    }
}
# dsp_numeric_vars_dstrb(setdiff(names(glb_allobs_df), 
#                                 union(myfind_chr_cols_df(glb_allobs_df), 
#                                       c(glb_rsp_var_raw, glb_rsp_var))))                                      

add_new_diag_feats <- function(obs_df, ref_df=glb_allobs_df) {
    require(plyr)
    
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>),

#         <col_name>.fctr=factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))), 
#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<ref_val>"), 
#         <col2_name>.fctr=relevel(factor(ifelse(<col1_name> == <val>, "<oth_val>", "<ref_val>")), 
#                               as.factor(c("R", "<ref_val>")),
#                               ref="<ref_val>"),

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>=<table>[as.character(<col2_name>)],
#         <col_name>=as.numeric(<col2_name>),

        .rnorm=rnorm(n=nrow(obs_df))
                        )

    if (!is.null(glb_log_vars)) {
        stop("not implemented yet")
        # Cycle thru glb_log_vars & create logs
#         <col_name>.log=log(1 + <col.name>),  
        # Add raw_vars to glb_exclude_vars_as_features
        # Add WordCount.log since WordCount is not distributed normally -> automatically do ???
        print("Replacing WordCount with WordCount.log in potential feature set")
        glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, "WordCount")
    }      
        
    # If levels of a factor are different across obs_df & glb_newobs_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    #print(summary(obs_df))
    #print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}
glb_allobs_df <- add_new_diag_feats(glb_allobs_df)
```

```
## Loading required package: plyr
```

```r
# Check distributions of newly transformed / extracted vars
#   Enhancement: remove vars that were displayed ealier
dsp_numeric_vars_dstrb(setdiff(names(glb_allobs_df), 
    union(myfind_chr_cols_df(glb_allobs_df), 
        union(glb_rsp_var_raw, 
            union(glb_rsp_var, glb_exclude_vars_as_features)))))
```

```
## [1] "var: Population"
```

```
## Warning in myplot_box(df = glb_allobs_df, ycol_names = var, xcol_name
## = glb_rsp_var): xcol_name:LifeExpectancy is not a factor; creating
## LifeExpectancy_fctr
```

```
## [1] "var: Under15"
```

```
## Warning in myplot_box(df = glb_allobs_df, ycol_names = var, xcol_name
## = glb_rsp_var): xcol_name:LifeExpectancy is not a factor; creating
## LifeExpectancy_fctr
```

![](WHO2_files/figure-html/inspect.data-2.png) 

```
## [1] "var: Over60"
```

```
## Warning in myplot_box(df = glb_allobs_df, ycol_names = var, xcol_name
## = glb_rsp_var): xcol_name:LifeExpectancy is not a factor; creating
## LifeExpectancy_fctr
```

![](WHO2_files/figure-html/inspect.data-3.png) 

```
## [1] "var: FertilityRate"
```

```
## Warning in myplot_box(df = glb_allobs_df, ycol_names = var, xcol_name
## = glb_rsp_var): xcol_name:LifeExpectancy is not a factor; creating
## LifeExpectancy_fctr
```

![](WHO2_files/figure-html/inspect.data-4.png) 

```
## Warning in loop_apply(n, do.ply): Removed 11 rows containing non-finite
## values (stat_boxplot).
```

```
## Warning in loop_apply(n, do.ply): Removed 11 rows containing missing values
## (stat_summary).
```

```
## Warning in loop_apply(n, do.ply): Removed 8 rows containing missing values
## (geom_text).
```

```
## [1] "var: ChildMortality"
```

```
## Warning in myplot_box(df = glb_allobs_df, ycol_names = var, xcol_name
## = glb_rsp_var): xcol_name:LifeExpectancy is not a factor; creating
## LifeExpectancy_fctr
```

![](WHO2_files/figure-html/inspect.data-5.png) 

```
## [1] "var: CellularSubscribers"
```

```
## Warning in myplot_box(df = glb_allobs_df, ycol_names = var, xcol_name
## = glb_rsp_var): xcol_name:LifeExpectancy is not a factor; creating
## LifeExpectancy_fctr
```

![](WHO2_files/figure-html/inspect.data-6.png) 

```
## Warning in loop_apply(n, do.ply): Removed 10 rows containing non-finite
## values (stat_boxplot).
```

```
## Warning in loop_apply(n, do.ply): Removed 10 rows containing missing values
## (stat_summary).
```

```
## Warning in loop_apply(n, do.ply): Removed 9 rows containing missing values
## (geom_text).
```

```
## [1] "var: LiteracyRate"
```

```
## Warning in myplot_box(df = glb_allobs_df, ycol_names = var, xcol_name
## = glb_rsp_var): xcol_name:LifeExpectancy is not a factor; creating
## LifeExpectancy_fctr
```

![](WHO2_files/figure-html/inspect.data-7.png) 

```
## Warning in loop_apply(n, do.ply): Removed 91 rows containing non-finite
## values (stat_boxplot).
```

```
## Warning in loop_apply(n, do.ply): Removed 91 rows containing missing values
## (stat_summary).
```

```
## Warning in loop_apply(n, do.ply): Removed 28 rows containing missing values
## (geom_text).
```

```
## [1] "var: GNI"
```

```
## Warning in myplot_box(df = glb_allobs_df, ycol_names = var, xcol_name
## = glb_rsp_var): xcol_name:LifeExpectancy is not a factor; creating
## LifeExpectancy_fctr
```

![](WHO2_files/figure-html/inspect.data-8.png) 

```
## Warning in loop_apply(n, do.ply): Removed 32 rows containing non-finite
## values (stat_boxplot).
```

```
## Warning in loop_apply(n, do.ply): Removed 32 rows containing missing values
## (stat_summary).
```

```
## Warning in loop_apply(n, do.ply): Removed 19 rows containing missing values
## (geom_text).
```

```
## [1] "var: PrimarySchoolEnrollmentMale"
```

```
## Warning in myplot_box(df = glb_allobs_df, ycol_names = var, xcol_name
## = glb_rsp_var): xcol_name:LifeExpectancy is not a factor; creating
## LifeExpectancy_fctr
```

![](WHO2_files/figure-html/inspect.data-9.png) 

```
## Warning in loop_apply(n, do.ply): Removed 93 rows containing non-finite
## values (stat_boxplot).
```

```
## Warning in loop_apply(n, do.ply): Removed 93 rows containing missing values
## (stat_summary).
```

```
## Warning in loop_apply(n, do.ply): Removed 33 rows containing missing values
## (geom_text).
```

```
## [1] "var: PrimarySchoolEnrollmentFemale"
```

```
## Warning in myplot_box(df = glb_allobs_df, ycol_names = var, xcol_name
## = glb_rsp_var): xcol_name:LifeExpectancy is not a factor; creating
## LifeExpectancy_fctr
```

![](WHO2_files/figure-html/inspect.data-10.png) 

```
## Warning in loop_apply(n, do.ply): Removed 93 rows containing non-finite
## values (stat_boxplot).
```

```
## Warning in loop_apply(n, do.ply): Removed 93 rows containing missing values
## (stat_summary).
```

```
## Warning in loop_apply(n, do.ply): Removed 33 rows containing missing values
## (geom_text).
```

```
## [1] "var: .rnorm"
```

```
## Warning in myplot_box(df = glb_allobs_df, ycol_names = var, xcol_name
## = glb_rsp_var): xcol_name:LifeExpectancy is not a factor; creating
## LifeExpectancy_fctr
```

![](WHO2_files/figure-html/inspect.data-11.png) ![](WHO2_files/figure-html/inspect.data-12.png) 

```r
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

#pairs(subset(glb_trnobs_df, select=-c(col_symbol)))
# Check for glb_newobs_df & glb_trnobs_df features range mismatches

# Other diagnostics:
# print(subset(glb_trnobs_df, <col1_name> == max(glb_trnobs_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_trnobs_df$<col1_name>, na.rm=TRUE)))

# print(glb_trnobs_df[which.max(glb_trnobs_df$<col_name>),])

# print(<col_name>_freq_glb_trnobs_df <- mycreate_tbl_df(glb_trnobs_df, "<col_name>"))
# print(which.min(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>)[, 2]))
# print(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>))
# print(table(is.na(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(table(sign(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(mycreate_xtab_df(glb_trnobs_df, <col1_name>))
# print(mycreate_xtab_df(glb_trnobs_df, c(<col1_name>, <col2_name>)))
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mycreate_xtab_df(glb_trnobs_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_trnobs_df[is.na(<col1_name>_<col2_name>_xtab_glb_trnobs_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_trnobs_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>.NA, glb_trnobs_df$<col2_name>, mean, na.rm=TRUE)))

# Other plots:
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_trnobs_df, Symbol %in% c("KO", "PG")), 
#                   "Date.my", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.Date("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.Date("1983-01-01")))        
#         )
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", colorcol_name="<Pred.fctr>") + 
#         geom_point(data=subset(glb_allobs_df, <condition>), 
#                     mapping=aes(x=<x_var>, y=<y_var>), color="red", shape=4, size=5))

rm(srt_allobs_df, last1, last10, last100, pd)
```

```
## Warning in rm(srt_allobs_df, last1, last10, last100, pd): object
## 'srt_allobs_df' not found
```

```
## Warning in rm(srt_allobs_df, last1, last10, last100, pd): object 'last1'
## not found
```

```
## Warning in rm(srt_allobs_df, last1, last10, last100, pd): object 'last10'
## not found
```

```
## Warning in rm(srt_allobs_df, last1, last10, last100, pd): object 'last100'
## not found
```

```
## Warning in rm(srt_allobs_df, last1, last10, last100, pd): object 'pd' not
## found
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cleanse.data", major.inc=FALSE)
```

```
##          label step_major step_minor    bgn    end elapsed
## 2 inspect.data          2          0  8.260 22.189  13.929
## 3 cleanse.data          2          1 22.189     NA      NA
```

### Step `2.1: cleanse data`

```r
# Options:
#   1. Not fill missing vars
#   2. Fill missing numerics with a different algorithm
#   3. Fill missing chars with data based on clusters 

dsp_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
##                 FertilityRate           CellularSubscribers 
##                            11                            10 
##                  LiteracyRate                           GNI 
##                            91                            32 
##   PrimarySchoolEnrollmentMale PrimarySchoolEnrollmentFemale 
##                            93                            93 
## [1] "numeric data w/ 0s in glb_allobs_df: "
##                    Population                       Under15 
##                             0                             0 
##                        Over60                 FertilityRate 
##                             0                             0 
##                LifeExpectancy                ChildMortality 
##                             0                             0 
##           CellularSubscribers                  LiteracyRate 
##                             0                             0 
##                           GNI   PrimarySchoolEnrollmentMale 
##                             0                             0 
## PrimarySchoolEnrollmentFemale                        .rnorm 
##                             0                             0 
## [1] "numeric data w/ Infs in glb_allobs_df: "
##                    Population                       Under15 
##                             0                             0 
##                        Over60                 FertilityRate 
##                             0                             0 
##                LifeExpectancy                ChildMortality 
##                             0                             0 
##           CellularSubscribers                  LiteracyRate 
##                             0                             0 
##                           GNI   PrimarySchoolEnrollmentMale 
##                             0                             0 
## PrimarySchoolEnrollmentFemale                        .rnorm 
##                             0                             0 
## [1] "numeric data w/ NaNs in glb_allobs_df: "
##                    Population                       Under15 
##                             0                             0 
##                        Over60                 FertilityRate 
##                             0                             0 
##                LifeExpectancy                ChildMortality 
##                             0                             0 
##           CellularSubscribers                  LiteracyRate 
##                             0                             0 
##                           GNI   PrimarySchoolEnrollmentMale 
##                             0                             0 
## PrimarySchoolEnrollmentFemale                        .rnorm 
##                             0                             0 
## [1] "string data missing in glb_allobs_df: "
## Country  Region 
##       0       0
```

```r
if (!is.null(glb_force_0_to_NA_vars)) {
    warning("Forcing ", nrow(subset(glb_allobs_df, WordCount.log == 0)),
            " obs with WordCount.log 0s to NA")
    glb_allobs_df[glb_allobs_df$WordCount.log == 0, "WordCount.log"] <- NA
}

dsp_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
##                 FertilityRate           CellularSubscribers 
##                            11                            10 
##                  LiteracyRate                           GNI 
##                            91                            32 
##   PrimarySchoolEnrollmentMale PrimarySchoolEnrollmentFemale 
##                            93                            93 
## [1] "numeric data w/ 0s in glb_allobs_df: "
##                    Population                       Under15 
##                             0                             0 
##                        Over60                 FertilityRate 
##                             0                             0 
##                LifeExpectancy                ChildMortality 
##                             0                             0 
##           CellularSubscribers                  LiteracyRate 
##                             0                             0 
##                           GNI   PrimarySchoolEnrollmentMale 
##                             0                             0 
## PrimarySchoolEnrollmentFemale                        .rnorm 
##                             0                             0 
## [1] "numeric data w/ Infs in glb_allobs_df: "
##                    Population                       Under15 
##                             0                             0 
##                        Over60                 FertilityRate 
##                             0                             0 
##                LifeExpectancy                ChildMortality 
##                             0                             0 
##           CellularSubscribers                  LiteracyRate 
##                             0                             0 
##                           GNI   PrimarySchoolEnrollmentMale 
##                             0                             0 
## PrimarySchoolEnrollmentFemale                        .rnorm 
##                             0                             0 
## [1] "numeric data w/ NaNs in glb_allobs_df: "
##                    Population                       Under15 
##                             0                             0 
##                        Over60                 FertilityRate 
##                             0                             0 
##                LifeExpectancy                ChildMortality 
##                             0                             0 
##           CellularSubscribers                  LiteracyRate 
##                             0                             0 
##                           GNI   PrimarySchoolEnrollmentMale 
##                             0                             0 
## PrimarySchoolEnrollmentFemale                        .rnorm 
##                             0                             0 
## [1] "string data missing in glb_allobs_df: "
## Country  Region 
##       0       0
```

```r
dsp_catgs <- function() {
    print("NewsDesk:")
    print(table(glb_allobs_df$NewsDesk))
    print("SectionName:")    
    print(table(glb_allobs_df$SectionName))
    print("SubsectionName:")        
    print(table(glb_allobs_df$SubsectionName))
}

sel_obs <- function(Popular=NULL, 
                    NewsDesk=NULL, SectionName=NULL, SubsectionName=NULL,
        Headline.contains=NULL, Snippet.contains=NULL, Abstract.contains=NULL,
        Headline.pfx=NULL, NewsDesk.nb=NULL, .clusterid=NULL, myCategory=NULL,
        perl=FALSE) {
    tmp_df <- glb_allobs_df
    # Does not work for Popular == NAs ???
    if (!is.null(Popular)) {
        if (is.na(Popular))
            tmp_df <- tmp_df[is.na(tmp_df$Popular), ] else   
            tmp_df <- tmp_df[tmp_df$Popular == Popular, ]    
    }    
    if (!is.null(NewsDesk)) 
        tmp_df <- tmp_df[tmp_df$NewsDesk == NewsDesk, ]
    if (!is.null(SectionName)) 
        tmp_df <- tmp_df[tmp_df$SectionName == SectionName, ]
    if (!is.null(SubsectionName)) 
        tmp_df <- tmp_df[tmp_df$SubsectionName == SubsectionName, ]
    if (!is.null(Headline.contains))
        tmp_df <- 
            tmp_df[grep(Headline.contains, tmp_df$Headline, perl=perl), ]
    if (!is.null(Snippet.contains))
        tmp_df <- 
            tmp_df[grep(Snippet.contains, tmp_df$Snippet, perl=perl), ]
    if (!is.null(Abstract.contains))
        tmp_df <- 
            tmp_df[grep(Abstract.contains, tmp_df$Abstract, perl=perl), ]
    if (!is.null(Headline.pfx)) {
        if (length(grep("Headline.pfx", names(tmp_df), fixed=TRUE, value=TRUE))
            > 0) tmp_df <- 
                tmp_df[tmp_df$Headline.pfx == Headline.pfx, ] else
        warning("glb_allobs_df does not contain Headline.pfx; ignoring that filter")                    
    }    
    if (!is.null(NewsDesk.nb)) {
        if (any(grepl("NewsDesk.nb", names(tmp_df), fixed=TRUE)) > 0) 
            tmp_df <- 
                tmp_df[tmp_df$NewsDesk.nb == NewsDesk.nb, ] else
        warning("glb_allobs_df does not contain NewsDesk.nb; ignoring that filter")                    
    }    
    if (!is.null(.clusterid)) {
        if (any(grepl(".clusterid", names(tmp_df), fixed=TRUE)) > 0) 
            tmp_df <- 
                tmp_df[tmp_df$clusterid == clusterid, ] else
        warning("glb_allobs_df does not contain clusterid; ignoring that filter")                       }
    if (!is.null(myCategory)) {    
        if (!(myCategory %in% names(glb_allobs_df)))
            tmp_df <-
                tmp_df[tmp_df$myCategory == myCategory, ] else
        warning("glb_allobs_df does not contain myCategory; ignoring that filter")              
    }    
    
    return(glb_allobs_df$UniqueID %in% tmp_df$UniqueID)
}

dsp_obs <- function(..., cols=c(NULL), all=FALSE) {
    tmp_df <- glb_allobs_df[sel_obs(...), 
                            union(c("UniqueID", "Popular", "myCategory", "Headline"), cols), FALSE]
    if(all) { print(tmp_df) } else { myprint_df(tmp_df) }
}
#dsp_obs(Popular=1, NewsDesk="", SectionName="", Headline.contains="Boehner")
# dsp_obs(Popular=1, NewsDesk="", SectionName="")
# dsp_obs(Popular=NA, NewsDesk="", SectionName="")

dsp_tbl <- function(...) {
    tmp_entity_df <- glb_allobs_df[sel_obs(...), ]
    tmp_tbl <- table(tmp_entity_df$NewsDesk, 
                     tmp_entity_df$SectionName,
                     tmp_entity_df$SubsectionName, 
                     tmp_entity_df$Popular, useNA="ifany")
    #print(names(tmp_tbl))
    #print(dimnames(tmp_tbl))
    print(tmp_tbl)
}

dsp_hdlxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
                           c("Headline.pfx", "Headline", glb_rsp_var)))
#dsp_hdlxtab("(1914)|(1939)")

dsp_catxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
        c("Headline.pfx", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# dsp_catxtab("1914)|(1939)")
# dsp_catxtab("19(14|39|64):")
# dsp_catxtab("19..:")

# Create myCategory <- NewsDesk#SectionName#SubsectionName
#   Fix some data before merging categories
# glb_allobs_df[sel_obs(Headline.contains="Your Turn:", NewsDesk=""),
#               "NewsDesk"] <- "Styles"
# glb_allobs_df[sel_obs(Headline.contains="School", NewsDesk="", SectionName="U.S.",
#                       SubsectionName=""),
#               "SubsectionName"] <- "Education"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SectionName"] <- "Business Day"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SubsectionName"] <- "Small Business"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SectionName"] <- "Opinion"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SubsectionName"] <- "Room For Debate"

# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName="", Popular=NA),
#               "SubsectionName"] <- "Small Business"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(7973), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName=""),
#               "SectionName"] <- "Technology"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5076, 5736, 5924, 5911, 6532), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(SectionName="Health"),
#               "NewsDesk"] <- "Science"
# glb_allobs_df[sel_obs(SectionName="Travel"),
#               "NewsDesk"] <- "Travel"
# 
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SectionName"] <- ""
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SubsectionName"] <- ""
# glb_allobs_df[sel_obs(NewsDesk="Styles", SectionName="", SubsectionName="", Popular=1),
#               "SectionName"] <- "U.S."
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5486), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df$myCategory <- paste(glb_allobs_df$NewsDesk, 
#                                   glb_allobs_df$SectionName,
#                                   glb_allobs_df$SubsectionName,
#                                   sep="#")

# dsp_obs( Headline.contains="Music:"
#         #,NewsDesk=""
#         #,SectionName=""  
#         #,SubsectionName="Fashion & Style"
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
# dsp_obs( Headline.contains="."
#         ,NewsDesk=""
#         ,SectionName="Opinion"  
#         ,SubsectionName=""
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
                                        
# Merge some categories
# glb_allobs_df$myCategory <-
#     plyr::revalue(glb_allobs_df$myCategory, c(      
#         "#Business Day#Dealbook"            = "Business#Business Day#Dealbook",
#         "#Business Day#Small Business"      = "Business#Business Day#Small Business",
#         "#Crosswords/Games#"                = "Business#Crosswords/Games#",
#         "Business##"                        = "Business#Technology#",
#         "#Open#"                            = "Business#Technology#",
#         "#Technology#"                      = "Business#Technology#",
#         
#         "#Arts#"                            = "Culture#Arts#",        
#         "Culture##"                         = "Culture#Arts#",        
#         
#         "#World#Asia Pacific"               = "Foreign#World#Asia Pacific",        
#         "Foreign##"                         = "Foreign#World#",    
#         
#         "#N.Y. / Region#"                   = "Metro#N.Y. / Region#",  
#         
#         "#Opinion#"                         = "OpEd#Opinion#",                
#         "OpEd##"                            = "OpEd#Opinion#",        
# 
#         "#Health#"                          = "Science#Health#",
#         "Science##"                         = "Science#Health#",        
#         
#         "Styles##"                          = "Styles##Fashion",                        
#         "Styles#Health#"                    = "Science#Health#",                
#         "Styles#Style#Fashion & Style"      = "Styles##Fashion",        
# 
#         "#Travel#"                          = "Travel#Travel#",                
#         
#         "Magazine#Magazine#"                = "myOther",
#         "National##"                        = "myOther",
#         "National#U.S.#Politics"            = "myOther",        
#         "Sports##"                          = "myOther",
#         "Sports#Sports#"                    = "myOther",
#         "#U.S.#"                            = "myOther",        
#         
# 
# #         "Business##Small Business"        = "Business#Business Day#Small Business",        
# #         
# #         "#Opinion#"                       = "#Opinion#Room For Debate",        
#         "##"                                = "##"
# #         "Business##" = "Business#Business Day#Dealbook",
# #         "Foreign#World#" = "Foreign##",
# #         "#Open#" = "Other",
# #         "#Opinion#The Public Editor" = "OpEd#Opinion#",
# #         "Styles#Health#" = "Styles##",
# #         "Styles#Style#Fashion & Style" = "Styles##",
# #         "#U.S.#" = "#U.S.#Education",
#     ))

# ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
#                           mycreate_sqlxtab_df(glb_allobs_df,
#     c("myCategory", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# myprint_df(ctgry_xtab_df)
# write.table(ctgry_xtab_df, paste0(glb_out_pfx, "ctgry_xtab.csv"), 
#             row.names=FALSE)

# ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
#                        myCategory + NewsDesk + SectionName + SubsectionName ~ 
#                            Popular.fctr, sum, value.var=".n"))
# myprint_df(ctgry_cast_df)
# write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_cast.csv"), 
#             row.names=FALSE)

# print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df[, glb_rsp_var], 
#                              useNA="ifany"))

dsp_chisq.test <- function(...) {
    sel_df <- glb_allobs_df[sel_obs(...) & 
                            !is.na(glb_allobs_df$Popular), ]
    sel_df$.marker <- 1
    ref_df <- glb_allobs_df[!is.na(glb_allobs_df$Popular), ]
    mrg_df <- merge(ref_df[, c(glb_id_vars, "Popular")],
                    sel_df[, c(glb_id_vars, ".marker")], all.x=TRUE)
    mrg_df[is.na(mrg_df)] <- 0
    print(mrg_tbl <- table(mrg_df$.marker, mrg_df$Popular))
    print("Rows:Selected; Cols:Popular")
    #print(mrg_tbl)
    print(chisq.test(mrg_tbl))
}
# dsp_chisq.test(Headline.contains="[Ee]bola")
# dsp_chisq.test(Snippet.contains="[Ee]bola")
# dsp_chisq.test(Abstract.contains="[Ee]bola")

# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola"), ], 
#                           c(glb_rsp_var, "NewsDesk", "SectionName", "SubsectionName")))

# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName))
# print(table(glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))
# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))

# glb_allobs_df$myCategory.fctr <- as.factor(glb_allobs_df$myCategory)
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                                       c("myCategory", "NewsDesk", "SectionName", "SubsectionName"))

# Copy Headline into Snipper & Abstract if they are empty
# print(glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, c("Headline", "Snippet")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Snippet, 
#                     c("UniqueID", "Headline", "Snippet")])
# glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Snippet"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Headline"]
# 
# print(glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, c("Headline", "Abstract")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Abstract, 
#                     c("UniqueID", "Headline", "Abstract")])
# glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Abstract"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Headline"]

# WordCount_0_df <- subset(glb_allobs_df, WordCount == 0)
# table(WordCount_0_df$Popular, WordCount_0_df$WordCount, useNA="ifany")
# myprint_df(WordCount_0_df[, 
#                 c("UniqueID", "Popular", "WordCount", "Headline")])

glb_chunks_df <- myadd_chunk(glb_chunks_df, "manage.missing.data", major.inc=FALSE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 3        cleanse.data          2          1 22.189 25.021   2.832
## 4 manage.missing.data          2          2 25.022     NA      NA
```

### Step `2.2: manage missing data`

```r
# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))
# glb_trnobs_df <- na.omit(glb_trnobs_df)
# glb_newobs_df <- na.omit(glb_newobs_df)
# df[is.na(df)] <- 0

dsp_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
##                 FertilityRate           CellularSubscribers 
##                            11                            10 
##                  LiteracyRate                           GNI 
##                            91                            32 
##   PrimarySchoolEnrollmentMale PrimarySchoolEnrollmentFemale 
##                            93                            93 
## [1] "numeric data w/ 0s in glb_allobs_df: "
##                    Population                       Under15 
##                             0                             0 
##                        Over60                 FertilityRate 
##                             0                             0 
##                LifeExpectancy                ChildMortality 
##                             0                             0 
##           CellularSubscribers                  LiteracyRate 
##                             0                             0 
##                           GNI   PrimarySchoolEnrollmentMale 
##                             0                             0 
## PrimarySchoolEnrollmentFemale                        .rnorm 
##                             0                             0 
## [1] "numeric data w/ Infs in glb_allobs_df: "
##                    Population                       Under15 
##                             0                             0 
##                        Over60                 FertilityRate 
##                             0                             0 
##                LifeExpectancy                ChildMortality 
##                             0                             0 
##           CellularSubscribers                  LiteracyRate 
##                             0                             0 
##                           GNI   PrimarySchoolEnrollmentMale 
##                             0                             0 
## PrimarySchoolEnrollmentFemale                        .rnorm 
##                             0                             0 
## [1] "numeric data w/ NaNs in glb_allobs_df: "
##                    Population                       Under15 
##                             0                             0 
##                        Over60                 FertilityRate 
##                             0                             0 
##                LifeExpectancy                ChildMortality 
##                             0                             0 
##           CellularSubscribers                  LiteracyRate 
##                             0                             0 
##                           GNI   PrimarySchoolEnrollmentMale 
##                             0                             0 
## PrimarySchoolEnrollmentFemale                        .rnorm 
##                             0                             0 
## [1] "string data missing in glb_allobs_df: "
## Country  Region 
##       0       0
```

```r
# Not refactored into mydsutils.R since glb_*_df might be reassigned
glb_impute_missing_data <- function() {
    
    require(mice)
    set.seed(glb_mice_complete.seed)
    inp_impent_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                union(glb_exclude_vars_as_features, glb_rsp_var))]
    print("Summary before imputation: ")
    print(summary(inp_impent_df))
    out_impent_df <- complete(mice(inp_impent_df))
    print(summary(out_impent_df))
    
    ret_vars <- sapply(names(out_impent_df), 
                       function(col) ifelse(!identical(out_impent_df[, col], inp_impent_df[, col]), 
                                            col, ""))
    ret_vars <- ret_vars[ret_vars != ""]
    return(out_impent_df[, ret_vars])
}

if (glb_impute_na_data && 
    (ncol(nonna_df <- glb_impute_missing_data()) > 0)) {
    for (col in names(nonna_df)) {
        glb_allobs_df[, paste0(col, ".nonNA")] <- nonna_df[, col]
        glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, col)        
    }
}    
```

```
## Loading required package: mice
## Loading required package: Rcpp
## mice 2.22 2014-06-10
```

```
## [1] "Summary before imputation: "
##     Region            Population         Under15          Over60     
##  Length:194         Min.   :      1   Min.   :13.12   Min.   : 0.81  
##  Class :character   1st Qu.:   1696   1st Qu.:18.72   1st Qu.: 5.20  
##  Mode  :character   Median :   7790   Median :28.65   Median : 8.53  
##                     Mean   :  36360   Mean   :28.73   Mean   :11.16  
##                     3rd Qu.:  24535   3rd Qu.:37.75   3rd Qu.:16.69  
##                     Max.   :1390000   Max.   :49.99   Max.   :31.92  
##                                                                      
##  FertilityRate   ChildMortality    CellularSubscribers  LiteracyRate  
##  Min.   :1.260   Min.   :  2.200   Min.   :  2.57      Min.   :31.10  
##  1st Qu.:1.835   1st Qu.:  8.425   1st Qu.: 63.57      1st Qu.:71.60  
##  Median :2.400   Median : 18.600   Median : 97.75      Median :91.80  
##  Mean   :2.941   Mean   : 36.149   Mean   : 93.64      Mean   :83.71  
##  3rd Qu.:3.905   3rd Qu.: 55.975   3rd Qu.:120.81      3rd Qu.:97.85  
##  Max.   :7.580   Max.   :181.600   Max.   :196.41      Max.   :99.80  
##  NA's   :11                        NA's   :10          NA's   :91     
##       GNI        PrimarySchoolEnrollmentMale PrimarySchoolEnrollmentFemale
##  Min.   :  340   Min.   : 37.20              Min.   : 32.50               
##  1st Qu.: 2335   1st Qu.: 87.70              1st Qu.: 87.30               
##  Median : 7870   Median : 94.70              Median : 95.10               
##  Mean   :13321   Mean   : 90.85              Mean   : 89.63               
##  3rd Qu.:17558   3rd Qu.: 98.10              3rd Qu.: 97.90               
##  Max.   :86440   Max.   :100.00              Max.   :100.00               
##  NA's   :32      NA's   :93                  NA's   :93                   
##      .rnorm        
##  Min.   :-2.05325  
##  1st Qu.:-0.60703  
##  Median :-0.07994  
##  Mean   : 0.01068  
##  3rd Qu.: 0.61548  
##  Max.   : 3.24104  
##                    
## 
##  iter imp variable
##   1   1  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   1   2  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   1   3  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   1   4  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   1   5  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   2   1  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   2   2  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   2   3  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   2   4  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   2   5  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   3   1  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   3   2  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   3   3  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   3   4  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   3   5  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   4   1  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   4   2  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   4   3  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   4   4  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   4   5  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   5   1  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   5   2  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   5   3  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   5   4  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##   5   5  FertilityRate  CellularSubscribers  LiteracyRate  GNI  PrimarySchoolEnrollmentMale  PrimarySchoolEnrollmentFemale
##     Region            Population         Under15          Over60     
##  Length:194         Min.   :      1   Min.   :13.12   Min.   : 0.81  
##  Class :character   1st Qu.:   1696   1st Qu.:18.72   1st Qu.: 5.20  
##  Mode  :character   Median :   7790   Median :28.65   Median : 8.53  
##                     Mean   :  36360   Mean   :28.73   Mean   :11.16  
##                     3rd Qu.:  24535   3rd Qu.:37.75   3rd Qu.:16.69  
##                     Max.   :1390000   Max.   :49.99   Max.   :31.92  
##  FertilityRate   ChildMortality    CellularSubscribers  LiteracyRate  
##  Min.   :1.260   Min.   :  2.200   Min.   :  2.57      Min.   :31.10  
##  1st Qu.:1.840   1st Qu.:  8.425   1st Qu.: 65.00      1st Qu.:74.97  
##  Median :2.420   Median : 18.600   Median : 98.11      Median :89.55  
##  Mean   :2.905   Mean   : 36.149   Mean   : 94.10      Mean   :84.67  
##  3rd Qu.:3.768   3rd Qu.: 55.975   3rd Qu.:121.38      3rd Qu.:97.28  
##  Max.   :7.580   Max.   :181.600   Max.   :196.41      Max.   :99.80  
##       GNI        PrimarySchoolEnrollmentMale PrimarySchoolEnrollmentFemale
##  Min.   :  340   Min.   : 37.20              Min.   : 32.50               
##  1st Qu.: 2652   1st Qu.: 86.50              1st Qu.: 85.60               
##  Median : 7235   Median : 93.50              Median : 94.40               
##  Mean   :13497   Mean   : 90.81              Mean   : 89.66               
##  3rd Qu.:16822   3rd Qu.: 97.70              3rd Qu.: 97.10               
##  Max.   :86440   Max.   :100.00              Max.   :100.00               
##      .rnorm        
##  Min.   :-2.05325  
##  1st Qu.:-0.60703  
##  Median :-0.07994  
##  Mean   : 0.01068  
##  3rd Qu.: 0.61548  
##  Max.   : 3.24104
```

```r
dsp_problem_data(glb_allobs_df, terminate = TRUE)
```

```
## [1] "numeric data missing in glb_allobs_df: "
##                 FertilityRate           CellularSubscribers 
##                            11                            10 
##                  LiteracyRate                           GNI 
##                            91                            32 
##   PrimarySchoolEnrollmentMale PrimarySchoolEnrollmentFemale 
##                            93                            93 
## [1] "numeric data w/ 0s in glb_allobs_df: "
##                          Population                             Under15 
##                                   0                                   0 
##                              Over60                       FertilityRate 
##                                   0                                   0 
##                      LifeExpectancy                      ChildMortality 
##                                   0                                   0 
##                 CellularSubscribers                        LiteracyRate 
##                                   0                                   0 
##                                 GNI         PrimarySchoolEnrollmentMale 
##                                   0                                   0 
##       PrimarySchoolEnrollmentFemale                              .rnorm 
##                                   0                                   0 
##                 FertilityRate.nonNA           CellularSubscribers.nonNA 
##                                   0                                   0 
##                  LiteracyRate.nonNA                           GNI.nonNA 
##                                   0                                   0 
##   PrimarySchoolEnrollmentMale.nonNA PrimarySchoolEnrollmentFemale.nonNA 
##                                   0                                   0 
## [1] "numeric data w/ Infs in glb_allobs_df: "
##                          Population                             Under15 
##                                   0                                   0 
##                              Over60                       FertilityRate 
##                                   0                                   0 
##                      LifeExpectancy                      ChildMortality 
##                                   0                                   0 
##                 CellularSubscribers                        LiteracyRate 
##                                   0                                   0 
##                                 GNI         PrimarySchoolEnrollmentMale 
##                                   0                                   0 
##       PrimarySchoolEnrollmentFemale                              .rnorm 
##                                   0                                   0 
##                 FertilityRate.nonNA           CellularSubscribers.nonNA 
##                                   0                                   0 
##                  LiteracyRate.nonNA                           GNI.nonNA 
##                                   0                                   0 
##   PrimarySchoolEnrollmentMale.nonNA PrimarySchoolEnrollmentFemale.nonNA 
##                                   0                                   0 
## [1] "numeric data w/ NaNs in glb_allobs_df: "
##                          Population                             Under15 
##                                   0                                   0 
##                              Over60                       FertilityRate 
##                                   0                                   0 
##                      LifeExpectancy                      ChildMortality 
##                                   0                                   0 
##                 CellularSubscribers                        LiteracyRate 
##                                   0                                   0 
##                                 GNI         PrimarySchoolEnrollmentMale 
##                                   0                                   0 
##       PrimarySchoolEnrollmentFemale                              .rnorm 
##                                   0                                   0 
##                 FertilityRate.nonNA           CellularSubscribers.nonNA 
##                                   0                                   0 
##                  LiteracyRate.nonNA                           GNI.nonNA 
##                                   0                                   0 
##   PrimarySchoolEnrollmentMale.nonNA PrimarySchoolEnrollmentFemale.nonNA 
##                                   0                                   0 
## [1] "string data missing in glb_allobs_df: "
## Country  Region 
##       0       0
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "encode.data", major.inc=FALSE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 4 manage.missing.data          2          2 25.022 26.252    1.23
## 5         encode.data          2          3 26.252     NA      NA
```

### Step `2.3: encode data`

```r
# map_<col_name>_df <- myimport_data(
#     url="<map_url>", 
#     comment="map_<col_name>_df", print_diagn=TRUE)
# map_<col_name>_df <- read.csv(paste0(getwd(), "/data/<file_name>.csv"), strip.white=TRUE)

# glb_trnobs_df <- mymap_codes(glb_trnobs_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
# glb_newobs_df <- mymap_codes(glb_newobs_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
    					
# glb_trnobs_df$<col_name>.fctr <- factor(glb_trnobs_df$<col_name>, 
#                     as.factor(union(glb_trnobs_df$<col_name>, glb_newobs_df$<col_name>)))
# glb_newobs_df$<col_name>.fctr <- factor(glb_newobs_df$<col_name>, 
#                     as.factor(union(glb_trnobs_df$<col_name>, glb_newobs_df$<col_name>)))

glb_chunks_df <- myadd_chunk(glb_chunks_df, "extract.features", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 5      encode.data          2          3 26.252 26.277   0.025
## 6 extract.features          3          0 26.277     NA      NA
```

## Step `3.0: extract features`

```r
#```{r extract_features, cache=FALSE, eval=!is.null(glb_txt_vars)}
extract.features_chunk_df <- myadd_chunk(NULL, "extract.features_bgn")
```

```
##                  label step_major step_minor    bgn end elapsed
## 1 extract.features_bgn          1          0 26.318  NA      NA
```

```r
# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_trnobs_df$<col_name>), -2, na.pad=TRUE)
# glb_trnobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_newobs_df$<col_name>), -2, na.pad=TRUE)
# glb_newobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_newobs_df[1, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df) - 1, 
#                                                    "<col_name>"]
# glb_newobs_df[2, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df), 
#                                                    "<col_name>"]
                                                   
# glb_allobs_df <- mutate(glb_allobs_df,
#     A.P.http=ifelse(grepl("http",Added,fixed=TRUE), 1, 0)
#                     )
# 
# glb_trnobs_df <- mutate(glb_trnobs_df,
#                     )
# 
# glb_newobs_df <- mutate(glb_newobs_df,
#                     )

#   Create factors of string variables
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "factorize.str.vars"), major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn    end
## 1                extract.features_bgn          1          0 26.318 26.325
## 2 extract.features_factorize.str.vars          2          0 26.326     NA
##   elapsed
## 1   0.007
## 2      NA
```

```r
print(str_vars <- myfind_chr_cols_df(glb_allobs_df))
```

```
##   Country    Region      .src 
## "Country"  "Region"    ".src"
```

```r
if (length(str_vars <- setdiff(str_vars, 
                               glb_exclude_vars_as_features)) > 0) {
    for (var in str_vars) {
        warning("Creating factors of string variable: ", var, 
                ": # of unique values: ", length(unique(glb_allobs_df[, var])))
        glb_allobs_df[, paste0(var, ".fctr")] <- factor(glb_allobs_df[, var], 
                        as.factor(unique(glb_allobs_df[, var])))
#         glb_trnobs_df[, paste0(var, ".fctr")] <- factor(glb_trnobs_df[, var], 
#                         as.factor(unique(glb_allobs_df[, var])))
#         glb_newobs_df[, paste0(var, ".fctr")] <- factor(glb_newobs_df[, var], 
#                         as.factor(unique(glb_allobs_df[, var])))
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, str_vars)
}
```

```
## Warning: Creating factors of string variable: Region: # of unique values: 6
```

```r
if (!is.null(glb_txt_vars)) {
    require(foreach)
    require(gsubfn)
    require(stringr)
    require(tm)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text"), major.inc=TRUE)
    
    chk_pattern_freq <- function(re_str, ignore.case=TRUE) {
        match_mtrx <- str_extract_all(txt_vctr, regex(re_str, ignore_case=ignore.case), 
                                      simplify=TRUE)
        match_df <- as.data.frame(match_mtrx[match_mtrx != ""])
        names(match_df) <- "pattern"
        return(mycreate_sqlxtab_df(match_df, "pattern"))        
    }
    #tmp_freq_df <- chk_pattern_freq("\\bNew (\\w)+", ignore.case=FALSE)
    #subset(chk_pattern_freq("\\bNew (\\w)+", ignore.case=FALSE), grepl("New [[:upper:]]", pattern))
    #chk_pattern_freq("\\bnew (\\W)+")

    chk_subfn <- function(pos_ix) {
        re_str <- gsubfn_args_lst[["re_str"]][[pos_ix]]
        print("re_str:"); print(re_str)
        rp_frmla <- gsubfn_args_lst[["rp_frmla"]][[pos_ix]]        
        print("rp_frmla:"); print(rp_frmla, showEnv=FALSE)
        tmp_vctr <- grep(re_str, txt_vctr, value=TRUE, ignore.case=TRUE)[1:5]
        print("Before:")
        print(tmp_vctr)
        print("After:")            
        print(gsubfn(re_str, rp_frmla, tmp_vctr, ignore.case=TRUE))
    }
    #chk_subfn(1)

    myapply_gsub <- function(...) {
        if ((length_lst <- length(names(gsub_map_lst))) == 0)
            return(txt_vctr)
        for (ptn_ix in 1:length_lst) {
            print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                            length(names(gsub_map_lst)), names(gsub_map_lst)[ptn_ix]))
            txt_vctr <- gsub(names(gsub_map_lst)[ptn_ix], gsub_map_lst[[ptn_ix]], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    myapply_txtmap <- function(txt_vctr, ...) {
        nrows <- nrow(glb_txt_map_df)
        for (ptn_ix in 1:nrows) {
            print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                            nrows, glb_txt_map_df[ptn_ix, "rex_str"]))
            txt_vctr <- gsub(glb_txt_map_df[ptn_ix, "rex_str"], 
                             glb_txt_map_df[ptn_ix, "rpl_str"], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    chk.equal <- function(bgn, end) {
        print(all.equal(sav_txt_lst[["Headline"]][bgn:end], glb_txt_lst[["Headline"]][bgn:end]))
    }    
    dsp.equal <- function(bgn, end) {
        print(sav_txt_lst[["Headline"]][bgn:end])
        print(glb_txt_lst[["Headline"]][bgn:end])
    }    
#sav_txt_lst <- glb_txt_lst; all.equal(sav_txt_lst, glb_txt_lst)
#all.equal(sav_txt_lst[["Headline"]][1:4200], glb_txt_lst[["Headline"]][1:4200])
#all.equal(sav_txt_lst[["Headline"]][1:2000], glb_txt_lst[["Headline"]][1:2000])
#all.equal(sav_txt_lst[["Headline"]][1:1000], glb_txt_lst[["Headline"]][1:1000])
#all.equal(sav_txt_lst[["Headline"]][1:500], glb_txt_lst[["Headline"]][1:500])
#all.equal(sav_txt_lst[["Headline"]][1:200], glb_txt_lst[["Headline"]][1:200])
#all.equal(sav_txt_lst[["Headline"]][1:100], glb_txt_lst[["Headline"]][1:100])
#chk.equal( 1, 100)
#chk.equal(51, 100)
#chk.equal(81, 100)
#chk.equal(81,  90)
#chk.equal(81,  85)
#chk.equal(86,  90)
#chk.equal(96, 100)

#dsp.equal(86, 90)
    
    glb_txt_map_df <- read.csv("mytxt_map.csv", comment.char="#", strip.white=TRUE)
    glb_txt_lst <- list(); 
    print(sprintf("Building glb_txt_lst..."))
    glb_txt_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_vctr <- glb_allobs_df[, txt_var]
        
        # myapply_txtmap shd be created as a tm_map::content_transformer ?
        #print(glb_txt_map_df)
        #txt_var=glb_txt_vars[3]; txt_vctr <- glb_txt_lst[[txt_var]]
        #print(rex_str <- glb_txt_map_df[glb_txt_map_df$rex_str == "\\bWall St\\.", "rex_str"])
        #print(rex_str <- glb_txt_map_df[grepl("du Pont", glb_txt_map_df$rex_str), "rex_str"])        
        #print(tmp_vctr <- grep(rex_str, txt_vctr, value=TRUE, ignore.case=FALSE))
        #ret_lst <- regexec(rex_str, txt_vctr, ignore.case=FALSE); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])
        #gsub(rex_str, glb_txt_map_df[glb_txt_map_df$rex_str == rex_str, "rpl_str"], tmp_vctr, ignore.case=FALSE)
        #grep("Hong Hong", txt_vctr, value=TRUE)
    
        txt_vctr <- myapply_txtmap(txt_vctr, ignore.case=FALSE)    
    }
    names(glb_txt_lst) <- glb_txt_vars

    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining Acronyms in %s:", txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]
        print(tmp_vctr <- grep("[[:upper:]]\\.", txt_vctr, value=TRUE, ignore.case=FALSE))
    }

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(Fort|Ft\\.|Hong|Las|Los|New|Puerto|Saint|San|St\\.)( |-)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]        
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl("( |-)[[:upper:]]", pattern))))
        print("    consider cleaning if relevant to problem domain; geography name; .n > 1")
        #grep("New G", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Wins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }        
        
    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(N|S|E|W|C)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))))
        #grep("N Weaver", txt_vctr, value=TRUE, ignore.case=FALSE)        
    }    

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(North|South|East|West|Central)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                        
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))))
        #grep("Central (African|Bankers|Cast|Italy|Role|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("East (Africa|Berlin|London|Poland|Rivals|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("North (American|Korean|West)", txt_vctr, value=TRUE, ignore.case=FALSE)        
        #grep("South (Pacific|Street)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Martins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }    

    find_cmpnd_wrds <- function(txt_vctr) {
        txt_corpus <- Corpus(VectorSource(txt_vctr))
        txt_corpus <- tm_map(txt_corpus, tolower)
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation, 
                             preserve_intra_word_dashes=TRUE)
        full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTf))
        print("   Full TermMatrix:"); print(full_Tf_DTM)
        full_Tf_mtrx <- as.matrix(full_Tf_DTM)
        rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_Tf_vctr <- colSums(full_Tf_mtrx)
        names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
        #grep("year", names(full_Tf_vctr), value=TRUE)
        #which.max(full_Tf_mtrx[, "yearlong"])
        full_Tf_df <- as.data.frame(full_Tf_vctr)
        names(full_Tf_df) <- "Tf.full"
        full_Tf_df$term <- rownames(full_Tf_df)
        #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
        full_Tf_df <- orderBy(~ -Tf.full, full_Tf_df)
        cmpnd_Tf_df <- full_Tf_df[grep("-", full_Tf_df$term, value=TRUE) ,]
        
        filter_df <- read.csv("mytxt_compound.csv", comment.char="#", strip.white=TRUE)
        cmpnd_Tf_df$filter <- FALSE
        for (row_ix in 1:nrow(filter_df))
            cmpnd_Tf_df[!cmpnd_Tf_df$filter, "filter"] <- 
            grepl(filter_df[row_ix, "rex_str"], 
                  cmpnd_Tf_df[!cmpnd_Tf_df$filter, "term"], ignore.case=TRUE)
        cmpnd_Tf_df <- subset(cmpnd_Tf_df, !filter)
        # Bug in tm_map(txt_corpus, removePunctuation, preserve_intra_word_dashes=TRUE) ???
        #   "net-a-porter" gets converted to "net-aporter"
        #grep("net-a-porter", txt_vctr, ignore.case=TRUE, value=TRUE)
        #grep("maser-laser", txt_vctr, ignore.case=TRUE, value=TRUE)
        #txt_corpus[[which(grepl("net-a-porter", txt_vctr, ignore.case=TRUE))]]
        #grep("\\b(across|longer)-(\\w)", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        #grep("(\\w)-(affected|term)\\b", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        
        print(sprintf("nrow(cmpnd_Tf_df): %d", nrow(cmpnd_Tf_df)))
        myprint_df(cmpnd_Tf_df)
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text_reporting_compound_terms"), major.inc=FALSE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining compound terms in %s: ", txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                        
#         find_cmpnd_wrds(txt_vctr)
        #grep("thirty-five", txt_vctr, ignore.case=TRUE, value=TRUE)
        #rex_str <- glb_txt_map_df[grepl("hirty", glb_txt_map_df$rex_str), "rex_str"]
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "build.corpus"), major.inc=TRUE)
    
    glb_corpus_lst <- list()
    print(sprintf("Building glb_corpus_lst..."))
    glb_corpus_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_corpus <- Corpus(VectorSource(glb_txt_lst[[txt_var]]))
        txt_corpus <- tm_map(txt_corpus, tolower) #nuppr
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation) #npnct<chr_ix>
#         txt-corpus <- tm_map(txt_corpus, content_transformer(function(x, pattern) gsub(pattern, "", x))   

        # Not to be run in production
        inspect_terms <- function() {
            full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                              control=list(weighting=weightTf))
            print("   Full TermMatrix:"); print(full_Tf_DTM)
            full_Tf_mtrx <- as.matrix(full_Tf_DTM)
            rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
            full_Tf_vctr <- colSums(full_Tf_mtrx)
            names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
            #grep("year", names(full_Tf_vctr), value=TRUE)
            #which.max(full_Tf_mtrx[, "yearlong"])
            full_Tf_df <- as.data.frame(full_Tf_vctr)
            names(full_Tf_df) <- "Tf.full"
            full_Tf_df$term <- rownames(full_Tf_df)
            #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
            full_Tf_df <- orderBy(~ -Tf.full +term, full_Tf_df)
            print(myplot_histogram(full_Tf_df, "Tf.full"))
            myprint_df(full_Tf_df)
            #txt_corpus[[which(grepl("zun", txt_vctr, ignore.case=TRUE))]]
            digit_terms_df <- subset(full_Tf_df, grepl("[[:digit:]]", term))
            myprint_df(digit_terms_df)
            return(full_Tf_df)
        }    
        #print("RemovePunct:"); remove_punct_Tf_df <- inspect_terms()

        txt_corpus <- tm_map(txt_corpus, removeWords, 
                             c(glb_append_stop_words[[txt_var]], 
                               stopwords("english"))) #nstopwrds
        #print("StoppedWords:"); stopped_words_Tf_df <- inspect_terms()
        txt_corpus <- tm_map(txt_corpus, stemDocument) #Features for lost information: Difference/ratio in density of full_TfIdf_DTM ???
        #txt_corpus <- tm_map(txt_corpus, content_transformer(stemDocument))        
        #print("StemmedWords:"); stemmed_words_Tf_df <- inspect_terms()
        #stemmed_stopped_Tf_df <- merge(stemmed_words_Tf_df, stopped_words_Tf_df, by="term", all=TRUE, suffixes=c(".stem", ".stop"))
        #myprint_df(stemmed_stopped_Tf_df)
        #print(subset(stemmed_stopped_Tf_df, grepl("compan", term)))
        #glb_corpus_lst[[txt_var]] <- txt_corpus
    }
    names(glb_corpus_lst) <- glb_txt_vars
        
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "extract.DTM"), major.inc=TRUE)

    glb_full_DTM_lst <- list(); glb_sprs_DTM_lst <- list();
    for (txt_var in glb_txt_vars) {
        print(sprintf("Extracting TfIDf terms for %s...", txt_var))        
        txt_corpus <- glb_corpus_lst[[txt_var]]
        
#         full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
#                                           control=list(weighting=weightTf))
        full_TfIdf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTfIdf))
        sprs_TfIdf_DTM <- removeSparseTerms(full_TfIdf_DTM, 
                                            glb_sprs_thresholds[txt_var])
        
#         glb_full_DTM_lst[[txt_var]] <- full_Tf_DTM
#         glb_sprs_DTM_lst[[txt_var]] <- sprs_Tf_DTM
        glb_full_DTM_lst[[txt_var]] <- full_TfIdf_DTM
        glb_sprs_DTM_lst[[txt_var]] <- sprs_TfIdf_DTM
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "report.DTM"), major.inc=TRUE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Reporting TfIDf terms for %s...", txt_var))        
        full_TfIdf_DTM <- glb_full_DTM_lst[[txt_var]]
        sprs_TfIdf_DTM <- glb_sprs_DTM_lst[[txt_var]]        

        print("   Full TermMatrix:"); print(full_TfIdf_DTM)
        full_TfIdf_mtrx <- as.matrix(full_TfIdf_DTM)
        rownames(full_TfIdf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_TfIdf_vctr <- colSums(full_TfIdf_mtrx)
        names(full_TfIdf_vctr) <- dimnames(full_TfIdf_DTM)[[2]]
        #grep("scene", names(full_TfIdf_vctr), value=TRUE)
        #which.max(full_TfIdf_mtrx[, "yearlong"])
        full_TfIdf_df <- as.data.frame(full_TfIdf_vctr)
        names(full_TfIdf_df) <- "TfIdf.full"
        full_TfIdf_df$term <- rownames(full_TfIdf_df)
        full_TfIdf_df$freq.full <- colSums(full_TfIdf_mtrx != 0)
        full_TfIdf_df <- orderBy(~ -TfIdf.full, full_TfIdf_df)

        print("   Sparse TermMatrix:"); print(sprs_TfIdf_DTM)
        sprs_TfIdf_vctr <- colSums(as.matrix(sprs_TfIdf_DTM))
        names(sprs_TfIdf_vctr) <- dimnames(sprs_TfIdf_DTM)[[2]]
        sprs_TfIdf_df <- as.data.frame(sprs_TfIdf_vctr)
        names(sprs_TfIdf_df) <- "TfIdf.sprs"
        sprs_TfIdf_df$term <- rownames(sprs_TfIdf_df)
        sprs_TfIdf_df$freq.sprs <- colSums(as.matrix(sprs_TfIdf_DTM) != 0)        
        sprs_TfIdf_df <- orderBy(~ -TfIdf.sprs, sprs_TfIdf_df)
        
        terms_TfIdf_df <- merge(full_TfIdf_df, sprs_TfIdf_df, all.x=TRUE)
        terms_TfIdf_df$in.sprs <- !is.na(terms_TfIdf_df$freq.sprs)
        plt_TfIdf_df <- subset(terms_TfIdf_df, 
                               TfIdf.full >= min(terms_TfIdf_df$TfIdf.sprs, na.rm=TRUE))
        plt_TfIdf_df$label <- ""
        plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "label"] <- 
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"]
        glb_important_terms[[txt_var]] <- union(glb_important_terms[[txt_var]],
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"])
        print(myplot_scatter(plt_TfIdf_df, "freq.full", "TfIdf.full", 
                             colorcol_name="in.sprs") + 
                  geom_text(aes(label=label), color="Black", size=3.5))
        
        melt_TfIdf_df <- orderBy(~ -value, melt(terms_TfIdf_df, id.var="term"))
        print(ggplot(melt_TfIdf_df, aes(value, color=variable)) + stat_ecdf() + 
                  geom_hline(yintercept=glb_sprs_thresholds[txt_var], 
                             linetype = "dotted"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, !is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(melt_TfIdf_df, "term", "value", 
                          colorcol_name="variable"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(head(melt_TfIdf_df, 10), "term", "value", 
                          colorcol_name="variable"))
    }

#     sav_full_DTM_lst <- glb_full_DTM_lst
#     sav_sprs_DTM_lst <- glb_sprs_DTM_lst
#     print(identical(sav_glb_corpus_lst, glb_corpus_lst))
#     print(all.equal(length(sav_glb_corpus_lst), length(glb_corpus_lst)))
#     print(all.equal(names(sav_glb_corpus_lst), names(glb_corpus_lst)))
#     print(all.equal(sav_glb_corpus_lst[["Headline"]], glb_corpus_lst[["Headline"]]))

#     print(identical(sav_full_DTM_lst, glb_full_DTM_lst))
#     print(identical(sav_sprs_DTM_lst, glb_sprs_DTM_lst))
        
    rm(full_TfIdf_mtrx, full_TfIdf_df, melt_TfIdf_df, terms_TfIdf_df)

    # Create txt features
    if ((length(glb_txt_vars) > 1) &&
        (length(unique(pfxs <- sapply(glb_txt_vars, 
                    function(txt) toupper(substr(txt, 1, 1))))) < length(glb_txt_vars)))
            stop("Prefixes for corpus freq terms not unique: ", pfxs)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DTM"), 
                                         major.inc=TRUE)
    for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DTM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))
        txt_X_df <- as.data.frame(as.matrix(glb_sprs_DTM_lst[[txt_var]]))
        colnames(txt_X_df) <- paste(txt_var_pfx, ".T.",
                                    make.names(colnames(txt_X_df)), sep="")
        rownames(txt_X_df) <- rownames(glb_allobs_df) # warning otherwise
#         plt_X_df <- cbind(txt_X_df, glb_allobs_df[, c(glb_id_vars, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today", xcol_name=glb_rsp_var))

#         log_X_df <- log(1 + txt_X_df)
#         colnames(log_X_df) <- paste(colnames(txt_X_df), ".log", sep="")
#         plt_X_df <- cbind(log_X_df, glb_allobs_df[, c(glb_id_vars, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today.log", xcol_name=glb_rsp_var))
        glb_allobs_df <- cbind(glb_allobs_df, txt_X_df) # TfIdf is normalized
        #glb_allobs_df <- cbind(glb_allobs_df, log_X_df) # if using non-normalized metrics 
    }
    #identical(chk_entity_df, glb_allobs_df)
    #chk_entity_df <- glb_allobs_df

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DXM"), 
                                         major.inc=TRUE)

#sav_allobs_df <- glb_allobs_df
    glb_punct_vctr <- c("!", "\"", "#", "\\$", "%", "&", "'", 
                        "\\(|\\)",# "\\(", "\\)", 
                        "\\*", "\\+", ",", "-", "\\.", "/", ":", ";", 
                        "<|>", # "<", 
                        "=", 
                        # ">", 
                        "\\?", "@", "\\[", "\\\\", "\\]", "^", "_", "`", 
                        "\\{", "\\|", "\\}", "~")
    txt_X_df <- glb_allobs_df[, c(glb_id_vars, ".rnorm"), FALSE]
    txt_X_df <- foreach(txt_var=glb_txt_vars, .combine=cbind) %dopar% {   
    #for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DXM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))        
        #txt_X_df <- glb_allobs_df[, c(glb_id_vars, ".rnorm"), FALSE]
        
        txt_full_DTM_mtrx <- as.matrix(glb_full_DTM_lst[[txt_var]])
        rownames(txt_full_DTM_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        #print(txt_full_DTM_mtrx[txt_full_DTM_mtrx[, "ebola"] != 0, "ebola"])
        
        # Create <txt_var>.T.<term> for glb_important_terms
        for (term in glb_important_terms[[txt_var]])
            txt_X_df[, paste0(txt_var_pfx, ".T.", make.names(term))] <- 
                txt_full_DTM_mtrx[, term]
                
        # Create <txt_var>.nwrds.log & .nwrds.unq.log
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")] <- 
            log(1 + mycount_pattern_occ("\\w+", glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.unq.log")] <- 
            log(1 + rowSums(txt_full_DTM_mtrx != 0))
        txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] <- 
            rowSums(txt_full_DTM_mtrx) 
        txt_X_df[, paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")] <- 
            txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] / 
            (exp(txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")]) - 1)

        # Create <txt_var>.nchrs.log
        txt_X_df[, paste0(txt_var_pfx, ".nchrs.log")] <- 
            log(1 + mycount_pattern_occ(".", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".nuppr.log")] <- 
            log(1 + mycount_pattern_occ("[[:upper:]]", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".ndgts.log")] <- 
            log(1 + mycount_pattern_occ("[[:digit:]]", glb_allobs_df[, txt_var]))

        # Create <txt_var>.npnct?.log
        # would this be faster if it's iterated over each row instead of 
        #   each created column ???
        for (punct_ix in 1:length(glb_punct_vctr)) { 
#             smp0 <- " "
#             smp1 <- "! \" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~"
#             smp2 <- paste(smp1, smp1, sep=" ")
#             print(sprintf("Testing %s pattern:", glb_punct_vctr[punct_ix])) 
#             results <- mycount_pattern_occ(glb_punct_vctr[punct_ix], c(smp0, smp1, smp2))
#             names(results) <- NULL; print(results)
            txt_X_df[, 
                paste0(txt_var_pfx, ".npnct", sprintf("%02d", punct_ix), ".log")] <-
                log(1 + mycount_pattern_occ(glb_punct_vctr[punct_ix], 
                                            glb_allobs_df[, txt_var]))
        }
#         print(head(glb_allobs_df[glb_allobs_df[, "A.npnct23.log"] > 0, 
#                                     c("UniqueID", "Popular", "Abstract", "A.npnct23.log")]))    
        
        # Create <txt_var>.nstopwrds.log & <txt_var>ratio.nstopwrds.nwrds
        stop_words_rex_str <- paste0("\\b(", paste0(c(glb_append_stop_words[[txt_var]], 
                                       stopwords("english")), collapse="|"),
                                     ")\\b")
        txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] <-
            log(1 + mycount_pattern_occ(stop_words_rex_str, glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".ratio.nstopwrds.nwrds")] <-
            exp(txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] - 
                txt_X_df[, paste0(txt_var_pfx, ".nwrds", ".log")])

        # Create <txt_var>.P.http
        txt_X_df[, paste(txt_var_pfx, ".P.http", sep="")] <- 
            as.integer(0 + mycount_pattern_occ("http", glb_allobs_df[, txt_var]))    
    
        # Create user-specified pattern vectors 
        #   <txt_var>.P.year.colon
        txt_X_df[, paste0(txt_var_pfx, ".P.year.colon")] <-
            as.integer(0 + mycount_pattern_occ("[0-9]{4}:", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".P.daily.clip.report")] <-
            as.integer(0 + mycount_pattern_occ("Daily Clip Report", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".P.fashion.week")] <-
            as.integer(0 + mycount_pattern_occ("Fashion Week", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".P.first.draft")] <-
            as.integer(0 + mycount_pattern_occ("First Draft", glb_allobs_df[, txt_var]))

#sum(mycount_pattern_occ("Metropolitan Diary:", glb_allobs_df$Abstract) > 0)
        if (txt_var %in% c("Snippet", "Abstract")) {
            txt_X_df[, paste0(txt_var_pfx, ".P.metropolitan.diary.colon")] <-
                as.integer(0 + mycount_pattern_occ("Metropolitan Diary:", 
                                                   glb_allobs_df[, txt_var]))
        }

#sum(mycount_pattern_occ("[0-9]{4}:", glb_allobs_df$Headline) > 0)
#sum(mycount_pattern_occ("Quandary(.*)(?=:)", glb_allobs_df$Headline, perl=TRUE) > 0)
#sum(mycount_pattern_occ("No Comment(.*):", glb_allobs_df$Headline) > 0)
#sum(mycount_pattern_occ("Friday Night Music:", glb_allobs_df$Headline) > 0)
        if (txt_var %in% c("Headline")) {
            txt_X_df[, paste0(txt_var_pfx, ".P.facts.figures")] <-
                as.integer(0 + mycount_pattern_occ("Facts & Figures:", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.friday.night.music")] <-
                as.integer(0 + mycount_pattern_occ("Friday Night Music", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.no.comment.colon")] <-
                as.integer(0 + mycount_pattern_occ("No Comment(.*):", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.on.this.day")] <-
                as.integer(0 + mycount_pattern_occ("On This Day", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.quandary")] <-
                as.integer(0 + mycount_pattern_occ("Quandary(.*)(?=:)", glb_allobs_df[, txt_var], perl=TRUE))
            txt_X_df[, paste0(txt_var_pfx, ".P.readers.respond")] <-
                as.integer(0 + mycount_pattern_occ("Readers Respond", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.recap.colon")] <-
                as.integer(0 + mycount_pattern_occ("Recap:", glb_allobs_df[, txt_var]))
            txt_X_df[, paste0(txt_var_pfx, ".P.s.notebook")] <-
                as.integer(0 + mycount_pattern_occ("s Notebook", glb_allobs_df[, txt_var]))
            txt_X_df[, paste0(txt_var_pfx, ".P.today.in.politic")] <-
                as.integer(0 + mycount_pattern_occ("Today in Politic", glb_allobs_df[, txt_var]))            
            txt_X_df[, paste0(txt_var_pfx, ".P.today.in.smallbusiness")] <-
                as.integer(0 + mycount_pattern_occ("Today in Small Business:", glb_allobs_df[, txt_var]))
            txt_X_df[, paste0(txt_var_pfx, ".P.verbatim.colon")] <-
                as.integer(0 + mycount_pattern_occ("Verbatim:", glb_allobs_df[, txt_var]))
            txt_X_df[, paste0(txt_var_pfx, ".P.what.we.are")] <-
                as.integer(0 + mycount_pattern_occ("What We're", glb_allobs_df[, txt_var]))
        }

#summary(glb_allobs_df[ ,grep("P.on.this.day", names(glb_allobs_df), value=TRUE)])
        txt_X_df <- subset(txt_X_df, select=-.rnorm)
        txt_X_df <- txt_X_df[, -grep(glb_id_vars, names(txt_X_df), fixed=TRUE), FALSE]
        #glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    }
    glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    #myplot_box(glb_allobs_df, "A.sum.TfIdf", glb_rsp_var)

    # Generate summaries
#     print(summary(glb_allobs_df))
#     print(sapply(names(glb_allobs_df), function(col) sum(is.na(glb_allobs_df[, col]))))
#     print(summary(glb_trnobs_df))
#     print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
#     print(summary(glb_newobs_df))
#     print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

    rm(log_X_df, txt_X_df)
}

# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

# print(myplot_scatter(glb_trnobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))

rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr, 
   glb_full_DTM_lst, glb_sprs_DTM_lst, txt_corpus, txt_vctr)
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'corpus_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_DTM' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_vctr' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'glb_full_DTM_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'glb_sprs_DTM_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'txt_corpus' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'txt_vctr' not found
```

```r
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, "extract.features_end", 
                                     major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 26.326 26.342
## 3                extract.features_end          3          0 26.342     NA
##   elapsed
## 2   0.016
## 3      NA
```

```r
myplt_chunk(extract.features_chunk_df)
```

```
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 26.326 26.342
## 1                extract.features_bgn          1          0 26.318 26.325
##   elapsed duration
## 2   0.016    0.016
## 1   0.007    0.007
## [1] "Total Elapsed Time: 26.342 secs"
```

![](WHO2_files/figure-html/extract.features-1.png) 

```r
# if (glb_save_envir)
#     save(glb_feats_df, 
#          glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
#          file=paste0(glb_out_pfx, "extract_features_dsk.RData"))
# load(paste0(glb_out_pfx, "extract_features_dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all","data.new")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0
```

![](WHO2_files/figure-html/extract.features-2.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cluster.data", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 6 extract.features          3          0 26.277 27.379   1.102
## 7     cluster.data          4          0 27.379     NA      NA
```

## Step `4.0: cluster data`

```r
if (glb_cluster) {
    require(proxy)
    #require(hash)
    require(dynamicTreeCut)

#     glb_hash <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#     glb_hash_lst <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#stophere; sav_allobs_df <- glb_allobs_df; 
    print("Clustering features: ")
    print(cluster_vars <- grep("[HSA]\\.[PT]\\.", names(glb_allobs_df), value=TRUE))
    #print(cluster_vars <- grep("[HSA]\\.", names(glb_allobs_df), value=TRUE))
    glb_allobs_df$.clusterid <- 1    
    #print(max(table(glb_allobs_df$myCategory.fctr) / 20))
    for (myCategory in c("##", "Business#Business Day#Dealbook", "OpEd#Opinion#", 
                         "Styles#U.S.#", "Business#Technology#", "Science#Health#",
                         "Culture#Arts#")) {
        ctgry_allobs_df <- glb_allobs_df[glb_allobs_df$myCategory == myCategory, ]
        
        dstns_dist <- dist(ctgry_allobs_df[, cluster_vars], method = "cosine")
        dstns_mtrx <- as.matrix(dstns_dist)
        print(sprintf("max distance(%0.4f) pair:", max(dstns_mtrx)))
        row_ix <- ceiling(which.max(dstns_mtrx) / ncol(dstns_mtrx))
        col_ix <- which.max(dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])
    
        min_dstns_mtrx <- dstns_mtrx
        diag(min_dstns_mtrx) <- 1
        print(sprintf("min distance(%0.4f) pair:", min(min_dstns_mtrx)))
        row_ix <- ceiling(which.min(min_dstns_mtrx) / ncol(min_dstns_mtrx))
        col_ix <- which.min(min_dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])                          
    
        clusters <- hclust(dstns_dist, method = "ward.D2")
        #plot(clusters, labels=NULL, hang=-1)
        myplclust(clusters, lab.col=unclass(ctgry_allobs_df[, glb_rsp_var]))
        
        #clusterGroups = cutree(clusters, k=7)
        clusterGroups <- cutreeDynamic(clusters, minClusterSize=20, method="tree", deepSplit=0)
        # Unassigned groups are labeled 0; the largest group has label 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")   
        #print(ctgry_allobs_df[which(clusterGroups == 1), c("UniqueID", "Popular", "Headline")])
        #print(ctgry_allobs_df[(clusterGroups == 1) & !is.na(ctgry_allobs_df$Popular) & (ctgry_allobs_df$Popular==1), c("UniqueID", "Popular", "Headline")])
        clusterGroups[clusterGroups == 0] <- 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
        #summary(factor(clusterGroups))
#         clusterGroups <- clusterGroups + 
#                 100 * # has to be > max(table(glb_allobs_df$myCategory.fctr) / minClusterSize=20)
#                             which(levels(glb_allobs_df$myCategory.fctr) == myCategory)
#         table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
    
        # add to glb_allobs_df - then split the data again
        glb_allobs_df[glb_allobs_df$myCategory==myCategory,]$.clusterid <- clusterGroups
        #print(unique(glb_allobs_df$.clusterid))
        #print(glb_feats_df[glb_feats_df$id == ".clusterid.fctr", ])
    }
    
    ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
                              mycreate_sqlxtab_df(glb_allobs_df,
        c("myCategory", ".clusterid", glb_rsp_var)))
    ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
                           myCategory + .clusterid ~ 
                               Popular.fctr, sum, value.var=".n"))
    print(ctgry_cast_df)
    #print(orderBy(~ myCategory -Y -NA, ctgry_cast_df))
    # write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_clst.csv"), 
    #             row.names=FALSE)
    
    print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df$.clusterid, 
                                 glb_allobs_df[, glb_rsp_var], 
                                 useNA="ifany"))
#     dsp_obs(.clusterid=1, myCategory="OpEd#Opinion#", 
#             cols=c("UniqueID", "Popular", "myCategory", ".clusterid", "Headline"),
#             all=TRUE)
    
    glb_allobs_df$.clusterid.fctr <- as.factor(glb_allobs_df$.clusterid)
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      ".clusterid")
    glb_interaction_only_features["myCategory.fctr"] <- c(".clusterid.fctr")
}

# Re-partition
glb_trnobs_df <- subset(glb_allobs_df, .src == "Train")
glb_newobs_df <- subset(glb_allobs_df, .src == "Test")

glb_chunks_df <- myadd_chunk(glb_chunks_df, "select.features", major.inc=TRUE)
```

```
##             label step_major step_minor    bgn    end elapsed
## 7    cluster.data          4          0 27.379 27.658   0.279
## 8 select.features          5          0 27.658     NA      NA
```

## Step `5.0: select features`

```r
print(glb_feats_df <- myselect_features(entity_df=glb_trnobs_df, 
                       exclude_vars_as_features=glb_exclude_vars_as_features, 
                       rsp_var=glb_rsp_var))
```

```
##                                                                      id
## ChildMortality                                           ChildMortality
## Under15                                                         Under15
## FertilityRate                                             FertilityRate
## FertilityRate.nonNA                                 FertilityRate.nonNA
## LiteracyRate                                               LiteracyRate
## PrimarySchoolEnrollmentFemale             PrimarySchoolEnrollmentFemale
## Over60                                                           Over60
## GNI                                                                 GNI
## LiteracyRate.nonNA                                   LiteracyRate.nonNA
## PrimarySchoolEnrollmentFemale.nonNA PrimarySchoolEnrollmentFemale.nonNA
## GNI.nonNA                                                     GNI.nonNA
## CellularSubscribers                                 CellularSubscribers
## PrimarySchoolEnrollmentMale                 PrimarySchoolEnrollmentMale
## CellularSubscribers.nonNA                     CellularSubscribers.nonNA
## PrimarySchoolEnrollmentMale.nonNA     PrimarySchoolEnrollmentMale.nonNA
## Region.fctr                                                 Region.fctr
## .rnorm                                                           .rnorm
## Population                                                   Population
##                                            cor.y exclude.as.feat
## ChildMortality                      -0.930540154               0
## Under15                             -0.828267850               0
## FertilityRate                       -0.821220176               1
## FertilityRate.nonNA                 -0.812058404               0
## LiteracyRate                         0.724901262               1
## PrimarySchoolEnrollmentFemale        0.715035749               1
## Over60                               0.693014776               0
## GNI                                  0.673926784               1
## LiteracyRate.nonNA                   0.665663573               0
## PrimarySchoolEnrollmentFemale.nonNA  0.650665778               0
## GNI.nonNA                            0.631623396               0
## CellularSubscribers                  0.628898883               1
## PrimarySchoolEnrollmentMale          0.623339427               1
## CellularSubscribers.nonNA            0.621594090               0
## PrimarySchoolEnrollmentMale.nonNA    0.519297226               0
## Region.fctr                         -0.453295147               0
## .rnorm                              -0.033249865               0
## Population                           0.007149689               0
##                                       cor.y.abs
## ChildMortality                      0.930540154
## Under15                             0.828267850
## FertilityRate                       0.821220176
## FertilityRate.nonNA                 0.812058404
## LiteracyRate                        0.724901262
## PrimarySchoolEnrollmentFemale       0.715035749
## Over60                              0.693014776
## GNI                                 0.673926784
## LiteracyRate.nonNA                  0.665663573
## PrimarySchoolEnrollmentFemale.nonNA 0.650665778
## GNI.nonNA                           0.631623396
## CellularSubscribers                 0.628898883
## PrimarySchoolEnrollmentMale         0.623339427
## CellularSubscribers.nonNA           0.621594090
## PrimarySchoolEnrollmentMale.nonNA   0.519297226
## Region.fctr                         0.453295147
## .rnorm                              0.033249865
## Population                          0.007149689
```

```r
# sav_feats_df <- glb_feats_df; glb_feats_df <- sav_feats_df
print(glb_feats_df <- orderBy(~-cor.y, 
          myfind_cor_features(feats_df=glb_feats_df, obs_df=glb_trnobs_df, 
                              rsp_var=glb_rsp_var)))
```

```
## Loading required package: reshape2
```

```
## [1] "cor(FertilityRate.nonNA, Under15)=0.9261"
## [1] "cor(LifeExpectancy, FertilityRate.nonNA)=-0.8121"
## [1] "cor(LifeExpectancy, Under15)=-0.8283"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified FertilityRate.nonNA as highly correlated with
## Under15
```

```
## [1] "cor(PrimarySchoolEnrollmentFemale.nonNA, PrimarySchoolEnrollmentMale.nonNA)=0.9117"
## [1] "cor(LifeExpectancy, PrimarySchoolEnrollmentFemale.nonNA)=0.6507"
## [1] "cor(LifeExpectancy, PrimarySchoolEnrollmentMale.nonNA)=0.5193"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified PrimarySchoolEnrollmentMale.nonNA as highly
## correlated with PrimarySchoolEnrollmentFemale.nonNA
```

```
## [1] "cor(Over60, Under15)=-0.8250"
## [1] "cor(LifeExpectancy, Over60)=0.6930"
## [1] "cor(LifeExpectancy, Under15)=-0.8283"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified Over60 as highly correlated with Under15
```

```
## [1] "cor(ChildMortality, Under15)=0.8084"
## [1] "cor(LifeExpectancy, ChildMortality)=-0.9305"
## [1] "cor(LifeExpectancy, Under15)=-0.8283"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df
## = glb_trnobs_df, : Identified Under15 as highly correlated with
## ChildMortality
```

```
## [1] "cor(ChildMortality, LiteracyRate.nonNA)=-0.7532"
## [1] "cor(LifeExpectancy, ChildMortality)=-0.9305"
## [1] "cor(LifeExpectancy, LiteracyRate.nonNA)=0.6657"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified LiteracyRate.nonNA as highly correlated with
## ChildMortality
```

```
## [1] "cor(ChildMortality, PrimarySchoolEnrollmentFemale.nonNA)=-0.7005"
## [1] "cor(LifeExpectancy, ChildMortality)=-0.9305"
## [1] "cor(LifeExpectancy, PrimarySchoolEnrollmentFemale.nonNA)=0.6507"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified PrimarySchoolEnrollmentFemale.nonNA as highly
## correlated with ChildMortality
```

```
##                                                                      id
## LiteracyRate                                               LiteracyRate
## PrimarySchoolEnrollmentFemale             PrimarySchoolEnrollmentFemale
## Over60                                                           Over60
## GNI                                                                 GNI
## LiteracyRate.nonNA                                   LiteracyRate.nonNA
## PrimarySchoolEnrollmentFemale.nonNA PrimarySchoolEnrollmentFemale.nonNA
## GNI.nonNA                                                     GNI.nonNA
## CellularSubscribers                                 CellularSubscribers
## PrimarySchoolEnrollmentMale                 PrimarySchoolEnrollmentMale
## CellularSubscribers.nonNA                     CellularSubscribers.nonNA
## PrimarySchoolEnrollmentMale.nonNA     PrimarySchoolEnrollmentMale.nonNA
## Population                                                   Population
## .rnorm                                                           .rnorm
## Region.fctr                                                 Region.fctr
## FertilityRate.nonNA                                 FertilityRate.nonNA
## FertilityRate                                             FertilityRate
## Under15                                                         Under15
## ChildMortality                                           ChildMortality
##                                            cor.y exclude.as.feat
## LiteracyRate                         0.724901262               1
## PrimarySchoolEnrollmentFemale        0.715035749               1
## Over60                               0.693014776               0
## GNI                                  0.673926784               1
## LiteracyRate.nonNA                   0.665663573               0
## PrimarySchoolEnrollmentFemale.nonNA  0.650665778               0
## GNI.nonNA                            0.631623396               0
## CellularSubscribers                  0.628898883               1
## PrimarySchoolEnrollmentMale          0.623339427               1
## CellularSubscribers.nonNA            0.621594090               0
## PrimarySchoolEnrollmentMale.nonNA    0.519297226               0
## Population                           0.007149689               0
## .rnorm                              -0.033249865               0
## Region.fctr                         -0.453295147               0
## FertilityRate.nonNA                 -0.812058404               0
## FertilityRate                       -0.821220176               1
## Under15                             -0.828267850               0
## ChildMortality                      -0.930540154               0
##                                       cor.y.abs
## LiteracyRate                        0.724901262
## PrimarySchoolEnrollmentFemale       0.715035749
## Over60                              0.693014776
## GNI                                 0.673926784
## LiteracyRate.nonNA                  0.665663573
## PrimarySchoolEnrollmentFemale.nonNA 0.650665778
## GNI.nonNA                           0.631623396
## CellularSubscribers                 0.628898883
## PrimarySchoolEnrollmentMale         0.623339427
## CellularSubscribers.nonNA           0.621594090
## PrimarySchoolEnrollmentMale.nonNA   0.519297226
## Population                          0.007149689
## .rnorm                              0.033249865
## Region.fctr                         0.453295147
## FertilityRate.nonNA                 0.812058404
## FertilityRate                       0.821220176
## Under15                             0.828267850
## ChildMortality                      0.930540154
##                                                              cor.high.X
## LiteracyRate                                                       <NA>
## PrimarySchoolEnrollmentFemale                                      <NA>
## Over60                                                          Under15
## GNI                                                                <NA>
## LiteracyRate.nonNA                                       ChildMortality
## PrimarySchoolEnrollmentFemale.nonNA                      ChildMortality
## GNI.nonNA                                                          <NA>
## CellularSubscribers                                                <NA>
## PrimarySchoolEnrollmentMale                                        <NA>
## CellularSubscribers.nonNA                                          <NA>
## PrimarySchoolEnrollmentMale.nonNA   PrimarySchoolEnrollmentFemale.nonNA
## Population                                                         <NA>
## .rnorm                                                             <NA>
## Region.fctr                                                        <NA>
## FertilityRate.nonNA                                             Under15
## FertilityRate                                                      <NA>
## Under15                                                  ChildMortality
## ChildMortality                                                     <NA>
##                                     freqRatio percentUnique zeroVar   nzv
## LiteracyRate                         1.000000     44.604317   FALSE FALSE
## PrimarySchoolEnrollmentFemale        1.000000     39.568345   FALSE FALSE
## Over60                               2.000000     93.525180   FALSE FALSE
## GNI                                  1.000000     79.856115   FALSE FALSE
## LiteracyRate.nonNA                   1.000000     50.359712   FALSE FALSE
## PrimarySchoolEnrollmentFemale.nonNA  1.000000     46.762590   FALSE FALSE
## GNI.nonNA                            1.500000     84.892086   FALSE FALSE
## CellularSubscribers                  1.000000     93.525180   FALSE FALSE
## PrimarySchoolEnrollmentMale          1.000000     42.446043   FALSE FALSE
## CellularSubscribers.nonNA            1.000000     94.244604   FALSE FALSE
## PrimarySchoolEnrollmentMale.nonNA    1.000000     48.201439   FALSE FALSE
## Population                           1.000000     98.561151   FALSE FALSE
## .rnorm                               1.000000    100.000000   FALSE FALSE
## Region.fctr                          1.181818      4.316547   FALSE FALSE
## FertilityRate.nonNA                  1.333333     79.136691   FALSE FALSE
## FertilityRate                        1.000000     78.417266   FALSE FALSE
## Under15                              2.000000     94.244604   FALSE FALSE
## ChildMortality                       1.500000     91.366906   FALSE FALSE
##                                     myNearZV is.cor.y.abs.low
## LiteracyRate                           FALSE            FALSE
## PrimarySchoolEnrollmentFemale          FALSE            FALSE
## Over60                                 FALSE            FALSE
## GNI                                    FALSE            FALSE
## LiteracyRate.nonNA                     FALSE            FALSE
## PrimarySchoolEnrollmentFemale.nonNA    FALSE            FALSE
## GNI.nonNA                              FALSE            FALSE
## CellularSubscribers                    FALSE            FALSE
## PrimarySchoolEnrollmentMale            FALSE            FALSE
## CellularSubscribers.nonNA              FALSE            FALSE
## PrimarySchoolEnrollmentMale.nonNA      FALSE            FALSE
## Population                             FALSE             TRUE
## .rnorm                                 FALSE            FALSE
## Region.fctr                            FALSE            FALSE
## FertilityRate.nonNA                    FALSE            FALSE
## FertilityRate                          FALSE            FALSE
## Under15                                FALSE            FALSE
## ChildMortality                         FALSE            FALSE
```

```r
#subset(glb_feats_df, id %in% c("A.nuppr.log", "S.nuppr.log"))
print(myplot_scatter(glb_feats_df, "percentUnique", "freqRatio", 
                     colorcol_name="myNearZV", jitter=TRUE) + 
          geom_point(aes(shape=nzv)) + xlim(-5, 25))
```

```
## Warning in myplot_scatter(glb_feats_df, "percentUnique", "freqRatio",
## colorcol_name = "myNearZV", : converting myNearZV to class:factor
```

```
## Warning in loop_apply(n, do.ply): Removed 17 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 17 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 17 rows containing missing values
## (geom_point).
```

![](WHO2_files/figure-html/select.features-1.png) 

```r
print(subset(glb_feats_df, myNearZV))
```

```
##  [1] id               cor.y            exclude.as.feat  cor.y.abs       
##  [5] cor.high.X       freqRatio        percentUnique    zeroVar         
##  [9] nzv              myNearZV         is.cor.y.abs.low
## <0 rows> (or 0-length row.names)
```

```r
glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                         subset(glb_feats_df, myNearZV)$id)]

if (!is.null(glb_interaction_only_features))
    glb_feats_df[glb_feats_df$id %in% glb_interaction_only_features, "interaction.feat"] <-
        names(glb_interaction_only_features) else
    glb_feats_df$interaction.feat <- NA        

glb_chunks_df <- myadd_chunk(glb_chunks_df, "partition.data.training", major.inc=TRUE)
```

```
##                     label step_major step_minor    bgn    end elapsed
## 8         select.features          5          0 27.658 28.272   0.614
## 9 partition.data.training          6          0 28.272     NA      NA
```

## Step `6.0: partition data training`

```r
if (all(is.na(glb_newobs_df[, glb_rsp_var]))) {
    require(caTools)
    
    set.seed(glb_split_sample.seed)
    split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
        SplitRatio=1 - (nrow(glb_newobs_df) * 1.1 / nrow(glb_trnobs_df)))
    glb_fitobs_df <- glb_trnobs_df[split, ] 
    glb_OOBobs_df <- glb_trnobs_df[!split ,]    
} else {
    print(sprintf("Newdata contains non-NA data for %s; setting OOB to Newdata", 
                  glb_rsp_var))
    glb_fitobs_df <- glb_trnobs_df; glb_OOBobs_df <- glb_newobs_df
}
```

```
## [1] "Newdata contains non-NA data for LifeExpectancy; setting OOB to Newdata"
```

```r
if (!is.null(glb_max_fitent_obs) && (nrow(glb_fitobs_df) > glb_max_fitent_obs)) {
    warning("glb_fitobs_df restricted to glb_max_fitent_obs: ", 
            format(glb_max_fitent_obs, big.mark=","))
    org_fitent_df <- glb_fitobs_df
    glb_fitobs_df <- 
        org_fitent_df[split <- sample.split(org_fitent_df[, glb_rsp_var_raw], 
                                            SplitRatio=glb_max_fitent_obs), ]
    org_fitent_df <- NULL
}

glb_allobs_df$.lcn <- ""
glb_allobs_df[glb_allobs_df[, glb_id_vars] %in% 
              glb_fitobs_df[, glb_id_vars], ".lcn"] <- "Fit"
glb_allobs_df[glb_allobs_df[, glb_id_vars] %in% 
              glb_OOBobs_df[, glb_id_vars], ".lcn"] <- "OOB"

dsp_class_dstrb <- function(obs_df, location_var, partition_var) {
    xtab_df <- mycreate_xtab_df(obs_df, c(location_var, partition_var))
    rownames(xtab_df) <- xtab_df[, location_var]
    xtab_df <- xtab_df[, -grepl(location_var, names(xtab_df))]
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Ensure proper splits by glb_rsp_var_raw & user-specified feature for OOB vs. new
if (!is.null(glb_category_vars)) {
    dsp_class_dstrb(glb_allobs_df, ".lcn", glb_rsp_var_raw)
    newent_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .src == "Test"), 
                                           "myCategory")
    OOBobs_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .lcn == "OOB"), 
                                           "myCategory")
    glb_ctgry_df <- merge(newent_ctgry_df, OOBobs_ctgry_df, by="myCategory", all=TRUE, 
                          suffixes=c(".Tst", ".OOB"))
    glb_ctgry_df$.freqRatio.Tst <- glb_ctgry_df$.n.Tst / sum(glb_ctgry_df$.n.Tst, na.rm=TRUE)
    glb_ctgry_df$.freqRatio.OOB <- glb_ctgry_df$.n.OOB / sum(glb_ctgry_df$.n.OOB, na.rm=TRUE)
    print(orderBy(~-.freqRatio.Tst-.freqRatio.OOB, glb_ctgry_df))
}

# Run this line by line
print("glb_feats_df:");   print(dim(glb_feats_df))
```

```
## [1] "glb_feats_df:"
```

```
## [1] 18 12
```

```r
sav_feats_df <- glb_feats_df
glb_feats_df <- sav_feats_df

glb_feats_df[, "rsp_var_raw"] <- FALSE
glb_feats_df[glb_feats_df$id == glb_rsp_var_raw, "rsp_var_raw"] <- TRUE 
glb_feats_df$exclude.as.feat <- (glb_feats_df$exclude.as.feat == 1)
if (!is.null(glb_id_vars) && glb_id_vars != ".rownames")
    glb_feats_df[glb_feats_df$id %in% glb_id_vars, "id_var"] <- TRUE 
add_feats_df <- data.frame(id=glb_rsp_var, exclude.as.feat=TRUE, rsp_var=TRUE)
row.names(add_feats_df) <- add_feats_df$id; print(add_feats_df)
```

```
##                            id exclude.as.feat rsp_var
## LifeExpectancy LifeExpectancy            TRUE    TRUE
```

```r
glb_feats_df <- myrbind_df(glb_feats_df, add_feats_df)
print(subset(glb_feats_df, rsp_var_raw | rsp_var | id_var))
```

```
##                            id cor.y exclude.as.feat cor.y.abs cor.high.X
## LifeExpectancy LifeExpectancy    NA            TRUE        NA       <NA>
##                freqRatio percentUnique zeroVar nzv myNearZV
## LifeExpectancy        NA            NA      NA  NA       NA
##                is.cor.y.abs.low interaction.feat rsp_var_raw id_var
## LifeExpectancy               NA               NA          NA     NA
##                rsp_var
## LifeExpectancy    TRUE
```

```r
print("glb_feats_df vs. glb_allobs_df: "); 
```

```
## [1] "glb_feats_df vs. glb_allobs_df: "
```

```r
print(setdiff(glb_feats_df$id, names(glb_allobs_df)))
```

```
## character(0)
```

```r
print("glb_allobs_df vs. glb_feats_df: "); 
```

```
## [1] "glb_allobs_df vs. glb_feats_df: "
```

```r
# Ensure these are only chr vars
print(setdiff(setdiff(names(glb_allobs_df), glb_feats_df$id), 
                myfind_chr_cols_df(glb_allobs_df)))
```

```
## character(0)
```

```r
#print(setdiff(setdiff(names(glb_allobs_df), glb_exclude_vars_as_features), 
#                glb_feats_df$id))

print("glb_allobs_df: "); print(dim(glb_allobs_df))
```

```
## [1] "glb_allobs_df: "
```

```
## [1] 194  23
```

```r
print("glb_trnobs_df: "); print(dim(glb_trnobs_df))
```

```
## [1] "glb_trnobs_df: "
```

```
## [1] 139  22
```

```r
print("glb_fitobs_df: "); print(dim(glb_fitobs_df))
```

```
## [1] "glb_fitobs_df: "
```

```
## [1] 139  22
```

```r
print("glb_OOBobs_df: "); print(dim(glb_OOBobs_df))
```

```
## [1] "glb_OOBobs_df: "
```

```
## [1] 55 22
```

```r
print("glb_newobs_df: "); print(dim(glb_newobs_df))
```

```
## [1] "glb_newobs_df: "
```

```
## [1] 55 22
```

```r
# # Does not handle NULL or length(glb_id_vars) > 1
# glb_allobs_df$.src.trn <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_vars] %in% glb_trnobs_df[, glb_id_vars], 
#                 ".src.trn"] <- 1 
# glb_allobs_df$.src.fit <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_vars] %in% glb_fitobs_df[, glb_id_vars], 
#                 ".src.fit"] <- 1 
# glb_allobs_df$.src.OOB <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_vars] %in% glb_OOBobs_df[, glb_id_vars], 
#                 ".src.OOB"] <- 1 
# glb_allobs_df$.src.new <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_vars] %in% glb_newobs_df[, glb_id_vars], 
#                 ".src.new"] <- 1 
# #print(unique(glb_allobs_df[, ".src.trn"]))
# write_cols <- c(glb_feats_df$id, 
#                 ".src.trn", ".src.fit", ".src.OOB", ".src.new")
# glb_allobs_df <- glb_allobs_df[, write_cols]
# 
# tmp_feats_df <- glb_feats_df
# tmp_entity_df <- glb_allobs_df

if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         file=paste0(glb_out_pfx, "blddfs_dsk.RData"))
# load(paste0(glb_out_pfx, "blddfs_dsk.RData"))

# if (!all.equal(tmp_feats_df, glb_feats_df))
#     stop("glb_feats_df r/w not working")
# if (!all.equal(tmp_entity_df, glb_allobs_df))
#     stop("glb_allobs_df r/w not working")

rm(split)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=TRUE)
```

```
##                      label step_major step_minor    bgn    end elapsed
## 9  partition.data.training          6          0 28.272 28.564   0.292
## 10              fit.models          7          0 28.564     NA      NA
```

## Step `7.0: fit models`

```r
# load(paste0(glb_out_pfx, "dsk.RData"))
# keep_cols <- setdiff(names(glb_allobs_df), 
#                      grep("^.src", names(glb_allobs_df), value=TRUE))
# glb_trnobs_df <- glb_allobs_df[glb_allobs_df$.src.trn == 1, keep_cols]
# glb_fitobs_df <- glb_allobs_df[glb_allobs_df$.src.fit == 1, keep_cols]
# glb_OOBobs_df <- glb_allobs_df[glb_allobs_df$.src.OOB == 1, keep_cols]
# glb_newobs_df <- glb_allobs_df[glb_allobs_df$.src.new == 1, keep_cols]
# 
# glb_models_lst <- list(); glb_models_df <- data.frame()
# 
if (glb_is_classification && glb_is_binomial && 
        (length(unique(glb_fitobs_df[, glb_rsp_var])) < 2))
    stop("glb_fitobs_df$", glb_rsp_var, ": contains less than 2 unique values: ",
         paste0(unique(glb_fitobs_df[, glb_rsp_var]), collapse=", "))

max_cor_y_x_vars <- orderBy(~ -cor.y.abs, 
        subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low & 
                                is.na(cor.high.X)))[1:2, "id"]
# while(length(max_cor_y_x_vars) < 2) {
#     max_cor_y_x_vars <- c(max_cor_y_x_vars, orderBy(~ -cor.y.abs, 
#             subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low))[3, "id"])    
# }
if (!is.null(glb_Baseline_mdl_var)) {
    if ((max_cor_y_x_vars[1] != glb_Baseline_mdl_var) & 
        (glb_feats_df[max_cor_y_x_vars[1], "cor.y.abs"] > 
         glb_feats_df[glb_Baseline_mdl_var, "cor.y.abs"]))
        stop(max_cor_y_x_vars[1], " has a lower correlation with ", glb_rsp_var, 
             " than the Baseline var: ", glb_Baseline_mdl_var)
}

glb_model_type <- ifelse(glb_is_regression, "regression", "classification")
    
# Baseline
if (!is.null(glb_Baseline_mdl_var)) 
    ret_lst <- myfit_mdl_fn(model_id="Baseline", model_method="mybaseln_classfr",
                            indep_vars_vctr=glb_Baseline_mdl_var,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)

# Most Frequent Outcome "MFO" model: mean(y) for regression
#   Not using caret's nullModel since model stats not avl
#   Cannot use rpart for multinomial classification since it predicts non-MFO
ret_lst <- myfit_mdl(model_id="MFO", 
                     model_method=ifelse(glb_is_regression, "lm", "myMFO_classfr"), 
                     model_type=glb_model_type,
                        indep_vars_vctr=".rnorm",
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: MFO.lm"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
```

![](WHO2_files/figure-html/fit.models_0-1.png) ![](WHO2_files/figure-html/fit.models_0-2.png) ![](WHO2_files/figure-html/fit.models_0-3.png) ![](WHO2_files/figure-html/fit.models_0-4.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -22.571  -6.205   2.162   6.734  13.581 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  69.6506     0.8037  86.665   <2e-16 ***
## .rnorm       -0.3269     0.8396  -0.389    0.698    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.443 on 137 degrees of freedom
## Multiple R-squared:  0.001106,	Adjusted R-squared:  -0.006186 
## F-statistic: 0.1516 on 1 and 137 DF,  p-value: 0.6976
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##   model_id model_method  feats max.nTuningRuns min.elapsedtime.everything
## 1   MFO.lm           lm .rnorm               0                      0.438
##   min.elapsedtime.final max.R.sq.fit min.RMSE.fit max.R.sq.OOB
## 1                 0.004  0.001105554     9.375044  0.009390806
##   min.RMSE.OOB max.Adj.R.sq.fit
## 1     8.838414     -0.006185647
```

```r
if (glb_is_classification)
    # "random" model - only for classification; 
    #   none needed for regression since it is same as MFO
    ret_lst <- myfit_mdl(model_id="Random", model_method="myrandom_classfr",
                            model_type=glb_model_type,                         
                            indep_vars_vctr=".rnorm",
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)

# Any models that have tuning parameters has "better" results with cross-validation
#   (except rf) & "different" results for different outcome metrics

# Max.cor.Y
#   Check impact of cv
#       rpart is not a good candidate since caret does not optimize cp (only tuning parameter of rpart) well
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: Max.cor.Y.cv.0.rpart"
## [1] "    indep_vars: ChildMortality, GNI.nonNA"
```

```
## Loading required package: rpart
```

```
## Fitting cp = 0.698 on full training set
```

```
## Loading required package: rpart.plot
```

![](WHO2_files/figure-html/fit.models_0-5.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 139 
## 
##          CP nsplit rel error
## 1 0.6980808      0         1
## 
## Node number 1: 139 observations
##   mean=69.67626, MSE=87.98872 
## 
## n= 139 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 139 12230.43 69.67626 *
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##               model_id model_method                     feats
## 1 Max.cor.Y.cv.0.rpart        rpart ChildMortality, GNI.nonNA
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      0.511                 0.009
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB
## 1            0      9.38023            0     8.880209
```

```r
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0.cp.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=0, 
            tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
```

```
## [1] "fitting model: Max.cor.Y.cv.0.cp.0.rpart"
## [1] "    indep_vars: ChildMortality, GNI.nonNA"
## Fitting cp = 0 on full training set
```

![](WHO2_files/figure-html/fit.models_0-6.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 139 
## 
##              CP nsplit  rel error
## 1  0.6980807729      0 1.00000000
## 2  0.1034320249      1 0.30191923
## 3  0.0796607700      2 0.19848720
## 4  0.0176912163      3 0.11882643
## 5  0.0173954340      4 0.10113522
## 6  0.0114193900      5 0.08373978
## 7  0.0079384852      6 0.07232039
## 8  0.0017231544      7 0.06438191
## 9  0.0009712485      8 0.06265875
## 10 0.0007929829      9 0.06168750
## 11 0.0007648582     10 0.06089452
## 12 0.0000000000     11 0.06012966
## 
## Variable importance
## ChildMortality      GNI.nonNA 
##             61             39 
## 
## Node number 1: 139 observations,    complexity param=0.6980808
##   mean=69.67626, MSE=87.98872 
##   left son=2 (50 obs) right son=3 (89 obs)
##   Primary splits:
##       ChildMortality < 37.5  to the right, improve=0.6980808, (0 missing)
##       GNI.nonNA      < 3245  to the left,  improve=0.4836100, (0 missing)
##   Surrogate splits:
##       GNI.nonNA < 3615  to the left,  agree=0.899, adj=0.72, (0 split)
## 
## Node number 2: 50 observations,    complexity param=0.103432
##   mean=59.22, MSE=38.4916 
##   left son=4 (22 obs) right son=5 (28 obs)
##   Primary splits:
##       ChildMortality < 78.65 to the right, improve=0.6572958, (0 missing)
##       GNI.nonNA      < 1075  to the left,  improve=0.1047542, (0 missing)
##   Surrogate splits:
##       GNI.nonNA < 1075  to the left,  agree=0.6, adj=0.091, (0 split)
## 
## Node number 3: 89 observations,    complexity param=0.07966077
##   mean=75.55056, MSE=19.86542 
##   left son=6 (44 obs) right son=7 (45 obs)
##   Primary splits:
##       ChildMortality < 10.65 to the right, improve=0.5510595, (0 missing)
##       GNI.nonNA      < 30695 to the left,  improve=0.4616534, (0 missing)
##   Surrogate splits:
##       GNI.nonNA < 14400 to the left,  agree=0.82, adj=0.636, (0 split)
## 
## Node number 4: 22 observations,    complexity param=0.01141939
##   mean=53.54545, MSE=13.15702 
##   left son=8 (7 obs) right son=9 (15 obs)
##   Primary splits:
##       ChildMortality < 118.6 to the right, improve=0.4825078, (0 missing)
##       GNI.nonNA      < 1090  to the left,  improve=0.1211919, (0 missing)
##   Surrogate splits:
##       GNI.nonNA < 905   to the left,  agree=0.773, adj=0.286, (0 split)
## 
## Node number 5: 28 observations,    complexity param=0.007938485
##   mean=63.67857, MSE=13.21811 
##   left son=10 (11 obs) right son=11 (17 obs)
##   Primary splits:
##       GNI.nonNA      < 1760  to the left,  improve=0.2623324, (0 missing)
##       ChildMortality < 61.45 to the right, improve=0.2582028, (0 missing)
##   Surrogate splits:
##       ChildMortality < 65.65 to the right, agree=0.714, adj=0.273, (0 split)
## 
## Node number 6: 44 observations,    complexity param=0.01769122
##   mean=72.20455, MSE=9.617252 
##   left son=12 (11 obs) right son=13 (33 obs)
##   Primary splits:
##       ChildMortality < 25.5  to the right, improve=0.5113236, (0 missing)
##       GNI.nonNA      < 4825  to the left,  improve=0.1726347, (0 missing)
##   Surrogate splits:
##       GNI.nonNA < 2800  to the left,  agree=0.818, adj=0.273, (0 split)
## 
## Node number 7: 45 observations,    complexity param=0.01739543
##   mean=78.82222, MSE=8.235062 
##   left son=14 (23 obs) right son=15 (22 obs)
##   Primary splits:
##       GNI.nonNA      < 25905 to the left,  improve=0.5741134, (0 missing)
##       ChildMortality < 5.8   to the right, improve=0.5238786, (0 missing)
##   Surrogate splits:
##       ChildMortality < 5.4   to the right, agree=0.8, adj=0.591, (0 split)
## 
## Node number 8: 7 observations
##   mean=49.85714, MSE=3.55102 
## 
## Node number 9: 15 observations
##   mean=55.26667, MSE=8.328889 
## 
## Node number 10: 11 observations
##   mean=61.36364, MSE=11.86777 
## 
## Node number 11: 17 observations
##   mean=65.17647, MSE=8.380623 
## 
## Node number 12: 11 observations
##   mean=68.36364, MSE=6.049587 
## 
## Node number 13: 33 observations,    complexity param=0.0009712485
##   mean=73.48485, MSE=4.24977 
##   left son=26 (11 obs) right son=27 (22 obs)
##   Primary splits:
##       ChildMortality < 18.55 to the right, improve=0.08470182, (0 missing)
##       GNI.nonNA      < 5175  to the left,  improve=0.05906979, (0 missing)
##   Surrogate splits:
##       GNI.nonNA < 4125  to the left,  agree=0.697, adj=0.091, (0 split)
## 
## Node number 14: 23 observations,    complexity param=0.001723154
##   mean=76.69565, MSE=4.646503 
##   left son=28 (16 obs) right son=29 (7 obs)
##   Primary splits:
##       ChildMortality < 6.05  to the right, improve=0.1972023, (0 missing)
##       GNI.nonNA      < 10140 to the right, improve=0.1270233, (0 missing)
##   Surrogate splits:
##       GNI.nonNA < 18330 to the left,  agree=0.783, adj=0.286, (0 split)
## 
## Node number 15: 22 observations,    complexity param=0.0007648582
##   mean=81.04545, MSE=2.316116 
##   left son=30 (15 obs) right son=31 (7 obs)
##   Primary splits:
##       GNI.nonNA      < 50695 to the left,  improve=0.1835861, (0 missing)
##       ChildMortality < 3.5   to the right, improve=0.1224672, (0 missing)
##   Surrogate splits:
##       ChildMortality < 2.85  to the right, agree=0.727, adj=0.143, (0 split)
## 
## Node number 26: 11 observations
##   mean=72.63636, MSE=5.140496 
## 
## Node number 27: 22 observations,    complexity param=0.0007929829
##   mean=73.90909, MSE=3.264463 
##   left son=54 (9 obs) right son=55 (13 obs)
##   Primary splits:
##       GNI.nonNA      < 7715  to the left,  improve=0.1350427, (0 missing)
##       ChildMortality < 13.55 to the left,  improve=0.1181435, (0 missing)
##   Surrogate splits:
##       ChildMortality < 16.8  to the right, agree=0.773, adj=0.444, (0 split)
## 
## Node number 28: 16 observations
##   mean=76.0625, MSE=3.558594 
## 
## Node number 29: 7 observations
##   mean=78.14286, MSE=4.122449 
## 
## Node number 30: 15 observations
##   mean=80.6, MSE=2.506667 
## 
## Node number 31: 7 observations
##   mean=82, MSE=0.5714286 
## 
## Node number 54: 9 observations
##   mean=73.11111, MSE=4.54321 
## 
## Node number 55: 13 observations
##   mean=74.46154, MSE=1.633136 
## 
## n= 139 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
##  1) root 139 12230.43000 69.67626  
##    2) ChildMortality>=37.5 50  1924.58000 59.22000  
##      4) ChildMortality>=78.65 22   289.45450 53.54545  
##        8) ChildMortality>=118.6 7    24.85714 49.85714 *
##        9) ChildMortality< 118.6 15   124.93330 55.26667 *
##      5) ChildMortality< 78.65 28   370.10710 63.67857  
##       10) GNI.nonNA< 1760 11   130.54550 61.36364 *
##       11) GNI.nonNA>=1760 17   142.47060 65.17647 *
##    3) ChildMortality< 37.5 89  1768.02200 75.55056  
##      6) ChildMortality>=10.65 44   423.15910 72.20455  
##       12) ChildMortality>=25.5 11    66.54545 68.36364 *
##       13) ChildMortality< 25.5 33   140.24240 73.48485  
##         26) ChildMortality>=18.55 11    56.54545 72.63636 *
##         27) ChildMortality< 18.55 22    71.81818 73.90909  
##           54) GNI.nonNA< 7715 9    40.88889 73.11111 *
##           55) GNI.nonNA>=7715 13    21.23077 74.46154 *
##      7) ChildMortality< 10.65 45   370.57780 78.82222  
##       14) GNI.nonNA< 25905 23   106.86960 76.69565  
##         28) ChildMortality>=6.05 16    56.93750 76.06250 *
##         29) ChildMortality< 6.05 7    28.85714 78.14286 *
##       15) GNI.nonNA>=25905 22    50.95455 81.04545  
##         30) GNI.nonNA< 50695 15    37.60000 80.60000 *
##         31) GNI.nonNA>=50695 7     4.00000 82.00000 *
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##                    model_id model_method                     feats
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart ChildMortality, GNI.nonNA
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      0.424                 0.007
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB
## 1    0.9398703     2.300159    0.8659683     3.251075
```

```r
if (glb_is_regression || glb_is_binomial) # For multinomials this model will be run next by default
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.rpart"
## [1] "    indep_vars: ChildMortality, GNI.nonNA"
```

```
## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info =
## trainInfo, : There were missing values in resampled performance measures.
```

```
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0797 on full training set
```

```
## Warning in myfit_mdl(model_id = "Max.cor.Y", model_method = "rpart",
## model_type = glb_model_type, : model's bestTune found at an extreme of
## tuneGrid for parameter: cp
```

![](WHO2_files/figure-html/fit.models_0-7.png) ![](WHO2_files/figure-html/fit.models_0-8.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 139 
## 
##           CP nsplit rel error
## 1 0.69808077      0 1.0000000
## 2 0.10343202      1 0.3019192
## 3 0.07966077      2 0.1984872
## 
## Variable importance
## ChildMortality      GNI.nonNA 
##             61             39 
## 
## Node number 1: 139 observations,    complexity param=0.6980808
##   mean=69.67626, MSE=87.98872 
##   left son=2 (50 obs) right son=3 (89 obs)
##   Primary splits:
##       ChildMortality < 37.5  to the right, improve=0.6980808, (0 missing)
##       GNI.nonNA      < 3245  to the left,  improve=0.4836100, (0 missing)
##   Surrogate splits:
##       GNI.nonNA < 3615  to the left,  agree=0.899, adj=0.72, (0 split)
## 
## Node number 2: 50 observations,    complexity param=0.103432
##   mean=59.22, MSE=38.4916 
##   left son=4 (22 obs) right son=5 (28 obs)
##   Primary splits:
##       ChildMortality < 78.65 to the right, improve=0.6572958, (0 missing)
##       GNI.nonNA      < 1075  to the left,  improve=0.1047542, (0 missing)
##   Surrogate splits:
##       GNI.nonNA < 1075  to the left,  agree=0.6, adj=0.091, (0 split)
## 
## Node number 3: 89 observations
##   mean=75.55056, MSE=19.86542 
## 
## Node number 4: 22 observations
##   mean=53.54545, MSE=13.15702 
## 
## Node number 5: 28 observations
##   mean=63.67857, MSE=13.21811 
## 
## n= 139 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 139 12230.4300 69.67626  
##   2) ChildMortality>=37.5 50  1924.5800 59.22000  
##     4) ChildMortality>=78.65 22   289.4545 53.54545 *
##     5) ChildMortality< 78.65 28   370.1071 63.67857 *
##   3) ChildMortality< 37.5 89  1768.0220 75.55056 *
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##          model_id model_method                     feats max.nTuningRuns
## 1 Max.cor.Y.rpart        rpart ChildMortality, GNI.nonNA               3
##   min.elapsedtime.everything min.elapsedtime.final max.R.sq.fit
## 1                      1.127                 0.009    0.8015128
##   min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Rsquared.fit min.RMSESD.fit
## 1     4.325582     0.764756     4.307076        0.8033837      0.6087925
##   max.RsquaredSD.fit
## 1         0.07876564
```

```r
# Used to compare vs. Interactions.High.cor.Y and/or Max.cor.Y.TmSrs
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.lm"
## [1] "    indep_vars: ChildMortality, GNI.nonNA"
## Aggregating results
## Fitting final model on full training set
```

![](WHO2_files/figure-html/fit.models_0-9.png) ![](WHO2_files/figure-html/fit.models_0-10.png) ![](WHO2_files/figure-html/fit.models_0-11.png) ![](WHO2_files/figure-html/fit.models_0-12.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.2768  -1.4423  -0.0022   1.9361   7.7014 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    75.5206008  0.5134028  147.10  < 2e-16 ***
## ChildMortality -0.2000039  0.0076272  -26.22  < 2e-16 ***
## GNI.nonNA       0.0001175  0.0000170    6.91 1.71e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.988 on 136 degrees of freedom
## Multiple R-squared:  0.9008,	Adjusted R-squared:  0.8993 
## F-statistic: 617.1 on 2 and 136 DF,  p-value: < 2.2e-16
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##       model_id model_method                     feats max.nTuningRuns
## 1 Max.cor.Y.lm           lm ChildMortality, GNI.nonNA               1
##   min.elapsedtime.everything min.elapsedtime.final max.R.sq.fit
## 1                      0.817                 0.003     0.900751
##   min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit max.Rsquared.fit
## 1     3.003298    0.8554912      3.37575        0.8992914        0.9058445
##   min.RMSESD.fit max.RsquaredSD.fit
## 1      0.7711282         0.05023664
```

```r
if (!is.null(glb_date_vars)) {
# ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly1", 
#                         model_method=ifelse(glb_is_regression, "lm", 
#                                         ifelse(glb_is_binomial, "glm", "rpart")),
#                      model_type=glb_model_type,
#                         indep_vars_vctr=c(max_cor_y_x_vars, paste0(glb_date_vars, ".day.minutes")),
#                         rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                         fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                         n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
# 
ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=c(max_cor_y_x_vars, 
                                          paste0(glb_date_vars, ".day.minutes.poly.", 1:5)),
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
}

# Interactions.High.cor.Y
if (length(int_feats <- setdiff(unique(glb_feats_df$cor.high.X), NA)) > 0) {
    # lm & glm handle interaction terms; rpart & rf do not
    if (glb_is_regression || glb_is_binomial) {
        indep_vars_vctr <- 
            c(max_cor_y_x_vars, paste(max_cor_y_x_vars[1], int_feats, sep=":"))            
    } else { indep_vars_vctr <- union(max_cor_y_x_vars, int_feats) }
    
    ret_lst <- myfit_mdl(model_id="Interact.High.cor.Y", 
                            model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                         model_type=glb_model_type,
                            indep_vars_vctr,
                            glb_rsp_var, glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)                        
}    
```

```
## [1] "fitting model: Interact.High.cor.Y.lm"
## [1] "    indep_vars: ChildMortality, GNI.nonNA, ChildMortality:Under15, ChildMortality:ChildMortality, ChildMortality:PrimarySchoolEnrollmentFemale.nonNA"
## Aggregating results
## Fitting final model on full training set
```

![](WHO2_files/figure-html/fit.models_0-13.png) ![](WHO2_files/figure-html/fit.models_0-14.png) ![](WHO2_files/figure-html/fit.models_0-15.png) ![](WHO2_files/figure-html/fit.models_0-16.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.4611 -1.3784 -0.0552  1.8585  8.5518 
## 
## Coefficients:
##                                                        Estimate Std. Error
## (Intercept)                                           7.686e+01  6.400e-01
## ChildMortality                                       -2.211e-01  6.066e-02
## GNI.nonNA                                             9.746e-05  1.731e-05
## `ChildMortality:Under15`                              1.895e-03  1.047e-03
## `ChildMortality:PrimarySchoolEnrollmentFemale.nonNA` -9.681e-04  3.734e-04
##                                                      t value Pr(>|t|)    
## (Intercept)                                          120.096  < 2e-16 ***
## ChildMortality                                        -3.645 0.000382 ***
## GNI.nonNA                                              5.631 1.01e-07 ***
## `ChildMortality:Under15`                               1.809 0.072723 .  
## `ChildMortality:PrimarySchoolEnrollmentFemale.nonNA`  -2.593 0.010572 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.872 on 134 degrees of freedom
## Multiple R-squared:  0.9097,	Adjusted R-squared:  0.907 
## F-statistic: 337.3 on 4 and 134 DF,  p-value: < 2.2e-16
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##                 model_id model_method
## 1 Interact.High.cor.Y.lm           lm
##                                                                                                                                  feats
## 1 ChildMortality, GNI.nonNA, ChildMortality:Under15, ChildMortality:ChildMortality, ChildMortality:PrimarySchoolEnrollmentFemale.nonNA
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                       0.82                 0.003
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit
## 1    0.9096579     2.981268    0.8458567     3.486467        0.9069611
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1        0.9116481       0.646475         0.03793371
```

```r
# Low.cor.X
# if (glb_is_classification && glb_is_binomial)
#     indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & 
#                                             is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"] else
indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & !myNearZV & 
                              (exclude.as.feat != 1))[, "id"]  
myadjust_interaction_feats <- function(vars_vctr) {
    for (feat in subset(glb_feats_df, !is.na(interaction.feat))$id)
        if (feat %in% vars_vctr)
            vars_vctr <- union(setdiff(vars_vctr, feat), 
                paste0(glb_feats_df[glb_feats_df$id == feat, "interaction.feat"], ":", feat))
    return(vars_vctr)
}
indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)
ret_lst <- myfit_mdl(model_id="Low.cor.X", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                        indep_vars_vctr=indep_vars_vctr,
                        model_type=glb_model_type,                     
                        glb_rsp_var, glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Low.cor.X.lm"
## [1] "    indep_vars: GNI.nonNA, CellularSubscribers.nonNA, Population, .rnorm, Region.fctr, ChildMortality"
## Aggregating results
## Fitting final model on full training set
```

![](WHO2_files/figure-html/fit.models_0-17.png) ![](WHO2_files/figure-html/fit.models_0-18.png) ![](WHO2_files/figure-html/fit.models_0-19.png) ![](WHO2_files/figure-html/fit.models_0-20.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.9613 -1.6261  0.1944  1.8748  6.1139 
## 
## Coefficients:
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   7.375e+01  1.341e+00  55.017  < 2e-16 ***
## GNI.nonNA                     1.226e-04  1.785e-05   6.868 2.54e-10 ***
## CellularSubscribers.nonNA    -1.523e-03  7.750e-03  -0.197   0.8445    
## Population                    1.007e-06  1.576e-06   0.639   0.5242    
## .rnorm                       -5.259e-02  2.604e-01  -0.202   0.8403    
## Region.fctrEurope             1.881e+00  9.845e-01   1.910   0.0583 .  
## Region.fctrAmericas           2.514e+00  1.051e+00   2.392   0.0182 *  
## `Region.fctrSouth-East Asia`  8.839e-01  1.275e+00   0.693   0.4893    
## Region.fctrAfrica            -1.848e+00  1.010e+00  -1.830   0.0695 .  
## `Region.fctrWestern Pacific`  6.173e-01  1.062e+00   0.581   0.5621    
## ChildMortality               -1.699e-01  1.122e-02 -15.138  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.85 on 128 degrees of freedom
## Multiple R-squared:  0.915,	Adjusted R-squared:  0.9083 
## F-statistic: 137.7 on 10 and 128 DF,  p-value: < 2.2e-16
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##       model_id model_method
## 1 Low.cor.X.lm           lm
##                                                                                   feats
## 1 GNI.nonNA, CellularSubscribers.nonNA, Population, .rnorm, Region.fctr, ChildMortality
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      0.816                 0.004
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit
## 1     0.914963      3.06124    0.8688668     3.215729        0.9083195
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1        0.8958836       0.643469         0.06005185
```

```r
rm(ret_lst)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn    end elapsed
## 10 fit.models          7          0 28.564 42.492  13.928
## 11 fit.models          7          1 42.492     NA      NA
```


```r
fit.models_1_chunk_df <- myadd_chunk(NULL, "fit.models_1_bgn")
```

```
##              label step_major step_minor    bgn end elapsed
## 1 fit.models_1_bgn          1          0 44.393  NA      NA
```

```r
# Options:
#   1. rpart & rf manual tuning
#   2. rf without pca (default: with pca)

#stop(here); sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
#glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df

# All X that is not user excluded
# if (glb_is_classification && glb_is_binomial) {
#     model_id_pfx <- "Conditional.X"
# # indep_vars_vctr <- setdiff(names(glb_fitobs_df), union(glb_rsp_var, glb_exclude_vars_as_features))
#     indep_vars_vctr <- subset(glb_feats_df, is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"]
# } else {
    model_id_pfx <- "All.X"
    indep_vars_vctr <- subset(glb_feats_df, !myNearZV &
                                            (exclude.as.feat != 1))[, "id"]
# }

indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)

for (method in glb_models_method_vctr) {
    fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, 
                                paste0("fit.models_1_", method), major.inc=TRUE)
    if (method %in% c("rpart", "rf")) {
        # rpart:    fubar's the tree
        # rf:       skip the scenario w/ .rnorm for speed
        indep_vars_vctr <- setdiff(indep_vars_vctr, c(".rnorm"))
        model_id <- paste0(model_id_pfx, ".no.rnorm")
    } else model_id <- model_id_pfx
    
    if (method %in% c("glm")) # for a "robust" glm model
        indep_vars_vctr <- setdiff(indep_vars_vctr, c(NULL
                                    ,"A.nchrs.log"      # correlated to "S.*"                                                      
                                    ,"A.ndgts.log"      # correlated to "S.*"
                                    ,"A.nuppr.log"      # correlated to "S.*"
                                    ,"A.npnct01.log" # identical  to "S.npnct01.log"
                                    ,"A.npnct03.log" # correlated to "S.npnct03.log"
                                    ,"A.npnct04.log" # correlated to "S.npnct04.log"
                                    ,"A.npnct06.log" # identical  to "S.npnct06.log"
                                    ,"A.npnct07.log" # identical  to "S.npnct07.log"
                                    ,"A.npnct08.log" # correlated to "S.npnct08.log"
                                    ,"A.npnct11.log" # correlated to "S.*"
                                    ,"A.npnct12.log" # correlated to "S.*"
                                    ,"S.npnct14.log" # correlated to "A.*"
                                    ,"A.npnct15.log" # correlated to "S.npnct15.log"
                                    ,"A.npnct16.log" # correlated to "S.npnct16.log"
                                    ,"A.npnct19.log" # correlated to "S.*"
                                    ,"A.npnct20.log" # identical  to "S.npnct20.log"
                                    ,"A.npnct21.log" # correlated to "S.npnct21.log"
                                    ,"A.P.daily.clip.report" # identical  to "S.*"
                                    ,"S.P.daily.clip.report" # identical  to "H.*"
                                    ,"A.P.http" # correlated  to "A.npnct14.log"
                                    ,"A.P.fashion.week" # identical  to "S.*"
                                    ,"H.P.first.draft" # correlated  to "H.T.first"
                                    ,"A.P.first.draft" # identical  to "S.*"
                                    ,"A.P.metropolitan.diary.colon" # identical  to "S.*"
                                    ,"A.P.year.colon" # identical  to "S.P.year.colon"
                                                      ))
    
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                            indep_vars_vctr=indep_vars_vctr,
                            model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    
    # If All.X.glm is less accurate than Low.Cor.X.glm
    #   check NA coefficients & filter appropriate terms in indep_vars_vctr
#     if (method == "glm") {
#         orig_glm <- glb_models_lst[[paste0(model_id, ".", model_method)]]$finalModel
#         orig_glm <- glb_models_lst[["All.X.glm"]]$finalModel; print(summary(orig_glm))
#           vif_orig_glm <- vif(orig_glm); print(vif_orig_glm)
#           print(vif_orig_glm[!is.na(vif_orig_glm) & (vif_orig_glm == Inf)])
#           print(which.max(vif_orig_glm))
#           print(sort(vif_orig_glm[vif_orig_glm >= 1.0e+03], decreasing=TRUE))
#           glb_fitobs_df[c(1143, 3637, 3953, 4105), c("UniqueID", "Popular", "H.P.quandary", "Headline")]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE), ]
#           all.equal(glb_allobs_df$S.nuppr.log, glb_allobs_df$A.nuppr.log)
#           all.equal(glb_allobs_df$S.npnct19.log, glb_allobs_df$A.npnct19.log)
#           all.equal(glb_allobs_df$S.P.year.colon, glb_allobs_df$A.P.year.colon)
#           all.equal(glb_allobs_df$S.T.share, glb_allobs_df$A.T.share)
#           all.equal(glb_allobs_df$H.T.clip, glb_allobs_df$H.P.daily.clip.report)
#           cor(glb_allobs_df$S.T.herald, glb_allobs_df$S.T.tribun)
#           dsp_obs(Abstract.contains="[Dd]iar", cols=("Abstract"), all=TRUE)
#           dsp_obs(Abstract.contains="[Ss]hare", cols=("Abstract"), all=TRUE)
#           subset(glb_feats_df, cor.y.abs <= glb_feats_df[glb_feats_df$id == ".rnorm", "cor.y.abs"])
#         corxx_mtrx <- cor(data.matrix(glb_allobs_df[, setdiff(names(glb_allobs_df), myfind_chr_cols_df(glb_allobs_df))]), use="pairwise.complete.obs"); abs_corxx_mtrx <- abs(corxx_mtrx); diag(abs_corxx_mtrx) <- 0
#           which.max(abs_corxx_mtrx["S.T.tribun", ])
#           abs_corxx_mtrx["A.npnct08.log", "S.npnct08.log"]
#         step_glm <- step(orig_glm)
#     }
    # Since caret does not optimize rpart well
#     if (method == "rpart")
#         ret_lst <- myfit_mdl(model_id=paste0(model_id_pfx, ".cp.0"), model_method=method,
#                                 indep_vars_vctr=indep_vars_vctr,
#                                 model_type=glb_model_type,
#                                 rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                                 fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,        
#             n_cv_folds=0, tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
}
```

```
##              label step_major step_minor    bgn    end elapsed
## 1 fit.models_1_bgn          1          0 44.393 44.407   0.014
## 2  fit.models_1_lm          2          0 44.407     NA      NA
## [1] "fitting model: All.X.lm"
## [1] "    indep_vars: Over60, LiteracyRate.nonNA, PrimarySchoolEnrollmentFemale.nonNA, GNI.nonNA, CellularSubscribers.nonNA, PrimarySchoolEnrollmentMale.nonNA, Population, .rnorm, Region.fctr, FertilityRate.nonNA, Under15, ChildMortality"
## Aggregating results
## Fitting final model on full training set
```

![](WHO2_files/figure-html/fit.models_1-1.png) ![](WHO2_files/figure-html/fit.models_1-2.png) ![](WHO2_files/figure-html/fit.models_1-3.png) ![](WHO2_files/figure-html/fit.models_1-4.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -7.5105 -1.6085  0.2761  1.4929  6.8723 
## 
## Coefficients:
##                                       Estimate Std. Error t value Pr(>|t|)
## (Intercept)                          7.651e+01  5.057e+00  15.130  < 2e-16
## Over60                               1.079e-01  8.234e-02   1.311   0.1923
## LiteracyRate.nonNA                  -2.200e-02  2.635e-02  -0.835   0.4054
## PrimarySchoolEnrollmentFemale.nonNA -1.355e-01  7.816e-02  -1.733   0.0856
## GNI.nonNA                            8.989e-05  2.091e-05   4.300 3.47e-05
## CellularSubscribers.nonNA           -1.682e-03  7.912e-03  -0.213   0.8320
## PrimarySchoolEnrollmentMale.nonNA    1.393e-01  7.909e-02   1.761   0.0808
## Population                           4.461e-07  1.602e-06   0.278   0.7812
## .rnorm                               2.091e-01  2.786e-01   0.751   0.4543
## Region.fctrEurope                    5.665e-01  1.278e+00   0.443   0.6584
## Region.fctrAmericas                  2.288e+00  1.137e+00   2.013   0.0463
## `Region.fctrSouth-East Asia`         8.995e-01  1.310e+00   0.687   0.4935
## Region.fctrAfrica                   -1.945e+00  1.042e+00  -1.867   0.0643
## `Region.fctrWestern Pacific`         6.166e-01  1.096e+00   0.563   0.5746
## FertilityRate.nonNA                  1.104e+00  6.505e-01   1.698   0.0921
## Under15                             -1.392e-01  1.073e-01  -1.298   0.1969
## ChildMortality                      -1.935e-01  1.615e-02 -11.979  < 2e-16
##                                        
## (Intercept)                         ***
## Over60                                 
## LiteracyRate.nonNA                     
## PrimarySchoolEnrollmentFemale.nonNA .  
## GNI.nonNA                           ***
## CellularSubscribers.nonNA              
## PrimarySchoolEnrollmentMale.nonNA   .  
## Population                             
## .rnorm                                 
## Region.fctrEurope                      
## Region.fctrAmericas                 *  
## `Region.fctrSouth-East Asia`           
## Region.fctrAfrica                   .  
## `Region.fctrWestern Pacific`           
## FertilityRate.nonNA                 .  
## Under15                                
## ChildMortality                      ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.756 on 122 degrees of freedom
## Multiple R-squared:  0.9243,	Adjusted R-squared:  0.9143 
## F-statistic: 93.04 on 16 and 122 DF,  p-value: < 2.2e-16
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##   model_id model_method
## 1 All.X.lm           lm
##                                                                                                                                                                                                                     feats
## 1 Over60, LiteracyRate.nonNA, PrimarySchoolEnrollmentFemale.nonNA, GNI.nonNA, CellularSubscribers.nonNA, PrimarySchoolEnrollmentMale.nonNA, Population, .rnorm, Region.fctr, FertilityRate.nonNA, Under15, ChildMortality
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      0.828                 0.006
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit
## 1    0.9242529     3.218807    0.8765709     3.119837        0.9143188
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1        0.8944665      0.3711841         0.03847592
##              label step_major step_minor    bgn    end elapsed
## 2  fit.models_1_lm          2          0 44.407 46.642   2.235
## 3 fit.models_1_glm          3          0 46.642     NA      NA
## [1] "fitting model: All.X.glm"
## [1] "    indep_vars: Over60, LiteracyRate.nonNA, PrimarySchoolEnrollmentFemale.nonNA, GNI.nonNA, CellularSubscribers.nonNA, PrimarySchoolEnrollmentMale.nonNA, Population, .rnorm, Region.fctr, FertilityRate.nonNA, Under15, ChildMortality"
## Aggregating results
## Fitting final model on full training set
```

![](WHO2_files/figure-html/fit.models_1-5.png) ![](WHO2_files/figure-html/fit.models_1-6.png) ![](WHO2_files/figure-html/fit.models_1-7.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -7.5105  -1.6085   0.2761   1.4929   6.8723  
## 
## Coefficients:
##                                       Estimate Std. Error t value Pr(>|t|)
## (Intercept)                          7.651e+01  5.057e+00  15.130  < 2e-16
## Over60                               1.079e-01  8.234e-02   1.311   0.1923
## LiteracyRate.nonNA                  -2.200e-02  2.635e-02  -0.835   0.4054
## PrimarySchoolEnrollmentFemale.nonNA -1.355e-01  7.816e-02  -1.733   0.0856
## GNI.nonNA                            8.989e-05  2.091e-05   4.300 3.47e-05
## CellularSubscribers.nonNA           -1.682e-03  7.912e-03  -0.213   0.8320
## PrimarySchoolEnrollmentMale.nonNA    1.393e-01  7.909e-02   1.761   0.0808
## Population                           4.461e-07  1.602e-06   0.278   0.7812
## .rnorm                               2.091e-01  2.786e-01   0.751   0.4543
## Region.fctrEurope                    5.665e-01  1.278e+00   0.443   0.6584
## Region.fctrAmericas                  2.288e+00  1.137e+00   2.013   0.0463
## `Region.fctrSouth-East Asia`         8.995e-01  1.310e+00   0.687   0.4935
## Region.fctrAfrica                   -1.945e+00  1.042e+00  -1.867   0.0643
## `Region.fctrWestern Pacific`         6.166e-01  1.096e+00   0.563   0.5746
## FertilityRate.nonNA                  1.104e+00  6.505e-01   1.698   0.0921
## Under15                             -1.392e-01  1.073e-01  -1.298   0.1969
## ChildMortality                      -1.935e-01  1.615e-02 -11.979  < 2e-16
##                                        
## (Intercept)                         ***
## Over60                                 
## LiteracyRate.nonNA                     
## PrimarySchoolEnrollmentFemale.nonNA .  
## GNI.nonNA                           ***
## CellularSubscribers.nonNA              
## PrimarySchoolEnrollmentMale.nonNA   .  
## Population                             
## .rnorm                                 
## Region.fctrEurope                      
## Region.fctrAmericas                 *  
## `Region.fctrSouth-East Asia`           
## Region.fctrAfrica                   .  
## `Region.fctrWestern Pacific`           
## FertilityRate.nonNA                 .  
## Under15                                
## ChildMortality                      ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 7.593608)
## 
##     Null deviance: 12230.43  on 138  degrees of freedom
## Residual deviance:   926.42  on 122  degrees of freedom
## AIC: 694.13
## 
## Number of Fisher Scoring iterations: 2
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##    model_id model_method
## 1 All.X.glm          glm
##                                                                                                                                                                                                                     feats
## 1 Over60, LiteracyRate.nonNA, PrimarySchoolEnrollmentFemale.nonNA, GNI.nonNA, CellularSubscribers.nonNA, PrimarySchoolEnrollmentMale.nonNA, Population, .rnorm, Region.fctr, FertilityRate.nonNA, Under15, ChildMortality
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                       0.84                 0.016
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB min.aic.fit
## 1    0.9242529     3.218807    0.8765709     3.119837    694.1276
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1        0.8944665      0.3711841         0.03847592
##                label step_major step_minor    bgn    end elapsed
## 3   fit.models_1_glm          3          0 46.642 48.936   2.294
## 4 fit.models_1_rpart          4          0 48.936     NA      NA
## [1] "fitting model: All.X.no.rnorm.rpart"
## [1] "    indep_vars: Over60, LiteracyRate.nonNA, PrimarySchoolEnrollmentFemale.nonNA, GNI.nonNA, CellularSubscribers.nonNA, PrimarySchoolEnrollmentMale.nonNA, Population, Region.fctr, FertilityRate.nonNA, Under15, ChildMortality"
```

```
## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info =
## trainInfo, : There were missing values in resampled performance measures.
```

![](WHO2_files/figure-html/fit.models_1-8.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0797 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](WHO2_files/figure-html/fit.models_1-9.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 139 
## 
##           CP nsplit rel error
## 1 0.69808077      0 1.0000000
## 2 0.10343202      1 0.3019192
## 3 0.07966077      2 0.1984872
## 
## Variable importance
##                      ChildMortality                 FertilityRate.nonNA 
##                                  25                                  16 
##                           GNI.nonNA                             Under15 
##                                  16                                  15 
##                              Over60           CellularSubscribers.nonNA 
##                                  13                                  13 
##                  LiteracyRate.nonNA PrimarySchoolEnrollmentFemale.nonNA 
##                                   2                                   1 
## 
## Node number 1: 139 observations,    complexity param=0.6980808
##   mean=69.67626, MSE=87.98872 
##   left son=2 (50 obs) right son=3 (89 obs)
##   Primary splits:
##       ChildMortality      < 37.5   to the right, improve=0.6980808, (0 missing)
##       Under15             < 36.2   to the right, improve=0.5848761, (0 missing)
##       FertilityRate.nonNA < 3.13   to the right, improve=0.5695908, (0 missing)
##       Over60              < 6.325  to the left,  improve=0.5129444, (0 missing)
##       Region.fctrAfrica   < 0.5    to the right, improve=0.4975190, (0 missing)
##   Surrogate splits:
##       GNI.nonNA                 < 3615   to the left,  agree=0.899, adj=0.72, (0 split)
##       FertilityRate.nonNA       < 3.13   to the right, agree=0.878, adj=0.66, (0 split)
##       Under15                   < 34.88  to the right, agree=0.871, adj=0.64, (0 split)
##       CellularSubscribers.nonNA < 69.355 to the left,  agree=0.849, adj=0.58, (0 split)
##       Over60                    < 5.78   to the left,  agree=0.835, adj=0.54, (0 split)
## 
## Node number 2: 50 observations,    complexity param=0.103432
##   mean=59.22, MSE=38.4916 
##   left son=4 (22 obs) right son=5 (28 obs)
##   Primary splits:
##       ChildMortality      < 78.65  to the right, improve=0.6572958, (0 missing)
##       Under15             < 36.67  to the right, improve=0.4353715, (0 missing)
##       FertilityRate.nonNA < 3.44   to the right, improve=0.3654708, (0 missing)
##       Over60              < 6.345  to the left,  improve=0.3498308, (0 missing)
##       Region.fctrAfrica   < 0.5    to the right, improve=0.3398993, (0 missing)
##   Surrogate splits:
##       LiteracyRate.nonNA                  < 56.5   to the left,  agree=0.80, adj=0.545, (0 split)
##       FertilityRate.nonNA                 < 4.67   to the right, agree=0.76, adj=0.455, (0 split)
##       PrimarySchoolEnrollmentFemale.nonNA < 77.1   to the left,  agree=0.72, adj=0.364, (0 split)
##       Under15                             < 36.67  to the right, agree=0.72, adj=0.364, (0 split)
##       Over60                              < 5.145  to the left,  agree=0.68, adj=0.273, (0 split)
## 
## Node number 3: 89 observations
##   mean=75.55056, MSE=19.86542 
## 
## Node number 4: 22 observations
##   mean=53.54545, MSE=13.15702 
## 
## Node number 5: 28 observations
##   mean=63.67857, MSE=13.21811 
## 
## n= 139 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 139 12230.4300 69.67626  
##   2) ChildMortality>=37.5 50  1924.5800 59.22000  
##     4) ChildMortality>=78.65 22   289.4545 53.54545 *
##     5) ChildMortality< 78.65 28   370.1071 63.67857 *
##   3) ChildMortality< 37.5 89  1768.0220 75.55056 *
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##               model_id model_method
## 1 All.X.no.rnorm.rpart        rpart
##                                                                                                                                                                                                             feats
## 1 Over60, LiteracyRate.nonNA, PrimarySchoolEnrollmentFemale.nonNA, GNI.nonNA, CellularSubscribers.nonNA, PrimarySchoolEnrollmentMale.nonNA, Population, Region.fctr, FertilityRate.nonNA, Under15, ChildMortality
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      0.903                 0.017
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Rsquared.fit
## 1    0.8015128     4.325582     0.764756     4.307076        0.8033837
##   min.RMSESD.fit max.RsquaredSD.fit
## 1      0.6087925         0.07876564
##                label step_major step_minor    bgn    end elapsed
## 4 fit.models_1_rpart          4          0 48.936 51.687   2.751
## 5    fit.models_1_rf          5          0 51.687     NA      NA
## [1] "fitting model: All.X.no.rnorm.rf"
## [1] "    indep_vars: Over60, LiteracyRate.nonNA, PrimarySchoolEnrollmentFemale.nonNA, GNI.nonNA, CellularSubscribers.nonNA, PrimarySchoolEnrollmentMale.nonNA, Population, Region.fctr, FertilityRate.nonNA, Under15, ChildMortality"
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

![](WHO2_files/figure-html/fit.models_1-10.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 15 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: mtry
```

![](WHO2_files/figure-html/fit.models_1-11.png) ![](WHO2_files/figure-html/fit.models_1-12.png) 

```
##                 Length Class      Mode     
## call              4    -none-     call     
## type              1    -none-     character
## predicted       139    -none-     numeric  
## mse             500    -none-     numeric  
## rsq             500    -none-     numeric  
## oob.times       139    -none-     numeric  
## importance       15    -none-     numeric  
## importanceSD      0    -none-     NULL     
## localImportance   0    -none-     NULL     
## proximity         0    -none-     NULL     
## ntree             1    -none-     numeric  
## mtry              1    -none-     numeric  
## forest           11    -none-     list     
## coefs             0    -none-     NULL     
## y               139    -none-     numeric  
## test              0    -none-     NULL     
## inbag             0    -none-     NULL     
## xNames           15    -none-     character
## problemType       1    -none-     character
## tuneValue         1    data.frame list     
## obsLevels         1    -none-     logical  
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##            model_id model_method
## 1 All.X.no.rnorm.rf           rf
##                                                                                                                                                                                                             feats
## 1 Over60, LiteracyRate.nonNA, PrimarySchoolEnrollmentFemale.nonNA, GNI.nonNA, CellularSubscribers.nonNA, PrimarySchoolEnrollmentMale.nonNA, Population, Region.fctr, FertilityRate.nonNA, Under15, ChildMortality
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      2.156                 0.388
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Rsquared.fit
## 1    0.9849445     2.951944    0.8806828     3.066581        0.9125122
##   min.RMSESD.fit max.RsquaredSD.fit
## 1      0.3147728         0.03773599
```

```r
# User specified
    # easier to exclude features
#model_id_pfx <- "";
# indep_vars_vctr <- setdiff(names(glb_fitobs_df), 
#                         union(union(glb_rsp_var, glb_exclude_vars_as_features), 
#                                 c("<feat1_name>", "<feat2_name>")))
# method <- ""                                

    # easier to include features
# sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df; sav_featsimp_df <- glb_featsimp_df
# glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df; glm_featsimp_df <- sav_featsimp_df
#table(glb_allobs_df$myCategory, glb_allobs_df$H.P.readers.respond, glb_allobs_df[, glb_rsp_var], useNA="ifany")
#model_id <- "Rank9.2"; indep_vars_vctr <- c(NULL
#    ,"<feat1>"
#    ,"<feat1>*<feat2>"
#    ,"<feat1>:<feat2>"
#                                            )
# for (method in c("bayesglm")) {
#     ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
#                                 indep_vars_vctr=indep_vars_vctr,
#                                 model_type=glb_model_type,
#                                 rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                                 fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                     n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
# #     csm_mdl_id <- paste0(model_id, ".", method)
# #     csm_featsimp_df <- myget_feats_importance(glb_models_lst[[paste0(model_id, ".", method)]]);         print(head(csm_featsimp_df))
# }
#csm_featsimp_df[grepl("H.npnct19.log", row.names(csm_featsimp_df)), , FALSE]
#csm_OOBobs_df <- glb_get_predictions(glb_OOBobs_df, mdl_id=csm_mdl_id, rsp_var_out=glb_rsp_var_out, prob_threshold_def=glb_models_df[glb_models_df$model_id == csm_mdl_id, "opt.prob.threshold.OOB"])
#print(sprintf("%s OOB confusion matrix & accuracy: ", csm_mdl_id)); print(t(confusionMatrix(csm_OOBobs_df[, paste0(glb_rsp_var_out, csm_mdl_id)], csm_OOBobs_df[, glb_rsp_var])$table))

#glb_models_df[, "max.Accuracy.OOB", FALSE]
#varImp(glb_models_lst[["Low.cor.X.glm"]])
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.2.glm"]])$importance)
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.3.glm"]])$importance)
#glb_feats_df[grepl("npnct28", glb_feats_df$id), ]
#print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id)); print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], glb_OOBobs_df[, glb_rsp_var])$table))

    # User specified bivariate models
#     indep_vars_vctr_lst <- list()
#     for (feat in setdiff(names(glb_fitobs_df), 
#                          union(glb_rsp_var, glb_exclude_vars_as_features)))
#         indep_vars_vctr_lst[["feat"]] <- feat

    # User specified combinatorial models
#     indep_vars_vctr_lst <- list()
#     combn_mtrx <- combn(c("<feat1_name>", "<feat2_name>", "<featn_name>"), 
#                           <num_feats_to_choose>)
#     for (combn_ix in 1:ncol(combn_mtrx))
#         #print(combn_mtrx[, combn_ix])
#         indep_vars_vctr_lst[[combn_ix]] <- combn_mtrx[, combn_ix]
    
    # template for myfit_mdl
    #   rf is hard-coded in caret to recognize only Accuracy / Kappa evaluation metrics
    #       only for OOB in trainControl ?
    
#     ret_lst <- myfit_mdl_fn(model_id=paste0(model_id_pfx, ""), model_method=method,
#                             indep_vars_vctr=indep_vars_vctr,
#                             rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                             fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                             n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df,
#                             model_loss_mtrx=glb_model_metric_terms,
#                             model_summaryFunction=glb_model_metric_smmry,
#                             model_metric=glb_model_metric,
#                             model_metric_maximize=glb_model_metric_maximize)

# Simplify a model
# fit_df <- glb_fitobs_df; glb_mdl <- step(<complex>_mdl)

# Non-caret models
#     rpart_area_mdl <- rpart(reformulate("Area", response=glb_rsp_var), 
#                                data=glb_fitobs_df, #method="class", 
#                                control=rpart.control(cp=0.12),
#                            parms=list(loss=glb_model_metric_terms))
#     print("rpart_sel_wlm_mdl"); prp(rpart_sel_wlm_mdl)
# 

print(glb_models_df)
```

```
##                                            model_id model_method
## MFO.lm                                       MFO.lm           lm
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart        rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart        rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart        rpart
## Max.cor.Y.lm                           Max.cor.Y.lm           lm
## Interact.High.cor.Y.lm       Interact.High.cor.Y.lm           lm
## Low.cor.X.lm                           Low.cor.X.lm           lm
## All.X.lm                                   All.X.lm           lm
## All.X.glm                                 All.X.glm          glm
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart        rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                             feats
## MFO.lm                                                                                                                                                                                                                                     .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                                    ChildMortality, GNI.nonNA
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                               ChildMortality, GNI.nonNA
## Max.cor.Y.rpart                                                                                                                                                                                                         ChildMortality, GNI.nonNA
## Max.cor.Y.lm                                                                                                                                                                                                            ChildMortality, GNI.nonNA
## Interact.High.cor.Y.lm                                                                                       ChildMortality, GNI.nonNA, ChildMortality:Under15, ChildMortality:ChildMortality, ChildMortality:PrimarySchoolEnrollmentFemale.nonNA
## Low.cor.X.lm                                                                                                                                                GNI.nonNA, CellularSubscribers.nonNA, Population, .rnorm, Region.fctr, ChildMortality
## All.X.lm                  Over60, LiteracyRate.nonNA, PrimarySchoolEnrollmentFemale.nonNA, GNI.nonNA, CellularSubscribers.nonNA, PrimarySchoolEnrollmentMale.nonNA, Population, .rnorm, Region.fctr, FertilityRate.nonNA, Under15, ChildMortality
## All.X.glm                 Over60, LiteracyRate.nonNA, PrimarySchoolEnrollmentFemale.nonNA, GNI.nonNA, CellularSubscribers.nonNA, PrimarySchoolEnrollmentMale.nonNA, Population, .rnorm, Region.fctr, FertilityRate.nonNA, Under15, ChildMortality
## All.X.no.rnorm.rpart              Over60, LiteracyRate.nonNA, PrimarySchoolEnrollmentFemale.nonNA, GNI.nonNA, CellularSubscribers.nonNA, PrimarySchoolEnrollmentMale.nonNA, Population, Region.fctr, FertilityRate.nonNA, Under15, ChildMortality
## All.X.no.rnorm.rf                 Over60, LiteracyRate.nonNA, PrimarySchoolEnrollmentFemale.nonNA, GNI.nonNA, CellularSubscribers.nonNA, PrimarySchoolEnrollmentMale.nonNA, Population, Region.fctr, FertilityRate.nonNA, Under15, ChildMortality
##                           max.nTuningRuns min.elapsedtime.everything
## MFO.lm                                  0                      0.438
## Max.cor.Y.cv.0.rpart                    0                      0.511
## Max.cor.Y.cv.0.cp.0.rpart               0                      0.424
## Max.cor.Y.rpart                         3                      1.127
## Max.cor.Y.lm                            1                      0.817
## Interact.High.cor.Y.lm                  1                      0.820
## Low.cor.X.lm                            1                      0.816
## All.X.lm                                1                      0.828
## All.X.glm                               1                      0.840
## All.X.no.rnorm.rpart                    3                      0.903
## All.X.no.rnorm.rf                       3                      2.156
##                           min.elapsedtime.final max.R.sq.fit min.RMSE.fit
## MFO.lm                                    0.004  0.001105554     9.375044
## Max.cor.Y.cv.0.rpart                      0.009  0.000000000     9.380230
## Max.cor.Y.cv.0.cp.0.rpart                 0.007  0.939870337     2.300159
## Max.cor.Y.rpart                           0.009  0.801512798     4.325582
## Max.cor.Y.lm                              0.003  0.900750962     3.003298
## Interact.High.cor.Y.lm                    0.003  0.909657884     2.981268
## Low.cor.X.lm                              0.004  0.914963005     3.061240
## All.X.lm                                  0.006  0.924252863     3.218807
## All.X.glm                                 0.016  0.924252863     3.218807
## All.X.no.rnorm.rpart                      0.017  0.801512798     4.325582
## All.X.no.rnorm.rf                         0.388  0.984944475     2.951944
##                           max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit
## MFO.lm                     0.009390806     8.838414     -0.006185647
## Max.cor.Y.cv.0.rpart       0.000000000     8.880209               NA
## Max.cor.Y.cv.0.cp.0.rpart  0.865968304     3.251075               NA
## Max.cor.Y.rpart            0.764755963     4.307076               NA
## Max.cor.Y.lm               0.855491202     3.375750      0.899291417
## Interact.High.cor.Y.lm     0.845856653     3.486467      0.906961104
## Low.cor.X.lm               0.868866804     3.215729      0.908319490
## All.X.lm                   0.876570916     3.119837      0.914318813
## All.X.glm                  0.876570916     3.119837               NA
## All.X.no.rnorm.rpart       0.764755963     4.307076               NA
## All.X.no.rnorm.rf          0.880682838     3.066581               NA
##                           max.Rsquared.fit min.RMSESD.fit
## MFO.lm                                  NA             NA
## Max.cor.Y.cv.0.rpart                    NA             NA
## Max.cor.Y.cv.0.cp.0.rpart               NA             NA
## Max.cor.Y.rpart                  0.8033837      0.6087925
## Max.cor.Y.lm                     0.9058445      0.7711282
## Interact.High.cor.Y.lm           0.9116481      0.6464750
## Low.cor.X.lm                     0.8958836      0.6434690
## All.X.lm                         0.8944665      0.3711841
## All.X.glm                        0.8944665      0.3711841
## All.X.no.rnorm.rpart             0.8033837      0.6087925
## All.X.no.rnorm.rf                0.9125122      0.3147728
##                           max.RsquaredSD.fit min.aic.fit
## MFO.lm                                    NA          NA
## Max.cor.Y.cv.0.rpart                      NA          NA
## Max.cor.Y.cv.0.cp.0.rpart                 NA          NA
## Max.cor.Y.rpart                   0.07876564          NA
## Max.cor.Y.lm                      0.05023664          NA
## Interact.High.cor.Y.lm            0.03793371          NA
## Low.cor.X.lm                      0.06005185          NA
## All.X.lm                          0.03847592          NA
## All.X.glm                         0.03847592    694.1276
## All.X.no.rnorm.rpart              0.07876564          NA
## All.X.no.rnorm.rf                 0.03773599          NA
```

```r
rm(ret_lst)
fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, "fit.models_1_end", 
                                     major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 5  fit.models_1_rf          5          0 51.687 55.369   3.682
## 6 fit.models_1_end          6          0 55.369     NA      NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn    end elapsed
## 11 fit.models          7          1 42.492 55.374  12.883
## 12 fit.models          7          2 55.375     NA      NA
```


```r
if (!is.null(glb_model_metric_smmry)) {
    stats_df <- glb_models_df[, "model_id", FALSE]

    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_fitobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "fit",
        						glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_OOBobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "OOB",
            					glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
#     tmp_models_df <- orderBy(~model_id, glb_models_df)
#     rownames(tmp_models_df) <- seq(1, nrow(tmp_models_df))
#     all.equal(subset(tmp_models_df[, names(stats_df)], model_id != "Random.myrandom_classfr"),
#               subset(stats_df, model_id != "Random.myrandom_classfr"))
#     print(subset(tmp_models_df[, names(stats_df)], model_id != "Random.myrandom_classfr")[, c("model_id", "max.Accuracy.fit")])
#     print(subset(stats_df, model_id != "Random.myrandom_classfr")[, c("model_id", "max.Accuracy.fit")])

    print("Merging following data into glb_models_df:")
    print(stats_mrg_df <- stats_df[, c(1, grep(glb_model_metric, names(stats_df)))])
    print(tmp_models_df <- orderBy(~model_id, glb_models_df[, c("model_id", grep(glb_model_metric, names(stats_df), value=TRUE))]))

    tmp2_models_df <- glb_models_df[, c("model_id", setdiff(names(glb_models_df), grep(glb_model_metric, names(stats_df), value=TRUE)))]
    tmp3_models_df <- merge(tmp2_models_df, stats_mrg_df, all.x=TRUE, sort=FALSE)
    print(tmp3_models_df)
    print(names(tmp3_models_df))
    print(glb_models_df <- subset(tmp3_models_df, select=-model_id.1))
}

plt_models_df <- glb_models_df[, -grep("SD|Upper|Lower", names(glb_models_df))]
for (var in grep("^min.", names(plt_models_df), value=TRUE)) {
    plt_models_df[, sub("min.", "inv.", var)] <- 
        #ifelse(all(is.na(tmp <- plt_models_df[, var])), NA, 1.0 / tmp)
        1.0 / plt_models_df[, var]
    plt_models_df <- plt_models_df[ , -grep(var, names(plt_models_df))]
}
print(plt_models_df)
```

```
##                                            model_id model_method
## MFO.lm                                       MFO.lm           lm
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart        rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart        rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart        rpart
## Max.cor.Y.lm                           Max.cor.Y.lm           lm
## Interact.High.cor.Y.lm       Interact.High.cor.Y.lm           lm
## Low.cor.X.lm                           Low.cor.X.lm           lm
## All.X.lm                                   All.X.lm           lm
## All.X.glm                                 All.X.glm          glm
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart        rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                             feats
## MFO.lm                                                                                                                                                                                                                                     .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                                    ChildMortality, GNI.nonNA
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                               ChildMortality, GNI.nonNA
## Max.cor.Y.rpart                                                                                                                                                                                                         ChildMortality, GNI.nonNA
## Max.cor.Y.lm                                                                                                                                                                                                            ChildMortality, GNI.nonNA
## Interact.High.cor.Y.lm                                                                                       ChildMortality, GNI.nonNA, ChildMortality:Under15, ChildMortality:ChildMortality, ChildMortality:PrimarySchoolEnrollmentFemale.nonNA
## Low.cor.X.lm                                                                                                                                                GNI.nonNA, CellularSubscribers.nonNA, Population, .rnorm, Region.fctr, ChildMortality
## All.X.lm                  Over60, LiteracyRate.nonNA, PrimarySchoolEnrollmentFemale.nonNA, GNI.nonNA, CellularSubscribers.nonNA, PrimarySchoolEnrollmentMale.nonNA, Population, .rnorm, Region.fctr, FertilityRate.nonNA, Under15, ChildMortality
## All.X.glm                 Over60, LiteracyRate.nonNA, PrimarySchoolEnrollmentFemale.nonNA, GNI.nonNA, CellularSubscribers.nonNA, PrimarySchoolEnrollmentMale.nonNA, Population, .rnorm, Region.fctr, FertilityRate.nonNA, Under15, ChildMortality
## All.X.no.rnorm.rpart              Over60, LiteracyRate.nonNA, PrimarySchoolEnrollmentFemale.nonNA, GNI.nonNA, CellularSubscribers.nonNA, PrimarySchoolEnrollmentMale.nonNA, Population, Region.fctr, FertilityRate.nonNA, Under15, ChildMortality
## All.X.no.rnorm.rf                 Over60, LiteracyRate.nonNA, PrimarySchoolEnrollmentFemale.nonNA, GNI.nonNA, CellularSubscribers.nonNA, PrimarySchoolEnrollmentMale.nonNA, Population, Region.fctr, FertilityRate.nonNA, Under15, ChildMortality
##                           max.nTuningRuns max.R.sq.fit max.R.sq.OOB
## MFO.lm                                  0  0.001105554  0.009390806
## Max.cor.Y.cv.0.rpart                    0  0.000000000  0.000000000
## Max.cor.Y.cv.0.cp.0.rpart               0  0.939870337  0.865968304
## Max.cor.Y.rpart                         3  0.801512798  0.764755963
## Max.cor.Y.lm                            1  0.900750962  0.855491202
## Interact.High.cor.Y.lm                  1  0.909657884  0.845856653
## Low.cor.X.lm                            1  0.914963005  0.868866804
## All.X.lm                                1  0.924252863  0.876570916
## All.X.glm                               1  0.924252863  0.876570916
## All.X.no.rnorm.rpart                    3  0.801512798  0.764755963
## All.X.no.rnorm.rf                       3  0.984944475  0.880682838
##                           max.Adj.R.sq.fit max.Rsquared.fit
## MFO.lm                        -0.006185647               NA
## Max.cor.Y.cv.0.rpart                    NA               NA
## Max.cor.Y.cv.0.cp.0.rpart               NA               NA
## Max.cor.Y.rpart                         NA        0.8033837
## Max.cor.Y.lm                   0.899291417        0.9058445
## Interact.High.cor.Y.lm         0.906961104        0.9116481
## Low.cor.X.lm                   0.908319490        0.8958836
## All.X.lm                       0.914318813        0.8944665
## All.X.glm                               NA        0.8944665
## All.X.no.rnorm.rpart                    NA        0.8033837
## All.X.no.rnorm.rf                       NA        0.9125122
##                           inv.elapsedtime.everything inv.elapsedtime.final
## MFO.lm                                     2.2831050             250.00000
## Max.cor.Y.cv.0.rpart                       1.9569472             111.11111
## Max.cor.Y.cv.0.cp.0.rpart                  2.3584906             142.85714
## Max.cor.Y.rpart                            0.8873114             111.11111
## Max.cor.Y.lm                               1.2239902             333.33333
## Interact.High.cor.Y.lm                     1.2195122             333.33333
## Low.cor.X.lm                               1.2254902             250.00000
## All.X.lm                                   1.2077295             166.66667
## All.X.glm                                  1.1904762              62.50000
## All.X.no.rnorm.rpart                       1.1074197              58.82353
## All.X.no.rnorm.rf                          0.4638219               2.57732
##                           inv.RMSE.fit inv.RMSE.OOB inv.aic.fit
## MFO.lm                       0.1066662    0.1131425          NA
## Max.cor.Y.cv.0.rpart         0.1066072    0.1126100          NA
## Max.cor.Y.cv.0.cp.0.rpart    0.4347525    0.3075906          NA
## Max.cor.Y.rpart              0.2311828    0.2321761          NA
## Max.cor.Y.lm                 0.3329673    0.2962304          NA
## Interact.High.cor.Y.lm       0.3354278    0.2868233          NA
## Low.cor.X.lm                 0.3266650    0.3109714          NA
## All.X.lm                     0.3106741    0.3205295          NA
## All.X.glm                    0.3106741    0.3205295 0.001440657
## All.X.no.rnorm.rpart         0.2311828    0.2321761          NA
## All.X.no.rnorm.rf            0.3387598    0.3260961          NA
```

```r
print(myplot_radar(radar_inp_df=plt_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 11. Consider specifying shapes manually. if you must have them.
```

```
## Warning in loop_apply(n, do.ply): Removed 8 rows containing missing values
## (geom_path).
```

```
## Warning in loop_apply(n, do.ply): Removed 58 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 19 rows containing missing values
## (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 11. Consider specifying shapes manually. if you must have them.
```

![](WHO2_files/figure-html/fit.models_2-1.png) 

```r
# print(myplot_radar(radar_inp_df=subset(plt_models_df, 
#         !(model_id %in% grep("random|MFO", plt_models_df$model_id, value=TRUE)))))

# Compute CI for <metric>SD
glb_models_df <- mutate(glb_models_df, 
                max.df = ifelse(max.nTuningRuns > 1, max.nTuningRuns - 1, NA),
                min.sd2ci.scaler = ifelse(is.na(max.df), NA, qt(0.975, max.df)))
for (var in grep("SD", names(glb_models_df), value=TRUE)) {
    # Does CI alredy exist ?
    var_components <- unlist(strsplit(var, "SD"))
    varActul <- paste0(var_components[1],          var_components[2])
    varUpper <- paste0(var_components[1], "Upper", var_components[2])
    varLower <- paste0(var_components[1], "Lower", var_components[2])
    if (varUpper %in% names(glb_models_df)) {
        warning(varUpper, " already exists in glb_models_df")
        # Assuming Lower also exists
        next
    }    
    print(sprintf("var:%s", var))
    # CI is dependent on sample size in t distribution; df=n-1
    glb_models_df[, varUpper] <- glb_models_df[, varActul] + 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
    glb_models_df[, varLower] <- glb_models_df[, varActul] - 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
}
```

```
## [1] "var:min.RMSESD.fit"
## [1] "var:max.RsquaredSD.fit"
```

```r
# Plot metrics with CI
plt_models_df <- glb_models_df[, "model_id", FALSE]
pltCI_models_df <- glb_models_df[, "model_id", FALSE]
for (var in grep("Upper", names(glb_models_df), value=TRUE)) {
    var_components <- unlist(strsplit(var, "Upper"))
    col_name <- unlist(paste(var_components, collapse=""))
    plt_models_df[, col_name] <- glb_models_df[, col_name]
    for (name in paste0(var_components[1], c("Upper", "Lower"), var_components[2]))
        pltCI_models_df[, name] <- glb_models_df[, name]
}

build_statsCI_data <- function(plt_models_df) {
    mltd_models_df <- melt(plt_models_df, id.vars="model_id")
    mltd_models_df$data <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) tail(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), "[.]")), 1))
    mltd_models_df$label <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) head(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), 
            paste0(".", mltd_models_df[row_ix, "data"]))), 1))
    #print(mltd_models_df)
    
    return(mltd_models_df)
}
mltd_models_df <- build_statsCI_data(plt_models_df)

mltdCI_models_df <- melt(pltCI_models_df, id.vars="model_id")
for (row_ix in 1:nrow(mltdCI_models_df)) {
    for (type in c("Upper", "Lower")) {
        if (length(var_components <- unlist(strsplit(
                as.character(mltdCI_models_df[row_ix, "variable"]), type))) > 1) {
            #print(sprintf("row_ix:%d; type:%s; ", row_ix, type))
            mltdCI_models_df[row_ix, "label"] <- var_components[1]
            mltdCI_models_df[row_ix, "data"] <- 
                unlist(strsplit(var_components[2], "[.]"))[2]
            mltdCI_models_df[row_ix, "type"] <- type
            break
        }
    }    
}
#print(mltdCI_models_df)
# castCI_models_df <- dcast(mltdCI_models_df, value ~ type, fun.aggregate=sum)
# print(castCI_models_df)
wideCI_models_df <- reshape(subset(mltdCI_models_df, select=-variable), 
                            timevar="type", 
        idvar=setdiff(names(mltdCI_models_df), c("type", "value", "variable")), 
                            direction="wide")
#print(wideCI_models_df)
mrgdCI_models_df <- merge(wideCI_models_df, mltd_models_df, all.x=TRUE)
#print(mrgdCI_models_df)

# Merge stats back in if CIs don't exist
goback_vars <- c()
for (var in unique(mltd_models_df$label)) {
    for (type in unique(mltd_models_df$data)) {
        var_type <- paste0(var, ".", type)
        # if this data is already present, next
        if (var_type %in% unique(paste(mltd_models_df$label, mltd_models_df$data,
                                       sep=".")))
            next
        #print(sprintf("var_type:%s", var_type))
        goback_vars <- c(goback_vars, var_type)
    }
}

if (length(goback_vars) > 0) {
    mltd_goback_df <- build_statsCI_data(glb_models_df[, c("model_id", goback_vars)])
    mltd_models_df <- rbind(mltd_models_df, mltd_goback_df)
}

mltd_models_df <- merge(mltd_models_df, glb_models_df[, c("model_id", "model_method")], 
                        all.x=TRUE)

png(paste0(glb_out_pfx, "models_bar.png"), width=480*3, height=480*2)
print(gp <- myplot_bar(mltd_models_df, "model_id", "value", colorcol_name="model_method") + 
        geom_errorbar(data=mrgdCI_models_df, 
            mapping=aes(x=model_id, ymax=value.Upper, ymin=value.Lower), width=0.5) + 
          facet_grid(label ~ data, scales="free") + 
          theme(axis.text.x = element_text(angle = 90,vjust = 0.5)))
```

```
## Warning in loop_apply(n, do.ply): Removed 3 rows containing missing values
## (position_stack).
```

```r
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
print(gp)
```

```
## Warning in loop_apply(n, do.ply): Removed 3 rows containing missing values
## (position_stack).
```

![](WHO2_files/figure-html/fit.models_2-2.png) 

```r
# used for console inspection
model_evl_terms <- c(NULL)
for (metric in glb_model_evl_criteria)
    model_evl_terms <- c(model_evl_terms, 
                         ifelse(length(grep("max", metric)) > 0, "-", "+"), metric)
if (glb_is_classification && glb_is_binomial)
    model_evl_terms <- c(model_evl_terms, "-", "opt.prob.threshold.OOB")
model_sel_frmla <- as.formula(paste(c("~ ", model_evl_terms), collapse=" "))
dsp_models_cols <- c("model_id", glb_model_evl_criteria) 
if (glb_is_classification && glb_is_binomial) 
    dsp_models_cols <- c(dsp_models_cols, "opt.prob.threshold.OOB")
print(dsp_models_df <- orderBy(model_sel_frmla, glb_models_df)[, dsp_models_cols])
```

```
##                                            model_id min.RMSE.OOB
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf     3.066581
## All.X.lm                                   All.X.lm     3.119837
## All.X.glm                                 All.X.glm     3.119837
## Low.cor.X.lm                           Low.cor.X.lm     3.215729
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart     3.251075
## Max.cor.Y.lm                           Max.cor.Y.lm     3.375750
## Interact.High.cor.Y.lm       Interact.High.cor.Y.lm     3.486467
## Max.cor.Y.rpart                     Max.cor.Y.rpart     4.307076
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart     4.307076
## MFO.lm                                       MFO.lm     8.838414
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart     8.880209
##                           max.R.sq.OOB max.Adj.R.sq.fit
## All.X.no.rnorm.rf          0.880682838               NA
## All.X.lm                   0.876570916      0.914318813
## All.X.glm                  0.876570916               NA
## Low.cor.X.lm               0.868866804      0.908319490
## Max.cor.Y.cv.0.cp.0.rpart  0.865968304               NA
## Max.cor.Y.lm               0.855491202      0.899291417
## Interact.High.cor.Y.lm     0.845856653      0.906961104
## Max.cor.Y.rpart            0.764755963               NA
## All.X.no.rnorm.rpart       0.764755963               NA
## MFO.lm                     0.009390806     -0.006185647
## Max.cor.Y.cv.0.rpart       0.000000000               NA
```

```r
print(myplot_radar(radar_inp_df=dsp_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 11. Consider specifying shapes manually. if you must have them.
```

```
## Warning in loop_apply(n, do.ply): Removed 5 rows containing missing values
## (geom_path).
```

```
## Warning in loop_apply(n, do.ply): Removed 20 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 6 rows containing missing values
## (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 11. Consider specifying shapes manually. if you must have them.
```

![](WHO2_files/figure-html/fit.models_2-3.png) 

```r
print("Metrics used for model selection:"); print(model_sel_frmla)
```

```
## [1] "Metrics used for model selection:"
```

```
## ~+min.RMSE.OOB - max.R.sq.OOB - max.Adj.R.sq.fit
```

```r
print(sprintf("Best model id: %s", dsp_models_df[1, "model_id"]))
```

```
## [1] "Best model id: All.X.no.rnorm.rf"
```

```r
if (is.null(glb_sel_mdl_id)) { 
    glb_sel_mdl_id <- dsp_models_df[1, "model_id"]
    if (glb_sel_mdl_id == "Interact.High.cor.Y.glm") {
        warning("glb_sel_mdl_id: Interact.High.cor.Y.glm; myextract_mdl_feats does not currently support interaction terms")
        glb_sel_mdl_id <- dsp_models_df[2, "model_id"]
    }
} else 
    print(sprintf("User specified selection: %s", glb_sel_mdl_id))   
    
myprint_mdl(glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]])
```

![](WHO2_files/figure-html/fit.models_2-4.png) 

```
##                 Length Class      Mode     
## call              4    -none-     call     
## type              1    -none-     character
## predicted       139    -none-     numeric  
## mse             500    -none-     numeric  
## rsq             500    -none-     numeric  
## oob.times       139    -none-     numeric  
## importance       15    -none-     numeric  
## importanceSD      0    -none-     NULL     
## localImportance   0    -none-     NULL     
## proximity         0    -none-     NULL     
## ntree             1    -none-     numeric  
## mtry              1    -none-     numeric  
## forest           11    -none-     list     
## coefs             0    -none-     NULL     
## y               139    -none-     numeric  
## test              0    -none-     NULL     
## inbag             0    -none-     NULL     
## xNames           15    -none-     character
## problemType       1    -none-     character
## tuneValue         1    data.frame list     
## obsLevels         1    -none-     logical
```

```
## [1] TRUE
```

```r
# From here to save(), this should all be in one function
#   these are executed in the same seq twice more:
#       fit.data.training & predict.data.new chunks
glb_get_predictions <- function(df, mdl_id, rsp_var_out, prob_threshold_def=NULL) {
    mdl <- glb_models_lst[[mdl_id]]
    rsp_var_out <- paste0(rsp_var_out, mdl_id)

    if (glb_is_regression) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        print(myplot_scatter(df, glb_rsp_var, rsp_var_out, smooth=TRUE))
        df[, paste0(rsp_var_out, ".err")] <- 
            abs(df[, rsp_var_out] - df[, glb_rsp_var])
        print(head(orderBy(reformulate(c("-", paste0(rsp_var_out, ".err"))), 
                           df)))                             
    }

    if (glb_is_classification && glb_is_binomial) {
        prob_threshold <- glb_models_df[glb_models_df$model_id == mdl_id, 
                                        "opt.prob.threshold.OOB"]
        if (is.null(prob_threshold) || is.na(prob_threshold)) {
            warning("Using default probability threshold: ", prob_threshold_def)
            if (is.null(prob_threshold <- prob_threshold_def))
                stop("Default probability threshold is NULL")
        }
        
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")[, 2]
        df[, rsp_var_out] <- 
        		factor(levels(df[, glb_rsp_var])[
    				(df[, paste0(rsp_var_out, ".prob")] >=
    					prob_threshold) * 1 + 1], levels(df[, glb_rsp_var]))
    
        # prediction stats already reported by myfit_mdl ???
    }    
    
    if (glb_is_classification && !glb_is_binomial) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
    }

    return(df)
}    
glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out)
```

![](WHO2_files/figure-html/fit.models_2-5.png) 

```
##                Country                Region Population Under15 Over60
## 129           Pakistan Eastern Mediterranean     179000   34.31   6.44
## 98               Libya Eastern Mediterranean       6155   29.45   6.96
## 16             Belarus                Europe       9405   15.10  19.31
## 91              Kuwait Eastern Mediterranean       3250   24.90   3.80
## 142 Russian Federation                Europe     143000   15.45  18.60
## 52  Dominican Republic              Americas      10277   30.53   8.97
##     FertilityRate LifeExpectancy ChildMortality CellularSubscribers
## 129          3.35             67           85.9               61.61
## 98           2.47             65           15.4              155.70
## 16           1.47             71            5.2              111.88
## 91           2.65             80           11.0              175.09
## 142          1.51             69           10.3              179.31
## 52           2.55             73           27.1               87.22
##     LiteracyRate   GNI PrimarySchoolEnrollmentMale
## 129           NA  2870                        81.3
## 98          89.2    NA                          NA
## 16            NA 14460                          NA
## 91            NA    NA                          NA
## 142         99.6 20560                          NA
## 52          89.5  9420                        95.5
##     PrimarySchoolEnrollmentFemale .src     .rnorm FertilityRate.nonNA
## 129                          66.5 Test 0.07455118                3.35
## 98                             NA Test 0.68430943                2.47
## 16                             NA Test 0.48545998                1.47
## 91                             NA Test 0.96252797                2.65
## 142                            NA Test 0.73649596                1.51
## 52                           90.4 Test 1.08079950                2.55
##     CellularSubscribers.nonNA LiteracyRate.nonNA GNI.nonNA
## 129                     61.61               56.2      2870
## 98                     155.70               89.2     10440
## 16                     111.88               93.2     14460
## 91                     175.09               96.3      3640
## 142                    179.31               99.6     20560
## 52                      87.22               89.5      9420
##     PrimarySchoolEnrollmentMale.nonNA PrimarySchoolEnrollmentFemale.nonNA
## 129                              81.3                                66.5
## 98                               98.9                                99.3
## 16                               96.5                                96.5
## 91                               96.5                                97.0
## 142                              97.0                                97.3
## 52                               95.5                                90.4
##               Region.fctr LifeExpectancy.predict.All.X.no.rnorm.rf
## 129 Eastern Mediterranean                                 55.39683
## 98  Eastern Mediterranean                                 73.97303
## 16                 Europe                                 78.17203
## 91  Eastern Mediterranean                                 73.98070
## 142                Europe                                 74.07800
## 52               Americas                                 67.95067
##     LifeExpectancy.predict.All.X.no.rnorm.rf.err
## 129                                    11.603167
## 98                                      8.973033
## 16                                      7.172033
## 91                                      6.019300
## 142                                     5.078000
## 52                                      5.049333
```

```r
predct_accurate_var_name <- paste0(glb_rsp_var_out, glb_sel_mdl_id, ".accurate")
glb_OOBobs_df[, predct_accurate_var_name] <-
                    (glb_OOBobs_df[, glb_rsp_var] == 
                     glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)])

#stop(here"); sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
glb_featsimp_df <- 
    myget_feats_importance(mdl=glb_sel_mdl, featsimp_df=NULL)
glb_featsimp_df[, paste0(glb_sel_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##                                       importance
## ChildMortality                      100.00000000
## GNI.nonNA                             2.43788756
## LiteracyRate.nonNA                    0.92477967
## FertilityRate.nonNA                   0.71952386
## Over60                                0.69741081
## Population                            0.61068053
## PrimarySchoolEnrollmentMale.nonNA     0.53976660
## Under15                               0.52805249
## Region.fctrAfrica                     0.52203704
## PrimarySchoolEnrollmentFemale.nonNA   0.51930063
## CellularSubscribers.nonNA             0.46309748
## Region.fctrAmericas                   0.12263012
## Region.fctrSouth-East Asia            0.03026750
## Region.fctrWestern Pacific            0.02854844
## Region.fctrEurope                     0.02524768
##                                     All.X.no.rnorm.rf.importance
## ChildMortality                                      100.00000000
## GNI.nonNA                                             2.43788756
## LiteracyRate.nonNA                                    0.92477967
## FertilityRate.nonNA                                   0.71952386
## Over60                                                0.69741081
## Population                                            0.61068053
## PrimarySchoolEnrollmentMale.nonNA                     0.53976660
## Under15                                               0.52805249
## Region.fctrAfrica                                     0.52203704
## PrimarySchoolEnrollmentFemale.nonNA                   0.51930063
## CellularSubscribers.nonNA                             0.46309748
## Region.fctrAmericas                                   0.12263012
## Region.fctrSouth-East Asia                            0.03026750
## Region.fctrWestern Pacific                            0.02854844
## Region.fctrEurope                                     0.02524768
```

```r
# Used again in fit.data.training & predict.data.new chunks
glb_analytics_diag_plots <- function(obs_df, mdl_id, prob_threshold=NULL) {
    featsimp_df <- glb_featsimp_df
    featsimp_df$feat <- gsub("`(.*?)`", "\\1", row.names(featsimp_df))    
    featsimp_df$feat.interact <- gsub("(.*?):(.*)", "\\2", featsimp_df$feat)
    featsimp_df$feat <- gsub("(.*?):(.*)", "\\1", featsimp_df$feat)    
    featsimp_df$feat.interact <- ifelse(featsimp_df$feat.interact == featsimp_df$feat, 
                                        NA, featsimp_df$feat.interact)
    featsimp_df$feat <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat)
    featsimp_df$feat.interact <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat.interact) 
    featsimp_df <- orderBy(~ -importance.max, summaryBy(importance ~ feat + feat.interact, 
                                                        data=featsimp_df, FUN=max))    
    #rex_str=":(.*)"; txt_vctr=tail(featsimp_df$feat); ret_lst <- regexec(rex_str, txt_vctr); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])    
    if (nrow(featsimp_df) > 5) {
        warning("Limiting important feature scatter plots to 5 out of ", nrow(featsimp_df))
        featsimp_df <- head(featsimp_df, 5)
    }
#     if (!all(is.na(featsimp_df$feat.interact)))
#         stop("not implemented yet")
    rsp_var_out <- paste0(glb_rsp_var_out, mdl_id)
    for (var in featsimp_df$feat) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_rsp_var, rsp_var_out))
#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", colorcol_name="variable",
                                 facet_colcol_name="variable", jitter=TRUE) + 
                      guides(color=FALSE))
    }
    
    if (glb_is_regression) {
        if (nrow(featsimp_df) == 0)
            warning("No important features in glb_fin_mdl") else
            print(myplot_prediction_regression(df=obs_df, 
                        feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2],
                                      ".rownames"), 
                                               feat_y=featsimp_df$feat[1],
                        rsp_var=glb_rsp_var, rsp_var_out=rsp_var_out,
                        id_vars=glb_id_vars)
    #               + facet_wrap(reformulate(featsimp_df$feat[2])) # if [1 or 2] is a factor
    #               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
                  )
    }    
    
    if (glb_is_classification) {
        if (nrow(featsimp_df) == 0)
            warning("No features in selected model are statistically important")
        else print(myplot_prediction_classification(df=obs_df, 
                feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2], 
                              ".rownames"),
                                               feat_y=featsimp_df$feat[1],
                     rsp_var=glb_rsp_var, 
                     rsp_var_out=rsp_var_out, 
                     id_vars=glb_id_vars,
                    prob_threshold=prob_threshold)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id)                  
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_OOBobs_df, mdl_id =
## glb_sel_mdl_id): Limiting important feature scatter plots to 5 out of 11
```

![](WHO2_files/figure-html/fit.models_2-6.png) ![](WHO2_files/figure-html/fit.models_2-7.png) ![](WHO2_files/figure-html/fit.models_2-8.png) ![](WHO2_files/figure-html/fit.models_2-9.png) ![](WHO2_files/figure-html/fit.models_2-10.png) 

```
##                Country                Region Population Under15 Over60
## 129           Pakistan Eastern Mediterranean     179000   34.31   6.44
## 98               Libya Eastern Mediterranean       6155   29.45   6.96
## 16             Belarus                Europe       9405   15.10  19.31
## 91              Kuwait Eastern Mediterranean       3250   24.90   3.80
## 142 Russian Federation                Europe     143000   15.45  18.60
##     FertilityRate LifeExpectancy ChildMortality CellularSubscribers
## 129          3.35             67           85.9               61.61
## 98           2.47             65           15.4              155.70
## 16           1.47             71            5.2              111.88
## 91           2.65             80           11.0              175.09
## 142          1.51             69           10.3              179.31
##     LiteracyRate   GNI PrimarySchoolEnrollmentMale
## 129           NA  2870                        81.3
## 98          89.2    NA                          NA
## 16            NA 14460                          NA
## 91            NA    NA                          NA
## 142         99.6 20560                          NA
##     PrimarySchoolEnrollmentFemale .src     .rnorm FertilityRate.nonNA
## 129                          66.5 Test 0.07455118                3.35
## 98                             NA Test 0.68430943                2.47
## 16                             NA Test 0.48545998                1.47
## 91                             NA Test 0.96252797                2.65
## 142                            NA Test 0.73649596                1.51
##     CellularSubscribers.nonNA LiteracyRate.nonNA GNI.nonNA
## 129                     61.61               56.2      2870
## 98                     155.70               89.2     10440
## 16                     111.88               93.2     14460
## 91                     175.09               96.3      3640
## 142                    179.31               99.6     20560
##     PrimarySchoolEnrollmentMale.nonNA PrimarySchoolEnrollmentFemale.nonNA
## 129                              81.3                                66.5
## 98                               98.9                                99.3
## 16                               96.5                                96.5
## 91                               96.5                                97.0
## 142                              97.0                                97.3
##               Region.fctr LifeExpectancy.predict.All.X.no.rnorm.rf
## 129 Eastern Mediterranean                                 55.39683
## 98  Eastern Mediterranean                                 73.97303
## 16                 Europe                                 78.17203
## 91  Eastern Mediterranean                                 73.98070
## 142                Europe                                 74.07800
##     LifeExpectancy.predict.All.X.no.rnorm.rf.err
## 129                                    11.603167
## 98                                      8.973033
## 16                                      7.172033
## 91                                      6.019300
## 142                                     5.078000
##     LifeExpectancy.predict.All.X.no.rnorm.rf.accurate             .label
## 129                                             FALSE           Pakistan
## 98                                              FALSE              Libya
## 16                                              FALSE            Belarus
## 91                                              FALSE             Kuwait
## 142                                             FALSE Russian Federation
```

![](WHO2_files/figure-html/fit.models_2-11.png) 

```r
# gather predictions from models better than MFO.*
#mdl_id <- "Conditional.X.rf"
#mdl_id <- "Conditional.X.cp.0.rpart"
#mdl_id <- "Conditional.X.rpart"
# glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id,
#                                      glb_rsp_var_out)
# print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, mdl_id)], 
#                         glb_OOBobs_df[, glb_rsp_var])$table))
FN_OOB_ids <- c(4721, 4020, 693, 92)
print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                    grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
```

```
## [1] LifeExpectancy                                   
## [2] LifeExpectancy.predict.All.X.no.rnorm.rf         
## [3] LifeExpectancy.predict.All.X.no.rnorm.rf.err     
## [4] LifeExpectancy.predict.All.X.no.rnorm.rf.accurate
## <0 rows> (or 0-length row.names)
```

```r
print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                    glb_feats_df$id[1:5]])
```

```
## [1] LiteracyRate                  PrimarySchoolEnrollmentFemale
## [3] Over60                        GNI                          
## [5] LiteracyRate.nonNA           
## <0 rows> (or 0-length row.names)
```

```r
print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                    glb_txt_vars])
```

```
## data frame with 0 columns and 0 rows
```

```r
write.csv(glb_OOBobs_df[, c(glb_id_vars, 
                grep(glb_rsp_var, names(glb_OOBobs_df), fixed=TRUE, value=TRUE))], 
    paste0(gsub(".", "_", paste0(glb_out_pfx, glb_sel_mdl_id), fixed=TRUE), 
           "_OOBobs.csv"), row.names=FALSE)

# print(glb_allobs_df[glb_allobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_txt_vars])
# dsp_tbl(Headline.contains="[Ee]bola")
# sum(sel_obs(Headline.contains="[Ee]bola"))
# ftable(xtabs(Popular ~ NewsDesk.fctr, data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,]))
# xtabs(NewsDesk ~ Popular, #Popular ~ NewsDesk.fctr, 
#       data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,],
#       exclude=NULL)
# print(mycreate_xtab_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], 
#                       tbl_col_names=c("Popular", "NewsDesk")))

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn    end elapsed
## 12 fit.models          7          2 55.375 64.353   8.978
## 13 fit.models          7          3 64.353     NA      NA
```


```r
print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## [1] "LifeExpectancy.predict.All.X.no.rnorm.rf"         
## [2] "LifeExpectancy.predict.All.X.no.rnorm.rf.err"     
## [3] "LifeExpectancy.predict.All.X.no.rnorm.rf.accurate"
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_sel_mdl, glb_sel_mdl_id,
         glb_model_type,
        file=paste0(glb_out_pfx, "selmdl_dsk.RData"))
#load(paste0(glb_out_pfx, "selmdl_dsk.RData"))

rm(ret_lst)
```

```
## Warning in rm(ret_lst): object 'ret_lst' not found
```

```r
replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "model.selected")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0
```

![](WHO2_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor    bgn   end elapsed
## 13        fit.models          7          3 64.353 67.65   3.298
## 14 fit.data.training          8          0 67.651    NA      NA
```

## Step `8.0: fit data training`

```r
#load(paste0(glb_inp_pfx, "dsk.RData"))

# To create specific models
# glb_fin_mdl_id <- NULL; glb_fin_mdl <- NULL; 
# glb_sel_mdl_id <- "Conditional.X.cp.0.rpart"; 
# glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]]; print(glb_sel_mdl)
    
if (!is.null(glb_fin_mdl_id) && (glb_fin_mdl_id %in% names(glb_models_lst))) {
    warning("Final model same as user selected model")
    glb_fin_mdl <- glb_sel_mdl
} else {    
#     print(mdl_feats_df <- myextract_mdl_feats(sel_mdl=glb_sel_mdl, 
#                                               entity_df=glb_fitobs_df))
    
    if ((model_method <- glb_sel_mdl$method) == "custom")
        # get actual method from the model_id
        model_method <- tail(unlist(strsplit(glb_sel_mdl_id, "[.]")), 1)
        
    tune_finmdl_df <- NULL
    if (nrow(glb_sel_mdl$bestTune) > 0) {
        for (param in names(glb_sel_mdl$bestTune)) {
            #print(sprintf("param: %s", param))
            if (glb_sel_mdl$bestTune[1, param] != "none")
                tune_finmdl_df <- rbind(tune_finmdl_df, 
                    data.frame(parameter=param, 
                               min=glb_sel_mdl$bestTune[1, param], 
                               max=glb_sel_mdl$bestTune[1, param], 
                               by=1)) # by val does not matter
        }
    } 
    
    # Sync with parameters in mydsutils.R
    require(gdata)
    ret_lst <- myfit_mdl(model_id="Final", model_method=model_method,
        indep_vars_vctr=trim(unlist(strsplit(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id,
                                                    "feats"], "[,]"))), 
                         model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out, 
                            fit_df=glb_trnobs_df, OOB_df=NULL,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=tune_finmdl_df,
                         # Automate from here
                         #  Issues if glb_sel_mdl$method == "rf" b/c trainControl is "oob"; not "cv"
                            model_loss_mtrx=glb_model_metric_terms,
                            model_summaryFunction=glb_sel_mdl$control$summaryFunction,
                            model_metric=glb_sel_mdl$metric,
                            model_metric_maximize=glb_sel_mdl$maximize)
    glb_fin_mdl <- glb_models_lst[[length(glb_models_lst)]] 
    glb_fin_mdl_id <- glb_models_df[length(glb_models_lst), "model_id"]
}
```

```
## Loading required package: gdata
## gdata: read.xls support for 'XLS' (Excel 97-2004) files ENABLED.
## 
## gdata: read.xls support for 'XLSX' (Excel 2007+) files ENABLED.
## 
## Attaching package: 'gdata'
## 
## The following object is masked from 'package:randomForest':
## 
##     combine
## 
## The following object is masked from 'package:stats':
## 
##     nobs
## 
## The following object is masked from 'package:utils':
## 
##     object.size
```

```
## [1] "fitting model: Final.rf"
## [1] "    indep_vars: Over60, LiteracyRate.nonNA, PrimarySchoolEnrollmentFemale.nonNA, GNI.nonNA, CellularSubscribers.nonNA, PrimarySchoolEnrollmentMale.nonNA, Population, Region.fctr, FertilityRate.nonNA, Under15, ChildMortality"
## Aggregating results
## Fitting final model on full training set
```

![](WHO2_files/figure-html/fit.data.training_0-1.png) 

```
##                 Length Class      Mode     
## call              4    -none-     call     
## type              1    -none-     character
## predicted       139    -none-     numeric  
## mse             500    -none-     numeric  
## rsq             500    -none-     numeric  
## oob.times       139    -none-     numeric  
## importance       15    -none-     numeric  
## importanceSD      0    -none-     NULL     
## localImportance   0    -none-     NULL     
## proximity         0    -none-     NULL     
## ntree             1    -none-     numeric  
## mtry              1    -none-     numeric  
## forest           11    -none-     list     
## coefs             0    -none-     NULL     
## y               139    -none-     numeric  
## test              0    -none-     NULL     
## inbag             0    -none-     NULL     
## xNames           15    -none-     character
## problemType       1    -none-     character
## tuneValue         1    data.frame list     
## obsLevels         1    -none-     logical  
## [1] "    calling mypredict_mdl for fit:"
##   model_id model_method
## 1 Final.rf           rf
##                                                                                                                                                                                                             feats
## 1 Over60, LiteracyRate.nonNA, PrimarySchoolEnrollmentFemale.nonNA, GNI.nonNA, CellularSubscribers.nonNA, PrimarySchoolEnrollmentMale.nonNA, Population, Region.fctr, FertilityRate.nonNA, Under15, ChildMortality
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.718                 0.385
##   max.R.sq.fit min.RMSE.fit max.Rsquared.fit min.RMSESD.fit
## 1    0.9849445     2.945254         0.912703      0.2981539
##   max.RsquaredSD.fit
## 1         0.03665828
```

```r
rm(ret_lst)
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor    bgn    end elapsed
## 14 fit.data.training          8          0 67.651 71.905   4.255
## 15 fit.data.training          8          1 71.906     NA      NA
```


```r
glb_trnobs_df <- glb_get_predictions(df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "opt.prob.threshold.OOB"], NULL))
```

![](WHO2_files/figure-html/fit.data.training_1-1.png) 

```
##              Country                Region Population Under15 Over60
## 166        Swaziland                Africa       1231   38.05   5.34
## 107 Marshall Islands       Western Pacific         53   30.10   8.84
## 160     South Africa                Africa      52386   29.53   8.44
## 88        Kazakhstan                Europe      16271   25.46  10.04
## 73            Guyana              Americas        795   36.77   5.18
## 1        Afghanistan Eastern Mediterranean      29825   47.42   3.82
##     FertilityRate LifeExpectancy ChildMortality CellularSubscribers
## 166          3.48             50           79.7               63.70
## 107            NA             60           37.9                  NA
## 160          2.44             58           44.6              126.83
## 88           2.52             67           18.7              155.74
## 73           2.64             63           35.2               69.94
## 1            5.40             60           98.5               54.26
##     LiteracyRate   GNI PrimarySchoolEnrollmentMale
## 166         87.4  5930                          NA
## 107           NA    NA                          NA
## 160           NA 10710                          NA
## 88          99.7 11250                          NA
## 73            NA    NA                        82.4
## 1             NA  1140                          NA
##     PrimarySchoolEnrollmentFemale  .src     .rnorm FertilityRate.nonNA
## 166                            NA Train  0.6179858                3.48
## 107                            NA Train  0.3104807                2.53
## 160                            NA Train  1.6756969                2.44
## 88                             NA Train -1.0491770                2.52
## 73                           85.9 Train  0.7690422                2.64
## 1                              NA Train -0.3475426                5.40
##     CellularSubscribers.nonNA LiteracyRate.nonNA GNI.nonNA
## 166                     63.70               87.4      5930
## 107                    104.55               89.5       540
## 160                    126.83               84.5     10710
## 88                     155.74               99.7     11250
## 73                      69.94               82.6      5930
## 1                       54.26               56.0      1140
##     PrimarySchoolEnrollmentMale.nonNA PrimarySchoolEnrollmentFemale.nonNA
## 166                              93.9                                94.5
## 107                              93.3                                94.5
## 160                              93.3                                94.4
## 88                               99.2                                99.2
## 73                               82.4                                85.9
## 1                                85.3                                79.5
##               Region.fctr LifeExpectancy.predict.Final.rf
## 166                Africa                        53.72063
## 107       Western Pacific                        63.21903
## 160                Africa                        61.14077
## 88                 Europe                        70.03887
## 73               Americas                        65.59923
## 1   Eastern Mediterranean                        57.41310
##     LifeExpectancy.predict.Final.rf.err
## 166                            3.720633
## 107                            3.219033
## 160                            3.140767
## 88                             3.038867
## 73                             2.599233
## 1                              2.586900
```

```r
sav_featsimp_df <- glb_featsimp_df
#glb_feats_df <- sav_feats_df
# glb_feats_df <- mymerge_feats_importance(feats_df=glb_feats_df, sel_mdl=glb_fin_mdl, 
#                                                entity_df=glb_trnobs_df)
glb_featsimp_df <- myget_feats_importance(mdl=glb_fin_mdl, featsimp_df=glb_featsimp_df)
glb_featsimp_df[, paste0(glb_fin_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##                                     All.X.no.rnorm.rf.importance
## ChildMortality                                      100.00000000
## GNI.nonNA                                             2.43788756
## LiteracyRate.nonNA                                    0.92477967
## FertilityRate.nonNA                                   0.71952386
## Over60                                                0.69741081
## Population                                            0.61068053
## PrimarySchoolEnrollmentMale.nonNA                     0.53976660
## Under15                                               0.52805249
## Region.fctrAfrica                                     0.52203704
## PrimarySchoolEnrollmentFemale.nonNA                   0.51930063
## CellularSubscribers.nonNA                             0.46309748
## Region.fctrAmericas                                   0.12263012
## Region.fctrSouth-East Asia                            0.03026750
## Region.fctrWestern Pacific                            0.02854844
## Region.fctrEurope                                     0.02524768
##                                       importance Final.rf.importance
## ChildMortality                      100.00000000        100.00000000
## GNI.nonNA                             2.43788756          2.43788756
## LiteracyRate.nonNA                    0.92477967          0.92477967
## FertilityRate.nonNA                   0.71952386          0.71952386
## Over60                                0.69741081          0.69741081
## Population                            0.61068053          0.61068053
## PrimarySchoolEnrollmentMale.nonNA     0.53976660          0.53976660
## Under15                               0.52805249          0.52805249
## Region.fctrAfrica                     0.52203704          0.52203704
## PrimarySchoolEnrollmentFemale.nonNA   0.51930063          0.51930063
## CellularSubscribers.nonNA             0.46309748          0.46309748
## Region.fctrAmericas                   0.12263012          0.12263012
## Region.fctrSouth-East Asia            0.03026750          0.03026750
## Region.fctrWestern Pacific            0.02854844          0.02854844
## Region.fctrEurope                     0.02524768          0.02524768
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id)                  
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_trnobs_df, mdl_id =
## glb_fin_mdl_id): Limiting important feature scatter plots to 5 out of 11
```

![](WHO2_files/figure-html/fit.data.training_1-2.png) ![](WHO2_files/figure-html/fit.data.training_1-3.png) ![](WHO2_files/figure-html/fit.data.training_1-4.png) ![](WHO2_files/figure-html/fit.data.training_1-5.png) ![](WHO2_files/figure-html/fit.data.training_1-6.png) 

```
##              Country          Region Population Under15 Over60
## 166        Swaziland          Africa       1231   38.05   5.34
## 107 Marshall Islands Western Pacific         53   30.10   8.84
## 160     South Africa          Africa      52386   29.53   8.44
## 88        Kazakhstan          Europe      16271   25.46  10.04
## 73            Guyana        Americas        795   36.77   5.18
##     FertilityRate LifeExpectancy ChildMortality CellularSubscribers
## 166          3.48             50           79.7               63.70
## 107            NA             60           37.9                  NA
## 160          2.44             58           44.6              126.83
## 88           2.52             67           18.7              155.74
## 73           2.64             63           35.2               69.94
##     LiteracyRate   GNI PrimarySchoolEnrollmentMale
## 166         87.4  5930                          NA
## 107           NA    NA                          NA
## 160           NA 10710                          NA
## 88          99.7 11250                          NA
## 73            NA    NA                        82.4
##     PrimarySchoolEnrollmentFemale  .src     .rnorm FertilityRate.nonNA
## 166                            NA Train  0.6179858                3.48
## 107                            NA Train  0.3104807                2.53
## 160                            NA Train  1.6756969                2.44
## 88                             NA Train -1.0491770                2.52
## 73                           85.9 Train  0.7690422                2.64
##     CellularSubscribers.nonNA LiteracyRate.nonNA GNI.nonNA
## 166                     63.70               87.4      5930
## 107                    104.55               89.5       540
## 160                    126.83               84.5     10710
## 88                     155.74               99.7     11250
## 73                      69.94               82.6      5930
##     PrimarySchoolEnrollmentMale.nonNA PrimarySchoolEnrollmentFemale.nonNA
## 166                              93.9                                94.5
## 107                              93.3                                94.5
## 160                              93.3                                94.4
## 88                               99.2                                99.2
## 73                               82.4                                85.9
##         Region.fctr LifeExpectancy.predict.Final.rf
## 166          Africa                        53.72063
## 107 Western Pacific                        63.21903
## 160          Africa                        61.14077
## 88           Europe                        70.03887
## 73         Americas                        65.59923
##     LifeExpectancy.predict.Final.rf.err           .label
## 166                            3.720633        Swaziland
## 107                            3.219033 Marshall Islands
## 160                            3.140767     South Africa
## 88                             3.038867       Kazakhstan
## 73                             2.599233           Guyana
```

![](WHO2_files/figure-html/fit.data.training_1-7.png) 

```r
dsp_feats_vctr <- c(NULL)
for(var in grep(".importance", names(glb_feats_df), fixed=TRUE, value=TRUE))
    dsp_feats_vctr <- union(dsp_feats_vctr, 
                            glb_feats_df[!is.na(glb_feats_df[, var]), "id"])

print(glb_trnobs_df[glb_trnobs_df$UniqueID %in% FN_OOB_ids, 
                    grep(glb_rsp_var, names(glb_trnobs_df), value=TRUE)])
```

```
## [1] LifeExpectancy                      LifeExpectancy.predict.Final.rf    
## [3] LifeExpectancy.predict.Final.rf.err
## <0 rows> (or 0-length row.names)
```

```r
print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## [1] "LifeExpectancy.predict.Final.rf"    
## [2] "LifeExpectancy.predict.Final.rf.err"
```

```r
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all.prediction","model.final")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0 
## 3.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  data.training.all.prediction 
## 4.0000 	 5 	 0 1 1 1 
## 4.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  model.final 
## 5.0000 	 4 	 0 0 2 1
```

![](WHO2_files/figure-html/fit.data.training_1-8.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor    bgn    end elapsed
## 15 fit.data.training          8          1 71.906 75.466    3.56
## 16  predict.data.new          9          0 75.467     NA      NA
```

## Step `9.0: predict data new`

```r
# Compute final model predictions
glb_newobs_df <- glb_get_predictions(glb_newobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                      "opt.prob.threshold.OOB"], NULL))
```

![](WHO2_files/figure-html/predict.data.new-1.png) 

```
##                Country                Region Population Under15 Over60
## 129           Pakistan Eastern Mediterranean     179000   34.31   6.44
## 98               Libya Eastern Mediterranean       6155   29.45   6.96
## 16             Belarus                Europe       9405   15.10  19.31
## 91              Kuwait Eastern Mediterranean       3250   24.90   3.80
## 142 Russian Federation                Europe     143000   15.45  18.60
## 52  Dominican Republic              Americas      10277   30.53   8.97
##     FertilityRate LifeExpectancy ChildMortality CellularSubscribers
## 129          3.35             67           85.9               61.61
## 98           2.47             65           15.4              155.70
## 16           1.47             71            5.2              111.88
## 91           2.65             80           11.0              175.09
## 142          1.51             69           10.3              179.31
## 52           2.55             73           27.1               87.22
##     LiteracyRate   GNI PrimarySchoolEnrollmentMale
## 129           NA  2870                        81.3
## 98          89.2    NA                          NA
## 16            NA 14460                          NA
## 91            NA    NA                          NA
## 142         99.6 20560                          NA
## 52          89.5  9420                        95.5
##     PrimarySchoolEnrollmentFemale .src     .rnorm FertilityRate.nonNA
## 129                          66.5 Test 0.07455118                3.35
## 98                             NA Test 0.68430943                2.47
## 16                             NA Test 0.48545998                1.47
## 91                             NA Test 0.96252797                2.65
## 142                            NA Test 0.73649596                1.51
## 52                           90.4 Test 1.08079950                2.55
##     CellularSubscribers.nonNA LiteracyRate.nonNA GNI.nonNA
## 129                     61.61               56.2      2870
## 98                     155.70               89.2     10440
## 16                     111.88               93.2     14460
## 91                     175.09               96.3      3640
## 142                    179.31               99.6     20560
## 52                      87.22               89.5      9420
##     PrimarySchoolEnrollmentMale.nonNA PrimarySchoolEnrollmentFemale.nonNA
## 129                              81.3                                66.5
## 98                               98.9                                99.3
## 16                               96.5                                96.5
## 91                               96.5                                97.0
## 142                              97.0                                97.3
## 52                               95.5                                90.4
##               Region.fctr LifeExpectancy.predict.Final.rf
## 129 Eastern Mediterranean                        55.39683
## 98  Eastern Mediterranean                        73.97303
## 16                 Europe                        78.17203
## 91  Eastern Mediterranean                        73.98070
## 142                Europe                        74.07800
## 52               Americas                        67.95067
##     LifeExpectancy.predict.Final.rf.err
## 129                           11.603167
## 98                             8.973033
## 16                             7.172033
## 91                             6.019300
## 142                            5.078000
## 52                             5.049333
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id)                  
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_newobs_df, mdl_id =
## glb_fin_mdl_id): Limiting important feature scatter plots to 5 out of 11
```

![](WHO2_files/figure-html/predict.data.new-2.png) ![](WHO2_files/figure-html/predict.data.new-3.png) ![](WHO2_files/figure-html/predict.data.new-4.png) ![](WHO2_files/figure-html/predict.data.new-5.png) ![](WHO2_files/figure-html/predict.data.new-6.png) 

```
##                Country                Region Population Under15 Over60
## 129           Pakistan Eastern Mediterranean     179000   34.31   6.44
## 98               Libya Eastern Mediterranean       6155   29.45   6.96
## 16             Belarus                Europe       9405   15.10  19.31
## 91              Kuwait Eastern Mediterranean       3250   24.90   3.80
## 142 Russian Federation                Europe     143000   15.45  18.60
##     FertilityRate LifeExpectancy ChildMortality CellularSubscribers
## 129          3.35             67           85.9               61.61
## 98           2.47             65           15.4              155.70
## 16           1.47             71            5.2              111.88
## 91           2.65             80           11.0              175.09
## 142          1.51             69           10.3              179.31
##     LiteracyRate   GNI PrimarySchoolEnrollmentMale
## 129           NA  2870                        81.3
## 98          89.2    NA                          NA
## 16            NA 14460                          NA
## 91            NA    NA                          NA
## 142         99.6 20560                          NA
##     PrimarySchoolEnrollmentFemale .src     .rnorm FertilityRate.nonNA
## 129                          66.5 Test 0.07455118                3.35
## 98                             NA Test 0.68430943                2.47
## 16                             NA Test 0.48545998                1.47
## 91                             NA Test 0.96252797                2.65
## 142                            NA Test 0.73649596                1.51
##     CellularSubscribers.nonNA LiteracyRate.nonNA GNI.nonNA
## 129                     61.61               56.2      2870
## 98                     155.70               89.2     10440
## 16                     111.88               93.2     14460
## 91                     175.09               96.3      3640
## 142                    179.31               99.6     20560
##     PrimarySchoolEnrollmentMale.nonNA PrimarySchoolEnrollmentFemale.nonNA
## 129                              81.3                                66.5
## 98                               98.9                                99.3
## 16                               96.5                                96.5
## 91                               96.5                                97.0
## 142                              97.0                                97.3
##               Region.fctr LifeExpectancy.predict.Final.rf
## 129 Eastern Mediterranean                        55.39683
## 98  Eastern Mediterranean                        73.97303
## 16                 Europe                        78.17203
## 91  Eastern Mediterranean                        73.98070
## 142                Europe                        74.07800
##     LifeExpectancy.predict.Final.rf.err             .label
## 129                           11.603167           Pakistan
## 98                             8.973033              Libya
## 16                             7.172033            Belarus
## 91                             6.019300             Kuwait
## 142                            5.078000 Russian Federation
```

![](WHO2_files/figure-html/predict.data.new-7.png) 

```r
if (glb_is_classification && glb_is_binomial) {
    submit_df <- glb_newobs_df[, c(glb_id_vars, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id, ".prob"))]
    names(submit_df)[2] <- "Probability1"
} else submit_df <- glb_newobs_df[, c(glb_id_vars, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id))]
write.csv(submit_df, 
    paste0(gsub(".", "_", paste0(glb_out_pfx, glb_fin_mdl_id), fixed=TRUE), 
           "_submit.csv"), row.names=FALSE)

# print(orderBy(~ -max.auc.OOB, glb_models_df[, c("model_id", 
#             "max.auc.OOB", "max.Accuracy.OOB")]))
if (glb_is_classification && glb_is_binomial)
    print(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                        "opt.prob.threshold.OOB"])
print(sprintf("glb_sel_mdl_id: %s", glb_sel_mdl_id))
```

```
## [1] "glb_sel_mdl_id: All.X.no.rnorm.rf"
```

```r
print(sprintf("glb_fin_mdl_id: %s", glb_fin_mdl_id))
```

```
## [1] "glb_fin_mdl_id: Final.rf"
```

```r
print(dim(glb_fitobs_df))
```

```
## [1] 139  22
```

```r
print(dsp_models_df)
```

```
##                                            model_id min.RMSE.OOB
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf     3.066581
## All.X.lm                                   All.X.lm     3.119837
## All.X.glm                                 All.X.glm     3.119837
## Low.cor.X.lm                           Low.cor.X.lm     3.215729
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart     3.251075
## Max.cor.Y.lm                           Max.cor.Y.lm     3.375750
## Interact.High.cor.Y.lm       Interact.High.cor.Y.lm     3.486467
## Max.cor.Y.rpart                     Max.cor.Y.rpart     4.307076
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart     4.307076
## MFO.lm                                       MFO.lm     8.838414
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart     8.880209
##                           max.R.sq.OOB max.Adj.R.sq.fit
## All.X.no.rnorm.rf          0.880682838               NA
## All.X.lm                   0.876570916      0.914318813
## All.X.glm                  0.876570916               NA
## Low.cor.X.lm               0.868866804      0.908319490
## Max.cor.Y.cv.0.cp.0.rpart  0.865968304               NA
## Max.cor.Y.lm               0.855491202      0.899291417
## Interact.High.cor.Y.lm     0.845856653      0.906961104
## Max.cor.Y.rpart            0.764755963               NA
## All.X.no.rnorm.rpart       0.764755963               NA
## MFO.lm                     0.009390806     -0.006185647
## Max.cor.Y.cv.0.rpart       0.000000000               NA
```

```r
if (glb_is_classification) {
    print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id))
    print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
                            glb_OOBobs_df[, glb_rsp_var])$table))

    tmp_OOBobs_df <- glb_OOBobs_df[, c("myCategory", predct_accurate_var_name)]
    names(tmp_OOBobs_df)[2] <- "accurate.OOB"
    aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBobs_df, names(tmp_OOBobs_df)) 
    aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
    aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                            .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                            max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
    intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
    glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
    print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
}    

dsp_myCategory_conf_mtrx <- function(myCategory) {
    print(sprintf("%s OOB::myCategory=%s confusion matrix & accuracy: ", 
                  glb_sel_mdl_id, myCategory))
    print(t(confusionMatrix(
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                      paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, glb_rsp_var])$table))
    print(sum(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                            predct_accurate_var_name]) / 
         nrow(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, ]))
    err_ids <- glb_OOBobs_df[(glb_OOBobs_df$myCategory == myCategory) & 
                             (!glb_OOBobs_df[, predct_accurate_var_name]), glb_id_vars]

    OOB_FNerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 1), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FN errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FNerr_df)))
    print(OOB_FNerr_df)

    OOB_FPerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 0), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FP errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FPerr_df)))
    print(OOB_FPerr_df)
}
#dsp_myCategory_conf_mtrx(myCategory="OpEd#Opinion#")
#dsp_myCategory_conf_mtrx(myCategory="Business#Business Day#Dealbook")
#dsp_myCategory_conf_mtrx(myCategory="##")

if (glb_is_classification) {
    print("FN_OOB_ids:")
    print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                        grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
    print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                        glb_txt_vars])
    print(dsp_vctr <- colSums(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
                        setdiff(grep("[HSA].", names(glb_OOBobs_df), value=TRUE),
                                union(myfind_chr_cols_df(glb_OOBobs_df),
                    grep(".fctr", names(glb_OOBobs_df), fixed=TRUE, value=TRUE)))]))
}

dsp_hdlpfx_results <- function(hdlpfx) {
    print(hdlpfx)
    print(glb_OOBobs_df[glb_OOBobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_newobs_df), value=TRUE)])
    print(dsp_vctr <- colSums(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        setdiff(grep("[HSA]\\.", names(glb_newobs_df), value=TRUE),
                                union(myfind_chr_cols_df(glb_newobs_df),
                    grep(".fctr", names(glb_newobs_df), fixed=TRUE, value=TRUE)))]))
    print(dsp_vctr <- dsp_vctr[dsp_vctr != 0])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        union(names(dsp_vctr), myfind_chr_cols_df(glb_newobs_df))])
}
#dsp_hdlpfx_results(hdlpfx="Ask Well::")

# print("myMisc::|OpEd|blank|blank|1:")
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% c(6446), 
#                     grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])

# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     c("WordCount", "WordCount.log", "myMultimedia",
#                       "NewsDesk", "SectionName", "SubsectionName")])
# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), ], 
#                           c(glb_rsp_var, "myMultimedia")))
# dsp_chisq.test(Headline.contains="[Vi]deo")
# print(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline")])
# print(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola", Popular=1), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline",
#                             "NewsDesk", "SectionName", "SubsectionName")])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.ConditionalX.y & is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
print(orderBy(as.formula(paste0("~ -", glb_sel_mdl_id, ".importance")), glb_featsimp_df))
```

```
##                                     All.X.no.rnorm.rf.importance
## ChildMortality                                      100.00000000
## GNI.nonNA                                             2.43788756
## LiteracyRate.nonNA                                    0.92477967
## FertilityRate.nonNA                                   0.71952386
## Over60                                                0.69741081
## Population                                            0.61068053
## PrimarySchoolEnrollmentMale.nonNA                     0.53976660
## Under15                                               0.52805249
## Region.fctrAfrica                                     0.52203704
## PrimarySchoolEnrollmentFemale.nonNA                   0.51930063
## CellularSubscribers.nonNA                             0.46309748
## Region.fctrAmericas                                   0.12263012
## Region.fctrSouth-East Asia                            0.03026750
## Region.fctrWestern Pacific                            0.02854844
## Region.fctrEurope                                     0.02524768
##                                       importance Final.rf.importance
## ChildMortality                      100.00000000        100.00000000
## GNI.nonNA                             2.43788756          2.43788756
## LiteracyRate.nonNA                    0.92477967          0.92477967
## FertilityRate.nonNA                   0.71952386          0.71952386
## Over60                                0.69741081          0.69741081
## Population                            0.61068053          0.61068053
## PrimarySchoolEnrollmentMale.nonNA     0.53976660          0.53976660
## Under15                               0.52805249          0.52805249
## Region.fctrAfrica                     0.52203704          0.52203704
## PrimarySchoolEnrollmentFemale.nonNA   0.51930063          0.51930063
## CellularSubscribers.nonNA             0.46309748          0.46309748
## Region.fctrAmericas                   0.12263012          0.12263012
## Region.fctrSouth-East Asia            0.03026750          0.03026750
## Region.fctrWestern Pacific            0.02854844          0.02854844
## Region.fctrEurope                     0.02524768          0.02524768
```

```r
print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "prdnew_dsk.RData"))

rm(submit_df, tmp_OOBobs_df)
```

```
## Warning in rm(submit_df, tmp_OOBobs_df): object 'tmp_OOBobs_df' not found
```

```r
# tmp_replay_lst <- replay.petrisim(pn=glb_analytics_pn, 
#     replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
#         "data.new.prediction")), flip_coord=TRUE)
# print(ggplot.petrinet(tmp_replay_lst[["pn"]]) + coord_flip())

glb_chunks_df <- myadd_chunk(glb_chunks_df, "display.session.info", major.inc=TRUE)
```

```
##                   label step_major step_minor    bgn    end elapsed
## 16     predict.data.new          9          0 75.467 79.053   3.586
## 17 display.session.info         10          0 79.053     NA      NA
```

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 
#```{r q1, cache=FALSE}
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
#```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
##                      label step_major step_minor    bgn    end elapsed
## 2             inspect.data          2          0  8.260 22.189  13.929
## 10              fit.models          7          0 28.564 42.492  13.928
## 11              fit.models          7          1 42.492 55.374  12.883
## 12              fit.models          7          2 55.375 64.353   8.978
## 14       fit.data.training          8          0 67.651 71.905   4.255
## 16        predict.data.new          9          0 75.467 79.053   3.586
## 15       fit.data.training          8          1 71.906 75.466   3.560
## 13              fit.models          7          3 64.353 67.650   3.298
## 3             cleanse.data          2          1 22.189 25.021   2.832
## 4      manage.missing.data          2          2 25.022 26.252   1.230
## 6         extract.features          3          0 26.277 27.379   1.102
## 8          select.features          5          0 27.658 28.272   0.614
## 1              import.data          1          0  7.716  8.259   0.544
## 9  partition.data.training          6          0 28.272 28.564   0.292
## 7             cluster.data          4          0 27.379 27.658   0.279
## 5              encode.data          2          3 26.252 26.277   0.025
##    duration
## 2    13.929
## 10   13.928
## 11   12.882
## 12    8.978
## 14    4.254
## 16    3.586
## 15    3.560
## 13    3.297
## 3     2.832
## 4     1.230
## 6     1.102
## 8     0.614
## 1     0.543
## 9     0.292
## 7     0.279
## 5     0.025
## [1] "Total Elapsed Time: 79.053 secs"
```

![](WHO2_files/figure-html/display.session.info-1.png) 

```
##                label step_major step_minor    bgn    end elapsed duration
## 5    fit.models_1_rf          5          0 51.687 55.369   3.682    3.682
## 4 fit.models_1_rpart          4          0 48.936 51.687   2.751    2.751
## 3   fit.models_1_glm          3          0 46.642 48.936   2.294    2.294
## 2    fit.models_1_lm          2          0 44.407 46.642   2.235    2.235
## 1   fit.models_1_bgn          1          0 44.393 44.407   0.014    0.014
## [1] "Total Elapsed Time: 55.369 secs"
```

![](WHO2_files/figure-html/display.session.info-2.png) 

```
## R version 3.2.0 (2015-04-16)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.3 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] grid      parallel  stats     graphics  grDevices utils     datasets 
## [8] methods   base     
## 
## other attached packages:
##  [1] gdata_2.16.1        randomForest_4.6-10 rpart.plot_1.5.2   
##  [4] rpart_4.1-9         reshape2_1.4.1      mice_2.22          
##  [7] Rcpp_0.11.6         plyr_1.8.2          caTools_1.17.1     
## [10] doMC_1.3.3          iterators_1.0.7     foreach_1.4.2      
## [13] doBy_4.5-13         survival_2.38-1     caret_6.0-47       
## [16] ggplot2_1.0.1       lattice_0.20-31    
## 
## loaded via a namespace (and not attached):
##  [1] compiler_3.2.0      RColorBrewer_1.1-2  formatR_1.2        
##  [4] nloptr_1.0.4        bitops_1.0-6        tools_3.2.0        
##  [7] digest_0.6.8        lme4_1.1-7          evaluate_0.7       
## [10] nlme_3.1-120        gtable_0.1.2        mgcv_1.8-6         
## [13] Matrix_1.2-1        yaml_2.1.13         brglm_0.5-9        
## [16] SparseM_1.6         proto_0.3-10        BradleyTerry2_1.0-6
## [19] stringr_1.0.0       knitr_1.10.5        gtools_3.5.0       
## [22] nnet_7.3-9          rmarkdown_0.6.1     minqa_1.2.4        
## [25] car_2.0-25          magrittr_1.5        scales_0.2.4       
## [28] codetools_0.2-11    htmltools_0.2.6     MASS_7.3-40        
## [31] splines_3.2.0       pbkrtest_0.4-2      colorspace_1.2-6   
## [34] labeling_0.3        quantreg_5.11       stringi_0.4-1      
## [37] munsell_0.4.2
```
