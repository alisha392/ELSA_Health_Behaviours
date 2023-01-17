#Check line 60 before starting
# As Mplus requires that files not have a header row and that the variable names

main_data <- read_csv(here("Data", "Processed_Data", "R_Data", "Health_Outcomes", "Outcomes_Demographics_Behaviours_4to9.csv"))

## Dummy coding Sex with Male as baseline, so Female =1
main_data <- main_data %>% 
  dplyr::mutate(Female = ifelse(main_data$Sex<0, -99, ifelse(main_data$Sex == 2, 1, 0))) %>% 
  dplyr::select(-2) 

main_data <- main_data[, c(1,11:31,54,2:10,43:53,32:42)]
head(main_data)

main_data[main_data == -99] <- NA
Complete_case_data <- main_data[complete.cases(main_data[23:32]), ]
Complete_case_data[is.na(Complete_case_data)]<- -99


# Making all variable names fit within the 8-character name limit (Mplus)
# while avoiding duplicates
# using fucntion : rename(new_name = old_name)
main_data_new_names <- Complete_case_data %>%
  rename(
    Pat_Int = Paternal_Occ_Intermediate,
    Pat_Hig = Paternal_Occ_High,
    Self_Int = Self_Occ_Intermediate,
    Self_Hig = Self_Occ_High,
    Edu_Upp = Education_Upper_Second,
    Edu_Ter = Education_Tertiary,
    Wealth_2 = Wealth_Second_Tertile,
    Wealth_3 = Wealth_Third_Tertile,
    Multi = M_Present,
    CMulti = CM_Present,
    Eye = Eye_disorders,
    Circul = Circulatory_disorders,
    Endocr = Endocrine_nutritional_metabolic,
    Muscul = Musculoskeletal_connective_system,
    Neopla = Neoplasms,
    Mental = Mental_behavioural,
    Nervou = Nervous_disorders,
    Respir = Respiratory,
    Luncan = Lung_Cancer,
    Multi_4 = M_Present_W4,
    CMulti_4 = CM_Present_W4,
    Eye_4 = Eye_disorders_W4,
    Circul_4 = Circulatory_disorders_W4,
    Endocr_4 = Endocrine_nutritional_metabolic_W4,
    Muscul_4 = Musculoskeletal_connective_system_W4,
    Neopla_4 = Neoplasms_W4,
    Mental_4 = Mental_behavioural_W4,
    Nervou_4 = Nervous_disorders_W4,
    Respir_4 = Respiratory_W4,
    Luncan_4 = Lung_Cancer_W4
  )

#### Class enumeration #####
#setting to relative path as MPlus only accepts 90 characters in each line 
setwd(here("Data", "Processed_Data", "MPlus_Data","MPlus_3787cases","Complete_sample"))
getwd()
lca_k1_6 <- lapply(1:9, function(k) {
  lca_enum <- mplusObject(
    TITLE = glue("C{k}_LCA_enumerate"),
    VARIABLE =
      glue(
        "CATEGORICAL = W4_Smoke,W5_Smoke,W6_Smoke,W7_Smoke,W8_Smoke,
W4_Drink,W5_Drink,W6_Drink,W7_Drink,W8_Drink,
W4_PA,W5_PA,W6_PA,W7_PA,W8_PA,W4_FV,W5_FV,W6_FV,
W7_FV,W8_FV;
USEVARIABLES = W4_Smoke,W5_Smoke,W6_Smoke,W7_Smoke,W8_Smoke,
W4_Drink,W5_Drink,W6_Drink,W7_Drink,W8_Drink,
W4_PA,W5_PA,W6_PA,W7_PA,W8_PA,W4_FV,W5_FV,W6_FV,
W7_FV,W8_FV,Weights;
        WEIGHT        = Weights;
MISSING ARE ALL (-99);
        classes = c({k});"
      ),
    ANALYSIS =
      "estimator = mlr;
    type = mixture;
    starts = 500 100;
    STITERATIONS=20;
LRTSTARTS = 50 10 400 20;",
    MODEL = "",
    OUTPUT = "TECH10 TECH11 TECH14",
    PLOT =
      "type = plot3;
    series is W4_Smoke(2) W5_Smoke(3) W6_Smoke(4) W7_Smoke(5)
    W8_Smoke(6) W4_Drink(7) W5_Drink(8)
    W6_Drink(9) W7_Drink(10)
    W8_Drink(11) W4_PA(12) W5_PA(13)
    W6_PA(14) W7_PA(15) W8_PA(16)
    W4_FV(17) W5_FV(18) W6_FV(19)
    W7_FV(20) W8_FV(21);",
    usevariables = colnames(main_data_new_names),
    rdata = main_data_new_names
  )
  
  lca_enum_fit <- mplusModeler(lca_enum,
                               dataout = glue("./c_lca_enumerate.dat"),
                               modelout = glue("./c{k}_lca_enumerate.inp"),
                               check = TRUE, run = TRUE, hashfilename = FALSE
  )
  
  
}


)


##### Class enumeration results ####
setwd(here("Data", "Processed_Data", "MPlus_Data","MPlus_3787cases", "Complete_sample"))
getwd()


output_enum <- readModels("./")

enum_summary <- LatexSummaryTable(output_enum,
                                  keepCols = c(
                                    "Title",
                                    "LL", "AIC",
                                    "BIC",
                                    "aBIC", "Entropy", "T11_VLMR_PValue", "T11_BLRT_PValue"
                                  ),
                                  sortBy = "Title"
)
save_kable(enum_summary,here("Figures","3787_cases","Fit_Indices_Complete_Sample.png"))
Indices <- enum_summary %>%
  kable(booktabs = T, linesep = "") %>%
  kable_styling(c("striped"),
                full_width = F,
                position = "left"
  )
Indices <- save_kable(Indices,here("Figures","3787_cases","Fit_Indices_Complete_Sample.png"))


#### Plotting AIC, BIC, aBIC of models with different number of classes
results1 <- enum_summary[,c(1,3:5)]
classes <- as.character(c(1:9))
results1[,1]<-classes
#convert to long format
results2<-tidyr::gather(results1,"criterias","values", -Title)
results2$criterias<-as.factor(results2$criterias)

Screeplot <- qplot(Title, values, data = results2, color = factor(criterias))+
  geom_point()+
  geom_line(aes(group = criterias)) + 
  xlab("Latent classes") + 
  ylab("Values") +
  scale_color_discrete("Information criteria")+
  ggtitle("Scree Plot of complete sample (3787 cases)") 
ggsave(here("Figures","3787_cases","Scree_Plot_Complete_Sample.png"),Screeplot,width=30, height=22,dpi=300, units=c("cm"),limitsize = FALSE)
#### Plotting the log likelihood of models with different number of classes

results1 <- enum_summary[,c(1:2)]
classes <- as.character(c(1:9))
results1[,1]<-classes
#convert to long format
results2<-tidyr::gather(results1,"criterias","values", -Title)
results2$criterias<-as.factor(results2$criterias)

Loglikelihood <- qplot(Title, values, data = results2, color = factor(criterias))+
  geom_point()+
  geom_line(aes(group = criterias)) + 
  xlab("Latent classes") + 
  ylab("Values") +
  scale_color_discrete("Information criteria") +
  ggtitle("Loglikelihood Plot of complete sample (3787 cases)") 
ggsave(here("Figures","3787_cases","Loglikelihood_Plot_Complete_Sample.png"),Loglikelihood,width=30, height=22, dpi=300, units=c("cm"),limitsize = FALSE)

##### Step 1 on the  6-class model####
setwd(here("Data", "Processed_Data", "MPlus_Data","MPlus_3787cases", "Complete_sample_selected_model"))
getwd()

step1 <- mplusObject(
  TITLE = "Step1-3step-LSAY-Lab9;",
  VARIABLE =
    "CATEGORICAL = W4_Smoke-W8_FV;
  USEVARIABLES = W4_Smoke-W8_FV,Weights;
  CLASSES = c (6);
  WEIGHT        = Weights;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99
  AND  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  MISSING ARE ALL (-99);
  AUXILIARY = Female-Luncan;",
  ANALYSIS = "TYPE = MIXTURE;
  STARTS = 500 100;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  SAVEDATA = "FILE = step-1-outcomes.dat;
  SAVE = CPROB;
  Missflag= -99;",
  PLOT = "TYPE=PLOT3;
  series = W4_Smoke-W8_FV(*);",
  usevariables = colnames(main_data_new_names),
  rdata = main_data_new_names
)

step1_fit <- mplusModeler(step1,
                          modelout = "./Step1_Outcomes.inp",
                          check = TRUE, run = TRUE, hashfilename = TRUE
)

logit_cprobs <- as.data.frame(step1_fit[["results"]]
                              [["class_counts"]]
                              [["logitProbs.mostLikely"]])

savedata <- as.data.frame(step1_fit[["results"]]
                          [["savedata"]])

colnames(savedata)[colnames(savedata) == "C"] <- "N"


#### Step 2 #####
step2 <- mplusObject(
  TITLE = "Step2 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  USEVAR = N;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;",
  MODEL =
    glue(
      "  %C#1%
  [n#1@{logit_cprobs[1,1]}];
  [n#2@{logit_cprobs[1,2]}];
  [n#3@{logit_cprobs[1,3]}];
  [n#4@{logit_cprobs[1,4]}];
  [n#5@{logit_cprobs[1,5]}];

  %C#2%
 [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];

    %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];

   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];

  %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];"
    ),
  usevariables = colnames(savedata),
  rdata = savedata
)

step2_fit <- mplusModeler(step2,
                          dataout = "./Step2_Outcomes.dat",
                          modelout = "./Step2_Outcomes.inp",
                          check = TRUE, run = TRUE, hashfilename = FALSE
)

###  Step3 MULTI ####
# without controlling for covariates

step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = Multi;
  usevar =  Multi Female;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%
      
    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [Multi$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [Multi$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [Multi$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [Multi$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [Multi$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [Multi$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_multi <- mplusModeler(step3,
                                dataout = "./Step1_MPlus_Outcomes.dat",
                                modelout = "./Step3_Outcomes_MULTI.inp",
                                check = TRUE, run = TRUE, hashfilename = FALSE
)
####### Step3  CMULTI ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = CMULTI;
  usevar =  CMULTI;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%

    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [CMULTI$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [CMULTI$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [CMULTI$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [CMULTI$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [CMULTI$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [CMULTI$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_cmulti <- mplusModeler(step3,
                                 dataout = "./Step1_MPlus_Outcomes.dat",
                                 modelout = "./Step3_Outcomes_CMULTI.inp",
                                 check = TRUE, run = TRUE, hashfilename = FALSE
)
####### Step3 EYE ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = Eye;
  usevar =  Eye;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%

    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [Eye$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [Eye$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [Eye$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [Eye$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [Eye$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [Eye$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_eye <- mplusModeler(step3,
                              dataout = "./Step1_MPlus_Outcomes.dat",
                              modelout = "./Step3_Outcomes_Eye.inp",
                              check = TRUE, run = TRUE, hashfilename = FALSE
)
####### Step3 CIRCUL ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = Circul;
  usevar =  Circul;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%

    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [Circul$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [Circul$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [Circul$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [Circul$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [Circul$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [Circul$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_circul <- mplusModeler(step3,
                                 dataout = "./Step1_MPlus_Outcomes.dat",
                                 modelout = "./Step3_Outcomes_Circul.inp",
                                 check = TRUE, run = TRUE, hashfilename = FALSE
)
####### Step3 ENDOCR ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = Endocr;
  usevar =  Endocr;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%

    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [Endocr$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [Endocr$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [Endocr$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [Endocr$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [Endocr$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [Endocr$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_endocr <- mplusModeler(step3,
                                 dataout = "./Step1_MPlus_Outcomes.dat",
                                 modelout = "./Step3_Outcomes_Endocr.inp",
                                 check = TRUE, run = TRUE, hashfilename = FALSE
)
####### Step3 MUSCUL ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = Muscul;
  usevar =  Muscul;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%

    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [Muscul$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [Muscul$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [Muscul$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [Muscul$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [Muscul$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [Muscul$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_muscul <- mplusModeler(step3,
                                 dataout = "./Step1_MPlus_Outcomes.dat",
                                 modelout = "./Step3_Outcomes_Muscul.inp",
                                 check = TRUE, run = TRUE, hashfilename = FALSE
)
####### Step3 NEOPLA ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = Neopla;
  usevar =  Neopla;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%

    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [Neopla$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [Neopla$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [Neopla$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [Neopla$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [Neopla$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [Neopla$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_neopla <- mplusModeler(step3,
                                 dataout = "./Step1_MPlus_Outcomes.dat",
                                 modelout = "./Step3_Outcomes_Neopla.inp",
                                 check = TRUE, run = TRUE, hashfilename = FALSE
)
####### Step3 MENTAL ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = Mental;
  usevar =  Mental;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%

    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [Mental$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [Mental$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [Mental$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [Mental$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [Mental$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [Mental$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_mental <- mplusModeler(step3,
                                 dataout = "./Step1_MPlus_Outcomes.dat",
                                 modelout = "./Step3_Outcomes_Mental.inp",
                                 check = TRUE, run = TRUE, hashfilename = FALSE
)
####### Step3 NERVOU ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = Nervou;
  usevar =  Nervou;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%

    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [Nervou$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [Nervou$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [Nervou$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [Nervou$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [Nervou$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [Nervou$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_nervou <- mplusModeler(step3,
                                 dataout = "./Step1_MPlus_Outcomes.dat",
                                 modelout = "./Step3_Outcomes_Nervou.inp",
                                 check = TRUE, run = TRUE, hashfilename = FALSE
)
####### Step3 RESPIR ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = Respir;
  usevar =  Respir;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%

    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [Respir$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [Respir$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [Respir$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [Respir$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [Respir$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [Respir$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_respir <- mplusModeler(step3,
                                 dataout = "./Step1_MPlus_Outcomes.dat",
                                 modelout = "./Step3_Outcomes_Respir.inp",
                                 check = TRUE, run = TRUE, hashfilename = FALSE
)
####### Step3 LUNG CANCER ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = Luncan;
  usevar =  Luncan;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%

    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [Luncan$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [Luncan$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [Luncan$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [Luncan$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [Luncan$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [Luncan$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_luncan <- mplusModeler(step3,
                                 dataout = "./Step1_MPlus_Outcomes.dat",
                                 modelout = "./Step3_Outcomes_Luncan.inp",
                                 check = TRUE, run = TRUE, hashfilename = FALSE
)

####### Step3 MULTI_4 ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = Multi_4;
  usevar =  Multi_4;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%

    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [Multi_4$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [Multi_4$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [Multi_4$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [Multi_4$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [Multi_4$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [Multi_4$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_Multi_4 <- mplusModeler(step3,
                                  dataout = "./Step1_MPlus_Outcomes.dat",
                                  modelout = "./Step3_Outcomes_Multi_4.inp",
                                  check = TRUE, run = TRUE, hashfilename = FALSE
)




####### Step3  CMULTI_4 ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = Cmulti_4;
  usevar =  Cmulti_4;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%

    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [Cmulti_4$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [Cmulti_4$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [Cmulti_4$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [Cmulti_4$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [Cmulti_4$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [Cmulti_4$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_Cmulti_4 <- mplusModeler(step3,
                                   dataout = "./Step1_MPlus_Outcomes.dat",
                                   modelout = "./Step3_Outcomes_Cmulti_4.inp",
                                   check = TRUE, run = TRUE, hashfilename = FALSE
)

####### Step3 EYE_4 ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = Eye_4;
  usevar =  Eye_4;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%

    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [Eye_4$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [Eye_4$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [Eye_4$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [Eye_4$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [Eye_4$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [Eye_4$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_Eye_4 <- mplusModeler(step3,
                                dataout = "./Step1_MPlus_Outcomes.dat",
                                modelout = "./Step3_Outcomes_Eye_4.inp",
                                check = TRUE, run = TRUE, hashfilename = FALSE
)

####### Step3 CIRCUL_4 ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = Circul_4;
  usevar =  Circul_4;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%

    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [Circul_4$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [Circul_4$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [Circul_4$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [Circul_4$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [Circul_4$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [Circul_4$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_Circul_4 <- mplusModeler(step3,
                                   dataout = "./Step1_MPlus_Outcomes.dat",
                                   modelout = "./Step3_Outcomes_Circul_4.inp",
                                   check = TRUE, run = TRUE, hashfilename = FALSE
)

####### Step3 ENDOCR_4 ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = Endocr_4;
  usevar =  Endocr_4;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%

    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [Endocr_4$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [Endocr_4$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [Endocr_4$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [Endocr_4$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [Endocr_4$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [Endocr_4$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_Endocr_4 <- mplusModeler(step3,
                                   dataout = "./Step1_MPlus_Outcomes.dat",
                                   modelout = "./Step3_Outcomes_Endocr_4.inp",
                                   check = TRUE, run = TRUE, hashfilename = FALSE
)
####### Step3 MUSCUL_4 ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = Muscul_4;
  usevar =  Muscul_4;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%

    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [Muscul_4$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [Muscul_4$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [Muscul_4$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [Muscul_4$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [Muscul_4$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [Muscul_4$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_Muscul_4 <- mplusModeler(step3,
                                   dataout = "./Step1_MPlus_Outcomes.dat",
                                   modelout = "./Step3_Outcomes_Muscul_4.inp",
                                   check = TRUE, run = TRUE, hashfilename = FALSE
)

####### Step3 NEOPLA_4 ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = Neopla_4;
  usevar =  Neopla_4;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%

    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [Neopla_4$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [Neopla_4$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [Neopla_4$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [Neopla_4$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [Neopla_4$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [Neopla_4$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_Neopla_4 <- mplusModeler(step3,
                                   dataout = "./Step1_MPlus_Outcomes.dat",
                                   modelout = "./Step3_Outcomes_Neopla_4.inp",
                                   check = TRUE, run = TRUE, hashfilename = FALSE
)




####### Step3 MENTAL_4 ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = Mental_4;
  usevar =  Mental_4;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%

    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [Mental_4$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [Mental_4$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [Mental_4$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [Mental_4$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [Mental_4$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [Mental_4$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_Mental_4 <- mplusModeler(step3,
                                   dataout = "./Step1_MPlus_Outcomes.dat",
                                   modelout = "./Step3_Outcomes_Mental_4.inp",
                                   check = TRUE, run = TRUE, hashfilename = FALSE
)

####### Step3 NERVOU_4 ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = Nervou_4;
  usevar =  Nervou_4;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%

    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [Nervou_4$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [Nervou_4$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [Nervou_4$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [Nervou_4$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [Nervou_4$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [Nervou_4$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_Nervou_4 <- mplusModeler(step3,
                                   dataout = "./Step1_MPlus_Outcomes.dat",
                                   modelout = "./Step3_Outcomes_Nervou_4.inp",
                                   check = TRUE, run = TRUE, hashfilename = FALSE
)

####### Step3 RESPIR_4 ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = Respir_4;
  usevar =  Respir_4;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%

    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [Respir_4$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [Respir_4$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [Respir_4$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [Respir_4$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [Respir_4$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [Respir_4$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_Respir_4 <- mplusModeler(step3,
                                   dataout = "./Step1_MPlus_Outcomes.dat",
                                   modelout = "./Step3_Outcomes_Respir_4.inp",
                                   check = TRUE, run = TRUE, hashfilename = FALSE
)

###  Step3 MULTI controlling for covariates ####

step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = MULTI;
  usevar = Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3 MULTI;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%

MULTI ON Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3;         ! covariate as predictor of distal

    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [MULTI$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [MULTI$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [MULTI$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [MULTI$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [MULTI$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [MULTI$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_multi_cov <- mplusModeler(step3,
                                    dataout = "./Step1_MPlus_Outcomes.dat",
                                    modelout = "./Step3_Outcomes_MULTI_cov.inp",
                                    check = TRUE, run = TRUE, hashfilename = FALSE
)
####### Step3  CMULTI controlling for covariates ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = CMULTI;
  usevar =  Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3 CMULTI;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%
CMULTI ON Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3;         ! covariate as predictor of distal


    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [CMULTI$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [CMULTI$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [CMULTI$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [CMULTI$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [CMULTI$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [CMULTI$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_cmulti_cov <- mplusModeler(step3,
                                     dataout = "./Step1_MPlus_Outcomes.dat",
                                     modelout = "./Step3_Outcomes_CMULTI_cov.inp",
                                     check = TRUE, run = TRUE, hashfilename = FALSE
)













####### Step3  EYE controlling for covariates######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = EYE;
  usevar =  Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3 EYE;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%
EYE ON Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3;         ! covariate as predictor of distal


    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [EYE$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [EYE$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [EYE$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [EYE$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [EYE$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [EYE$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_eye_cov <- mplusModeler(step3,
                                  dataout = "./Step1_MPlus_Outcomes.dat",
                                  modelout = "./Step3_Outcomes_EYE_cov.inp",
                                  check = TRUE, run = TRUE, hashfilename = FALSE
)














####### Step3  CIRCUL controlling for covariates ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = CIRCUL;
  usevar =  Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3 CIRCUL;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%
CIRCUL ON Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3;         ! covariate as predictor of distal


    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [CIRCUL$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [CIRCUL$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [CIRCUL$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [CIRCUL$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [CIRCUL$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [CIRCUL$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_circul_cov <- mplusModeler(step3,
                                     dataout = "./Step1_MPlus_Outcomes.dat",
                                     modelout = "./Step3_Outcomes_CIRCUL_cov.inp",
                                     check = TRUE, run = TRUE, hashfilename = FALSE
)














####### Step3  ENDOCR controlling for covariates ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = ENDOCR;
  usevar =  Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3 ENDOCR;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%
ENDOCR ON Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3;         ! covariate as predictor of distal


    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [ENDOCR$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [ENDOCR$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [ENDOCR$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [ENDOCR$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [ENDOCR$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [ENDOCR$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_endocr_cov <- mplusModeler(step3,
                                     dataout = "./Step1_MPlus_Outcomes.dat",
                                     modelout = "./Step3_Outcomes_ENDOCR_cov.inp",
                                     check = TRUE, run = TRUE, hashfilename = FALSE
)





####### Step3  MUSCUL controlling for covariates######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = MUSCUL;
  usevar =  Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3 MUSCUL;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%
MUSCUL ON Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3;         ! covariate as predictor of distal


    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [MUSCUL$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [MUSCUL$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [MUSCUL$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [MUSCUL$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [MUSCUL$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [MUSCUL$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_muscul_cov <- mplusModeler(step3,
                                     dataout = "./Step1_MPlus_Outcomes.dat",
                                     modelout = "./Step3_Outcomes_MUSCUL_cov.inp",
                                     check = TRUE, run = TRUE, hashfilename = FALSE
)


####### Step3  NEOPLA controlling for covariates######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = NEOPLA;
  usevar =  Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3 NEOPLA;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%
NEOPLA ON Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3;         ! covariate as predictor of distal


    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [NEOPLA$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [NEOPLA$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [NEOPLA$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [NEOPLA$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [NEOPLA$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [NEOPLA$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_neopla_cov <- mplusModeler(step3,
                                     dataout = "./Step1_MPlus_Outcomes.dat",
                                     modelout = "./Step3_Outcomes_NEOPLA_cov.inp",
                                     check = TRUE, run = TRUE, hashfilename = FALSE
)




####### Step3  MENTAL controlling for covariates######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = MENTAL;
  usevar =  Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3 MENTAL;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%
MENTAL ON Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3;         ! covariate as predictor of distal


    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [MENTAL$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [MENTAL$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [MENTAL$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [MENTAL$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [MENTAL$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [MENTAL$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_mental_cov <- mplusModeler(step3,
                                     dataout = "./Step1_MPlus_Outcomes.dat",
                                     modelout = "./Step3_Outcomes_MENTAL_cov.inp",
                                     check = TRUE, run = TRUE, hashfilename = FALSE
)


####### Step3  NERVOU controlling for covariates######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = NERVOU;
  usevar =  Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3 NERVOU;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%
NERVOU ON Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3;         ! covariate as predictor of distal


    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [NERVOU$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [NERVOU$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [NERVOU$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [NERVOU$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [NERVOU$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [NERVOU$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_nervou_cov <- mplusModeler(step3,
                                     dataout = "./Step1_MPlus_Outcomes.dat",
                                     modelout = "./Step3_Outcomes_NERVOU_cov.inp",
                                     check = TRUE, run = TRUE, hashfilename = FALSE
)


####### Step3  RESPIR controlling for covariates######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  WEIGHT        = Weights;
  CATEGORICAL = RESPIR;
  usevar =  Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3 RESPIR;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%
RESPIR ON Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3;         ! covariate as predictor of distal


    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [RESPIR$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [RESPIR$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [RESPIR$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [RESPIR$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [RESPIR$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [RESPIR$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_respir_cov <- mplusModeler(step3,
                                     dataout = "./Step1_MPlus_Outcomes.dat",
                                     modelout = "./Step3_Outcomes_RESPIR_cov.inp",
                                     check = TRUE, run = TRUE, hashfilename = FALSE
)


####### Step3  LUNCAN controlling for covariates######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = LUNCAN;
  usevar =  Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3 LUNCAN;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%
LUNCAN ON Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3;         ! covariate as predictor of distal


    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [LUNCAN$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [LUNCAN$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [LUNCAN$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [LUNCAN$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [LUNCAN$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [LUNCAN$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_luncan_cov <- mplusModeler(step3,
                                     dataout = "./Step1_MPlus_Outcomes.dat",
                                     modelout = "./Step3_Outcomes_LUNCAN_cov.inp",
                                     check = TRUE, run = TRUE, hashfilename = FALSE
)



###### Step 3 MULTI_4 controlling for covariates ####
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = MULTI_4;
  usevar =  Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3 MULTI_4;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%
MULTI_4 ON Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3;         ! covariate as predictor of distal


    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [MULTI_4$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [MULTI_4$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [MULTI_4$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [MULTI_4$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [MULTI_4$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [MULTI_4$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_MULTI_cov_4 <- mplusModeler(step3,
                                      dataout = "./Step1_MPlus_Outcomes.dat",
                                      modelout = "./Step3_Outcomes_MULTI_4_cov.inp",
                                      check = TRUE, run = TRUE, hashfilename = FALSE
)










####### Step3  CMULTI_4 controlling for covariates ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = CMULTI_4;
  usevar =  Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3 CMULTI_4;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%
CMULTI_4 ON Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3;         ! covariate as predictor of distal


    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [CMULTI_4$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [CMULTI_4$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [CMULTI_4$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [CMULTI_4$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [CMULTI_4$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [CMULTI_4$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_CMULTI_cov_4 <- mplusModeler(step3,
                                       dataout = "./Step1_MPlus_Outcomes.dat",
                                       modelout = "./Step3_Outcomes_CMULTI_4_cov.inp",
                                       check = TRUE, run = TRUE, hashfilename = FALSE
)





####### Step3  EYE_4 controlling for covariates######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = EYE_4;
  usevar =  Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3 EYE_4;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%
EYE_4 ON Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3;         ! covariate as predictor of distal


    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [EYE_4$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [EYE_4$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [EYE_4$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [EYE_4$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [EYE_4$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [EYE_4$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_EYE_cov_4 <- mplusModeler(step3,
                                    dataout = "./Step1_MPlus_Outcomes.dat",
                                    modelout = "./Step3_Outcomes_EYE_4_cov.inp",
                                    check = TRUE, run = TRUE, hashfilename = FALSE
)














####### Step3  CIRCUL_4 controlling for covariates ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = CIRCUL_4;
  usevar =  Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3 CIRCUL_4;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%
CIRCUL_4 ON Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3;         ! covariate as predictor of distal


    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [CIRCUL_4$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [CIRCUL_4$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [CIRCUL_4$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [CIRCUL_4$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [CIRCUL_4$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [CIRCUL_4$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_CIRCUL_cov_4 <- mplusModeler(step3,
                                       dataout = "./Step1_MPlus_Outcomes.dat",
                                       modelout = "./Step3_Outcomes_CIRCUL_4_cov.inp",
                                       check = TRUE, run = TRUE, hashfilename = FALSE
)














####### Step3  ENDOCR_4 controlling for covariates ######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = ENDOCR_4;
  usevar =  Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3 ENDOCR_4;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%
ENDOCR_4 ON Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3;         ! covariate as predictor of distal


    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [ENDOCR_4$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [ENDOCR_4$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [ENDOCR_4$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [ENDOCR_4$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [ENDOCR_4$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [ENDOCR_4$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_ENDOCR_cov_4 <- mplusModeler(step3,
                                       dataout = "./Step1_MPlus_Outcomes.dat",
                                       modelout = "./Step3_Outcomes_ENDOCR_4_cov.inp",
                                       check = TRUE, run = TRUE, hashfilename = FALSE
)





####### Step3  MUSCUL_4 controlling for covariates######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = MUSCUL_4;
  usevar =  Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3 MUSCUL_4;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%
MUSCUL_4 ON Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3;         ! covariate as predictor of distal


    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [MUSCUL_4$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [MUSCUL_4$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [MUSCUL_4$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [MUSCUL_4$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [MUSCUL_4$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [MUSCUL_4$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_MUSCUL_cov_4 <- mplusModeler(step3,
                                       dataout = "./Step1_MPlus_Outcomes.dat",
                                       modelout = "./Step3_Outcomes_MUSCUL_4_cov.inp",
                                       check = TRUE, run = TRUE, hashfilename = FALSE
)


####### Step3  NEOPLA_4 controlling for covariates######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = NEOPLA_4;
  usevar =  Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3 NEOPLA_4;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%
NEOPLA_4 ON Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3;         ! covariate as predictor of distal


    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [NEOPLA_4$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [NEOPLA_4$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [NEOPLA_4$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [NEOPLA_4$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [NEOPLA_4$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [NEOPLA_4$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_NEOPLA_cov_4 <- mplusModeler(step3,
                                       dataout = "./Step1_MPlus_Outcomes.dat",
                                       modelout = "./Step3_Outcomes_NEOPLA_4_cov.inp",
                                       check = TRUE, run = TRUE, hashfilename = FALSE
)




####### Step3  MENTAL_4 controlling for covariates######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = MENTAL_4;
  usevar =  Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3 MENTAL_4;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%
MENTAL_4 ON Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3;         ! covariate as predictor of distal


    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [MENTAL_4$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [MENTAL_4$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [MENTAL_4$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [MENTAL_4$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [MENTAL_4$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [MENTAL_4$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_MENTAL_cov_4 <- mplusModeler(step3,
                                       dataout = "./Step1_MPlus_Outcomes.dat",
                                       modelout = "./Step3_Outcomes_MENTAL_4_cov.inp",
                                       check = TRUE, run = TRUE, hashfilename = FALSE
)


####### Step3  NERVOU_4 controlling for covariates######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = NERVOU_4;
  usevar =  Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3 NERVOU_4;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%
NERVOU_4 ON Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3;         ! covariate as predictor of distal


    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [NERVOU_4$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [NERVOU_4$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [NERVOU_4$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [NERVOU_4$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [NERVOU_4$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [NERVOU_4$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_NERVOU_cov_4 <- mplusModeler(step3,
                                       dataout = "./Step1_MPlus_Outcomes.dat",
                                       modelout = "./Step3_Outcomes_NERVOU_4_cov.inp",
                                       check = TRUE, run = TRUE, hashfilename = FALSE
)


####### Step3  RESPIR_4 controlling for covariates######
step3 <- mplusObject(
  TITLE = "Step3 - 3step LSAY - Lab9",
  VARIABLE =
    "nominal=N;
  usevar = n;
  USEOBSERVATIONS = Female NE -99 AND Age NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  missing are all (-99);
  classes = c(6);
  CATEGORICAL = RESPIR_4;
  usevar =  Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3 RESPIR_4;
  WEIGHT  = Weights;",
  ANALYSIS =
    "estimator = mlr;
  type = mixture;
  starts = 0;
  ITERATIONS = 1000;",
  MODEL =
    glue(
      "%OVERALL%
RESPIR_4 ON Female Age
   Pat_Int Pat_Hig Self_Int Self_Hig Edu_Upp
   Edu_Ter Wealth_2 Wealth_3;         ! covariate as predictor of distal


    %C#1%
    [n#1@{logit_cprobs[1,1]}];
    [n#2@{logit_cprobs[1,2]}];
    [n#3@{logit_cprobs[1,3]}];
    [n#4@{logit_cprobs[1,4]}];
    [n#5@{logit_cprobs[1,5]}];

  [RESPIR_4$1](m1);

   %C#2%
  [n#1@{logit_cprobs[2,1]}];
  [n#2@{logit_cprobs[2,2]}];
  [n#3@{logit_cprobs[2,3]}];
  [n#4@{logit_cprobs[2,4]}];
  [n#5@{logit_cprobs[2,5]}];

  [RESPIR_4$1](m2);


  %C#3%
 [n#1@{logit_cprobs[3,1]}];
  [n#2@{logit_cprobs[3,2]}];
  [n#3@{logit_cprobs[3,3]}];
  [n#4@{logit_cprobs[3,4]}];
  [n#5@{logit_cprobs[3,5]}];


  [RESPIR_4$1](m3);


   %C#4%
  [n#1@{logit_cprobs[4,1]}];
  [n#2@{logit_cprobs[4,2]}];
  [n#3@{logit_cprobs[4,3]}];
  [n#4@{logit_cprobs[4,4]}];
  [n#5@{logit_cprobs[4,5]}];


  [RESPIR_4$1](m4);



   %C#5%
  [n#1@{logit_cprobs[5,1]}];
  [n#2@{logit_cprobs[5,2]}];
  [n#3@{logit_cprobs[5,3]}];
  [n#4@{logit_cprobs[5,4]}];
  [n#5@{logit_cprobs[5,5]}];


  [RESPIR_4$1](m5);


    %C#6%
  [n#1@{logit_cprobs[6,1]}];
  [n#2@{logit_cprobs[6,2]}];
  [n#3@{logit_cprobs[6,3]}];
  [n#4@{logit_cprobs[6,4]}];
  [n#5@{logit_cprobs[6,5]}];

    [RESPIR_4$1](m6);"
    ),
  MODELCONSTRAINT =
    "New (diff12 diff13 diff23 diff14
  diff24 diff34 diff15 diff16 diff25
  diff26 diff35 diff36 diff45
  diff46 diff56);
  diff12 = m1-m2;  ! test pairwise distal mean differences
  diff13 = m1-m3;
  diff23 = m2-m3;
  diff14 = m1-m4;
  diff24 = m2-m4;
  diff34 = m3-m4;
  diff15 = m1-m5;
  diff16 = m1-m6;
  diff25 = m2-m5;
  diff26 = m2-m6;
  diff35 = m3-m5;
  diff36 = m3-m6;
  diff45 = m4-m5;
  diff46 = m4-m6;
  diff56 = m5-m6;

  ",
  MODELTEST = " 0=m1-m2;
   0=m2-m3;
   0=m3-m4; ! omnibus test of distal thresholds
   0=m4-m5;
   0=m5-m6;",
  OUTPUT = "sampstat residual TECH11 TECH14;",
  usevariables = colnames(savedata),
  rdata = savedata
)

step3_fit_RESPIR_cov_4 <- mplusModeler(step3,
                                       dataout = "./Step1_MPlus_Outcomes.dat",
                                       modelout = "./Step3_Outcomes_RESPIR_4_cov.inp",
                                       check = TRUE, run = TRUE, hashfilename = FALSE
)


##### Regressing latent classes on predictors #######
setwd(here("Data", "Processed_Data", "MPlus_Data", "Covariates_Wave9_Only"))
getwd()

step1 <- mplusObject(
  TITLE = "R3STEP LCA with covariates;",
  VARIABLE =
    "CATEGORICAL = W4_Smoke-W8_FV;
  USEVARIABLES = W4_Smoke-W8_FV,Weights;
  CLASSES = c (6);
  USEOBSERVATIONS = Female NE -99 AND AGE NE -99 AND
  Pat_Int NE -99 AND
Pat_Hig NE -99 AND Self_Int NE -99 AND
Self_Hig NE -99 AND Edu_Upp NE -99 AND
Edu_Ter NE -99 AND
Wealth_2 NE -99 AND Wealth_3 NE -99;
  MISSING ARE ALL (-99);
  AUXILIARY = (R3STEP) Female-Wealth_3;
  WEIGHT  = Weights;",
  ANALYSIS = "TYPE = MIXTURE;
  STARTS = 500 100;
  STITERATIONS=20;
  LRTSTARTS = 50 10 400 20;",
  OUTPUT = "sampstat TECH11 TECH14;",
  SAVEDATA = "FILE = mplus_covariates_LCA.cprob;
  SAVE = CPROB;
  Missflag= -99;",
  PLOT = "TYPE=PLOT3;
  series = W4_Smoke-W8_FV(*);",
  usevariables = colnames(main_data_new_names),
  rdata = main_data_new_names
)

step1_fit_reg <- mplusModeler(step1,
                              dataout = "./mplus_covariates_LCA.dat",
                              modelout = "./mplus_covariates_LCA.inp",
                              check = TRUE, run = TRUE, hashfilename = TRUE
)

####### Wave 9 - Extracting data on  proportions and significant differences between latent class of distal outcomes#####
####### i.e. diseases of different body systems, multimorbidity and complex multimorbidity)
#### NOTE : these results are for models that DO NOT ADJUST for covariates

Outcome_Values <- function(Outcome) {
  main_output <- {
    Outcome
  }$results$output
  start <- str_which(main_output, "RESULTS IN PROBABILITY SCALE")
  end <- str_which(main_output, "LATENT CLASS ODDS RATIO RESULTS")
  Outcome_proportions <- main_output[start:end]
  
  Latent_Classes <- Outcome_proportions[str_which(Outcome_proportions, "Latent Class\\s*[0-9]")]
  
  Proportions_extracted <- stri_extract_all_regex(Outcome_proportions, "y\\s[2]\\s*[0-1]\\.[0-9]{3}") %>% unlist()
  Proportions_extracted <- Proportions_extracted[!is.na(Proportions_extracted)]
  
  Proportions_extracted <- str_extract(Proportions_extracted, pattern = "[0-1]\\.[0-9]{3}")
  Proportions_extracted <- as.numeric(Proportions_extracted)
  
  Table_1 <- map2_dfr(Latent_Classes, Proportions_extracted, ~ tibble(Latent_Class = .x, Outcome = .y))
  return(Table_1)
}


Significant_Outcome_Differences <- function(Outcome) {
  main_output <- {
    Outcome
  }$results$output
  start <- str_which(main_output, "New/Additional Parameters")
  end <- str_which(main_output, "RESULTS IN PROBABILITY SCALE")
  Significant_difference_lines <- main_output[(start + 1):(end - 3)]
  
  Classes <- str_extract(Significant_difference_lines, pattern = "DIFF..")
  
  Significance_Level <- str_sub(Significant_difference_lines, -5, -1)
  Significance_Level_Numeric <- as.numeric(unlist(Significance_Level))
  Table_2 <- map2_dfr(Classes, Significance_Level_Numeric, ~ tibble(Latent_Class = .x, Significance = .y))
  return(Table_2)
}

multi_proportion <- Outcome_Values(step3_fit_multi) %>%
  rename(Multi = Outcome)
multi_difference <- Significant_Outcome_Differences(step3_fit_multi) %>%
  rename(Multi_sig = Significance)

cmulti_proportion <- Outcome_Values(step3_fit_cmulti) %>%
  rename(CMulti = Outcome)
cmulti_difference <- Significant_Outcome_Differences(step3_fit_cmulti) %>%
  rename(CMulti_sig = Significance)

eye_proportion <- Outcome_Values(step3_fit_eye) %>%
  rename(Eye = Outcome)
eye_difference <- Significant_Outcome_Differences(step3_fit_eye) %>%
  rename(Eye_sig = Significance)

circul_proportion <- Outcome_Values(step3_fit_circul) %>%
  rename(Circulatory = Outcome)
circul_difference <- Significant_Outcome_Differences(step3_fit_circul) %>%
  rename(Circulatory_sig = Significance)

endocr_proportion <- Outcome_Values(step3_fit_endocr) %>%
  rename(Endocrine = Outcome)
endocr_difference <- Significant_Outcome_Differences(step3_fit_endocr) %>%
  rename(Endocrine_sig = Significance)

muscul_proportion <- Outcome_Values(step3_fit_muscul) %>%
  rename(Muscular = Outcome)
muscul_difference <- Significant_Outcome_Differences(step3_fit_muscul) %>%
  rename(Muscular_sig = Significance)

neopla_proportion <- Outcome_Values(step3_fit_neopla) %>%
  rename(Neoplasm = Outcome)
neopla_difference <- Significant_Outcome_Differences(step3_fit_neopla) %>%
  rename(Neoplasm_sig = Significance)

mental_proportion <- Outcome_Values(step3_fit_mental) %>%
  rename(Mental = Outcome)
mental_difference <- Significant_Outcome_Differences(step3_fit_mental) %>%
  rename(Mental_sig = Significance)

nervou_proportion <- Outcome_Values(step3_fit_nervou) %>%
  rename(Nervous = Outcome)
nervou_difference <- Significant_Outcome_Differences(step3_fit_nervou) %>%
  rename(Nervous_sig = Significance)

respir_proportion <- Outcome_Values(step3_fit_respir) %>%
  rename(Respiratory = Outcome)
respir_difference <- Significant_Outcome_Differences(step3_fit_respir) %>%
  rename(Respiratory_sig = Significance)

luncan_proportion <- Outcome_Values(step3_fit_luncan) %>%
  rename(Lung_cancer = Outcome)
luncan_difference <- Significant_Outcome_Differences(step3_fit_luncan) %>%
  rename(Lung_cancer_sig = Significance)


Outcome_proportion <- list(
  multi_proportion, cmulti_proportion,
  eye_proportion, circul_proportion,
  endocr_proportion, muscul_proportion,
  neopla_proportion,
  mental_proportion, nervou_proportion,
  respir_proportion, luncan_proportion
) %>% reduce(inner_join, by = "Latent_Class")

Outcome_difference <- list(
  multi_difference, cmulti_difference,
  eye_difference, circul_difference,
  endocr_difference, muscul_difference,
  neopla_difference,
  mental_difference, nervou_difference,
  respir_difference, luncan_difference
) %>% reduce(inner_join, by = "Latent_Class")



####### Wave 4 - Extracting data on  proportions and significant differences between latent class of distal outcomes#####
####### i.e. diseases of different body systems, multimorbidity and complex multimorbidity)
#### NOTE : these results are for models that DO NOT ADJUST for covariates

Outcome_Values <- function(Outcome) {
  main_output <- {
    Outcome
  }$results$output
  start <- str_which(main_output, "RESULTS IN PROBABILITY SCALE")
  end <- str_which(main_output, "LATENT CLASS ODDS RATIO RESULTS")
  Outcome_proportions <- main_output[start:end]
  
  Latent_Classes <- Outcome_proportions[str_which(Outcome_proportions, "Latent Class\\s*[0-9]")]
  
  Proportions_extracted <- stri_extract_all_regex(Outcome_proportions, "y\\s[2]\\s*[0-1]\\.[0-9]{3}") %>% unlist()
  Proportions_extracted <- Proportions_extracted[!is.na(Proportions_extracted)]
  
  Proportions_extracted <- str_extract(Proportions_extracted, pattern = "[0-1]\\.[0-9]{3}")
  Proportions_extracted <- as.numeric(Proportions_extracted)
  
  Table_1 <- map2_dfr(Latent_Classes, Proportions_extracted, ~ tibble(Latent_Class = .x, Outcome = .y))
  return(Table_1)
}


Significant_Outcome_Differences <- function(Outcome) {
  main_output <- {
    Outcome
  }$results$output
  start <- str_which(main_output, "New/Additional Parameters")
  end <- str_which(main_output, "RESULTS IN PROBABILITY SCALE")
  Significant_difference_lines <- main_output[(start + 1):(end - 3)]
  
  Classes <- str_extract(Significant_difference_lines, pattern = "DIFF..")
  
  Significance_Level <- str_sub(Significant_difference_lines, -5, -1)
  Significance_Level_Numeric <- as.numeric(unlist(Significance_Level))
  Table_2 <- map2_dfr(Classes, Significance_Level_Numeric, ~ tibble(Latent_Class = .x, Significance = .y))
  return(Table_2)
}

multi_proportion_4 <- Outcome_Values(step3_fit_Multi_4) %>%
  rename(Multi_4 = Outcome)
multi_difference_4 <- Significant_Outcome_Differences(step3_fit_Multi_4) %>%
  rename(Multi_sig_4 = Significance)

cmulti_proportion_4 <- Outcome_Values(step3_fit_Cmulti_4) %>%
  rename(CMulti_4 = Outcome)
cmulti_difference_4 <- Significant_Outcome_Differences(step3_fit_Cmulti_4) %>%
  rename(CMulti_sig_4 = Significance)

eye_proportion_4 <- Outcome_Values(step3_fit_Eye_4) %>%
  rename(Eye_4 = Outcome)
eye_difference_4 <- Significant_Outcome_Differences(step3_fit_Eye_4) %>%
  rename(Eye_sig_4 = Significance)

circul_proportion_4 <- Outcome_Values(step3_fit_Circul_4) %>%
  rename(Circulatory_4 = Outcome)
circul_difference_4 <- Significant_Outcome_Differences(step3_fit_Circul_4) %>%
  rename(Circulatory_sig_4 = Significance)

endocr_proportion_4 <- Outcome_Values(step3_fit_Endocr_4) %>%
  rename(Endocrine_4 = Outcome)
endocr_difference_4 <- Significant_Outcome_Differences(step3_fit_Endocr_4) %>%
  rename(Endocrine_sig_4 = Significance)

muscul_proportion_4 <- Outcome_Values(step3_fit_Muscul_4) %>%
  rename(Muscular_4 = Outcome)
muscul_difference_4 <- Significant_Outcome_Differences(step3_fit_Muscul_4) %>%
  rename(Muscular_sig_4 = Significance)

neopla_proportion_4 <- Outcome_Values(step3_fit_Neopla_4) %>%
  rename(Neoplasm_4 = Outcome)
neopla_difference_4 <- Significant_Outcome_Differences(step3_fit_Neopla_4) %>%
  rename(Neoplasm_sig_4 = Significance)

mental_proportion_4 <- Outcome_Values(step3_fit_Mental_4) %>%
  rename(Mental_4 = Outcome)
mental_difference_4 <- Significant_Outcome_Differences(step3_fit_Mental_4) %>%
  rename(Mental_sig_4 = Significance)

nervou_proportion_4 <- Outcome_Values(step3_fit_Nervou_4) %>%
  rename(Nervous_4 = Outcome)
nervou_difference_4 <- Significant_Outcome_Differences(step3_fit_Nervou_4) %>%
  rename(Nervous_sig_4 = Significance)

respir_proportion_4 <- Outcome_Values(step3_fit_Respir_4) %>%
  rename(Respiratory_4 = Outcome)
respir_difference_4 <- Significant_Outcome_Differences(step3_fit_Respir_4) %>%
  rename(Respiratory_sig_4 = Significance)


Outcome_proportion_4 <- list(
  multi_proportion_4, cmulti_proportion_4,
  eye_proportion_4, circul_proportion_4,
  endocr_proportion_4, muscul_proportion_4,
  neopla_proportion_4,
  mental_proportion_4, nervou_proportion_4,
  respir_proportion_4
) %>% reduce(inner_join, by = "Latent_Class")

Outcome_difference_4 <- list(
  multi_difference_4, cmulti_difference_4,
  eye_difference_4, circul_difference_4,
  endocr_difference_4, muscul_difference_4,
  neopla_difference_4,
  mental_difference_4, nervou_difference_4,
  respir_difference_4
) %>% reduce(inner_join, by = "Latent_Class")



####### Wave 9 - Extracting data on  proportions and significant differences between latent class of distal outcomes #####
####### i.e. diseases of different body systems, multimorbidity and complex multimorbidity)
#### NOTE : these results are for models that adjust for covariates

Outcome_Values <- function(Outcome) {
  main_output <- {
    Outcome
  }$results$output
  start <- str_which(main_output, "RESULTS IN PROBABILITY SCALE")
  end <- str_which(main_output, "LOGISTIC REGRESSION ODDS RATIO RESULTS")
  Outcome_proportions <- main_output[start:end]
  
  Condition_Present_Index <- Outcome_proportions[str_which(Outcome_proportions, "Category 2")]
  Condition_Present_Probability <- as.numeric(unlist(sub(".* [0-9]", "", Condition_Present_Index)))
  
  Latent_Classes <- Outcome_proportions[str_which(Outcome_proportions, "Latent Class")]
  
  Outcome_name <- Outcome_proportions[(str_which(Outcome_proportions, "Estimate") + 4)]
  
  
  Table_1 <- map2_dfr(Latent_Classes, Condition_Present_Probability, ~ tibble(Latent_Class = .x, Outcome = .y))
  
  return(Table_1)
}

Significant_Outcome_Differences <- function(Outcome) {
  main_output <- {
    Outcome
  }$results$output
  start <- str_which(main_output, "New/Additional Parameters")
  end <- str_which(main_output, "RESULTS IN PROBABILITY SCALE")
  Significant_difference_lines <- main_output[(start + 1):(end - 3)]
  
  Classes <- str_extract(Significant_difference_lines, pattern = "DIFF..")
  
  
  Significance_Level <- str_sub(Significant_difference_lines, -5, -1)
  Significance_Level_Numeric <- as.numeric(unlist(Significance_Level))
  Table_2 <- map2_dfr(Classes, Significance_Level_Numeric, ~ tibble(Latent_Class = .x, Significance = .y))
  return(Table_2)
}

multi_cov_proportion <- Outcome_Values(step3_fit_multi_cov) %>%
  rename(Multi_cov = Outcome)
multi_cov_difference <- Significant_Outcome_Differences(step3_fit_multi_cov) %>%
  rename(Multi_cov_sig = Significance)

cmulti_cov_proportion <- Outcome_Values(step3_fit_cmulti_cov) %>%
  rename(CMulti_cov = Outcome)
cmulti_cov_difference <- Significant_Outcome_Differences(step3_fit_cmulti_cov) %>%
  rename(CMulti_cov_sig = Significance)

eye_cov_proportion <- Outcome_Values(step3_fit_eye_cov) %>%
  rename(Eye_cov = Outcome)
eye_cov_difference <- Significant_Outcome_Differences(step3_fit_eye_cov) %>%
  rename(Eye_cov_sig = Significance)

circul_cov_proportion <- Outcome_Values(step3_fit_circul_cov) %>%
  rename(Circulatory_cov = Outcome)
circul_cov_difference <- Significant_Outcome_Differences(step3_fit_circul_cov) %>%
  rename(Circulatory_cov_sig = Significance)

endocr_cov_proportion <- Outcome_Values(step3_fit_endocr_cov) %>%
  rename(Endocrine_cov = Outcome)
endocr_cov_difference <- Significant_Outcome_Differences(step3_fit_endocr_cov) %>%
  rename(Endocrine_cov_sig = Significance)

muscul_cov_proportion <- Outcome_Values(step3_fit_muscul_cov) %>%
  rename(Muscular_cov = Outcome)
muscul_cov_difference <- Significant_Outcome_Differences(step3_fit_muscul_cov) %>%
  rename(Muscular_cov_sig = Significance)

neopla_cov_proportion <- Outcome_Values(step3_fit_neopla_cov) %>%
  rename(Neoplasm_cov = Outcome)
neopla_cov_difference <- Significant_Outcome_Differences(step3_fit_neopla_cov) %>%
  rename(Neoplasm_cov_sig = Significance)

mental_cov_proportion <- Outcome_Values(step3_fit_mental_cov) %>%
  rename(Mental_cov = Outcome)
mental_cov_difference <- Significant_Outcome_Differences(step3_fit_mental_cov) %>%
  rename(Mental_cov_sig = Significance)

nervou_cov_proportion <- Outcome_Values(step3_fit_nervou_cov) %>%
  rename(Nervous_cov = Outcome)

nervou_cov_difference <- Significant_Outcome_Differences(step3_fit_nervou_cov) %>%
  rename(Nervous_cov_sig = Significance)

respir_cov_proportion <- Outcome_Values(step3_fit_respir_cov) %>%
  rename(Respiratory_cov = Outcome)
respir_cov_difference <- Significant_Outcome_Differences(step3_fit_respir_cov) %>%
  rename(Respiratory_cov_sig = Significance)

luncan_cov_proportion <- Outcome_Values(step3_fit_luncan_cov) %>%
  rename(Lung_cancer_cov = Outcome)
luncan_cov_difference <- Significant_Outcome_Differences(step3_fit_luncan_cov) %>%
  rename(Lung_cancer_cov_sig = Significance)

Outcome_cov_proportion <- list(
  multi_cov_proportion, cmulti_cov_proportion,
  eye_cov_proportion, circul_cov_proportion,
  endocr_cov_proportion, muscul_cov_proportion,
  neopla_cov_proportion,
  mental_cov_proportion, nervou_cov_proportion,
  respir_cov_proportion, luncan_cov_proportion
) %>% reduce(inner_join, by = "Latent_Class")

Outcome_cov_difference <- list(
  multi_cov_difference, cmulti_cov_difference,
  eye_cov_difference, circul_cov_difference,
  endocr_cov_difference, muscul_cov_difference,
  neopla_cov_difference,
  mental_cov_difference, nervou_cov_difference,
  respir_cov_difference, luncan_cov_difference
) %>% reduce(inner_join, by = "Latent_Class")

####### Wave 4 - Extracting data on  proportions and significant differences between latent class of distal outcomes #####
####### i.e. diseases of different body systems, multimorbidity and complex multimorbidity)
#### NOTE : these results are for models that adjust for covariates

Outcome_Values <- function(Outcome) {
  main_output <- {
    Outcome
  }$results$output
  start <- str_which(main_output, "RESULTS IN PROBABILITY SCALE")
  end <- str_which(main_output, "LOGISTIC REGRESSION ODDS RATIO RESULTS")
  Outcome_proportions <- main_output[start:end]
  
  Condition_Present_Index <- Outcome_proportions[str_which(Outcome_proportions, "Category 2")]
  Condition_Present_Probability <- as.numeric(unlist(sub(".* [0-9]", "", Condition_Present_Index)))
  
  Latent_Classes <- Outcome_proportions[str_which(Outcome_proportions, "Latent Class")]
  
  Outcome_name <- Outcome_proportions[(str_which(Outcome_proportions, "Estimate") + 4)]
  
  
  Table_1 <- map2_dfr(Latent_Classes, Condition_Present_Probability, ~ tibble(Latent_Class = .x, Outcome = .y))
  
  return(Table_1)
}

Significant_Outcome_Differences <- function(Outcome) {
  main_output <- {
    Outcome
  }$results$output
  start <- str_which(main_output, "New/Additional Parameters")
  end <- str_which(main_output, "RESULTS IN PROBABILITY SCALE")
  Significant_difference_lines <- main_output[(start + 1):(end - 3)]
  
  Classes <- str_extract(Significant_difference_lines, pattern = "DIFF..")
  
  
  Significance_Level <- str_sub(Significant_difference_lines, -5, -1)
  Significance_Level_Numeric <- as.numeric(unlist(Significance_Level))
  Table_2 <- map2_dfr(Classes, Significance_Level_Numeric, ~ tibble(Latent_Class = .x, Significance = .y))
  return(Table_2)
}

multi_cov_proportion_4 <- Outcome_Values(step3_fit_MULTI_cov_4) %>%
  rename(Multi_cov_4 = Outcome)
multi_cov_difference_4 <- Significant_Outcome_Differences(step3_fit_MULTI_cov_4) %>%
  rename(Multi_cov_sig_4 = Significance)

cmulti_cov_proportion_4 <- Outcome_Values(step3_fit_CMULTI_cov_4) %>%
  rename(CMulti_cov_4 = Outcome)
cmulti_cov_difference_4 <- Significant_Outcome_Differences(step3_fit_CMULTI_cov_4) %>%
  rename(CMulti_cov_sig_4 = Significance)

eye_cov_proportion_4 <- Outcome_Values(step3_fit_EYE_cov_4) %>%
  rename(Eye_cov_4 = Outcome)
eye_cov_difference_4 <- Significant_Outcome_Differences(step3_fit_EYE_cov_4) %>%
  rename(Eye_cov_sig_4 = Significance)

circul_cov_proportion_4 <- Outcome_Values(step3_fit_CIRCUL_cov_4) %>%
  rename(Circulatory_cov_4 = Outcome)
circul_cov_difference_4 <- Significant_Outcome_Differences(step3_fit_CIRCUL_cov_4) %>%
  rename(Circulatory_cov_sig_4 = Significance)

endocr_cov_proportion_4 <- Outcome_Values(step3_fit_ENDOCR_cov_4) %>%
  rename(Endocrine_cov_4 = Outcome)
endocr_cov_difference_4 <- Significant_Outcome_Differences(step3_fit_ENDOCR_cov_4) %>%
  rename(Endocrine_cov_sig_4 = Significance)

muscul_cov_proportion_4 <- Outcome_Values(step3_fit_MUSCUL_cov_4) %>%
  rename(Muscular_cov_4 = Outcome)
muscul_cov_difference_4 <- Significant_Outcome_Differences(step3_fit_MUSCUL_cov_4) %>%
  rename(Muscular_cov_sig_4 = Significance)

neopla_cov_proportion_4 <- Outcome_Values(step3_fit_NEOPLA_cov_4) %>%
  rename(Neoplasm_cov_4 = Outcome)
neopla_cov_difference_4 <- Significant_Outcome_Differences(step3_fit_NEOPLA_cov_4) %>%
  rename(Neoplasm_cov_sig_4 = Significance)

mental_cov_proportion_4 <- Outcome_Values(step3_fit_MENTAL_cov_4) %>%
  rename(Mental_cov_4 = Outcome)
mental_cov_difference_4 <- Significant_Outcome_Differences(step3_fit_MENTAL_cov_4) %>%
  rename(Mental_cov_sig_4 = Significance)

nervou_cov_proportion_4 <- Outcome_Values(step3_fit_NERVOU_cov_4) %>%
  rename(Nervous_cov_4 = Outcome)

nervou_cov_difference_4 <- Significant_Outcome_Differences(step3_fit_NERVOU_cov_4) %>%
  rename(Nervous_cov_sig_4 = Significance)

respir_cov_proportion_4 <- Outcome_Values(step3_fit_RESPIR_cov_4) %>%
  rename(Respiratory_cov_4 = Outcome)
respir_cov_difference_4 <- Significant_Outcome_Differences(step3_fit_RESPIR_cov_4) %>%
  rename(Respiratory_cov_sig_4 = Significance)


Outcome_cov_proportion_4 <- list(
  multi_cov_proportion_4, cmulti_cov_proportion_4,
  eye_cov_proportion_4, circul_cov_proportion_4,
  endocr_cov_proportion_4, muscul_cov_proportion_4,
  neopla_cov_proportion_4,
  mental_cov_proportion_4, nervou_cov_proportion_4,
  respir_cov_proportion_4
) %>% reduce(inner_join, by = "Latent_Class")

Outcome_cov_difference_4 <- list(
  multi_cov_difference_4, cmulti_cov_difference_4,
  eye_cov_difference_4, circul_cov_difference_4,
  endocr_cov_difference_4, muscul_cov_difference_4,
  neopla_cov_difference_4,
  mental_cov_difference_4, nervou_cov_difference_4,
  respir_cov_difference_4
) %>% reduce(inner_join, by = "Latent_Class")




#### Merging and writing files for outcome proportions and differences #####
setwd(here())

All_outcome_proportions <- list(
  Outcome_proportion, Outcome_proportion_4,
  Outcome_cov_proportion, Outcome_cov_proportion_4
) %>% reduce(inner_join, by = "Latent_Class")
All_outcome_difference <- list(
  Outcome_difference, Outcome_difference_4,
  Outcome_cov_difference, Outcome_cov_difference_4
) %>% reduce(inner_join, by = "Latent_Class")
write.csv(All_outcome_difference, here("Shiny_App", "Outcome_differences.csv"))
write.csv(All_outcome_proportions, here("Shiny_App", "app_3", "Outcome_proportions.csv"))
