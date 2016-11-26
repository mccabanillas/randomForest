# AUTHOR: MC CABANILLAS CAMPOS
# This script trains a Random Forest model based on the data,
# using k fold cross validation.
# Women's Health Risk Assessment competition!
# https://gallery.cortanaintelligence.com/Experiment/Women-s-Health-Risk-Assessment-2

library(ggplot2)
library(randomForest)


set.seed(0)
dataset1 <- read.csv("../Azure/woman/train.csv", stringsAsFactors=FALSE,header = TRUE,na.strings = "?")

# describing data and preparing them for regression analysis
nulos_debut<- 0;
data_lm    <- data.frame( )
for (i in 1:nrow(dataset1)) {
    if (is.na(dataset1$Debut[i])){
	}else{
		par_datos<-c(dataset1$age[i],dataset1$Debut[i])
		data_lm <- rbind(data_lm, par_datos)
	}
}
names(data_lm)<-c("age","Debut")
debu.lm		=	lm(data_lm$Debut~data_lm$age,data=data_lm)	

for (i in 1:nrow(dataset1)) {
  if ( is.null(dataset1$Debut[i]) || is.na(dataset1$Debut[i]) ){
	debu.debuts      <- data.frame(age = seq(dataset1$age[i],dataset1$age[i]))
   	dataset1$Debut[i]<-predict(debu.lm, debu.debuts)
  }
}

# Removal Procedure 
extractFeatures <- function(data) {
  features <- c(
"christian",
"muslim",
"hindu",
"other",
"cellphone",
"motorcycle",
"radio",
"cooker",
"fridge",
"furniture",
"cart",
"irrigation",
"thrasher",
#"car",
"generator",
"REGION_PROVINCE",
"DISTRICT",
"electricity",
"age",
"tribe",
"foodinsecurity",
"EVER_HAD_SEX",
"EVER_BEEN_PREGNANT",
"CHILDREN",
"india",
"married",
"multpart",
"educ",
"inschool",
"ownincome",
"LaborDeliv",
"babydoc",
"Debut",
"ModCon",
"usecondom",
"hivknow",
"lowlit",
"highlit",
"urban",
"rural"
)
  fea <- data[,features]
  fea$christian[is.na(fea$christian)] <- 0
  fea$muslim[is.na(fea$muslim)] <- 0
  fea$hindu[is.na(fea$hindu)] <- 0
  fea$other[is.na(fea$other)] <- 0
  fea$cellphone[is.na(fea$cellphone)] <- 0
  fea$motorcycle[is.na(fea$motorcycle)] <- 0
  fea$radio[is.na(fea$radio)] <- 0
  fea$cooker[is.na(fea$cooker)] <- 0
  fea$fridge[is.na(fea$fridge)] <- 0
  fea$furniture[is.na(fea$furniture)] <- 0
  fea$cart[is.na(fea$cart)] <- 0
  fea$irrigation[is.na(fea$irrigation)] <- 0
  fea$thrasher[is.na(fea$thrasher)] <- 0
  fea$generator[is.na(fea$generator)] <- 0
  fea$REGION_PROVINCE[is.na(fea$REGION_PROVINCE)] <- 0
  fea$DISTRICT[is.na(fea$DISTRICT)] <- 0
  fea$electricity[is.na(fea$electricity)] <- 0
  fea$age[is.na(fea$age)] <- median(fea$age, na.rm=TRUE)
  fea$foodinsecurity[is.na(fea$foodinsecurity)] <- 0
  fea$EVER_HAD_SEX[is.na(fea$EVER_HAD_SEX)] <- 0
  fea$EVER_BEEN_PREGNANT[is.na(fea$EVER_BEEN_PREGNANT)] <- 0
  fea$usecondom[is.na(fea$usecondom)] <- 0
  fea$hivknow[is.na(fea$hivknow)] <- 0
  fea$ModCon[is.na(fea$ModCon)] <- 0
  fea$multpart[is.na(fea$multpart)] <- 0
  fea$CHILDREN[is.na(fea$CHILDREN)] <- 0
  fea$india[is.na(fea$india)] <- 0
  fea$married[is.na(fea$married)] <- 0
  fea$educ[is.na(fea$educ)] <- 0
  fea$inschool[is.na(fea$inschool)] <- 0
  fea$ownincome[is.na(fea$ownincome)] <- 0
  fea$LaborDeliv[is.na(fea$LaborDeliv)] <- 0
  fea$babydoc[is.na(fea$babydoc)] <- 0
  fea$Debut[is.na(fea$Debut)] <-median(fea$Debut, na.rm=TRUE)
  fea$usecondom[is.na(fea$usecondom)] <- 0
  fea$hivknow[is.na(fea$hivknow)] <- 0
  fea$lowlit[is.na(fea$lowlit)] <- 0
  fea$highlit[is.na(fea$highlit)] <- 0
  fea$urban[is.na(fea$urban)] <- 0
  fea$rural[is.na(fea$rural)] <- 0
   
  return(fea)
}
#--------------------------------------------- CROSS VALIDATION ---------------------------------------------
n_folds <-10
n_train <- nrow(dataset1)
suma_tot_aciertos <-0
print(n_train)
folds_i <- sample(rep(1:n_folds, length.out = n_train))
for (k in 1:n_folds) {
    test_i <- which(folds_i == k)
	train  <- dataset1[-test_i, ]
    test   <- dataset1[test_i, ]


	#new column to predict TRAIN
	combined_label 	<- 100*train$geo + 10*train$segment + train$subgroup
	train 			<- cbind(train, combined_label)

	#new column to predict TEST
	combined_label 	<- 100*test$geo + 10*test$segment + test$subgroup
	test   			<- cbind(test, combined_label)
   
	
	train2 <- extractFeatures(train)
	test2  <- extractFeatures(test)

	common <- intersect(names(train2), names(test2)) 
	for (p in common) { 
	  if (class(train2[[p]]) == "factor") { 
		levels(test2[[p]]) <- levels(train2[[p]]) 
	  } 
	}

	
	#model
	rf <- randomForest(extractFeatures(train2), as.factor(train$combined_label), ntree=100, importance=TRUE,mtry=	4)

	

	submission                <- data.frame(patientID = test$patientID)
	submission$combined_label <- predict(rf, extractFeatures(test2))

	# results evaluation
	tab				<-table(pred = submission$combined_label, true = test[,"combined_label"])
	row_tab_names 	<-rownames(tab)
	col_tab_names 	<-colnames(tab)
	rows		 	<-nrow(tab)
	cols 			<-ncol(tab)
	aciertos		<-0
	bucle 			<- rows
	if (cols>bucle){
		bucle <-cols
	}
	f<-1
	c<-1
	for (i in 1:bucle) {
	   f<-i
	   if (c<=cols &&  row_tab_names[f] == col_tab_names[c]){
			aciertos <- aciertos + tab[f,c]
			
			c <- c+1;
			
		}
	}
	
	

	# hit rate
	total				<-nrow(test)
	hitrate         	<- (100*aciertos)/total
	suma_tot_aciertos 	<- suma_tot_aciertos + hitrate
	
	cat(k," -> ",aciertos ," / "  ,total ,"= ",hitrate,"%"," -> ",cols)
	print("")
	
	
	imp <- importance(rf, type=1)
	featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
	
	# show the most important assets
	p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
		 geom_bar(stat="identity", fill="#53cfff") +
		 coord_flip() + 
		 theme_light(base_size=20) +
		 xlab("") +
		 ylab("Importance") + 
		 ggtitle("Random Forest Feature Importance\n") +
		 theme(plot.title=element_text(size=18))

		ggsave("../Azure/woman/2_feature_importance.png", p)
	
}
cat(" FINAL  -> ",suma_tot_aciertos / n_folds ," %")


