pacman::p_load(readr,autoplotly,ggplot2,plotly,tidyverse,party,lubridate, caret,dplyr,corrplot)

mushroom <- read.csv("Data/Raw/data_train3.csv",header = TRUE, sep = ",")


# Factorizing 2

levels(mushroom$class) <- c("edible", "poisonous")
levels(mushroom$cap.shape) <- c("bell","conical","convex","flat","knobbed","sunken")
levels(mushroom$cap.surface) <- c("fibrous","grooves","scaly","smooth")
levels(mushroom$cap.color) <- c("brown","buff","cinnamon","gray","green","pink","purple","red","white","yellow")
levels(mushroom$bruises) <- c("bruises","no")
levels(mushroom$odor) <- c("almond","anise","creosote","fishy","foul","musty","none","pungent","spicy")
levels(mushroom$gill.attachment) <- c("attached","descending","free","notched")
levels(mushroom$gill.spacing) <- c("close","crowded","distant")
levels(mushroom$gill.size) <- c("broad","narrow")
levels(mushroom$gill.color) <- c("black","brown","buff","chocolate","gray","green","orange","pink","purple","red","white","yellow")
levels(mushroom$stalk.shape) <- c("enlarging","tapering")
levels(mushroom$stalk.root) <- c("bulbous","club","cup","equal","rhizomorphs","rooted","NA")
levels(mushroom$stalk.surface.above.ring) <- c("fibrous","scaly","silky","smooth")
levels(mushroom$stalk.surface.below.ring) <- c("fibrous","scaly","silky","smooth")
levels(mushroom$stalk.color.above.ring) <- c("brown","buff","cinnamon","gray","orange","pink","red","white","yellow")
levels(mushroom$stalk.color.below.ring) <- c("brown","buff","cinnamon","gray","orange","pink","red","white","yellow")
levels(mushroom$veil.type) <- c("partial","universal")
levels(mushroom$veil.color) <- c("brown","orange","white","yellow")
levels(mushroom$ring.number) <- c("none","one","two")
levels(mushroom$ring.type) <- c("cobwebby","evanescent","flaring","large","none","pendant","sheathing","zone")
levels(mushroom$spore.print.color) <- c("black","brown","buff","chocolate","green","orange","purple","white","yellow")
levels(mushroom$population) <- c("abundant","clustered","numerous","scattered","several","solitary")
levels(mushroom$habitat) <- c("grasses","leaves","meadows","paths","urban","waste","woods")




#### Exploring data ####

ggplot(mushroom, aes(x = gill.color, y = cap.color, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))

ggplot(mushroom, aes(x = class, y = gill.attachment, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))

# Plot for class
ggplot(mushroom_noNa,aes(class,fill=class))+geom_bar()



# Omit NAs
mushroom_noNa <- na.omit(mushroom)

mushroom_pic <- mushroom_noNa
mushroom_pic$odor <- NULL
mushroom_pic$X <- NULL
train_mushroom_svm$veil.type <- NULL
# mushroom_pic <- mushroom_pic %>% filter(stalk.root !="?") # This line was not tested. Model trained with "?"



#### Preprocessing ####

mushroom_noNa <- na.omit(mushroom)
mushroom_pic <- mushroom_noNa
mushroom_pic$odor <- NULL
mushroom_pic$X <- NULL
mushroom_pic$veil.type <- NULL
mushroom_pic$stalk.root <- NULL

#### Data partition ####

set.seed(1810)
mushsample <- caret::createDataPartition(y = mushroom_pic$class, times = 1, p = 0.8, list = FALSE)
train_mushroom <- mushroom_pic[mushsample, ]
test_mushroom <- mushroom_pic[-mushsample, ]


#### !Random Forest with penalty matrix ####
PenaltyMatrix <- matrix(c(0, 0.000001, 10000, 0), byrow = TRUE, nrow = 2)  

#Random Forest Model
model_rf_pen<-randomForest(class ~ ., data = train_mushroom,ntree=400,nodesize=20,cp=0.001,parms = list(loss = PenaltyMatrix)) # Random Forest model

saveRDS(model_rf_pen, file= "model_rf_pen_factor_2.rds")


predrf <-  predict(model_rf_pen,newdata=test_mushroom,type="response") 
table(test_mushroom$class,predrf)

