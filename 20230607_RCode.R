
setwd('C:/Users/debap/Box/Research/YaelProject/Quant Analysis')
# Load the poLCA library
library(poLCA)
#install.packages('polycor')
library(polycor)
library(psych)
library(dplyr)
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(gtools)
library(gridExtra)
### Importing Datasets
mydata <- read.csv('qns.csv')
demo=read.csv('demo.csv')
desc=read_excel('DataDescription.xlsx')


### Defining the model for LCA for both surveys


fwModStr<-with(mydata, 
               cbind(fwQ1_1,fwQ1_2,fwQ1_3,fwQ1_4,fwQ1_5,fwQ1_6,fwQ1_7,fwQ1_8,fwQ1_9,fwQ1_10,fwQ1_11,fwQ1_12,fwQ1_13,fwQ1_14,fwQ1_15,fwQ1_16,fwQ2_1,fwQ2_2,fwQ2_3,fwQ2_4,fwQ2_5,fwQ2_6,fwQ3,fwQ4,fwQ5,fwQ6,fwQ7,fwQ8,fwQ9,fwQ10,fwQ11,fwQ12,fwQ13,fwQ14,fwQ15,fwQ16,fwQ17,fwQ18,fwQ19,fwQ20,fwQ21,fwQ22,fwQ23,fwQ24,fwQ25,fwQ26,fwQ27,fwQ28,fwQ29,fwQ30,fwQ31,fwQ32,fwQ33,fwQ34,fwQ35,fwQ36,fwQ37,fwQ38,fwQ39,fwQ40,fwQ41,fwQ42,fwQ43,fwQ44,fwQ45,fwQ46
                     #poe1Q1,poe1Q2,poe1Q3,poe1Q4,poe1Q5,poe1Q6,poe1Q7,poe1Q8,poe1Q9,poe1Q10,poe1Q11,poe1Q12,poe1Q13,poe1Q14,poe1Q15,poe1Q16,poe1Q17,poe1Q18
                     #,poe2Q1,poe2Q2,poe2Q3,poe2Q4,poe2Q5,poe2Q6,poe2Q7,poe2Q8,poe2Q9,poe2Q10,poe2Q11,poe2Q12,poe2Q13,poe2Q14,poe2Q15,poe2Q16,poe2Q17,poe2Q18
               )~1)


poeModStr<-with(mydata, 
        cbind(#fwQ1_1,fwQ1_2,fwQ1_3,fwQ1_4,fwQ1_5,fwQ1_6,fwQ1_7,fwQ1_8,fwQ1_9,fwQ1_10,fwQ1_11,fwQ1_12,fwQ1_13,fwQ1_14,fwQ1_15,fwQ1_16,fwQ2_1,fwQ2_2,fwQ2_3,fwQ2_4,fwQ2_5,fwQ2_6,fwQ3,fwQ4,fwQ5,fwQ6,fwQ7,fwQ8,fwQ9,fwQ10,fwQ11,fwQ12,fwQ13,fwQ14,fwQ15,fwQ16,fwQ17,fwQ18,fwQ19,fwQ20,fwQ21,fwQ22,fwQ23,fwQ24,fwQ25,fwQ26,fwQ27,fwQ28,fwQ29,fwQ30,fwQ31,fwQ32,fwQ33,fwQ34,fwQ35,fwQ36,fwQ37,fwQ38,fwQ39,fwQ40,fwQ41,fwQ42,fwQ43,fwQ44,fwQ45,fwQ46,
              poe1Q1,poe1Q2,poe1Q3,poe1Q4,poe1Q5,poe1Q6,poe1Q7,poe1Q8,poe1Q9,poe1Q10,poe1Q11,poe1Q12,poe1Q13,poe1Q14,poe1Q15,poe1Q16,poe1Q17,poe1Q18
              ,poe2Q1,poe2Q2,poe2Q3,poe2Q4,poe2Q5,poe2Q6,poe2Q7,poe2Q8,poe2Q9,poe2Q10,poe2Q11,poe2Q12,poe2Q13,poe2Q14,poe2Q15,poe2Q16,poe2Q17,poe2Q18
              )~1)



#####################################################################
################## For First Week ###################################
#####################################################################

max_II <- -100000
min_bic <- 100000
for(i in 2:10){
  lc <- poLCA(poeModStr, mydata, nclass=i, maxiter=3000, 
              tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=TRUE)
  print(i)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model<-lc
  }
} ## Failed at number of classes = 5    	
LCA_best_model


### Selecting columns


fwdata <- mydata %>% dplyr::select(fwQ1_1,fwQ1_2,fwQ1_3,fwQ1_4,fwQ1_5,fwQ1_6,fwQ1_7,fwQ1_8,fwQ1_9,fwQ1_10,fwQ1_11,fwQ1_12,fwQ1_13,fwQ1_14,fwQ1_15,fwQ1_16,fwQ2_1,fwQ2_2,fwQ2_3,fwQ2_4,fwQ2_5,fwQ2_6,fwQ3,fwQ4,fwQ5,fwQ6,fwQ7,fwQ8,fwQ9,fwQ10,fwQ11,fwQ12,fwQ13,fwQ14,fwQ15,fwQ16,fwQ17,fwQ18,fwQ19,fwQ20,fwQ21,fwQ22,fwQ23,fwQ24,fwQ25,fwQ26,fwQ27,fwQ28,fwQ29,fwQ30,fwQ31,fwQ32,fwQ33,fwQ34,fwQ35,fwQ36,fwQ37,fwQ38,fwQ39,fwQ40,fwQ41,fwQ42,fwQ43,fwQ44,fwQ45,fwQ46
                                 #,poe1Q1,poe1Q2,poe1Q3,poe1Q4,poe1Q5,poe1Q6,poe1Q7,poe1Q8,poe1Q9,poe1Q10,poe1Q11,poe1Q12,poe1Q13,poe1Q14,poe1Q15,poe1Q16,poe1Q17,poe1Q18
                                 #,poe2Q1,poe2Q2,poe2Q3,poe2Q4,poe2Q5,poe2Q6,poe2Q7,poe2Q8,poe2Q9,poe2Q10,poe2Q11,poe2Q12,poe2Q13,poe2Q14,poe2Q15,poe2Q16,poe2Q17,poe2Q18)
                                 )

set.seed(01012)

lc1<-poLCA(fwModStr, data=fwdata, nclass=1, na.rm = FALSE, nrep=10, maxiter=3000) #Loglinear independence model.
lc2<-poLCA(fwModStr, data=fwdata, nclass=2, na.rm = FALSE, nrep=10, maxiter=3000)
lc3<-poLCA(fwModStr, data=fwdata, nclass=3, na.rm = FALSE, nrep=10, maxiter=3000)
lc4<-poLCA(fwModStr, data=mydata, nclass=4, na.rm = FALSE, nrep=10, maxiter=3000) 
#lc5<-poLCA(fwModStr, data=mydata, nclass=5, na.rm = FALSE, nrep=10, maxiter=3000) 


## Calculate bic 
bic=c(lc1$bic,lc2$bic,lc3$bic,lc4$bic)

print(bic) ## 4 is minimum while 3 is close by 

### Visualising class response probabilities 

lcmodel=reshape2::melt(lc4$probs, level=2)
lcmodel=as.data.frame(lcmodel)
lcmodel=lcmodel[order(lcmodel$L2), ]
vec=unique(lcmodel[order(lcmodel$L2), ]$L2)
sorted_vec=mixedsort(vec)
lcmodel$L2 <- factor(lcmodel$L2,      
                     levels = sorted_vec)


# vec <- c("poe1Q1", "poe1Q10", "poe1Q11", "poe1Q12", "poe1Q13", "poe1Q14", "poe1Q15",
#          "poe1Q16", "poe1Q17", "poe1Q18", "poe1Q2", "poe1Q3", "poe1Q4", "poe1Q5",
#          "poe1Q6", "poe1Q7", "poe1Q8", "poe1Q9", "poe2Q1", "poe2Q10", "poe2Q11",
#          "poe2Q12", "poe2Q13", "poe2Q14", "poe2Q15", "poe2Q16", "poe2Q17", "poe2Q18",
#          "poe2Q2", "poe2Q3", "poe2Q4", "poe2Q5", "poe2Q6", "poe2Q7", "poe2Q8",
#          "poe2Q9")
# 
# sorted_vec=c("poe1Q1","poe1Q2","poe1Q3","poe1Q4","poe1Q5","poe1Q6","poe1Q7","poe1Q8","poe1Q9",
#              "poe2Q1","poe2Q2","poe2Q3","poe2Q4","poe2Q5","poe2Q6","poe2Q7","poe2Q8","poe2Q9",
#              "poe1Q10","poe1Q11","poe1Q12","poe1Q13","poe1Q14","poe1Q15","poe1Q16","poe1Q17","poe1Q18",
#              "poe2Q10","poe2Q11","poe2Q12","poe2Q13","poe2Q14","poe2Q15","poe2Q16","poe2Q17","poe2Q18")
# 
# 
# # Reordering group factor levels

#lcmodel
###### Viz #################
zp2 <- ggplot(lcmodel, aes(x = Var1, y = value, fill = Var2))

# Define the custom color scale
custom_colors <- c("Pr(1)" = "red", "Pr(2)" = "tomato", "Pr(3)" = "orange",
                   "Pr(4)" = "springgreen3", "Pr(5)" = "green", "Pr(6)" = "forestgreen")

zp2 <- zp2 + geom_bar(stat = "identity", position = "stack")
zp2 <- zp2 + facet_wrap(~ L2,ncol=9, scales = "free_y")
zp2 <- zp2 + scale_x_discrete("Items", expand = c(0, 0))
zp2 <- zp2 + scale_y_continuous("Class Probabilities", expand = c(0, 0))
zp2 <- zp2 + scale_fill_manual(values = custom_colors) +  # Use the custom color scale
  theme_bw()
zp2 <- zp2 + labs(fill = "Probability of each response")
zp2 <- zp2 + theme(axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   panel.grid.major.y = element_blank())
zp2 <- zp2 + guides(fill = guide_legend(reverse = TRUE))
zp2 <- zp2 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(zp2)

############ Posterior Inference ###################

## Posterior Probabilities

post = round(lc4$posterior,0)
rownames(post) = rownames(lc4$y)

post=as.data.frame(post)



### Grades 

grade=demo[,c('finalletter.grade','mdtLetter.grade', "finalraw.grade","mdtOverall.raw.grade"  )]
# head(grade[grade$finalletter.grade=="D",])
# head(grade[grade$finalletter.grade==" ",]) ## 541 - C , 15 - D, 162 - F
grade$finalletter.grade[541] <- "C"
grade$finalletter.grade[15] <- "D"
grade$finalletter.grade[162] <- "F"

grade$finalletter.grade <- gsub("H", "", grade$finalletter.grade)
# 
# grade_summary <- grade %>%
#   group_by(finalletter.grade) %>%
#   summarise(
#     mean_finalraw = mean(finalraw.grade, na.rm = TRUE),
#     median_finalraw = median(finalraw.grade, na.rm = TRUE),
#     sd_finalraw = sd(finalraw.grade, na.rm = TRUE)
#   )

# grade_summary
names(post)=c('Class 1','Class 2','Class 3','Class 4')
post$Class <- colnames(post)[max.col(post)]


### Grades and Class Labels  
gradeclass=cbind(grade,post)



finalfreqs <- as.data.frame(table(gradeclass[,c('finalletter.grade','Class')]))
mdtfreqs = as.data.frame(table(gradeclass[,c('mdtLetter.grade','Class')]))

## Getting row percentage 

mdtfreqs$row_percent <- mdtfreqs$Freq / tapply(mdtfreqs$Freq, mdtfreqs$Class, sum)[mdtfreqs$Class]
finalfreqs$row_percent <- finalfreqs$Freq / tapply(finalfreqs$Freq, finalfreqs$Class, sum)[finalfreqs$Class]


## Heatmap 
# Midterm
ggplot(mdtfreqs, aes(x = mdtLetter.grade, y = Class, fill = row_percent)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "green") +
  theme_minimal()

# Final
ggplot(finalfreqs, aes(x = finalletter.grade, y = Class, fill = row_percent)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "green") +
  theme_minimal()




p1 <- ggplot(gradeclass, aes(x = Class, y = finalraw.grade, fill = Class)) +
  geom_boxplot() +
  ggtitle("Box Plot of Final Score by Class") +
  theme_minimal()

# Create the second plot
p2 <- ggplot(gradeclass, aes(x = Class, y = mdtOverall.raw.grade, fill = Class)) +
  geom_boxplot() +
  ggtitle("Box Plot of Midterm Score by Class") +
  theme_minimal()

# Arrange the plots side by side
grid.arrange(p2, p1, ncol = 2)

# Set the plot size
options(repr.plot.width = 95/10 , repr.plot.height = 50/10)



#####################################################################
################## For Post Exam Survey ###################################
#####################################################################

max_II <- -100000
min_bic <- 100000
for(i in 2:10){
  lc <- poLCA(poeModStr, mydata, nclass=i, maxiter=3000, 
              tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=TRUE)
  print(i)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model<-lc
  }
} ## Failed at number of classes = 5    	
LCA_best_model


### Selecting columns


fwdata <- mydata %>% dplyr::select(fwQ1_1,fwQ1_2,fwQ1_3,fwQ1_4,fwQ1_5,fwQ1_6,fwQ1_7,fwQ1_8,fwQ1_9,fwQ1_10,fwQ1_11,fwQ1_12,fwQ1_13,fwQ1_14,fwQ1_15,fwQ1_16,fwQ2_1,fwQ2_2,fwQ2_3,fwQ2_4,fwQ2_5,fwQ2_6,fwQ3,fwQ4,fwQ5,fwQ6,fwQ7,fwQ8,fwQ9,fwQ10,fwQ11,fwQ12,fwQ13,fwQ14,fwQ15,fwQ16,fwQ17,fwQ18,fwQ19,fwQ20,fwQ21,fwQ22,fwQ23,fwQ24,fwQ25,fwQ26,fwQ27,fwQ28,fwQ29,fwQ30,fwQ31,fwQ32,fwQ33,fwQ34,fwQ35,fwQ36,fwQ37,fwQ38,fwQ39,fwQ40,fwQ41,fwQ42,fwQ43,fwQ44,fwQ45,fwQ46
                                   #,poe1Q1,poe1Q2,poe1Q3,poe1Q4,poe1Q5,poe1Q6,poe1Q7,poe1Q8,poe1Q9,poe1Q10,poe1Q11,poe1Q12,poe1Q13,poe1Q14,poe1Q15,poe1Q16,poe1Q17,poe1Q18
                                   #,poe2Q1,poe2Q2,poe2Q3,poe2Q4,poe2Q5,poe2Q6,poe2Q7,poe2Q8,poe2Q9,poe2Q10,poe2Q11,poe2Q12,poe2Q13,poe2Q14,poe2Q15,poe2Q16,poe2Q17,poe2Q18)
)

set.seed(01012)

lc1<-poLCA(fwModStr, data=fwdata, nclass=1, na.rm = FALSE, nrep=10, maxiter=3000) #Loglinear independence model.
lc2<-poLCA(fwModStr, data=fwdata, nclass=2, na.rm = FALSE, nrep=10, maxiter=3000)
lc3<-poLCA(fwModStr, data=fwdata, nclass=3, na.rm = FALSE, nrep=10, maxiter=3000)
lc4<-poLCA(fwModStr, data=mydata, nclass=4, na.rm = FALSE, nrep=10, maxiter=3000) 
#lc5<-poLCA(fwModStr, data=mydata, nclass=5, na.rm = FALSE, nrep=10, maxiter=3000) 


## Calculate bic 
bic=c(lc1$bic,lc2$bic,lc3$bic,lc4$bic)

print(bic) ## 4 is minimum while 3 is close by 

### Visualising class response probabilities 

lcmodel=reshape2::melt(lc4$probs, level=2)
lcmodel=as.data.frame(lcmodel)
lcmodel=lcmodel[order(lcmodel$L2), ]
vec=unique(lcmodel[order(lcmodel$L2), ]$L2)
sorted_vec=mixedsort(vec)
lcmodel$L2 <- factor(lcmodel$L2,      
                     levels = sorted_vec)


# vec <- c("poe1Q1", "poe1Q10", "poe1Q11", "poe1Q12", "poe1Q13", "poe1Q14", "poe1Q15",
#          "poe1Q16", "poe1Q17", "poe1Q18", "poe1Q2", "poe1Q3", "poe1Q4", "poe1Q5",
#          "poe1Q6", "poe1Q7", "poe1Q8", "poe1Q9", "poe2Q1", "poe2Q10", "poe2Q11",
#          "poe2Q12", "poe2Q13", "poe2Q14", "poe2Q15", "poe2Q16", "poe2Q17", "poe2Q18",
#          "poe2Q2", "poe2Q3", "poe2Q4", "poe2Q5", "poe2Q6", "poe2Q7", "poe2Q8",
#          "poe2Q9")
# 
# sorted_vec=c("poe1Q1","poe1Q2","poe1Q3","poe1Q4","poe1Q5","poe1Q6","poe1Q7","poe1Q8","poe1Q9",
#              "poe2Q1","poe2Q2","poe2Q3","poe2Q4","poe2Q5","poe2Q6","poe2Q7","poe2Q8","poe2Q9",
#              "poe1Q10","poe1Q11","poe1Q12","poe1Q13","poe1Q14","poe1Q15","poe1Q16","poe1Q17","poe1Q18",
#              "poe2Q10","poe2Q11","poe2Q12","poe2Q13","poe2Q14","poe2Q15","poe2Q16","poe2Q17","poe2Q18")
# 
# 
# # Reordering group factor levels

#lcmodel
###### Viz #################
zp2 <- ggplot(lcmodel, aes(x = Var1, y = value, fill = Var2))

# Define the custom color scale
custom_colors <- c("Pr(1)" = "red", "Pr(2)" = "tomato", "Pr(3)" = "orange",
                   "Pr(4)" = "springgreen3", "Pr(5)" = "green", "Pr(6)" = "forestgreen")

zp2 <- zp2 + geom_bar(stat = "identity", position = "stack")
zp2 <- zp2 + facet_wrap(~ L2,ncol=9, scales = "free_y")
zp2 <- zp2 + scale_x_discrete("Items", expand = c(0, 0))
zp2 <- zp2 + scale_y_continuous("Class Probabilities", expand = c(0, 0))
zp2 <- zp2 + scale_fill_manual(values = custom_colors) +  # Use the custom color scale
  theme_bw()
zp2 <- zp2 + labs(fill = "Probability of each response")
zp2 <- zp2 + theme(axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   panel.grid.major.y = element_blank())
zp2 <- zp2 + guides(fill = guide_legend(reverse = TRUE))
zp2 <- zp2 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(zp2)

############ Posterior Inference ###################

## Posterior Probabilities

post = round(lc4$posterior,0)
rownames(post) = rownames(lc4$y)

post=as.data.frame(post)



### Grades 

grade=demo[,c('finalletter.grade','mdtLetter.grade', "finalraw.grade","mdtOverall.raw.grade"  )]
# head(grade[grade$finalletter.grade=="D",])
# head(grade[grade$finalletter.grade==" ",]) ## 541 - C , 15 - D, 162 - F
grade$finalletter.grade[541] <- "C"
grade$finalletter.grade[15] <- "D"
grade$finalletter.grade[162] <- "F"

grade$finalletter.grade <- gsub("H", "", grade$finalletter.grade)
# 
# grade_summary <- grade %>%
#   group_by(finalletter.grade) %>%
#   summarise(
#     mean_finalraw = mean(finalraw.grade, na.rm = TRUE),
#     median_finalraw = median(finalraw.grade, na.rm = TRUE),
#     sd_finalraw = sd(finalraw.grade, na.rm = TRUE)
#   )

# grade_summary
names(post)=c('Class 1','Class 2','Class 3','Class 4')
post$Class <- colnames(post)[max.col(post)]


### Grades and Class Labels  
gradeclass=cbind(grade,post)



finalfreqs <- as.data.frame(table(gradeclass[,c('finalletter.grade','Class')]))
mdtfreqs = as.data.frame(table(gradeclass[,c('mdtLetter.grade','Class')]))

## Getting row percentage 

mdtfreqs$row_percent <- mdtfreqs$Freq / tapply(mdtfreqs$Freq, mdtfreqs$Class, sum)[mdtfreqs$Class]
finalfreqs$row_percent <- finalfreqs$Freq / tapply(finalfreqs$Freq, finalfreqs$Class, sum)[finalfreqs$Class]


## Heatmap 
# Midterm
ggplot(mdtfreqs, aes(x = mdtLetter.grade, y = Class, fill = row_percent)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "green") +
  theme_minimal()

# Final
ggplot(finalfreqs, aes(x = finalletter.grade, y = Class, fill = row_percent)) +
  geom_tile() +
  scale_fill_gradient(low = "red", high = "green") +
  theme_minimal()




p1 <- ggplot(gradeclass, aes(x = Class, y = finalraw.grade, fill = Class)) +
  geom_boxplot() +
  ggtitle("Box Plot of Final Score by Class") +
  theme_minimal()

# Create the second plot
p2 <- ggplot(gradeclass, aes(x = Class, y = mdtOverall.raw.grade, fill = Class)) +
  geom_boxplot() +
  ggtitle("Box Plot of Midterm Score by Class") +
  theme_minimal()

# Arrange the plots side by side
grid.arrange(p2, p1, ncol = 2)

# Set the plot size
options(repr.plot.width = 95/10 , repr.plot.height = 50/10)