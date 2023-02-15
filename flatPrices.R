
#libraries instalation
install.packages("Hmisc")
library("Hmisc")
install.packages("corrplot")
library("corrplot")
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
install.packages("correlation")
library(correlation)
install.packages("GGally")
library(GGally)
install.packages("AICcmodavg")
library(AICcmodavg)
install.packages("car")
library(car)
install.packages('AICcmodavg')
library(AICcmodavg)
install.packages('relaimpo')
library(relaimpo)
install.packages('lmtest')
library(lmtest)
install.packages('mctest')
library(mctest)
install.packages('ggplot2')
library(ggplot2)
install.packages('dplyr')
library(dplyr)
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(tidyverse)
library(ggplot2)
library(car)

#We need to download dataset from clipboard with headers
my_houses <- read.table(file = "clipboard", 
                        sep = "\t", header=TRUE)
#Створимо нові стовпчики для кожної фіктивної змінної та заповнимо їх нулями
my_houses$sv <- 0
my_houses$pch <- 0
my_houses$gl <- 0
my_houses$dn <- 0
my_houses$ob <- 0
my_houses$pod <- 0
my_houses$borsch <- 0
my_houses$shev <- 0
my_houses$darn <- 0

#Напишемо окд для заповнення стовпчиків одиницями за певною умовою, напрклад для першої фіктивної змінної 
#Якщо регіон має назву Святошиньский, то заповнюємо ті рядочки в стовпчику sv одиничками
my_houses$sv[my_houses$rajon=="sviatoshyn"]<-1
my_houses$pch[my_houses$rajon=="pechersk"]<-1
my_houses$gl[my_houses$rajon=="golosievski"]<-1
my_houses$dn[my_houses$rajon=="dniprovski"]<-1
my_houses$ob[my_houses$rajon=="obolon"]<-1
my_houses$pod[my_houses$rajon=="podilski"]<-1
my_houses$borsch[my_houses$rajon=="borschagivka"]<-1
my_houses$shev[my_houses$rajon=="shevchenkivskiy"]<-1
my_houses$darn[my_houses$rajon=="darnytskiy"]<-1


#Створимо нові стовпчики для кожної фіктивної змінної для матеріалу з якого був побудований будинок 
my_houses$monolithic <- 0
my_houses$brick <- 0
my_houses$monolithic[my_houses$material_type=="monolithic-frame"]<-1
my_houses$brick[my_houses$material_type=="brick"]<-1

#Створимо нові стовпчики для кожної фіктивної змінної для типів опалення наявних в забудові
my_houses$central <- 0
my_houses$central[my_houses$heating=="central-heating"]<-1



#Створимо нові стовпчики для кожної фіктивної змінної для типів квартир
my_houses$special <- 0
my_houses$separate <- 0
my_houses$khrush  <- 0
my_houses$small  <- 0
my_houses$stud  <- 0
my_houses$adj  <- 0
my_houses$euro  <- 0
my_houses$stalin  <- 0
my_houses$chech  <- 0
my_houses$pre  <- 0
my_houses$special[my_houses$flat_type=="special"]<-1
my_houses$separate[my_houses$flat_type=="separate"]<-1
my_houses$khrush[my_houses$flat_type=="khrushchev"]<-1
my_houses$stud[my_houses$flat_type=="studio"]<-1
my_houses$small[my_houses$flat_type=="small_size"]<-1
my_houses$euro[my_houses$flat_type=="euro"]<-1
my_houses$stalin[my_houses$flat_type=="stalinka"]<-1
my_houses$chech[my_houses$flat_type=="chech"]<-1
my_houses$adj[my_houses$flat_type=="adjacent"]<-1
my_houses$pre[my_houses$flat_type=="pre-revolutionary"]<-1








#Створимо новий масив зі старого, з якого видалимо не потрібні нам стовпчики,  а саме ті, які ми перетворили у фіктивні зінні
my_houses<-my_houses %>%
  mutate(priceM2=as.double(gsub(",", ".",my_houses$prica.m.2),2),
         floor=as.double(gsub(",", ".",my_houses$floor),2),
         rooms=as.double(gsub(",", ".",my_houses$rooms),2),
         year=as.double(gsub(",", ".",my_houses$year),2),
         monolithic=as.double(gsub(",", ".",my_houses$monolithic),2),
         remont=as.double(gsub(",", ".",my_houses$remont),2),
         baths=as.double(gsub(",", ".",my_houses$baths),2),
         balcony=as.double(gsub(",", ".",my_houses$balcony),2),
         height=as.double(gsub(",", ".",my_houses$height),2))


#Видалимо не потрібні стовпчики
my_houses <- subset(my_houses, select = -c(prica.m.2,price,rajon,material_type,heating,flat_type))              
vec_new <- gsub(",", ".", my_houses)     


# Створимо кореляційну матрицю
houses_coreliations <- cor(my_houses)
round(houses_coreliations, 2)


#Створимо кореляції p-value за допомогою функції rcorr пакета Hmisc
corPV <- rcorr(as.matrix(my_houses))
corPV
round(corPV$P, 2)

#Візуалізація кореляційної матриці за допомогою пакету corplot
corrplot(houses_coreliations, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, sig.level = 1)
my_houses <- subset(my_houses, select = -c(floor,year,rooms,monolithic,special))
corrplot(houses_coreliations, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, sig.level = 0.5)
chart.Correlation(my_houses, histogram=TRUE, pch=19)
pairs(my_houses)

#Зобразимо кореляції та p-значення в покращеному вигляді
CorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
PV_Cor<- CorrMatrix(corPV$r, corPV$P)
PV_Cor
#Регресійний аналіз
anov_model <- aov(my_houses$priceM2 ~ my_houses$remont+my_houses$baths+my_houses$balcony+my_houses$height+my_houses$sv+my_houses$pch+my_houses$gl+my_houses$dn+my_houses$ob+my_houses$pod+my_houses$borsch+my_houses$shev+my_houses$darn+my_houses$brick+my_houses$central+my_houses$separate+my_houses$khrush+my_houses$small+my_houses$stud+my_houses$adj+my_houses$euro+my_houses$stalin+my_houses$chech+my_houses$pre, my_houses)
anov_model_lm <- lm(my_houses$priceM2 ~ my_houses$remont+my_houses$baths+my_houses$balcony+my_houses$height+my_houses$sv+my_houses$pch+my_houses$gl+my_houses$dn+my_houses$ob+my_houses$pod+my_houses$borsch+my_houses$shev+my_houses$darn+my_houses$brick+my_houses$central+my_houses$separate+my_houses$khrush+my_houses$small+my_houses$stud+my_houses$adj+my_houses$euro+my_houses$stalin+my_houses$chech+my_houses$pre, my_houses)
anova1<-Anova(anov_model, type=3)
anova1
summary(anov_model_lm)

#Покращення
anov_model_better <- aov(my_houses$priceM2 ~ my_houses$pch+my_houses$dn+my_houses$shev+my_houses$separate+my_houses$khrush+my_houses$chech, my_houses)
anov_model_lm_better <- lm(my_houses$priceM2 ~ my_houses$pch+my_houses$dn+my_houses$shev+my_houses$separate+my_houses$khrush+my_houses$chech, my_houses)
anova1_better<-Anova(anov_model_better, type=3)
anova1_better
summary(anov_model_lm_better)


#Модель лише з фіктивних змінних
anov_model_fict <- aov(my_houses$priceM2 ~ my_houses$sv+my_houses$pch+my_houses$gl+my_houses$dn+my_houses$ob+my_houses$pod+my_houses$borsch+my_houses$shev+my_houses$darn+my_houses$brick+my_houses$central+my_houses$separate+my_houses$khrush+my_houses$small+my_houses$stud+my_houses$adj+my_houses$euro+my_houses$stalin+my_houses$chech+my_houses$pre, my_houses)
anov_model_lm_fict <- lm(my_houses$priceM2 ~ my_houses$sv+my_houses$pch+my_houses$gl+my_houses$dn+my_houses$ob+my_houses$pod+my_houses$borsch+my_houses$shev+my_houses$darn+my_houses$brick+my_houses$central+my_houses$separate+my_houses$khrush+my_houses$small+my_houses$stud+my_houses$adj+my_houses$euro+my_houses$stalin+my_houses$chech+my_houses$pre, my_houses)
anova2_fict<-Anova(anov_model_fict, type=3)
anova2_fict
summary(anov_model_lm_fict)

#Покращена
anov_model_fict_better <- aov(my_houses$priceM2 ~ my_houses$pch+my_houses$dn+my_houses$shev+my_houses$separate+my_houses$khrush+my_houses$adj+my_houses$stalin+my_houses$chech, my_houses)
anov_model_fict_lm_better <- lm(my_houses$priceM2 ~ my_houses$pch+my_houses$dn+my_houses$shev+my_houses$separate+my_houses$khrush+my_houses$adj+my_houses$stalin+my_houses$chech, my_houses)
anova2_fict_better<-Anova(anov_model_fict_better, type=3)
anova2_fict_better
summary(anov_model_fict_lm_better)

#Модель лише з кількісних змінних
anov_model_kilk <- aov(my_houses$priceM2 ~ my_houses$remont+my_houses$baths+my_houses$balcony+my_houses$height, my_houses)
anov_model_lm_kilk <- lm(my_houses$priceM2 ~ my_houses$remont+my_houses$baths+my_houses$balcony+my_houses$height, my_houses)
anova3_kilk<-Anova(anov_model_kilk, type=3)
anova3_kilk
summary(anov_model_lm_kilk)

#Пошук найбільш якісної моделі
model.set <- list(anov_model,anov_model_better,anov_model_fict,anov_model_fict_better,anov_model_kilk)
model.names <- c("Повна модель", "Покращена модель зі всіма змінними", "повна якісн", "покращена якісна", "лише кількісна модель")
aictab(model.set, modnames = model.names)

#Visualization regression data

# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(anov_model_fict_lm_better, b = 1000, type = c("lmg"), rank = TRUE,
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result 

#Графік залишків моделі
res <- resid(anov_model_fict_lm_better)
plot(fitted(anov_model_fict_lm_better), res)
abline(0,0)

#Гістограма залишків
hist(anov_model_fict_lm_better$residuals, main = "Histogram of Residuals", xlab= "")

# create Q-Q plot for residuals
qqnorm(res)
# add a straight diagonal line 
# to the plot
qqline(res)
#відобразимо графік щільності
plot(density(res))

#Тест гольфренда кванта на наявність гетероскадестичності
plot(anov_model_fict_lm_better)

par(mfrow=c(2,2))
plot(anov_model_fict_lm_better)





#Мультиколінеарінсть
install.packages("caTools")    # For Linear regression 
install.packages('car')        # To check multicollinearity 
install.packages("quantmod")
install.packages("MASS")
install.packages("corrplot")   # plot correlation plot

library(caTools)
library(car)
library(quantmod)
library(MASS)
library(corrplot)

vif(anov_model_fict_lm_better)
var_inv <- ginv(houses_coreliations)   

corrplot(var_inv,method='number',is.corr = F)  

#Прогнозування
summary(anov_model_fict_lm_better)
new <- data.frame(dn=1, separate=1,pch=0,shev=0,khrush=0,adj=0,stalin=0,adj=0,chech=0)

#use the fitted model to predict the rating for the new player
predict(anov_model_fict_lm_better, newdata=new)
#Для всіх змінних існують досить низькі значення, а це говорить нам про відсутність високої  кореляції з іншими зміними та не є проблемою.

#Anova test
install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

#Проведемо аналіз гомоскадестичності

par(mfrow=c(2,2))
plot(anov_model_fict_lm_better)
par(mfrow=c(1,1))


#Автокореляція
durbinWatsonTest(anov_model_fict_lm_better)





