---
  title: "Cardiovascular Diseases Death Prediction"
author: "Zihao Shen; Yuchen Shen; Yinan Gu"
date: "2020/11/30"
output: word_document
---
  # import the data
  ```{r import data, warning=FALSE,message=FALSE}
library(readr)
heart_failure_clinical_records_dataset <- read_csv("heart_failure_clinical_records_dataset.csv",col_types = cols(anaemia = col_factor(levels = c("0", "1")), diabetes = col_factor(levels = c("0","1")), high_blood_pressure = col_factor(levels = c("0","1")), sex = col_factor(levels = c("0","1")), smoking = col_factor(levels = c("0","1")), DEATH_EVENT = col_factor(levels = c("0","1"))))
heart <- heart_failure_clinical_records_dataset
```

# all the libries and packages we will use & the elementary strudy on the dataset
```{r elementary cognition, warning=FALSE,message=FALSE}
library(modelr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
library(splines)
library(ggrepel)
sum(is.na(heart))
sum(heart$sex==0)
sum(heart$sex==1)
#age
ggplot(data=heart,mapping = aes(x=age))+geom_histogram()
ggplot(data=heart,mapping = aes(x=age))+geom_histogram()+facet_wrap(heart$DEATH_EVENT)
#the variables information
ggplot(data=heart,mapping = aes(x=time,fill=DEATH_EVENT))+geom_bar(position = "fill")
ggplot(data=heart,mapping = aes(x=smoking,fill=DEATH_EVENT))+geom_bar(position = "fill")
ggplot(data=heart,mapping = aes(x=high_blood_pressure,fill=DEATH_EVENT))+geom_bar(position = "fill")
ggplot(data=heart,mapping = aes(x=diabetes,fill=DEATH_EVENT))+geom_bar(position = "fill")
ggplot(data=heart,mapping = aes(x=anaemia,fill=DEATH_EVENT))+geom_bar(position = "fill")
ggplot(data=heart,mapping = aes(x=anaemia,fill=DEATH_EVENT))+geom_bar(position = "stack")
ggplot(data=heart,mapping = aes(x=sex,fill=DEATH_EVENT))+geom_bar(position = "fill")
#continuous variables evaluation
ggplot(data = heart,mapping = aes(x=heart$creatinine_phosphokinase,fill=DEATH_EVENT))+geom_histogram(position = "stack")
ggplot(data = heart,mapping = aes(x=heart$ejection_fraction,fill=DEATH_EVENT))+geom_histogram(position = "stack")
ggplot(data = heart,mapping = aes(x=heart$platelets,fill=DEATH_EVENT))+geom_histogram(position = "stack")
ggplot(data = heart,mapping = aes(x=heart$serum_creatinine,fill=DEATH_EVENT))+geom_histogram(position = "stack")
ggplot(data = heart,mapping = aes(x=heart$serum_sodium,fill=DEATH_EVENT))+geom_histogram(position = "stack")

```

# Some interesting phonomenon
## the trend of medium value of serum creatinine of diabetes or not is the same.
```{r expected phonomena }
ggplot(data=heart,mapping = aes(x=heart$high_blood_pressure,y=heart$serum_creatinine,color=DEATH_EVENT))+geom_boxplot()+xlab("high blood pressure?(0:NO; 1:YES)")+ylab("serum creatinine")

ggplot(data=heart,mapping = aes(x=heart$diabetes,y=heart$serum_creatinine,color=heart$DEATH_EVENT))+geom_boxplot()#the trends are the same
```


## we want to see some effect of some of the vaiables on the death event, and from the facet boxplot, we find that: People who die within the observation period without high blood pressure and diabetes have lower serum creatinine than those who do not have high blood pressure but get diabetes.
```{r unexpected trend}
ggplot(data=heart,mapping = aes(x=heart$high_blood_pressure,y=heart$serum_creatinine,color=DEATH_EVENT))+geom_boxplot()+facet_wrap(heart$diabetes)+xlab("high blood pressure?(0:NO; 1:YES)")+ylab("serum creatinine (mg/dL)")+scale_color_hue("death event",breaks=c("0","1"),labels=c("survive","dead"))
#the trend is different!
```

## we can find that that some clearly bad factors to our health may somehow have a positive effect on the diease.
```{r smoking people are more likely to survive}
heart %>% count(anaemia, DEATH_EVENT)%>%
  ggplot(mapping=aes(x=factor(anaemia), y=factor(DEATH_EVENT)))+
  geom_tile(mapping=aes(fill=n))+
  labs(x="anaemia", y="death")
```

# study on the covariation
```{r}
need_con_data <- melt(select(heart, age, creatinine_phosphokinase, ejection_fraction, platelets, serum_creatinine,serum_sodium, DEATH_EVENT),id=c("DEATH_EVENT"), variable.name = "x", value.name = "y")
need_cate_data <- melt(select(heart, anaemia, diabetes, high_blood_pressure, smoking, DEATH_EVENT),id=c("DEATH_EVENT"), variable.name = "x", value.name = "y")
need_cate_data$y <- as.numeric(as.character(need_cate_data$y))
```


## continuous factors
```{r models for continuous factors}

mod_6con <- lm(log10(y)~x*DEATH_EVENT, data = need_con_data)
grid_6con <- need_con_data%>%data_grid(x,DEATH_EVENT)%>%gather_predictions(mod_6con)
ggplot(grid_6con, aes(x=x, y= pred, col=DEATH_EVENT))+geom_point()+
  geom_text_repel(aes(x=x, y= pred, label=round(pred,2)))+
  labs(title="The comparison of the most effective continous factors", x="factors", y="prediction", col="Health situation")+
  scale_color_discrete(labels=c("Survive","Dead"))+coord_flip()
```

## categorical factors
```{r models for categorical factors}
mod_4cate <- lm(y~x*DEATH_EVENT, data = need_cate_data)
grid_4cate <- need_cate_data%>%data_grid(x,DEATH_EVENT)%>%gather_predictions(mod_4cate)
ggplot(grid_4cate, aes(x=x, y= pred, col=DEATH_EVENT))+geom_point()+
  geom_text_repel(aes(x=x, y= pred, label=round(pred,2)))+
  labs(title="The comparison of the most effective category factors", x="factors", y="prediction", col="Health situation")+
  scale_color_discrete(labels=c("Survive","Dead"))+coord_flip()
```
As we have discovered that when all the factors are influencing on the prediction of the death event, the factors: high blood pressure, anaemia, serum creatinine and ejection fraction are the top four.
As the factors anaemia and high blood pressure are just categorical variables, so we just treat them as conditions.

## merge the categorical and continuous variables
```{r}
grid_all <- merge(grid_4cate, grid_6con, all=TRUE)
ggplot(grid_all, aes(x=x, y= pred, col=DEATH_EVENT))+geom_point()+
  labs(title="The comparison of the most effective factors", x="factors", y="prediction", col="Health situation")+
  scale_color_discrete(labels=c("Survive","Dead"))+coord_flip()
```


# focuse on the 4 factors only
## try to use the serumn creatinine and ejection fraction level explicitly to predict the death event.
From the result, we can find that even the four factors are all very sensitive to death event, but in the serum creatinine case, we cannot see the effect of the top 2 categorical variables on the level of serum cratinine in terms of death event.
```{r group for prediction}
#group
x_grs <- heart%>%
  group_by(anaemia,high_blood_pressure)%>%
  summarise(n())
x_gr <- heart%>%
  group_by(anaemia,high_blood_pressure)%>%
  nest()
ggplot(data=x_gr[[3]][[1]],mapping = aes(serum_creatinine))+geom_density(aes(color=DEATH_EVENT))+scale_x_continuous(limits = c(0,9))+scale_color_discrete(labels=c("survive","dead"))+xlab("Level of serum creatinine in the blood (mg/dL)")+labs(title="Anaemia:0;  High_blood_pressure:1; ")
ggplot(data=x_gr[[3]][[2]],mapping = aes(serum_creatinine))+geom_density(aes(color=DEATH_EVENT))+scale_x_continuous(limits = c(0,9))+scale_color_discrete(labels=c("survive","dead"))+xlab("Level of serum creatinine in the blood (mg/dL)")+labs(title="Anaemia:0;  High_blood_pressure:0; ")
ggplot(data=x_gr[[3]][[3]],mapping = aes(serum_creatinine))+geom_density(aes(color=DEATH_EVENT))+scale_x_continuous(limits = c(0,9))+scale_color_discrete(labels=c("survive","dead"))+xlab("Level of serum creatinine in the blood (mg/dL)")+labs(title="Anaemia:1;  High_blood_pressure:0; ")
ggplot(data=x_gr[[3]][[4]],mapping = aes(serum_creatinine))+geom_density(aes(color=DEATH_EVENT))+scale_x_continuous(limits = c(0,9))+scale_color_discrete(labels=c("survive","dead"))+xlab("Level of serum creatinine in the blood (mg/dL)")+labs(title="Anaemia:1;  High_blood_pressure:1; ")

ggplot(data=x_gr[[3]][[1]],mapping = aes(ejection_fraction))+geom_density(aes(color=DEATH_EVENT))+scale_x_continuous(limits = c(0,80))+scale_color_discrete(labels=c("survive","dead"))+xlab("Level of ejection_fractione in the blood (mg/dL)")+labs(title="Anaemia:0;  High_blood_pressure:1; ")
ggplot(data=x_gr[[3]][[2]],mapping = aes(ejection_fraction))+geom_density(aes(color=DEATH_EVENT))+scale_x_continuous(limits = c(0,80))+scale_color_discrete(labels=c("survive","dead"))+xlab("Level of ejection_fraction in the blood (mg/dL)")+labs(title="Anaemia:0;  High_blood_pressure:0; ")
ggplot(data=x_gr[[3]][[3]],mapping = aes(ejection_fraction))+geom_density(aes(color=DEATH_EVENT))+scale_x_continuous(limits = c(0,80))+scale_color_discrete(labels=c("survive","dead"))+xlab("Level of ejection_fraction in the blood (mg/dL)")+labs(title="Anaemia:1;  High_blood_pressure:0; ")
ggplot(data=x_gr[[3]][[4]],mapping = aes(ejection_fraction))+geom_density(aes(color=DEATH_EVENT))+scale_x_continuous(limits = c(0,80))+scale_color_discrete(labels=c("survive","dead"))+xlab("Level of ejection_fraction in the blood (mg/dL)")+labs(title="Anaemia:1;  High_blood_pressure:1; ")

```

## try to model the relationship between serum creatinine and the ejection fraction in terms of death event
```{r modelling}
ggplot(data=x_gr[[3]][[1]],mapping = aes(ejection_fraction,serum_creatinine))+geom_jitter()+geom_smooth(aes(color=DEATH_EVENT),se=FALSE)+scale_color_discrete(labels=c("survive","dead"))+xlab("ejection fraction(%)")+labs(title="Anaemia:0;  High_blood_pressure:1; ")
mod_1 <- lm(serum_creatinine~ejection_fraction+DEATH_EVENT,data=x_gr[[3]][[1]])
mod_2 <- lm(serum_creatinine~ejection_fraction*DEATH_EVENT,data=x_gr[[3]][[1]])
grid <- x_gr[[3]][[1]] %>%
  data_grid(ejection_fraction,DEATH_EVENT) %>%
  gather_predictions(mod_1,mod_2)
ggplot(x_gr[[3]][[1]],aes(ejection_fraction,serum_creatinine,color=DEATH_EVENT))+geom_jitter()+geom_line(data=grid,aes(y=pred))+facet_wrap(~model)+scale_color_discrete(labels=c("survive","dead"))+ylab("Level of serum creatinine in the blood (mg/dL)")+xlab("ejection fraction(%)")+labs(title="Anaemia:0;  High_blood_pressure:1; ")
x_gr[[3]][[1]]<- x_gr[[3]][[1]]%>%
  gather_residuals(mod_1,mod_2)
ggplot(x_gr[[3]][[1]],aes(ejection_fraction,resid,color=DEATH_EVENT))+geom_jitter()+facet_grid(model~DEATH_EVENT)+geom_smooth()+scale_color_discrete(labels=c("survive","dead"))+xlab("ejection fraction(%)")+labs(title="Anaemia:0;  High_blood_pressure:1; ")

ggplot(data=x_gr[[3]][[2]],mapping = aes(ejection_fraction,serum_creatinine))+geom_jitter()+geom_smooth(aes(color=DEATH_EVENT),se=FALSE)+scale_color_discrete(labels=c("survive","dead"))+xlab("ejection fraction(%)")+labs(title="Anaemia:0;  High_blood_pressure:0; ")
mod_1 <- lm(serum_creatinine~ejection_fraction+DEATH_EVENT,data=x_gr[[3]][[2]])
mod_2 <- lm(serum_creatinine~ejection_fraction*DEATH_EVENT,data=x_gr[[3]][[2]])
grid <- x_gr[[3]][[2]] %>%
  data_grid(ejection_fraction,DEATH_EVENT) %>%
  gather_predictions(mod_1,mod_2)
ggplot(x_gr[[3]][[2]],aes(ejection_fraction,serum_creatinine,color=DEATH_EVENT))+geom_jitter()+geom_line(data=grid,aes(y=pred))+facet_wrap(~model)+scale_color_discrete(labels=c("survive","dead"))+ylab("Level of serum creatinine in the blood (mg/dL)")+xlab("ejection fraction(%)")+labs(title="Anaemia:0;  High_blood_pressure:0; ")
x_gr[[3]][[2]]<- x_gr[[3]][[2]]%>%
  gather_residuals(mod_1,mod_2)
ggplot(x_gr[[3]][[2]],aes(ejection_fraction,resid,color=DEATH_EVENT))+geom_jitter()+facet_grid(model~DEATH_EVENT)+geom_smooth()+scale_color_discrete(labels=c("survive","dead"))+xlab("ejection fraction(%)")+labs(title="Anaemia:0;  High_blood_pressure:0; ")

ggplot(data=x_gr[[3]][[3]],mapping = aes(ejection_fraction,serum_creatinine))+geom_jitter()+geom_smooth(aes(color=DEATH_EVENT),se=FALSE)+scale_color_discrete(labels=c("survive","dead"))+xlab("ejection fraction(%)")+labs(title="Anaemia:1;  High_blood_pressure:0; ")
mod_1 <- lm(serum_creatinine~ejection_fraction+DEATH_EVENT,data=x_gr[[3]][[3]])
mod_2 <- lm(serum_creatinine~ejection_fraction*DEATH_EVENT,data=x_gr[[3]][[3]])
grid <- x_gr[[3]][[3]] %>%
  data_grid(ejection_fraction,DEATH_EVENT) %>%
  gather_predictions(mod_1,mod_2)
ggplot(x_gr[[3]][[3]],aes(ejection_fraction,serum_creatinine,color=DEATH_EVENT))+geom_jitter()+geom_line(data=grid,aes(y=pred))+facet_wrap(~model)+scale_color_discrete(labels=c("survive","dead"))+ylab("Level of serum creatinine in the blood (mg/dL)")+xlab("ejection fraction(%)")+labs(title="Anaemia:1;  High_blood_pressure:0; ")
x_gr[[3]][[3]]<- x_gr[[3]][[3]]%>%
  gather_residuals(mod_1,mod_2)
ggplot(x_gr[[3]][[3]],aes(ejection_fraction,resid,color=DEATH_EVENT))+geom_jitter()+facet_grid(model~DEATH_EVENT)+geom_smooth()+scale_color_discrete(labels=c("survive","dead"))+xlab("ejection fraction(%)")+labs(title="Anaemia:1;  High_blood_pressure:0; ")

ggplot(data=x_gr[[3]][[4]],mapping = aes(ejection_fraction,serum_creatinine))+geom_jitter()+geom_smooth(aes(color=DEATH_EVENT),se=FALSE)+scale_color_discrete(labels=c("survive","dead"))+xlab("ejection fraction(%)")+labs(title="Anaemia:1;  High_blood_pressure:1; ")
mod_1 <- lm(serum_creatinine~ejection_fraction+DEATH_EVENT,data=x_gr[[3]][[4]])
mod_2 <- lm(serum_creatinine~ejection_fraction*DEATH_EVENT,data=x_gr[[3]][[4]])
grid <- x_gr[[3]][[4]] %>%
  data_grid(ejection_fraction,DEATH_EVENT) %>%
  gather_predictions(mod_1,mod_2)
ggplot(x_gr[[3]][[4]],aes(ejection_fraction,serum_creatinine,color=DEATH_EVENT))+geom_jitter()+geom_line(data=grid,aes(y=pred))+facet_wrap(~model)+scale_color_discrete(labels=c("survive","dead"))+ylab("Level of serum creatinine in the blood (mg/dL)")+xlab("ejection fraction(%)")+labs(title="Anaemia:1;  High_blood_pressure:1; ")
x_gr[[3]][[4]]<- x_gr[[3]][[4]]%>%
  gather_residuals(mod_1,mod_2)
ggplot(x_gr[[3]][[4]],aes(ejection_fraction,resid,color=DEATH_EVENT))+geom_jitter()+facet_grid(model~DEATH_EVENT)+geom_smooth()+scale_color_discrete(labels=c("survive","dead"))+xlab("ejection fraction(%)")+labs(title="Anaemia:1;  High_blood_pressure:1; ")


```

## Actually we only need to use the model lm(y ~ x1 * x2 ).
```{r}
x_gr <- heart%>%
  group_by(anaemia,high_blood_pressure)%>%
  nest()

mod_2 <- lm(serum_creatinine~ejection_fraction*DEATH_EVENT,data=x_gr[[3]][[1]])
grid <- x_gr[[3]][[1]] %>%
  data_grid(ejection_fraction,DEATH_EVENT) %>%
  add_predictions(mod_2)
ggplot(x_gr[[3]][[1]],aes(ejection_fraction,serum_creatinine,color=DEATH_EVENT))+geom_jitter()+geom_line(data=grid,aes(y=pred))+scale_color_discrete(labels=c("survive","dead"))+ylab("Level of serum creatinine in the blood (mg/dL)")+xlab("ejection fraction(%)")+labs(title="Anaemia:0;  High_blood_pressure:1; ")

mod_2 <- lm(serum_creatinine~ejection_fraction*DEATH_EVENT,data=x_gr[[3]][[2]])
grid <- x_gr[[3]][[2]] %>%
  data_grid(ejection_fraction,DEATH_EVENT) %>%
  add_predictions(mod_2)
ggplot(x_gr[[3]][[2]],aes(ejection_fraction,serum_creatinine,color=DEATH_EVENT))+geom_jitter()+geom_line(data=grid,aes(y=pred))+scale_color_discrete(labels=c("survive","dead"))+ylab("Level of serum creatinine in the blood (mg/dL)")+xlab("ejection fraction(%)")+labs(title="Anaemia:0;  High_blood_pressure:0; ")

mod_2 <- lm(serum_creatinine~ejection_fraction*DEATH_EVENT,data=x_gr[[3]][[3]])
grid <- x_gr[[3]][[3]] %>%
  data_grid(ejection_fraction,DEATH_EVENT) %>%
  add_predictions(mod_2)
ggplot(x_gr[[3]][[3]],aes(ejection_fraction,serum_creatinine,color=DEATH_EVENT))+geom_jitter()+geom_line(data=grid,aes(y=pred))+scale_color_discrete(labels=c("survive","dead"))+ylab("Level of serum creatinine in the blood (mg/dL)")+xlab("ejection fraction(%)")+labs(title="Anaemia:1;  High_blood_pressure:0; ")

mod_2 <- lm(serum_creatinine~ejection_fraction*DEATH_EVENT,data=x_gr[[3]][[4]])
grid <- x_gr[[3]][[4]] %>%
  data_grid(ejection_fraction,DEATH_EVENT) %>%
  add_predictions(mod_2)
ggplot(x_gr[[3]][[4]],aes(ejection_fraction,serum_creatinine,color=DEATH_EVENT))+geom_jitter()+geom_line(data=grid,aes(y=pred))+scale_color_discrete(labels=c("survive","dead"))+ylab("Level of serum creatinine in the blood (mg/dL)")+xlab("ejection fraction(%)")+labs(title="Anaemia:1;  High_blood_pressure:1; ")


```

