---
title: "Species behaviour analysis with mixed models"
author: "Pablo Bolanos"
date: '2022-11-09'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Statistical methods

To explain differences in the acoustic activity by year, a linear mixed effect analysis was built with the environmental variables as fixed effects, namely: average wind direction (°), average cloudiness (oktas), average rain (mm), average wind velocity (km/h), average relative humidity (%) and average temperature (°c) as fixed effects (without interaction term). Number of detections by hour was the response variable, and time of day and site were included as random effects. Number of detections were not log-transformed, because it is not recommended on count data (O’Hara & Kotze, 2010; Brooks et al, 2017). Mixed model analysis was performed with the R package glmmTMB (Magnusson et al, 2018) adapted to analyse count data, the package works increasing the range of models that can be fitted using maximum likelihood estimation (Brooks et al, 2017). Due to an over dispersion of data, a negative binomial distribution model was then conducted. Interaction between variables was tested by determination of Variance Inflation Factors (VIF, Field, 2009) for a standard linear model excluding effects and interactions, this was performed with the R package car (Fox, 2018). It revealed a VIF of 3 or less for all the environmental variables, values greater than 3 can lead to biased results in the regression (Zuur et al, 2007), thus all variables were kept in the analysis. Critical probabilities (p) values were obtained by likelihood ratio tests of the full model with the effects in question against the model without one of the effects at a t ime.

```{r include=FALSE}

datahi3 <- read.table("datah_insiv_ter_pca_am3_byday.txt", sep = "\t", h=TRUE)

# take out the column of time, because we are working just with one row by day

datahi3 <- datahi3[,-8] 

```

**The data:**
The data set consists of quantity of vocalizations of a bird species (*Pharomachrus mocinno*) detected by day. This was complemented with weather information, as is shown in this extract:    

```{r echo = FALSE}
head(datahi3)
```

```{r echo = FALSE}

knitr::kable(head, caption = "Data structure")

```

**Data preparation**
Some columns contain characters, so it is necessary to convert them to numeric

```{r include=TRUE}

datahi3$cloud.med <- as.numeric(datahi3$cloud.med)
datahi3$dir.wind.deg <- as.numeric(datahi3$dir.wind.deg)
datahi3$date <- as.factor(datahi3$date)

```

**Constructing and testing the different models**
In order to compare which model is better, depending on the fixed and random effects selected, first different variants were created.

```{r include=TRUE, message=FALSE, warning=FALSE}
library(glmmTMB)

detect.model.random = glmmTMB(detection ~ year +
                                dir.wind.deg + cloud.med,
                              data=datahi3, family = poisson)

detect.model.random1 = glmmTMB(detection ~ year +
                                dir.wind.deg + cloud.med + (1|site),
                              data=datahi3, family = poisson)

detect.model.random2 = glmmTMB(detection ~ year +
                                dir.wind.deg + cloud.med + (1|date),
                              data=datahi3, family = poisson)

detect.model.random3 = glmmTMB(detection ~ year +
                                 dir.wind.deg + cloud.med + (1|site) + (1|date),
                               data=datahi3, family = poisson)

```

**Then the models can be compared using AIC criteria**

```{r include=TRUE, message=FALSE, warning=FALSE}
library(bbmle) 

AICtab(detect.model.random,detect.model.random1,detect.model.random2,detect.model.random3) 

detect.model.1 = glmmTMB(detection ~ year +
                           dir.wind.deg + cloud.med + (1|site)+(1|date),
                         data=datahi3, family = poisson)                  

summary(detect.model.1)

```

best model is detect.model.random3, we have to keep the random effect since they allow a better explanation of the data

**Due to an overdispersion of the data, a negative binomial distribution model is then conducted.** 

```{r include=TRUE, message=FALSE, warning=FALSE}

# lineary link between mean and variance
detect.model.NB1 = glmmTMB(detection ~ year +
                             dir.wind.deg + cloud.med + (1|site) + (1|date),
                           data=datahi3, family = nbinom1)

# quadratic link between mean and variance  
detect.model.NB2 = glmmTMB(detection ~ year +
                             dir.wind.deg + cloud.med + (1|site) + (1|date),
                           data=datahi3, family = nbinom2) 

```

**Then the models can be compared using AIC criteria**


```{r include=TRUE, message=FALSE, warning=FALSE}

AICtab(detect.model.1,detect.model.NB1,detect.model.NB2)

summary(detect.model.NB2)

```
the best mothel is detect.model.NB2 (quadratic link)

**Finally collinarity between variables was tested, because it can cause bias in the analysis, it can complicate or prevent the identification of an optimal set of explanatory variables for a statistical model.** 


```{r include=TRUE, message=FALSE, warning=FALSE}
library(DHARMa)

## checking for glmm hypothesis
mod_vif<-lm(detection~year +
              dir.wind.deg + cloud.med,data=datahi3)
library(car)
vif(mod_vif) # threshold < 2, then no multicollinearity otherwise there is residuals structure
residuals_mod=simulateResiduals(fittedModel = detect.model.NB2)

```


```{r echo=FALSE, message=FALSE, warning=FALSE}

plot(residuals_mod,rank=T) 

```

![alt text](https://github.com/PabloRBS/mixed_model_vocal_activity/blob/main/dharma_diagnostics.png?raw=true)
