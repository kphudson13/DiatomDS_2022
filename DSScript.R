library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra) #puts the plots together nice
library(knitr)
library(broom)
library(ggeffects)

#call in the data
continuousdata <- read.csv("CCdata.csv",
                      header = TRUE,
                      stringsAsFactors = TRUE)

##### Clean the data #####

continuousdata <- na_if(continuousdata, "na")
continuousdata <- na_if(continuousdata, "#DIV/0!")

#first make all the values characters so they can be make numeric
continuousdata[ ,13:14]<- 
  sapply(continuousdata[ ,13:14], as.character)

#make all the values numeric so they can be used in geom_smooth functions
continuousdata[ ,13:14]<- 
  sapply(continuousdata[ ,13:14], as.numeric)

#cut out the blank for some plots
trimmeddata <- filter(continuousdata,
                      Density != "")

##### Randomize the orders and export as a csv ######

#first make an order for filling them
set.seed(727)

FillOrder <- c(1:84)
FillOrder <- FillOrder[sample(FillOrder)]

#create a random order to put the flasks in the incubator
GrowthOrder <- c(1:84)
GrowthOrder <- GrowthOrder[sample(GrowthOrder)]

RandomOrders <- data.frame(FillOrder)
RandomOrders$GrowthOrder <- GrowthOrder

#output the growth list as a csv
write.csv(RandomOrders, 
          "RandomOrder.csv",
          row.names = FALSE)

##### Linear models #####

#create a linear model with glass = nutrients  
LinearModel <- lm(formula = Ind.Density ~ 
                    Nutrient*Glass, 
                 data = continuousdata %>% 
                   filter(Nutrient < 250, Glass < 250))

#stats summary of that model 
SummBaseM <- summary(LinearModel)

SummBaseM

LogModelInt <- lm(formula = log(Ind.Density) ~ 
                 Nutrient*Glass, 
               data = (continuousdata %>% 
                         filter(Nutrient < 250, Glass < 250)))

LogModel <- lm(formula = log(Ind.Density) ~ 
             Nutrient + Glass, 
           data = (continuousdata %>% 
                     filter(Nutrient < 250, Glass < 250)))

SummLogM <- summary(LogModel)

SummLogM

#start the table
Log.output <- coefficients(SummLogM)

#add the confidence intervals 
Log.output <- cbind(Log.output,
                    confint(LogModel))

#clean it up
Log.output <- round(Log.output, digits = 4)
Log.output <- as.data.frame(Log.output)
Log.output[1,4] <- "<0.0001"

#change the table names
names(Log.output) <- c("Estimate",
                       "Std. Error",
                       "T value", 
                       "P value", 
                       "Lower 95 CI", 
                       "Upper 95 CI")

#making a theme for the table output 
tt1 <- ttheme_default(core=list(fg_params=list(hjust=0, x=0.1)),
                      rowhead=list(fg_params=list(hjust=0, x=0, 
                                                  fontface = "bold"),
                                   bg_params=list(fill="grey80")))

#export the summary stats
png("SummaryStats.png", 
    height = 100*nrow(Log.output), 
    width = 350*ncol(Log.output),
    res = 288)
grid.table(Log.output, theme = tt1)
dev.off()


##### Validation #####

#qqplots first 

qq.plot1 <- (plot(LinearModel, which = 2))

qq.plot1

qq.plotlog <- (plot(LogModel, which = 2))

qq.plotlog

#validate the assumptions of the model 
#this should be normally distributed about 0 
Residualsplot1 <- ggplot() +
  geom_jitter(aes(x = fitted(LinearModel), 
                  y = residuals(LinearModel)),
              width = 0.1,
              height = 0,
              alpha = 0.5) +
  theme_classic() +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Fitted Values",
       y = "Residuals")

#just view it
Residualsplot1

ResidualsplotLog <- ggplot() +
  geom_jitter(aes(x = fitted(LogModel), 
                  y = residuals(LogModel)),
              width = 0.1,
              height = 0,
              alpha = 0.5) +
  theme_classic() +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Log Fitted Values",
       y = "Residuals")

#just view it
ResidualsplotLog

Residual.plots <- grid.arrange(Residualsplot1,
                               ResidualsplotLog,
                               ncol = 2)

Residual.plots <- arrangeGrob(Residualsplot1,
                              ResidualsplotLog,
                              ncol = 2)


##### ANOVA #####

#this anova compares the two models 
mod.aov <- print(anova(LogModel, LogModelInt))


##### Trend graphs ######


#make axis labels 
labelAY <- expression(Density~(Cells~x~10^7~"/"~ml))
labelAX <- expression(Nutrient~Concentration~("%"~of~Standard))

#calculate fitted values and stuff for nutrient as predictor 
Nut.predicted <- ggpredict(LogModel, "Nutrient")
Nut.predicted <- data.frame(Nut.predicted)

#plot nutrient effect
NutrientplotAll <- ggplot(na.omit(continuousdata %>% 
                                   filter(Nutrient < 250, 
                                          Glass < 250)))+
  geom_point(aes(x = Nutrient, y = Ind.Density),
             colour = "darkgreen")+
  geom_line(data = Nut.predicted, 
            aes(x = x, y = predicted))+
  geom_ribbon(data = Nut.predicted, 
              aes(ymin = conf.low, 
                  ymax = conf.high, 
                  x = x),
              fill = "olivedrab1",
              alpha = 0.4) +
  theme_minimal() +
  theme(axis.line = element_line(colour="black"),
        legend.position = " ") +
  scale_x_continuous(breaks = seq(70,220, by=30))+
  ylab(labelAY)+
  xlab(labelAX)

NutrientplotAll

#make axis labels 
labelBX <- expression(Glass~Concentration~("%"~of~Standard))

#calculate fitted values and stuff for glass as predictor 
Gla.predicted <- ggpredict(LogModel, "Glass")
Gla.predicted <- data.frame(Gla.predicted)

#plot glass effect 
GlassplotAll <- ggplot(na.omit(continuousdata %>% 
                                 filter(Nutrient < 250, Glass < 250))) +
  geom_point(aes(x = Glass, y = Ind.Density),
             colour = "darkgreen")+
  geom_line(data = Gla.predicted,
            aes(x = x, y = predicted)) +
  geom_ribbon(data = Gla.predicted, 
              aes(ymin = conf.low, 
                  ymax = conf.high, 
                  x = x),
              fill = "olivedrab1",
              alpha = 0.4) +
  theme_minimal() +
  theme(axis.line = element_line(colour="black"),
        legend.position = " ")+
  scale_x_continuous(breaks = seq(70,220, by=30))+
  ylab(labelAY)+
  xlab(labelBX)

#just view it
GlassplotAll

plots <- grid.arrange(NutrientplotAll, 
             GlassplotAll,
             ncol = 2)


##### Build a heat map for the density plot ######

#make a string of names based off concentration
ConcNames <- (unique(continuousdata$Glass)) 

LabelC <- expression(Cells~x~10^7~"/"~ml)

#make it a factor so it isnt spaced out
factordata <- trimmeddata

factordata[ ,3:4] <- 
  sapply(factordata[ ,3:4], as.factor)

#graph that bad boy 
ggplot (data = factordata, 
        aes(x = Glass, y = Nutrient, fill = Density)) +
  geom_tile() +
  geom_text(aes(label = Density)) +
  scale_fill_gradient(low="khaki1", 
                      high="darkgreen",
                      na.value = "grey80") +
  scale_x_discrete(name = labelBX,
                   limits = paste(ConcNames),
                   expand = c(0, 0)) +
  scale_y_discrete(name = labelAX,
                   limits = paste(ConcNames),
                   expand = c(0, 0)) +
  theme(panel.border = element_rect(colour="black", 
                                    fill = NA,
                                    linewidth = 1),
        panel.grid = element_line(colour = NA)) +
  labs(fill = LabelC)