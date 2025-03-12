rm(list = ls())

library(mgcv)
library(ggplot2)
library(doBy)
library(dplyr)
library(tidyverse)
library(patchwork) # to plot several plots as panels

source("~/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/leaf_production_MZ.R")
head(dLAIlitter)


LAI <- dLAIlitter[,c(1,5,19)]

dLAIdt <- dLAIlitter[,c(1,18,19)]
dLAIdt$data <- "dLAI/dt"
names(dLAIdt)[2] <- "value"
dlit <- dLAIlitter[,c(1,17,19)]
dlit$data <- "dlit/dt"
names(dlit)[2] <- "value"
laprod <- dLAIlitter[,c(1,16,19)]
laprod$data <- "LeafProd"
names(laprod)[2] <- "value"

leafprod_df <- rbind(dLAIdt, dlit, laprod)
leafprod_df$year <- year(leafprod_df$Date)
head(leafprod_df)



g1 <- ggplot(data=leafprod_df[leafprod_df$data == "LeafProd",])+
  geom_boxplot(aes(x=as.factor(month), y=value), fill = "green4")+
  theme_bw()+
  labs(x="month", y="Leaf Prod. m2/m2/month")

g2 <- ggplot()+
  geom_line(data=dLAIdt, aes(x=Date, y=value),  color = "green")+
  geom_line(data=dlit,    aes(x=Date, y=value),  color = "grey")+
  theme_bw()+
  labs(x="Date", y="value. m2/m2/month")

# g3 <- ggplot(data=dLAIlitter)+
#   geom_boxplot(aes(x=as.factor(month), y=lit), fill="brown")+
#   theme_bw()+
#   labs(x="month", y="Litter m2/m2/month")

g3 <- ggplot(data=leafprod_df[leafprod_df$data != "LeafProd", ])+
  geom_boxplot(aes(x=as.factor(month), y=value, fill=data))+
  theme_bw()+
  geom_hline(yintercept=0, linetype="dotted", color="gray4", size=1)+
  labs(x="month", y="value, m2/m2")+ 
  scale_fill_manual(values = c("green","brown"))

g4 <- ggplot(data=LAI)+
  geom_boxplot(aes(x=as.factor(month), y=LAI), fill="green")+
  theme_bw()+ylim(0.3, 2.7)+
  labs(x="month", y="LAI m2/m2")

g2 +g1 + g3+ g4

ggplot(data=leafprod_df)+
  geom_boxplot(aes(x=as.factor(month), y=value, fill=data))+
  theme_bw()+
  geom_hline(yintercept=0, linetype="dotted", color="gray4", size=1)+
  labs(x="month", y="value, m2/m2")+
  facet_wrap(.~data)+ 
  scale_fill_manual(values = c("green","brown","green4"))

ggplot(data=leafprod_df[leafprod_df$data != "dLAI/dt", ])+
  geom_boxplot(aes(x=as.factor(month), y=value, fill=data))+
  theme_bw()+
  labs(x="month", y="value, m2/m2")+
  scale_fill_manual(values = c("brown","green4"))+
  geom_hline(yintercept=0, linetype="dotted", color="gray4", size=1)


  