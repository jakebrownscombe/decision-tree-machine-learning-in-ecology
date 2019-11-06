# Introduction to machine learning with ecological (acoustic telemetry) data

# Jake Brownscombe, PhD
# Sept 2019


# This exercise is meant to provide ecologists with a basic introduction to applying a few different
# decision tree algorithms to ecological data. Under the broader umbrella of machine learning, there
# are a TON of different options for analytical techniques that are super useful for exploring the 
# structure and relationships in [ecological] data. See:
# https://cran.r-project.org/web/views/MachineLearning.html
# for examples of different options that can be implemented in R. Within these examples there are a heap
# of different approaches, see: https://www.r-bloggers.com/a-brief-tour-of-the-trees-and-forests/

# and info on the approach from Leo Breiman:
# https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm

# and a great text book on Statistical Learning:
# Elements of Statistical Learning. 2009. New York. Springer. xxii, 745 p. : ill. ; 24 cm.

# We will explore an acoustic telemetry dataset from a permit spawning site in the Florida Keys, looking
# at the influence of various ecological factors on permit presence/absence

#install some packages we'll need (if you don't have them already):
install.packages('ggplot2')
install.packages('dplyr')
install.packages('tree')
install.packages('rpart')
install.packages('caret')
install.packages('randomForest')
install.packages('rfPermute')
install.packages('ggmap')
install.packages('rpart') 
install.packages('partykit')
install.packages('e1071')
install.packages('cforest')
install.packages('ranger')
install.packages('Boruta')
install.packages('Amelia')
install.packages('pdp')
install.packages('lunar')
install.packages('gbm')
install.packages('beepr')

#set working directory to where you have placed the data associated with this R script

#check out and prepare the data: ####
WDR <- readRDS("WDR_detections.rds") #best way to store R objects
head(WDR)
str(WDR)

library(dplyr)
WDRloc <- WDR %>% group_by(station) %>% summarise(logdets=log(length(Transmitter)), lat=mean(lat),lon=mean(lon)) %>% as.data.frame()
head(WDRloc)


load("~/Desktop/workshops/machine learning Dal 2019/data/Acoustic telemetry workshop workspace.RData")
library(ggmap)
ggmap(FLmap, extent='normal') + 
  geom_point(data=WDRloc, aes(x=lon, y=lat),col="yellow")+ 
  xlab("Longitude")+
  ylab("Latitude")+
  scale_x_continuous(limits=c(-82.3, -81.1))+
  scale_y_continuous(limits=c(24.2, 25.0))



# interested in what factors influence permit presence at this spawning site. 
# importantly, the dataset is presence-only right now. We have to generate a new dataset with all dates of interest:

WDRdates <- data.frame(day=seq(from=min(WDR$day), to=max(WDR$day), by="day"))
head(WDRdates)
str(WDRdates)

#summarise number of individuals present on each day:
WDRdatesum <- WDR %>% group_by(day) %>% summarise(IDcount=length(unique(FishID)))

#assign to WDRdates:
WDRdates$IDcount <- WDRdatesum$IDcount[match(WDRdates$day, WDRdatesum$day)]
#make NAs 0s:
WDRdates$IDcount[is.na(WDRdates$IDcount)]<-0

#generate simple presence/absence at the site:
WDRdates$pres <- ifelse(WDRdates$IDcount==0, 0, 1)
head(WDRdates)

ggplot(WDRdates, aes(x=day, y=IDcount))+geom_point()


#let's just look at 2017 data on:
WDR2 <- WDRdates %>% filter(day>="2017-01-01")
head(WDR2)

ggplot(WDR2, aes(x=day, y=IDcount))+geom_point()


#interested to know how month, temperature, lunar phase influence spawning activity 

#month:
WDR2$month <- strftime(WDR2$day, tz="EST5EDT", format="%B")

#lunar phase:
library(lunar)
WDR2$lunar <- lunar.phase(WDR2$day, shift = 0, name = 8)
table(WDR2$lunar)

#pull in temperature data:
temp <-read.csv("WDRtemp.csv")
str(temp)
temp$day <- as.Date(temp$day)

WDR2$temp <- temp$temp[match(WDR2$day, temp$day)]
head(WDR2)
anyNA(WDR2$temp)

#what data is missing?
missingtemp <- WDR2 %>% filter(is.na(temp))
missingtemp$temp <- 1
#20 days.. 
ggplot(temp, aes(x=day, y=temp))+geom_point()+geom_point(data=missingtemp, aes(x=day, y=temp),col="red")

#just most recent data. remove:

WDR3 <- WDR2 %>% filter(!is.na(temp))
head(WDR3)






# get a sense of the relationships between these factors and permit presence:

ggplot(data=WDR3, aes(x=day, y=IDcount))+geom_point()
  
#by date:
head(WDR3)
WDR3$moday <- strftime(WDR3$day, format="%m-%d")
WDRmoday <- WDR3 %>% group_by(moday) %>% summarise(IDcount=sum(IDcount))
WDRmoday$moday <-as.Date(WDRmoday$moday, format="%m-%d")
ggplot(WDRmoday, aes(x=moday, y=IDcount))+geom_histogram(stat="identity")+scale_x_date(breaks='month')

#month, lunar:
WDR3$month <- factor(WDR3$month, levels=c("January","February","March","April","May","June","July","August","September","October","November","December"))
WDR3$lunar <- factor(WDR3$lunar, levels=c("New","Waxing crescent","First quarter","Waxing gibbous","Full","Waning gibbous",
                                          "Last quarter", "Waning crescent"))
ggplot(WDR3, aes(x=month, y=IDcount, col=lunar))+geom_boxplot()
#definite month effect. Some tendency for full moons. look closer:

ggplot(WDR3, aes(x=lunar, y=IDcount))+geom_boxplot()

ggplot(WDR3, aes(x=lunar, y=IDcount))+ stat_summary(fun.y=mean, geom="point")+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",width=0.1)+coord_cartesian(ylim=c(0,2))

#temperature:
ggplot(WDR3, aes(x=day, y=temp, col=temp))+geom_point()
ggplot(WDR3, aes(x=day, y=IDcount, col=temp))+geom_point()

WDR3pres <- WDR3 %>% filter(IDcount>0)
ggplot(WDR3, aes(x=temp))+geom_histogram(col="black")+
  geom_histogram(data=WDR3pres, aes(x=temp),fill="red",alpha=0.7)



#all combined
ggplot(data=WDR3, aes(x=day, y=5, fill=lunar))+geom_tile(alpha=0.5, height=10)+
  scale_fill_manual(values=c("grey70", "white", "white", "white", "black", "white","white", "white"))+
  geom_point(data=WDR3, aes(x=day, y=IDcount,col=temp))+
  theme_classic()+ylab("probability of presence")+
  scale_x_date(breaks="month")+theme(axis.text.x = element_text(angle = 45, hjust = 1))


# What are the best predictors of permit spawning? Or at least, occupancy of a known spawning site  ####
# We will explore this question using various decision tree models:


# CART ####
# we'll start with a simple CART model.
# pros - simple and interpretable. Can make reasonably good predictions
# cons - prone to overfitting and tuning is subjective. 
#      - more complicated models make much better predictions

#two different packages that can fit these models:
library(tree)
library(rpart)
?tree
?rpart


#let's play with tree:
str(WDR3)
#make presence a factor:
WDR3$presf <- as.factor(WDR3$pres)
WDR3$month <- as.factor(as.character(WDR3$month))

z <- formula(presf~month+temp+lunar)
tree <- tree(formula=z, data = WDR3)

summary(tree)
print(tree)
plot(tree)
text(tree)

# these trees have a tendency to overfit the data with excessive numbers of splits. 
# to avoid this it's best to 'prune' the tree based on the amount of variance explained.

my.tree.seq = prune.tree(tree) # figure out level of pruning vs loss rate
plot(my.tree.seq)


# not really a clear elbow point here to select from. try 5

tree2 <- prune.tree(tree,best=5)

plot(tree2)
text(tree2)
summary(tree2)




#rpart is usually better:

# grow tree 
rpart <- rpart(formula=z, method="class", control=(minsplit=10), cp=0.0001, data=WDR3)

printcp(rpart) # display the results 
plotcp(rpart) # visualize cross-validation results 
summary(rpart) # detailed summary of splits


# plot tree 
plot(rpart, uniform=TRUE, 
     main="Classification Tree for WDR Presence")
text(rpart, use.n=TRUE, all=TRUE, cex=.8)

#  Greater numbers of splits improves predictive performance to a point, but increased complexity 
# increases the chances of model overfitting, which reduces predictive performance outside of the dataset
# (we'll get to that soon) and also makes the model hard to interpret

# general rule for pruning is to select the lowest nsplit where rel_error+xstd < xerror
printcp(rpart) #only 2!
prpart<- prune(rpart, cp=0.0180723)

# plot the pruned tree 
plot(prpart, uniform=TRUE, 
     main="Pruned Classification Tree for RAMP")
text(prpart, use.n=TRUE, all=TRUE, cex=.8)
summary(prpart)

monthsalpha <- data.frame(month=c("January","February","March","April",
                                  "May","June","July","August","September","October","November","December"))
monthsalpha$letter <- c('a','b','c','d','e','f','g','h','i','j','k','l')
monthsalpha




# CART models have a tendency to overfit data and the pruning process can be critisized as subjective.
# To overcome this issue, conditional inference trees fit the data based only on statistically
# significant splits.

library(partykit)
??partykit

ctree <- ctree(formula=z, data=WDR3)
plot(ctree)
ctree

#predict to examine accuracy 
pred <- predict(ctree, WDR3)

library(caret) #useful package for maling predictions with all types of these models
confusionMatrix(pred, WDR3$presf)


# how would this perform in non-training data? 

# split into training and out of bag prediction datasets:
head(WDR3)
train <- WDR3[sample(1:nrow(WDR3),nrow(WDR3)*0.7, replace=FALSE),]
test <- WDR3[!(WDR3$day %in% train$day),]

#fit tree to training data
trainctree <- ctree(formula=z, data=train)

#predict on to test data
pred <- predict(trainctree, test)
confusionMatrix(pred, test$presf)

# performing poorly on 1s. This happens often with these models, where classes with small sample size
# get neglected because of the way error is optimized with the greedy algorithm. Can set weights to fix this.

# A common approach to weighting is to make them inversely proportional to the class populations.

N <- train %>% group_by(pres) %>% summarise(count=length(pres))
N$w <- round((1/N$count)*1000,0)
N

train$w <- N$w[match(train$pres, N$pres)]
head(train)

?ctree
#fit tree to training data
trainctreeW <- ctree(formula=z, weights=w, data=train)

#predict into test data:
predW <- predict(trainctreeW, test)
confusionMatrix(predW, test$presf)











# Random Forests ####
# a major criticism of CART models is they are influenced heavily by even minor changes to the 
# data, can vary a lot even due to the random selection of the training set, and are prone to 
# overfitting (except maybe CIT). Random Forests provides solutions to these issues by
# using bootstrapping and random variable selection -  has the added
# benefit of being more robust to collinearity issues in the predictors. 

library(randomForest)
?randomForest
str(WDR3)

set.seed(123) # runs permutations so model output will change everytime unless you set.seed
z = formula(presf~month+temp+lunar)
rForest <- randomForest(formula=z, data = WDR3, replace=FALSE, na.action=na.omit,
                        importance=TRUE, do.trace=1000, ntree=1000)

print(rForest)
importance(rForest)
rForest$importanceSD

varImpPlot(rForest,type=1)
varImpPlot(rForest,type=2)

fpreds <- predict(rForest, WDR3, type="response")
confusionMatrix(fpreds, WDR3$presf)
#performs well on its own data, as to be expected. Better metrics in oob data come from print(rForest)

print(rForest)

#not super well balanced. 
N <- WDR3 %>% group_by(presf) %>% summarise(count=length(presf))
N$w <- (sum(N$count)/N$count)
N
WDR3$weights <- N$w[match(WDR3$presf, N$presf)]
head(WDR3)

rForestw <- randomForest(formula=z, data = WDR3, replace=FALSE, classwt=N$w,na.action=na.omit,
                        importance=TRUE, do.trace=1000, ntree=1000)
print(rForestw)
#weighted the 1s too heavily. try manually playing with these numbers:

rForestw <- randomForest(formula=z, data = WDR3, replace=FALSE, classwt=c(1,1.01),na.action=na.omit,
                         importance=TRUE, do.trace=1000, ntree=1000)
print(rForestw)
# in this case it's having a hard time balancing, but still better than without weights


# these 'black box' algorithms can be hard to interpret. Partial dependance plots are super useful to 
# explore and visualize the relationships between important predictors and the response, accounting
# for the impacts of other predictors in the model
library(pdp)
?pdp

rForest %>%
  partial(pred.var = "lunar", prob = TRUE, which.class=2) %>%
  plotPartial(rug = TRUE, train = WDR3)


partialMonth <- rForest %>% partial(pred.var = "month", prob = TRUE, which.class=2) 
partialMonth$month <- factor(partialMonth$month, levels=c("January","February","March","April","May","June","July","August","September","October","November","December"))
p1 <- ggplot(partialMonth, aes(x=month, y=yhat))+geom_boxplot()

partialTemp <- rForest %>% partial(pred.var = "temp", prob = TRUE, which.class=2) 
p2 <- ggplot(partialTemp, aes(x=temp, y=yhat))+geom_point()+geom_smooth()

partialLunar <- rForest %>% partial(pred.var = "lunar", prob = TRUE, which.class=2) 
p3 <- ggplot(partialLunar, aes(x=lunar, y=yhat))+geom_boxplot()



# multiplot function:
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


multiplot(p1, p2, p3)



# can also do 2 way interactions
partialMonthTemp <- partial(rForest, pred.var = c("month", "temp"), prob = TRUE, which.class=2)
plotPartial(partialMonthTemp)








#plot actual and predicted values:
head(WDR3)
WDR3$rforestpred <- fpreds
WDR3$rforestpred2 <- as.numeric(fpreds)-1

WDR3$month <- factor(WDR3$month, levels=c("January","February","March","April","May","June","July","August","September","October","November","December"))
ggplot(WDR3, aes(x=month, y=rforestpred2))+
  stat_summary(fun.y=mean, geom="point")+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",width=0.1)+
  stat_summary(data=WDR3, aes(x=month, y=pres),fun.y=mean, geom="point",col="red")+
  stat_summary(data=WDR3, aes(x=month, y=pres),fun.data = mean_cl_normal, geom = "errorbar",
               width=0.1, col="red")

WDR3$lunar <- factor(WDR3$lunar, levels=c("New","Waxing crescent","First quarter","Waxing gibbous",
                                          "Full","Waning gibbous","Last quarter","Waning crescent"))
ggplot(WDR3, aes(x=lunar, y=rforestpred2))+
  stat_summary(fun.y=mean, geom="point")+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",width=0.1)+
  stat_summary(data=WDR3, aes(x=lunar, y=pres),fun.y=mean, geom="point",col="red")+
  stat_summary(data=WDR3, aes(x=lunar, y=pres),fun.data = mean_cl_normal, geom = "errorbar",
               width=0.1, col="red")


ggplot(WDR3, aes(x=temp, y=rforestpred2))+geom_smooth()+
  geom_smooth(data=WDR3, aes(x=temp, y=pres), col="red")







# when will permit be there in 2020???

start <- as.Date("2020-01-01")
end <- as.Date("2020-12-31")

twenty20 <- data.frame(day=as.Date(seq(from=start, to=end, by="day")))
head(twenty20)

twenty20$month <- strftime(twenty20$day, format="%B")


#let's start with average temperatures from 2017-2019:

Avgtemp <- WDR3 %>% group_by(moday) %>%  summarise(temp=mean(temp))

twenty20$moday <- strftime(twenty20$day, format="%m-%d")
twenty20$temp <- Avgtemp$temp[match(twenty20$moday, Avgtemp$moday)]

#grab lunar phases:
twenty20$lunar <- lunar.phase(twenty20$day, shift = 0, name = 8)
head(twenty20)
str(twenty20)
twenty20$month <- as.factor(twenty20$month)

#predict probabilities:
preds2020 <- as.data.frame(predict(rForest, twenty20, type="prob"))
twenty20$rforestpred <- preds2020$'1'

ggplot(data=twenty20, aes(x=day, y=0.5, fill=lunar))+geom_raster(alpha=0.5)+
  scale_fill_manual(values=c("grey70", "white", "white", "white", "black", "white","white", "white"))+
      geom_point(data=twenty20, aes(x=day, y=rforestpred,col=temp))+
      theme_classic()+ylab("probability of presence")+
      scale_x_date(breaks="month")+theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
#only time will tell.. 

#what about a climate change scenario.. let's increase temperature by 2 deg C and increase variability

x <- rnorm(length(twenty20$day), mean=2, sd=0.5)
hist(x)

twenty20$tempCC <- twenty20$temp+x
hist(twenty20$temp)
hist(twenty20$tempCC)

head(twenty20)
twenty20CC <- twenty20 %>% dplyr::select(day, month, lunar, temp=tempCC)
head(twenty20CC)

preds20CC <- as.data.frame(predict(rForest, twenty20CC, type="prob"))
twenty20CC$rforestpred <- preds20CC$'1'


#plot both scenarios:


p1 <- ggplot(data=twenty20, aes(x=day, y=0.5, fill=lunar))+geom_raster(alpha=0.5)+
  scale_fill_manual(values=c("grey70", "white", "white", "white", "black", "white","white", "white"))+
  geom_point(data=twenty20, aes(x=day, y=rforestpred,col=temp))+
  theme_classic()+ylab("probability of presence")+
  scale_x_date(breaks="month")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+labs(title="2020")

p2 <- ggplot(data=twenty20CC, aes(x=day, y=0.5, fill=lunar))+geom_raster(alpha=0.5)+
  scale_fill_manual(values=c("grey70", "white", "white", "white", "black", "white","white", "white"))+
  geom_point(data=twenty20CC, aes(x=day, y=rforestpred,col=temp))+
  theme_classic()+ylab("probability of presence")+
  scale_x_date(breaks="month")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+labs(title="Climate change")

multiplot(p1, p2)





#interesting model predictions. However, while month is a valuable predictor for 
# applied purposes (influencing fisheries management), month isn't an ecologically signicant predictor
# and therefore potentially biases predictions, expecially when extrapolating. In the field of machine 
# learning and Data Science more generally this is called 'feature selection' 

#use degree days (summed temperature throughout the year) as a predictor:

head(WDR3)
str(WDR3)

WDR4 <- WDR3 %>% dplyr::select(day, IDcount, pres, presf, month, lunar, temp)
WDR4$degdays <- cumsum(WDR4$temp)
str(WDR4)

z1 = formula(presf~temp+degdays+month+lunar)
rForest2 <- randomForest(formula=z1, data = WDR4, replace=FALSE, na.action=na.omit,
                        importance=TRUE, do.trace=1000, ntree=1000)
print(rForest2)
randomForest::importance(rForest2)

varImpPlot(rForest2,type=1)
varImpPlot(rForest2,type=2)

fpreds2 <- predict(rForest2, WDR4, type="response")
confusionMatrix(fpreds2, WDR4$presf)





# See what it thinks about 2020. 

head(twenty20) #needs degdays
twenty20 <- twenty20 %>% filter(!is.na(temp))
twenty20$degdays <- cumsum(twenty20$temp)

preds2020DD <- as.data.frame(predict(rForest2, twenty20, type="prob"))
twenty20$rforestpredDD <- preds2020DD$'1'

ggplot(data=twenty20, aes(x=day, y=0.5, fill=lunar))+geom_raster(alpha=0.5)+
  scale_fill_manual(values=c("grey70", "white", "white", "white", "black", "white","white", "white"))+
  geom_point(data=twenty20, aes(x=day, y=rforestpredDD,col=temp))+
  theme_classic()+ylab("probability of presence")+
  scale_x_date(breaks="month")+theme(axis.text.x = element_text(angle = 45, hjust = 1))

# climate change scenario

head(twenty20CC)
twenty20CC <- twenty20CC %>% filter(!is.na(temp))
twenty20CC$degdays <- cumsum(twenty20CC$temp)

preds20CC <- as.data.frame(predict(rForest2, twenty20CC, type="prob"))
twenty20CC$rforestpredDD <- preds20CC$'1'


#plot both scenarios:

p1 <- ggplot(data=twenty20, aes(x=day, y=0.5, fill=lunar))+geom_tile(alpha=0.5)+
  scale_fill_manual(values=c("grey70", "white", "white", "white", "black", "white","white", "white"))+
  geom_point(data=twenty20, aes(x=day, y=rforestpredDD,col=temp))+
  theme_classic()+ylab("probability of presence")+
  scale_x_date(breaks="month")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+labs(title="2020")

p2 <- ggplot(data=twenty20CC, aes(x=day, y=0.5, fill=lunar))+geom_tile(alpha=0.5)+
  scale_fill_manual(values=c("grey70", "white", "white", "white", "black", "white","white", "white"))+
  geom_point(data=twenty20CC, aes(x=day, y=rforestpredDD,col=temp))+
  theme_classic()+ylab("probability of presence")+
  scale_x_date(breaks="month")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+labs(title="Climate change")

multiplot(p1, p2)













# Newer method - using rfPermute to assess variable importance:

library(rfPermute)
rpForest <- rfPermute(formula = z1, data = WDR4, na.action=na.omit, 
                      replace = FALSE, ntree = 1000, nrep = 100, a = 0.05)

print(rpForest)
rpForest$importance

#compare to original forest:
rForest2$importance

#plots
varImpPlot(rpForest,type=1)
varImpPlot(rpForest,type=2)

#now have p-values for sig of terms:
rpForest$pval









# Strobl et al 2007 https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-8-25
# suggest variable importance can be highly biased due to the nature of the data (eg numeric vs categorical, # of categories)
# therefore 1. ctree is better, 2. models should be fit WITHOUT replacement, 3. permutation importance is best metric (eg %IncMSE)

library(partykit)

?cforest()
controls <- list(replace=FALSE, fraction=0.632)
cForest <- partykit::cforest(formula = z1, data = WDR4, perturb=controls, na.action=na.omit, ntree=100)

partykit::varimp(cForest, conditional = TRUE) #use conditional=TRUE when predictor variables are correlated



#can also do partial dependencies with cforest (and many other models)

partialDegdays <- cForest %>% partial(pred.var = "degdays", prob = TRUE, train=WDR4, which.class=2) 
p1 <- ggplot(partialDegdays, aes(x=degdays, y=yhat))+geom_point()+geom_smooth()

partialTemp <- cForest %>% partial(pred.var = "temp", prob = TRUE, train=WDR4, which.class=2) 
p2 <- ggplot(partialTemp, aes(x=temp, y=yhat))+geom_point()+geom_smooth()

partialLunar <- cForest %>% partial(pred.var = "lunar", prob = TRUE, train=WDR4, which.class=2) 
p3 <- ggplot(partialLunar, aes(x=lunar, y=yhat))+geom_boxplot()

multiplot(p1, p2, p3)









#ranger can compute RF or survival forests. Option to include importance output - impurity corrected 
# avoids biases when there are different numbers of levels in categorical predictors. Also has 
# some great prediction functions for spatial analysis 
library(ranger)
?ranger

# can't handle NAs implicitly.
anyNA(WDR4) #not a problem with our dataset


#fit ranger model:
ranger <- ranger(formula = z1, replace=FALSE, importance="impurity_corrected", data = WDR4)
ranger
ranger$variable.importance
ranger$confusion.matrix

# different variable importances but order remained the same





#Boruta is considered to be one of the better predictor importance approaches:
library(Boruta)
?Boruta

Boruta <- Boruta(formula = z1, data = WDR4)
Boruta
plot(Boruta)




# ranger and Boruta don't like NAs so have to drop or can fix this by imputing missing values with Amelia:

library(Amelia)
??Amelia




# So far we've been ignoring model tuning for the most part. 
# a few other parameters you can play with in RF are:
# M - number of trees, set as ntree, 
# mrty - number of variables tried at each split defaults to # parameters/3
# nodesize - min # of data points allowed to fill a node, defaults to 5)


# The rebsustituation method used in random forest to assess out of bag predictions 
# is more optimistic than a true cross validation. The optimal approach to assessing the influence 
# of model parameters is to generate a training and test set as we did above. Ignoring that, 
# this will give you some sense of how varying these parameters influences the model:

#number of trees:

rForestnt <- randomForest(formula=z1, data = WDR4, replace=FALSE, na.action=na.omit,
                        importance=TRUE, do.trace=10000, ntree=10000)
plot(rForestnt)


#mrty

rForestm1 <- randomForest(formula=z1, data = WDR4, replace=FALSE, na.action=na.omit,
                          importance=TRUE, do.trace=2000, ntree=2000, mrty=1)
rForestm2 <- randomForest(formula=z1, data = WDR4, replace=FALSE, na.action=na.omit,
                          importance=TRUE, do.trace=2000, ntree=2000, mrty=2)
rForestm3 <- randomForest(formula=z1, data = WDR4, replace=FALSE, na.action=na.omit,
                          importance=TRUE, do.trace=2000, ntree=2000, mrty=3)
rForestm4 <- randomForest(formula=z1, data = WDR4, replace=FALSE, na.action=na.omit,
                          importance=TRUE, do.trace=2000, ntree=2000, mrty=4)
rForestm5 <- randomForest(formula=z1, data = WDR4, replace=FALSE, na.action=na.omit,
                          importance=TRUE, do.trace=2000, ntree=2000, mrty=5)
#nodesize:

rForestn1 <- randomForest(formula=z1, data = WDR4, replace=FALSE, na.action=na.omit,
                          importance=TRUE, do.trace=2000, ntree=2000, nodesize=1)
rForestn1 <- randomForest(formula=z1, data = WDR4, replace=FALSE, na.action=na.omit,
                          importance=TRUE, do.trace=2000, ntree=2000, nodesize=5)
rForestn1 <- randomForest(formula=z1, data = WDR4, replace=FALSE, na.action=na.omit,
                          importance=TRUE, do.trace=2000, ntree=2000, nodesize=10)
rForestn1 <- randomForest(formula=z1, data = WDR4, replace=FALSE, na.action=na.omit,
                          importance=TRUE, do.trace=2000, ntree=2000, nodesize=50)





# Another related model is boosted regression trees aka gradient boosting machines. These models fit a series of regressions
# is a stage-wise fashion, focusing on explaining the residual erros from the previous stage(s) of the model. This approach
# takes some parameter tuning, when done properly they *can outperform* random forests. Also performs better
# for certain data types (poisson, ranked)

library(gbm)
?gbm
utils::browseVignettes("gbm")

source('brt.functions.R') # from Elith et al. (2008)


# See Elith et al. (2008) for model tuning. Generally speaking you want to have a slow learning rate, which increases
# the number of trees, as well as a high tree complexity. gbm.step is from Elith et al. (2008), which uses 
# a cross validation procedure to figure out the optimal number of trees based on set tree complexity and learning rate:
str(WDR4)
WDR4$presI <- as.integer(WDR4$pres)
gbm1 <- gbm.step(data=WDR4, 
                          gbm.x = 5:7,
                          gbm.y = 8,
                          family = "bernoulli",
                          tree.complexity = 5,
                          learning.rate = 0.001,
                          bag.fraction = 0.5,
                          max.trees=20000)
print(gbm1)
summary(gbm1)
names(gbm1)


#plot fitted probability values:
gbm.plot.fits(gbm1, plot.layout = c(1,3))


#check to see if there are any uninformative predictors, to consider removing:
WDR4.simp <- gbm.simplify(gbm1, n.drops = 2)


#partial dependency plots (accounting for effects of other predictor variables):
gbm.plot(gbm1, n.plots=3, write.title = F, plot.layout = c(1,3))


#explore variable interactions:

find.int <- gbm.interactions(gbm1)
find.int

#plot
WDR4$gbm.fitted <- gbm1$fitted
ggplot(WDR4, aes(x=degdays, y=gbm.fitted,col=lunar))+geom_point()+geom_smooth()+facet_wrap(~lunar)


#brt.functions has a nice 3D interaction plot for continuous variables:
par(mfrow=c(1,1))
gbm.perspec(gbm1,2,3, y.range=c(0,24000), z.range=c(0,1))




#celebrate:
library('beepr')
beep(sound = 3, expr = NULL)
beep(sound = 4, expr = NULL)
