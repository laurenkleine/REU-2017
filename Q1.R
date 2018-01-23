Markdown can be found: http://rpubs.com/laurenkleine96/286781

#Original GMPD 2.0 database

GMPD_main <- read.csv("data_GMPD_main.csv")

#This line searches for all species (HostCorrectedName) that have the
#character "No" under the variable "Native Range"
as.vector(GMPD_main$HostCorrectedName[grep("No",as.character(GMPD_main$NativeRange))])

#The unique function removes repeated species
#This line of code creates a list of every host that have been recorded outside and inside of their
#their native range
unique(as.vector(GMPD_main$HostCorrectedName[grep("No",as.character(GMPD_main$NativeRange))]))

#stored species list in "list"
list <- unique(as.vector(GMPD_main$HostCorrectedName[grep("No",as.character(GMPD_main$NativeRange))]))

#I still do not understand this code
grep(paste(list,collapse="|"),as.character(GMPD_main$HostCorrectedName))

I <- grep(paste(list,collapse="|"),as.character(GMPD_main$HostCorrectedName))

invasives <- GMPD_main[I,]

write.csv(invasives, file = "data_invasives.csv")

##A list of 39 species found outside of Native Range has been created
sp.<- unique(invasives$HostCorrectedName)
write.csv(sp., "Figure 1.csv")

#This is the data for invasives 
idata <- read.csv("data_invasives.csv")


#The species (Host Correceted Name) is found using grep
#Each time this code is run, a new species pasted into the "pattern"
rows <- grep("Vulpes vulpes",as.character(idata$HostCorrectedName))

temp.df <- idata[rows,]

#These lines are used to calculate the number of times "No"
#was reported for the species listed above
No <- grep("No",as.character(temp.df$NativeRange))
length(No)


#These lines are used to calculate the number of times "Yes"
#was reported for the species listed above
Yes <- grep("Yes",as.character(temp.df$NativeRange))
length(Yes)


#Now we are looking for how many citations there are for each species.
#The total for citation value is broken down further for either citations of
#species found within native range (Yes) or species not found in native range (No)

yes.frame <- idata[Yes,]
length(unique(yes.frame$Citation))

no.frame <- idata[No,]
length(unique(no.frame$Citation))

##This step is where PSR was calculated for all hosts with a complete binomial entry

invasives <- read.csv('data_invasives.csv')

as.vector(invasives$ParasiteCorrectedName[grep("yes",as.character(invasives$HasBinomialName))])
unique(as.vector(invasives$ParasiteCorrectedName[grep("yes",as.character(invasives$HasBinomialName))]))

list<-unique(as.vector(invasives$ParasiteCorrectedName[grep("yes",as.character(invasives$HasBinomialName))]))
grep(paste(list,collapse="|"),as.character(invasives$ParasiteCorrectedName))

B <- grep(paste(list,collapse="|"),as.character(invasives$ParasiteCorrectedName))

Has_Binomial_Name <- invasives[B,]

#A file was written with all complete binomial named parasites and 
#affiliated hosts
write.csv(Has_Binomial_Name, file = "data_BinomialName.csv")

#List of the unique parasite binomials
unique(Has_Binomial_Name$ParasiteCorrectedName)


bdata <- read.csv("data_BinomialName.csv")

rows <- grep("yes",as.character(bdata$HasBinomialName))
temp.df <- bdata[rows,]

No <- grep("No",as.character(temp.df$NativeRange))
length(No)

write.csv(temp.df[No,], file = "data_No_Native_Range_Complete_Binomial.csv")
View(temp.df[No,])

Yes <- grep("Yes",as.character(temp.df$NativeRange))
write.csv(temp.df[Yes,], file = "data_Yes_Native_Range_Complete_Binomial.csv")

length(Yes)

#Seperate CSV files were made "Yes_Native_Range.csv" and No_Native_Range.csv"
#These files only have the host binomail and complete parasite binomial 
#load counter function

#[COUNTER
  
  counter <- function(x){
    #x is a list of host names where each host name
    #occurs a number of times equal to the number of
    #parasite species found in the host
    
    #this function calculates and returns
    #the parasite species richness of each host
    
    names <- unique(x)
    
    PSR <- length(which(x==names[1]))
    
    for (i in 2:length(names)) PSR <- append(PSR, length(which(x==names[i])))  
    
    #generate output
    names(PSR) <- names
    PSR
  }
#]

yes <- read.csv("data_Yes_Native_Range.csv")
str(yes)

yes.hosts <- yes[,1]

yes.out <- counter(yes.hosts)
head(yes.out)

no<- read.csv("data_No_Native_Range.csv")
no.hosts <- no[,1]

no.out<- counter(no.hosts)
head(no.out)

#These two files were written into CSVs with the PSR (only complete binomials)
write.csv(yes.out, file = "data_yes.csv")
write.csv(no.out, file = "data_no.csv")

#PSR was adjusted by adding in all appripriate parasite species without a 
#complete binomial to their appropriate list

#Spearman rank 


Spearman_data <- read.csv("Master_Sheet_Spearman.csv")
View(Spearman_data)
str(Spearman_data)
#Spearman

#Our two mesaures of sampling effort are coorelated with species richness

cor.test((log(Spearman_data$Yes.Cit)), (log(Spearman_data$Yes.PSR)))
cor.test(Spearman_data$Yes.Cit, Spearman_data$Yes.PSR, method = "spearman")
hist(Spearman_data$Yes.Cit)


#Comparing Citation count to raw PSR
cor.test(Spearman_data$Yes.Cit, Spearman_data$Yes.PSR, method = "spearman")
cor.test(Spearman_data$No.Cit, Spearman_data$No.PSR, method = "spearman")

#Comparing Invidual counts to raw PSR
cor.test(Spearman_data$Yes.Indiv, Spearman_data$Yes.PSR, method = "spearman")
cor.test(Spearman_data$No.Indiv, Spearman_data$No.PSR, method = "spearman")

#Comparing Ciation counts to Individual counts
cor.test(Spearman_data$Yes.Indiv, Spearman_data$Yes.Cit, method = "spearman")
cor.test(Spearman_data$No.Indiv, Spearman_data$No.Cit, method = "spearman")
str(data)


###GAM (Generalized Additive Model)

##Compared univariate models vs. multivarite models to
#get residual values (compared AIC values, smalles = best)

library(mgcv)

data<- read.csv("Master_Sheet_ttest.csv")
str(data)

#GAM univariate (only looking at the number of individuals as an explainatory variable)
model1 <- gam(PSR~s(log(Indiv)), data=data)
summary(model1)
AIC(model1)
plot(model1)
resid1<- residuals(model1)
names(resid1)<- data$X
resid1

#GAM univariate (only looking at the number of citations as an explainatory variable)
model2 <- gam(PSR~s(log(Cit)), data=data)
summary(model2)
AIC(model2)
plot(model2)
resid2<- residuals(model2)
names(resid2)<- data$X
resid2

#GAM multivariate (both measures of sampling effort (number of individuals and number of citations) 
#as explainatory variables)
model3 <- gam(PSR~s(log(Cit))+s(log(Indiv)), data=data)
summary(model3)
AIC(model3)
plot(model3)
resid3<- residuals(model3)
names(resid3)<- data$X
resid3


##Ttest comparing the means of yes/no native range means in raw PSR, then 
#comparing the resdual values


data<- read.csv("Master_Sheet_ttest.csv")

str(data)

#Raw PSR ttest
t.test(PSR ~ Yes.No, data = data)

PSR_boxplot<- boxplot(PSR~Yes.No,data=data, main="t-test Parasite Species Richness", 
  	xlab="No (0) Means Yes (1)", ylab="Parasite Species Richness")

#Residual ttest
t.test(Resid ~ Yes.No, data = data)

boxplot(Resid~Yes.No,data=data, main="t-test Residual Values", 
  	xlab="No (0) Means Yes (1)", ylab="Residual Values (considering sampling effort")


#independent 2-group Mann-Whitney U Test

wilcox.test(data$PSR~data$Yes.No)

wilcox.test(data$Resid~data$Yes.No)

#########################################################################################################
#Same analysis as above, but with "Well-studied" entries: *15 or greater citations 

data2<- read.csv("Master_Sheet_ttest2.csv")

#Raw PSR ttest
t.test(PSR ~ Yes.No, data = data2)

PSR_boxplot<- boxplot(PSR~Yes.No,data=data2, main="t-test Parasite Species Richness", 
                      xlab="No (0) Means Yes (1)", ylab="Parasite Species Richness")

#Residual ttest
t.test(Resid ~ Yes.No, data = data2)

boxplot(Resid~Yes.No,data=data2, main="t-test Residual Values", 
        xlab="No (0) Means Yes (1)", ylab="Residual Values (considering sampling effort")


#independent 2-group Mann-Whitney U Test

wilcox.test(data2$PSR~data2$Yes.No)

wilcox.test(data2$Resid~data2$Yes.No)
