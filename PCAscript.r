
require(Amelia)
worldbank<- read.csv("Worldbank (1).csv")


#Rename variables which are very long
#names(worldbank)
names(worldbank)[names(worldbank) == 'Health.expenditure.
                 .public....of.GDP...SH.XPD.PUBL.ZS.'] <- 'Healthexp%GDP'
names(worldbank)[names(worldbank) == 'Health.expenditure.per.capita..PPP..constant.2011.inter
                 national.....SH.XPD.PCAP.PP.KD.'] <- 'Healthexppercap'
names(worldbank)[names(worldbank) == 'Life.expectancy.at
                 .birth..total..years...SP.DYN.LE00.IN.'] <- 'LifeExp'
names(worldbank)[names(worldbank) == 'Air.transport..
                 passengers.carried..IS.AIR.PSGR.'] <- 'Airpassengers'
names(worldbank)[names(worldbank) == 'Mobile.cellular
                 .subscriptions..IT.CEL.SETS.'] <- 'MobileCellSubs'
names(worldbank)[names(worldbank) == 'Internet.u
                 sers..per.100.people...IT.NET.USER.P2.'] <- 'InternetUsersPer100'
names(worldbank)[names(worldbank) == 'Fixed.broadba
                 nd.subscriptions..IT.NET.BBND.'] <- 'FixedBroadSubs'
names(worldbank)[names(worldbank) == 'Agricultural.
                 land....of.land.area...AG.LND.AGRI.ZS.'] <- 'AgricultureLand%area'
names(worldbank)[names(worldbank) == 'CO2.emissio
                 ns..kg.per.2010.US..of.GDP...EN.ATM.CO2E.KD.GD.'] <- 'C02emKgper2010USofGDP'
names(worldbank)[names(worldbank) == 'Cereal.prod
                 uction..metric.tons...AG.PRD.CREL.MT.'] <- 'CerealProdM3tons'
names(worldbank)[names(worldbank) == 'Capture.fisheries
                 .production..metric.tons...ER.FSH.CAPT.MT.'] <- 'CaptureFishprodM3tons'
names(worldbank)[names(worldbank) == 'Forest.area...
                 .of.land.area...AG.LND.FRST.ZS.'] <- 'ForestArea%area'
names(worldbank)[names(worldbank) == 'Population.dens
                 ity..people.per.sq..km.of.land.area...EN.POP.DNST.'] <- 'PopDensityperSqKmland'
names(worldbank)[names(worldbank) == 'Urban.populati
                 on....of.total...SP.URB.TOTL.IN.ZS.'] <- 'Urbanpop%total'
names(worldbank)[names(worldbank) == 'Population..tota
                 l..SP.POP.TOTL.'] <- 'TotalPopulation'
names(worldbank)[names(worldbank) == 'Adolescent.fertility.rate..births.per.1.000.women.age
                 s.15.19...SP.ADO.TFRT.'] <- 'Adolesfertilityper1000'
names(worldbank)[names(worldbank) == 'Food.productio
                 n.index..2004.2006...100...AG.PRD.FOOD.XD.'] <- 'FoodprodIndex'
names(worldbank)[names(worldbank) == 'Duration.of.comp
                 ulsory.education..years...SE.COM.DURS.'] <- 'CompulsoryEdYears'
names(worldbank)[names(worldbank) == 'Tax.revenue....
                 of.GDP...GC.TAX.TOTL.GD.ZS.'] <- 'TaxRev%GDP'
names(worldbank)[names(worldbank) == 'GDP.per.capita.
                 .current.US....NY.GDP.PCAP.CD.'] <- 'GDPperCapCurrUS$'
names(worldbank)[names(worldbank) == 'International.
                 tourism..number.of.arrivals..ST.INT.ARVL.'] <- 'IntTourismArrival'
names(worldbank)[names(worldbank) == 'International.tourism..expenditures.for.travel.items
                 ..current.US....ST.INT.TVLX.CD.'] <- 'IntTourismExp'
worldbank<- worldbank[,-27]
worldbank<-worldbank[,5:length(worldbank)]


missmap(worldbank)
#Check current state of missing values
#Check current state of missing values
invisible(sapply(worldbank, function(x) sum(is.na(x))/nrow(worldbank)*100))

##plot the histograms
par(mfrow=c(2, 4)) 

worldbank$`TaxRev%GDP`<-NULL
worldbank$`Healthexp%GDP`<-NULL
worldbank$FoodprodIndex<-NULL
worldbank$`Urbanpop%total`<-NULL
worldbank$`AgricultureLand%area`<-NULL
worldbank$CompulsoryEdYears<-NULL
worldbank$`ForestArea%area`<-NULL
worldbank$InternetUsersPer100<-NULL
worldbank$LifeExp<-NULL

for (i in 1:length(worldbank)){
  hist(worldbank[,i],xlab=paste( names(worldbank)[i]),main=NULL)}





par(mfrow=c(1,2))
unscaleddata<- read.csv("C:/Users/robin/Dropbox/Applied Multivariate Analysis/project/dataset.csv")


unscaleddata$Continent<-as.character(unscaleddata$Continent)

## Change name of the Australian COntinent to Oceania :
index<-which(unscaleddata$Continent=="Australia")
unscaleddata[index,length(unscaleddata)]<-"Oceania"


unscaleddata$Rule.Type<-NULL
unscaleddata$Flag.Colours<-NULL
unscaleddata$Flag.Colours.Raw<-NULL
unscaleddata$Official.Languages<-NULL
unscaleddata$Official.Languages.Raw<-NULL

## Assign the countries to the rownames 
## assign the row names

unscaleddata_nocat<-unscaleddata[,3:24]

## Look at correlation plots instead 
library(corrplot)
corr_data<-unscaleddata_nocat
colnames(corr_data)<-abbreviate(colnames(corr_data))
#corrplot(cor(corr_data), method="ellipse")


cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

res1 <- cor.mtest(corr_data,0.95)

## specialized the insignificant value according to the significant level
corrplot(cor(corr_data), p.mat = res1[[1]], insig="blank")

###plot the  correlation matrix for the scaled and transformed data #####################

# The final scaled and transformed data :
transformedandscaleddata<-read.csv("worldbank_transformed_csv.csv")
finaldata<-transformedandscaleddata
## Remoive HealthExpGDp as contains the same information as the transformed variable 
#logHealthexppercapita     
finaldata$Healthexp.GDP<-NULL

# Change the name of Australia to Oceania :
finaldata$Continent<-as.character(finaldata$Continent)
index<-which(finaldata$Continent=="Australia")
finaldata[index,length(finaldata)]<-"Oceania"
## Assign the countries to the rownames 
finaldata1<- finaldata[,-1]
rownames(finaldata1) <- finaldata[,1]
finaldata<-finaldata1



# Change irrleelvant variables
finaldata$Rule.Type<-NULL
finaldata$Flag.Colours<-NULL
finaldata$Flag.Colours.Raw<-NULL
finaldata$Official.Languages<-NULL
finaldata$Official.Languages.Raw<-NULL

data_nocat<-finaldata[1:19]
#str(finaldata)
#str(data_nocat)

## Look at correlation plots instead 


corr_data1<-data_nocat
colnames(corr_data1)<-abbreviate(colnames(corr_data1))
#corrplot(cor(corr_data1), method="ellipse")


res2 <- cor.mtest(corr_data1,0.95)

## specialized the insignificant value according to the significant level
corrplot(cor(corr_data1), p.mat = res2[[1]], insig="blank")


## use the covariance matrix
data_nocat1<-data_nocat
rownames(data_nocat1)<-abbreviate(rownames(data_nocat1))
colnames(data_nocat1)<-abbreviate(colnames(data_nocat1))
pcawithout_scaling<-princomp(data_nocat1,cor=F)  



par(mfrow=c(1,1))
## Consider PCa with scaling correlation matrix =T
## Still use the correlation matrix ?
pcawith_scaling<-princomp(data_nocat1,cor=T)



##Scree plots
pr.var <- (pcawith_scaling$sdev)^2
# Variance explained by each principal component: pve
pve <- pr.var /sum(pr.var)

##
##for the unscaled data:

pr.noscalevar<-(pcawithout_scaling$sdev)^2
pvenoscale<-pr.noscalevar/(sum(pr.noscalevar))


###Plot the pve and cummulative prinicipal experience:

# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")


points(pvenoscale,col="red",type="b")


legend("topright",col=c("black","red"),pch=16,c("PCA-Correlation matrix","PCA- Covariance Matrix"),cex=0.7)


par(mfrow=c(1,2))
biplot(pcawith_scaling,main=" PCA -corr matrix",cex=0.7)
biplot(pcawithout_scaling,main="PCA - cov matrix",cex=0.7)


eigv<- eigen(cor(data_nocat))$values
eigv>mean(eigen(cor(data_nocat))$values)
require(scales)
par(mfrow=c(1,1))
pca_score1<-pcawith_scaling$scores[,1]

pca_score2<-pcawith_scaling$scores[,2]

plot(pca_score1,pca_score2,pch=16,col=cscale(finaldata$logGDPperCapCurrUS.,
                                             seq_gradient_pal("red", "green")))

legend("topright",col=c("red","green"),pch=16,c("Low GDP","High GDP"),cex=0.4)

text(pca_score1,pca_score2, labels=abbreviate(finaldata[,23]),cex=0.6) 

plot(pca_score1,pca_score2,pch=16,col=cscale(finaldata$logCaptureFishprodM3tonspertotpopulation,seq_gradient_pal("red", "green")))

legend("topright",col=c("red","green"),pch=16,c("Low Capture","High Capture"),cex=0.4)

text(pca_score1,pca_score2, labels=abbreviate(finaldata[,22]),cex=0.6)


###Code for the table of loadings
require(pander)
#install.packages("plyr")
#require(plyr)
pca_corr<-princomp(data_nocat,cor=T)

pca_loadings<-unclass(loadings(pca_corr))

Mardia1stpc<-which(abs(pca_loadings[,1])>0.7*max(abs(pca_loadings[,1])))


##
loadinsonly<-pca_loadings
LoadingsPC1<-loadinsonly[,1]
Mardiaspc1laodings<-LoadingsPC1[Mardia1stpc]


# Using Mardia's condition we obtain 9 variables which are important to the  1st principal component
#The variable "logHealthexpperCap","logGDPperCapCurrUS", and"InternetUsersper100"  serves to be the variable with the highest 
# all variables negative except for the "Adolesfertillity per100".

# Similarly we can look at the 2nd compoenent using the Mardias condition L


## Look at the 2nd component for the loadings

Mardia2ndpc<-which(abs(pca_loadings[,2])>0.7*max(abs(pca_loadings[,2])))


LoadingsPC2<-loadinsonly[,2]
Mardiaspc2laodings<-LoadingsPC2[Mardia2ndpc]


## Most important variable in the 2nd PC : AgriculatureLand area
# Contrast beween Forestarea  and the Agriculture Land Area and  C02kgper2010
#US$ of GDP  , a decrease in Agriculture Land Area and C02 emissions that stem from 
#burning of fossil fuels and the manufacture of cement  leads to an increase in the Forest
#Area of the countries .


## Look at the 3rd component for the loadings :

MardiasPC3<-which(abs(pca_loadings[,3])>0.7*max(abs(pca_loadings[,3])))

loadingsPC3<-loadinsonly[,3]
Mardiapc3loadings<-loadingsPC3[MardiasPC3]


## Look at the 4th component for the loadings 
MardiasPC4<-which(abs(pca_loadings[,4])>0.7*max(abs(pca_loadings[,4])))

loadingsPC4<-loadinsonly[,4]
Mardiapc4loadings<-loadingsPC4[MardiasPC4]



## On the 4th Component , an increase in Population density , leads to a decrease in Forest Area


##LOokking at the 5th componnet :
MardiasPC5<-which(abs(pca_loadings[,5])>0.7*max(abs(pca_loadings[,5])))

loadingsPC5<-loadinsonly[,5]
Mardiapc5loadings<-loadingsPC5[MardiasPC5]




## Looking at the 6 th compoennet :
MardiaPC6<-which(abs(pca_loadings[,6])>0.7*max(abs(pca_loadings[,6])))
loadingsPC6<-loadinsonly[,6]
Mardiapc6loadings<-loadingsPC6[MardiaPC6]


a<-data.frame(names(pca_loadings[,1]))

b<-data.frame(Mardiaspc1laodings)
c<-data.frame(Mardiaspc2laodings)
d<-data.frame(Mardiapc3loadings)
e<-data.frame(Mardiapc4loadings)
f<-data.frame(Mardiapc5loadings)
g<-data.frame(Mardiapc6loadings)



set.caption("loadings of the first 6 components on Mardia's criterion")
pander(list(`PC1 Loadings on MArdia`=b,`PC2 Loadings on MArdia`=c,`PC3 Loadings on MArdia`=d,`PC4 Loadings on MArdia`=e,`PC5 Loadings on MArdia`=f,`PC6 Loadings on MArdia`=g))

