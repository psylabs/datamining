#### Purchases of Ben and Jerry's Ice Cream
benjer = read.csv("BenAndJerry.csv")

## explore a bit
names(benjer)

## create a new variable for price per unit
priceper1 = (benjer$price_paid_deal + benjer$price_paid_non_deal)/benjer$quantity
y <- log(1+priceper1)

## grab some covariates of interest
## we'll create a properly formatted data.frame
x <- benjer[,c("flavor_descr","size1_descr",
	"household_income","household_size")]

## relevel 'flavor' to have baseline of vanilla
x$flavor_descr <- relevel(x$flavor_descr,"VAN")
## coupon usage
x$usecoup = factor(benjer$coupon_value>0)
x$couponper1 <- benjer$coupon_value/benjer$quantity
## organize some demographics
x$region <- factor(benjer$region, 
	levels=1:4, labels=c("East","Central","South","West"))
x$married <- factor(benjer$marital_status==1)
x$race <- factor(benjer$race,
	levels=1:4,labels=c("white","black","asian","other"))
x$hispanic_origin <- benjer$hispanic_origin==1
x$microwave <- benjer$kitchen_appliances %in% c(1,4,5,7)
x$dishwasher <- benjer$kitchen_appliances %in% c(2,4,6,7)
x$sfh <- benjer$type_of_residence==1
x$internet <- benjer$household_internet_connection==1
x$tvcable <- benjer$tv_items>1

## fit the regression
fit <- glm(y~., data=x) 

## grab the non-intercept p-values from a glm
## -1 to drop the intercept, 4 is 4th column
pvals <- summary(fit)$coef[-1,4] 

## source the fdr_cut function
source("fdr.R")

### ####### ###
### Mo Code ###
### ####### ###

#### Exploratory Analysis ####

# What Flavors do we have?
par(mfrow=c(1,1))
par(mar=c(5,10,5,5))
barplot(table(benjer$flavor),border=NA, las=2)

class(benjer$promotion_type) #gotta make it a factor
hist(y); hist(priceper1);  #the non-logged is more normal
hist(benjer$promotion_type) #Mostly store features
#benjer$promotion_type[is.na(benjer$promotion_type)]=0 #Cant have blanks, making it 5 is going to bite you in the ass, should be 0
#benjer$promotion_type=factor(benjer$promotion_type)
boxplot(y[benjer$promotion_type>0] ~ benjer$promotion_type, col= levels(factor(benjer$promotion))) #gotta add levels to the boxplot
#Not super helpful; lots of data outside >1sd and no clear differences
plot(priceper1[benjer$promotion_type>0] ~ benjer$quantity[benjer$promotion_type>0], col=factor(benjer$promotion_type), pch=20)
legend("topright", fill=levels(factor(benjer$promotion_type)), legend=c("Store Feature","Store Coupon", "Manufacturer Coupon", "Other Deal"))
#green clustered to top and price volatility reduces with quantity; marginally helpful graph

class(benjer$region)
boxplot(y ~ benjer$region, col=levels(factor(benjer$region))) #useless
plot(y ~ quantity, data=benjer, col=factor(benjer$region), pch=20) #useless
regionpromo=glm(promotion_type ~ region, data=benjer)

#salestable <- tapply(exp(oj$logmove), oj[,c("feat","brand")], sum)
table <- tapply(y[benjer$promotion_type>0], benjer[,c("promotion_type","region")], sum)
#rownames(table)= c("Store Feature","Store Coupon", "Manufacturer Coupon", "Other Deal")
colnames(table)= c("East","Central","South","West")
mosaicplot(table, col=levels(factor(benjer$region)), main="(price?) by promotion and region")
#not sure this is right, is the y above irrelavent? Professor suggested plotting only 2 categorical variables.
## West advertises the most, East has more manufacture coupons (is B&J from the East)

### Analyze the given model ###

summary(fit)
print("The model sumamrizes log price versus a number of regressors, specifically"); names(x)
fit2 <- glm(y~. - flavor_descr, data=x) 
summary(fit2)
print("ignoring flavor we find that all the regressors save asisan, hispanic and dishwasher were significant, let's remove these")
fit3 <- glm(y~. -flavor_descr -race -dishwasher - hispanic_origin, data=x)
summary(fit3)
n=nrow(x)
BIC <- c(reg1=extractAIC(fit, k=log(n))[2],
         reg2=extractAIC(fit2, k=log(n))[2],
         reg3=extractAIC(fit3, k=log(n))[2])
# Model probabilities
eBIC <- exp(-0.5*(BIC-min(BIC)))
probs <- eBIC/sum(eBIC)
round(probs, 5)
print("The simplified model is better according to BIC. Could use out-of-sample to test, but this should work for now")


### Mo Notes
#discrete quantitites make it hard to see relations
#would like to see sale vs region in mosaic



