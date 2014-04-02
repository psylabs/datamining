
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
mtable <- tapply(y[benjer$promotion_type>0], benjer[,c("promotion_type","region")], sum)
#rownames(table)= c("Store Feature","Store Coupon", "Manufacturer Coupon", "Other Deal")
colnames(mtable)= c("East","Central","South","West")
mosaicplot(mtable, col=levels(factor(benjer$region)), main="(price?) by promotion and region")
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

dim(benjer)
summary(fit3)

plot(y ~ benjer$size1) #KEEP
boxplot(y ~ benjer$region) #no beuno
boxplot(y ~ benjer$household_income) #beuno
plot(benjer$coupon[])

#FDR stuff
cutoff <- fdr_cut(pvals, 0.01)