###################################################
############# Orange Juice Regression #############
###################################################

## read in the data
oj <- read.csv("oj.csv") 
head(oj)
dim(oj)
levels(oj$brand)

# getmode <- function(v) {
#   uniqv <- unique(v)
#   uniqv[which.max(tabulate(match(v, uniqv)))]
# }
# ModePrice = aggregate(price~brand, data = oj, getmode)
# names(ModePrice)[names(ModePrice) == "price"] <- "modePrice"
# oj = merge(oj, ModePrice)
# oj$discount = ifelse(oj$price < oj$modePrice, 1, 0)
# write.csv(oj, "oj2.csv", row.names = FALSE)

aggregate(sales~feat, data = oj, mean)
summary(oj[oj$feat == 0, "sales"])
summary(oj[oj$feat == 1, "sales"])

aggregate(price~feat, data = oj, mean)

aggregate(price~brand, data = oj, max)
aggregate(price~brand, data = oj, mean)

aggregate(discount~feat, data = oj, mean)
aggregate(sales~discount, data = oj, mean)

reg1 = lm(sales~feat, data = oj)
summary(reg1)
reg2 = lm(sales~discount, data = oj)
summary(reg2)

reg3 = lm(sales~price, data = oj)
summary(reg3)
plot(oj$price,oj$sales, main = "Price and sales",
     xlab = "Price", ylab = "Sales")
abline(lm(sales~price, data = oj), col = "blue")

reg4 = lm(sales ~ feat + discount, data = oj)
summary(reg4)
# Compare with regression with single independent variable
summary(reg1)
summary(reg2)

reg5 = lm(sales ~ feat + price, data = oj)
summary(reg5)
summary(reg3)

summary(lm(sales ~ price, data = oj[oj$feat==0,]))
summary(lm(sales ~ price, data = oj[oj$feat==1,]))

oj$feat_price = oj$feat * oj$price
reg6 = lm(sales ~ feat + price + feat_price, data = oj)
summary(reg6)