context("ModelFrame")

# we need data
require(reshape2)
data("tips", package="reshape2")
mod1 <- lm(tip ~ total_bill, data=tips)
mod2 <- lm(tip ~ total_bill + sex, data=tips)
mod3 <- lm(tip ~ total_bill + sex + smoker, data=tips)
mod4 <- lm(tip ~ total_bill + sex*smoker, data=tips)
mod5 <- lm(tip ~ total_bill + sex:smoker, data=tips)
mod6 <- lm(tip ~ total_bill*sex, data=tips)
mod7 <- lm(tip ~ sex*smoker, data=tips)

coefDF1 <- buildModelCI(mod1)
coefDF2 <- buildModelCI(mod2)
coefDF3 <- buildModelCI(mod3)
coefDF4 <- buildModelCI(mod4)
coefDF5 <- buildModelCI(mod5)
coefDF6 <- buildModelCI(mod6)
coefDF7 <- buildModelCI(mod7)

tips$Threshold <- tips$tip >= 4.5
modG1 <- glm(Threshold ~ total_bill, data=tips, family=binomial(link="logit"))
modG2 <- glm(Threshold ~ total_bill + sex, data=tips, family=binomial(link="logit"))
modG3 <- glm(Threshold ~ total_bill + sex + smoker, data=tips, family=binomial(link="logit"))
modG4 <- glm(Threshold ~ total_bill + sex*smoker, data=tips, family=binomial(link="logit"))
modG5 <- glm(Threshold ~ total_bill + sex:smoker, data=tips, family=binomial(link="logit"))
modG6 <- glm(Threshold ~ total_bill*sex, data=tips, family=binomial(link="logit"))
modG7 <- glm(Threshold ~ sex*smoker, data=tips, family=binomial(link="logit"))

coefDFG1 <- buildModelCI(modG1)
coefDFG2 <- buildModelCI(modG2)
coefDFG3 <- buildModelCI(modG3)
coefDFG4 <- buildModelCI(modG4)
coefDFG5 <- buildModelCI(modG5)
coefDFG6 <- buildModelCI(modG6)
coefDFG7 <- buildModelCI(modG7)

test_that("Models come as data.frames", {
    expect_is(coefDF1, "data.frame")
    expect_is(coefDF2, "data.frame")
    expect_is(coefDF3, "data.frame")
    expect_is(coefDF4, "data.frame")
    expect_is(coefDF5, "data.frame")
    expect_is(coefDF6, "data.frame")
    expect_is(coefDF7, "data.frame")
    
    expect_is(coefDFG1, "data.frame")
    expect_is(coefDFG2, "data.frame")
    expect_is(coefDFG3, "data.frame")
    expect_is(coefDFG4, "data.frame")
    expect_is(coefDFG5, "data.frame")
    expect_is(coefDFG6, "data.frame")
    expect_is(coefDFG7, "data.frame")
})

test_that("Coefficients have proper dimenions", {
    expect_equal(dim(coefDF1), c(2, 7))
    expect_equal(dim(coefDF2), c(3, 7))
    expect_equal(dim(coefDF3), c(4, 7))
    expect_equal(dim(coefDF4), c(5, 7))
    expect_equal(dim(coefDF5), c(5, 7))
    expect_equal(dim(coefDF6), c(4, 7))
    expect_equal(dim(coefDF7), c(4, 7))
    
    expect_equal(dim(coefDFG1), c(2, 7))
    expect_equal(dim(coefDFG2), c(3, 7))
    expect_equal(dim(coefDFG3), c(4, 7))
    expect_equal(dim(coefDFG4), c(5, 7))
    expect_equal(dim(coefDFG5), c(5, 7))
    expect_equal(dim(coefDFG6), c(4, 7))
    expect_equal(dim(coefDFG7), c(4, 7))
})

test_that("Coefficients have proper types", {
    expect_equal(sapply(coefDF1, class), c(Value="numeric", Coefficient="factor", HighInner="numeric", LowInner="numeric", HighOuter="numeric", LowOuter="numeric", Model="character"))
    expect_equal(sapply(coefDF2, class), c(Value="numeric", Coefficient="factor", HighInner="numeric", LowInner="numeric", HighOuter="numeric", LowOuter="numeric", Model="character"))
    expect_equal(sapply(coefDF3, class), c(Value="numeric", Coefficient="factor", HighInner="numeric", LowInner="numeric", HighOuter="numeric", LowOuter="numeric", Model="character"))
    expect_equal(sapply(coefDF4, class), c(Value="numeric", Coefficient="factor", HighInner="numeric", LowInner="numeric", HighOuter="numeric", LowOuter="numeric", Model="character"))
    expect_equal(sapply(coefDF5, class), c(Value="numeric", Coefficient="factor", HighInner="numeric", LowInner="numeric", HighOuter="numeric", LowOuter="numeric", Model="character"))
    expect_equal(sapply(coefDF6, class), c(Value="numeric", Coefficient="factor", HighInner="numeric", LowInner="numeric", HighOuter="numeric", LowOuter="numeric", Model="character"))
    expect_equal(sapply(coefDF7, class), c(Value="numeric", Coefficient="factor", HighInner="numeric", LowInner="numeric", HighOuter="numeric", LowOuter="numeric", Model="character"))
    
    expect_equal(sapply(coefDFG1, class), c(Value="numeric", Coefficient="factor", HighInner="numeric", LowInner="numeric", HighOuter="numeric", LowOuter="numeric", Model="character"))
    expect_equal(sapply(coefDFG2, class), c(Value="numeric", Coefficient="factor", HighInner="numeric", LowInner="numeric", HighOuter="numeric", LowOuter="numeric", Model="character"))
    expect_equal(sapply(coefDFG3, class), c(Value="numeric", Coefficient="factor", HighInner="numeric", LowInner="numeric", HighOuter="numeric", LowOuter="numeric", Model="character"))
    expect_equal(sapply(coefDFG4, class), c(Value="numeric", Coefficient="factor", HighInner="numeric", LowInner="numeric", HighOuter="numeric", LowOuter="numeric", Model="character"))
    expect_equal(sapply(coefDFG5, class), c(Value="numeric", Coefficient="factor", HighInner="numeric", LowInner="numeric", HighOuter="numeric", LowOuter="numeric", Model="character"))
    expect_equal(sapply(coefDFG6, class), c(Value="numeric", Coefficient="factor", HighInner="numeric", LowInner="numeric", HighOuter="numeric", LowOuter="numeric", Model="character"))
    expect_equal(sapply(coefDFG7, class), c(Value="numeric", Coefficient="factor", HighInner="numeric", LowInner="numeric", HighOuter="numeric", LowOuter="numeric", Model="character"))
})
