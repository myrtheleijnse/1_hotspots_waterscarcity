### multivariate test ###
# from https://data.library.virginia.edu/getting-started-with-multivariate-multiple-regression/

library(car)

ami_data <- read.table("http://static.lib.virginia.edu/statlab/materials/data/ami_data.DAT")
names(ami_data) <- c("TOT","AMI","GEN","AMT","PR","DIAP","QRS")

mlm1 <- lm(cbind(TOT, AMI) ~ GEN + AMT + PR + DIAP + QRS, data = ami_data)
summary(mlm1)

Anova(mlm1)

mlm2 <- update(mlm1, . ~ . - PR - DIAP - QRS)
anova(mlm1, mlm2)

linearHypothesis(mlm1, hypothesis.matrix = c("PR = 0", "DIAP = 0", "QRS = 0"))
