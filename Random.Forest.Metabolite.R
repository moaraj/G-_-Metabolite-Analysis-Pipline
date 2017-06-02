require(rpart.plot)


tree.test <- metab_data4[,c(8,11,12:ncol(metab_data4))]
tree.test <- filter(tree.test, Diet %in% c("CD","HF"))
f.rp  <- rpart(Diet ~ ., data=tree.test)
prpKy(f.rp)


## slightly larger than default:
(f.rp2 <- rpart(Diet ~ ., data=tree.test, minsplit = 8, cp = .1))
prp(f.rp2, type=1, extra=3)

(f.rp3 <- rpart(Diet ~ ., data=tree.test, minsplit = 8, cp = .01))
prp(f.rp3, type=1, extra=3)

(f.rp4 <- rpart(Diet ~ ., data=tree.test, minsplit = 8, cp = .001))
prp(f.rp4, type=1, extra=3)

(f.rp5 <- rpart(Diet ~ ., data=tree.test, minsplit = 8, cp = .0001))
prp(f.rp5, type=1, extra=3)

require(plotmo)

## For Cp-plot, we want a more "interesting" (larger) example
plotcp(f.rp2) # not interesting: trivial model is best  !!!
plotcp(f.rp3) # not interesting: trivial model is best  !!!
plotcp(f.rp4)



## silly too large model
f.rp.age <- rpart(Diet ~ ., data = tree.test,
               control = rpart.control(minsplit = 5, cp=0))


tree.test <- metab_data4[,c(8,11,12:ncol(metab_data4))]
tree.test <- filter(tree.test, Age.Cohort %in% c(1:4))  %>% filter(Diet == "CD")
f.rp.age.0  <- rpart(Age.Cohort ~ ., data=tree.test)
prpKy(f.rp)

## slightly larger than default:
(f.rp2.age.1 <- rpart(Age.Cohort ~ ., data=tree.test, minsplit = 4, cp = .1))
prp(f.rp2.age.1, type=3, extra=3)

(f.rp2.age.2 <- rpart(Age.Cohort ~ ., data=tree.test, minsplit = 4, cp = .01))
prp(f.rp2.age.2, type=3, extra=3)

(f.rp2.age.3 <- rpart(Age.Cohort ~ ., data=tree.test, minsplit = 4, cp = .001))
prp(f.rp2.age.3, type=3, extra=3)



require(plotmo)
## For Cp-plot, we want a more "interesting" (larger) example
plotcp(f.rp) #
plotcp(f.rp2) 



