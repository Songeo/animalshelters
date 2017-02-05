library(ProjectTemplate)
reload.project()

head(dat.train)
dim(dat.train)
# 26729


head(dat.breed)
dat.mod <- dat.breed %>% 
  gather(breed, bin, 5:228) %>% 
  filter(bin != 0, 
         animaltype == 'Dog') %>% 
  left_join(
    read.csv('doc/breed_chars.csv') %>% 
        filter(animaltype == 'Dog') ,
    by = c("animaltype", "breed")
    ) %>% 
  left_join(
    dat.train %>% 
      select(animalid, gender, stateoutcome,
             weekday, month, year.num, 
             age, age.months, name.know),
    by = c("animalid")
  ) %>% 
  filter( !is.na(age)) %>% 
  mutate(age.f = cut_number(age, 3), 
         age.f2 = cut(x = age, breaks = c(0, 1, 3, 7, 19), 
                      include.lowest = T),
         name.know = factor(name.know),
         month.f = factor(month), 
         year.f = factor(year.num)
         ) 
apply(is.na(dat.mod), 2, sum)
dat.mod[apply(is.na(dat.mod), 1, sum)>0, ]
dim(dat.mod)
n_distinct(dat.mod$animalid)
head(dat.mod)


# • Bayesian Network
library(bnlearn)
aux.train <- select(dat.mod, 
       outcometype, breedtype, breedgroup, 
       coat.size, gender, stateoutcome, age.f2,
       name.know) %>% 
  filter(stateoutcome != 'Unknown', 
         breedgroup != 'Unknown'
         ) %>% 
  mutate(outcometype = factor(as.character(outcometype)),
         breedtype = factor(breedtype),
         gender = factor(gender),
         coat.size = factor(as.character(coat.size)),
         breedgroup = factor( str_replace(
           str_replace( as.character(breedgroup), 
                        'Working/Guardian', 'Working'), 
           'Hounds', 'Hound')),
         stateoutcome = factor(as.character(stateoutcome))
         ) #%>% 
  # select(-breedgroup)
str(aux.train)



# several algorithms
bn.gs <- gs(aux.train)
bn.iam <- iamb(aux.train)
bn.fiam <- fast.iamb(aux.train)
bn.iiam <- inter.iamb(aux.train)
bn.hc <- hc(aux.train, score = 'bic')
compare(bn.gs, bn.hc)

# par(mfrow = c(1, 2))
plot(bn.gs, main = "Constraint-based algorithms")
plot(bn.hc, main = "Hill-Climbing")
plot(bn.iiam, main = "Others")

# blacklist
n.rep <- ncol(aux.train)
black.list <- data.frame(
  from = rep('outcometype', n.rep-1),
  to = names(aux.train)[!str_detect(names(aux.train), 'outcometype')]
  ) %>% 
  rbind(
    data.frame(
      from = names(aux.train)[!str_detect( names(aux.train), 
                                           'gender|outcometype')],
      to = rep('gender', n.rep-2)
    )
  ) %>% 
  rbind(
    data.frame(
      from = names(aux.train)[!str_detect(names(aux.train), 
                                          'age.f2|outcometype')],
      to = rep('age.f2', n.rep-2)
    )
  ) %>% 
  rbind(
    data.frame(
      from = names(aux.train)[!str_detect(names(aux.train), 
                                          'coat.size|outcometype|breedgroup')],
      to = rep('coat.size', n.rep-3)
    )
  )
black.list


# Hill Climbing vs Grow-Shrink with Blacklist
bn.hc.bckl <- hc(aux.train, score = 'bic', 
                 blacklist = black.list)
bn.gs.bckl <- cextend(gs(aux.train, blacklist = black.list))

compare(bn.hc.bckl, bn.gs.bckl)
all.equal(bn.hc.bckl, bn.gs.bckl)
modelstring(bn.hc.bckl)
undirected.arcs(bn.hc.bckl)

# par(mfrow = c(1,2))
# graphviz.plot(bn.hc.bckl,shape="ellipse")
# graphviz.plot(bn.gs.bckl,shape="ellipse")

# Probabilistic Relationship Strength
strength.hc.bckl <- arc.strength(bn.hc.bckl, aux.train, criterion = 'bic')
strength.plot(bn.hc.bckl, strength.hc.bckl, shape="ellipse")
strength.gs.bckl <- arc.strength(bn.gs.bckl, aux.train)
strength.plot(bn.gs.bckl, strength.gs.bckl, shape="ellipse")

# Fit
fit.hc.bayes <- bn.fit(bn.hc.bckl, data = aux.train, method = "bayes")
fit.gs.bayes <- bn.fit(bn.gs.bckl, data = aux.train, method = "bayes")

write.net('doc/hc_shelteranimals.net',fit.hc.bayes)
write.net('doc/gs_shelteranimals.net',fit.gs.bayes)
write.dot('doc/hc_shelteranimals.dot',fit.hc.bayes)


cache('fit.gs.bayes')
