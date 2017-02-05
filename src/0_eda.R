library('ProjectTemplate')
reload.project()

head(dat.train)
n_distinct(dat.train$animalid) #26729
nrow(dat.train)

# Outcome type
dat.train %>% 
  group_by(outcometype, outcomesubtype) %>% 
  tally %>% 
  data.frame()



# • Heat maps (DATE TIME)
tab <-  dat.train %>% 
  select(outcometype, weekday, month, year.num, animalid) %>% 
  gather(variable, value, weekday:year.num) %>% 
  group_by(outcometype, variable, value) %>% 
  tally
tab$value <- factor(tab$value, 
                    levels = c(levels(dat.train$weekday), 
                               levels(dat.train$month), 
                               sort(unique(dat.train$year.num))
                               ))
# select variable and outcometype
ggplot(filter(tab, variable == 'year.num' & outcometype == 'Adoption'), 
       aes(x = outcometype, y = value, fill = n)) + 
  geom_tile(colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + 
  ylab(NULL) + xlab(NULL) + 
  ggtitle('Number of cases\n(n)')



# • Age, gender, state
summary(filter(dat.train, outcometype == 'Died')$age.months)
summary(filter(dat.train, outcometype == 'Adoption')$age)
summary(filter(dat.train, outcometype == 'Euthanasia')$age)
ggplot(dat.train, aes(x = outcometype, y = age)) + 
  geom_boxplot(position = 'dodge', alpha = .3) + 
  xlab(NULL) + 
  ylab('Age (years)')

ggplot(dat.train, aes(x = outcometype, y = age, 
                      fill = animaltype)) + 
  geom_boxplot(position = 'dodge', alpha = .3) + 
  xlab(NULL) + 
  ylab('Age (years)')

ggplot(dat.train, aes(x = outcometype, y = age, 
                      fill = gender)) + 
  geom_boxplot(position = 'dodge', alpha = .3) + 
  xlab(NULL) + 
  ylab('Age (years)')

ggplot(dat.train, aes(x = outcometype, y = age, 
                      fill = stateoutcome)) + 
  geom_boxplot(position = 'dodge', alpha = .3) + 
  xlab(NULL) + 
  ylab('Age (years)')

ggplot(dat.train, aes(x = outcometype, y = age, 
                      fill = name.know)) + 
  geom_boxplot(position = 'dodge', alpha = .3) + 
  xlab(NULL) + 
  ylab('Age (years)')

ggplot(dat.train, aes(x = outcometype, y = age, 
                      fill = breedtype)) + 
  geom_boxplot(position = 'dodge', alpha = .3, varwidth = T) + 
  xlab(NULL) + 
  ylab('Age (years)')




# • Breed 
head(dat.breed)

# dog breeds
dat.pca.dog <- dat.breed %>% 
  filter(animaltype == 'Dog') %>% 
  select(-(1:4))
cols.dog <- apply(dat.pca.dog, 2, sum)[apply(dat.pca.dog, 2, sum) > 0]
pca.dog <- PCA(dat.pca.dog[, names(cols.dog)])
plot(pca.dog, axes = c(1,2), choix = "var")
plot(pca.dog, axes = c(3,4), choix = "var")
plot(pca.dog, axes = c(4,5), choix = "var")




# cat breeds
dat.pca.cat <- dat.breed %>% 
  filter(animaltype == 'Cat') %>% 
  select(-(1:4))
cols.cat <- apply(dat.pca.cat, 2, sum)[apply(dat.pca.cat, 2, sum) > 0]
pca.cat <- PCA(dat.pca.cat[, names(cols.cat)])
plot(pca.cat, axes = c(1,2), choix = "var")
plot(pca.cat, axes = c(3,4), choix = "var")
plot(pca.cat, axes = c(4,5), choix = "var")




# breeds per outcome type and animal
tab.breed <- dat.breed %>% 
  gather(breed, bin, Abyssinian:`Yorkshire Terrier`) %>% 
  group_by(animaltype, outcometype, breed) %>% 
  summarise(frec = sum(bin)) 

tab.breed.dog <- tab.breed %>% 
  filter(animaltype == 'Dog') %>% 
  dcast(outcometype~breed, value.var = 'frec')
rownames(tab.breed.dog) <- tab.breed.dog[,1]
cols.dog <- 
  apply(tab.breed.dog[, -1], 2, sum)[apply(tab.breed.dog[, -1], 2, sum)>0]
tab.breed.dog.p <- Perfiles(as.matrix(tab.breed.dog[, names(cols.dog)]), 'd')
PCA(tab.breed.dog.p)

tab.breed.cat <- tab.breed %>% 
  filter(animaltype == 'Cat') %>% 
  dcast(outcometype~breed, value.var = 'frec')
rownames(tab.breed.cat) <- tab.breed.cat[,1]
cols.cat <- 
  apply(tab.breed.cat[, -1], 2, sum)[apply(tab.breed.cat[, -1], 2, sum)>0]
tab.breed.cat.p <- Perfiles(as.matrix(tab.breed.cat[, names(cols.cat)]), 'd')
PCA(tab.breed.cat.p)



# • Color

# colors per outcome type and animal
tab.color <- dat.color %>% 
  gather(color, bin, Agouti:`Yellow Brindle`) %>% 
  group_by(animaltype, outcometype, color) %>% 
  summarise(frec = sum(bin)) 

tab.color.dog <- tab.color %>% 
  filter(animaltype == 'Dog') %>% 
  dcast(outcometype~color, value.var = 'frec')
rownames(tab.color.dog) <- tab.color.dog[,1]
cols.dog <- 
  apply(tab.color.dog[, -1], 2, sum)[apply(tab.color.dog[, -1], 2, sum)>0]
tab.color.dog.p <- Perfiles(as.matrix(tab.color.dog[, names(cols.dog)]), 'd')
PCA(tab.color.dog.p)

tab.color.cat <- tab.color %>% 
  filter(animaltype == 'Cat') %>% 
  dcast(outcometype~color, value.var = 'frec')
rownames(tab.color.cat) <- tab.color.cat[,1]
cols.cat <- 
  apply(tab.color.cat[, -1], 2, sum)[apply(tab.color.cat[, -1], 2, sum)>0]
tab.color.cat.p <- Perfiles(as.matrix(tab.color.cat[, names(cols.cat)]), 'd')
PCA(tab.color.cat.p)