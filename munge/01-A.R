
aux.df <- read.csv('data/train.csv', stringsAsFactors = F)
names(aux.df) <- tolower(names(aux.df))
class(aux.df)
head(aux.df)

dat.train <- aux.df %>% 
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S"), 
         weekday = factor(weekdays(datetime, abbreviate = T), 
                          levels = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri',
                                     'Sat', 'Sun')),
         month = factor(months(datetime, abbreviate = T),
                        levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                                   'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')),
         year.num = year(datetime),
         sexuponoutcome = str_replace_all( recode(sexuponoutcome, "''='Unknown'"),
                                          'Unknown', 'Unknown Unknown')
         ) %>% 
  separate(sexuponoutcome, c('stateoutcome', 'gender'), sep = ' ', remove = F) %>% 
  separate(ageuponoutcome, c('age.num', 'age.time'), sep = ' ', remove = F) %>% 
  mutate(age.time = str_replace(age.time, '[s]', ''),
         age.timeint = recode(age.time, 
                           "'day'=365; 'week'=53; 'month'=12; 'year'= 1"),
         age = as.numeric(age.num)/age.timeint,
         age.months = age*12,
         name.know = recode(name, "'' = 'no name'; else = 'given'"),
         stateoutcome = recode(stateoutcome, 
                    "'' = 'Unknown'; c('Neutered', 'Spayed')= 'Neutered/Spayed'"),
         breedtype = recode( as.character(str_detect(breed, pattern = 'Mix|/')), 
                            "'TRUE'='Mixed';else = 'Purebred'")
  ) 
tail(dat.train)
apply( is.na(dat.train), 2, sum)



# • Breeds 
dat.breed <- dat.train %>% 
  select(animalid, animaltype, outcometype, 
         breedtype, breed) %>% 
  separate(breed, c('b1','b2'), sep = "/") %>% 
  gather(variable, breed, b1:b2, na.rm = T) %>% 
  mutate(breed = str_trim(str_replace_all(breed, ' Mix', '')),
         bin = 1) %>%
  select(-variable) %>% 
  unique %>% 
  spread(breed, bin, fill = 0) %>% 
  rename(`Black Tan Hound`= Black) %>% 
  select(-`Tan Hound`)
head(dat.breed)
dim(dat.breed)
names(dat.breed)


# Tipo de razas
dat.breed %>% 
  select(-1, -3, -4) %>% 
  gather(breed, bin, -animaltype) %>% 
  filter(bin == 1) %>% 
  select(-bin) %>% 
  unique() %>%
  arrange(animaltype, breed) #%>% 
# write.csv('doc/breed_characteristics.csv', col.names = F)


# Unir nuevas characteristicas a base
aux.chars <- read.csv('doc/breed_chars.csv', stringsAsFactors = F)
dim(aux.chars)
head(aux.chars)

tt <- dat.breed %>% 
  gather(breedmod, bin, -(1:4)) %>% 
  filter(bin == 1) %>% 
  left_join( aux.chars, by = c('breedmod' = 'breed', 'animaltype'))
dim(tt)
head(tt)

tt %>% 
  group_by(animaltype, breedgroup) %>% 
  tally

tt %>% 
  filter(animaltype == 'Dog' & is.na(breedgroup) ) %>% 
  group_by(animaltype, breedmod) %>% 
  tally





# • Color 
dat.color <- dat.train %>% 
  select(animalid, animaltype, outcometype, 
         breedtype, color) %>% 
  separate(color, c('c1','c2'), sep = "/") %>% 
  gather(variable, color, c1:c2, na.rm = T) %>% 
  mutate(bin = 1) %>%
  select(-variable) %>% 
  unique %>% 
  spread(color, bin, fill = 0)
head(dat.color)
dim(dat.color)

rm('aux.df')


# breeds

dat.train %>% 
  dplyr::select(animalid, breed)
