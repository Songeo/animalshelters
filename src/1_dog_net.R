
library(ProjectTemplate)
load.project()


fit.gs.bayes
names(fit.gs.bayes)

tab.l <- lapply(names(fit.gs.bayes), function(nom){
  tab <- fit.gs.bayes[[nom]]$prob %>% 
    data.frame()
  names(tab)[1] <- nom
  tab
})
names(tab.l) <- names(fit.gs.bayes)
lapply(tab.l, head)

tab.l$name.know %>% 
  group_by(name.know) %>% 
  summarise(ss = sum(Freq)) %>% 
  ungroup() %>% 
  mutate(tot = sum(ss), 
         porc = ss/tot)


filter(tab.l$outcometype, outcometype == 'Adoption')
