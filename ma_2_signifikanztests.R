#### SKRIPT 2: SIGNIFKANTSTESTS ####
#signifikanztests von Artenreichtum, Individuenabundanz und Nestabundanz zwischen den Standorten und Flächen

#erforderliche Pakete in skript laden
library(tidyverse)  
library(ggplot2)    
library(vegan)      
library(car)
library(FSA)
library(dplyr)
library(patchwork)
library(grid)
library(png)

# Daten aus CSV-Datei einlesen
rohdaten_1 <- read.csv2("C:/Users/p-ber/OneDrive/1_TH Bingen/MA und FORSCH/1_auswertung/tab_nisthilfen_20250903.csv", header = T)

# alle Nester mit 100% Mortalität entfernen
rohdaten_2 <- rohdaten_1 %>% filter(host.family!="Arachnidae", bemerkung!="angefangene BZ", n.gefressen.geschluepft==0, !(n.host.individuals<=0 & n.attacked.brood.cells<=0))

# Datensatz für signifikanztests vorbereiten (für (1) und (2))
filter_b <- rohdaten_2 %>% group_by(plot) %>% 
  summarise(ges_abund_indiv = sum(n.host.individuals, na.rm = TRUE), 
            ges_abund_nest = n(), 
            ges_abund_paras_indiv = sum(n.enemy.individuals.1, n.enemy.individuals.2, na.rm = TRUE),
            ges_abund_paras_nest = sum(!is.na(enemy.species.1) & trimws(enemy.species.1) != "", !is.na(enemy.species.2) & trimws(enemy.species.2) != ""),
            ges_arten_paras = n_distinct(enemy.species.1, enemy.species.2),
            ges_arten = n_distinct(host.species),
            rl_arten = sum(rlp.he_RL.status %in% c("R","3","2","1","0")))%>%  
  mutate(flaechentyp = case_when(str_detect(plot, "afs") ~ "AFS",str_detect(plot, "ref") ~ "REF"))%>%
  mutate(strukturtyp = case_when(str_detect(plot, "br") ~ "br",str_detect(plot, "acker") ~ "acker"))%>%
  mutate(standorttyp = case_when(str_detect(plot, "kw-") ~ "kw",str_detect(plot, "wi-") ~ "wi",str_detect(plot, "wit-") ~ "wit",str_detect(plot, "bv-") ~ "bv")) %>%
mutate (standort_flaeche = paste0(standorttyp, "(", flaechentyp, ")"))
filter_ges_arten <- filter_b %>% mutate(ges_arten_host_paras = ges_arten_paras + ges_arten) %>% select(standort_flaeche, flaechentyp, standorttyp, strukturtyp, ges_arten_host_paras, ges_abund_nest, plot)

#Datensatz für signifikanztests vorbereiten (für (3))
filter_k <- rohdaten_2 %>% filter(Wespe..w...biene..b...spinne..s...diptera..d. == "b") %>% group_by(plot) %>% 
  summarise(bee_abund_indiv = sum(n.host.individuals, na.rm = TRUE), 
            bee_abund_nest = n(), 
            bee_abund_paras = sum(!is.na(enemy.species.1) & trimws(enemy.species.1) != ""),
            bee_male = sum(n.male, na.rm = TRUE),
            bee_female = sum(n.female, na.rm = TRUE),
            bee_arten = n_distinct(host.species), 
            bee_rl_arten = sum(rlp.he_RL.status %in% c("R","3","2","1","0"), na.rm = TRUE)) %>%
  mutate(bee_female_anteil = bee_female / (bee_female + bee_male)) %>%
  mutate(flaechentyp = case_when(str_detect(plot, "afs") ~ "AFS",str_detect(plot, "ref") ~ "REF"),
         strukturtyp = case_when(str_detect(plot, "br") ~ "br",str_detect(plot, "acker") ~ "acker"),
         standorttyp = case_when(str_detect(plot, "kw-") ~ "kw",str_detect(plot, "wi-") ~ "wi",str_detect(plot, "wit-") ~ "wit",str_detect(plot, "bv-") ~ "bv")) %>%
  mutate (standort_flaeche = paste0(standorttyp, "(", flaechentyp, ")"))

#Datensatz für signifikanztests vorbereiten (für (4))
filter_no_wasps <- rohdaten_2 %>% group_by(plot) %>% summarise()
filter_m <- rohdaten_2 %>% filter(Wespe..w...biene..b...spinne..s...diptera..d. == "w") %>% group_by(plot) %>% 
  summarise(wasp_abund_indiv = sum(n.host.individuals, na.rm = TRUE), 
            wasp_abund_nest = n(), 
            wasp_abund_paras = sum(!is.na(enemy.species.1) & trimws(enemy.species.1) != ""),
            wasp_arten = n_distinct(host.species),
            wasp_rl_arten = sum(rlp.he_RL.status %in% c("R","3","2","1","0"), na.rm = TRUE))
filter_m <- filter_no_wasps %>% left_join(filter_m, by = "plot") %>%
  mutate(wasp_abund_indiv = replace_na(wasp_abund_indiv, 0),
         wasp_abund_nest = replace_na(wasp_abund_nest, 0),
         wasp_abund_paras = replace_na(wasp_abund_paras, 0),
         wasp_arten = replace_na(wasp_arten, 0),
         wasp_rl_arten = replace_na(wasp_rl_arten, 0))%>%
  mutate(flaechentyp = case_when(str_detect(plot, "afs") ~ "AFS",str_detect(plot, "ref") ~ "REF"),
         strukturtyp = case_when(str_detect(plot, "br") ~ "br",str_detect(plot, "acker") ~ "acker"),
         standorttyp = case_when(str_detect(plot, "kw-") ~ "kw",str_detect(plot, "wi-") ~ "wi",str_detect(plot, "wit-") ~ "wit",str_detect(plot, "bv-") ~ "bv")) %>%
  mutate (standort_flaeche = paste0(standorttyp, "(", flaechentyp, ")"))

#Datensatz für signifikanztests vorbereiten (für (5))
bee_wasp_daten <- filter_k %>% left_join(filter_m, by = "plot") 

#### (1) allgmemein ####
  #Artenreichtum
    #Artenreichtum AFS - REF
by(filter_ges_arten$ges_arten_host_paras, filter_ges_arten$flaechentyp, shapiro.test) #nicht normalverteilt 
leveneTest(ges_arten_host_paras ~ flaechentyp, data = filter_ges_arten) #Varianzhomogen
wilcox.test(ges_arten_host_paras ~ flaechentyp, data = filter_ges_arten) #p=0,0027 signifikante Unterschiede

    #Artenreichtum Acker - Baumreihe
by(filter_ges_arten$ges_arten_host_paras, filter_ges_arten$strukturtyp, shapiro.test) #normalverteilt
leveneTest(ges_arten_host_paras ~ strukturtyp, data = filter_ges_arten) #Varianzhomogen
summary(aov(filter_ges_arten$ges_arten_host_paras ~ filter_ges_arten$strukturtyp)) # p=0,355 kein signifikanter Unterschied

    #Artenreichtum Alle Standorte
by(filter_ges_arten$ges_arten_host_paras, filter_ges_arten$standorttyp, shapiro.test) #teilw. nicht normalverteilt
leveneTest(ges_arten_host_paras ~ standorttyp, data = filter_ges_arten) #Varianzhomogen
kruskal.test(ges_arten_host_paras ~ standorttyp, data = filter_ges_arten) #signifikante Unterschiede
dunnTest(ges_arten_host_paras ~ standorttyp, data = filter_ges_arten, method = "bonferroni") #bv-kw, bv-wi, kw-wit
filter_ges_arten_kw <- filter_ges_arten %>% filter(standorttyp == "kw")
by(filter_ges_arten_kw$ges_arten_host_paras, filter_ges_arten_kw$flaechentyp, shapiro.test) #normalverteilt
leveneTest(ges_arten_host_paras ~ flaechentyp, data = filter_ges_arten_kw) #varianzhomogen
summary(aov(filter_ges_arten_kw$ges_arten_host_paras ~ filter_ges_arten_kw$flaechentyp)) #0,496 nicht signifikant
      #Wittlich AFS - REF
filter_ges_arten_wit <- filter_ges_arten %>% filter(standorttyp == "wit")
by(filter_ges_arten_wit$ges_arten_host_paras, filter_ges_arten_wit$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(ges_arten_host_paras ~ flaechentyp, data = filter_ges_arten_wit) #varianzhomogen
wilcox.test(ges_arten_host_paras ~ flaechentyp, data = filter_ges_arten_wit) #0,01484 signifikant
      #Wiesbaden AFS - REF
filter_ges_arten_wi <- filter_ges_arten %>% filter(standorttyp == "wi")
by(filter_ges_arten_wi$ges_arten_host_paras, filter_ges_arten_wi$flaechentyp, shapiro.test) #normalverteilt
leveneTest(ges_arten_host_paras ~ flaechentyp, data = filter_ges_arten_wi) #varianzhomogen
summary(aov(filter_ges_arten_wi$ges_arten_host_paras ~ filter_ges_arten_wi$flaechentyp)) #0,06 nicht signifikant
      #Bad Vilbel AFS - REF
filter_ges_arten_bv <- filter_ges_arten %>% filter(standorttyp == "bv")
by(filter_ges_arten_bv$ges_arten_host_paras, filter_ges_arten_bv$flaechentyp, shapiro.test) # nicht normalverteilt
leveneTest(ges_arten_host_paras ~ flaechentyp, data = filter_ges_arten_bv) #varianzhomogen
wilcox.test(ges_arten_host_paras ~ flaechentyp, data = filter_ges_arten_bv) #0,03652 signifikant

  #Abundanzen
    #Individuenabundanz AFS - REF
by(filter_b$ges_abund_indiv, filter_b$flaechentyp, shapiro.test)#icht nomalverteilt
leveneTest(ges_abund_indiv ~ flaechentyp, data = filter_b) #Varianzhomogen
wilcox.test(ges_abund_indiv ~ flaechentyp, data = filter_b) #0,0097 signifikante Unterschiede
    #Nestabundanz AFS - REF
by(filter_b$ges_abund_nest, filter_b$flaechentyp, shapiro.test) #icht normalverteilt
leveneTest(ges_abund_nest ~ flaechentyp, data = filter_b) #varianzhomogen
wilcox.test(ges_abund_nest ~ flaechentyp, data = filter_b) #0,01035 signifikante Unterschiede

    #Individuenabundanz Acker - Baumreihe
by(filter_b$ges_abund_indiv, filter_b$strukturtyp, shapiro.test) #normalverteilt
leveneTest(ges_abund_indiv ~ strukturtyp, data = filter_b)  #Varianzhomogen
summary(aov(filter_b$ges_abund_indiv ~ filter_b$strukturtyp)) #0,8 keine signifikanten Unterschiede
    #Nestabundanz Acker - Baumreihe
by(filter_b$ges_abund_nest, filter_b$strukturtyp, shapiro.test) #normalverteilt
leveneTest(ges_abund_nest ~ strukturtyp, data = filter_b) #varianzhomogen
summary(aov(filter_b$ges_abund_nest ~ filter_b$strukturtyp)) #0,3 keine signifikanten Unterschied
    
    #Individuenabundanz alle Standorte
by(filter_b$ges_abund_indiv, filter_b$standorttyp, shapiro.test) #teilw. nicht normalverteilt
leveneTest(ges_abund_indiv ~ standorttyp, data = filter_b) #Varianzhomogen
kruskal.test(ges_abund_indiv ~ standorttyp, data = filter_b) #0,0748 keine signifikanten Unterschiede
    #Nestabundanz alle Standorte
by(filter_b$ges_abund_nest, filter_b$standorttyp, shapiro.test) #tweilw. nicht normalverteilt
leveneTest(ges_abund_nest ~ standorttyp, data = filter_b) #Varianzhomogen
kruskal.test(ges_abund_nest ~ standorttyp, data = filter_b) #0,004 signifikante Unterschiede
dunnTest(ges_abund_nest ~ standorttyp, data = filter_b, method = "bonferroni") #0,03 bv-kw, 0,008 kw-wit

      #Individuenabundanz Klein-Winternheim
filter_b_kw <- filter_b %>% filter(standorttyp == "kw")
by(filter_b_kw$ges_abund_indiv, filter_b_kw$flaechentyp, shapiro.test) #normalverteilt
leveneTest(ges_abund_indiv ~ flaechentyp, data = filter_b_kw) #varianzhomogen
summary(aov(filter_b_kw$ges_abund_indiv ~ filter_b_kw$flaechentyp)) #0,0438 signifikant
      #Nestabundanz Klein-Winternheim
by(filter_b_kw$ges_abund_nest, filter_b_kw$flaechentyp, shapiro.test) #normalverteilt
leveneTest(ges_abund_nest ~ flaechentyp, data = filter_b_kw) #varianzhomogen
summary(aov(filter_b_kw$ges_abund_nest ~ filter_b_kw$flaechentyp)) #0,127 nicht signifikant

      #Individuenabundanz Wittlich AFS - REF
filter_b_wit <- filter_b %>% filter(standorttyp == "wit")
by(filter_b_wit$ges_abund_indiv, filter_b_wit$flaechentyp, shapiro.test) #normalverteilt
leveneTest(ges_abund_indiv ~ flaechentyp, data = filter_b_wit) #varianzhomogen
summary(aov(filter_b_wit$ges_abund_indiv ~ filter_b_wit$flaechentyp)) #0,337 nicht signifikant
      #Nestabundanz Wittlich AFS - REF
by(filter_b_wit$ges_abund_nest, filter_b_wit$flaechentyp, shapiro.test) #normalverteilt
leveneTest(ges_abund_nest ~ flaechentyp, data = filter_b_wit) #varianzhomogen
summary(aov(filter_b_wit$ges_abund_nest ~ filter_b_wit$flaechentyp)) #0,0856 nicht signifikant

      #Individuenabundanz Wiesbaden AFS - REF
filter_b_wi <- filter_b %>% filter(standorttyp == "wi")
by(filter_b_wi$ges_abund_indiv, filter_b_wi$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(ges_abund_indiv ~ flaechentyp, data = filter_b_wi) #varianzhomogen
wilcox.test(ges_abund_indiv ~ flaechentyp, data = filter_b_wi) #0,4619 nicht signifikant
      #Nestabundanz Wiesbaden AFS - REF
by(filter_b_wi$ges_abund_nest, filter_b_wi$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(ges_abund_nest ~ flaechentyp, data = filter_b_wi) #varianzhomogen
wilcox.test(ges_abund_nest ~ flaechentyp, data = filter_b_wi) #0,1406 nicht signifikant

      #Individuenabundanz Bad Vilbel AFS - REF
filter_b_bv <- filter_b %>% filter(standorttyp == "bv")
by(filter_b_bv$ges_abund_indiv, filter_b_bv$flaechentyp, shapiro.test) #normalverteilt
leveneTest(ges_abund_indiv ~ flaechentyp, data = filter_b_bv) #nicht varianzhomogen
wilcox.test(ges_abund_indiv ~ flaechentyp, data = filter_b_bv) #0,1889 nicht signifikant
      #Nestabundanz Bad Vilbel AFS - REF
by(filter_b_bv$ges_abund_nest, filter_b_bv$flaechentyp, shapiro.test) #normalverteilt
leveneTest(ges_abund_nest ~ flaechentyp, data = filter_b_bv) #varianzhomogen
summary(aov(filter_b_bv$ges_abund_nest ~ filter_b_bv$flaechentyp)) #0,149 nicht signifikant


#### (2) nur Gegenspieler ####
  #Artenreichtum
    #Artenreichtum AFS - REF
by(filter_b$ges_arten_paras, filter_b$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(ges_arten_paras ~ flaechentyp, data = filter_b) #varianzhomogen
wilcox.test(ges_arten_paras ~ flaechentyp, data = filter_b) #0,01066 signifikante Unetrschiede

    #Artenreichtum Acker - Baumreihe
by(filter_b$ges_arten_paras, filter_b$strukturtyp, shapiro.test) #nicht normalverteilt
leveneTest(ges_arten_paras ~ strukturtyp, data = filter_b) #varianzhomogen
wilcox.test(ges_arten_paras ~ strukturtyp, data = filter_b) #0,4531 keine signifikanten unterschiede

    #Artenreichtum STandorte
by(filter_b$ges_arten_paras, filter_b$standorttyp, shapiro.test) #nicht normalverteilt
leveneTest(ges_arten_paras ~ standorttyp, data = filter_b) #varianzhomogen
kruskal.test(ges_arten_paras ~ standorttyp, data = filter_b) #0,0007 signifikant
dunnTest(ges_arten_paras ~ standorttyp, data = filter_b, method = "bonferroni") #bv-wit 0,012, kw-wit 0,006, wi-wit 0,048

#Klein-Winternheim AFS - REF
by(filter_b_kw$ges_arten_paras, filter_b_kw$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(ges_arten_paras ~ flaechentyp, data = filter_b_kw) #varianzhomogen
wilcox.test(ges_arten_paras ~ flaechentyp, data = filter_b_kw) #p=0,2 nicht signifikanter Unterschied

#Bad Vilbel AFS - REF
by(filter_b_bv$ges_arten_paras, filter_b_bv$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(ges_arten_paras ~ flaechentyp, data = filter_b_bv) #nicht varianzhomogen
wilcox.test(ges_arten_paras ~ flaechentyp, data = filter_b_bv) #p=0,08924 kein signifikanter Unterschied

#Wiesbaden AFS - REF
by(filter_b_wi$ges_arten_paras, filter_b_wi$flaechentyp, shapiro.test) #normalverteilt
leveneTest(ges_arten_paras ~ flaechentyp, data = filter_b_wi) #varianzhomogen
summary(aov(filter_b_wi$ges_arten_paras ~ filter_b_wi$flaechentyp)) #p=0,201 kein signifikanter Unterschied

#Wittlich AFS - REF
by(filter_b_wit$ges_arten_paras, filter_b_wit$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(ges_arten_paras ~ flaechentyp, data = filter_b_wit) #nicht varianzhomogen
wilcox.test(ges_arten_paras ~ flaechentyp, data = filter_b_wit) #p=0,1459 kein signifikanter Unterschied

  #Abundanz
    #Nestabundanz AFS - REF
by(filter_b$ges_abund_paras_nest, filter_b$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(ges_abund_paras_nest ~ flaechentyp, data = filter_b) #varianzhomogen
wilcox.test(ges_abund_paras_nest ~ flaechentyp, data = filter_b) #0,1078 nicht signifikant
    #Individuenabundanz AFS - REF
by(filter_b$ges_abund_paras_indiv, filter_b$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(ges_abund_paras_indiv ~ flaechentyp, data = filter_b) #varianzhomogen
wilcox.test(ges_abund_paras_indiv ~ flaechentyp, data = filter_b) #0,2951 nicht signifikant

    #Nestabundanz Acker - Baumreihe
by(filter_b$ges_abund_paras_nest, filter_b$strukturtyp, shapiro.test) #normalverteilt
leveneTest(ges_abund_paras_nest ~ strukturtyp, data = filter_b) #varianzhomogen
summary(aov(filter_b$ges_abund_paras_nest ~ filter_b$flaechentyp)) #0,971 keine signifikanten unterschiede
    #Individuenabundanz Acker - Baumreihe
by(filter_b$ges_abund_paras_indiv, filter_b$strukturtyp, shapiro.test) #nicht normalverteilt
leveneTest(ges_abund_paras_indiv ~ strukturtyp, data = filter_b) #varianzhomogen
wilcox.test(ges_abund_paras_indiv ~ strukturtyp, data = filter_b) #0,4586 keine signifikanten unterschiede

    #Nestabundanz Standorte
by(filter_b$ges_abund_paras_nest, filter_b$standorttyp, shapiro.test) #nicht normalverteilt
leveneTest(ges_abund_paras_nest ~ standorttyp, data = filter_b) #varianzhomogen
kruskal.test(ges_abund_paras_nest ~ standorttyp, data = filter_b) #signifikant
dunnTest(ges_abund_paras_nest ~ standorttyp, data = filter_b, method = "bonferroni") #bv-kw 0,003, kw-wit 0,002
    #Individuenabundanz Standorte
by(filter_b$ges_abund_paras_indiv, filter_b$standorttyp, shapiro.test) #nicht normalverteilt
leveneTest(ges_abund_paras_indiv ~ standorttyp, data = filter_b) #nicht varianzhomogen
kruskal.test(ges_abund_paras_indiv ~ standorttyp, data = filter_b) #signifikant
dunnTest(ges_abund_paras_indiv ~ standorttyp, data = filter_b, method = "bonferroni") #bv-kw 0,003, kw-wit 0,0004

#Individuenabundanz Klein-Winternheim AFS - REF
by(filter_b_kw$ges_abund_paras_indiv, filter_b_kw$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(ges_abund_paras_indiv ~ flaechentyp, data = filter_b_kw) #varianzhomogen
wilcox.test(ges_abund_paras_indiv ~ flaechentyp, data = filter_b_kw) #p=0,7209 nicht signifikanter Unterschied
#Nestabundanz Klein-Winternheim AFS - REF
by(filter_b_kw$ges_abund_paras_nest, filter_b_kw$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(ges_abund_paras_nest ~ flaechentyp, data = filter_b_kw) #varianzhomogen
summary(aov(filter_b_kw$ges_abund_paras_nest ~ filter_b_kw$flaechentyp)) #p=1 kein signifikanter Unterschied

#Individuenabundanz Bad Vilbel AFS - REF
by(filter_b_bv$ges_abund_paras_indiv, filter_b_bv$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(ges_abund_paras_indiv ~ flaechentyp, data = filter_b_bv) #varianzhomogen
wilcox.test(ges_abund_paras_indiv ~ flaechentyp, data = filter_b_bv) #p=0,1182 kein signifikanter Unterschied
#Nestabundanz Bad Vilbel AFS - REF
by(filter_b_bv$ges_abund_paras_nest, filter_b_bv$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(ges_abund_paras_nest ~ flaechentyp, data = filter_b_bv) #nicht varianzhomogen
wilcox.test(ges_abund_paras_indiv ~ flaechentyp, data = filter_b_bv) #p=0,1182 kein signifikanter Unterschied

#Individuenabundanz Wiesbaden AFS - REF
by(filter_b_wi$ges_abund_paras_indiv, filter_b_wi$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(ges_abund_paras_indiv ~ flaechentyp, data = filter_b_wi) #varianzhomogen
wilcox.test(ges_abund_paras_indiv ~ flaechentyp, data = filter_b_wi) #p=0,8744 kein signifikanter Unterschied
#Nestabundanz Wiesbaden AFS - REF
by(filter_b_wi$ges_abund_paras_nest, filter_b_wi$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(ges_abund_paras_nest ~ flaechentyp, data = filter_b_wi) #varianzhomogen
wilcox.test(ges_abund_paras_nest ~ flaechentyp, data = filter_b_wi) #p=0,5513 kein signifikanter Unterschied

#Individuenabundanz Wittlich AFS - REF
by(filter_b_wit$ges_abund_paras_indiv, filter_b_wit$flaechentyp, shapiro.test) # normalverteilt
leveneTest(ges_abund_paras_indiv ~ flaechentyp, data = filter_b_wit) #varianzhomogen
wilcox.test(ges_abund_paras_indiv ~ flaechentyp, data = filter_b_wit) #p=0,6473 kein signifikanter Unterschied
#Nestabundanz Wittlich AFS - REF
by(filter_b_wit$ges_abund_paras_nest, filter_b_wit$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(ges_abund_paras_nest ~ flaechentyp, data = filter_b_wit) #varianzhomogen
wilcox.test(ges_abund_paras_nest ~ flaechentyp, data = filter_b_wit) #p=0,3219 kein signifikanter Unterschied


#### (3) nur Wildbienen ####
  #Artenreichtum
    #Artenreichtum AFS - REF
by(filter_k$bee_arten, filter_k$flaechentyp, shapiro.test) #nicht normalverteilt 
leveneTest(bee_arten ~ flaechentyp, data = filter_k) #Varianzhomogen
wilcox.test(bee_arten ~ flaechentyp, data = filter_k) #p=0,115 kein signifikanter Unterschiede

    #Artenreichtum Acker - Baumreihe
by(filter_k$bee_arten, filter_k$strukturtyp, shapiro.test) #nicht normalverteilt
leveneTest(bee_arten ~ strukturtyp, data = filter_k) # nicht Varianzhomogen
wilcox.test(bee_arten ~ strukturtyp, data = filter_k) # p=0,2622 kein signifikanter Unterschied

    #Artenreichtum Standorte
by(filter_k$bee_arten, filter_k$standorttyp, shapiro.test) #nicht normalverteilt
leveneTest(bee_arten ~ standorttyp, data = filter_k) #varianzhomogen
kruskal.test(bee_arten ~ standorttyp, data = filter_k) #p=0,0006 signifikante Unterschiede
dunnTest(bee_arten ~ standorttyp, data = filter_k, method = "bonferroni") #bv-kw 0,008, bv-wi 0,02, kw-wit 0,015, wi-wit 0,03

      #Klein-Winternheim AFS - REF
filter_bee_kw <- filter_k %>% filter(standorttyp == "kw")
by(filter_bee_kw$bee_arten, filter_bee_kw$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(bee_arten ~ flaechentyp, data = filter_bee_kw) #varianzhomogen
wilcox.test(bee_arten ~ flaechentyp, data = filter_bee_kw) #p=1 kein signifikanter Unterschied

      #Bad Vilbel AFS - REF
filter_bee_bv <- filter_k %>% filter(standorttyp == "bv")
by(filter_bee_bv$bee_arten, filter_bee_bv$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(bee_arten ~ flaechentyp, data = filter_bee_bv) #varianzhomogen
wilcox.test(bee_arten ~ flaechentyp, data = filter_bee_bv) #p=0,3485 kein signifikanter Unterschied

      #Wiesbaden AFS - REF
filter_bee_wi <- filter_k %>% filter(standorttyp == "wi")
by(filter_bee_wi$bee_arten, filter_bee_wi$flaechentyp, shapiro.test) #normalverteilt
leveneTest(bee_arten ~ flaechentyp, data = filter_bee_wi) #varianzhomogen
summary(aov(filter_bee_wi$bee_arten ~ filter_bee_wi$flaechentyp)) #p=0,0671 kein signifikanter Unterschied
      #Wittlich AFS - REF
filter_bee_wit <- filter_k %>% filter(standorttyp == "wit")
by(filter_bee_wit$bee_arten, filter_bee_wit$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(bee_arten ~ flaechentyp, data = filter_bee_wit) #varianzhomogen
wilcox.test(bee_arten ~ flaechentyp, data = filter_bee_wit) #p=0,7148 kein signifikanter Unterschied

  #Abundanzen
    #Individuenabundanz AFS - REF
by(filter_k$bee_abund_indiv, filter_k$flaechentyp, shapiro.test)#nicht nomalverteilt
leveneTest(bee_abund_indiv ~ flaechentyp, data = filter_k) #varianzhomogen
wilcox.test(bee_abund_indiv ~ flaechentyp, data = filter_k) #0,04 signifikante Unterschiede
    #Nestabundanz AFS - REF
by(filter_k$bee_abund_nest, filter_k$flaechentyp, shapiro.test) #anicht normalverteilt
leveneTest(bee_abund_nest ~ flaechentyp, data = filter_k) #varianzhomogen
wilcox.test(bee_abund_nest ~ flaechentyp, data = filter_k) #0,06 keine signifikante Unterschiede

    #Individuenabundanz Acker - Baumreihe
by(filter_k$bee_abund_indiv, filter_k$strukturtyp, shapiro.test) #normalverteilt
leveneTest(bee_abund_indiv ~ strukturtyp, data = filter_k)  #Varianzhomogen
summary(aov(filter_k$bee_abund_indiv ~ filter_k$strukturtyp)) #0,953 keine signifikanten Unterschiede
    #Nestabundanz Acker - Baumreihe
by(filter_k$bee_abund_nest, filter_k$strukturtyp, shapiro.test) #normalverteilt
leveneTest(bee_abund_nest ~ strukturtyp, data = filter_k) #varianzhomogen
summary(aov(filter_k$bee_abund_nest ~ filter_k$strukturtyp)) #0,528 keine signifikanten Unterschiede

    #Individuenabundanz Standorte
by(filter_k$bee_abund_indiv, filter_k$standorttyp, shapiro.test) #teilw. nicht normalverteilt
leveneTest(bee_abund_indiv ~ standorttyp, data = filter_k) #Varianzhomogen
kruskal.test(bee_abund_indiv ~ standorttyp, data = filter_k) # nicht signifikant
    #Nestabundanz Standorte
by(filter_k$bee_abund_nest, filter_k$standorttyp, shapiro.test) #tweilw. nicht normalverteilt
leveneTest(bee_abund_nest ~ standorttyp, data = filter_k) #Varianzhomogen
kruskal.test(bee_abund_nest ~ standorttyp, data = filter_k) #signifikante Unterschiede
dunnTest(bee_abund_nest ~ standorttyp, data = filter_k, method = "bonferroni") #0,0017 kw-wit

      #Individuenabundanz Klein-Winternheim AFS - REF
by(filter_bee_kw$bee_abund_indiv, filter_bee_kw$flaechentyp, shapiro.test) #normalverteilt
leveneTest(bee_abund_indiv ~ flaechentyp, data = filter_bee_kw) #varianzhomogen
summary(aov(filter_bee_kw$bee_abund_indiv ~ filter_bee_kw$flaechentyp)) #p=0,027 signifikanter Unterschied
      #Nestabundanz Klein-Winternheim AFS - REF
by(filter_bee_kw$bee_abund_nest, filter_bee_kw$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(bee_abund_nest ~ flaechentyp, data = filter_bee_kw) #varianzhomogen
wilcox.test(bee_abund_nest ~ flaechentyp, data = filter_bee_kw) #p=0,2192 kein signifikanter Unterschied

      #Individuenabundanz Bad Vilbel AFS - REF
by(filter_bee_bv$bee_abund_indiv, filter_bee_bv$flaechentyp, shapiro.test) #normalverteilt
leveneTest(bee_abund_indiv ~ flaechentyp, data = filter_bee_bv) #nicht varianzhomogen
wilcox.test(bee_abund_indiv ~ flaechentyp, data = filter_bee_bv) #p=0,1889 kein signifikanter Unterschied
      #Nestabundanz Bad Vilbel AFS - REF
by(filter_bee_bv$bee_abund_nest, filter_bee_bv$flaechentyp, shapiro.test) #normalverteilt
leveneTest(bee_abund_nest ~ flaechentyp, data = filter_bee_bv) #varianzhomogen
summary(aov(filter_bee_bv$bee_abund_nest ~ filter_bee_bv$flaechentyp)) #p=0,149 kein signifikanter Unterschied

      #Individuenabundanz Wiesbaden AFS - REF
by(filter_bee_wi$bee_abund_indiv, filter_bee_wi$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(bee_abund_indiv ~ flaechentyp, data = filter_bee_wi) #varianzhomogen
wilcox.test(bee_abund_indiv ~ flaechentyp, data = filter_bee_wi) #p=0,8665 kein signifikanter Unterschied
      #Nestabundanz Wiesbaden AFS - REF
by(filter_bee_wi$bee_abund_nest, filter_bee_wi$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(bee_abund_nest ~ flaechentyp, data = filter_bee_wi) #varianzhomogen
wilcox.test(bee_abund_nest ~ flaechentyp, data = filter_bee_wi) #p=0,4515 kein signifikanter Unterschied

      #Individuenabundanz Wittlich AFS - REF
by(filter_bee_wit$bee_abund_indiv, filter_bee_wit$flaechentyp, shapiro.test) # normalverteilt
leveneTest(bee_abund_indiv ~ flaechentyp, data = filter_bee_wit) #varianzhomogen
summary(aov(filter_bee_wit$bee_abund_indiv ~ filter_bee_wit$flaechentyp)) #p=0,541 kein signifikanter Unterschied
      #Wittlich AFS - REF
by(filter_bee_wit$bee_abund_nest, filter_bee_wit$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(bee_abund_nest ~ flaechentyp, data = filter_bee_wit) #varianzhomogen
wilcox.test(bee_abund_nest ~ flaechentyp, data = filter_bee_wit) #p=0,2719 kein signifikanter Unterschied


#### (4) nur Wespen ####

  #Artenreichtum
    #Artenreichtum AFS - REF
by(filter_m$wasp_arten, filter_m$flaechentyp, shapiro.test) #nicht normalverteilt 
leveneTest(wasp_arten ~ flaechentyp, data = filter_m) #Varianzhomogen
wilcox.test(wasp_arten ~ flaechentyp, data = filter_m) #p=0,4533 signifikante Unterschiede

    #Artenreichtum Acker - Baumreihe
by(filter_m$wasp_arten, filter_m$strukturtyp, shapiro.test) #nicht normalverteilt
leveneTest(wasp_arten ~ strukturtyp, data = filter_m) # Varianzhomogen
summary(aov(filter_m$wasp_arten ~ filter_m$strukturtyp)) # p=0,409 kein signifikanter Unterschied

    #Artenreichtum Standorte
by(filter_m$wasp_arten, filter_m$standorttyp, function(x) {if (length(x) >= 3 && length(unique(x)) > 1) {
  shapiro.test(x)} else {return("Zu wenige Werte oder keine Variation – Test nicht möglich")}}) #nicht normalverteilt
leveneTest(wasp_arten ~ standorttyp, data = filter_m) #signifikante Unterschiede in der varianz
kruskal.test(wasp_arten ~ standorttyp, data = filter_m) #p=0,0001073 signifikante Unterschiede
dunnTest(wasp_arten ~ standorttyp, data = filter_m, method = "bonferroni") #signifikante Unterschiede bv-kw, bv-wit

      #Klein-Winternheim AFS - REF
filter_wasp_kw <- filter_m %>% filter(standorttyp == "kw")
by(filter_wasp_kw$wasp_arten, filter_wasp_kw$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(wasp_arten ~ flaechentyp, data = filter_wasp_kw) #varianzhomogen
wilcox.test(wasp_arten ~ flaechentyp, data = filter_wasp_kw) #p=1 kein signifikanter Unterschied

      #Bad Vilbel AFS - REF
filter_wasp_bv <- filter_m %>% filter(standorttyp == "bv")
by(filter_wasp_bv$wasp_arten, filter_wasp_bv$flaechentyp, shapiro.test) #keine Wespen in Bad Vilbel

      #Wiesbaden AFS - REF
filter_wasp_wi <- filter_m %>% filter(standorttyp == "wi")
by(filter_wasp_wi$wasp_arten, filter_wasp_wi$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(wasp_arten ~ flaechentyp, data = filter_wasp_wi) #varianzhomogen
wilcox.test(wasp_arten ~ flaechentyp, data = filter_wasp_wi) #p=0,8193 kein signifikanter Unterschied

      #Wittlich AFS - REF
filter_wasp_wit <- filter_m %>% filter(standorttyp == "wit")
by(filter_wasp_wit$wasp_arten, filter_wasp_wit$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(wasp_arten ~ flaechentyp, data = filter_wasp_wit) #varianzhomogen
summary(aov(filter_wasp_wit$wasp_arten ~ filter_wasp_wit$flaechentyp)) #p=0,0448 signifikanter Unterschied

  #Abundanzen
    #Individuenabundanz AFS - REF
by(filter_m$wasp_abund_indiv, filter_m$flaechentyp, shapiro.test)#nicht nomalverteilt
leveneTest(wasp_abund_indiv ~ flaechentyp, data = filter_m) #varianzhomogen
wilcox.test(wasp_abund_indiv ~ flaechentyp, data = filter_m) #0,4 keine signifikanten Unterschiede
    #Nestabundanz AFS - REF
by(filter_m$wasp_abund_nest, filter_m$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(wasp_abund_nest ~ flaechentyp, data = filter_m) #varianzhomogen
wilcox.test(wasp_abund_nest ~ flaechentyp, data = filter_m) #0,4 signifikante Unterschiede

    #Individuenabundanz Acker - Baumreihe
by(filter_m$wasp_abund_indiv, filter_m$strukturtyp, shapiro.test) #nicht normalverteilt
leveneTest(wasp_abund_indiv ~ strukturtyp, data = filter_m)  #varianzhomogen
wilcox.test(wasp_abund_indiv ~ strukturtyp, data = filter_m) #0,48 keine signifikanten Unterschiede
    #Nestabundanz Acker - Baumreihe
by(filter_m$wasp_abund_nest, filter_m$strukturtyp, shapiro.test) #nicht normalverteilt
leveneTest(wasp_abund_nest ~ strukturtyp, data = filter_m) #varianzhomogen
wilcox.test(wasp_abund_nest ~ strukturtyp, data = filter_m) #0,614 keine signifikanten Unterschiede

    #Individuenabundanz Standorte
by(filter_m$wasp_abund_indiv, filter_m$standorttyp, function(x) {if (length(x) >= 3 && length(unique(x)) > 1) {
  shapiro.test(x)} else {return("Zu wenige Werte oder keine Variation – Test nicht möglich")}}) #teilw. nicht normalverteilt
leveneTest(wasp_abund_indiv ~ standorttyp, data = filter_m) # nicht varianzhomogen
kruskal.test(wasp_abund_indiv ~ standorttyp, data = filter_m) #0,0003 signifikante Unterschiede
dunnTest(wasp_abund_indiv ~ standorttyp, data = filter_m, method = "bonferroni") #bv-kw 0,02, bv-wi 0,04, bv-wit 0,0001
    #Nestabundanz Standorte
by(filter_m$wasp_abund_nest, filter_m$standorttyp, function(x) {if (length(x) >= 3 && length(unique(x)) > 1) {
  shapiro.test(x)} else {return("Zu wenige Werte oder keine Variation – Test nicht möglich")}}) #teilw. nicht normalverteilt
leveneTest(wasp_abund_nest ~ standorttyp, data = filter_m) # nicht Varianzhomogen
kruskal.test(wasp_abund_nest ~ standorttyp, data = filter_m) #0,0001 signifikante Unterschiede
dunnTest(wasp_abund_nest ~ standorttyp, data = filter_m, method = "bonferroni") #bv-kw 0,019, bv-wit 0,0001

      #Individuenabundanz Klein-Winternheim AFS - REF
by(filter_wasp_kw$wasp_abund_indiv, filter_wasp_kw$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(wasp_abund_indiv ~ flaechentyp, data = filter_wasp_kw) #varianzhomogen
wilcox.test(wasp_abund_indiv ~ flaechentyp, data = filter_wasp_kw) #p=0,7832 kein signifikanter Unterschied
      #Nestabundanz Klein-Winternheim AFS - REF
by(filter_wasp_kw$wasp_abund_nest, filter_wasp_kw$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(wasp_abund_nest ~ flaechentyp, data = filter_wasp_kw) #varianzhomogen
wilcox.test(wasp_abund_nest ~ flaechentyp, data = filter_wasp_kw) #p=0,8688 kein signifikanter Unterschied

      #Individuenabundanz Bad Vilbel AFS - REF
by(filter_wasp_bv$wasp_abund_indiv, filter_wasp_bv$flaechentyp, shapiro.test) #keine Wespen vorhanden
      #Nestabundanz Bad Vilbel AFS - REF
by(filter_wasp_bv$wasp_abund_nest, filter_wasp_bv$flaechentyp, shapiro.test) #keine wespen vorhanden

      #Individuenabundanz Wiesbaden AFS - REF
by(filter_wasp_wi$wasp_abund_indiv, filter_wasp_wi$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(wasp_abund_indiv ~ flaechentyp, data = filter_wasp_wi) #varianzhomogen
wilcox.test(wasp_abund_indiv ~ flaechentyp, data = filter_wasp_wi) #p=0,614 signifikanter Unterschied
      #Nestabundanz Wiesbaden AFS - REF
by(filter_wasp_wi$wasp_abund_nest, filter_wasp_wi$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(wasp_abund_nest ~ flaechentyp, data = filter_wasp_wi) #varianzhomogen
wilcox.test(wasp_abund_nest ~ flaechentyp, data = filter_wasp_wi) #p=0,822 kein signifikanter Unterschied

      #Individuenabundanz Wittlich AFS - REF
by(filter_wasp_wit$wasp_abund_indiv, filter_wasp_wit$flaechentyp, shapiro.test) # normalverteilt
leveneTest(wasp_abund_indiv ~ flaechentyp, data = filter_wasp_wit) #varianzhomogen
summary(aov(filter_wasp_wit$wasp_abund_indiv ~ filter_wasp_wit$flaechentyp)) #p=0,559 kein signifikanter Unterschied
      #Nestabundanz Wittlich AFS - REF
by(filter_wasp_wit$wasp_abund_nest, filter_wasp_wit$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(wasp_abund_nest ~ flaechentyp, data = filter_wasp_wit) #varianzhomogen
summary(aov(filter_wasp_wit$wasp_abund_nest ~ filter_wasp_wit$flaechentyp)) #p=0,216 kein signifikanter Unterschied


#### (5) Wildbienen vs. Wespen ####

#bee_wasp_abund_nest
bee_wasp_abund_nest <- bee_wasp_daten %>% pivot_longer(cols = c(bee_abund_nest, wasp_abund_nest),names_to = "Kategorie",values_to = "Wert")
bee_wasp_abund_nest <- bee_wasp_abund_nest %>% select(plot, flaechentyp.x, Kategorie, Wert)
bee_wasp_abund_nest <- bee_wasp_abund_nest %>% mutate(flaechentyp_Kategorie = paste(flaechentyp.x, Kategorie))
by(bee_wasp_abund_nest$Wert, bee_wasp_abund_nest$flaechentyp_Kategorie, shapiro.test) #nicht normalverteilt
leveneTest(Wert ~ flaechentyp_Kategorie, data = bee_wasp_abund_nest) #nicht varianzhomogen
kruskal.test(Wert ~ flaechentyp_Kategorie, data = bee_wasp_abund_nest) #signifikante Unterschiede
dunnTest(Wert ~ flaechentyp_Kategorie, data = bee_wasp_abund_nest, method = "bonferroni")

#bee_wasp_abund_indiv
bee_wasp_abund_indiv <- bee_wasp_daten %>% pivot_longer(cols = c(bee_abund_indiv, wasp_abund_indiv),names_to = "Kategorie",values_to = "Wert")
bee_wasp_abund_indiv <- bee_wasp_abund_indiv %>% select(plot, flaechentyp.x, Kategorie, Wert)
bee_wasp_abund_indiv <- bee_wasp_abund_indiv %>% mutate(flaechentyp_Kategorie = paste(flaechentyp.x, Kategorie))
by(bee_wasp_abund_indiv$Wert, bee_wasp_abund_indiv$flaechentyp_Kategorie, shapiro.test) #nicht normalverteilt
leveneTest(Wert ~ flaechentyp_Kategorie, data = bee_wasp_abund_indiv) #nicht varianzhomogen
kruskal.test(Wert ~ flaechentyp_Kategorie, data = bee_wasp_abund_indiv) #signifikante Unterschiede
dunnTest(Wert ~ flaechentyp_Kategorie, data = bee_wasp_abund_indiv, method = "bonferroni")#signifikante Unterschiede

#bee_wasp_arten
bee_wasp_arten <- bee_wasp_daten %>% pivot_longer(cols = c(bee_arten, wasp_arten),names_to = "Kategorie",values_to = "Wert")
bee_wasp_arten <- bee_wasp_arten %>% select(plot, flaechentyp.x, Kategorie, Wert)
bee_wasp_arten <- bee_wasp_arten %>% mutate(flaechentyp_Kategorie = paste(flaechentyp.x, Kategorie))
by(bee_wasp_arten$Wert, bee_wasp_arten$flaechentyp_Kategorie, shapiro.test) #nicht normalverteilt
leveneTest(Wert ~ flaechentyp_Kategorie, data = bee_wasp_arten) #varianzhomogen
kruskal.test(Wert ~ flaechentyp_Kategorie, data = bee_wasp_arten) #signifikante Unterschiede
dunnTest(Wert ~ flaechentyp_Kategorie, data = bee_wasp_arten, method = "bonferroni")#signifikante Unterschiede

#bee_wasp_paras
bee_wasp_paras <- bee_wasp_daten %>% pivot_longer(cols = c(wasp_abund_paras, bee_abund_paras),names_to = "Kategorie",values_to = "Wert")
bee_wasp_paras <- bee_wasp_paras %>% select(plot, flaechentyp.x, Kategorie, Wert)
bee_wasp_paras <- bee_wasp_paras %>% mutate(flaechentyp_Kategorie = paste(flaechentyp.x, Kategorie))
by(bee_wasp_paras$Wert, bee_wasp_paras$flaechentyp_Kategorie, shapiro.test) #nicht normalverteilt
leveneTest(Wert ~ flaechentyp_Kategorie, data = bee_wasp_paras) #nicht varianzhomogen
kruskal.test(Wert ~ flaechentyp_Kategorie, data = bee_wasp_paras) #signifikante Unterschiede
dunnTest(Wert ~ flaechentyp_Kategorie, data = bee_wasp_paras, method = "bonferroni")

#### (6) Visualisierung ####
#Datenvorbereitung und -auswahl für barplots
plot_data_grouped <- rohdaten_2 %>% mutate(tiergruppe = case_when(Wespe..w...biene..b...spinne..s...diptera..d. == "w" ~ "Wespe",
                                                                  Wespe..w...biene..b...spinne..s...diptera..d. == "b" ~ "Biene",TRUE ~ NA_character_)) %>%
  filter(tiergruppe %in% c("Wespe", "Biene")) %>%group_by(plot, tiergruppe) %>%
  summarise(ges_abund_nest = n(),ges_abund_indiv = sum(n.host.individuals, na.rm = TRUE),ges_arten = n_distinct(host.species),.groups = "drop")

plot_data_grouped <- plot_data_grouped %>%  #bienen und wespen
  mutate (flaechentyp = case_when(str_detect(plot, "afs") ~ "AFS",str_detect(plot, "ref") ~ "REF",TRUE ~ NA_character_),
          standorttyp = case_when(str_detect(plot, "kw-") ~ "Klein-Winternheim",str_detect(plot, "wi-") ~ "Wiesbaden",str_detect(plot, "wit-") ~ "Wittlich",str_detect(plot, "bv-") ~ "Bad Vilbel",TRUE ~ NA_character_),
          standort_flaeche = paste0(standorttyp, " (", flaechentyp, ")"))

antag_data_grouped <- rohdaten_2 %>% group_by(plot) %>%  #parasiten
  summarise(ges_abund_indiv = sum(n.enemy.individuals.1, n.enemy.individuals.2, na.rm = TRUE),
            ges_abund_nest = sum(!is.na(enemy.species.1) & trimws(enemy.species.1) != "", !is.na(enemy.species.2) & trimws(enemy.species.2) != ""),
            ges_arten = n_distinct(enemy.species.1, enemy.species.2))%>%  
  mutate(flaechentyp = case_when(str_detect(plot, "afs") ~ "AFS",str_detect(plot, "ref") ~ "REF"),
         strukturtyp = case_when(str_detect(plot, "br") ~ "br",str_detect(plot, "acker") ~ "acker"),
         standorttyp = case_when(str_detect(plot, "kw-") ~ "Klein-Winternheim",str_detect(plot, "wi-") ~ "Wiesbaden",str_detect(plot, "wit-") ~ "Wittlich",str_detect(plot, "bv-") ~ "Bad Vilbel",TRUE ~ NA_character_),
         standort_flaeche = paste0(standorttyp, " (", flaechentyp, ")"))

#median sowie unteres(q1) und oberes(q3) Quartil berechnen
se_nach_tiergruppe <- plot_data_grouped %>%  #bienen und wespen
  group_by(tiergruppe, standort_flaeche) %>% summarise(
    n_nest = sum(!is.na(ges_abund_nest)), median_nest = median(ges_abund_nest, na.rm = TRUE), 
    q1_nest = quantile(ges_abund_nest, 0.25, na.rm = TRUE),
    q3_nest = quantile(ges_abund_nest, 0.75, na.rm = TRUE),#für Nestabundanz
    n_indiv = sum(!is.na(ges_abund_indiv)), median_indiv = median(ges_abund_indiv, na.rm = TRUE),
    q1_indiv = quantile(ges_abund_indiv, 0.25, na.rm = TRUE),
    q3_indiv = quantile(ges_abund_indiv, 0.75, na.rm = TRUE),#für individuenabundanz
    n_arten = sum(!is.na(ges_arten)), median_arten = median(ges_arten, na.rm = TRUE),
    q1_arten = quantile(ges_arten, 0.25, na.rm = TRUE),
    q3_arten = quantile(ges_arten, 0.75, na.rm = TRUE)) #für Artenzhal

se_antag <- antag_data_grouped %>% group_by(standort_flaeche) %>% summarise( #parasiten
    n_nest = sum(!is.na(ges_abund_nest)),median_nest = median(ges_abund_nest, na.rm = TRUE),
    q1_nest = quantile(ges_abund_nest, 0.25, na.rm = TRUE),
    q3_nest = quantile(ges_abund_nest, 0.75, na.rm = TRUE),#für Nestabundanz
    n_indiv = sum(!is.na(ges_abund_indiv)),median_indiv = median(ges_abund_indiv, na.rm = TRUE),
    q1_indiv = quantile(ges_abund_indiv, 0.25, na.rm = TRUE),
    q3_indiv = quantile(ges_abund_indiv, 0.75, na.rm = TRUE),#für individuenabundanz
    n_arten = sum(!is.na(ges_arten)),median_arten = median(ges_arten, na.rm = TRUE),
    q1_arten = quantile(ges_arten, 0.25, na.rm = TRUE),
    q3_arten = quantile(ges_arten, 0.75, na.rm = TRUE)) #für Artenzhal

#median und quantile jeweils in eine spalte
se_nach_tiergruppe_long <- se_nach_tiergruppe %>% pivot_longer( #bienen und wespen
    cols = -c(tiergruppe, standort_flaeche),names_to = c(".value", "variable"),names_pattern = "(.*)_(.*)")

se_bienen <- se_nach_tiergruppe_long %>% filter(tiergruppe == "Biene") #nur bienen

se_wespen <- se_nach_tiergruppe_long %>% filter(tiergruppe == "Wespe") #nur wespen
se_wespen <- se_nach_tiergruppe_long %>% filter(tiergruppe == "Wespe")
se_wespen_zusatz <- data.frame(tiergruppe = c("Wespe","Wespe","Wespe","Wespe","Wespe","Wespe"),
                               standort_flaeche = c("Bad Vilbel (AFS)","Bad Vilbel (AFS)","Bad Vilbel (AFS)","Bad Vilbel (REF)","Bad Vilbel (REF)","Bad Vilbel (REF)"),
                               variable = c("nest","indiv","arten","nest","indiv","arten"),
                               n = 0, median = 0, q1 = 0, q3 = 0)
se_wespen_2 <- bind_rows(se_wespen,se_wespen_zusatz)

se_antag_long <- se_antag %>% pivot_longer( #parasiten
    cols = -standort_flaeche,names_to = c(".value", "variable"),names_pattern = "(.*)_(.*)" )

#Piktogramme hochladen
pik_biene <- readPNG("pictogramm_biene.png")
pik_biene <- rasterGrob(pik_biene, interpolate = TRUE)
pik_wespe <- readPNG("pictogramm_wespe.png")
pik_wespe <- rasterGrob(pik_wespe, interpolate = TRUE)
pik_parasit <- readPNG("pictogramm_antagonist.png")
pik_parasit <- rasterGrob(pik_parasit, interpolate = TRUE)

#barplot mit bienendaten
ggplot(se_bienen, aes(x = standort_flaeche, y = median, fill = variable)) + geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  geom_errorbar(aes(ymin = q1, ymax = q3),position = position_dodge(width = 0.7),width = 0.2,color = "black") +
  geom_text(aes(label = round(median, 1)),position = position_dodge(width = 0.7),color = "white",vjust = 1.1, size = 2.5) +
  scale_fill_manual(values = c("arten" = "grey12","indiv" = "grey35","nest" = "grey70"),labels = c("Artenreichtum", "Individuenabundanz", "Nestabundanz"),
                    name = "Variablen") + 
  scale_y_continuous(limits = c(0, 65),breaks = seq(0, 65, 5)) +
  labs(x = "Flächen",y = "median mit IQR",title = "Artenreichtum, Individuenabundanz und Nestabundanz der Wildbienen") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.major.x = element_blank()) +
  geom_vline(xintercept = 2.5, linetype = "dotted", size = 0.2)+geom_vline(xintercept = 4.5, linetype = "dotted", size = 0.2)+
  geom_vline(xintercept = 6.5, linetype = "dotted", size = 0.2)+ annotation_custom(pik_biene, xmin = 7.3, xmax = 9, ymin = 60, ymax = 67)+
  annotate("text", x=3, y=65, label="a") + annotate("text", x=4, y=37, label="b")

#barplot mit wespendaten
ggplot(se_wespen_2, aes(x = standort_flaeche, y = median, fill = variable)) + geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  geom_errorbar(aes(ymin = q1, ymax = q3),position = position_dodge(width = 0.7),width = 0.2,color = "black") +
  geom_text(aes(label = round(median, 1)),position = position_dodge(width = 0.7),color = "white",vjust = 1.1, size = 2.5) +
  scale_fill_manual(values = c("arten" = "grey12","indiv" = "grey35","nest" = "grey70"),labels = c("Artenreichtum", "Individuenabundanz", "Nestabundanz"),
                    name = "Variablen") + 
  scale_y_continuous(limits = c(0, 65),breaks = seq(0, 65, 5)) +
  labs(x = "Flächen",y = "median mit IQR",title = "Artenreichtum, Individuenabundanz und Nestabundanz der Wespen") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.major.x = element_blank()) +
  geom_vline(xintercept = 2.5, linetype = "dotted", size = 0.2)+geom_vline(xintercept = 4.5, linetype = "dotted", size = 0.2)+
  geom_vline(xintercept = 6.5, linetype = "dotted", size = 0.2)+ annotation_custom(pik_wespe, xmin = 7.3, xmax = 9, ymin = 60, ymax = 67)+
  annotate("text", x=6.75, y=6, label="a") + annotate("text", x=7.75, y=5, label="b")

#barplot mit parasitendaten
ggplot(se_antag_long, aes(x = standort_flaeche, y = median, fill = variable)) + geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  geom_errorbar(aes(ymin = q1, ymax = q3),position = position_dodge(width = 0.7),width = 0.2,color = "black") +
  geom_text(aes(label = round(median, 1)),position = position_dodge(width = 0.7),color = "white",vjust = 1.1, size = 2.5) +
  scale_fill_manual(values = c("arten" = "grey12","indiv" = "grey35","nest" = "grey70"),labels = c("Artenreichtum", "Individuenabundanz", "Nestabundanz"),
                    name = "Variablen") + 
  scale_y_continuous(limits = c(-0.8, 65),breaks = seq(0, 65, 5)) +
  labs(x = "Flächen",y = "median mit IQR",title = "Artenreichtum, Individuenabundanz und Nestabundanz der Gegenspieler") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.major.x = element_blank())+
  annotate("text", x=7, y=-0.225, label="0.5", size = 2.5)+annotate("text", x=7.235, y=-0.225, label="0.5", size = 2.5)+
  annotate("text", x=8, y=-0.65, label="0", size = 2.5)+annotate("text", x=8.235, y=-0.65, label="0", size = 2.5)+
  annotate("text", x=2, y=-0.65, label="0", size = 2.5)+annotate("text", x=2.235, y=-0.65, label="0", size = 2.5) +
  geom_vline(xintercept = 2.5, linetype = "dotted", size = 0.2)+geom_vline(xintercept = 4.5, linetype = "dotted", size = 0.2)+
  geom_vline(xintercept = 6.5, linetype = "dotted", size = 0.2)+ annotation_custom(pik_parasit, xmin = 7.3, xmax = 9, ymin = 60, ymax = 67)
  

