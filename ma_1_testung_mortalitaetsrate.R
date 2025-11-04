#### SKRIPT 1: TESTUNG DER MORTALITAETSRATE ####
#Mortalität wird zwischen den Standorten und Flächen miteinander verglichen (mittels signifikanzanalyse der Nestabundanz) 
#um zu überprüfen, wie die Mortalitätsrate über den Datensatz verteilt ist.

#erforderliche Pakete in skript laden
library(tidyverse)
library(ggplot2)
library(vegan)
library(car)
library(FSA)
# Daten aus CSV-Datei einlesen
rohdaten_1 <- read.csv2("C:/Users/p-ber/OneDrive/1_TH Bingen/MA und FORSCH/1_auswertung/tab_nisthilfen_20250903.csv", header = T)
rohdaten_2 <- rohdaten_1 %>% filter(host.family!="Arachnidae", bemerkung!="angefangene BZ", n.gefressen.geschluepft==0)
str(rohdaten_2) #Datentyp passt

# Darstellung der Mortalitätsrate
count_mortality <- rohdaten_2 %>% count(mortality)
plot_mortality <- barplot(count_mortality$n,names.arg = count_mortality$x,col = c("steelblue", "darkorange"),main = "Mortalitätsrate",ylab = "Anzahl",
        xlab = "alife / dead",ylim = c(0, max(count_mortality$n) * 1.2),)
text(x = plot_mortality, y = count_mortality$n, labels = count_mortality$n, pos = 3)
table(count_mortality)

# Testen ob die Mortaliätsrate mit umweltvariablen und Zielgrößen zusammenhängen
rohdaten_3 <- rohdaten_2 %>% mutate(anteil_dead_broodcells = case_when(mortality == "alife" ~ n.unknown.mortality / n.brood.cells,
                                                                       mortality == "dead"  ~ 1,TRUE ~ NA_real_))

filter_a <- rohdaten_3 %>% group_by(plot) %>% 
  summarise(mean_dead_broodcells = mean(anteil_dead_broodcells, na.rm = TRUE))%>%  
  mutate(flaechentyp = case_when(str_detect(plot, "afs") ~ "AFS",str_detect(plot, "ref") ~ "REF"))%>%
  mutate(strukturtyp = case_when(str_detect(plot, "br") ~ "br",str_detect(plot, "acker") ~ "acker"))%>%
  mutate(standorttyp = case_when(str_detect(plot, "kw-") ~ "kw",str_detect(plot, "wi-") ~ "wi",str_detect(plot, "wit-") ~ "wit",str_detect(plot, "bv-") ~ "bv"))

# signifikanztest flaechentyp  
by(filter_a$mean_dead_broodcells, filter_a$flaechentyp, shapiro.test) #nicht normalverteilt
leveneTest(mean_dead_broodcells~ flaechentyp, data = filter_a) #nicht varianzhomogen
wilcox.test(mean_dead_broodcells ~ flaechentyp, data = filter_a) # p=0,902 kein signifikanter unterschied
ggplot(filter_a, aes(x = flaechentyp, y = mean_dead_broodcells)) + geom_boxplot() + theme_minimal() +
  labs(title = "Anteil dead nach Standorttyp", x = "Standorttyp", y = "Anteil dead")

#signifikanztest strukturtyp
by(filter_a$mean_dead_broodcells, filter_a$strukturtyp, shapiro.test) #nicht normalverteilt
leveneTest(mean_dead_broodcells~ strukturtyp, data = filter_a) #varianzhomogen
wilcox.test(mean_dead_broodcells ~ strukturtyp, data = filter_a) #0,5847 kein signifikanter Unterschied

#signifikanztest standorttyp
by(filter_a$mean_dead_broodcells, filter_a$standorttyp, shapiro.test) #nicht alles normalverteilt
leveneTest(mean_dead_broodcells~ standorttyp, data = filter_a) #Varianzhomogen
kruskal.test(mean_dead_broodcells ~ standorttyp, data = filter_a) #0,0034 signifikante unterschiede
dunnTest(mean_dead_broodcells ~ standorttyp, data = filter_a, method = "bonferroni") #signif. unterschiede zw. bv-wi

ggplot(filter_a, aes(x = standorttyp, y = mean_dead_broodcells)) + geom_boxplot(fill = "skyblue") +
  theme_minimal() + labs(title = "Anteil dead broodcells nach Standorttyp",x = "Standorttyp", y = "Anteil dead broodcells")

