#### SKRIPT 5: Geschlechterverhältnis, gegenspieler-Wirt-Interaktionen ####
#Esrtellung von Grafiken zu oben geannten Themen

#erforderliche Pakete in skript laden
library(tidyverse)  # Für Datenmanipulation und Visualisierung
library(ggplot2)    # Für erweiterte Grafiken
library(vegan)      # Für ökologische Analysen (NMDS, Diversitätsindizes)
library(car)
library(FSA)
library(dplyr)
library(patchwork)
library(readr)

# Daten aus CSV-Datei einlesen
rohdaten_1 <- read.csv2("C:/Users/p-ber/OneDrive/1_TH Bingen/MA und FORSCH/1_auswertung/tab_nisthilfen_20250903.csv", header = T)
rohdaten_2 <- rohdaten_1 %>% filter(host.family!="Arachnidae", bemerkung!="angefangene BZ", n.gefressen.geschluepft==0, !(n.host.individuals<=0 & n.attacked.brood.cells<=0))


#### Geschlechterverhältnis und Niströhrendurchmesser ####
#nur bienendaten auswählen
sex_ratio_1 <- rohdaten_2 %>% filter(Wespe..w...biene..b...spinne..s...diptera..d. == "b")
str(sex_ratio_1$cavity.diameter)

#individuenabundanz von männlichen/weiblichen Bienen un zusätzlicher Spalte extrahieren
males <- na.omit(sex_ratio_1[,c("cavity.diameter","n.male")])
males$sex <- "male"
males <- rename(males, abundanz =n.male)
females <- na.omit(sex_ratio_1[,c("cavity.diameter","n.female")])
females$sex <- "female"
females <- rename(females, abundanz = n.female)
sex_ratio_2 <- rbind(males, females)
sex_ratio_2$cavity.diameter <- as.numeric(sex_ratio_2$cavity.diameter)

#plot für Verhältnis zwischen m und w bienen
sex <- ggplot(sex_ratio_2, aes(x = cavity.diameter, y = abundanz, color = sex)) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    x = "Niströhrendurchmesser (mm)",
    y = "Individuenabundanz (n)",
    color = "Geschlecht",
    title = "(b) Geschlechterverhältnis und Niströhrendurchmesser"
  ) +
  theme(
    legend.position.inside = c(0.9, 0.2),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )
#  Daten für Gesamtblütendeckung einlesen
krautarten <- read.csv2("C:/Users/p-ber/OneDrive/1_TH Bingen/MA und FORSCH/1_auswertung/krautarten.csv", header = T)

#Datensatz aufbereiten
krautarten_a <- krautarten %>%mutate(flaechentyp = case_when(str_detect(flaechen_ID, "AFS") ~ "AFS", str_detect(flaechen_ID, "REF") ~ "REF"),
                                     standorttyp = case_when(str_detect(flaechen_ID, "KW-") ~ "kw",str_detect(flaechen_ID, "WI-") ~ "wi",str_detect(flaechen_ID, "WIT-") ~ "wit",str_detect(flaechen_ID, "BV-") ~ "bv"))

krautarten_b <- krautarten_a %>% mutate(standorttyp = factor(standorttyp),flaechentyp = factor(flaechentyp)) %>%group_by(flaechentyp, standorttyp) %>%
  summarise(standort_flaeche = paste0(unique(standorttyp), "(", unique(flaechentyp), ")"),krautdeckung = sum(deckung, na.rm = TRUE),
            kraut_gbd_median = median(gesamtbluetendeckung, na.rm = TRUE),kraut_artenzahl = n_distinct(art),.groups = "drop")

filter_k2 <- filter_k %>%  group_by(standort_flaeche) %>% 
  summarise(bee_female = mean( bee_female, na.rm = TRUE),bee_female_anteil = mean(bee_female_anteil, na.rm = TRUE))

#veg.daten und bienendaten aggregieren
filter_join <- krautarten_b %>% left_join(filter_k2, by = "standort_flaeche")

#voreinstellungen für plot-darstellung
filter_join$standort_flaeche <- as.character(filter_join$standort_flaeche)
filter_join$label_kat <- dplyr::recode(filter_join$standort_flaeche,"bv(AFS)" = "AFS \nBad Vilbel","bv(REF)" = "REF \nBad Vilbel",
                                       "wi(AFS)" = "AFS \nWiesbaden","wi(REF)" = "REF \nWiesbaden","kw(AFS)" = "AFS \nKlein-Winternheim",
                                "kw(REF)" = "REF \nKlein-Winternheim","wit(AFS)" = "AFS \nWittlich","wit(REF)" = "REF \nWittlich")

color_kat <- c("bv(AFS)"="yellow", "bv(REF)"="yellow","wi(AFS)"="steelblue3","wi(REF)"="steelblue3",
               "kw(AFS)"="orangered3", "kw(REF)"="orangered3","wit(AFS)"="chartreuse2", "wit(REF)"="chartreuse2")

shape_kat <- c("bv(AFS)"=25,"bv(REF)"=21,"wi(AFS)"=25,"wi(REF)"=21,"kw(AFS)"=25,"kw(REF)"=21,"wit(AFS)"=25,"wit(REF)"=21)

sex_2 <- ggplot(filter_join, aes(x = kraut_gbd_median, y = bee_female_anteil)) + geom_point(aes(shape = standort_flaeche, fill = standort_flaeche), size = 3, color = "black") +
  scale_shape_manual(values = shape_kat) +scale_fill_manual(values = color_kat) +
  scale_y_continuous(limits = c(0.15, 0.8),breaks = seq(0.15, 0.8, 0.1))+
  scale_x_continuous(limits = c(-0.5, 3.5),breaks = seq(-0.5, 3.5, 0.5))+
  theme_minimal() + labs(x = "Gesamtblütendeckung", y = "durchschnittlicher Anteil weiblicher Wildbienen (n)", title = "(a) Anteil weiblicher Wildbienen und Gesamtblütendeckung") +
  theme(legend.position = "none")+ geom_segment(aes(x = 0, y = 0.31, xend = 0.9, yend = 0.39),arrow = arrow(length = unit(0.2, "cm")),color = "orangered3")+
  geom_segment(aes(x = 1.45, y = 0.32, xend = 1.1, yend = 0.29),arrow = arrow(length = unit(0.2, "cm")),color = "yellow2")+
  geom_segment(aes(x = 1.1, y = 0.23, xend = 1.9, yend = 0.38),arrow = arrow(length = unit(0.2, "cm")),color = "steelblue3")+
  geom_segment(aes(x = 1.1, y = 0.73, xend = 2.9, yend = 0.46),arrow = arrow(length = unit(0.2, "cm")),color = "chartreuse2")+
  geom_text(aes(label = label_kat), vjust = -0.5, size = 3)

sex_2 + sex


#### Wirt-Gegenspieler-Interaktionen ####

filter_n <- rohdaten_2 %>% group_by(plot) %>% 
  summarise(ges_abund_indiv = sum(n.host.individuals, na.rm = TRUE), 
            ges_abund_nest = n(), 
            ges_abund_paras_nest = sum(!is.na(enemy.species.1) & trimws(enemy.species.1) != "", !is.na(enemy.species.2) & trimws(enemy.species.2) != ""),
            ges_abund_paras_indiv = sum(n.enemy.individuals.1, na.rm = TRUE),
            ges_abund_wasp_nest = sum(Wespe..w...biene..b...spinne..s...diptera..d. == "w"),
            ges_abund_bee_nest = sum(Wespe..w...biene..b...spinne..s...diptera..d. == "b"),
            ges_arten = n_distinct(host.species),
            ges_arten_paras = n_distinct(enemy.species.1, enemy.species.2))%>%
  mutate(flaechentyp = case_when(str_detect(plot, "afs") ~ "AFS",str_detect(plot, "ref") ~ "REF"))%>%
  mutate(strukturtyp = case_when(str_detect(plot, "br") ~ "br",str_detect(plot, "acker") ~ "acker"))%>%
  mutate(standorttyp = case_when(str_detect(plot, "kw-") ~ "kw",str_detect(plot, "wi-") ~ "wi",str_detect(plot, "wit-") ~ "wit",str_detect(plot, "bv-") ~ "bv"))

#### abundanz-korrelation zwischen hosts und antagonists ####
plot_abund_1 <- ggplot(filter_n, aes(x = ges_abund_paras_nest,y = ges_abund_nest,color = flaechentyp, shape = flaechentyp)) +
  geom_jitter(width = 0.1, height = 0.1, size = 2) + geom_smooth(method = "lm") + scale_color_manual(values = c("AFS" = "hotpink3","REF" = "tan1")) +
  labs( title = "(a) Nestabundanz von Gegenspielern und Wirten ",x = "Nestabundanz der Gegenspieler (n)",y = "Nestabundanz der Wespen und Wildbienen (n)") +
  theme_minimal()+ theme(legend.position = "none")

#### artenreichtum-korrelation zwischen hosts und antagonists ####
plot_arten_1 <- ggplot(filter_n, aes(x=ges_arten_paras, y=ges_arten,color = flaechentyp, shape = flaechentyp))+
  geom_jitter(width = 0.1, height = 0.1, size = 2)+ geom_smooth(method= "lm")+ scale_color_manual(values = c("AFS" = "hotpink3","REF" = "tan1"), name = "Flächentyp")+
  labs(title = "(b) Artenreichtum von Gegenspielern und Wirten", x = "Artenreichtum der Gegenspieler (n)", y = "Artenreichtum der Wespen und Wildbienen (n)") +
  theme_minimal() + guides(shape ="none")

plot_abund_1 + plot_arten_1


ggplot(filter_n, aes(x = flaechentyp, y = ges_abund_paras_nest)) + geom_boxplot() + theme_minimal() +
  labs(title = "nestabundanz antagonisten", x = "flaechentyp", y = "ges_arten"