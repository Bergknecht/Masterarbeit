#### SKRIPT 3: ARTENVERTEILUNG ####
#Überprüfung der Artenverteilung über den gesamten Datensatz mittels NMDS und Prüfung von Korrelation 
#mit lokalen und landschaftlichen Parametern

#erforderliche Pakete in skript laden
library(tidyverse) 
library(ggplot2)    
library(vegan)      
library(car)

# Daten aus CSV-Datei einlesen
rohdaten_1 <- read.csv2("C:/Users/p-ber/OneDrive/1_TH Bingen/MA und FORSCH/1_auswertung/tab_nisthilfen_20250903.csv", header = T)
# alle Nester mit 100% Mortalität entfernen
rohdaten_4 <- rohdaten_1 %>% filter(host.family!="Arachnidae", bemerkung!="angefangene BZ", n.gefressen.geschluepft==0, n.host.individuals!=0)

#### (1) Datenvorbereitung ####
filter_d <- rohdaten_4 %>%
  mutate(flaechentyp = case_when(str_detect(plot, "afs") ~ "AFS",str_detect(plot, "ref") ~ "REF"),
         strukturtyp = case_when(str_detect(plot, "br") ~ "br",str_detect(plot, "acker") ~ "acker"),
         standorttyp = case_when(str_detect(plot, "kw-") ~ "kw",str_detect(plot, "wi-") ~ "wi",str_detect(plot, "wit-") ~ "wit",str_detect(plot, "bv-") ~ "bv")) %>%
  group_by(plot, standorttyp, flaechentyp, host.species) %>%summarize(Abundanz = sum(n.host.individuals, na.rm = TRUE),.groups = 'drop') %>%
  mutate(standort_flaeche = paste0(standorttyp, "(", flaechentyp, ")")) %>%
  relocate(standort_flaeche, .before = 3)

# Pflanzendaten und Landschaftsparameter einlesen und aufbereiten
krautarten <- read.csv2("C:/Users/p-ber/OneDrive/1_TH Bingen/MA und FORSCH/1_auswertung/krautarten.csv", header = T)
gehoelzarten <- read.csv2("C:/Users/p-ber/OneDrive/1_TH Bingen/MA und FORSCH/1_auswertung/gehoelzarten.csv", header = T)
umgebung <- read.csv("C:/Users/p-ber/OneDrive/1_TH Bingen/MA und FORSCH/1_auswertung/umgebungsstrukturen/25_09_23_radien_parameter.csv", header = T) #von Winzer (2025)
flaechenanteile <- read.csv2("C:/Users/p-ber/OneDrive/1_TH Bingen/MA und FORSCH/1_auswertung/umgebungsstrukturen/25_09_17_flaechenanteile_radien.csv", header = T) #von Winzer (2025)

#  aggregieren
krautarten_a <- krautarten %>%
  mutate(flaechentyp = case_when(str_detect(flaechen_ID, "AFS") ~ "AFS", str_detect(flaechen_ID, "REF") ~ "REF"),
         standorttyp = case_when(str_detect(flaechen_ID, "KW-") ~ "kw",str_detect(flaechen_ID, "WI-") 
                                 ~ "wi",str_detect(flaechen_ID, "WIT-") ~ "wit",str_detect(flaechen_ID, "BV-") ~ "bv"))
krautarten_b <- krautarten_a %>% 
  mutate(standorttyp = factor(standorttyp),flaechentyp = factor(flaechentyp)) %>%
  group_by(flaechentyp, standorttyp) %>%
  summarise(standort_flaeche = paste0(unique(standorttyp), "(", unique(flaechentyp), ")"),
            krautdeckung = sum(deckung, na.rm = TRUE),
            kraut_gbd_median = median(gesamtbluetendeckung, na.rm = TRUE),
            kraut_artenzahl = n_distinct(art),.groups = "drop")

gehoelzarten_a <- gehoelzarten %>%
  mutate(flaechentyp = case_when(str_detect(flaechen_ID, "afs") ~ "AFS", str_detect(flaechen_ID, "ref") ~ "REF"),
         standorttyp = case_when(str_detect(flaechen_ID, "kw-") ~ "kw",str_detect(flaechen_ID, "wi-") 
                                 ~ "wi",str_detect(flaechen_ID, "wit-") ~ "wit",str_detect(flaechen_ID, "bv-") ~ "bv"))
gehoelzarten_b <- gehoelzarten_a %>%
  mutate(standorttyp = factor(standorttyp),flaechentyp = factor(flaechentyp)) %>% 
  group_by(flaechentyp, standorttyp) %>%
  summarise(standort_flaeche = paste0(unique(standorttyp), "(", unique(flaechentyp), ")"),
    gehoelz_gesamtanzahl = sum(anzahl, na.rm = TRUE),
            gehoelz_artenzahl = n_distinct(art),.groups = "drop")

umgebung_a <- umgebung %>%
  mutate(distanz = case_when(str_detect(id, "250") ~ "250", str_detect(id, "1000") ~ "1000"),
         flaechentyp = case_when( str_detect(id, "afs") ~ "AFS",str_detect(id, "ref") ~ "REF"),
         standorttyp = case_when(str_detect(id, "kw_") ~ "kw",str_detect(id, "wi_") ~ "wi",str_detect(id, "wt_") ~ "wit",str_detect(id, "bv_") ~ "bv"),
         flaechentyp = factor(flaechentyp),standorttyp = factor(standorttyp),
         standort_flaeche = paste0(standorttyp, "(", flaechentyp, ")"))

umgebung_b <- umgebung_a %>%pivot_wider(id_cols = c(standorttyp, flaechentyp, standort_flaeche),names_from = distanz,
                                    values_from = c(shannon_oberkat, shannon_codierung, shannon_vergleich_alkis, anteil_snh, anteil_flower_rich, 
                                                    anteil_wb_wichtig, anteil_wb_wichtig_ohne_dgl,anteil_wb_allg, anteil_wb_alles, anteil_ground_nesting, 
                                                    anteil_stem_nesting, mean_count_per_km2, mean_size_ha_lw),values_fill = 0, values_fn = sum) 

flaechenanteile_a <- flaechenanteile %>%
  mutate(distanz = case_when(str_detect(id, "250") ~ "250", str_detect(id, "1000") ~ "1000"),
         flaechentyp = case_when( str_detect(id, "afs") ~ "AFS",str_detect(id, "ref") ~ "REF"),
         standorttyp = case_when(str_detect(id, "kw_") ~ "kw",str_detect(id, "wi_") ~ "wi",str_detect(id, "wt_") ~ "wit",str_detect(id, "bv_") ~ "bv"),
         flaechentyp = factor(flaechentyp),standorttyp = factor(standorttyp),
         standort_flaeche = paste0(standorttyp, "(", flaechentyp, ")"))
flaechenanteile_a <- flaechenanteile_a %>%
  mutate(across(c(acker, acker_blueh, ackerbrache, 
                  afs, erdwege_offenboden, gehoelze, siedlung, sonstige, staudenflur, 
                  streuobst, garten_obst, gruenland_int, urban_green, wasser, wein, 
                  wiese_ext, wald, weide_ext),
                ~ as.numeric(as.character(.))))

flaechenanteile_b <- flaechenanteile_a %>% pivot_wider(id_cols = c(standorttyp, flaechentyp, standort_flaeche),names_from = distanz,
                                        values_from = c(acker, acker_blueh, ackerbrache, 
                                                        afs, erdwege_offenboden, gehoelze, siedlung, sonstige, staudenflur, 
                                                        streuobst, garten_obst, gruenland_int, urban_green, wasser, wein, 
                                                        wiese_ext, wald, weide_ext),values_fill = 0, values_fn = sum) 

# Daten zusammenführen, formatieren
filter_d <- filter_d %>%
  mutate(Abundanz = as.numeric(Abundanz))
filter_o <- filter_d %>%pivot_wider(id_cols = c(standorttyp, flaechentyp, standort_flaeche),names_from = host.species,
                                    values_from = Abundanz,values_fill = 0, values_fn = sum) %>%
  left_join(krautarten_b %>%select(standort_flaeche, krautdeckung, kraut_gbd_median, kraut_artenzahl),by = "standort_flaeche") %>%
  left_join(gehoelzarten_b %>%select(standort_flaeche, gehoelz_gesamtanzahl, gehoelz_artenzahl),by = "standort_flaeche") %>%
  left_join(umgebung_b %>% select(standort_flaeche,shannon_oberkat_250,shannon_oberkat_1000,
                                  shannon_codierung_250,shannon_codierung_1000,shannon_vergleich_alkis_250,shannon_vergleich_alkis_1000,
                                  anteil_snh_250,anteil_snh_1000, anteil_flower_rich_250,anteil_flower_rich_1000, anteil_wb_wichtig_250,
                                  anteil_wb_wichtig_1000,anteil_wb_wichtig_ohne_dgl_250,anteil_wb_wichtig_ohne_dgl_1000, anteil_wb_allg_250,
                                  anteil_wb_allg_1000, anteil_wb_alles_250,anteil_wb_alles_1000,anteil_ground_nesting_250,anteil_ground_nesting_1000,
                                  anteil_stem_nesting_250, anteil_stem_nesting_1000,mean_count_per_km2_250,mean_count_per_km2_1000,mean_size_ha_lw_250,
                                  mean_size_ha_lw_1000), by ="standort_flaeche") %>%
  left_join(flaechenanteile_b %>%select(standort_flaeche, acker_1000, acker_blueh_1000, ackerbrache_1000, afs_1000, erdwege_offenboden_1000, gehoelze_1000, 
                                        siedlung_1000, sonstige_1000, staudenflur_1000, streuobst_1000, garten_obst_1000, gruenland_int_1000, 
                                        urban_green_1000, wasser_1000, wein_1000, wiese_ext_1000, wald_1000, weide_ext_1000,
                                        acker_250, acker_blueh_250, ackerbrache_250, afs_250, erdwege_offenboden_250, gehoelze_250, 
                                        siedlung_250, sonstige_250, staudenflur_250, streuobst_250, garten_obst_250, gruenland_int_250, 
                                        urban_green_250, wasser_250, wein_250, wiese_ext_250, wald_250, weide_ext_250),by ="standort_flaeche")

data.frame(Spaltennummer = seq_along(names(umwelt_daten)),Spaltenname = names(umwelt_daten))

#Datenpakete erstellen
art_daten <- filter_o[, c(4:27)] #Artendaten (nur Abundanzen)
umwelt_daten <- filter_o[,c(28:94)] #alle Umweltvariablen
umwelt_daten[is.na(umwelt_daten)] <-0

standort_daten <- filter_o [,c(1:3)] #Standort- und flächeninfo
umwelt_daten_kraut <- umwelt_daten[,c(1:3)] #Pflanzendaten, krautige Pflanzen
umwelt_daten_gehoelz <- umwelt_daten [,c(4:5)] #Pflanzendaten, gehölze
umwelt_daten_gehoelz[is.na(umwelt_daten_gehoelz)] <-0
umwelt_daten_umgebung <- umwelt_daten [,c(6:31)] #landschaftsparameter umgebung
umwelt_daten_flaechenanteile <- umwelt_daten[,c(32:67)] #landschaftsparameter flaechenanteile

# NMDS berechnen (Bray-Curtis-Distanz, 1-5, max. 100 Iterationen)
nmds1 <- metaMDS(art_daten, distance = "bray", k = 1, trymax = 100)
print(nmds1) #0,165 ungenau
stressplot(nmds1)
nmds2 <- metaMDS(art_daten, distance = "bray", k = 2, trymax = 100)
print(nmds2) #0,052 gut
stressplot(nmds2)

#### (2) Signifikanztests ####
# zusammenhang zwischen NMDS und Umweltvariablen testen

envfit_kraut <- envfit(nmds2, umwelt_daten_kraut, permutations = 999)
summary(envfit_kraut)
envfit_kraut$vectors$pvals #keine signifikante Beziehung

envfit_gehoelz <- envfit(nmds2, umwelt_daten_gehoelz, permutations = 999)
summary(envfit_gehoelz)
envfit_gehoelz$vectors$pvals #keine signifikante Beziehung


envfit_standort <- envfit(nmds2, standort_daten, permutations = 999)
summary(envfit_standort)
envfit_standort$factors$pvals #Standort hat signifikanten (p=0,011) Einfluss

envfit_umgebung <- envfit(nmds2, umwelt_daten_umgebung, permutations = 999)
summary(envfit_umgebung)
envfit_umgebung$vectors$pvals# (p=0,034)anteil_wb_wichtig_ohne_dgl_1000, (p=0,001)shannon_vergleich_alkis_1000, (0,008)shannon_oberkat_1000

#für NMDS-plot nur bestimmte auswählen
selected_vars <- c("anteil_wb_wichtig_ohne_dgl_1000","shannon_oberkat_1000") # die beiden shannon-paramter sagen das gleiche aus, daher hier nur eins gewählt
envfit_umgebung_subset <- envfit_umgebung
envfit_umgebung_subset$vectors$arrows <- envfit_umgebung$vectors$arrows[selected_vars, , drop = FALSE]
envfit_umgebung_subset$vectors$r <- envfit_umgebung$vectors$r[selected_vars]
envfit_umgebung_subset$vectors$pvals <- envfit_umgebung$vectors$pvals[selected_vars]
new_names <- c("Wildbienen-Habitate", "Landschaftsdiversität")
rownames(envfit_umgebung_subset$vectors$arrows) <- new_names
names(envfit_umgebung_subset$vectors$r) <- new_names
names(envfit_umgebung_subset$vectors$pvals) <- new_names

envfit_flaechenanteile <- envfit(nmds2, umwelt_daten_flaechenanteile, permutations = 999)
summary(envfit_flaechenanteile)
envfit_flaechenanteile$vectors$pvals#unten      (p=0,032)acker_1000, (p=0,044)garten_obst_1000, (p=0,046)acker_250
                                    #oben       (p=0,001)gruenland_int_1000, (p=0,035)wiese_ext_250, (p=0,049)gruenland_int_250, (p=0,019)wald_250
                                    #links      (p=0,003)streuobst_250, (p=0,015)afs_1000, (p=0,017)afs_250,(p=0,013)urban_green_1000, (p=0,031)wasser_1000, 
                                    #obenlinks  (p=0,011)acker_blueh_250 # ist ziemlich ähnlich mit den Shannonindex-parametern

#für NMDS-plot nur bestimmte auswählen
selected_vars_fl <- c("acker_1000", "wald_250", "afs_250")
envfit_flaechenanteile_subset <- envfit_flaechenanteile
envfit_flaechenanteile_subset$vectors$arrows <- envfit_flaechenanteile$vectors$arrows[selected_vars_fl, , drop = FALSE]
envfit_flaechenanteile_subset$vectors$r <- envfit_flaechenanteile$vectors$r[selected_vars_fl]
envfit_flaechenanteile_subset$vectors$pvals <- envfit_flaechenanteile$vectors$pvals[selected_vars_fl]
new_names <- c("Acker", "Grünland/Wald", "Gehölze/Gewässer/AFS")
rownames(envfit_flaechenanteile_subset$vectors$arrows) <- new_names
names(envfit_flaechenanteile_subset$vectors$r) <- new_names
names(envfit_flaechenanteile_subset$vectors$pvals) <- new_names

#### (3) NMDS visualisieren ####
#Farben und Formen für Darstellung auswählen
color_kat <- c("bv(AFS)"="yellow", "bv(REF)"="yellow","wi(AFS)"="steelblue3","wi(REF)"="steelblue3",
               "kw(AFS)"="orangered3", "kw(REF)"="orangered3","wit(AFS)"="chartreuse2", "wit(REF)"="chartreuse2")
shape_kat <- c("bv(AFS)"=25,"bv(REF)"=21,"wi(AFS)"=25,"wi(REF)"=21,"kw(AFS)"=25,"kw(REF)"=21,"wit(AFS)"=25,"wit(REF)"=21)

#überlappende Species-Punkte etwas auseinanderrücken
species_coords <- scores(nmds2, display = "species")
jittered_coords <- species_coords
jittered_coords[,1] <- jitter(species_coords[,1], amount = 0.03)
jittered_coords[,2] <- jitter(species_coords[,2], amount = 0.03)

# NMDS-Plot mit zusätzlichen Informationen
plot(nmds2, type = "n", main="Artenverteilung über die Agroforst- und Referenzflächen")  # Leeres Diagramm
points(nmds2, display = "sites", pch = shape_kat, bg = color_kat, cex=2.5)  # Standorte als Punkte
points(jittered_coords, pch = 20, cex = 1.5)  # Arten als Punkte
plot(envfit_umgebung_subset, col = "red") #umgebungsparameter
plot(envfit_flaechenanteile_subset, col = "red") #flaechenparameter
legend(1.5,1.5, legend = c("AFS Wiesbaden", "REF Wiesbaden", "AFS Wittlich", "REF Wittlich","AFS Klein-Winternheim", "REF Klein-Winternheim", "AFS Bad Vilbel", "REF Bad Vilbel"), pch = shape_kat, 
       pt.bg = c("steelblue2","steelblue2","chartreuse2","chartreuse2","orangered3","orangered3","yellow","yellow"), cex = 0.8)


#### (4) Artenverteilung in barplots ####
#Daten vorbereiten
#alle host.species
rohdaten_host <- rohdaten_1 %>% filter(host.family!="Arachnidae", bemerkung!="angefangene BZ", n.host.individuals!=0)%>%
  mutate(host.species.2 = paste0(host.species, " (", Wespe..w...biene..b...spinne..s...diptera..d., ")"))

Artenverteilung_c <- rohdaten_host %>%
  mutate(flaechentyp = case_when(str_detect(plot, "afs") ~ "AFS",str_detect(plot, "ref") ~ "REF"),
         strukturtyp = case_when(str_detect(plot, "br") ~ "br",str_detect(plot, "acker") ~ "acker"),
         standorttyp = case_when(str_detect(plot, "kw-") ~ "kw",str_detect(plot, "wi-") ~ "wi",str_detect(plot, "wit-") ~ "wit",str_detect(plot, "bv-") ~ "bv")) %>%
  group_by(plot, standorttyp, flaechentyp, host.species.2) %>%summarize(Abundanz = n(),.groups = 'drop') %>%
  mutate(standort_flaeche = paste0(standorttyp, "(", flaechentyp, ")")) %>%
  relocate(standort_flaeche, .before = 3)

#alle parasiten aus spalte enemy.species.1
rohdaten_enemy_1 <- rohdaten_1 %>% filter(host.family!="Arachnidae", bemerkung!="angefangene BZ", n.enemy.individuals.1!=0)%>%
  mutate(enemy.species.1.2 = paste0(enemy.species.1, " (g)"))

artenverteilung_d <- rohdaten_enemy_1 %>%
  mutate(flaechentyp = case_when(str_detect(plot, "afs") ~ "AFS",str_detect(plot, "ref") ~ "REF"),
         strukturtyp = case_when(str_detect(plot, "br") ~ "br",str_detect(plot, "acker") ~ "acker"),
         standorttyp = case_when(str_detect(plot, "kw-") ~ "kw",str_detect(plot, "wi-") ~ "wi",str_detect(plot, "wit-") ~ "wit",str_detect(plot, "bv-") ~ "bv")) %>%
  group_by(plot, standorttyp, flaechentyp, enemy.species.1.2) %>%summarize(Abundanz = n(),.groups = 'drop') %>%
  mutate(standort_flaeche = paste0(standorttyp, "(", flaechentyp, ")")) %>%
  relocate(standort_flaeche, .before = 3) %>%
  filter(enemy.species.1.2 != "") %>% rename(host.species.2=enemy.species.1.2)

#alle parasiten aus spalte enemy.species.2
rohdaten_enemy_2 <- rohdaten_1 %>% filter(host.family!="Arachnidae", bemerkung!="angefangene BZ", n.enemy.individuals.2!=0)%>%
  mutate(enemy.species.1.2 = paste0(enemy.species.2, " (g)"))

artenverteilung_e <- rohdaten_enemy_2 %>%
  mutate(flaechentyp = case_when(str_detect(plot, "afs") ~ "AFS",str_detect(plot, "ref") ~ "REF"),
         strukturtyp = case_when(str_detect(plot, "br") ~ "br",str_detect(plot, "acker") ~ "acker"),
         standorttyp = case_when(str_detect(plot, "kw-") ~ "kw",str_detect(plot, "wi-") ~ "wi",str_detect(plot, "wit-") ~ "wit",str_detect(plot, "bv-") ~ "bv")) %>%
  group_by(plot, standorttyp, flaechentyp, enemy.species.2) %>%summarize(Abundanz = n(),.groups = 'drop') %>%
  mutate(standort_flaeche = paste0(standorttyp, "(", flaechentyp, ")")) %>%
  mutate(enemy.species.2.2 = paste0(enemy.species.2, " (g)")) %>%
  relocate(standort_flaeche, .before = 3) %>%
  filter(enemy.species.2 != "") %>% rename(host.species.2=enemy.species.2.2)

artenverteilung_f <- bind_rows(artenverteilung_d, artenverteilung_e, Artenverteilung_c)
artenverteilung_f <- artenverteilung_f %>% filter(host.species.2 !="na (na)") %>% filter(host.species.2 !=" (g)")


arten_relativ <- artenverteilung_f %>% group_by(host.species.2) %>% mutate(gesamt_abundanz = sum(Abundanz, na.rm = TRUE),
                                                                           rel_abundanz = Abundanz / gesamt_abundanz * 100) %>% ungroup()
arten_relativ <- arten_relativ %>%
  mutate(host.species.2 = gsub("I", "*", host.species.2))


#Für jede Art die Fläche mit maximaler Abundanz finden
arten_schwerpunkt_abs <- arten_relativ %>%
  group_by(host.species.2) %>%
  filter(Abundanz == max(Abundanz, na.rm = TRUE)) %>%
  slice(1) %>%  # falls mehrere Maxima, nur eins nehmen
  ungroup() %>%
  arrange(standort_flaeche, desc(Abundanz))

#Faktorlevels für host.species setzen
levels_order_abs <- arten_relativ %>%
  group_by(host.species.2) %>%
  summarise(gesamt_abundanz = sum(Abundanz, na.rm = TRUE)) %>%
  arrange(desc(gesamt_abundanz)) %>%
  pull(host.species.2)

arten_relativ$host.species.2 <- factor(arten_relativ$host.species.2,
                                       levels = rev(levels_order_abs))
#Farben und labels für plot bestimmen
color_kat <- c("wi(AFS)"="steelblue2", "wi(REF)"="steelblue4", "wit(AFS)"="chartreuse1", "wit(REF)"="chartreuse3",
               "kw(AFS)"="orangered2", "kw(REF)"="orangered4", "bv(AFS)"="yellow3", "bv(REF)"="yellow4")
label_kat <- c("wi(AFS)"="AFS Wiesbaden", "wi(REF)"="REF Wiesbaden", "wit(AFS)"="AFS Wittlich", "wit(REF)"="REF Wittlich",
               "kw(AFS)"="AFS Klein-Winternheim", "kw(REF)"="REF Klein-Winternheim", "bv(AFS)"="AFS Bad Vilbel", "bv(REF)"="REF Bad Vilbel")

#Plot mit absoluten Abundanzen
ggplot(arten_relativ, aes(x = host.species.2, y = Abundanz, fill = standort_flaeche)) +
  geom_col() + labs(title = "Nestabundanz aller Arten über alle Flächen ",x = "Arten und Artengruppen",y = "Nestabundanz (n)",fill = "Flächentyp") +
  scale_fill_manual(values = color_kat, labels =label_kat) + scale_y_continuous(breaks = seq(0, 300, by = 50)) + 
  theme_minimal() + theme(axis.text.y = element_text(size = 8, face = "italic")) + coord_flip() 

