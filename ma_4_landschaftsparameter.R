#### SKRIPT 4: LANDSCHAFTSPARAMETER ####
#### SKRIPTERSTELLUNG MITHILFE VON sKRIPT VON WINZER (2025) ####
#Korrelationen zwischen lokalen und landschaftlichen Parametern mit den Nestabundanzen und dem Artenreichtum

#erforderliche Pakete in skript laden
library(tidyverse)
library(ggplot2)
library(vegan)
library(writexl)
library(dplyr)
library(purrr)
library(stringr)
library(pheatmap)
library(openxlsx)
library(Hmisc)

# Daten aus CSV-Datei einlesen
radien_kategorien <- read.csv("C:/Users/p-ber/OneDrive/1_TH Bingen/MA und FORSCH/1_auswertung/umgebungsstrukturen/25_09_23_radien_parameter.csv")
radien_flaechenanteile <- read.csv2("C:/Users/p-ber/OneDrive/1_TH Bingen/MA und FORSCH/1_auswertung/umgebungsstrukturen/25_09_17_flaechenanteile_radien.csv")
bienen_trapnest <- read.csv2("C:/Users/p-ber/OneDrive/1_TH Bingen/MA und FORSCH/1_auswertung/umgebungsstrukturen/bienendaten_trapnest.csv", header=T)


#### (1) Datenrpüfung ####
# Datenformat anpassen
prefixes <- c("bv", "kw", "wi", "wt")
types    <- c("afs", "ref")
suffixes <- c("250", "1000")
radien_kategorien <- radien_kategorien %>%mutate(standort = sapply(id, function(x) prefixes[str_detect(x, prefixes)]),typ = sapply(id, function(x) types[str_detect(x, types)]),
                                                 radius = sapply(id, function(x) suffixes[str_detect(x, suffixes)]),)
radien_kategorien2 <- radien_kategorien %>% select(-any_of(c("X", "radius", "typ", "standort")))
radien_flaechenanteile2 <- radien_flaechenanteile %>% select(-any_of(c("X", "radius", "typ", "standort")))

radien_flaechenanteile_long <- radien_flaechenanteile %>% pivot_longer(cols = c(acker, acker_blueh, ackerbrache, 
                                                                                afs, erdwege_offenboden, gehoelze, siedlung, sonstige, staudenflur, 
                                                                                streuobst, garten_obst, gruenland_int, urban_green, wasser, wein, 
                                                                                wiese_ext, wald, weide_ext), names_to = "parameter", values_to = "wert")
radien_flaechenanteile_long$wert <- as.numeric(radien_flaechenanteile_long$wert)

radien_kategorien_long <- radien_kategorien2 %>% pivot_longer(cols = c(shannon_oberkat, shannon_codierung, shannon_vergleich_alkis, anteil_snh, anteil_flower_rich, 
                                                                       anteil_wb_wichtig, anteil_wb_wichtig_ohne_dgl,anteil_wb_allg, anteil_wb_alles, anteil_ground_nesting, 
                                                                       anteil_stem_nesting, mean_count_per_km2, mean_size_ha_lw), names_to = "parameter", values_to = "wert")

radien <- bind_rows(radien_flaechenanteile_long, radien_kategorien_long)

radien_bienen_trap <- left_join(radien, bienen_trapnest, by = "id")
radien_bienen_trap <- radien_bienen_trap %>%
  mutate(
    radius = case_when(
      grepl("1000", id) ~ 1000,
      grepl("250", id) ~ 250,
      grepl("50", id) ~ 50,
      TRUE ~ NA_real_  # falls nichts davon zutrifft
    )
  )
# Scatterplot prüfen
plot(radien_bienen_trap$wert, radien_bienen_trap$Abundanz)
plot(radien_bienen_trap$wert[radien_bienen_trap$wert <= 3.5],
     radien_bienen_trap$Abundanz[radien_bienen_trap$wert <= 3.5],xlab = "Wert", ylab = "Abundanz")

plot(radien_bienen_trap$wert, radien_bienen_trap$Artenzahl)
plot(radien_bienen_trap$wert[radien_bienen_trap$wert <= 4],
     radien_bienen_trap$Artenzahl[radien_bienen_trap$wert <= 4],xlab = "Wert", ylab = "Artenzahl")

# Normalverteilung prüfen
shapiro.test(radien_bienen_trap$wert) # signifikanten Unterschiede
shapiro.test(radien_bienen_trap$Abundanz) # signifikanten Unterschiede
shapiro.test(radien_bienen_trap$Artenzahl) # signifikanten Unterschiede
#wegen den signifikanten Unterschieden in allen Datensätzen werden die Korrelationen mit der Spearman-methode getestet

veg_daten <- read.csv2 ("C:/Users/p-ber/OneDrive/1_TH Bingen/MA und FORSCH/1_auswertung/vegetation_short.csv")
bienendaten_trap <- read.csv2("C:/Users/p-ber/OneDrive/1_TH Bingen/MA und FORSCH/1_auswertung/umgebungsstrukturen/bienendaten_trapnest.csv", header=T)
radien_parameter <- read.csv("C:/Users/p-ber/OneDrive/1_TH Bingen/MA und FORSCH/1_auswertung/umgebungsstrukturen/25_09_23_radien_parameter.csv")


bienen_2 <- bienendaten_trap %>% 
  mutate(
    standort = dplyr::recode(as.character(Standort),
                             "Bad Vilbel" = "bv",
                             "Klein-Winternheim" = "kw",
                             "Wiesbaden" = "wi",
                             "Wittlich" = "wt"),
    typ = tolower(as.character(Management))
  ) %>%
  select(-X, -Management, -Standort)
#### (2) Radien_Kategorien ####

# anpassen
radien_parameter <- radien_parameter %>%
  mutate(
    standort = sapply(id, function(x) prefixes[str_detect(x, prefixes)]),
    typ   = sapply(id, function(x) types[str_detect(x, types)]),
    radius = sapply(id, function(x) suffixes[str_detect(x, suffixes)]),
  ) %>%
  select(-X)

#Daten extrahieren und zusammenführen



#Subsets erstellen
subsets <- radien_kategorien %>% group_by(typ, radius) %>% group_split()

# Namen für die Liste bauen
names(subsets) <- radien_kategorien %>%distinct(typ, radius) %>%mutate(name = paste(typ, radius, sep = "_")) %>%pull(name)

cor_bienen_2_param <- function(bienen_2_sub, param_sub) {
  num_bienen_2 <- bienen_2_sub %>% select(where(is.numeric))
  num_param  <- param_sub  %>% select(where(is.numeric))
  
  res <- matrix("", nrow = ncol(num_bienen_2), ncol = ncol(num_param),
                dimnames = list(colnames(num_bienen_2), colnames(num_param)))
  
  for (i in seq_along(num_bienen_2)) {
    for (j in seq_along(num_param)) {
      test <- cor.test(num_bienen_2[[i]], num_param[[j]], method = "spearman")
      cor_val <- round(test$estimate, 2)
      p <- test$p.value
      stars <- ifelse(p < 0.001, "***",
                      ifelse(p < 0.01, "**",
                             ifelse(p < 0.05, "*", "")))
      res[i, j] <- paste0(cor_val, stars)
    }
  }
  
  as.data.frame(res)
}

# Funktion für alle Subgruppen mit Standort-Matching

cor_all_subgroups_matched <- function(bienen_2, radien_parameter) {
  
  results <- list()
  
  for (typ_val in c("afs","ref")) {
    for (radius_val in c("250","1000")) {
      
      # Parameter-Subgruppe
      param_sub <- radien_parameter %>%
        filter(typ == typ_val & radius == radius_val)
      
      # Bienendaten-Subgruppe
      bienen_2_sub <- bienen_2 %>%
        filter(typ == typ_val)
      
      # Join nach standort
      df_joined <- inner_join(bienen_2_sub, param_sub, by = "standort")
      
      if(nrow(df_joined) > 0) {
        # numerische Spalten aus bienen_2 (Abundanz, Artenzahl)
        num_bienen_2 <- df_joined %>% select(all_of(colnames(bienen_2_sub)[sapply(bienen_2_sub, is.numeric)]))
        
        # numerische Spalten aus param_sub (alle außer radius, typ, standort, id)
        num_param <- df_joined %>% select(where(is.numeric)) %>%
          select(-all_of(colnames(num_bienen_2)))  # nicht die Bienendaten-Spalten doppelt nehmen
        
        cor_mat <- cor_bienen_2_param(num_bienen_2, num_param)
        
        results[[paste0(typ_val,"_",radius_val)]] <- cor_mat
      }
      
    }
  }
  
  results
}


correlation_results <- cor_all_subgroups_matched(bienen_2, radien_parameter)

# Zugriff auf z. B. afs_250:
# Neues Workbook erstellen
wb <- createWorkbook()

# Namen der Arbeitsblätter (deine Listennamen)
sheet_names <- c("afs_250", "afs_1000", "ref_250", "ref_1000")

for (sheet in sheet_names) {
  if (!is.null(correlation_results[[sheet]])) {
    addWorksheet(wb, sheet)
    writeData(wb, sheet, correlation_results[[sheet]])
  } else {
    message(paste("Tabelle", sheet, "ist NULL und wird übersprungen."))
  }
}

# Excel speichern
saveWorkbook(wb, "kor_kategorien_spearman.xlsx", overwrite = TRUE)

#### (2) Radien_flaechenanteile####
cor_bienen_2_param <- function(bienen_2_sub, param_sub) {
  num_bienen_2 <- bienen_2_sub %>% select(where(is.numeric))
  num_param  <- param_sub  %>% select(where(is.numeric))
  
  res <- matrix("", nrow = ncol(num_bienen_2), ncol = ncol(num_param),
                dimnames = list(colnames(num_bienen_2), colnames(num_param)))
  
  for (i in seq_along(num_bienen_2)) {
    for (j in seq_along(num_param)) {
      test <- cor.test(num_bienen_2[[i]], num_param[[j]], method = "spearman")
      cor_val <- round(test$estimate, 2)
      p <- test$p.value
      stars <- ifelse(p < 0.001, "***",
                      ifelse(p < 0.01, "**",
                             ifelse(p < 0.05, "*", "")))
      res[i, j] <- paste0(cor_val, stars)
    }
  }
  
  as.data.frame(res)
}

# Funktion für alle Subgruppen mit Standort-Matching

cor_all_subgroups_matched <- function(bienen_2, radien_flaechenanteile) {
  
  results <- list()
  
  for (typ_val in c("afs","ref")) {
    for (radius_val in c("250","1000")) {
      
      # Parameter-Subgruppe
      param_sub <- radien_flaechenanteile %>%
        filter(typ == typ_val & radius == radius_val)
      
      # Bienendaten-Subgruppe
      bienen_2_sub <- bienen_2 %>%
        filter(typ == typ_val)
      
      # Join nach standort
      df_joined <- inner_join(bienen_2_sub, param_sub, by = "standort")
      
      if(nrow(df_joined) > 0) {
        # numerische Spalten aus bienen_2 (Abundanz, Artenzahl)
        num_bienen_2 <- df_joined %>% select(all_of(colnames(bienen_2_sub)[sapply(bienen_2_sub, is.numeric)]))
        
        # numerische Spalten aus param_sub (alle außer radius, typ, standort, id)
        num_param <- df_joined %>% select(where(is.numeric)) %>%
          select(-all_of(colnames(num_bienen_2)))  # nicht die Bienendaten-Spalten doppelt nehmen
        
        cor_mat <- cor_bienen_2_param(num_bienen_2, num_param)
        
        results[[paste0(typ_val,"_",radius_val)]] <- cor_mat
      }
      
    }
  }
  
  results
}


correlation_results <- cor_all_subgroups_matched(bienen_2, radien_flaechenanteile)


# Neues Workbook erstellen
wb <- createWorkbook()
sheet_names <- c("afs_250", "afs_1000", "ref_250", "ref_1000")

for (sheet in sheet_names) {
 if (!is.null(correlation_results[[sheet]])) {
    addWorksheet(wb, sheet)
    writeData(wb, sheet, correlation_results[[sheet]])
  } else {
    message(paste("Tabelle", sheet, "ist NULL und wird übersprungen."))
  }
}

# Excel speichern
saveWorkbook(wb, "kor_flaechenanteile_spearman.xlsx", overwrite = TRUE)

#### (3) vegetationsdaten ####
veg_daten <- veg_daten %>%
  mutate(across(where(is.character), ~ {
    # Ersetze Komma durch Punkt (falls vorhanden)
    tmp <- gsub(",", ".", .)
    # Versuche umwandeln
    num <- suppressWarnings(as.numeric(tmp))
    # Nur wenn mindestens ein Wert kein NA ist, Umwandlung übernehmen, sonst original behalten
    if (any(!is.na(num))) num else .
  }))
# anpassen
veg_daten <- veg_daten %>%
  mutate(
    standort = sapply(id, function(x) prefixes[str_detect(x, prefixes)]),
    typ   = sapply(id, function(x) types[str_detect(x, types)]),
    radius = sapply(id, function(x) suffixes[str_detect(x, suffixes)])
  )


cor_bienen_2_param <- function(bienen_2_sub, param_sub) {
  num_bienen_2 <- bienen_2_sub %>% select(where(is.numeric))
  num_param  <- param_sub  %>% select(where(is.numeric))
  
  res <- matrix("", nrow = ncol(num_bienen_2), ncol = ncol(num_param),
                dimnames = list(colnames(num_bienen_2), colnames(num_param)))
  
  for (i in seq_along(num_bienen_2)) {
    for (j in seq_along(num_param)) {
      test <- cor.test(num_bienen_2[[i]], num_param[[j]], method = "spearman")
      cor_val <- round(test$estimate, 2)
      p <- test$p.value
      stars <- ifelse(p < 0.001, "***",
                      ifelse(p < 0.01, "**",
                             ifelse(p < 0.05, "*", "")))
      res[i, j] <- paste0(cor_val, stars)
    }
  }
  
  as.data.frame(res)
}

# Funktion für alle Subgruppen mit Standort-Matching

cor_all_subgroups_matched <- function(bienen_2, veg_daten) {
  
  results <- list()
  
  for (typ_val in c("afs","ref")) {
    for (radius_val in c("250","1000")) {
      
      # Parameter-Subgruppe
      param_sub <- veg_daten %>%
        filter(typ == typ_val & radius == radius_val)
      
      # Bienendaten-Subgruppe
      bienen_2_sub <- bienen_2 %>%
        filter(typ == typ_val)
      
      # Join nach standort
      df_joined <- inner_join(bienen_2_sub, param_sub, by = "standort")
      
      if(nrow(df_joined) > 0) {
        # numerische Spalten aus bienen_2 (Abundanz, Artenzahl)
        num_bienen_2 <- df_joined %>% select(all_of(colnames(bienen_2_sub)[sapply(bienen_2_sub, is.numeric)]))
        
        # numerische Spalten aus param_sub (alle außer radius, typ, standort, id)
        num_param <- df_joined %>% select(where(is.numeric)) %>%
          select(-all_of(colnames(num_bienen_2)))  # nicht die Bienendaten-Spalten doppelt nehmen
        
        cor_mat <- cor_bienen_2_param(num_bienen_2, num_param)
        
        results[[paste0(typ_val,"_",radius_val)]] <- cor_mat
      }
      
    }
  }
  
  results
}


correlation_results <- cor_all_subgroups_matched(bienen_2, veg_daten)

# Zugriff auf z. B. afs_250:
# Neues Workbook erstellen
wb <- createWorkbook()

# Namen der Arbeitsblätter (deine Listennamen)
sheet_names <- c("afs_250", "afs_1000", "ref_250", "ref_1000")

for (sheet in sheet_names) {
  if (!is.null(correlation_results[[sheet]])) {
    addWorksheet(wb, sheet)
    writeData(wb, sheet, correlation_results[[sheet]])
  } else {
    message(paste("Tabelle", sheet, "ist NULL und wird übersprungen."))
  }
}

# Excel speichern
saveWorkbook(wb, "kor_vegetation_spearman.xlsx", overwrite = TRUE)
