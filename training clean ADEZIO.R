
# data challenge Giuditta Adezio 880076 -----------------------------------

library(tidyverse)

data <- read.csv("training.csv", row.names = 'ID')

library(skimr)
skim(data)

### OTHER FEATURES --------------------------------------------------------

# modifico other features staccando la parola pvc dalle successive 
# poiché probabilmente per errore restano attaccate, stessa cosa vale 
# per property land 
data$other_features <- data$other_features %>%
  gsub("(pvc)([a-zA-Z])","pvc|\\2", .) %>%
  gsub("(property land)([0-9])","property land|\\2", .) 

# rendo tutte le other features che si trovavano nella stessa variabile 
# delle dummy che assumono valore 1 se la casa ha la caratteristica aggiuntiva
# e zero altrimenti
data2 <- data %>% 
  mutate(row_id = row_number()) %>%
  separate_rows(other_features, sep = " *\\| *") %>%
  mutate(other_features = trimws(other_features))
 
data2 <- data2 %>%  
  mutate(value = 1) %>% 
  pivot_wider(
    id_cols = row_id,
    names_from = other_features,
    values_from = value,
    values_fill = list(value = 0))

# rimuovo la colonna degli id
data2 <- data2[,-1]

# per costruzione delle variabili non ci saranno missing
library(visdat)
vis_dat(data2)

# visualizziamo il dataset
skim(data2)


# CONCIENRGE RECEPTION --------------------------------------------------

# unisco full day concierge, half-day cocierge e reception
data2['concierge/reception'] <- ifelse(data2$reception == 1 |
                                       data2$`full day concierge` == 1 | 
                                       data2$`half-day concierge`, 1, 0)


# EXPOSURE ---------------------------------------------------------------

# creo una variabile categorica per l'exposure che assume i seguenti 
# valori: internal external e double
data2 <- data2 %>%
  mutate(exposure = case_when(
      `external exposure` == 1 ~ "external",
      `internal exposure` == 1 ~ "internal",
      `double exposure` == 1 ~ "double",
      TRUE ~ 'other'))

# creo un dataset sulle esposizioni (escluse quello che ho unito subito sopra)
data_exp <- data2 %>%
  dplyr::select(-c(`internal exposure`, `external exposure`, 
                   `double exposure`, exposure)) %>%
  mutate(south = if_else(if_any(contains("exposure"), ~ . == 1), 1, 0))
sum(data_exp[,'south']) # 184
# l'esposizione a nord / sud / est / ovest è indicata solo per 184 case su 8000
# decido di non includere nessuna di queste nel mio modello

# sarebbe stato interessante creare una variabile dummy che assuma valore 1
# per le case esposte a sud, 0 altrimenti 
sum(data2$`exposure south` + data2$`exposure south, east` +
    data2$`exposure south, east, west` + data2$`exposure south, west`+
    data2$`exposure north, south` + data2$`exposure north, south, east`+
    data2$`exposure north, south, east, west` + 
    data2$`exposure north, south, west`)
# vedo però che l'esposizione a sud è indicata per 86 case su 8000 quindi 
# continuo con la mia idea di non includere queste nel modello


# BALCONY TERRACE ---------------------------------------------------------

# balcony + terrace messe insieme in una dummy 
data2['balcony/terrace'] <- ifelse(data2$balcony == 1 | 
                                   data2$`8 balconies` == 1 | 
                                   data2$`6 balconies` == 1 |
                                   data2$`1 balcony` == 1 |
                                   data2$terrace == 1, 1, 0)


# GARDEN ------------------------------------------------------------------

# creo un'unica variabile categorica per garden (private, shared e entrambi)
data2 <- data2 %>%
  mutate(garden = case_when(
    `private garden` == 1 ~ "private",
    `shared garden` == 1 ~ "shared",
    `private and shared garden` == 1 ~ "private and shared",
    TRUE ~ 'no garden'))


# WINDOWS -----------------------------------------------------------------

sum(data2$`window frames in double glass / metal` + 
      data2$`window frames in double glass / pvc` +
      data2$`window frames in double glass / wood` +
      data2$`window frames in glass / metal`+
      data2$`window frames in glass / pvc` +
      data2$`window frames in glass / wood` +
      data2$`window frames in triple glass / metal`+
      data2$`window frames in triple glass / pvc` +
      data2$`window frames in triple glass / wood`)
# sono 6818 le case per cui sono indicate specifiche sule finestre 

# divido le finestre in finestre di legno o no
data2["wood_frames"] <- ifelse(data2$`window frames in double glass / wood` == 1 |
                               data2$`window frames in glass / wood`        == 1 | 
                               data2$`window frames in triple glass / wood` == 1, 1, 0)
boxplot(data$selling_price ~ data2$wood_frames)

# creo una variabile dummy che mi indichi se le finestre hanno un vetro singolo 
# o no (doppio o triplo)
data2["single_glass"] <- ifelse(data2$`window frames in glass / metal` == 1 |
                                data2$`window frames in glass / pvc`   == 1 | 
                                data2$`window frames in glass / wood` == 1, 1, 0)


# FURNISHED ---------------------------------------------------------------

# raggruppo furnished in una sola variabile 
data2 <- data2 %>%
  mutate(furnished = case_when( 
     furnished == 1 ~ "totally",
    `partially furnished` == 1 ~ "partially",
    TRUE ~ 'not furnished'))


# PROPERTY LAND -----------------------------------------------------------

sum(data2$`property land`) # le case con property land sono 2
cor(data2$`property land`, data$selling_price) 
# la correlazione con il prezzo è molto bassa


# TV SYSTEMS --------------------------------------------------------------
data_tv <- data2 %>%
  dplyr::select(c(`centralized tv system`, `single tv system`,
                  `tv system with satellite dish`))
sum(data2$`centralized tv system`)
# quesi tutte le case hanno un sistema tv centralizzato

data_tv <- cbind(data_tv, data$selling_price)

library(ggcorrplot)
ggcorrplot(cor(data_tv), lab = T)
# elimino le variabili relative ai sistemi tv perché poco correlate col prezzo


# KITCHEN ----------------------------------------------------------------

data_kitchen <- data2 %>%
  dplyr::select(c(`only kitchen furnished`, kitchen))

data_kitchen <- cbind(data_kitchen, data$selling_price)
ggcorrplot(cor(data_kitchen), lab = T)
# elimino tutte le variabili relative alla cucina in qanto più di tre quarti 
# delle case non hanno un valore relativo alla cucina e la correlazione
# con il prezzo risulta molto bassa


# DISABLED ACCESS --------------------------------------------------------

sum(data2$`disabled access`) 
# solo 3 case hanno l'accesso ai disabili, elimino la variabile


# ULTERIORI MODIFICHE ------------------------------------------------------

# elimino le variabili non più utili
data2 <- data2 %>%
  dplyr::select(-c(`half-day concierge`, `full day concierge`, reception, 
            `internal exposure`, `external exposure`, `double exposure`,
            `exposure south`, `exposure south, east`, 
            `exposure south, east, west`, `exposure south, west`,
            `exposure north, south`, `exposure north, south, east`,
            `exposure north, south, east, west`, `exposure east, west`,
            `exposure north, west`, `exposure east`, 
            `exposure north, east, west`,`exposure west`,
            `exposure north, south, west`, balcony, `8 balconies`,
            `6 balconies`, terrace, `property land`, `1 balcony`,
            `partially furnished`, `window frames in glass / metal`,
            `window frames in glass / pvc`, `window frames in glass / wood`,
            `window frames in double glass / wood`,
            `window frames in glass / wood`,
            `window frames in triple glass / wood`,
            `centralized tv system`, `single tv system`, `shared garden`, 
            `private garden`, `private and shared garden`, 
            `tv system with satellite dish`, `NA`, 
            `window frames in double glass / pvc`,
            `window frames in double glass / metal`,
            `window frames in double glass / wood`, 
            `window frames in triple glass / pvc`, 
            `window frames in triple glass / wood`, 
            `window frames in triple glass / metal`, `exposure north`,
            `exposure north, east`, `only kitchen furnished`, kitchen,
            `disabled access`))

skim(data2)
vis_dat(data2) # non ci sono missing giustamente per costruzione

# correlazione other_features ------------------------------------------------

data_cor <- data2
data_cor$garden    <- as.numeric(as.factor(data_cor$garden))
data_cor$exposure  <- as.numeric(as.factor(data_cor$exposure))
data_cor$furnished <- as.numeric(as.factor(data_cor$furnished))

ggcorrplot(cor(data_cor))



# riprendiamo il dataset originale per la pulizia delle altre variabili ------

skim(data)
data <- dplyr::select(data, -other_features) # tolgo other features
data2 <- data2 %>%
  mutate(across(c(`optic fiber`, tavern, `security door`, cellar, 
                  `video entryphone`, `alarm system`, closet, `electric gate`, 
                  hydromassage, fireplace, attic, pool, `tennis court`,
                  `concierge/reception`, `balcony/terrace`, wood_frames,
                  single_glass), as.factor)) 
# per comodità ho reso tutte le var di questo dataset factor

# unisco i due dataset per riottenere il dataset completo per le 
# analisi successive
data <- cbind(data, data2)

# vediamo i missing in tutto il dataset 
data_no_NA <- data[complete.cases(data), ]
vis_dat(data)

a <- ggplot(data, aes(x = selling_price, after_stat(density))) +
  geom_histogram(col = 'white', fill = 'slateblue2', bins = 30)+
  labs(title = 'distribuzione di selling price')

b <- ggplot(data, aes(x = log(selling_price), after_stat(density))) +
  geom_histogram(col = 'white', fill = 'slateblue2', bins = 35)+
  geom_density( col = 'lightblue', fill = 'lightblue', alpha = 0.3) +
  labs(title = 'distribuzione del log di selling price') +
  xlim(c(11, 15.5))
hist(log(data$selling_price), freq = F)


# SQUARE METERS ------------------------------------------------------------

sum(is.na(data$square_meters)) # non ci sono NA
table(data$square_meters)

hist(data$square_meters, freq = F)
c <- ggplot(data, aes(x = log(square_meters), after_stat(density))) +
  geom_histogram(col = 'white', fill = 'slateblue2', bins = 30) +
  geom_density( col = 'lightblue', fill = 'lightblue', alpha = 0.3) +
  labs(title = 'distribuzione del log di square meters') 

d <- ggplot(data, aes(x = log(square_meters), y = log(selling_price))) +
  geom_point(size = 1.5, col = 'slateblue', alpha = 0.15) +
  geom_smooth(method = "lm", col = "slateblue") +
  labs(title = 'relazione tra square meters e selling price') 

library(cowplot)

plot_grid(a, b, d, c,
          ncol = 2, align = 'hv', axis = 'tblr')

# imposto come NA tutti i valori di square meters sotto i 13 metri quadri in 
# quanto insensati per imputarli successivamente
data <- data %>%
  mutate(square_meters = case_when( 
    square_meters < 13 ~ NA,
    TRUE ~ square_meters))


# YEAR OF COSTRUCTION ------------------------------------------------------

sum(is.na(data$year_of_construction)) # 789 NA
length(which(data$year_of_construction < 1750)) # 32
hist(data$year_of_construction[data$year_of_construction >= 1750], 
     col = 'slateblue', main = 'year of construction hist')
c(mean(data$year_of_construction, na.rm = T), 
  median(data$year_of_construction, na.rm = T))
# 1962.519 1960.000


# ENERGY EFFICIENCY CLASS ---------------------------------------------------

table(data$energy_efficiency_class, useNA = 'always')

# imposto i valori , come NA per imputarli successivamente
data <- data %>%
  mutate(energy_efficiency_class = case_when(
    energy_efficiency_class == ',' ~ NA,
    TRUE ~ energy_efficiency_class ))

ggplot(data, aes(x = energy_efficiency_class, y = (selling_price))) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  ylim(0, 1000000) +
  labs(title = "Distribuzione del prezzo per classe energetica",
       x = "Classe energetica",
       y = "Prezzo")

# ricategorizzo dividendo le classi di efficienza alta media e bassa
data <- data %>%
  mutate(energy_efficiency_class = case_when(
    energy_efficiency_class == 'a' ~ 'high',
    energy_efficiency_class %in% c('b','c','d') ~ 'medium',
    energy_efficiency_class %in% c('e','f','g') ~ 'low',
    TRUE ~ NA ))

ggplot(data, aes(x = energy_efficiency_class, y = (selling_price))) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  ylim(0, 1000000) +
  labs(title = "Distribuzione del prezzo per classe energetica",
       x = "Classe energetica",
       y = "Prezzo")
table(data$energy_efficiency_class)
# le case con classe di efficienza bassa sono 4908, molte di più rispetto a 
# quelle di efficienza alta (938)


# ROOMS NUMBER --------------------------------------------------------------

table(data$rooms_number, useNA = 'always') # non ci sono missing
data$rooms_number <- as.factor(data$rooms_number)

stanze <- ggplot(data, aes(x = rooms_number, y = log(selling_price))) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "distribuzione del prezzo per numero di stanze") 

cor(as.numeric(data$rooms_number), data$square_meters)
cor(as.numeric(data$bathrooms_number), data$square_meters)

# BATHROOMS NUMBER -----------------------------------------------------------

bagni <- ggplot(data, aes(x = bathrooms_number, y = log(selling_price))) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "distribuzione del prezzo per numero di bagni") 

plot_grid(stanze, bagni)
# mi aspetto che questa variabile sia molto rilevante
# dai boxplot si evince una sostanziale differenza di prezzo a seconda del 
# numero di bagni
table(data$bathrooms_number, useNA = 'always') # 25 missing
# la maggior parte delle case ha un solo bagno (4907) 


# LIFT ---------------------------------------------------------------------

table(data$lift, useNA = 'always') # lift è da imputare ci sono 141 missing
table(data$lift, data$total_floors_in_building)

ggplot(data, aes(x = lift, y = log(selling_price))) +
  geom_boxplot(fill = "lightblue", color = "blue") 
# dal boxplot il prezzo sembra essere più alto quando nella casa è 
# presente l'ascensore 


# FLOOR -------------------------------------------------------------------

ggplot(data, aes(x = as.factor(floor), y = log(selling_price))) +
  geom_boxplot(fill = "lightblue", color = "blue") 

# per renderla numerica assegno un numero a tutte le categorie 0 per il 
# ground floor (piano terra), 0.5 per il mezzanino (questo numero è stato 
# assegnato per facilitare la stima dei modelli), -1 per il semibasement
# (seminterrato)
data$floor <- as.character(data$floor)
data <- data %>%
  mutate(floor = case_when(
    floor == "ground floor" ~ 0,
    floor == "mezzanine" ~ 0.5,     
    floor == "semi-basement" ~ -1,
    TRUE ~ as.double(floor)))
table(data$floor, useNA = 'always')

# TOTAL FLOORS IN BUILDING -------------------------------------------------

table(data$total_floors_in_building, data$floor, useNA = 'always')
table(data$total_floors_in_building, useNA = 'always')
data$total_floors_in_building[data$total_floors_in_building == '1 floor'] = 1

sum(data$total_floors_in_building > 10, na.rm = T)

table(data$floor)
# metto insieme tutti i palazzi con più di 10 piani perché gli appartamenti
# in vendita dalla variabile floor non risultano essere mai sopra al nono piano
data <- data %>%
  mutate(total_floors_in_building = case_when(
      as.numeric(total_floors_in_building) >= 10 ~ "9+",
      TRUE ~ as.character(total_floors_in_building)))

data$total_floors_in_building <- as.factor(data$total_floors_in_building)

ggplot(data, aes(x = as.factor(total_floors_in_building), 
                 y = log(selling_price))) +
  geom_boxplot(fill = "lightblue", color = "blue") 


# CAR PARKING -------------------------------------------------------------

table(data$car_parking)
levels(as.factor(data$car_parking))

# riduco le categorie di garage dal momento che sono davvero poche le persone 
# che appartengono ad alcune delle categorie
data <- data %>%
  mutate(car_parking = case_when(
    str_detect(car_parking, "^1 in garage/box") ~ "one garage/box",
    str_detect(car_parking, "^[2-9][0-9]* in garage/box") ~ "one + garage/box",
    car_parking == "no" ~ "no",
    TRUE ~ "shared parking" ))

table(data$car_parking)

# CONDOMINIUM FEES --------------------------------------------------------

data$condominium_fees[data$condominium_fees == 'No condominium fees'] = 0
table(as.numeric(data$condominium_fees), useNA = 'always')

# AVAILABILITY ------------------------------------------------------------

table(data$availability, useNA = 'always')

library(stringr)
library(lubridate)

data <- data %>%
    mutate(extracted_date = str_extract(availability, "\\d{2}/\\d{2}/\\d{4}"),
           parsed_date = dmy(extracted_date),
           availability = case_when(
             availability == "available" ~ "available",
             !is.na(parsed_date) & parsed_date >= ymd("2023-01-01") ~ "not available yet",
             TRUE ~ NA_character_)) %>%
  dplyr::select(-c(extracted_date, parsed_date))

table(data$availability, useNA = 'always')

# trasformo le case che chiaramente non sono ancora state costruite (con date di 
# costruzione dal 2023 in poi visto che il dataset sembra essere stato fatto 
# nel 2022 circa) da available nelle categorie create in precedenza
data <- data %>%
  mutate(availability = case_when(
    (availability == 'available' & 
      year_of_construction  > 2023) ~ "not available yet",
    TRUE ~ availability))


# CONDITIONS --------------------------------------------------------------

table(data$conditions, useNA = 'always')


# ZONE --------------------------------------------------------------------
library("geosphere")
dist_zone <- data.frame(
  zone = c("quadronno - crocetta", "ticinese", "palestro", "brera", "porta venezia", 
           "arco della pace", "sempione", "turati", "borgogna - largo augusto", "missori", 
           "vincenzo monti", "lanza", "duomo", "carrobbio", "scala - manzoni", 
           "san babila", "cadorna - castello", "quadrilatero della moda",
           "porta romana - medaglie d'oro", "martini - insubria", "navigli - darsena", 
           "morgagni", "de angeli", "dezza", "pagano", "isola", "indipendenza", 
           "amendola - buonarroti", "corso genova", "piave - tricolore", "cadore", 
           "repubblica", "centrale", "buenos aires", "lodi - brenta", "corso san gottardo", 
           "montenero", "cenisio", "bocconi", "san carlo", "paolo sarpi", "vercelli - wagner", 
           "guastalla", "rubattino", "farini", "moscova", "washington", "ascanio sforza", 
           "solari", "melchiorre gioia", "zara", "arena", "city life", "frua", 
           "portello - parco vittoria", "garibaldi - corso como", "piazza napoli", 
           "porta nuova", "san vittore", "gallaratese", "giambellino", "cermenate - abbiategrasso",
           "vigentino - fatima", "rovereto", "pezzotti - meda", "dergano", "ghisolfa - mac mahon",
           "crescenzago", "villa san giovanni", "barona", "quartiere olmi", "famagosta",
           "tre castelli - faenza", "viale ungheria - mecenate", "cascina dei pomi", "san siro", "baggio", 
           "cantalupa - san paolo", "corvetto", "bruzzano", "bisceglie", "quinto romano", "qt8", 
           "città studi", "pasteur", "cascina merlata - musocco", "niguarda", "bovisa", 
           "cimiano", "quartiere adriano", "piazzale siena", "parco trotter", "molise - cuoco", 
           "roserio", "greco - segnano", "gambara", "ortica", "rogoredo", "bignami - ponale", 
           "certosa", "tripoli - soderini", "bologna - sulmona", "udine", "precotto", 
           "monte rosa - lotto", "turro", "chiesa rossa", "bande nere", "quartiere forlanini", 
           "ponte nuovo", "gorla", "bovisasca", "primaticcio", "via fra' cristoforo", 
           "affori", "argonne - corsica", "quarto oggiaro", "porta vittoria", "maggiolina", 
           "ripamonti", "casoretto", "istria", "ca' granda", "vialba", "prato centenaro", 
           "quintosole - chiaravalle", "santa giulia", "inganni", "comasina", "quarto cagnino", 
           "sant'ambrogio", "gratosoglio", "monte stella", "bicocca", "ponte lambro", 
           "trenno", "lambrate", "via canelli", "figino", "via calizzano", "lorenteggio", 
           "plebisciti - susa", "muggiano", "quartiere feltre", "cascina gobba", "parco lambro"),
  longitude = c(9.1918549, 9.1837887, 9.1996026, 9.1874068, 9.2051853, 9.1742533, 9.1682917, 9.1946796,
                9.1980432, 9.1884906, 9.1690085, 9.1815902, 9.1919429, 9.1813177, 9.1900182, 9.1969990,
                9.1767835, 9.1951715, 9.2101169, 9.2209893, 9.1696288, 9.2123140, 9.1481373, 9.1616095,
                9.1660462, 9.1898099, 9.2132199, 9.1517190, 9.1755234, 9.2061651, 9.2129602, 9.1988695,
                9.2060969, 9.2110256, 9.2183182, 9.1798076, 9.2052836, 9.1657118, 9.1871523, 9.1434214,
                9.1751551, 9.1552276, 9.2024805, 9.2554604, 9.1799501, 9.1849250, 9.1570942, 9.1764880,
                9.1623956, 9.1995499, 9.1946357, 9.1795271, 9.1576894, 9.1498421, 9.1464863, 9.1874564,
                9.1527380, 9.1953301, 9.1713335, 9.1121123, 9.1423274, 9.1762974, 9.2011449, 9.2194384,
                9.1788326, 9.1762181, 9.1600773, 9.2399984, 9.2266438, 9.1546830, 9.0816431, 9.1665103,
                9.1483846, 9.2508454, 9.2115598, 9.1304605, 9.0863314, 9.1579914, 9.2242604, 9.1741345,
                9.1139569, 9.0891011, 9.1364844, 9.2236325, 9.2182812, 9.1041991, 9.1918933, 9.1597554,
                9.2426101, 9.2457965, 9.1370530, 9.2253791, 9.2242630, 9.1242034, 9.2128533, 9.1421790,
                9.2467318, 9.2479677, 9.2071906, 9.1334870, 9.1399727, 9.2279333, 9.2366280, 9.2276605,
                9.1435048, 9.2227167, 9.1608482, 9.1361252, 9.2447626, 9.2370955, 9.2249727, 9.1561871,
                9.1303891, 9.1723419, 9.1723099, 9.2289088, 9.1375854, 9.2236994, 9.2013277, 9.2036471,
                9.2282325, 9.1980441, 9.2009948, 9.1291373, 9.1973050, 9.2085134, 9.2408535, 9.1239775,
                9.1633473, 9.1090585, 9.1638820, 9.1715555, 9.1344428, 9.2075123, 9.2639241, 9.1010977,
                9.2512533, 9.2528104, 9.0771489, 9.1610926, 9.1409823, 9.2200812, 9.0728595, 9.2448493,
                9.2641956, 9.2419599),
  latitude = c(45.4543369, 45.455595, 45.4730368, 45.4712356, 45.4721710, 45.4768829, 45.4751540, 45.4752910,
               45.4647364, 45.4607307, 45.4710942, 45.4713291, 45.4641892, 45.4602594, 45.4680566, 45.4660079,
               45.4681260, 45.4682332, 45.4544252, 45.4539556, 45.4477479, 45.4786499, 45.4690621, 45.4602568,
               45.4715433, 45.48766645, 45.4676288, 45.4711478, 45.4572295, 45.4678489, 45.4583853, 45.4805564,
               45.4839709, 45.4811965, 45.4427597, 45.4495428, 45.4567872, 45.4877441, 45.4499951, 45.4830863,
               45.4817502, 45.4684910, 45.4593325, 45.4782664, 45.4932121, 45.4776319, 45.4629993, 45.4447658,
               45.4552222, 45.4884801, 45.4959496, 45.4562075, 45.4783834, 45.4648742, 45.4887850, 45.4829767,
               45.4529223, 45.4777182, 45.4600816, 45.4302539, 45.4487613, 45.3990913, 45.4331793, 45.4955590,
               45.4418846, 45.5042041, 45.4948158, 45.5036758, 45.5203204, 45.4373609, 45.4536934, 45.4373777,
               45.4370742, 45.4509889, 45.4972552, 45.4789394, 45.4622552, 45.4184398, 45.4401682, 45.5265714,
               45.4553814, 45.4769300, 45.4848397, 45.4797591, 45.4911672, 45.5111849, 45.5179464, 45.5065037,
               45.4976348, 45.5163865, 45.4646212, 45.4929162, 45.4538568, 45.5190854, 45.5048036, 45.4649337,
               45.4699845, 45.4287264, 45.5231604, 45.4981439, 45.4559037, 45.4446635, 45.4910622, 45.5123635,
               45.4795257, 45.4992513, 45.4067542, 45.4611334, 45.4594607, 45.507878,45.5061964,  45.5188894,
               45.4568407, 45.4354349, 45.5157980, 45.4684086, 45.5107230, 45.4598303, 45.4962340, 45.4244082,
               45.4893009, 45.5018453, 45.5071510, 45.5163128, 45.5108157, 45.4045183, 45.4350595, 45.4549824,
               45.5286305, 45.4715679, 45.4321961, 45.4131957, 45.4908844, 45.5185100, 45.4431605, 45.4924291,
               45.4838657, 45.4902390, 45.4925633, 45.5296062, 45.4536158, 45.4678921, 45.4495831, 45.4913758,
               45.5101342, 45.4977392)
)

duomo = c(9.19429, 45.4641892)

# zone ha un NA, lo imputo a mano
which(is.na(data$zone))
data[2679,]
table(data$zone, useNA = 'always')
data <- mutate(data, zone = case_when(
  is.na(zone) ~ "città studi",
  TRUE ~ zone
))

dist_zone = dist_zone %>%
  rowwise() %>%
  mutate(distance_from_duomo_km = ifelse(!is.na(latitude) & !is.na(longitude),
                                         distHaversine(c(longitude, latitude), 
                                                       duomo) / 1000,
                                         NA_real_))
data = data %>%
  left_join(dist_zone, by = "zone") %>%
  mutate(distance = distance_from_duomo_km) %>%
  dplyr::select(-longitude, -latitude, -distance_from_duomo_km)

plot(data$distance, log(data$selling_price))
cor(data$distance, log(data$selling_price))


# 1. Calcolo il prezzo medio per zona
prezzi <- data %>%
  group_by(zone) %>%
  summarise(prezzo_mq = mean(selling_price/square_meters, na.rm = TRUE))

# 2. Estraggo la colonna prezzo per clustering
X <- prezzi$prezzo_mq
X_matrix <- as.matrix(X)

# 3. Applico K-Means per clusterizzare le zone in base al prezzo medio
set.seed(123)
kmeans_result <- kmeans(X_matrix, centers = 20)  

# 4. Associo il cluster a ogni zona
prezzi$cluster <- kmeans_result$cluster

print(prezzi)

data <- data %>%
  left_join(prezzi %>% dplyr::select(zone, cluster), by = "zone")


# -------------------------------------------------------------------------
### IMPUTAZIONE ### 
# -------------------------------------------------------------------------


### imputazione per square meters -----------------------------------------

# imputo i valori che prima ho messo come NA in quanto ritenute insensate
data_sq <- data %>% 
  group_by(rooms_number, bathrooms_number) %>% 
  summarize(media1 = as.integer(mean(square_meters, na.rm = T)),
            .groups = "drop")
data_sq2 <- data %>% 
  group_by(bathrooms_number) %>% 
  summarize(media2 = as.integer(mean(square_meters, na.rm = T)),
            .groups = "drop")


data <- data %>% 
  left_join(data_sq, by = c("rooms_number", "bathrooms_number"))
data <- data %>% 
  left_join(data_sq2, by = c("bathrooms_number"))


data <- data %>% 
  mutate(square_meters = case_when(
    !is.na(square_meters) ~ square_meters, 
    !is.na(media1) ~ media1,
    TRUE ~ media2
  )) %>% 
  dplyr::select(-c(media1, media2))

table(data$square_meters, useNA = 'always')

### imputazione per bathrooms number ---------------------------------------

ggplot(data, aes(x = bathrooms_number, y = square_meters)) +
  geom_boxplot(fill = "lightblue", color = "blue") 

data$bathrooms_number <- factor(data$bathrooms_number, 
                                levels = c("1", "2", "3", "3+"), ordered = T)

library(MASS)
mod_bath <- polr(bathrooms_number ~ rooms_number + square_meters,
                 data = data[!is.na(data$bathrooms_number),])
pred_bath <- predict(mod_bath, data[is.na(data$bathrooms_number),])
data$bathrooms_number[is.na(data$bathrooms_number)] <- pred_bath

table(data$bathrooms_number, useNA = 'always')



### imputazione per year of construction e availability ---------------------

# dal momento che le variabili year of construction e availability sono tra loro
# correlate potrebbero risultare problemi di multicollinearità
# imputo solo year of construction per evitare di avere dati ridondanti e 
# perché ci sono molti meno missing

ggplot(data, aes(x = as.factor(energy_efficiency_class),
                 y = year_of_construction)) +
  geom_boxplot(fill = "lightblue", color = "blue") 
ggplot(data, aes(x = as.factor(conditions), y = year_of_construction)) +
  geom_boxplot(fill = "lightblue", color = "blue") 
ggplot(data, aes(x = as.factor(total_floors_in_building),
                 y = year_of_construction)) +
  geom_boxplot(fill = "lightblue", color = "blue") 

sum <- data %>% 
  group_by(conditions, energy_efficiency_class, availability)  %>% 
  summarize(media1 = as.integer(mean(year_of_construction, na.rm = T)),
            .groups = "drop")
sum2 <- data %>% 
  group_by(conditions, energy_efficiency_class)  %>% 
  summarize(media2 = as.integer(mean(year_of_construction, na.rm = T)),
            .groups = "drop")


data <- data %>% 
  left_join(sum, by = c("conditions", "energy_efficiency_class", "availability"))
# risultano 3 NA quindi calcoliamo le medie sempre divise in gruppi ma senza
# availability così da avere un valore da imputare per quegli NA
data <- data %>% 
  left_join(sum2, by = c("conditions", "energy_efficiency_class"))

data <- data %>% 
  mutate(year_of_construction = case_when(
    !is.na(year_of_construction) ~ year_of_construction,
    !is.na(media1) ~ media1,
    TRUE ~ media2
  )) %>% 
 dplyr::select(-c(media1, media2)) 

table(data$year_of_construction, useNA = 'always')

# elimino availability in quanto come precedentemente detto troppo correlata 
# con year of construction appena imputata
data <- data %>% 
  dplyr::select(-availability) 


### imputazione per condominium fees --------------------------------------

data$condominium_fees <- as.integer(as.numeric(data$condominium_fees))
# i valori sopra 11000 sembrano insensati
which(data$condominium_fees > 12000)
# li metto come NA e li reimputo
data <- data %>% 
  mutate(condominium_fees = case_when(
    condominium_fees > 12000 ~ NA,
    TRUE ~ condominium_fees
  ))

# imputo i valori mancanti assegnando la media dividendo le case per gruppi 
# in base a total floors in building, lift e heating centralized
media3 <- data %>%
  group_by(lift, total_floors_in_building, heating_centralized) %>%
  summarize(media3 = as.integer(mean(condominium_fees, na.rm = TRUE)), 
            .groups = "drop")

# calcoliamo un'altra media nel caso in cui la media di condominium fees 
# raggruppate per le tre variabili nominate in precedenza non sia calcolabile
media4 <- data %>%
  group_by(total_floors_in_building, heating_centralized) %>%
  summarize(media4 = as.integer(mean(condominium_fees, na.rm = TRUE)), 
            .groups = "drop")

data <- data %>%
  left_join(media3, by = c("lift", "total_floors_in_building", 
                           "heating_centralized")) %>%
  left_join(media4, by = c("total_floors_in_building", 
                           "heating_centralized")) 

data <- data %>%
  mutate(condominium_fees = case_when(
    !is.na(condominium_fees) ~ condominium_fees,
    !is.na(media3) ~ media3,
    TRUE ~ media4
  )) %>%
  dplyr::select(-c(media3, media4))

table(data$condominium_fees, useNA = "always")


### imputazione per total floors in building --------------------------------

table(data$total_floors_in_building, useNA = 'always')

data$total_floors_in_building <- factor(data$total_floors_in_building, 
                                levels = c("1", "2", "3", "4", "5", "6", "7",
                                           "8", "9", "9+"), ordered = T)

mod_totfloor <- polr(total_floors_in_building ~ floor + condominium_fees +
                   distance + year_of_construction,
                 data = data[!is.na(data$total_floors_in_building),])
pred_totfloor <- predict(mod_totfloor, data[is.na(data$total_floors_in_building),])

table(pred_totfloor, useNA = 'always')
data$total_floors_in_building[is.na(data$total_floors_in_building)] <- pred_totfloor


### imputazione per lift ---------------------------------------------------

# essendo una variabile binary imputo i valori con un logit
data$lift <- as.numeric(as.factor(data$lift)) - 1
mod_lift <- glm(lift ~ total_floors_in_building, 
                family = binomial(link = logit), data[!is.na(data$lift),])

pred <- predict(mod_lift, data[is.na(data$lift),])
pred <- ifelse(pred > 0.5, 1, 0)
table(pred, useNA = 'always')
data$lift[is.na(data$lift)] <- pred


### imputazione per conditions -----------------------------------------------

table(data$conditions, useNA = 'always')
data$conditions <- as.factor(data$conditions)

mod_cond <- polr(conditions ~ year_of_construction + distance + furnished, 
                 data[!is.na(data$conditions),])
pred_cond <- predict(mod_cond, data[is.na(data$conditions),])
table(pred_cond, useNA = "always")

data$conditions[is.na(data$conditions)] <- pred_cond


### imputazione per heating centralized ------------------------------------

table(data$heating_centralized, useNA = 'always')
ggplot(data, aes(x = as.factor(heating_centralized), y = selling_price)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  ylim(0, 1000000)

data$heating_centralized <- as.numeric(as.factor(data$heating_centralized)) - 1

mod_heat <- glm(heating_centralized ~ bathrooms_number + rooms_number, 
                data = data[!is.na(data$heating_centralized),], 
                family = binomial(link = logit))
pred_heat <- predict(mod_heat, data[is.na(data$heating_centralized),])
pred_heat <- ifelse(pred_heat > 0.5, 1, 0)
table(pred_heat, useNA = 'always')
data$heating_centralized[is.na(data$heating_centralized)] <- pred_heat

# lo ritrasformo nelle categorie iniziali
data$heating_centralized <- ifelse(data$heating_centralized == 0, 'central', 
                                   'independent')
data$heating_centralized <- as.factor(data$heating_centralized)


### imputazione per energy efficiency class ---------------------------------

data$energy_efficiency_class <- factor(data$energy_efficiency_class,
                                       levels = sort(
                                         unique(data$energy_efficiency_class)))

mod_energy <- polr(energy_efficiency_class ~ bathrooms_number + conditions +
                     heating_centralized,
                   data = data[!is.na(data$energy_efficiency_class),])
pred_energy <- predict(mod_energy, data[is.na(data$energy_efficiency_class),])
data$energy_efficiency_class[is.na(data$energy_efficiency_class)] <- pred_energy

table(data$energy_efficiency_class, useNA = 'always')


### imputazione per exposure -----------------------------------------------

ggplot(data, aes(x = as.factor(exposure), y = selling_price)) +
  geom_boxplot(fill = "lightblue", color = "blue")
# dai boxplot le categorie internal e external mi sembrano avere più o meno lo 
# stesso prezzo mediano e la stessavariabilità quindi per comodità riduco le 
# categorie a due: double e single 

data <- data %>%
  mutate(exposure = case_when(
    exposure == 'double' ~ 'double',
    exposure %in% c('external', 'internal') ~ 'single',
    TRUE ~ NA
  ))

# ora imputiamo con un GLM visto che la risposta è a due categorie
data$exposure <- as.numeric(as.factor(data$exposure)) - 1
# 1 quando è single 0 quando è double 

mod_exp <- glm(exposure ~ square_meters + rooms_number + bathrooms_number +
                 energy_efficiency_class, 
               data = data[!is.na(data$exposure),], 
               family = binomial(link = logit))
pred_exp <- predict(mod_exp, data[is.na(data$exposure),])
pred_exp <- ifelse(pred_exp > 0.5, 1, 0)
table(pred_exp, useNA = 'always')
data$exposure[is.na(data$exposure)] <- pred_exp
table(data$exposure, useNA = 'always')

data <- data %>%
  mutate(exposure = case_when(
    exposure == 0 ~ 'double',
    exposure == 1 ~ 'single',
    TRUE ~ NA
  ))


### elimino e trasformo variabili ------------------------------------------

skim(data)

ggplot(data, aes(x = `optic fiber`, y = selling_price)) +
    geom_boxplot(fill = "lightblue", color = "blue")
ggplot(data, aes(x = `electric gate`, y = selling_price)) +
  geom_boxplot(fill = "lightblue", color = "blue")
# queste sue variabili presentano differenze impercettibili 
# di prezzo tra i 2 gruppi

ggplot(data, aes(x = `security door`, y = selling_price)) +
  geom_boxplot(fill = "lightblue", color = "blue")
ggplot(data, aes(x = `video entryphone`, y = selling_price)) +
  geom_boxplot(fill = "lightblue", color = "blue")
ggplot(data, aes(x = `alarm system`, y = selling_price)) +
  geom_boxplot(fill = "lightblue", color = "blue")
# metto insieme le tre variabili security door video entryphone e alarm system
# in una variabile che mi indica il livello di sicurezza della casa
data$`alarm system` <- as.numeric(data$`alarm system`) - 1
data$`video entryphone` <- as.numeric(data$`video entryphone`) - 1
data$`security door` <- as.numeric(data$`security door`) - 1

data <- data %>%
  mutate(security_level = rowSums(across(c(`alarm system`, `video entryphone`,
                                           `security door`))))
table(data$security_level)

data <- dplyr::select(data, 
                      -c(`alarm system`, `video entryphone`, `security door`))

ggplot(data, aes(x = pool, y = selling_price)) +
  geom_boxplot(fill = "lightblue", color = "blue")
ggplot(data, aes(x = `tennis court`, y = selling_price)) +
  geom_boxplot(fill = "lightblue", color = "blue")
ggplot(data, aes(x = `concierge/reception`, y = selling_price)) +
  geom_boxplot(fill = "lightblue", color = "blue") # decisiva
ggplot(data, aes(x = exposure, y = selling_price)) +
  geom_boxplot(fill = "lightblue", color = "blue") 
# questa può essere eliminata, l'unica categoria che sembra un minimo spostare
# la risposta è double ma ho messo le case in cui non er indicata l'exposure

ggplot(data, aes(x = garden, y = selling_price)) +
  geom_boxplot(fill = "lightblue", color = "blue")
data$garden <- ifelse(data$garden == 'private and shared', 'private',
                      data$garden)
# le due categorie private e private e shared sembravano più o meno simili dai
# boxplot quindi le ho unite

ggplot(data, aes(x = `balcony/terrace`, y = selling_price)) +
  geom_boxplot(fill = "lightblue", color = "blue")

ggplot(data, aes(x = wood_frames, y = selling_price)) +
  geom_boxplot(fill = "lightblue", color = "blue")
ggplot(data, aes(x = single_glass, y = selling_price)) +
  geom_boxplot(fill = "lightblue", color = "blue")

data <- dplyr::select(data, -c(single_glass))
data <- dplyr::select(data, -c(`electric gate`))

ggplot(data, aes(x = tavern, y = selling_price)) +
  geom_boxplot(fill = "lightblue", color = "blue")
ggplot(data, aes(x = cellar, y = selling_price)) +
  geom_boxplot(fill = "lightblue", color = "blue")
ggplot(data, aes(x = fireplace, y = selling_price)) +
  geom_boxplot(fill = "lightblue", color = "blue")

# faccio la stessa cosa fatta per il livello di sicurezza per il livello di 
# beni di lusso della casa 
data$pool <- as.numeric(data$pool) - 1
data$closet <- as.numeric(data$closet) - 1
data$`tennis court` <- as.numeric(data$`tennis court`) - 1
data$hydromassage <- as.numeric(data$hydromassage) - 1
data$`optic fiber` <- as.numeric(data$`optic fiber`) - 1
data$attic <- as.numeric(data$attic) - 1

data <- data %>%
  mutate(other_features = rowSums(across(c(pool, closet,`tennis court`, 
                                         hydromassage, attic, `optic fiber`))),
         other_features = factor(other_features, 
                               levels = 0:6, ordered = T))
table(data$other_features)

data$other_features <- ifelse(data$other_features >= 2, "1+",
                            as.character(data$other_features))

data$other_features <- factor(data$other_features, 
                            levels = c("0", "1", "1+"), ordered = T)
table(data$other_features)

data <- dplyr::select(data, -c(pool, closet, `tennis court`, hydromassage, 
                               attic, `optic fiber`))

write.csv(data.frame(data), "training clean.csv", row.names = FALSE)


# grafico delle zone clusterizzate 
library(sf)
library(viridis)

milano_map <- st_read("quartieri.geojson")

cluster_coords <- prezzi %>%
  left_join(dist_zone, by = "zone") %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

ggplot() +
  geom_sf(data = milano_map, fill = "white", color = "grey70") +
  geom_sf(data = cluster_coords, aes(color = as.factor(cluster)), size = 3) +
  geom_sf(data = st_sfc(st_point(c(9.19429, 45.4641892)), crs = 4326),
          color = "red", shape = 8, size = 4) +
  scale_color_viridis_d(option = "turbo", name = "cluster prezzo") +
  labs(title = "cluster delle zone di Milano in base al prezzo al mq") +
  theme_minimal()


