library(tidyverse)
library(mirt)
library(plotly)
library(plyr)
library(readxl)
#library(basicspace)
mirtCluster()

load("Sweden2010.Rda")
#Data finns här: http://193.11.44.161/?q=nytt-dataset-view
#url <- "http://193.11.44.161/sites/default/files/enk%C3%A4t.xlsx"
#destfile <- "enk_C3_A4t.xlsx"
#download.file(url, destfile)
data_orig <- read_excel("enk_C3_A4t.xlsx")
names(data_orig)[156] <- "person_id2"
#url <- "http://193.11.44.161/sites/default/files/valsedlar.xlsx"
#destfile <- "persons.xlsx"
#download.file(url, destfile)
persons_orig <- read_excel("persons.xlsx", col_types = c("numeric", 
                                                   "text", "text", "text", "date", "text", 
                                                   "text", "text", "numeric"))
rm(destfile)
rm(url)
persons <- persons_orig %>% 
  mutate(namn = paste(`förnamn`, `efternamn`)) %>% 
  select(id, namn, parti) %>% 
  unique()

valkompass <- data_orig %>% 
  mutate(namn = paste(`förnamn`, `efternamn`)) %>% 
  left_join(persons) %>% 
  transmute(Id = id, 
            Namn = namn, 
            Parti = parti,
            `Var någonstans skulle du placera dig själv på en politisk vänster-högerskala?` = q_170_hoger_vanster,
            `1. Trängselavgifter ska införas i fler storstäder` = q_19_trangselskatt,
            `2. Sverige ska satsa på höghastighetståg` = q_22_hoghastighetstag,
            `3. Sverige ska ha licensjakt på varg` = q_25_vargjakt,
            `4. Kärnkraften ska byggas ut` = q_28_karnkraft,
            `5. Bensinskatten ska höjas` = q_31_bensinskatt,
            `6. Det ska vara fri entré till statliga museer` = q_34_museer,
            `7. Upphovsrättsskyddat material från internet ska fritt få laddas ner för eget bruk` = q_37_nedladdning,
            `8. TV-licensen ska avskaffas och ersättas med en obligatorisk skatt` = q_40_tvlicensen,
            `9. Ingen kommun ska kunna säga nej till att ta emot flyktingar` = q_43_flyktingmottagandet,
            `10. Färre asylsökande ska få stanna i Sverige` = q_46_asylsokande,
            `11. Papperslösa flyktingar ska få fri vård` = q_49_vard_t_papperslosa,
            `12. Det ska vara förbjudet för lärare att bära heltäckande slöja i svenska skolor` = q_52_skolsloja,
            `13. En större del av föräldraledigheten ska reserveras för papporna` = q_55_pappaledigt,
            `14. "Positiv särbehandling" vid antagning till högskolan ska avskaffas` = q_58_positiv_sarbehandling,
            `15. Vårdnadsbidraget ska avskaffas` = q_61_vardnadsbidraget,
            `16. Fastighetsskatten ska höjas för villor med taxeringsvärde över 4,5 miljoner` = q_64_fastighetsskatten,
            `17. "Förmögenhetsskatten" ska återinföras` = q_67_formogenhetsskatten,
            `18. Skatten för höginkomstagare ska höjas` = q_70_hoginkomstskatten,
            `19. Pension och lön ska beskattas lika` = q_73_beskattn_av_pension,
            `20. "RUT-avdraget" ska behållas` = q_76_rut,
            `21. Skatten på arbete ska sänkas` = q_79_arbeteskatten,
            `22. Sexköpslagen ska avskaffas` = q_82_sexkopslagen,
            `23. Straffet för rattfylleri ska skärpas` = q_85_rattfylleri,
            `24. Våldsbrott ska bestraffas hårdare` = q_88_valdsbrott,
            `25. "FRA-lagen" ska rivas upp` = q_91_fra,
            `26. Den som är 58 år ska kunna få förtida pension` = q_94_fortida_pension,
            `27. Sjukförsäkringen ska vara tidsbegränsad` = q_97_sjukforsakringen,
            `28. Tandvård ska ingå i sjukförsäkringen` = q_100_tandvard,
            `29. Det ska vara mer konkurrens och valfrihet i offentlig verksamhet` = q_103_konkurrens_i_offentl,
            `30. A-kassan ska vara obligatorisk` = q_106_obl_akassa,
            `31. Kommunernas hemtjänst ska kosta lika mycket oavsett var man bor` = q_109_lika_hemtjanst,
            `32. Svensk sjukvård ska erbjuda omskärelse enbart av medicinska skäl` = q_112_omskarelse,
            `33. Privata vårdföretag som drivs med skattemedel ska kunna ge vinst till ägarna` = q_115_privata_vardforetag,
            `34. Alkoholmonopolet ska avskaffas` = q_118_alkoholmonopol,
            `35. Det ska bli svårare att använda bemanningsföretag för att kringgå LAS` = q_121_kringga_las,
            `36. Statliga bolag ska inte säljas` = q_124_utforsaljning,
            `37. Svenska soldater ska snarast tas hem från Afghanistan` = q_127_afghanistan,
            `38. Sveriges vapenexport ska upphöra` = q_130_vapenexport,
            `39. Biståndet ska inte gå till odemokratiska länder` = q_133_bistand,
            `40. Den militära värnplikten ska återinföras` = q_136_varnplikt,
            `41. Sverige ska lämna EU` = q_139_eu,
            `42. Stökiga elever ska kunna flyttas från sin skola mot elevens och föräldrarnas vilja` = q_142_stokiga_elever,
            `43. Den kommunala skolan ska förstatligas` = q_145_forstatliga_skolan,
            `44. Betyg ska införas från årskurs 6` = q_148_betyg,
            `45. Antalet friskolor ska begränsas` = q_151_friskolor,
            `46. Alla gymnasieprogram ska leda till högskolebehörighet` = q_154_hogskolebehorighet,
            `47. Det ska vara straffbart att vara med i rasistiska organisationer` = q_157_rasistiska_org,
            `48. Monarkin ska avskaffas` = q_160_monarkin,
            `49. Personvalets betydelse i Sverige ska stärkas` = q_163_personval,
            `50. Det ska hållas fler folkomröstningar i Sverige` = q_166_folkomrostning)

valkompass[4] <- valkompass[4] %>% as.matrix() %>% factor(levels=c("Klart till vänster", 
                                                                   "Något till vänster", 
                                                                   "Varken vänster eller höger", 
                                                                   "Något till höger",
                                                                   "Klart till höger"),ordered=T)
for(i in 5:54){
  valkompass[i] <- valkompass[i] %>% as.matrix() %>% factor(levels=c("Mycket dåligt förslag",
                                                                     "Ganska dåligt förslag",
                                                                     "Ingen åsikt",
                                                                     "Ganska bra förslag",
                                                                     "Mycket bra förslag"), ordered=T)
}

valkompass[149,3] <- "Centerpartiet"
valkompass[2435,3] <- "Sverigedemokraterna"

#attr(Sweden2010, "label.table")$kat
names(Sweden2010)[6:56] <- names(valkompass)[4:54]
for(i in 7:56){
  Sweden2010[i]<-Sweden2010[i] %>% 
    as.matrix() %>%
    factor(
      labels = c(
        "Mycket dåligt förslag",
        "Ganska dåligt förslag",
        "Ganska bra förslag",
        "Mycket bra förslag",
        "Ingen åsikt"
      ),
      levels = c(1, 2, 3, 4, 8),
      ordered = F
    ) %>%
    factor(
      levels = c(
        "Mycket dåligt förslag",
        "Ganska dåligt förslag",
        "Ingen åsikt",
        "Ganska bra förslag",
        "Mycket bra förslag"
      ),
      ordered = T
    )
}

Sweden2010[6] <-  Sweden2010[6] %>% as.matrix() %>% factor(
  labels = c("Klart till vänster", 
             "Något till vänster", 
             "Varken vänster eller höger", 
             "Något till höger",
             "Klart till höger"),
  levels = c(1, 2, 3, 4, 8),
  ordered = T
)
names(valkompass)[1]<-"id"

names(Sweden2010)[2] <- "vald"

Sweden2010 %<>% select(-party.name, -party.code,-govt.party)

left_right <- Sweden2010 %>% select(id, `Var någonstans skulle du placera dig själv på en politisk vänster-högerskala?`) %>% 
  na.omit %>% 
  full_join(valkompass %>% transmute(id, `Var någonstans skulle du placera dig själv på en politisk vänster-högerskala?`) %>% na.omit)

valkompass[,5:54] %<>% replace_na(list(`26. Den som är 58 år ska kunna få förtida pension` = "Ingen åsikt", `41. Sverige ska lämna EU`="Ingen åsikt"))

#names(Sweden2010)[2] <- "Namn"
#names(Sweden2010)[3] <- "Parti"

names <- persons %>% filter(parti!="Åsele kommunlista") %>%  transmute(id, Namn=namn, Parti=parti)
parties <- persons %>% filter(parti!="Åsele kommunlista") %>%  transmute(id, Namn=namn, Parti=parti)

output <- Sweden2010 %>% select(id, -`Var någonstans skulle du placera dig själv på en politisk vänster-högerskala?`) %>% 
  left_join(persons %>% filter(parti!="Åsele kommunlista") %>%  transmute(id, Namn=namn, Parti=parti)) %>% 
  left_join(Sweden2010 %>% select(-`Var någonstans skulle du placera dig själv på en politisk vänster-högerskala?`)) %>% 
  full_join(valkompass %>% select(-`Var någonstans skulle du placera dig själv på en politisk vänster-högerskala?`)) %>% 
  full_join(left_right)

#valkompass[valkompass$id%in%c(10264, 44328),] %>% View

#output$id %>% table %>% as.data.frame() %>% dplyr::filter(Freq>1)
#output[output$id%in%c(10264, 44328),c(7,30)] %>% View

output[output$id==4478,]$Parti <- c("Centerpartiet", "Centerpartiet")
output[output$id==4478,]$Namn <- c("Sylvia Samuelsson", "Sylvia Samuelsson")
output[output$id==45635,]$Parti <- c("Sverigedemokraterna", "Sverigedemokraterna")
output[output$id==45635,]$Namn <- c("William Petzäll", "William Petzäll")
output[output$id==4478,]$`vald` <- 0
output[output$id==10264,]$`vald` <- 1
output[output$id==44328,]$`vald` <- 0
output[output$id==45635,]$`vald` <- 1
output[output$id==10264,]$`3. Sverige ska ha licensjakt på varg` <- "Mycket dåligt förslag"
output[output$id==44328,]$`26. Den som är 58 år ska kunna få förtida pension` <- "Ingen åsikt"

output <- unique(output)

output <- output[output$id!= 41986,] #NA name, not elected

output <- output %>% 
  select(id, 
         Namn, 
         Parti, 
         `vald`,
         `Var någonstans skulle du placera dig själv på en politisk vänster-högerskala?`,
         `1. Trängselavgifter ska införas i fler storstäder`,
         `2. Sverige ska satsa på höghastighetståg`,
         `3. Sverige ska ha licensjakt på varg`,
         `4. Kärnkraften ska byggas ut`,
         `5. Bensinskatten ska höjas`,
         `6. Det ska vara fri entré till statliga museer`,
         `7. Upphovsrättsskyddat material från internet ska fritt få laddas ner för eget bruk`,
         `8. TV-licensen ska avskaffas och ersättas med en obligatorisk skatt`,
         `9. Ingen kommun ska kunna säga nej till att ta emot flyktingar`,
         `10. Färre asylsökande ska få stanna i Sverige`,
         `11. Papperslösa flyktingar ska få fri vård`,
         `12. Det ska vara förbjudet för lärare att bära heltäckande slöja i svenska skolor`,
         `13. En större del av föräldraledigheten ska reserveras för papporna`,
         `14. "Positiv särbehandling" vid antagning till högskolan ska avskaffas`,
         `15. Vårdnadsbidraget ska avskaffas`,
         `16. Fastighetsskatten ska höjas för villor med taxeringsvärde över 4,5 miljoner`,
         `17. "Förmögenhetsskatten" ska återinföras`,
         `18. Skatten för höginkomstagare ska höjas`,
         `19. Pension och lön ska beskattas lika`,
         `20. "RUT-avdraget" ska behållas`,
         `21. Skatten på arbete ska sänkas`,
         `22. Sexköpslagen ska avskaffas`,
         `23. Straffet för rattfylleri ska skärpas`,
         `24. Våldsbrott ska bestraffas hårdare`,
         `25. "FRA-lagen" ska rivas upp`,
         `26. Den som är 58 år ska kunna få förtida pension`,
         `27. Sjukförsäkringen ska vara tidsbegränsad`,
         `28. Tandvård ska ingå i sjukförsäkringen`,
         `29. Det ska vara mer konkurrens och valfrihet i offentlig verksamhet`,
         `30. A-kassan ska vara obligatorisk`,
         `31. Kommunernas hemtjänst ska kosta lika mycket oavsett var man bor`,
         `32. Svensk sjukvård ska erbjuda omskärelse enbart av medicinska skäl`,
         `33. Privata vårdföretag som drivs med skattemedel ska kunna ge vinst till ägarna`,
         `34. Alkoholmonopolet ska avskaffas`,
         `35. Det ska bli svårare att använda bemanningsföretag för att kringgå LAS`,
         `36. Statliga bolag ska inte säljas`,
         `37. Svenska soldater ska snarast tas hem från Afghanistan`,
         `38. Sveriges vapenexport ska upphöra`,
         `39. Biståndet ska inte gå till odemokratiska länder`,
         `40. Den militära värnplikten ska återinföras`,
         `41. Sverige ska lämna EU`,
         `42. Stökiga elever ska kunna flyttas från sin skola mot elevens och föräldrarnas vilja`,
         `43. Den kommunala skolan ska förstatligas`,
         `44. Betyg ska införas från årskurs 6`,
         `45. Antalet friskolor ska begränsas`,
         `46. Alla gymnasieprogram ska leda till högskolebehörighet`,
         `47. Det ska vara straffbart att vara med i rasistiska organisationer`,
         `48. Monarkin ska avskaffas`,
         `49. Personvalets betydelse i Sverige ska stärkas`,
         `50. Det ska hållas fler folkomröstningar i Sverige`)




#Sweden2010 %>% full_join(valkompass) %>% xlsx::write.xlsx("output.xlsx")
rm(data_orig)
rm(valkompass)
rm(i)
rm(left_right)
rm(persons)
rm(persons_orig)
rm(Sweden2010)
rm(parties)
rm(names)
#survey <-

survey <- output[,6:55] %>% data.matrix() %>% as.data.frame()
#survey[survey==3] <- NA

for(i in 1:50){
  survey[,i] <- survey[,i] %>% as.numeric()
}


mirt_obj <- mirt(survey, model = 2, itemtype = "nominal")

# Alternativt: mirt_obj <- readRDS("mirt_object.RDS")

scores <- fscores(mirt_obj, rotate="entropy") %>% as.data.frame


#survey_blackbox <- blackbox(survey, dims=2, minscale=20)

#survey_blackbox$individuals[[2]]

candidates <- c(output, scores) %>% as_tibble
#candidates <- c(valkompass, as_tibble(mirt_scores)) %>% as_tibble
#candidates[, 56] = -1 * candidates[, 56]

candidates[candidates$Parti=="Arbetarepartiet-Socialdemokraterna",]$Parti <- "Socialdemokraterna"
candidates[candidates$Parti=="Moderata Samlingspartiet",]$Parti <- "Moderaterna"
candidates[candidates$Parti=="Miljöpartiet de gröna",]$Parti <- "Miljöpartiet"
candidates[candidates$Parti=="Folkpartiet liberalerna",]$Parti <- "Folkpartiet"
#candidates %<>% mutate(c1=c1*-1)



candidates[candidates[, 56:57] %>% duplicated() %>% which(), ] <-
  candidates[candidates[, 56:57] %>% duplicated() %>% which(), ] %>% 
  mutate(F1 = jitter(F1, factor = 0.1, 0.1), 
         F2 = jitter(F2, factor = 0.1, 0.1))

#names(candidates)[56:57] <- c("F1","F2")

joined_dataset <- candidates
rm(candidates)
rm(survey)
rm(scores)
rm(output)
rm(i)
rm(mirt_obj)
save.image(".RData")
