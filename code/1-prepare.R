#--------------------------------------------------------------------
# Voter Turnout and Income Inequality in Canada
# Bruno St-Jacques (Hertie School of Governance)
# 2019
#--------------------------------------------------------------------

# FEDS 2015
income2015_all <- read.csv("./FEDS_2015/table1a-eng.csv")
income2015_incomecat <- read.csv("./FEDS_2015/table1b-eng.csv")
income2015_male <- read.csv("./FEDS_2015/table1c-eng.csv")
income2015_female <- read.csv("./FEDS_2015/table1d-eng.csv")
income2015_source <- read.csv("./FEDS_2015/table1e-eng.csv")
income2015_age <- read.csv("./FEDS_2015/table2-eng.csv")
income2015_TFSA <- read.csv("./FEDS_2015/table3-eng.csv")
income2015_taxcredit <- read.csv("./FEDS_2015/table4-eng.csv")
income2015_childbenefit <- read.csv("./FEDS_2015/table5-eng.csv")

censusincgroup <- read.csv("./census2016incomedata.csv")

fed_area <- as.data.frame(read_xlsx("./FED_area.xlsx"))
pop_density_census <- read.csv("./Elections-Can/pop_density_census.csv")
# https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/pd-pl/comprehensive.cfm

# turnout and candidates 2015 general elections
# https://open.canada.ca/data/en/dataset/775f3136-1aa3-4854-a51e-1a2dab362525

turnout2015 <- read.csv("./Elections-CAN/table_tableau11.csv")
names(turnout2015)[names(turnout2015) == 'Electoral.District.Name.Nom.de.circonscription'] <- 'Federal.Electoral.Districts'
names(turnout2015)[names(turnout2015) == 'Electoral.District.Number.Numéro.de.circonscription'] <- 'FED.ID'

candidates2015 <- read.csv("./Elections-CAN/table_tableau12.csv")
names(candidates2015)[names(candidates2015) == 'Electoral.District.Name.Nom.de.circonscription'] <- 'Federal.Electoral.Districts'
names(candidates2015)[names(candidates2015) == 'Electoral.District.Number.Numéro.de.circonscription'] <- 'FED.ID'

majority <- candidates2015 %>% 
  subset(Majority.Percentage.Pourcentage.de.majorité!="NA") %>% 
  select(FED.ID, Majority.Percentage.Pourcentage.de.majorité, Percentage.of.Votes.Obtained..Pourcentage.des.votes.obtenus) %>% 
  dplyr::rename(FED.ID = FED.ID,majority = Majority.Percentage.Pourcentage.de.majorité, votes_perc = Percentage.of.Votes.Obtained..Pourcentage.des.votes.obtenus)

# gini of every FED
cat2015 <- income2015_incomecat
cat2015 <- cat2015[,-c(3,4,5,25) ]
cat2015_10001 <- cat2015[1,]
cat2015_10001 <- cat2015_10001[,-1]
Gini(cat2015_10001)
cat2015$gini <- apply(cat2015[,-1], 1, Gini)
turn2015 <- turnout2015[,c(3,12)]
names(turn2015)[names(turn2015) == 'Percentage.of.Voter.Turnout.Pourcentage.de.la.participation.électorale'] <- 'turnout'
giniturn2015 <- inner_join(turn2015, cat2015)
giniturn2015 <- giniturn2015[,c(1,2,22)]


# cleaner 
dat1 <- income2015_incomecat %>% 
  mutate(average_inc = (Total.Income / Total)) # average income per household
dat1 <- dat1 %>% # gini
  mutate(gini = apply(dat1[, 6:24], 1, Gini))

taxcredit <- as.data.frame(select(income2015_taxcredit, X, Total.of.GST.HST.Credit.Recipients, X.2))
taxcredit <- taxcredit[-1,]
taxcredit <- taxcredit %>% 
  dplyr::rename(FED.ID = X, taxcredit_pop = Total.of.GST.HST.Credit.Recipients, taxcredit_sum = X.2) # tax credit
indx <- sapply(taxcredit, is.factor)
taxcredit[indx] <- lapply(taxcredit[indx], function(x) as.numeric(as.character(x)))
popincome2015 <- select(income2015_all, FED.ID, Total, Total.Income) # source
taxcredit <- inner_join(taxcredit, popincome2015)
taxcredit <- subset(taxcredit, FED.ID!="NA") # remove province total
taxcredit <- taxcredit %>% 
  mutate(creditpercent_pop = (taxcredit_pop / Total) * 100,
         creditpercent_inc = (taxcredit_sum / Total.Income) * 100)
dat1 <- inner_join(dat1, taxcredit)


source2015 <- income2015_source # source
source2015$Prov.Terr <- as.character(source2015$Prov.Terr)
source2015 <- subset(source2015, Prov.Terr!="TOTAL") # remove province total
source2015 <- inner_join(source2015, popincome2015)
source2015 <- source2015 %>% 
  mutate(emp_pop = (Employment.Income../Total) * 100,
         emp_inc = (Employment.Income...1/Total.Income) * 100,
         pension_pop = (Pension.Income../Total) * 100,
         pension_inc = (Pension.Income...1/Total.Income) * 100,
         invest_pop = (Investment.Income../Total) * 100,
         invest_inc = (Investment.Income...1/Total.Income) * 100,
         self_pop = (Self.employment.Income../Total) * 100,
         self_inc = (Self.employment.Income...1/Total.Income) * 100,
         benefits_pop = (Benefits.Income../Total) * 100,
         benefits_inc = (Benefits.Income...1/Total.Income) * 100,
         other_pop = (Other.Income../Total) * 100,
         other_inc = (Other.Income...1/Total.Income) * 100)

source20151 <-source2015[, c(2,26,27,29,31,33,35,37,39)]
dat2 <- dat1[, c(1,2,26,27,30,31)]
dat3 <- inner_join(dat2, source20151)

turn2015 <- turnout2015[,c(3,12)] # turnout
names(turn2015)[names(turn2015) == 'Percentage.of.Voter.Turnout.Pourcentage.de.la.participation.électorale'] <- 'turnout'
dat4 <- inner_join(dat3, turn2015)
dat4 <- inner_join(dat4, majority)

income2015_female <- income2015_female %>%  # female 
  dplyr::rename(Female.Total.Income = Total.Income, Female.Total = Total) 
income2015_female <- subset(income2015_female, Prov.Terr!="TOTAL") # remove province total
female <- inner_join(income2015_female, popincome2015)
female <- female %>% 
  mutate(fem_pop = (Female.Total / Total) * 100,
         fem_inc = (Female.Total.Income / Total.Income) * 100)
female <- female[,c(2,28,29)]         
dat5 <- inner_join(dat4, female)

income2015_male <- income2015_male %>% # male
  dplyr::rename(Male.Total.Income = Total.Income, Male.Total = Total) 
income2015_male <- subset(income2015_male, Prov.Terr!="TOTAL") # remove province total
male <- inner_join(income2015_male, popincome2015)
male <- male %>% 
  mutate(male_pop = (Male.Total / Total) * 100,
         male_inc = (Male.Total.Income / Total.Income) * 100)
male <- male[ ,c(2,28,29)]
dat6 <- inner_join(dat5, male)

# percentage pop per income bracket
incomecat <- income2015_incomecat %>% 
  mutate(under5k_pop_perc = (Under..5.000/Total)*100,
         x.5to10k_pop_perc = (X.5.000.to..9.999/Total)*100,
         x.10to15k_pop_perc = (X.10.000.to..14.999/Total)*100,
         x.15to20k_pop_perc = (X.15.000.to..19.999/Total)*100,
         x.20to25k_pop_perc = (X.20.000.to..24.999/Total)*100,
         x.25to30k_pop_perc = (X.25.000.to..29.999/Total)*100,
         x.30to35k_pop_perc = (X.30.000.to..34.999/Total)*100,
         x.35to40k_pop_perc = (X.35.000.to..39.999/Total)*100,
         x.40to45k_pop_perc = (X.40.000.to..44.999/Total)*100,
         x.45to50k_pop_perc = (X.45.000.to..49.999/Total)*100,
         x.50to55k_pop_perc = (X.50.000.to..54.999/Total)*100,
         x.55to60k_pop_perc = (X.55.000.to..59.999/Total)*100,
         x.60to70k_pop_perc = (X.60.000.to..69.999/Total)*100,
         x.70to80k_pop_perc = (X.70.000.to..79.999/Total)*100,
         x.80to90k_pop_perc = (X.80.000.to..89.999/Total)*100,
         x.90to100k_pop_perc = (X.90.000.to..99.999/Total)*100,
         x.100to150k_pop_perc = (X.100.000.to..149.999/Total)*100,
         x.150to250k_pop_perc = (X.150.000.to..249.999/Total)*100,
         x.250andup_pop_perc = (X.250.000.and.over/Total)*100)
incomecat <- incomecat %>% 
  mutate(under5k_mean = (2500),
         x.5to10k_mean = (7500),
         x.10to15k_mean = (12500),
         x.15to20k_mean = (17500),
         x.20to25k_mean = (22500),
         x.25to30k_mean = (27500),
         x.30to35k_mean = (32500),
         x.35to40k_mean = (37500),
         x.40to45k_mean = (42500),
         x.45to50k_mean = (47500),
         x.50to55k_mean = (52500),
         x.55to60k_mean = (57500),
         x.60to70k_mean = (65000),
         x.70to80k_mean = (75000),
         x.80to90k_mean = (85000),
         x.90to100k_mean = (95000),
         x.100to150k_mean = (125000),
         x.150to250k_mean = (200000),
         x.250andup_mean = (300000))

incomecat <- incomecat %>% 
  mutate(under5k_total_mean = (Under..5.000)*2500,
         x.5to10k_total_mean = (X.5.000.to..9.999)*7500,
         x.10to15k_total_mean = (X.10.000.to..14.999)*12500,
         x.15to20k_total_mean = (X.15.000.to..19.999)*17500,
         x.20to25k_total_mean = (X.20.000.to..24.999)*22500,
         x.25to30k_total_mean = (X.25.000.to..29.999)*27500,
         x.30to35k_total_mean = (X.30.000.to..34.999)*32500,
         x.35to40k_total_mean = (X.35.000.to..39.999)*37500,
         x.40to45k_total_mean = (X.40.000.to..44.999)*42500,
         x.45to50k_total_mean = (X.45.000.to..49.999)*47500,
         x.50to55k_total_mean = (X.50.000.to..54.999)*52500,
         x.55to60k_total_mean = (X.55.000.to..59.999)*57500,
         x.60to70k_total_mean = (X.60.000.to..69.999)*65000,
         x.70to80k_total_mean = (X.70.000.to..79.999)*75000,
         x.80to90k_total_mean = (X.80.000.to..89.999)*85000,
         x.90to100k_total_mean = (X.90.000.to..99.999)*95000,
         x.100to150k_total_mean = (X.100.000.to..149.999)*125000,
         x.150to250k_total_mean = (X.150.000.to..249.999)*200000,
         x.250andup_total_mean = (X.250.000.and.over)*300000)

incomecat <- incomecat %>% 
  mutate(total_mean = under5k_total_mean + 
           x.5to10k_total_mean + 
           x.10to15k_total_mean + 
           x.15to20k_total_mean + 
           x.20to25k_total_mean + 
           x.25to30k_total_mean + 
           x.30to35k_total_mean + 
           x.35to40k_total_mean + 
           x.40to45k_total_mean + 
           x.45to50k_total_mean + 
           x.50to55k_total_mean + 
           x.55to60k_total_mean + 
           x.60to70k_total_mean + 
           x.70to80k_total_mean + 
           x.80to90k_total_mean + 
           x.90to100k_total_mean + 
           x.100to150k_total_mean + 
           x.150to250k_total_mean + 
           x.250andup_total_mean)

# 20:20 ratio
incomecat_percent <- incomecat[,c(2,26:44)]
percmean <- as.data.frame(colMeans(incomecat_percent[,-1]))

incper <- as.data.frame(colMeans(incomecat_percent[,-1]))
incper[1,] + incper[2,] + incper[3,]
incper[19, ] + incper[18,] + incper[17,] + incper[16,] + incper[15, ] + incper[14, ] + incper[13,]
incmean <- incomecat[,c(2,45:83)]
incmean <- incmean %>% 
  mutate(bottom20 = under5k_total_mean + x.5to10k_total_mean + x.10to15k_total_mean,
         top20 = x.250andup_total_mean + x.150to250k_total_mean + x.100to150k_total_mean + x.90to100k_total_mean + x.80to90k_total_mean + x.70to80k_total_mean + x.60to70k_total_mean)
incmean <- incmean %>% 
  mutate(ratio20 = top20 / bottom20)

dat6 <- inner_join(dat5, incmean[,c(1,43)])

income_total <- select(incomecat, c(1,2,5,83))  
income_total <- income_total %>% 
  mutate(total_diff = total_mean - Total.Income)
var(income_total$Total.Income)
var(income_total$total_mean)

# 20k income brackets
income20k <- income2015_incomecat %>% 
  mutate(x.0to20 = Under..5.000 +
           X.5.000.to..9.999 +
           X.10.000.to..14.999 +
           X.15.000.to..19.999,
         x.20to40 = X.20.000.to..24.999 +
           X.25.000.to..29.999 +
           X.30.000.to..34.999 +
           X.35.000.to..39.999,
         x.40to60 =  X.40.000.to..44.999 +
           X.45.000.to..49.999 +
           X.50.000.to..54.999 +
           X.55.000.to..59.999,
         x.60to80= X.60.000.to..69.999 +
           X.70.000.to..79.999,
         x.80to100 = X.80.000.to..89.999 +
           X.90.000.to..99.999,
         x.100andup =  X.100.000.to..149.999 +
           X.150.000.to..249.999 +
           X.250.000.and.over)

income20k <- income20k %>% # gini
  mutate(gini20k = apply(income20k[, 25:31], 1, Gini))
income20k <- income20k %>% 
  mutate(giniindex = gini20k * 100)
dat6 <- inner_join(dat6, income20k)

age <- income2015_age # age
age <- age %>% 
  mutate(ageYouth = age$Under.20 + age$X20.24,
         ageAdult = age$X25.29 + age$X30.34 + age$X35.39 + age$X40.44 + age$X45.49 + age$X50.54 + age$X55.59 + age$X60.64,
         ageSenior = age$X65.69 + age$X70.74 + age$Over.75)
age <- age %>% 
  mutate(ageTotal = ageYouth + ageAdult + ageSenior)
age <- age %>% 
  mutate(percYouth = ageYouth / Total *100,
         percAdult = ageAdult/Total *100,
         percSenior = ageSenior/Total*100)
age <- subset(age, Prov.Terr!="TOTAL") # remove province total
age_df <- age[,c(2,30:32)]
dat7 <- inner_join(dat6, age_df)

tfsa <- income2015_TFSA %>% 
  dplyr::rename(FED.ID = X)
tfsa <- subset(tfsa, Summary.Statistics.of.TFSA.by.Federal.Electoral.Districts!="TOTAL") # remove province total
tfsa <- tfsa[-1,]
tfsa$FED.ID <- as.numeric(as.character(tfsa$FED.ID))
tfsa <- inner_join(tfsa, popincome2015)
tfsa$X.2 <- as.numeric(as.character(tfsa$X.2))
tfsa <- mutate(tfsa, percTFSA = (X.2 / Total)*100)
tfsa_perc <- tfsa[,c(2,28)]
dat8 <- inner_join(dat7, tfsa_perc)

# compare inequality measures
gini_compare <- select(dat8, FED.ID, gini, gini20k, turnout)
gini_compare <- mutate(gini_compare, diff = gini20k - gini)
mean(gini_compare$diff)
ineq_compare <- inner_join(gini_compare, incmean[,c(1,43)])


# area 
#area_km <- select(fed_area, FED.ID,district,AREA)
#area_km <- inner_join(popincome2015, area_km)
#area_km$AREA <- as.numeric(as.character(area_km$AREA))
#area_km <- area_km %>% 
# mutate(pop_density = Total / AREA /1000)
#dat8 <- inner_join(dat8, area_km)

# density
pop_density <- pop_density_census %>% 
  select(Geographic.code, Population.density.per.square.kilometre..2016, Population..2016) %>% 
  dplyr::rename(FED.ID = Geographic.code, fed_density = Population.density.per.square.kilometre..2016)
pop_density <- pop_density[-c(339:347),]
pop_density$FED.ID <- as.numeric(as.character(pop_density$FED.ID))
pop_density <- pop_density %>% 
  mutate(urban = ifelse(fed_density >= 300, 1, 0))
pop_density$urban <- as.factor(pop_density$urban)
dat9 <- inner_join(dat8, pop_density)

# incumbent
candidates2011 <-read.csv("./Elections-CAN/table_tableau12_2011.csv", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")

candidates2011 <- candidates2011[-which(candidates2011$V9 == ""), ]
candidates2011 <-candidates2011 %>% 
  mutate(PCC_2011 = ifelse(grepl("Conservative", V4),1,0 ))
names(candidates2011)[names(candidates2011) == 'V3'] <- 'FED.ID'
candidates2011_select <- candidates2011[-1,c(3,11)]
candidates2011_select$FED.ID <- as.numeric(as.character(candidates2011_select$FED.ID))
dat10 <- full_join(dat9, candidates2011_select)
dat10$PCC_2011 <- as.character(dat10$PCC_2011)
dat10 <- dat10 %>%
  mutate(PCC_2011 = ifelse(is.na(PCC_2011), 0, PCC_2011))
dat10$PCC_2011 <- as.factor(dat10$PCC_2011)

# total population
poptotal <- pop_density_census[,c(1,9,17)]
names(poptotal)[names(poptotal) == 'Geographic.code'] <- 'FED.ID'
names(poptotal)[names(poptotal) == 'Population..2011'] <- 'pop_2011'
names(poptotal)[names(poptotal) == 'Private.dwellings.occupied.by.usual.residents..2011'] <- 'owner_occupiers'
poptotal$FED.ID <- as.numeric(as.character(poptotal$FED.ID))
dat11 <- inner_join(dat10, poptotal)

# final dataset
colnames(dat11)
#data <- select(dat9, c(3,6,7,9,10,11,12,13,14,15,16,17,19,20,48,49,50,51,53,55))
#data1 <- select(dat9, c(3,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,43,44,45,46,47,48,49,50,51,52,53,54,55))

# subset provinces
gini20k_prov <- income20k %>% 
  subset(Prov.Terr == "TOTAL") %>% 
  select(Prov.Terr,gini20k)
provnames <- c('NL', 'PE', 'NS', 'NB', 'QC', 'ON', 'MB', 'SK', 'AB', 'BC', 'YT', 'NT', 'NU', 'CANADA')
gini20k_prov <- gini20k_prov %>% mutate(prov_code = provnames)

gini_prov <- cat2015 %>% 
  subset(Prov.Terr == "TOTAL") %>% 
  select(Prov.Terr,gini)
provnames <- c('NL', 'PE', 'NS', 'NB', 'QC', 'ON', 'MB', 'SK', 'AB', 'BC', 'YT', 'NT', 'NU', 'CANADA')
gini_prov <- gini_prov %>% mutate(prov_code = provnames)

# gini is in line with data from OECD
# https://data.oecd.org/inequality/income-inequality.htm


# province 
cat2015_prov <- income2015_incomecat
prov <- cat2015_prov$Prov.Terr == "TOTAL"
cat2015_prov <- cat2015_prov[prov == TRUE, ]
cat2015_prov <- cat2015_prov[,-c(1,2,3,4,5,25) ]
cat2015_prov <- cat2015_prov %>% 
  mutate(x.0to20 = Under..5.000 +
           X.5.000.to..9.999 +
           X.10.000.to..14.999 +
           X.15.000.to..19.999,
         x.20to40 = X.20.000.to..24.999 +
           X.25.000.to..29.999 +
           X.30.000.to..34.999 +
           X.35.000.to..39.999,
         x.40to60 =  X.40.000.to..44.999 +
           X.45.000.to..49.999 +
           X.50.000.to..54.999 +
           X.55.000.to..59.999,
         x.60to80= X.60.000.to..69.999 +
           X.70.000.to..79.999,
         x.80to100 = X.80.000.to..89.999 +
           X.90.000.to..99.999,
         x.100andup =  X.100.000.to..149.999 +
           X.150.000.to..249.999 +
           X.250.000.and.over)
cat2015_prov$gini20k <- apply(cat2015_prov[,20:25], 1, Gini)