library(tidyverse)
library(stringr)


#Import Industry HS lookup tables (1976-1988)
hs.casetype <- read.table("https://download.bls.gov/pub/time.series/hs/hs.case.type", header = TRUE, fill = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
hs.datatype <- read.table("https://download.bls.gov/pub/time.series/hs/hs.data.type", header = TRUE, fill = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
hs.division <- read.table("https://download.bls.gov/pub/time.series/hs/hs.division", header = TRUE, fill = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
    #fix missing tab issue with hs.industry
hs.industry <- readLines('https://download.bls.gov/pub/time.series/hs/hs.industry')
hs.industry <- gsub('\\W{4,}', '\t', hs.industry)
hs.industry <- paste(hs.industry, collapse = '\n')
hs.industry <- read.table(text = hs.industry, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)
hs.industry <- hs.industry[,2:3]

hs.series.names <- read.table("https://download.bls.gov/pub/time.series/hs/hs.series", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
hs.series <- read.table("https://download.bls.gov/pub/time.series/hs/hs.series", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
hs.series <- select(hs.series, -V6, -V11)
names(hs.series) <- hs.series.names

#Import Industry SH lookup tables (1989 - 2001)
sh.casetype <- read.table("https://download.bls.gov/pub/time.series/sh/sh.case.type", header = TRUE, fill = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
sh.datatype <- read.table("https://download.bls.gov/pub/time.series/sh/sh.data.type", header = TRUE, fill = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
sh.division <- read.table("https://download.bls.gov/pub/time.series/sh/sh.division", header = TRUE, fill = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
    #industry requires additional work
sh.industry.names <- read.table("https://download.bls.gov/pub/time.series/sh/sh.industry", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
sh.industry <- read.table("https://download.bls.gov/pub/time.series/sh/sh.industry", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
sh.industry <- sh.industry[, 2:3]
names(sh.industry) <- sh.industry.names[2:3]
sh.industry <- sh.industry %>% 
  filter(industry_code != "indu")
sh.series.names <- read.table("https://download.bls.gov/pub/time.series/sh/sh.series", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
sh.series <- read.table("https://download.bls.gov/pub/time.series/sh/sh.series", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
sh.series <- select(sh.series, -V6)
names(sh.series) <- sh.series.names

#Import SI lookup tables (2002)
si.casetype <- read.table("https://download.bls.gov/pub/time.series/si/si.case_type", header = TRUE, fill = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
si.datatype <- read.table("https://download.bls.gov/pub/time.series/si/si.data_type", header = TRUE, fill = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
si.division <- read.table("https://download.bls.gov/pub/time.series/si/si.division", header = TRUE, fill = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
    #Industry requires additional work
si.industry.names <- read.table("https://download.bls.gov/pub/time.series/sh/sh.industry", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
si.industry <- read.table("https://download.bls.gov/pub/time.series/sh/sh.industry", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
si.industry <- si.industry[, 2:3]
names(si.industry) <- si.industry.names[2:3]
si.industry <- si.industry %>% 
  filter(industry_code != "indu")

si.series.names <- read.table("https://download.bls.gov/pub/time.series/si/si.series", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
si.series <- read.table("https://download.bls.gov/pub/time.series/si/si.series", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
si.series <- select(si.series, -V6)
names(si.series) <- si.series.names

si.alldata <- read.table("https://download.bls.gov/pub/time.series/si/si.data.1.AllData", header = TRUE, fill = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)

#import Industry ii lookup tables 2003 - 2013
ii.casetype.names <- read.table("https://download.bls.gov/pub/time.series/ii/ii.case_type", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
ii.casetype <- read.table("https://download.bls.gov/pub/time.series/ii/ii.case_type", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
names(ii.casetype) <- ii.casetype.names

ii.datatype.names <- read.table("https://download.bls.gov/pub/time.series/ii/ii.data_type", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
ii.datatype <- read.table("https://download.bls.gov/pub/time.series/ii/ii.data_type", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
names(ii.datatype) <- ii.datatype.names

ii.supersector.names <- read.table("https://download.bls.gov/pub/time.series/ii/ii.supersector", nrow = 1, header = FALSE, fill = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
ii.supersector <- read.table("https://download.bls.gov/pub/time.series/ii/ii.supersector", skip = 1, header = FALSE, fill = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
ii.supersector <- ii.supersector[,1:2]
names(ii.supersector) <- ii.supersector.names[,1:2]

ii.industry.names <- read.table("https://download.bls.gov/pub/time.series/ii/ii.industry", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
ii.industry <- read.table("https://download.bls.gov/pub/time.series/ii/ii.industry", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
names(ii.industry) <- ii.industry.names
ii.industry <- ii.industry[,1:6] %>% 
  filter(industry_code != "indu")

ii.series <- read.table("https://download.bls.gov/pub/time.series/ii/ii.series", header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)

ii.area.names <- read.table("https://download.bls.gov/pub/time.series/ii/ii.area", nrow = 1, header = FALSE, fill = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
ii.area <- read.table("https://download.bls.gov/pub/time.series/ii/ii.area", skip = 1, header = FALSE, fill = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
ii.area <- ii.area[,1:2]
names(ii.area) <- ii.area.names[,1:2]

#import Industry is lookup tables 2014 - present 
is.casetype.names <- read.table("https://download.bls.gov/pub/time.series/is/is.case_type", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
is.casetype <- read.table("https://download.bls.gov/pub/time.series/is/is.case_type", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
names(is.casetype) <- is.casetype.names

is.datatype.names <- read.table("https://download.bls.gov/pub/time.series/is/is.data_type", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
is.datatype <- read.table("https://download.bls.gov/pub/time.series/is/is.data_type", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
names(is.datatype) <- is.datatype.names

is.supersector.names <- read.table("https://download.bls.gov/pub/time.series/is/is.supersector", nrow = 1, header = FALSE, fill = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
is.supersector <- read.table("https://download.bls.gov/pub/time.series/is/is.supersector", skip = 1, header = FALSE, fill = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
names(is.supersector) <- is.supersector.names

#Industry requires additional work
is.industry.names <- read.table("https://download.bls.gov/pub/time.series/is/is.industry", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
is.industry <- read.table("https://download.bls.gov/pub/time.series/is/is.industry", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
names(is.industry) <- is.industry.names
is.industry <- is.industry[,1:6] %>% 
  filter(industry_code != "indu")

is.area.names <- read.table("https://download.bls.gov/pub/time.series/is/is.area", nrow = 1, header = FALSE, fill = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
is.area <- read.table("https://download.bls.gov/pub/time.series/is/is.area", skip = 1, header = FALSE, fill = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
names(is.area) <- is.area.names

#----------
is.series <- read.table("https://download.bls.gov/pub/time.series/is/is.series", header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)


#C&D lookup tables 2011 - present

cs.category <- readLines('https://download.bls.gov/pub/time.series/cs/cs.category')
cs.category <- gsub("\\t$", "", cs.category)
cs.category <- paste(cs.category, collapse = '\n')
cs.category <- read.table(text = cs.category, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)
cs.category <- cs.category[,2:3]

cs.datatype <- readLines('https://download.bls.gov/pub/time.series/cs/cs.datatype')
cs.datatype <- gsub("\\t$", "", cs.datatype)
cs.datatype <- paste(cs.datatype, collapse = '\n')
cs.datatype <- read.table(text = cs.datatype, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cs.case <- readLines('https://download.bls.gov/pub/time.series/cs/cs.case')
cs.case <- gsub("\\t$", "", cs.case)
cs.case <- paste(cs.case, collapse = '\n')
cs.case <- read.table(text = cs.case, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cs.age <- readLines('https://download.bls.gov/pub/time.series/cs/cs.age')
cs.age <- gsub("\\t$", "", cs.age)
cs.age <- paste(cs.age, collapse = '\n')
cs.age <- read.table(text = cs.age, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cs.los <- readLines('https://download.bls.gov/pub/time.series/cs/cs.los')
cs.los <- gsub("\\t$", "", cs.los)
cs.los <- paste(cs.los, collapse = '\n')
cs.los <- read.table(text = cs.los, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cs.race <- readLines('https://download.bls.gov/pub/time.series/cs/cs.race')
cs.race <- gsub("\\t$", "", cs.race)
cs.race <- paste(cs.race, collapse = '\n')
cs.race <- read.table(text = cs.race, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cs.gender <- readLines('https://download.bls.gov/pub/time.series/cs/cs.gender')
cs.gender <- gsub("\\t$", "", cs.gender)
cs.gender <- paste(cs.gender, collapse = '\n')
cs.gender <- read.table(text = cs.gender, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cs.pob <- readLines('https://download.bls.gov/pub/time.series/cs/cs.pob')
cs.pob <- gsub("\\t$", "", cs.pob)
cs.pob <- paste(cs.pob, collapse = '\n')
cs.pob <- read.table(text = cs.pob, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cs.event <- readLines('https://download.bls.gov/pub/time.series/cs/cs.event')
cs.event <- gsub("\\t$", "", cs.event)
cs.event <- paste(cs.event, collapse = '\n')
cs.event <- read.table(text = cs.event, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cs.source <- readLines('https://download.bls.gov/pub/time.series/cs/cs.source')
cs.source <- gsub("\\t$", "", cs.source)
cs.source <- paste(cs.source, collapse = '\n')
cs.source <- read.table(text = cs.source, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cs.occupation <- readLines('https://download.bls.gov/pub/time.series/cs/cs.occupation')
cs.occupation <- gsub("\\t$", "", cs.occupation)
cs.occupation <- paste(cs.occupation, collapse = '\n')
cs.occupation <- read.table(text = cs.occupation, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cs.nature <- readLines('https://download.bls.gov/pub/time.series/cs/cs.nature')
cs.nature <- gsub("\\t$", "", cs.nature)
cs.nature <- paste(cs.nature, collapse = '\n')
cs.nature <- read.table(text = cs.nature, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cs.industry <- readLines('https://download.bls.gov/pub/time.series/cs/cs.industry')
cs.industry <- gsub("\\t$", "", cs.industry)
cs.industry <- paste(cs.industry, collapse = '\n')
cs.industry <- read.table(text = cs.industry, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cs.time <- readLines('https://download.bls.gov/pub/time.series/cs/cs.time')
cs.time <- gsub("\\t$", "", cs.time)
cs.time <- paste(cs.time, collapse = '\n')
cs.time <- read.table(text = cs.time, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cs.hour <- readLines('https://download.bls.gov/pub/time.series/cs/cs.hour')
cs.hour <- gsub("\\t$", "", cs.hour)
cs.hour <- paste(cs.hour, collapse = '\n')
cs.hour <- read.table(text = cs.hour, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cs.weekday <- readLines('https://download.bls.gov/pub/time.series/cs/cs.weekday')
cs.weekday <- gsub("\\t$", "", cs.weekday)
cs.weekday <- paste(cs.weekday, collapse = '\n')
cs.weekday <- read.table(text = cs.weekday, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cs.special <- readLines('https://download.bls.gov/pub/time.series/cs/cs.special')
cs.special <- gsub("\\t$", "", cs.special)
cs.special <- paste(cs.special, collapse = '\n')
cs.special <- read.table(text = cs.special, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cs.ownership <- readLines('https://download.bls.gov/pub/time.series/cs/cs.ownership')
cs.ownership <- gsub("\\t$", "", cs.ownership)
cs.ownership <- paste(cs.ownership, collapse = '\n')
cs.ownership <- read.table(text = cs.ownership, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cs.state <- readLines('https://download.bls.gov/pub/time.series/cs/cs.state')
cs.state <- gsub("\\t$", "", cs.state)
cs.state <- paste(cs.state, collapse = '\n')
cs.state <- read.table(text = cs.state, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

#cs.series <- readLines('https://download.bls.gov/pub/time.series/cs/cs.series')
#cs.series <- gsub("\\t$", "", cs.series)
#cs.series <- paste(cs.series, collapse = '\n')
#cs.series <- read.table(text = cs.series, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)



#C&D lookup tables 1992 - 2001

cd.nature.names <- read.table("https://download.bls.gov/pub/time.series/cd/cd.nature", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
cd.nature <- read.table("https://download.bls.gov/pub/time.series/cd/cd.nature", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
cd.nature <- cd.nature[,1:2]
cd.nature.names <- cd.nature.names %>% 
  select(V1, V2)
names(cd.nature) <- cd.nature.names

cd.category2.names <- read.table("https://download.bls.gov/pub/time.series/cd/cd.category2", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
cd.category2 <- read.table("https://download.bls.gov/pub/time.series/cd/cd.category2", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
names(cd.category2) <- cd.category2.names

cd.pob.names <- read.table("https://download.bls.gov/pub/time.series/cd/cd.pob", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
cd.pob <- read.table("https://download.bls.gov/pub/time.series/cd/cd.pob", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
cd.pob <- cd.pob[,1:2]
cd.pob.names <- cd.pob.names %>% 
  select(V1, V2)
names(cd.pob) <- cd.pob.names

cd.source.names <- read.table("https://download.bls.gov/pub/time.series/cd/cd.source", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
cd.source <- read.table("https://download.bls.gov/pub/time.series/cd/cd.source", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
cd.source <- cd.source[,1:2]
cd.source.names <- cd.source.names %>% 
  select(V1, V2)
names(cd.source) <- cd.source.names

cd.occupation.names <- read.table("https://download.bls.gov/pub/time.series/cd/cd.occupation", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
cd.occupation <- read.table("https://download.bls.gov/pub/time.series/cd/cd.occupation", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
cd.occupation <- cd.occupation[,1:2]
cd.occupation.names <- cd.occupation.names %>% 
  select(V1, V2)
names(cd.occupation) <- cd.occupation.names

cd.event <- readLines('https://download.bls.gov/pub/time.series/cd/cd.event')
cd.event <- gsub("\\t$", "", cd.event)
cd.event <- paste(cd.event, collapse = '\n')
cd.event <- read.table(text = cd.event, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cd.datatype <- readLines('https://download.bls.gov/pub/time.series/cd/cd.datatype')
cd.datatype <- gsub("\\t$", "", cd.datatype)
cd.datatype <- paste(cd.datatype, collapse = '\n')
cd.datatype <- read.table(text = cd.datatype, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cd.case <- readLines('https://download.bls.gov/pub/time.series/cd/cd.case')
cd.case <- gsub("\\t$", "", cd.case)
cd.case <- paste(cd.case, collapse = '\n')
cd.case <- read.table(text = cd.case, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cd.age <- readLines('https://download.bls.gov/pub/time.series/cd/cd.age')
cd.age <- gsub("\\t$", "", cd.age)
cd.age <- paste(cd.age, collapse = '\n')
cd.age <- read.table(text = cd.age, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cd.los <- readLines('https://download.bls.gov/pub/time.series/cd/cd.los')
cd.los <- gsub("\\t$", "", cd.los)
cd.los <- paste(cd.los, collapse = '\n')
cd.los <- read.table(text = cd.los, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cd.race <- readLines('https://download.bls.gov/pub/time.series/cd/cd.race')
cd.race <- gsub("\\t$", "", cd.race)
cd.race <- paste(cd.race, collapse = '\n')
cd.race <- read.table(text = cd.race, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cd.gender <- readLines('https://download.bls.gov/pub/time.series/cd/cd.gender')
cd.gender <- gsub("\\t$", "", cd.gender)
cd.gender <- paste(cd.gender, collapse = '\n')
cd.gender <- read.table(text = cd.gender, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cd.industry <- readLines('https://download.bls.gov/pub/time.series/cd/cd.industry')
cd.industry <- gsub("\\t$", "", cd.industry)
cd.industry <- paste(cd.industry, collapse = '\n')
cd.industry <- read.table(text = cd.industry, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)


#C&D lookup tables 2002

hc.nature.names <- read.table("https://download.bls.gov/pub/time.series/hc/hc.nature", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
hc.nature <- read.table("https://download.bls.gov/pub/time.series/hc/hc.nature", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
hc.nature <- hc.nature[,1:2]
hc.nature.names <- hc.nature.names %>% 
  select(V1, V2)
names(hc.nature) <- hc.nature.names

hc.category2.names <- read.table("https://download.bls.gov/pub/time.series/hc/hc.category2", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
hc.category2 <- read.table("https://download.bls.gov/pub/time.series/hc/hc.category2", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
names(hc.category2) <- hc.category2.names

hc.pob.names <- read.table("https://download.bls.gov/pub/time.series/hc/hc.pob", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
hc.pob <- read.table("https://download.bls.gov/pub/time.series/hc/hc.pob", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
hc.pob <- hc.pob[,1:2]
hc.pob.names <- hc.pob.names %>% 
  select(V1, V2)
names(hc.pob) <- hc.pob.names

hc.source.names <- read.table("https://download.bls.gov/pub/time.series/hc/hc.source", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
hc.source <- read.table("https://download.bls.gov/pub/time.series/hc/hc.source", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
hc.source <- hc.source[,1:2]
hc.source.names <- hc.source.names %>% 
  select(V1, V2)
names(hc.source) <- hc.source.names

hc.occupation.names <- read.table("https://download.bls.gov/pub/time.series/hc/hc.occupation", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
hc.occupation <- read.table("https://download.bls.gov/pub/time.series/hc/hc.occupation", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
hc.occupation <- hc.occupation[,1:2]
hc.occupation.names <- hc.occupation.names %>% 
  select(V1, V2)
names(hc.occupation) <- hc.occupation.names

hc.event <- readLines('https://download.bls.gov/pub/time.series/hc/hc.event')
hc.event <- gsub("\\t$", "", hc.event)
hc.event <- paste(hc.event, collapse = '\n')
hc.event <- read.table(text = hc.event, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

hc.datatype <- readLines('https://download.bls.gov/pub/time.series/hc/hc.datatype')
hc.datatype <- gsub("\\t$", "", hc.datatype)
hc.datatype <- paste(hc.datatype, collapse = '\n')
hc.datatype <- read.table(text = hc.datatype, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

hc.case <- readLines('https://download.bls.gov/pub/time.series/hc/hc.case')
hc.case <- gsub("\\t$", "", hc.case)
hc.case <- paste(hc.case, collapse = '\n')
hc.case <- read.table(text = hc.case, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

hc.age <- readLines('https://download.bls.gov/pub/time.series/hc/hc.age')
hc.age <- gsub("\\t$", "", hc.age)
hc.age <- paste(hc.age, collapse = '\n')
hc.age <- read.table(text = hc.age, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

hc.los <- readLines('https://download.bls.gov/pub/time.series/hc/hc.los')
hc.los <- gsub("\\t$", "", hc.los)
hc.los <- paste(hc.los, collapse = '\n')
hc.los <- read.table(text = hc.los, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

hc.race <- readLines('https://download.bls.gov/pub/time.series/hc/hc.race')
hc.race <- gsub("\\t$", "", hc.race)
hc.race <- paste(hc.race, collapse = '\n')
hc.race <- read.table(text = hc.race, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

hc.gender <- readLines('https://download.bls.gov/pub/time.series/hc/hc.gender')
hc.gender <- gsub("\\t$", "", hc.gender)
hc.gender <- paste(hc.gender, collapse = '\n')
hc.gender <- read.table(text = hc.gender, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

hc.industry <- readLines('https://download.bls.gov/pub/time.series/hc/hc.industry')
hc.industry <- gsub("\\t$", "", hc.industry)
hc.industry <- paste(hc.industry, collapse = '\n')
hc.industry <- read.table(text = hc.industry, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)


#C&D lookup tables 2003 - 2010

ch.category <- readLines('https://download.bls.gov/pub/time.series/ch/ch.category')
ch.category <- gsub("\\t$", "", ch.category)
ch.category <- paste(ch.category, collapse = '\n')
ch.category <- read.table(text = ch.category, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)
ch.category <- ch.category[,2:3]

ch.datatype <- readLines('https://download.bls.gov/pub/time.series/ch/ch.datatype')
ch.datatype <- gsub("\\t$", "", ch.datatype)
ch.datatype <- paste(ch.datatype, collapse = '\n')
ch.datatype <- read.table(text = ch.datatype, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

ch.case <- readLines('https://download.bls.gov/pub/time.series/ch/ch.case')
ch.case <- gsub("\\t$", "", ch.case)
ch.case <- paste(ch.case, collapse = '\n')
ch.case <- read.table(text = ch.case, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

ch.age <- readLines('https://download.bls.gov/pub/time.series/ch/ch.age')
ch.age <- gsub("\\t$", "", ch.age)
ch.age <- paste(ch.age, collapse = '\n')
ch.age <- read.table(text = ch.age, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

ch.los <- readLines('https://download.bls.gov/pub/time.series/ch/ch.los')
ch.los <- gsub("\\t$", "", ch.los)
ch.los <- paste(ch.los, collapse = '\n')
ch.los <- read.table(text = ch.los, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

ch.race <- readLines('https://download.bls.gov/pub/time.series/ch/ch.race')
ch.race <- gsub("\\t$", "", ch.race)
ch.race <- paste(ch.race, collapse = '\n')
ch.race <- read.table(text = ch.race, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

ch.gender <- readLines('https://download.bls.gov/pub/time.series/ch/ch.gender')
ch.gender <- gsub("\\t$", "", ch.gender)
ch.gender <- paste(ch.gender, collapse = '\n')
ch.gender <- read.table(text = ch.gender, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

ch.pob <- readLines('https://download.bls.gov/pub/time.series/ch/ch.pob')
ch.pob <- gsub("\\t$", "", ch.pob)
ch.pob <- paste(ch.pob, collapse = '\n')
ch.pob <- read.table(text = ch.pob, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

ch.event <- readLines('https://download.bls.gov/pub/time.series/ch/ch.event')
ch.event <- gsub("\\t$", "", ch.event)
ch.event <- paste(ch.event, collapse = '\n')
ch.event <- read.table(text = ch.event, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

ch.source <- readLines('https://download.bls.gov/pub/time.series/ch/ch.source')
ch.source <- gsub("\\t$", "", ch.source)
ch.source <- paste(ch.source, collapse = '\n')
ch.source <- read.table(text = ch.source, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

ch.occupation <- readLines('https://download.bls.gov/pub/time.series/ch/ch.occupation')
ch.occupation <- gsub("\\t$", "", ch.occupation)
ch.occupation <- paste(ch.occupation, collapse = '\n')
ch.occupation <- read.table(text = ch.occupation, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

ch.nature <- readLines('https://download.bls.gov/pub/time.series/ch/ch.nature')
ch.nature <- gsub("\\t$", "", ch.nature)
ch.nature <- paste(ch.nature, collapse = '\n')
ch.nature <- read.table(text = ch.nature, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

ch.industry <- readLines('https://download.bls.gov/pub/time.series/ch/ch.industry')
ch.industry <- gsub("\\t$", "", ch.industry)
ch.industry <- paste(ch.industry, collapse = '\n')
ch.industry <- read.table(text = ch.industry, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

ch.time <- readLines('https://download.bls.gov/pub/time.series/ch/ch.time')
ch.time <- gsub("\\t$", "", ch.time)
ch.time <- paste(ch.time, collapse = '\n')
ch.time <- read.table(text = ch.time, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

ch.hour <- readLines('https://download.bls.gov/pub/time.series/ch/ch.hour')
ch.hour <- gsub("\\t$", "", ch.hour)
ch.hour <- paste(ch.hour, collapse = '\n')
ch.hour <- read.table(text = ch.hour, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

ch.weekday <- readLines('https://download.bls.gov/pub/time.series/ch/ch.weekday')
ch.weekday <- gsub("\\t$", "", ch.weekday)
ch.weekday <- paste(ch.weekday, collapse = '\n')
ch.weekday <- read.table(text = ch.weekday, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

ch.special <- readLines('https://download.bls.gov/pub/time.series/ch/ch.special')
ch.special <- gsub("\\t$", "", ch.special)
ch.special <- paste(ch.special, collapse = '\n')
ch.special <- read.table(text = ch.special, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

ch.ownership <- readLines('https://download.bls.gov/pub/time.series/ch/ch.ownership')
ch.ownership <- gsub("\\t$", "", ch.ownership)
ch.ownership <- paste(ch.ownership, collapse = '\n')
ch.ownership <- read.table(text = ch.ownership, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

ch.state <- readLines('https://download.bls.gov/pub/time.series/ch/ch.state')
ch.state <- gsub("\\t$", "", ch.state)
ch.state <- paste(ch.state, collapse = '\n')
ch.state <- read.table(text = ch.state, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

#ch.series <- readLines('https://download.bls.gov/pub/time.series/ch/ch.series')
#ch.series <- gsub("\\t$", "", ch.series)
#ch.series <- paste(ch.series, collapse = '\n')
#ch.series <- read.table(text = ch.series, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)


#fatal lookup tables 2011 - present 

fw.category2.names <- read.table("https://download.bls.gov/pub/time.series/fw/fw.category2", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
fw.category2 <- read.table("https://download.bls.gov/pub/time.series/fw/fw.category2", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
names(fw.category2) <- fw.category2.names

fw.source.names <- read.table("https://download.bls.gov/pub/time.series/fw/fw.source", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
fw.source <- read.table("https://download.bls.gov/pub/time.series/fw/fw.source", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
fw.source <- fw.source[,1:2]
fw.source.names <- fw.source.names %>% 
  select(V1, V2)
names(fw.source) <- fw.source.names

fw.occupation.names <- read.table("https://download.bls.gov/pub/time.series/fw/fw.occupation", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
fw.occupation <- read.table("https://download.bls.gov/pub/time.series/fw/fw.occupation", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
fw.occupation <- fw.occupation[,1:2]
fw.occupation.names <- fw.occupation.names %>% 
  select(V1, V2)
names(fw.occupation) <- fw.occupation.names

fw.event <- readLines('https://download.bls.gov/pub/time.series/fw/fw.event')
fw.event <- gsub("\\t$", "", fw.event)
fw.event <- paste(fw.event, collapse = '\n')
fw.event <- read.table(text = fw.event, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

fw.datatype <- readLines('https://download.bls.gov/pub/time.series/fw/fw.datatype')
fw.datatype <- gsub("\\t$", "", fw.datatype)
fw.datatype <- paste(fw.datatype, collapse = '\n')
fw.datatype <- read.table(text = fw.datatype, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

fw.case <- readLines('https://download.bls.gov/pub/time.series/fw/fw.case')
fw.case <- gsub("\\t$", "", fw.case)
fw.case <- paste(fw.case, collapse = '\n')
fw.case <- read.table(text = fw.case, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

fw.industry <- readLines('https://download.bls.gov/pub/time.series/fw/fw.industry')
fw.industry <- gsub("\\t$", "", fw.industry)
fw.industry <- paste(fw.industry, collapse = '\n')
fw.industry <- read.table(text = fw.industry, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

fw.area <- readLines('https://download.bls.gov/pub/time.series/fw/fw.area')
fw.area <- gsub("\\t$", "", fw.area)
fw.area <- paste(fw.area, collapse = '\n')
fw.area <- read.table(text = fw.area, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)


#fatal lookup tables 2003 - 2010 

fi.category2.names <- read.table("https://download.bls.gov/pub/time.series/fi/fi.category2", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
fi.category2 <- read.table("https://download.bls.gov/pub/time.series/fi/fi.category2", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
names(fi.category2) <- fi.category2.names

fi.source.names <- read.table("https://download.bls.gov/pub/time.series/fi/fi.source", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
fi.source <- read.table("https://download.bls.gov/pub/time.series/fi/fi.source", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
fi.source <- fi.source[,1:2]
fi.source.names <- fi.source.names %>% 
  select(V1, V2)
names(fi.source) <- fi.source.names

fi.occupation.names <- read.table("https://download.bls.gov/pub/time.series/fi/fi.occupation", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
fi.occupation <- read.table("https://download.bls.gov/pub/time.series/fi/fi.occupation", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
fi.occupation <- fi.occupation[,1:2]
fi.occupation.names <- fi.occupation.names %>% 
  select(V1, V2)
names(fi.occupation) <- fi.occupation.names

fi.event <- readLines('https://download.bls.gov/pub/time.series/fi/fi.event')
fi.event <- gsub("\\t$", "", fi.event)
fi.event <- paste(fi.event, collapse = '\n')
fi.event <- read.table(text = fi.event, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

fi.datatype <- readLines('https://download.bls.gov/pub/time.series/fi/fi.datatype')
fi.datatype <- gsub("\\t$", "", fi.datatype)
fi.datatype <- paste(fi.datatype, collapse = '\n')
fi.datatype <- read.table(text = fi.datatype, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

fi.case <- readLines('https://download.bls.gov/pub/time.series/fi/fi.case')
fi.case <- gsub("\\t$", "", fi.case)
fi.case <- paste(fi.case, collapse = '\n')
fi.case <- read.table(text = fi.case, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

fi.industry <- readLines('https://download.bls.gov/pub/time.series/fi/fi.industry')
fi.industry <- gsub("\\t$", "", fi.industry)
fi.industry <- paste(fi.industry, collapse = '\n')
fi.industry <- read.table(text = fi.industry, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

fi.area <- readLines('https://download.bls.gov/pub/time.series/fi/fi.area')
fi.area <- gsub("\\t$", "", fi.area)
fi.area <- paste(fi.area, collapse = '\n')
fi.area <- read.table(text = fi.area, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)


#fatal lookup tables 1992 - 2002 

cf.category2.names <- read.table("https://download.bls.gov/pub/time.series/cf/cf.category2", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
cf.category2 <- read.table("https://download.bls.gov/pub/time.series/cf/cf.category2", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
names(cf.category2) <- cf.category2.names

cf.source.names <- read.table("https://download.bls.gov/pub/time.series/cf/cf.source", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
cf.source <- read.table("https://download.bls.gov/pub/time.series/cf/cf.source", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
cf.source <- cf.source[,1:2]
cf.source.names <- cf.source.names %>% 
  select(V1, V2)
names(cf.source) <- cf.source.names

cf.occupation.names <- read.table("https://download.bls.gov/pub/time.series/cf/cf.occupation", nrow = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
cf.occupation <- read.table("https://download.bls.gov/pub/time.series/cf/cf.occupation", skip = 1, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
cf.occupation <- cf.occupation[,1:2]
cf.occupation.names <- cf.occupation.names %>% 
  select(V1, V2)
names(cf.occupation) <- cf.occupation.names

cf.event <- readLines('https://download.bls.gov/pub/time.series/cf/cf.event')
cf.event <- gsub("\\t$", "", cf.event)
cf.event <- paste(cf.event, collapse = '\n')
cf.event <- read.table(text = cf.event, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cf.datatype <- readLines('https://download.bls.gov/pub/time.series/cf/cf.datatype')
cf.datatype <- gsub("\\t$", "", cf.datatype)
cf.datatype <- paste(cf.datatype, collapse = '\n')
cf.datatype <- read.table(text = cf.datatype, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cf.case <- readLines('https://download.bls.gov/pub/time.series/cf/cf.case')
cf.case <- gsub("\\t$", "", cf.case)
cf.case <- paste(cf.case, collapse = '\n')
cf.case <- read.table(text = cf.case, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)

cf.industry <- readLines('https://download.bls.gov/pub/time.series/cf/cf.industry')
cf.industry <- gsub("\\t$", "", cf.industry)
cf.industry <- paste(cf.industry, collapse = '\n')
cf.industry <- read.table(text = cf.industry, sep = '\t', header = T, quote = '', stringsAsFactors = FALSE)


#fix leading zeros

hs.division$division_code <- as.factor(str_pad(hs.division$division_code, 2, pad = "0"))
hs.industry$industry_code <- as.factor(str_pad(hs.industry$industry_code, 4, pad = "0"))

sh.division$division_code <- as.character(str_pad(sh.division$division_code, 2, pad = "0"))
sh.industry$industry_code <- as.factor(str_pad(sh.industry$industry_code, 4, pad = "0"))

si.division$division_code <- str_pad(si.division$division_code, 2, pad = "0")
si.industry$industry_code <- as.factor(str_pad(si.industry$industry_code, 4, pad = "0"))

is.area$area_code <- as.factor(str_pad(is.area$area_code, 3, pad = "0"))
ii.area$area_code <- as.factor(str_pad(ii.area$area_code, 3, pad = "0"))

is.series$area_code <- as.factor(str_pad(is.series$area_code, 3, pad="0"))

#Write data

#Remmove .names dataframes
rm(list=ls(pattern=".names"))

#create list of dfs
list_df <- mget(ls()[1:length(ls())])

#write to data folder
setwd("./data")
lapply(seq_along(list_df),
       function(i) write.table(list_df[[i]], 
                               paste0(names(list_df)[i], ".csv"),
                               row.names = FALSE, quote = TRUE, 
                               sep = ";", dec = "."))
#set wd back to main
setwd("..")


#------------splitting area name

# statesplit <- strsplit(as.character(ii.area$area_name), ",", fixed = TRUE)
# 
# ii.area <- ii.area %>% 
#   mutate(ownership = sapply(statesplit, "[", 1), state = sapply(statesplit, "[", 2))












































