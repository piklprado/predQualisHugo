library(MASS)
library(bbmle)
library(dplyr)
library(tidyr)
library(lme4)

################################################################################
## Data preparation
################################################################################
## Beals' list of standalone predatory journals
beals <- read.csv2("../static/beals2.csv", as.is=TRUE, header=FALSE)
names(beals) <- "title"
beals$title2 <- sub(" \\(.*$", "", beals$title)
beals$title2 <- gsub(" s+"," ", beals$title2)
## Beals'list of predatory publishers
bealsp <- read.csv2("../static/beals_publishers.csv", as.is=TRUE)
bealsp$name <- sub(" \\(.*$", "",bealsp$name.in.list)
## Scopus list ##
scopus <- read.csv2("../static/Scopus_Source_List.csv", as.is=TRUE)
scopus$cancelled <- grepl("cancelled", scopus$coverage)
## Extrating journals from Scopus list that are published by publishers in Beal's list
sc.bealsp <- rbind(merge(scopus, bealsp, by.x="publisher.name", by.y="name"),
                   merge(scopus, bealsp, by.x="main.publisher", by.y="name"))
sc.bealsp <- sc.bealsp[!duplicated(sc.bealsp$id),]
sc.bealsp$title <- toupper(sc.bealsp$title)
## Number of sources in this list
#nrow(sc.bealsp)
##Number of sources cancelled by SCOPUS
#sum(sc.bealsp$cancelled)
## Filtering titles already in Beals list of stand-alone Journals
sc.bealsp <- sc.bealsp[!(sc.bealsp$title%in%beals$title2),] ## seven titles already in the original list
## save in csv
write.csv2(sc.bealsp, file="../static/beals_publishers_in_scopus.csv", row.names=FALSE)
## Paper published by OMICS ##
omics <- read.csv2("../static/omics.csv", as.is=TRUE)
omics$title <- toupper(omics$title)
## QUALIS knowledge areas classification
areas <- read.csv2("../static/areas.csv", as.is=TRUE)
areas$area <- gsub(" +", " ", areas$area)
areas$area <- gsub(" $", "", areas$area)
areas$res.area <- ifelse(areas$grande.area=="AGRÁRIAS"|areas$grande.area=="ENGENHARIAS"|
                         areas$grande.area=="MULTIDISCIPLINAR"|areas$grande.area=="SAÚDE"|
                         areas$grande.area=="SOCIAIS APLICADAS", "APPLIED", "BASIC")
## QUALIS list of journals by area and ranking
qualis <- read.csv2("../static/qualis_13_16.csv", as.is=TRUE)
qualis$titulo.abr <- sub(" \\(.*$", "",qualis$titulo)
qualis$titulo.abr <- gsub(" +"," ", qualis$titulo.abr)
qualis$area <- gsub(" +", " ", qualis$area)
qualis$area <- gsub(" $", "", qualis$area)
qualis$estrato[qualis$estrato=="C "] <- "C"
qualis$estrato <- factor(qualis$estrato)
## Titles in Beals list of standalone Journals
qualis$pred <- qualis$titulo.abr %in% beals$title2
## Titles from Beals list of Predatory publishers in Scopus list
qualis$predp <- qualis$titulo.abr %in% sc.bealsp$title
## Titles from Beals list of Predatory publishers in Scopus list and that have been discontinued in Scopus
qualis$predp2 <- qualis$titulo.abr %in% sc.bealsp$title[sc.bealsp$cancelled]
## Title published by OMICS
qualis$omics <- qualis$titulo.abr %in% omics$title
## All predatory
qualis$predatory <- qualis$pred|qualis$predp|qualis$omics

## Correcting titles in QUALIS that have been hijacked
qualis$predatory[qualis$ISSN=="1520-6025"] <- FALSE
qualis$predatory[qualis$ISSN=="0163-3864"] <- FALSE
qualis$predatory[qualis$ISSN=="1840-3662"] <- FALSE
qualis$titulo.abr[qualis$ISSN=="1520-6025"] <- paste(qualis$titulo.abr[qualis$ISSN=="1520-6025"],"no predatory")
qualis$titulo.abr[qualis$ISSN=="0163-3864"] <- paste(qualis$titulo.abr[qualis$ISSN=="0163-3864"],"no predatory")
qualis$titulo.abr[qualis$ISSN=="1840-3662"] <- paste(qualis$titulo.abr[qualis$ISSN=="1840-3662"],"no predatory")
## Save a worksheet of the raw data
write.csv2(qualis[,-(6:9)], file="../static/qualis_id_predadores.csv")

## A dataframe with a row for each journal title (eliminates duplicated names due online x print versions)
qualis2 <- qualis %>%
    group_by(area, estrato, titulo.abr) %>%
    summarise(N=n()) %>%
    as.data.frame()
## Which titles are predatory
qualis2$pred <- qualis2$titulo.abr %in% beals$title2
## Titles from Beals list of Predatory publishers in Scopus list
qualis2$predp <- qualis2$titulo.abr %in% sc.bealsp$title
## Titles from Beals list of Predatory publishers in Scopus list and that have been discontinued in Scopus
qualis2$predp2 <- qualis2$titulo.abr %in% sc.bealsp$title[sc.bealsp$cancelled]
## Titles from omics
qualis2$omics <- qualis2$titulo.abr %in% omics$title
## Title in at least one list
qualis2$all.pred <- qualis2$pred|qualis2$predp|qualis2$omics
## DOAJ list of removed titles##
doaj <- read.csv2("../static/doaj_removed.csv", as.is=TRUE)
doaj$title <- toupper(doaj$title)
doaj.filter <- c("Suspected editorial misconduct by publisher",
                 "Journal not adhering to Best Practice",
                 "ISSN not registered",
                 "Journal not adhering to Best practice",
                 "Malicious website",
                 "Journal not adhering to Best practice (one url per issue)",
                 "Journal not adhering to Best practice and issn dead",
                 "Reapp. not adhering to Best practice",
                 "Invalid ISSN",
                 "No licence , peer review inadequate",
                 "Not in the issn database",
                 "use of fake impact factor",
                 "Journal not adhering to Best Practice\n",
                 "BMC stopped publishing",
                 "the issn 20888708 given by editor leads to another journal")
qualis2$doaj.removed <- qualis2$titulo.abr %in% doaj$title[doaj$reason%in%doaj.filter]
## Beals x DOAJ removed listing
# table(qualis2$doaj.removed, qualis2$pred|qualis2$predp)

## DOAJ whitelist ##
## in.doaj <- function(x) jaod_journal_search(paste("title",x,sep=":"))$total==0
## qualis2$pred.doaj <- NA
## ## Names of potentially predatory
## ## Loop is not working due connexion errors
## pred.names <- qualis2$titulo.abr[qualis2$all.pred]
## results <- c()
## for(i in 1:length(pred.names)){
##     results[i] <- in.doaj(pred.names[i])
##     }

################################################################################
## Number of predatory journals in each area and their ranking (A to C strata)
################################################################################
pred.area <- qualis2 %>%
    group_by(area,estrato) %>%
    summarize(total=n(), n.pred=sum(pred), n.predp=sum(predp), n.predp2=sum(predp2), all.pred=sum(all.pred)) %>%
    as.data.frame()
## Exports to a  csv file
write.csv2(pred.area,file="../static/proporcao_pred_estrato_area.csv")
## Range of proportion of predatory Journals in each stratum
pred.area %>%
    group_by(estrato) %>%
    summarize(pred.p.mean=100*mean(all.pred/total), pred.p.min = 100*min(all.pred/total), pred.p.max = 100*max(all.pred/total))

## Proportion of predatory ranked at C stratum, for each area
## detecta.pred <- qualis2 %>%
##     group_by(area,estrato) %>%
##     summarize(n.pred=sum(pred)) %>%
##     spread(estrato, n.pred) %>%
##     mutate(tot.pred=A1+A2+B1+B2+B3+B4+B5+C)
## ## Adds total number of journals
## n.pred <- qualis2 %>%
##     group_by(area) %>%
##     summarise(tot=n()) %>%
##     inner_join(detecta.pred, by="area") %>%
##     dplyr::select(area, A1:C, tot.pred, tot) %>%
##     as.data.frame()
## Same, including journals form publishers in Beals' list and OMICS
## Proportion of predatory ranked at C strata, for each area
detecta.pred2 <- qualis2 %>%
    group_by(area,estrato) %>%
    summarize(n.pred=sum(all.pred)) %>%
    spread(estrato, n.pred) %>%
    mutate(tot.pred=A1+A2+B1+B2+B3+B4+B5+C)
## Adds total number of journals
n.pred2 <- qualis2 %>%
    group_by(area) %>%
    summarise(tot=n()) %>%
    inner_join(detecta.pred2, by="area") %>%
    dplyr::select(area, A1:C, tot.pred, tot) %>%
    as.data.frame()
## Exports csv file
#write.csv2(n.pred,file="n_predadores_area.csv")
write.csv2(n.pred2,file="../static/n_predadores_inclusivo_area.csv")

################################################################################
## A model to spot predatory Journals
################################################################################
## Only for standalone journals #
## tmp1 <- merge(areas, n.pred, by="area")
## m0 <- glm(cbind(C,tot.pred)~1 , data=tmp1, family=binomial)
## m1 <- glm(cbind(C,tot.pred)~area-1 , data=tmp1, family=binomial)
## m2 <- glm(cbind(C,tot.pred)~ grande.area-1, data=tmp1, family=binomial)
## m3 <- glm(cbind(C,tot.pred)~colegio -1, data=tmp1, family=binomial)
## m4 <- glm(cbind(C,tot.pred)~colegio+res.area -1, data=tmp1, family=binomial)
## m5 <- glm(cbind(C,tot.pred)~ res.area -1, data=tmp1, family=binomial)
## m6 <- glm(cbind(C,tot.pred)~colegio*res.area -1, data=tmp1, family=binomial)

## AICctab(m0,m1,m2,m3,m4, m5, m6)
logist <- function(x) exp(x)/(1+exp(x))
## logist(coef(m6))
## logist(confint(m6))

## For standalone journals + journals of potentially predatory publishers included by Scopus #
tmp2 <- merge(areas, n.pred2, by="area")
mb0 <- glm(cbind(C,tot.pred)~1 , data=tmp2, family=binomial)
mb1 <- glm(cbind(C,tot.pred)~area-1 , data=tmp2, family=binomial)
mb2 <- glm(cbind(C,tot.pred)~ grande.area-1, data=tmp2, family=binomial)
mb3  <- glm(cbind(C,tot.pred)~colegio -1, data=tmp2, family=binomial)
mb4 <- glm(cbind(C,tot.pred)~ res.area -1, data=tmp2, family=binomial)
mb5 <- glm(cbind(C,tot.pred)~colegio+res.area -1, data=tmp2, family=binomial)
mb6 <- glm(cbind(C,tot.pred)~colegio*res.area -1, data=tmp2, family=binomial)
#AICctab(mb0,mb1,mb2,mb3,mb4, mb5, mb6)
#logist(coef(mb1))
#logist(confint(m1))



################################################################################
## Number of journal titles
################################################################################
## Number of areas that pubished in each predatory journal
## Including both lists (standalone journals and journals from listed publishers)
pred.titles <- 
    qualis2 %>%
    filter(all.pred==TRUE) %>%
    group_by(titulo.abr) %>%
    summarize(N=n(), standalone=as.logical(max(pred)),
              bealls.scopus=as.logical(max(predp)),
              omics= as.logical(max(omics)),
              NC = sum(estrato=="C"),
              estrato.max=levels(estrato)[min(as.numeric(estrato))],
              estrato.min=levels(estrato)[max(as.numeric(estrato))])%>%
    as.data.frame
## Contingency tables minimum x maximum strata
with(pred.titles, table (estrato.min, estrato.max))
with(pred.titles[pred.titles$N>1,], table (estrato.min, estrato.max))
## Proporção dos periódicos avaliados por mais de uma área
sum(pred.titles$N>1)/nrow(pred.titles)
## proporcao que foi indicada como C por todas as áreas
with(pred.titles, sum(N>1&estrato.max=="C")/sum(pred.titles$N>1))
## proporção que foi indicada C por pelo menos uma
with(pred.titles, sum(N>1&estrato.min=="C")/sum(pred.titles$N>1))

## Proportion of titles classified in C strata by at least one area but no in other areas
100*with(pred.titles[pred.titles$N>1,], sum(estrato.min=="C"&estrato.max!="C")/length(estrato.min))
## Some figures
## Total of predadory journals in QUALIS
nrow(pred.titles)
## Number of predatory journals evaluated by more than one area
sum(pred.titles$N>1)
## Percentages of total number of journals in Qualis
100*nrow(pred.titles)/length(unique(qualis2$titulo.abr))
## Percentage of standalone relative to the total number of journals in Beals' list
100*sum(pred.titles$standalone)/nrow(beals)

## Same figures only for predatory journal NOT ranked as C by at least one area ##
## Total number in QUALIS
sum(pred.titles$estrato.max!="C")
## Percentage of the total number of predatory journals in QUALIS
100*sum(pred.titles$estrato.max!="C")/nrow(pred.titles)
## Percentage of the total number of journals in QUALIS
100*sum(pred.titles$estrato.max!="C")/length(unique(qualis2$titulo.abr))
## Percentage of the total number of journals in Beals' list
100*sum(pred.titles$estrato.max!="C"&pred.titles$standalone)/nrow(beals)

## Predatory journals ranked "A"
pred.titles[pred.titles$estrato.max=="A1",1]
## Checking qualis
qualis2[qualis2$titulo.abr%in%pred.titles[pred.titles$estrato.max=="A1",1]&qualis2$estrato=="A1",]
qualis2[qualis2$titulo.abr%in%pred.titles[pred.titles$estrato.max=="A2",1]&qualis2$estrato=="A2",c(1,3)]
qualis2[qualis2$titulo.abr%in%pred.titles[pred.titles$estrato.max=="B1",1]&qualis2$estrato=="B1",c(1,3)]
## Predatory journals ranked by one than one area
table(pred.titles$N)
## Predatory Journals ranked by more than one area and that have not spoted
pred.titles[pred.titles$N>1&pred.titles$NC==0,]
## Number of such journals and proportion of number of predatory journals in CAPES
sum(pred.titles$N>1&pred.titles$NC==0)
100*sum(pred.titles$N>1&pred.titles$NC==0)/nrow(pred.titles) 

length(unique(qualis2$titulo.abr))

## For DOAJ
pred.titles2 <- 
    qualis2 %>%
    filter(doaj.removed==TRUE) %>%
    group_by(titulo.abr) %>%
    summarize(N=n(), standalone=as.logical(max(pred)), NC = sum(estrato=="C"),
              estrato.max=levels(estrato)[min(as.numeric(estrato))],
              estrato.min=levels(estrato)[max(as.numeric(estrato))])%>%
    as.data.frame()
## Some figures
## Total of predadory journals in QUALIS
nrow(pred.titles2)
## Number of predatory journals evaluated by more than one area
sum(pred.titles2$N>1)
## Percentages of total number of journals in Qualis
100*nrow(pred.titles2)/length(unique(qualis2$titulo.abr))
## Checking against Beall's list
sum(pred.titles2$titulo.abr %in% pred.titles$titulo.abr)
pred.titles2[!(pred.titles2$titulo.abr %in% pred.titles$titulo.abr),] ## many legitimate titles like Caderno CEDES ...
## Maybe check titles in Beals list which are not in the Whitelist of DOAJ or are in the list of removed of DOAJ
