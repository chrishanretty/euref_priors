# Estimate local authority opinion on EU referendum via: 
# global smoothing + constituency predictors + ILPP

rm(list = ls())

library(rio)
library(reshape2)
library(car)
library(arm)
library(lme4)
library(ggplot2)
library(plyr)
library(dplyr)
library(merTools)
library(boot)
library(lubridate)
library(gridExtra)

basenm <- "lad-eu-leave"
estimate  <- TRUE

### You'll need to download the BES data if you haven't got it
### See www.britishelectionstudy.com

bes <- import("data/BES2015_W6_v1.2.sav")
lu <- read.csv("data/bes_to_gss_lu.csv")
bes <- merge(bes, lu, all.x = T, all.y = F)
bes <- subset(bes, !is.na(GSSCode))

### Dependent variable:
###
if (grepl("leave",basenm)) {
    bes$y <- as.numeric(bes$euRefVote == "Leave the EU")
} else {
    bes$y <- as.numeric(bes$euRefVote == "Stay in the EU")
}

                                        # Age
## remove w/o response
bes <- subset(bes,!is.na(bes$y))

## Get auxiliary seat information
## From EP results
ep <- read.csv("data/ep_checked.csv")
ep <- ep[,c("GSSCode","PctTurnout","Con","UKIP")]
ep <- subset(ep, !is.na(GSSCode))

### AGE
### Get median
qs103ew <- read.csv("data/QS103EW.csv")
median.pos <- apply(qs103ew[,3:ncol(qs103ew)], 1, function(x) {
    which(cumsum(x) > (sum(x)/2))[1]
})
qs103ew <- data.frame(GSSCode = qs103ew$mnemonic,
                      MedianAge = median.pos - 1)
qs103sc <- read.csv("data/QS103SC.csv")
median.pos <- apply(qs103sc[,3:ncol(qs103sc)], 1, function(x) {
    which(cumsum(x) > (sum(x)/2))[1]
})
qs103sc <- data.frame(GSSCode = qs103sc$Scottish.Council.Area.2011,
                      MedianAge = median.pos - 1)
qs103 <- rbind(qs103ew, qs103sc)

## Tenure
## England
qs403ew <- read.csv("data/QS403EW.csv")
qs403ew$Owned <- qs403ew$Owned..Total / rowSums(qs403ew[,3:ncol(qs403ew)],na.rm = T)
qs403ew <- data.frame(GSSCode = qs403ew$mnemonic,
                      OwnedPct = qs403ew$Owned)

qs402sc <- read.csv("data/QS402SC.csv")
qs402sc$Owned <- rowSums(qs402sc[,c("Owned..Owned.outright","Owned..Owned.with.a.mortgage.or.loan")], na.rm = T) /
    rowSums(qs402sc[,3:ncol(qs402sc)], na.rm = T)
qs402sc <- data.frame(GSSCode = qs402sc$Scottish.Council.Area.2011,
                      OwnedPct = qs402sc$Owned)
qs403 <- rbind(qs403ew, qs402sc)

### Education
qs501ew <- read.csv("data/QS501EW.csv")
### Add in apprenticeship to Level 1
qs501ew$Level.1.qualifications <- qs501ew$Level.1.qualifications +
    qs501ew$Apprenticeship
qs501ew$Apprenticeship <- NULL
### Add in other to Level 1
qs501ew$Level.1.qualifications <- qs501ew$Level.1.qualifications +
    qs501ew$Other.qualifications
qs501ew$Other.qualifications <- NULL

qs501ew[,3:ncol(qs501ew)] <- qs501ew[,3:ncol(qs501ew)] / rowSums(qs501ew[,3:ncol(qs501ew)])

qs501sc <- read.csv("data/QS501SC.csv")
qs501sc$Highest.level.of.qualification <- sub("All people aged 16 and over: ","",
                                              qs501sc$Highest.level.of.qualification)
qs501sc <- subset(qs501sc, Highest.level.of.qualification != "Total")
qs501sc <- dcast(qs501sc, Scottish.Council.Area.2011 ~ Highest.level.of.qualification,
                 value = "Count")
qs501sc[,2:ncol(qs501sc)] <- qs501sc[,2:ncol(qs501sc)] / rowSums(qs501sc[,2:ncol(qs501sc)])

qs501sc <- data.frame(GSSCode = qs501sc$Scottish.Council.Area.2011,
                      EducL1 = qs501sc$`Level 1`,
                      EducL2 = qs501sc$`Level 2`,
                      EducL3 = qs501sc$`Level 3`,
                      EducL4 = qs501sc$`Level 4 and above`)

qs501ew <- data.frame(GSSCode = qs501ew$mnemonic,
                      EducL1 = qs501ew$`Level.1.qualifications`,
                      EducL2 = qs501ew$`Level.2.qualifications`,
                      EducL3 = qs501ew$`Level.3.qualifications`,
                      EducL4 = qs501ew$`Level.4.qualifications.and.above`)

qs501 <- rbind(qs501ew, qs501sc)

### NS-SEC
qs607ew <- read.csv("data/QS607EW.csv")
names(qs607ew) <- c("name", "GSSCode",
                    paste0("NSSEC",1:8),
                    "NSSECNC")
qs607ew[,3:ncol(qs607ew)] <- qs607ew[,3:ncol(qs607ew)] / rowSums(qs607ew[,3:ncol(qs607ew)])
qs607ew$name <- NULL

qs607sc <- read.csv("data/QS607SC.csv")
names(qs607sc) <- c("GSSCode","NSSec","Count")
qs607sc <- subset(qs607sc,
                  NSSec != "All people aged 16 to 74")
qs607sc$NSSec <- gsub(" .*","",qs607sc$NSSec)
qs607sc$NSSec <- gsub("\\..*","",qs607sc$NSSec)
qs607sc$NSSec <- car:::recode(qs607sc$NSSec,
                              "c('L1','L2','L3')='NSSEC1';
c('L4','L5','L6')='NSSEC2';
c('L7')='NSSEC3';
c('L8','L9')='NSSEC4';
c('L10','L11')='NSSEC5';
c('L12')='NSSEC6';
c('L13')='NSSEC7';
c('L14')='NSSEC8';
else ='NSSECNC'")
qs607sc <- qs607sc %>%
    group_by(GSSCode, NSSec) %>%
    summarize(Count = sum(Count))
qs607sc <- dcast(qs607sc, GSSCode ~ NSSec,
                 value.var = "Count")
qs607sc[,2:ncol(qs607sc)] <- qs607sc[,2:ncol(qs607sc)] /
    rowSums(qs607sc[,2:ncol(qs607sc)])

qs607 <- rbind(qs607ew, qs607sc)

aux.seat <- merge(ep, qs103, all = T)
if (nrow(aux.seat) != nrow(qs103)) {
    stop("merge introduced new rows")
}
aux.seat <- merge(aux.seat, qs403, all = T)
if (nrow(aux.seat) != nrow(ep)) {
    stop("merge introduced new rows")
}
aux.seat <- merge(aux.seat, qs501, all = T)
if (nrow(aux.seat) != nrow(ep)) {
    stop("merge introduced new rows")
}
aux.seat <- merge(aux.seat, qs607, all = T)
if (nrow(aux.seat) != nrow(ep)) {
    stop("merge introduced new rows")
}
## merge in region information to LAD-level data
lad2region <- read.csv("../local_auths/lad2region.csv", header = TRUE)
aux.seat <- merge(aux.seat, lad2region[,c("LAD13CD", "LAD13NM","GOR10NM")],
                  by.x = "GSSCode", 
                  by.y = "LAD13CD",
                  all.x = TRUE)
names(aux.seat)[names(aux.seat)=="GOR10NM"] <- "region"
aux.seat$region <- as.character(aux.seat$region)
aux.seat$region[grep("^S",aux.seat$GSSCode)] <- "Scotland"
aux.seat$region[grep("^W",aux.seat$GSSCode)] <- "Wales"
aux.seat$region[aux.seat$GSSCode == "E06000057"] <- "North East"
aux.seat$region[aux.seat$GSSCode == "E07000240"] <- "East of England"
aux.seat$region[aux.seat$GSSCode == "E07000241"] <- "East of England"
aux.seat$region[aux.seat$GSSCode == "E07000242"] <- "East of England"
aux.seat$region[aux.seat$GSSCode == "E07000243"] <- "East of England"
aux.seat$region[aux.seat$GSSCode == "E08000037"] <- "North East"

# merge in seat-level variables to survey data
bes <- merge(bes, aux.seat, 
    by.x = "GSSCode", 
    by.y = "GSSCode",
    all.x = TRUE,
    suffixes = c(".x","")) # suffix option ensures that, where variable names match, aux.seat versions take primacy (no suffix)


cont.vars <- c("Con","UKIP","MedianAge","OwnedPct",
               "EducL1","EducL2","EducL3","EducL4",
               "NSSEC1","NSSEC2","NSSEC3","NSSEC4","NSSEC5","NSSEC6","NSSEC7","NSSEC8","NSSECNC")

for (v in cont.vars) {
    bes[,v] <- as.vector(scale(bes[,v]))
}

outmod <- glmer(y ~ Con + UKIP +
                MedianAge + OwnedPct +
                EducL1 + EducL2 + EducL3 + EducL4 + ### none omitted
                NSSEC1 + NSSEC2 + NSSEC4 + NSSEC5 + NSSEC6 + NSSEC7 + NSSEC8 + NSSECNC + # NSSEC4 omitted
                (1|region) + (1|GSSCode),
            family = binomial,
            data = bes)
newdata <- unique(bes[,c("GSSCode","region",
                     "NSSEC1", "NSSEC2", "NSSEC4", "NSSEC5", "NSSEC6", "NSSEC7", "NSSEC8", "NSSECNC",
                     "EducL1", "EducL2", "EducL3", "EducL4",
                     "MedianAge", "OwnedPct",
                     "Con", "UKIP")])

bes$y <- as.numeric(bes$euRefVote == "Stay in the EU")
inmod <- glmer(y ~ Con + UKIP +
                MedianAge + OwnedPct +
                EducL1 + EducL2 + EducL3 + EducL4 + ### none omitted
                NSSEC1 + NSSEC2 + NSSEC4 + NSSEC5 + NSSEC6 + NSSEC7 + NSSEC8 + NSSECNC + # NSSEC4 omitted
                (1|region) + (1|GSSCode),
            family = binomial,
            data = bes)

### Get a matrix of predictions

outpreds <- predictInterval(outmod,
                            newdata = newdata,
                            n.sims = 999,
							include.resid.var = FALSE,
                            returnSims = TRUE )

inpreds <- predictInterval(inmod,
                            newdata = newdata,
                            n.sims = 999,
							include.resid.var = FALSE,
                            returnSims = TRUE )


save.image("preds.RData")

### Work out what the national average is
### just using the fit for each observation
ep <- read.csv("data/ep_checked.csv")
newdata$check <- 1:nrow(newdata)
newdata <- merge(newdata, ep[,c("GSSCode","Eligible")],
	all.x = T, all.y = F,
	sort = FALSE)
newdata$leave <- inv.logit(outpreds$fit)
newdata$remain <- inv.logit(inpreds$fit)

### Check the unweighted mean
mean(newdata$leave)
mean(newdata$remain)

### Check the weighted mean
wtd.leave <- weighted.mean(newdata$leave, newdata$Eligible)
wtd.remain <- weighted.mean(newdata$remain, newdata$Eligible)

### How much do these differ from current national polling?
### average of last three polls:
### Leave
leave.tgt <- mean(c(.41,.41,.47))
remain.tgt <- mean(c(.41,.43,.44))

add.to.leave <- leave.tgt - wtd.leave
add.to.remain <- remain.tgt - wtd.remain

leavesims <- inv.logit(attr(outpreds, "sim.results")) + add.to.leave
remainsims <- inv.logit(attr(inpreds, "sim.results")) + add.to.remain

leavesims <- melt(leavesims)
names(leavesims) <- c("LA","iter","leave")
leavesims$GSSCode <- newdata$GSSCode[leavesims$LA]
leavesims$LA <- NULL
remainsims <- melt(remainsims)
names(remainsims) <- c("LA","iter","remain")
remainsims$GSSCode <- newdata$GSSCode[remainsims$LA]
remainsims$LA <- NULL
sims <- merge(remainsims, leavesims, all = T)

### Now work out the gap between the two over iterations
sims$gap <- (sims$remain - sims$leave)

sims <- sims %>% 
	group_by(GSSCode) %>%
	summarise(meanleave = mean(leave),
		hileave = quantile(leave, 0.95),
		loleave = quantile(leave, 0.05),
		hiremain = quantile(remain, 0.95),
		loremain = quantile(remain, 0.05),
		meanremain = mean(remain),
		mediangap = median(gap),
		meangap = mean(gap),
		lo = quantile(gap, 0.025),
		hi = quantile(gap, 0.975))

sims <- merge(sims, ep[,c("GSSCode","Local.Authority","UKIP")])

### Add in information about times
timings <- read.csv("data/EURef_timingvenue_KK_withGSS.csv")
timings$Datetime <- gsub("‘","",timings$Datetime)
timings$Datetime <- ymd_hms(timings$Datetime)
timings$HoursAfterMidnight <- as.numeric(timings$Datetime - ymd_hms("2016-06-24 00:00:01"))/60

timings <- merge(timings, sims, by = "GSSCode", all.x = T, all.y = T)

tmp <- subset(timings, !is.na(GSSCode))
tmp <- tmp[order(tmp$HoursAfterMidnight),]

tmp$rankleave <- rank(-tmp$meanleave)
tmp$rankremain <- rank(-tmp$meanremain)
tmp$rankgap <- rank(-tmp$meangap)
tmp$meangap <- round(tmp$meangap * 100,1)
tmp$meanleave <- round(tmp$meanleave * 100,1)
tmp$meanremain <- round(tmp$meanremain * 100,1)

tmp$remainci <- paste0("(",
	round(tmp$loremain*100,1),", ",
	round(tmp$hiremain*100,1),")")


tmp$leaveci <- paste0("(",
	round(tmp$loleave*100,1),", ",
	round(tmp$hileave*100,1),")")

tmp$gapci <- paste0("(",
	round(tmp$lo*100,1),", ",
	round(tmp$hi*100,1),")")

tmp <- tmp[,c("Counting.Officer", "meanremain", "remainci", "meanleave", "leaveci", "meangap", "gapci")]
names(tmp) <- c("Area","Remain","90% CI", "Leave", "90% CI", "Gap","90% CI")
write.csv(tmp, file ="data/results.csv", row.names = FALSE)
tmp <- tmp[1:5,]

grid.table(tmp, rows = NULL)


