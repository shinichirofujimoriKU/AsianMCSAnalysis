=======
# This program is for Asian Mid-Centry Strategy Analysis
# 30, 06, 2020
# Yuki Ochi

library(gdxrrw)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(stringr)
library(maps)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(scales)
library(cowplot)

#-------------------------
# Make directories
#-------------------------
dir.create("../output/")
dir.create("../output/fig/")
dir.create("../output/fig/CrossCountries_vsBAU/")
dir.create("../output/fig/CrossCountries_vsBASE/")
dir.create("../output/fig/EffortSharing/")

#-------------------------
# Set figure theme
#-------------------------
pastelpal1 <- brewer.pal(9, "Pastel1")
spectpal <- brewer.pal(11, "Spectral")
AR6pal <- c("C1: 1.5C with no or low OS"="#8fbc8f","C2: 1.5C with high OS"="#7fffd4","C3: lower 2C"="#6495ed","C4: higher 2C"="#f0e68c","C5: below 2.5C"="#ffa07a","C6: below 3.0C"="#ee82ee","C7: above 3.0C"="#a9a9a9")

MyThemeLine_grid <- theme_bw() +
  theme(
    panel.border=element_rect(fill=NA),
    panel.grid.minor = element_line(color = NA), 
    axis.line=element_line(colour="black"),
    panel.background=element_rect(fill = "white"),
    panel.grid.major=element_blank(),
    strip.background=element_rect(fill="white", colour="white"),
    strip.text.x = element_text(size=15, colour = "black", angle = 0,face="bold"),
    axis.text.x=element_text(angle=45, vjust=0.9, hjust=1),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 7)
  )

MyThemeLine_grid2 <- MyThemeLine_grid +
  theme(
    strip.text.x = element_text(size=10),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 7)
  )

#-------------------------
# Country data of AIM/CGE results
#-------------------------
# Read and arrange data
LoadingCountries <- scan("../define/country_load.txt", what=character(), sep="\n", blank.lines.skip=F, quiet=T)
n <- 0
for(i in LoadingCountries){
  tmp <- read.csv(paste0("../data/",i,"_iiasa_database.csv"), header=T)
  assign(i, tmp)
  scenariomap <- read.table(paste0("../define/scenariomap_",i,".txt"),sep="\t",header=T)
  tmp2 <- tmp %>% inner_join(scenariomap,by="SCENARIO") %>% select(-SCENARIO) %>% rename(SCENARIO = SCENARIO2)
  assign(paste0(i,"2"),tmp2)
  n <- n + 1
  if(n==1){
    ALLDATA <- tmp2
  } else if(n>1){
    ALLDATA <- bind_rows(ALLDATA, tmp2)
  }   
}

ALLDATA1.0 <- rename(ALLDATA, "2005"=X2005,"2010"=X2010,"2015"=X2015,"2020"=X2020,"2025"=X2025,"2030"=X2030,"2035"=X2035,"2040"=X2040,"2045"=X2045,"2050"=X2050)

colhead <- c("MODEL","SCENARIO","REGION","VARIABLE","UNIT")
colhead2 <- c("MODELN","SCENARIO","REGION","VARIABLE2","UNIT")

variablemap <- read.table("../define/variablemap.txt",sep="\t",header=T) 
variablemap2 <- read.table("../define/variablemap2.txt",sep="\t",header=T) 

ALLDATA1.1 <- ALLDATA1.0 %>% gather(key=YEAR,value=VALUE,-MODEL,-SCENARIO,-REGION,-VARIABLE,-UNIT) %>% 
  inner_join(variablemap,by="VARIABLE") %>% select(-VARIABLE) %>% rename(MODELN = MODEL) %>%
  select(append(colhead2,c("YEAR","VALUE")))

row.has.na.ALLDATA <- apply(ALLDATA1.1, 1, function(x){any(is.na(x))})
ALLDATA1.2 <- ALLDATA1.1[!row.has.na.ALLDATA,] 

allvariables2 <- paste0("\"",sort(as.vector(unique(ALLDATA1.2$VARIABLE2))),"\"")
write(allvariables2, "../output/allvariables2.txt")

allunits <- paste0("\"",sort(as.vector(unique(ALLDATA1.2$UNIT))),"\"")
write(allunits, "../output/allunits.txt")

# Make data of change rate from BAU
symDim1 <- 7
attr(ALLDATA1.2, "symName") <- "ALLDATA"
lst1 <- wgdx.reshape(ALLDATA1.2, symDim1)
wgdx.lst(gdxName = "../output/ALLDATA.gdx",lst1)

system(paste("gams ChangeRate.gms",sep=" "))

vsBAU2.0 <- rgdx.param("../output/ChangeRate_out.gdx","vsBAU") %>% rename("VALUE"=vsBAU)
vsBAU2.0$UNIT <- "%" 
vsBASE2.0 <- rgdx.param("../output/ChangeRate_out.gdx","vsBASE") %>% rename("VALUE"=vsBASE)
vsBASE2.0$UNIT <- "%" 
ALLDATA2.0 <- rgdx.param("../output/ChangeRate_out.gdx","ALLDATA") %>% rename("VALUE"=ALLDATA)

ALLDATA2.1 <- ALLDATA2.0 %>% inner_join(variablemap2,by="VARIABLE2") %>% select(-VARIABLE2) %>% 
  rename(MODEL = MODELN) %>% select(append(colhead,c("YEAR","VALUE")))
vsBAU2.1 <- vsBAU2.0 %>% inner_join(variablemap2,by="VARIABLE2") %>% select(-VARIABLE2) %>% 
  rename(MODEL = MODELN) %>% select(append(colhead,c("YEAR","VALUE")))
vsBASE2.1 <- vsBASE2.0 %>% inner_join(variablemap2,by="VARIABLE2") %>% select(-VARIABLE2) %>% 
  rename(MODEL = MODELN) %>% select(append(colhead,c("YEAR","VALUE")))

vsBAU2.1$vsBAU <- "Change rate from BAU|"
vsBAU2.1 <- mutate(vsBAU2.1, VARIABLE_vsBAU = paste(!!!rlang::syms(c("vsBAU", "VARIABLE")), sep=""))
vsBAU2.2 <- vsBAU2.1 %>% select(-VARIABLE) %>% select(-vsBAU) %>%
  rename(VARIABLE = VARIABLE_vsBAU) %>% select(append(colhead,c("YEAR","VALUE")))

vsBASE2.1$vsBASE <- "Change rate from 2010|"
vsBASE2.1 <- mutate(vsBASE2.1, VARIABLE_vsBASE = paste(!!!rlang::syms(c("vsBASE", "VARIABLE")), sep=""))
vsBASE2.2 <- vsBASE2.1 %>% select(-VARIABLE) %>% select(-vsBASE) %>%
  rename(VARIABLE = VARIABLE_vsBASE) %>% select(append(colhead,c("YEAR","VALUE")))

bind_rows_all <- function(dfs, ...){
  base <- dfs[1]
  lapply(dfs[-1], function(i) base <<- bind_rows(base, i, ...))
  return(base)
}

ALLDATA3.0 <- bind_rows_all(list(ALLDATA2.1, vsBAU2.2,vsBASE2.2))

RedRate_3gas_vsBAU <- vsBAU2.2 %>% subset(VARIABLE=="Change rate from BAU|Emissions|3 Gases") %>%
  select(-VARIABLE) %>% rename(ReductionRate = VALUE)
RedRate_3gas_vsBAU$ReductionRate <- -1 * RedRate_3gas_vsBAU$ReductionRate 

RedRate_3gas_vsBASE <- vsBASE2.2 %>% subset(VARIABLE=="Change rate from 2010|Emissions|3 Gases") %>%
  select(-VARIABLE) %>% rename(ReductionRate = VALUE)
RedRate_3gas_vsBASE$ReductionRate <- -1 * RedRate_3gas_vsBASE$ReductionRate 

vsBAU.Red <- inner_join(ALLDATA3.0, select(RedRate_3gas_vsBAU,-UNIT), by=c("MODEL","SCENARIO","REGION","YEAR"))
vsBASE.Red <- inner_join(ALLDATA3.0, select(RedRate_3gas_vsBASE,-UNIT), by=c("MODEL","SCENARIO","REGION","YEAR"))

variable_BAU.Red <- as.vector(unique(vsBAU.Red$VARIABLE))
write(variable_BAU.Red, "../output/variable_BAU.Red.txt")
unit_BaU.Red <- unique(select(vsBAU.Red, c("VARIABLE","UNIT")))
plot_BAU.Red_load <- read.table("../define/plot_BAU.Red.txt",header=F,sep="\t",)
plot_BAU.Red <- as.vector(plot_BAU.Red_load$V1)
unit_BAU.Red_load <- unique(select(vsBAU.Red,c("VARIABLE","UNIT")))
unit_BAU.Red <- left_join(rename(plot_BAU.Red_load,VARIABLE=V1),unit_BAU.Red_load)
#name_BAU.Red <- plot_BAU.Red %>% str_sub(start = 22)
name_BAU.Red <- lapply(plot_BAU.Red, gsub, pattern="|", replacement="_", fixed=TRUE)
name_BAU.Red <- lapply(name_BAU.Red, gsub, pattern="w/o", replacement="wo", fixed=TRUE)
name_BAU.Red <- lapply(name_BAU.Red, gsub, pattern="/", replacement="per", fixed=TRUE)

variable_BASE.Red <- as.vector(unique(vsBASE.Red$VARIABLE))
write(variable_BASE.Red, "../output/variable_BASE.Red.txt")
unit_BaSE.Red <- unique(select(vsBAU.Red, c("VARIABLE","UNIT")))
plot_BASE.Red_load <- read.table("../define/plot_BASE.Red.txt",header=F,sep="\t",)
plot_BASE.Red <- as.vector(plot_BASE.Red_load$V1)
unit_BASE.Red_load <- unique(select(vsBASE.Red,c("VARIABLE","UNIT")))
unit_BASE.Red <- left_join(rename(plot_BASE.Red_load,VARIABLE=V1),unit_BASE.Red_load)
#name_BASE.Red <- plot_BASE.Red %>% str_sub(start = 23)
name_BASE.Red <- lapply(plot_BASE.Red, gsub, pattern="|", replacement="_", fixed=TRUE)
name_BASE.Red <- lapply(name_BASE.Red, gsub, pattern="w/o", replacement="wo", fixed=TRUE)
name_BASE.Red <- lapply(name_BASE.Red, gsub, pattern="/", replacement="_", fixed=TRUE)

GDPpCap <- ALLDATA2.1 %>% subset(VARIABLE=="GDP per capita") %>%
  select(-VARIABLE) %>% rename(GDPpCap = VALUE)
vsGDPpCap <- inner_join(ALLDATA2.1, select(GDPpCap,-UNIT), by=c("MODEL","SCENARIO","REGION","YEAR"))

#-------------------------
# Effort sharing
#-------------------------
EA <- rgdx.param("../data/Results.gdx", "EA")
names(EA) <- c("Approach","Country","Range","Goal","Year","Emission_Pathway")
EMI <- rgdx.param("../data/Results.gdx", "EMI")
names(EMI) <- c("Country","Type","Year","Emission")
EA$Year <- as.numeric(levels(EA$Year))[EA$Year]
EMI$Year <- as.numeric(levels(EMI$Year))[EMI$Year]
Country_List <- scan("../define/countries.txt", what=character(), sep="\n", blank.lines.skip=F, quiet=T)

EMI$Red10 <- EMI$Emission * 0.9
EMI$Red20 <- EMI$Emission * 0.8
EMI$Red30 <- EMI$Emission * 0.7
EMI$Red40 <- EMI$Emission * 0.6
EMI$Red50 <- EMI$Emission * 0.5
EMI$Red60 <- EMI$Emission * 0.4
EMI$Red70 <- EMI$Emission * 0.3
EMI$Red80 <- EMI$Emission * 0.2
EMI$Red90 <- EMI$Emission * 0.1

TGT <- data.frame(Country=c(Country_List))
TGT$MIN <- 0
TGT$MAX <- 0
n <- 0
for(i in Country_List){
  n <- n + 1
  if(i=="JPN"){
    TGT[n,2] <- min((subset(EMI, Country==i & Type=="GHGinLU" & Year>=1990, c(Emission)))) * 0.2
    TGT[n,3] <- max((subset(EMI, Country==i & Type=="GHGinLU" & Year>=1990, c(Emission)))) * 0.2
  }
}

#-------------------------
# Make Graphs
#-------------------------
# Relation between each variables and GHG reduction rate (Cross-countries)
for (i in 1:length(plot_BAU.Red)){
  vsBAU.Red.sel <- filter(vsBAU.Red, YEAR %in% c(2050) & VARIABLE==plot_BAU.Red[i]) 
  if(nrow(vsBAU.Red.sel)>=2){
    g1 <- ggplot() +
      geom_point(data=vsBAU.Red.sel, aes(x=ReductionRate, y=VALUE, color=REGION, fill=REGION),shape=21) + 
      ylab(unit_BAU.Red$UNIT[i]) + xlab("3 gases emission reduction rate from BAU (%)") +
      MyThemeLine_grid + 
      ggtitle(label=plot_BAU.Red[i]) + scale_colour_manual(values=pastelpal1)
    outname <- paste0("../output/fig/CrossCountries_vsBAU/",name_BAU.Red[i],".png")
    ggsave(g1, file=outname, dpi=600, width=4, height=3, limitsize=FALSE)
  }
}

for (i in 1:length(plot_BASE.Red)){
  vsBASE.Red.sel <- filter(vsBASE.Red, YEAR %in% c(2050) & VARIABLE==plot_BASE.Red[i]) 
  if(nrow(vsBASE.Red.sel)>=2){
    g2 <- ggplot() +
      geom_point(data=vsBASE.Red.sel, aes(x=ReductionRate, y=VALUE, color=REGION, fill=REGION),shape=21) + 
      ylab(unit_BASE.Red$UNIT[i]) + xlab("3 gases emission reduction rate from 2010 (%)") +
      MyThemeLine_grid + 
      ggtitle(label=plot_BASE.Red[i]) + scale_colour_manual(values=pastelpal1)
    outname <- paste0("../output/fig/CrossCountries_vsBASE/",name_BASE.Red[i],".png")
    ggsave(g2, file=outname, dpi=600, width=4, height=3, limitsize=FALSE)
  }
}

# GHG emissions in 2050 for 1.5/2 degree goal in each effort sharing approach (By country)
for(i in Country_List){
  minv <- subset(TGT, Country==i, c(MIN))
  maxv <- subset(TGT, Country==i, c(MAX))
  minv <- as.vector(minv$MIN)
  maxv <- as.vector(maxv$MAX)
  g3 <- ggplot(subset(EA, Country==i & Year==2050), aes(x=Approach, y=Emission_Pathway, group=Approach, shape=Approach, colour=Goal)) +
    geom_point() + geom_line() + labs(y="Emission Pathway (MtCO2eq/year)", title=i) + facet_wrap(~Goal) + 
    geom_hline(data=subset(EMI, Country==i & Type=="GHGinLU" & Year==2010), aes(yintercept=Emission), linetype="dashed", colour="red") +
    geom_hline(data=subset(EMI, Country==i & Type=="GHGinLU" & Year==2010), aes(yintercept=Red10), linetype="dashed", colour="gray") +
    geom_hline(data=subset(EMI, Country==i & Type=="GHGinLU" & Year==2010), aes(yintercept=Red20), linetype="dashed", colour="gray") +
    geom_hline(data=subset(EMI, Country==i & Type=="GHGinLU" & Year==2010), aes(yintercept=Red30), linetype="dashed", colour="gray") +
    geom_hline(data=subset(EMI, Country==i & Type=="GHGinLU" & Year==2010), aes(yintercept=Red40), linetype="dashed", colour="gray") +
    geom_hline(data=subset(EMI, Country==i & Type=="GHGinLU" & Year==2010), aes(yintercept=Red50), linetype="dashed", colour="gray") +
    geom_hline(data=subset(EMI, Country==i & Type=="GHGinLU" & Year==2010), aes(yintercept=Red60), linetype="dashed", colour="gray") +
    geom_hline(data=subset(EMI, Country==i & Type=="GHGinLU" & Year==2010), aes(yintercept=Red70), linetype="dashed", colour="gray") +
    geom_hline(data=subset(EMI, Country==i & Type=="GHGinLU" & Year==2010), aes(yintercept=Red80), linetype="dashed", colour="gray") +
    geom_hline(data=subset(EMI, Country==i & Type=="GHGinLU" & Year==2010), aes(yintercept=Red90), linetype="dashed", colour="gray") +
    geom_hline(yintercept=0, linetype="dashed", colour="black") +
    annotate("rect", ymin=minv, ymax=maxv, xmin=-Inf, xmax=Inf, alpha=0.2, fill=spectpal[10]) +
    MyThemeLine_grid
  outname <- paste0("../output/fig/EffortSharing/Emission_Pathway_",i,".png")
  ggsave(g3, file=outname, dpi=600, width=4, height=5, limitsize=FALSE)
}

