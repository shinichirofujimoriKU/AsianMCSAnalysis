# This program is for Asian Mid-Centry Strategy Analysis
# 03, 06, 2020
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
dir.create("../output/fig/CrossCountries/")
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
# Read data and definitions
#-------------------------
# Results from AIM/CGE
CHN <- read.csv("../data/CHN_iiasa_database.csv", header=T) 
IND <- read.csv("../data/IND_iiasa_database.csv", header=T) 
JPN <- read.csv("../data/JPN_iiasa_database_final.csv", header=T) 
KOR <- read.csv("../data/KOR_iiasa_database.csv", header=T) 
THA <- read.csv("../data/THA_iiasa_database.csv", header=T) 
VNM <- read.csv("../data/VNM_iiasa_database.csv", header=T) 

CHN_BAU <- subset(CHN, SCENARIO=="SSP2_BaU_NoCC")
IND_BAU <- subset(IND, SCENARIO=="SSP2_BaU_NoCC")
JPN_BAU <- subset(JPN, SCENARIO=="NDC_Nuk_M_BaU_NoCC")
KOR_BAU <- subset(KOR, SCENARIO=="SSP2-NS2_BaU_NoCC")
THA_BAU <- subset(THA, SCENARIO=="SSP2_THA_BaU_NoCC")
VNM_BAU <- subset(VNM, SCENARIO=="SSP2_NS1_BaU_NoCC")

colhead <- c("MODEL","SCENARIO","REGION","VARIABLE","UNIT")
colhead2 <- c("MODELN","SCENARIO","REGION","VARIABLE2","UNIT")
#yearxlist <- c("X2005","X2010","X2015","X2020","X2025","X2030","X2035","X2040","X2045","X2050")

variablemap <- read.table("../define/variablemap.txt",sep="\t",header=T) 
variablemap2 <- read.table("../define/variablemap2.txt",sep="\t",header=T) 

# Effort sharing
EA2 <- rgdx.param("../data/EA_2deg.gdx", "EA") 
EA15 <- rgdx.param("../data/EA_15deg.gdx", "EA") 
names(EA2) <- c("Approach","Country","Range","Year","Emission_Pathway")
names(EA15) <- c("Approach","Country","Range","Year","Emission_Pathway")
Country_List <- scan("../define/countries.txt", what=character(), sep="\n", blank.lines.skip=F, quiet=T)

#-------------------------
# Data processing
#-------------------------
# Arrange data format of AIM/CGE results
bind_rows_all <- function(dfs, ...){
  base <- dfs[1]
  lapply(dfs[-1], function(i) base <<- bind_rows(base, i, ...))
  return(base)
}

ALLDATA <- bind_rows_all(list(CHN, IND, JPN, KOR, THA, VNM))
ALL_BAU <- bind_rows_all(list(CHN_BAU, IND_BAU, JPN_BAU, KOR_BAU, THA_BAU, VNM_BAU))

ALLDATA1.0 <- rename(ALLDATA, "2005"=X2005,"2010"=X2010,"2015"=X2015,"2020"=X2020,"2025"=X2025,"2030"=X2030,"2035"=X2035,"2040"=X2040,"2045"=X2045,"2050"=X2050)
ALL_BAU1.0 <- rename(ALL_BAU, "2005"=X2005,"2010"=X2010,"2015"=X2015,"2020"=X2020,"2025"=X2025,"2030"=X2030,"2035"=X2035,"2040"=X2040,"2045"=X2045,"2050"=X2050)


ALLDATA1.1 <- ALLDATA1.0 %>% gather(key=YEAR,value=VALUE,-MODEL,-SCENARIO,-REGION,-VARIABLE,-UNIT) %>% 
  inner_join(variablemap,by="VARIABLE") %>% select(-VARIABLE) %>% rename(MODELN = MODEL) %>%
  select(append(colhead2,c("YEAR","VALUE")))
ALL_BAU1.1 <- ALL_BAU1.0 %>% gather(key=YEAR,value=VALUE,-MODEL,-SCENARIO,-REGION,-VARIABLE,-UNIT) %>% 
  inner_join(variablemap,by="VARIABLE") %>% select(-VARIABLE) %>% rename(MODELN = MODEL) %>%
  select(append(colhead2,c("YEAR","VALUE"))) %>% select(-SCENARIO)

row.has.na.ALLDATA <- apply(ALLDATA1.1, 1, function(x){any(is.na(x))})
ALLDATA1.2 <- ALLDATA1.1[!row.has.na.ALLDATA,] 
row.has.na.ALL_BAU <- apply(ALL_BAU1.1, 1, function(x){any(is.na(x))})
ALL_BAU1.2 <- ALL_BAU1.1[!row.has.na.ALL_BAU,] 

# Make data of change rate from BAU
symDim1 <- 7
symDim2 <- 6
attr(ALLDATA1.2, "symName") <- "ALLDATA"
attr(ALL_BAU1.2, "symName") <- "BAUDATA"
lst1 <- wgdx.reshape(ALLDATA1.2, symDim1)
wgdx.lst(gdxName = "../output/ALLDATA.gdx",lst1)
lst2 <- wgdx.reshape(ALL_BAU1.2, symDim2)
wgdx.lst(gdxName = "../output/ALL_BAU.gdx",lst2)

system(paste("gams vsBAU.gms",sep=" "))

vsBAU1.2 <- rgdx.param("../output/vsBAU.gdx","vsBAU") %>% rename("Value"=vsBAU)
vsBAU1.2$UNIT <- "%" 

ALLDATA1.3 <- ALLDATA1.2 %>% inner_join(variablemap2,by="VARIABLE2") %>% select(-VARIABLE2) %>% 
  rename(MODEL = MODELN) %>% select(append(colhead,c("YEAR","VALUE")))
vsBAU1.3 <- vsBAU1.2 %>% inner_join(variablemap2,by="VARIABLE2") %>% select(-VARIABLE2) %>% 
  rename(MODEL = MODELN) %>% rename(VALUE = Value) %>% select(append(colhead,c("YEAR","VALUE")))

vsBAU1.3$vsBAU <- "Change rate from BAU|"
vsBAU1.3 <- mutate(vsBAU1.3, VARIABLE_vsBAU = paste(!!!rlang::syms(c("vsBAU", "VARIABLE")), sep=""))
vsBAU1.4 <- vsBAU1.3 %>% select(-VARIABLE) %>% select(-vsBAU) %>%
  rename(VARIABLE = VARIABLE_vsBAU) %>% select(append(colhead,c("YEAR","VALUE")))

ALLDATA2.0 <- bind_rows(ALLDATA1.3, vsBAU1.4)

GHGReductionRate <- vsBAU1.4 %>% subset(VARIABLE=="Change rate from BAU|Emissions|Kyoto Gases") %>%
  select(-VARIABLE) %>% rename(ReductionRate = VALUE)
GHGReductionRate$ReductionRate <- -1 * GHGReductionRate$ReductionRate 

vsBAU.Red <- inner_join(vsBAU1.4, select(GHGReductionRate,-UNIT), by=c("MODEL","SCENARIO","REGION","YEAR"))

variable_BAU.Red <- as.vector(unique(vsBAU.Red$VARIABLE))
write(variable_BAU.Red, "../output/variable_BAU.Red.txt")
unit_BaU.Red <- unique(select(vsBAU.Red, c("VARIABLE","UNIT")))
plot_BAU.Red_load <- read.table("../define/plot_BAU.Red.txt",header=F,sep="\t",)
plot_BAU.Red <- as.vector(plot_BAU.Red_load$V1)
name_BAU.Red <- plot_BAU.Red %>% str_sub(start = 22)
name_BAU.Red <- lapply(name_BAU.Red, gsub, pattern="|", replacement="_", fixed=TRUE)
name_BAU.Red <- lapply(name_BAU.Red, gsub, pattern="w/o", replacement="wo", fixed=TRUE)

# Arrange data format of Effort sharing data
EA2$Goal <- "2C"
EA15$Goal <- "1.5C"
EA <- bind_rows(EA2, EA15)
EA <- EA[,c(1,2,6,3,4,5)]

#-------------------------
# Make Graphs
#-------------------------
# Relation between each variables and GHG reduction rate (Cross-countries)
for (i in 1:length(plot_BAU.Red)){
  vsBAU.Red.sel <- filter(vsBAU.Red, YEAR %in% c(2050) & VARIABLE==plot_BAU.Red[i]) 
  if(nrow(vsBAU.Red.sel)>=2){
    g1 <- ggplot() +
      geom_point(data=vsBAU.Red.sel, aes(x=ReductionRate, y=VALUE, color=REGION, fill=REGION),shape=21) + 
      ylab("%") + xlab("GHG emission reduction rate from BAU (%)") +
      MyThemeLine_grid + 
      ggtitle(label=plot_BAU.Red[i]) + scale_colour_manual(values=pastelpal1)
    outname <- paste0("../output/fig/CrossCountries/",name_BAU.Red[i],".png")
    ggsave(g1, file=outname, dpi=600, width=6, height=5, limitsize=FALSE)
  }
}

# GHG emissions in 2050 for 1.5/2 degree goal in each effort sharing approach (By country)
for(i in Country_List){
  g2 <- ggplot(subset(EA, Country==i & Year=="2050"), aes(x=Approach, y=Emission_Pathway, group=Approach, shape=Approach, colour=Goal)) +
    geom_point() + geom_line() + labs(y="Emission Pathway (MtCO2eq/year)", title=i) + facet_wrap(~Goal) + 
    MyThemeLine_grid
  outname <- paste0("../output/fig/EffortSharing/Emission_Pathway_",i,".png")
  ggsave(g2, file=outname, dpi=300, width=8, height=5, limitsize=F)
}

