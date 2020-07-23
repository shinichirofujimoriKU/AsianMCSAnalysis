SET
MODELN
REGION
SCENARIO
VARIABLE2/
$include ../output/allvariables2.txt
"Emissions|3 Gases"
"Energy Intensity(PE/GDP|PPP)"
"Carbon Intensity(3 Gases/PE)"
"Energy Intensity Improvement Speed(PE/GDP|PPP)"
"Carbon Intensity Improvement Speed(3 Gases/PE)"
"Energy Intensity Improvement Speed(PE/GDP|PPP)|vs2020"
"Carbon Intensity Improvement Speed(3 Gases/PE)|vs2020"
"Share of Reneable Energy"
"Share of Reneable Energy|Non-Biomass"
"Share of Low Carbon Energy Source"
"Electrification Rate"
"CCS Rate"
/
UNIT/
$include ../output/allunits.txt
"t CO2-equiv/PE EJ"
/
YEAR
VAR_LossRate(VARIABLE2)/
"Policy Cost|GDP Loss rate"
"Policy Cost|Consumption Loss rate"
/
;

PARAMETER
ALLDATA_load(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR,*)       All Data (input data)
ALLDATA(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR)              All Data for output
vsBAU(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR)                Change rate from BAU
vsBASE(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR)               Change rate from the base year
;

$gdxin "../output/ALLDATA.gdx"
$load MODELN,REGION,SCENARIO,YEAR,ALLDATA_load=ALLDATA

ALLDATA(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR)=ALLDATA_load(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR,"Value");

* Make new variables
ALLDATA(MODELN,SCENARIO,REGION,"Emissions|3 Gases","Mt CO2-equiv/yr",YEAR)
         =ALLDATA(MODELN,SCENARIO,REGION,"Emissions|CO2","Mt CO2/yr",YEAR)
         +ALLDATA(MODELN,SCENARIO,REGION,"Emissions|CH4","Mt CH4/yr",YEAR)*25
         +ALLDATA(MODELN,SCENARIO,REGION,"Emissions|N2O","kt N2O/yr",YEAR)*298/1000;

ALLDATA(MODELN,SCENARIO,REGION,"Energy Intensity(PE/GDP|PPP)","PE GJ/GDP $",YEAR)$ALLDATA(MODELN,SCENARIO,REGION,"GDP|PPP","billion US$2005/yr",YEAR)
         =ALLDATA(MODELN,SCENARIO,REGION,"Primary Energy","EJ/yr",YEAR)/ALLDATA(MODELN,SCENARIO,REGION,"GDP|PPP","billion US$2005/yr",YEAR);

ALLDATA(MODELN,SCENARIO,REGION,"Carbon Intensity(3 Gases/PE)","CO2-equiv/PE EJ",YEAR)$ALLDATA(MODELN,SCENARIO,REGION,"Primary Energy","EJ/yr",YEAR)
         =ALLDATA(MODELN,SCENARIO,REGION,"Emissions|3 Gases","Mt CO2-equiv/yr",YEAR)/ALLDATA(MODELN,SCENARIO,REGION,"Primary Energy","EJ/yr",YEAR);

LOOP(YEAR,
         ALLDATA(MODELN,SCENARIO,REGION,"Energy Intensity Improvement Speed(PE/GDP|PPP)","%/year",YEAR)
                 $(ALLDATA(MODELN,SCENARIO,REGION,"Energy Intensity(PE/GDP|PPP)","PE GJ/GDP $",YEAR)>0 AND ALLDATA(MODELN,SCENARIO,REGION,"Energy Intensity(PE/GDP|PPP)","PE GJ/GDP $",YEAR-1)>0)
                 =-((ALLDATA(MODELN,SCENARIO,REGION,"Energy Intensity(PE/GDP|PPP)","PE GJ/GDP $",YEAR)/ALLDATA(MODELN,SCENARIO,REGION,"Energy Intensity(PE/GDP|PPP)","PE GJ/GDP $",YEAR-1))**(1/5)-1)*100;

         ALLDATA(MODELN,SCENARIO,REGION,"Carbon Intensity Improvement Speed(3 Gases/PE)","%/year",YEAR)
                 $(ALLDATA(MODELN,SCENARIO,REGION,"Carbon Intensity(3 Gases/PE)","CO2-equiv/PE EJ",YEAR)>0 AND ALLDATA(MODELN,SCENARIO,REGION,"Carbon Intensity(3 Gases/PE)","CO2-equiv/PE EJ",YEAR-1)>0)
                 =-((ALLDATA(MODELN,SCENARIO,REGION,"Carbon Intensity(3 Gases/PE)","CO2-equiv/PE EJ",YEAR)/ALLDATA(MODELN,SCENARIO,REGION,"Carbon Intensity(3 Gases/PE)","CO2-equiv/PE EJ",YEAR-1))**(1/5)-1)*100;
);

ALLDATA(MODELN,SCENARIO,REGION,"Energy Intensity Improvement Speed(PE/GDP|PPP)|vs2020","%/year",YEAR)$ALLDATA(MODELN,SCENARIO,REGION,"Energy Intensity(PE/GDP|PPP)","PE GJ/GDP $","2020")
         =-(ALLDATA(MODELN,SCENARIO,REGION,"Energy Intensity(PE/GDP|PPP)","PE GJ/GDP $",YEAR)-ALLDATA(MODELN,SCENARIO,REGION,"Energy Intensity(PE/GDP|PPP)","PE GJ/GDP $","2020"))
         /ALLDATA(MODELN,SCENARIO,REGION,"Energy Intensity(PE/GDP|PPP)","PE GJ/GDP $","2020")/30*100;
ALLDATA(MODELN,SCENARIO,REGION,"Carbon Intensity Improvement Speed(3 Gases/PE)|vs2020","%/year",YEAR)$ALLDATA(MODELN,SCENARIO,REGION,"Carbon Intensity(3 Gases/PE)","CO2-equiv/PE EJ","2020")
         =-(ALLDATA(MODELN,SCENARIO,REGION,"Carbon Intensity(3 Gases/PE)","CO2-equiv/PE EJ",YEAR)-ALLDATA(MODELN,SCENARIO,REGION,"Carbon Intensity(3 Gases/PE)","CO2-equiv/PE EJ","2020"))
         /ALLDATA(MODELN,SCENARIO,REGION,"Carbon Intensity(3 Gases/PE)","CO2-equiv/PE EJ","2020")/30*100;

ALLDATA(MODELN,SCENARIO,REGION,"Share of Reneable Energy|Non-Biomass","%",YEAR)$ALLDATA(MODELN,SCENARIO,REGION,"Primary Energy","EJ/yr",YEAR)
         =ALLDATA(MODELN,SCENARIO,REGION,"Primary Energy|Non-Biomass Renewables","EJ/yr",YEAR)/ALLDATA(MODELN,SCENARIO,REGION,"Primary Energy","EJ/yr",YEAR)*100;

ALLDATA(MODELN,SCENARIO,REGION,"Share of Reneable Energy","%",YEAR)$ALLDATA(MODELN,SCENARIO,REGION,"Primary Energy","EJ/yr",YEAR)
         =(ALLDATA(MODELN,SCENARIO,REGION,"Primary Energy|Non-Biomass Renewables","EJ/yr",YEAR)
         +ALLDATA(MODELN,SCENARIO,REGION,"Primary Energy|Biomass","EJ/yr",YEAR))
         /ALLDATA(MODELN,SCENARIO,REGION,"Primary Energy","EJ/yr",YEAR)*100;

ALLDATA(MODELN,SCENARIO,REGION,"Share of Low Carbon Energy Source","%",YEAR)$ALLDATA(MODELN,SCENARIO,REGION,"Primary Energy","EJ/yr",YEAR)
         =(ALLDATA(MODELN,SCENARIO,REGION,"Primary Energy|Non-Biomass Renewables","EJ/yr",YEAR)
         +ALLDATA(MODELN,SCENARIO,REGION,"Primary Energy|Biomass","EJ/yr",YEAR)
         +ALLDATA(MODELN,SCENARIO,REGION,"Primary Energy|Nuclear","EJ/yr",YEAR)
         +ALLDATA(MODELN,SCENARIO,REGION,"Primary Energy|Fossil|w/ CCS","EJ/yr",YEAR))
         /ALLDATA(MODELN,SCENARIO,REGION,"Primary Energy","EJ/yr",YEAR)*100;

ALLDATA(MODELN,SCENARIO,REGION,"Electrification Rate","%",YEAR)$ALLDATA(MODELN,SCENARIO,REGION,"Final Energy","EJ/yr",YEAR)
         =ALLDATA(MODELN,SCENARIO,REGION,"Final Energy|Electricity","EJ/yr",YEAR)/ALLDATA(MODELN,SCENARIO,REGION,"Final Energy","EJ/yr",YEAR)*100;

ALLDATA(MODELN,SCENARIO,REGION,"CCS Rate","%",YEAR)$(ALLDATA(MODELN,SCENARIO,REGION,"Emissions|Kyoto Gases","Mt CO2-equiv/yr",YEAR)+ALLDATA(MODELN,SCENARIO,REGION,"Carbon Sequestration|CCS","Mt CO2/yr",YEAR))
         =ALLDATA(MODELN,SCENARIO,REGION,"Carbon Sequestration|CCS","Mt CO2/yr",YEAR)
         /(ALLDATA(MODELN,SCENARIO,REGION,"Emissions|Kyoto Gases","Mt CO2-equiv/yr",YEAR)+ALLDATA(MODELN,SCENARIO,REGION,"Carbon Sequestration|CCS","Mt CO2/yr",YEAR));


* Calculate Change rate compared with BAU and base year
vsBAU(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR)$(ALLDATA(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR) AND ALLDATA(MODELN,"BAU",REGION,VARIABLE2,UNIT,YEAR))
         =(ALLDATA(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR)/ALLDATA(MODELN,"BAU",REGION,VARIABLE2,UNIT,YEAR)-1)*100;
vsBAU(MODELN,SCENARIO,REGION,VAR_LossRate,UNIT,YEAR)$ALLDATA(MODELN,SCENARIO,REGION,VAR_LossRate,UNIT,YEAR)
         =ALLDATA(MODELN,SCENARIO,REGION,VAR_LossRate,UNIT,YEAR);

vsBASE(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR)$(ALLDATA(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR) AND ALLDATA(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,"2010"))
         =(ALLDATA(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR)/ALLDATA(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,"2010")-1)*100;

execute_unload '../output/ChangeRate_out.gdx'
ALLDATA,vsBAU,vsBASE
