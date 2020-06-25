SET
MODELN
REGION
SCENARIO
VARIABLE2/
$include ../define/allvariables2.txt
"Emissions|3 Gases"
/
UNIT
YEAR
VAR_LossRate(VARIABLE2)/
"Policy Cost|GDP Loss rate"
"Policy Cost|Consumption Loss rate"
/
;

PARAMETER
ALLDATA_load(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR,*)       All Data (input data)
*BAUDATA(MODELN,REGION,VARIABLE2,UNIT,YEAR,*)             All BAU Data
ALLDATA(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR)              All Data for output
vsBAU(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR)                Change rate from BAU
;

$gdxin "../output/ALLDATA.gdx"
$load MODELN,REGION,SCENARIO,UNIT,YEAR,ALLDATA_load=ALLDATA
*$gdxin "../output/ALL_BAU.gdx"
*$load BAUDATA

ALLDATA(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR)=ALLDATA_load(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR,"Value");

ALLDATA(MODELN,SCENARIO,REGION,"Emissions|3 Gases","Mt CO2-equiv/yr",YEAR)
         =ALLDATA(MODELN,SCENARIO,REGION,"Emissions|CO2","Mt CO2/yr",YEAR)
         +ALLDATA(MODELN,SCENARIO,REGION,"Emissions|CH4","Mt CH4/yr",YEAR)*25
         +ALLDATA(MODELN,SCENARIO,REGION,"Emissions|N2O","kt N2O/yr",YEAR)*298/1000;

vsBAU(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR)$(ALLDATA(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR) AND ALLDATA(MODELN,"BAU",REGION,VARIABLE2,UNIT,YEAR))
         =(ALLDATA(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR)/ALLDATA(MODELN,"BAU",REGION,VARIABLE2,UNIT,YEAR)-1)*100;
vsBAU(MODELN,SCENARIO,REGION,VAR_LossRate,UNIT,YEAR)$ALLDATA(MODELN,SCENARIO,REGION,VAR_LossRate,UNIT,YEAR)
         =ALLDATA(MODELN,SCENARIO,REGION,VAR_LossRate,UNIT,YEAR);

$ontext
vsBAU(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR)$(ALLDATA(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR,"Value") AND BAUDATA(MODELN,REGION,VARIABLE2,UNIT,YEAR,"Value"))
         =(ALLDATA(MODELN,SCENARIO,REGION,VARIABLE2,UNIT,YEAR,"Value")/BAUDATA(MODELN,REGION,VARIABLE2,UNIT,YEAR,"Value")-1)*100;
vsBAU(MODELN,SCENARIO,REGION,VAR_LossRate,UNIT,YEAR)$ALLDATA(MODELN,SCENARIO,REGION,VAR_LossRate,UNIT,YEAR,"Value")
         =ALLDATA(MODELN,SCENARIO,REGION,VAR_LossRate,UNIT,YEAR,"Value");
$offtext

execute_unload '../output/vsBAU_out.gdx'
ALLDATA,vsBAU
