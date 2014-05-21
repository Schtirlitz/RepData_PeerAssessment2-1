Impact Analysis: How Severe Weather Events impact the United States 
========================================================

Synopsis
--------
This analysis hopes to find some answers to the following questions:  
1. What weather events are most harmful with respect to poulation health?  
2. Which events have the greatest ecomomic consequences?  

These first observations show that tornados are the largest cause of lifes  
whereas floods generate the biggest finanicial losses

Loading and Processing the Raw Data
-----------------------------------

The data can be downloaded from [Storm Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2). Please uncompress the data with a utility such as   bunzip2 or gunzip. If you wish to rerun these calculations, please use setwd() to set the   working directory to the directory where "StormData.csv" is located. 


```r
storm_table <- read.csv("./StormData.csv", nrow = 1)
names(storm_table)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```

A more detailed description of the columns can be found [here](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)

For the discussion of damages to person and property only the following columns are   relevant:   


```r

storm_table <- read.table("./StormData.csv", header = TRUE, sep = ",", colClasses = c(rep("NULL", 
    7), "factor", rep("NULL", 14), rep("numeric", 2), "numeric", "factor", "numeric", 
    "factor", rep("NULL", 9)))

names(storm_table)
```

```
## [1] "EVTYPE"     "FATALITIES" "INJURIES"   "PROPDMG"    "PROPDMGEXP"
## [6] "CROPDMG"    "CROPDMGEXP"
```


RESULTS
-------
### What category of weather has the biggest impact on human lifes? 
In order to answer this question we aggregate the fatalities and injures by weather type:


```r
# maximum number of rows shown in this publication
max_row <- 40
library(plyr)
ev_table <- ddply(storm_table, "EVTYPE", function(x) colSums(x[c("FATALITIES", 
    "INJURIES")]))
num_rows <- if (nrow(ev_table) > max_row) max_row else nrow(ev_table)

```

The following table displays the 40 most dangerous weather patterns in the US:


```r
s_data <- ev_table[with(ev_table, order(-ev_table[2], -ev_table[, 3])), ]
s_data[1:num_rows, ]
```

```
##                         EVTYPE FATALITIES INJURIES
## 834                    TORNADO       5633    91346
## 130             EXCESSIVE HEAT       1903     6525
## 153                FLASH FLOOD        978     1777
## 275                       HEAT        937     2100
## 464                  LIGHTNING        816     5230
## 856                  TSTM WIND        504     6957
## 170                      FLOOD        470     6789
## 585                RIP CURRENT        368      232
## 359                  HIGH WIND        248     1137
## 19                   AVALANCHE        224      170
## 972               WINTER STORM        206     1321
## 586               RIP CURRENTS        204      297
## 278                  HEAT WAVE        172      309
## 140               EXTREME COLD        160      231
## 760          THUNDERSTORM WIND        133     1488
## 310                 HEAVY SNOW        127     1021
## 141    EXTREME COLD/WIND CHILL        125       24
## 676                STRONG WIND        103      280
## 30                    BLIZZARD        101      805
## 350                  HIGH SURF        101      152
## 290                 HEAVY RAIN         98      251
## 142               EXTREME HEAT         96      155
## 79             COLD/WIND CHILL         95       12
## 427                  ICE STORM         89     1975
## 957                   WILDFIRE         75      911
## 411          HURRICANE/TYPHOON         64     1275
## 786         THUNDERSTORM WINDS         64      908
## 188                        FOG         62      734
## 402                  HURRICANE         61       46
## 848             TROPICAL STORM         58      340
## 342       HEAVY SURF/HIGH SURF         42       48
## 442                  LANDSLIDE         38       52
## 376                 HIGH WINDS         35      302
## 66                        COLD         35       48
## 978             WINTER WEATHER         33      398
## 877                    TSUNAMI         33      129
## 888  UNSEASONABLY WARM AND DRY         29        0
## 919       URBAN/SML STREAM FLD         28       79
## 980         WINTER WEATHER/MIX         28       72
## 842 TORNADOES, TSTM WIND, HAIL         25        0
```


To summarize the most deadly weather patterns in the US the 5 highest ranked are display   in a pie chart. All other weather related causes are grouped in "OTHER".

```r

slices <- c(s_data[1:5, 2], sum(s_data[6:nrow(s_data), 2]))
lbls <- c(as.character(s_data$EVTYPE[1]), as.character(s_data$EVTYPE[2]), as.character(s_data$EVTYPE[3]), 
    as.character(s_data$EVTYPE[4]), as.character(s_data$EVTYPE[5]), "OTHER")
pie(round(slices/sum(slices) * 100), labels = lbls, col = rainbow(length(lbls)), 
    main = "Fatalities by Weather Pattern")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

### What category of weather has the biggest financial impact on the US?
To answer this question we summarize the damage to property and crop and aggregate the summarized data based on the weather type.


```r
# utility function to aggregate the cost per event
sum_damage <- function(x) {
    
    if (toupper(x[5]) == "H") {
        mul_p <- 100
    } else if (toupper(x[5]) == "K") {
        mul_p <- 1000
    } else if (toupper(x[5]) == "M") {
        mul_p <- 1e+06
    } else if (toupper(x[5]) == "B") {
        mul_p <- 1e+09
    } else {
        mul_p <- 0
    }
    
    if (toupper(x[7]) == "H") {
        mul_c <- 100
    } else if (toupper(x[7]) == "K") {
        mul_c <- 1000
    } else if (toupper(x[7]) == "M") {
        mul_c <- 1e+06
    } else if (toupper(x[7]) == "B") {
        mul_c <- 1e+09
    } else {
        mul_c <- 0
    }
    (as.numeric(x[4]) * mul_p + as.numeric(x[6]) * mul_c)/1e+06
}

storm_table$"Sum of Damages in Mil $" <- apply(storm_table, 1, function(row) sum_damage(row))
ev_table <- ddply(storm_table, "EVTYPE", function(x) colSums(x["Sum of Damages in Mil $"]))
```

The following table displays the 40 weather patterns with the financial impact in the US:

```r
s_data <- ev_table[with(ev_table, order(-ev_table[2])), ]
s_data[1:num_rows, ]
```

```
##                         EVTYPE Sum of Damages in Mil $
## 170                      FLOOD                150319.7
## 411          HURRICANE/TYPHOON                 71913.7
## 834                    TORNADO                 57352.1
## 670                STORM SURGE                 43323.5
## 244                       HAIL                 18758.2
## 153                FLASH FLOOD                 17562.1
## 95                     DROUGHT                 15018.7
## 402                  HURRICANE                 14610.2
## 590                RIVER FLOOD                 10148.4
## 427                  ICE STORM                  8967.0
## 848             TROPICAL STORM                  8382.2
## 972               WINTER STORM                  6715.4
## 359                  HIGH WIND                  5908.6
## 957                   WILDFIRE                  5060.6
## 856                  TSTM WIND                  5038.9
## 671           STORM SURGE/TIDE                  4642.0
## 760          THUNDERSTORM WIND                  3898.0
## 408             HURRICANE OPAL                  3191.8
## 955           WILD/FOREST FIRE                  3108.6
## 299  HEAVY RAIN/SEVERE WEATHER                  2500.0
## 786         THUNDERSTORM WINDS                  1926.6
## 842 TORNADOES, TSTM WIND, HAIL                  1602.5
## 290                 HEAVY RAIN                  1427.6
## 140               EXTREME COLD                  1360.7
## 604        SEVERE THUNDERSTORM                  1205.6
## 212               FROST/FREEZE                  1103.6
## 310                 HEAVY SNOW                  1067.2
## 464                  LIGHTNING                   940.8
## 30                    BLIZZARD                   771.3
## 376                 HIGH WINDS                   649.0
## 954                 WILD FIRES                   624.1
## 879                    TYPHOON                   601.1
## 130             EXCESSIVE HEAT                   500.2
## 192                     FREEZE                   446.4
## 275                       HEAT                   403.3
## 405             HURRICANE ERIN                   394.1
## 442                  LANDSLIDE                   344.6
## 164             FLASH FLOODING                   322.9
## 161          FLASH FLOOD/FLOOD                   273.0
## 87             DAMAGING FREEZE                   270.1
```

To summarize the most damaging weather patterns in the US the 5 highest ranked are display in a pie chart 

```r

slices <- c(s_data[1:5, 2], sum(s_data[6:nrow(s_data), 2]))
lbls <- c(as.character(s_data$EVTYPE[1]), as.character(s_data$EVTYPE[2]), as.character(s_data$EVTYPE[3]), 
    as.character(s_data$EVTYPE[4]), as.character(s_data$EVTYPE[5]), "OTHER")
pie(round(slices/sum(slices) * 100), labels = lbls, col = rainbow(length(lbls)), 
    main = "Financial impact by Weather Pattern")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

Note: This date is somewhat skewed as the EVTYPE needs to get cleaned up. As an example "TSTM WIND" and "TSTM WIND (G45)" are considered different types. Also there are EVTYPEs like "Summary September 4", but since most of these have no impact on the results of this lab they were left in the table

