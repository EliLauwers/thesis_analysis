﻿* Encoding: UTF-8.
* Custom Tables.
CTABLES
  /VLABELS VARIABLES=p_lo p_mid p_hi cond_prate DISPLAY=DEFAULT
  /TABLE p_lo [C] + p_mid [C] + p_hi [C] BY cond_prate [C][COLPCT.COUNT PCT40.1]
  /CATEGORIES VARIABLES=p_lo p_mid p_hi ORDER=A KEY=VALUE EMPTY=EXCLUDE
  /CATEGORIES VARIABLES=cond_prate ORDER=A KEY=VALUE EMPTY=INCLUDE
  /COMPARETEST TYPE=PROP ALPHA=0.05 ADJUST=NONE ORIGIN=COLUMN INCLUDEMRSETS=YES 
    CATEGORIES=ALLVISIBLE MERGE=NO.