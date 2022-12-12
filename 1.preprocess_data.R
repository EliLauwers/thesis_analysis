# Read data
# TITLE: pizza experiment cheap vs not expensive on intent via qual & price;
# DATA: 
# file =  '4_data.dat';
dpath = "data/input/4_data.dat"
data = read.table(dpath)
# VARIABLE:
#   names = id compflag freq ref target qual intent d_cheap;
# !ref = reference price, intent = intention, 
# !target = expected price, qual = expected quality
# !d_cheap = cheap vs. not expensive;
nms = c("id", "compflag", "freq", "ref", "target", "qual", "intent", "d_cheap")
names(data) = nms
# idvar = id;
# missing = all(-99);
data[data==-99] = NA
# useobs = (compflag EQ 1);
data = data[data$compflag == 1,]
# usevar = intent qual d_cheap target ref;
data = data[,c("intent","qual","d_cheap","target","ref")]
# ANALYSIS: 
#   type is general;
# BOOTSTRAP = 1000;
# 
# OUTPUT: CINTERVAL(bootstrap);
# PLOT: type is plot2;
# 
# DEFINE:
#   STANDARDIZE (intent qual target);
to_standard = c("intent","qual","target")
data[to_standard] = lapply(data[to_standard], function(x) as.numeric(scale(x)))


rm(dpath, nms, to_standard)
