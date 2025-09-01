library(dplyr)
analysis_dat = readRDS("Data/analysis.rds")
analysis_dat = droplevels(analysis_dat)

## Total---
dim(analysis_dat) #369 #44
length(unique(analysis_dat$pub_ID)) #64

###Species--------
analysis_dat$sp_name =  as.factor(analysis_dat$sp_name)
spp <- levels(analysis_dat$sp_name)

n.studies= data.frame(
  species = spp,
  n.stud = rep(NA,length(spp))
)

# Loop to count the number of unique studies for each age group
i=1
for(p in spp){
  sub = analysis_dat$pub_ID[analysis_dat$sp_name == p]
  n.studies$n.stud[i] = length(unique(sub))
  i = i+1
} 

n.studies
# species n.stud
# 1             Daphnia carinata      2
# 2 Daphnia galeata x longispina      1
# 3                Daphnia magna     56
# 4                Daphnia pulex      6

# number of data points:
analysis_dat %>% group_by(sp_name) %>% summarize(n = n())

# sp_name                          n
# 1 Daphnia carinata                 4
# 2 Daphnia galeata x longispina     2
# 3 Daphnia magna                  331
# 4 Daphnia pulex                   32

####Age-----
analysis_dat$age_d = as.factor(analysis_dat$age_d)
age <- levels(analysis_dat$age_d)
age

# Create the dataframe to store results, plus one row for NA
n.studies = data.frame(
  age = c(age, "NA"),
  n.stud = rep(NA, length(age) + 1)
)

# Loop to count the number of unique studies for each age group 
i = 1
for(p in age){
  sub = analysis_dat$pub_ID[analysis_dat$age_d == p]
  n.studies$n.stud[i] = length(unique(sub))
  i = i + 1
}

# Count NAs
sub_na <- analysis_dat$pub_ID[is.na(analysis_dat$age_d)]
n.studies$n.stud[i] <- length(unique(sub_na))

n.studies
# age n.stud
# 1    0     53
# 2    1      1
# 3    2      2
# 4  2.5      1
# 5  3.5      1
# 6    4      2
# 7    5      3
# 8    6      1
# 9    7      1
# 10  18      2
# 11  NA      0


# number of data points:
analysis_dat %>% group_by(age_d) %>% summarize(n = n())

# age_d     n
# 1 0       324
# 2 1         2
# 3 2         5
# 4 2.5       3
# 5 3.5       9
# 6 4         5
# 7 5         9
# 8 6         1
# 9 7         3
# 10 18        8

####Exposure time-----
analysis_dat$exp_time_d = as.factor(analysis_dat$exp_time_d)
exp <- levels(analysis_dat$exp_time_d)
exp

n.studies= data.frame(
  exp_time = c(exp,NA),
  n.stud = rep(NA,length(exp) + 1)
)

# Loop to count the number of unique studies for each age group
i=1
for(p in exp){
  sub = analysis_dat$pub_ID[analysis_dat$exp_time_d == p]
  n.studies$n.stud[i] = length(unique(sub))
  i = i+1
} 

# Count NAs
sub_na <- analysis_dat$pub_ID[is.na(analysis_dat$exp_time_d)]
n.studies$n.stud[i] <- length(unique(sub_na))

n.studies
# exp_time n.stud
# 1         6      1
# 2         7      2
# 3         9      1
# 4        10      1
# 5        11      1
# 6        13      1
# 7        14      3
# 8        15      1
# 9        16      2
# 10       17      1
# 11       21     52
# 12       26      2
# 13       27      1
# 14       29      1
# 15       39      1
# 16     <NA>      0

# number of data points:
analysis_dat %>% group_by(exp_time_d) %>% summarize(n = n())

# exp_time_d     n
# 1 6              8
# 2 7              6
# 3 9              1
# 4 10             4
# 5 11             3
# 6 13             1
# 7 14            18
# 8 15             3
# 9 16             2
# 10 17             2
# 11 21           294
# 12 26             4
# 13 27            18
# 14 29             2
# 15 39             3

####Temperature-----
analysis_dat$temperature <- as.factor(analysis_dat$temperature)
temp <- levels(analysis_dat$temperature)
temp

# Create the dataframe to store results, plus one row for NA
n.studies <- data.frame(
  temperature = c(temp, "NA"),
  n.stud = rep(NA, length(temp) + 1)
)

# Loop to count the number of unique studies for each temperature group
i <- 1
for (p in temp) {
  sub <- analysis_dat$pub_ID[analysis_dat$temperature == p]
  n.studies$n.stud[i] <- length(unique(sub))
  i <- i + 1
}

# Count NAs
sub_na <- analysis_dat$pub_ID[is.na(analysis_dat$temperature)]
n.studies$n.stud[i] <- length(unique(sub_na))

n.studies
# temperature n.stud
# 1           15      1
# 2           18      3
# 3           19      1
# 4           20     41
# 5           21      3
# 6           22      2
# 7           23      6
# 8           24      2
# 9           25      6
# 10        26.2      1
# 11        27.5      1
# 12          28      2
# 13          30      1
# 14          NA      0



# number of data points:
analysis_dat %>% group_by(temperature) %>% summarize(n = n())

# temperature     n
# 1 15              3
# 2 18             19
# 3 19              2
# 4 20            238
# 5 21             23
# 6 22             23
# 7 23             16
# 8 24             10
# 9 25             23
# 10 26.2            1
# 11 27.5            4
# 12 28              4
# 13 30              3

###Food-------
#number of studies with food provided
length(unique(analysis_dat$pub_ID[which(analysis_dat$food_present=="yes")])) #64 > all the studies
# number of samples with food provided
nrow(analysis_dat[which(analysis_dat$food_present=="yes"),]) #369
#number of studies without food provided
length(unique(analysis_dat$pub_ID[which(analysis_dat$food_present=="no")])) #0
# number of samples without food provided
nrow(analysis_dat[which(analysis_dat$food_present=="no"),]) #0


### Polymer type-------
#number of studies per polymer type:
analysis_dat$polymer = as.factor(analysis_dat$polymer)
poly <- levels(analysis_dat$polymer)
poly
n.studies= data.frame(
  polymer = c(poly,NA),
  n.stud = rep(NA,length(poly) + 1)
)

# Loop to count the number of unique studies for each age group
i=1
for(p in poly){
  sub = analysis_dat$pub_ID[analysis_dat$polymer == p]
  n.studies$n.stud[i] = length(unique(sub))
  i = i+1
} 

# Count NAs
sub_na <- analysis_dat$pub_ID[is.na(analysis_dat$polymer)]
n.studies$n.stud[i] <- length(unique(sub_na))

n.studies
# polymer n.stud
# 1     others      5
# 2         PA      1
# 3        PCL      1
# 4         PE     14
# 5        PES      2
# 6        PET      2
# 7        PLA      2
# 8         PS     34
# 9        PUR      1
# 10       PVC      4
# 11      LDPE      1
# 12 Thermoset      5
# 13        TW      2
# 14      <NA>      0


# number of polymer types (data points):
analysis_dat %>% group_by(polymer) %>% summarize(n = n())

# polymer       n
# 1 others       13
# 2 PA            8
# 3 PCL           5
# 4 PE           36
# 5 PES          10
# 6 PET           4
# 7 PLA           6
# 8 PS          231
# 9 PUR           4
# 10 PVC           9
# 11 LDPE          6
# 12 Thermoset    24
# 13 TW           13

### Shape-----
#number of studies for each shape
analysis_dat$shape = as.factor(analysis_dat$shape)
length(unique(analysis_dat$pub_ID[which(analysis_dat$shape=="fiber")])) #4
length(unique(analysis_dat$pub_ID[which(analysis_dat$shape=="fragment")])) #22
length(unique(analysis_dat$pub_ID[which(analysis_dat$shape=="spherical")])) #45

# number of samples for each shape
nrow(analysis_dat[which(analysis_dat$shape=="fiber"),]) #14
nrow(analysis_dat[which(analysis_dat$shape=="fragment"),]) #124
nrow(analysis_dat[which(analysis_dat$shape=="spherical"),]) #231

###Size----------
analysis_dat$MP.NP = as.factor(analysis_dat$MP.NP)

#number of studies with MP vs. NP
length(unique(analysis_dat$pub_ID[which(analysis_dat$MP.NP=="Microplastic")]))#54
length(unique(analysis_dat$pub_ID[which(analysis_dat$MP.NP=="Nanoplastic")]))#15
# number of samples with MP vs. NP
nrow(analysis_dat[which(analysis_dat$MP.NP=="Microplastic"),]) #299
nrow(analysis_dat[which(analysis_dat$MP.NP=="Nanoplastic"),]) #70


### Controlled surface and other modifications---------
#number of studies with modifications
length(unique(analysis_dat$pub_ID[which(analysis_dat$modified=="yes")])) #16
length(unique(analysis_dat$pub_ID[which(analysis_dat$modified=="no")])) #57
# number of samples with modified MNP
nrow(analysis_dat[which(analysis_dat$modified=="yes"),]) #73
nrow(analysis_dat[which(analysis_dat$modified=="no"),]) #296

# number of data points for  different modification types:
analysis_dat %>% group_by(mod_type) %>% summarize(n = n())

# mod_type            n
# 1 Aluminium oxide     4
# 2 Aminated            3
# 3 BP-3                4
# 4 Carboxylated       52
# 5 DiNP                1
# 6 Recycled            3
# 7 UV-weathered        6
# 8 NA                296

# number of studies per modification type:
# fluorescence tags:
length(unique(analysis_dat$pub_ID[which(analysis_dat$fluorescence=="yes")])) #23 studies
nrow(analysis_dat[which(analysis_dat$fluorescence=="yes"),]) #123 samples

# controlled surface modifications: number of studies:
length(unique(analysis_dat$pub_ID[which(analysis_dat$mod_type=="Aluminium oxide")])) #1
length(unique(analysis_dat$pub_ID[which(analysis_dat$mod_type=="Aminated")])) #1
length(unique(analysis_dat$pub_ID[which(analysis_dat$mod_type=="BP-3")])) #3
length(unique(analysis_dat$pub_ID[which(analysis_dat$mod_type=="Carboxylated")])) #7
length(unique(analysis_dat$pub_ID[which(analysis_dat$mod_type=="DiNP")])) #1
length(unique(analysis_dat$pub_ID[which(analysis_dat$mod_type=="Recycled")])) #1
length(unique(analysis_dat$pub_ID[which(analysis_dat$mod_type=="UV-weathered")])) #3


# milling
nrow(analysis_dat[which(analysis_dat$milled=="yes"),]) #96 data points
length(unique(analysis_dat$pub_ID[which(analysis_dat$milled=="yes")])) #18 studies


### END-------------------