# Workbook 6: analyze NHANES data

# Set up
library(survey)
library(Hmisc)

demo <- sasxport.get('./data/DEMO_I.XPT')
alco <- sasxport.get('./data/ALQ_I.XPT')

# join data set
nhanes <- merge(x = demo, y = alco, all = TRUE, by = 'seqn')

### sample weight ###
# interpretation
# wt_sum: the sample weight represents the overall US population
wt_sum <- sum(nhanes$wtint2yr, na.rm = TRUE)

### analysis ###
# assign 2 as 0
nhanes$alq151[nhanes$alq151 == 2] <- 0

# ignore 7 and 9
nhanes$alq151[nhanes$alq151 == 7] <- NA
nhanes$alq151[nhanes$alq151 == 9] <- NA

# create survey design
nhanes_survey <- svydesign(
  data = nhanes,
  id = ~sdmvpsu,
  strata = ~sdmvstra,
  weights = ~wtint2yr,
  nest = TRUE
)

nhanes_mean <- svymean(~alq151, design = nhanes_survey,
                       na.rm = TRUE)
mean_by_gender <- svyby(~alq151, ~riagendr, design = nhanes_survey,
                        FUN = svymean, na.rm = TRUE)
