#install.packages("Boruta")
#install.packages("randomForest")
#install.packages("caTools")
library(tidyverse)
library(tidymodels)
library(stringr)
library(lubridate)
library(randomForest)
require(caTools)                    



options(dplyr.width = Inf) #show all columns when pringting to console
set.seed(2022) #setting the seed for the AC challenge to make our work reproducible
options(digits=20) #print more digits

trnsc_df = as_tibble(read_csv("transactions.csv"))

print("[INFO] Number of NA values in each column:")
for (i in 1:ncol(trnsc_df)) {
  print(paste0(names(trnsc_df)[i], ": ", sum(is.na(trnsc_df[, i])), "/", nrow(trnsc_df)))
}

trnsc_df$CUSTOMER = (substring(trnsc_df$CUSTOMER, 2, nchar(trnsc_df$CUSTOMER)-1))

geo_df = as_tibble(read_csv("geo.csv"))

print("[INFO] Number of NA values in each column:")
for (i in 1:ncol(geo_df)) {
  print(paste0(names(geo_df)[i], ": ", sum(is.na(geo_df[, i])), "/", nrow(geo_df)))
}
# Rename column COUNTRY COUNTRY_CODE, since it only contains codes like CH, FR
geo_df = rename(geo_df, COUNTRY_CODE = COUNTRY)

# Perform left join using dplyr
trnsc_geo_df = left_join(trnsc_df, geo_df, by = 'SALES_LOCATION')

cst_df = as_tibble(read_csv("customers.csv"))

print("[INFO] Number of NA values in each column:")
for (i in 1:ncol(cst_df)) {
  print(paste0(names(cst_df)[i], ": ", sum(is.na(cst_df[, i])), "/", nrow(cst_df)))
}

#cst_df = cst_df %>% mutate(REV_CURRENT_YEAR.1 = ifelse(REV_CURRENT_YEAR.1 == 0,REV_CURRENT_YEAR.2, REV_CURRENT_YEAR.1))
#cst_df = cst_df %>% mutate(PREV_YEAR_PERCENTAGE_INCREASE.1 = ((REV_CURRENT_YEAR.1 - REV_CURRENT_YEAR.2)/REV_CURRENT_YEAR.2)*100)
#cst_df = cst_df %>% mutate(PREV_YEAR_PERCENTAGE_INCREASE.1 = ifelse(is.infinite(PREV_YEAR_PERCENTAGE_INCREASE.1),0, PREV_YEAR_PERCENTAGE_INCREASE.1))

# Change data type of CUSTOMER column in customers dataset
cst_df$CUSTOMER <- as.character(cst_df$CUSTOMER)

# Create IDX_CUSTOMER for trnsc_geo_df
trnsc_geo_df = mutate(trnsc_geo_df,
                      IDX_CUSTOMER = paste0(COUNTRY_CODE, "_", CUSTOMER))

# Create IDX_CUSTOMER for customers
cst_df = cst_df %>% mutate(COUNTRY_CODE = case_when(
  COUNTRY == 'Switzerland' ~ "CH",
  COUNTRY == 'France' ~ "FR"
))

cst_df = mutate(cst_df, IDX_CUSTOMER = paste0(COUNTRY_CODE, "_", CUSTOMER))

# Perform left join using dplyr
all_merged = left_join(trnsc_geo_df, cst_df, by = 'IDX_CUSTOMER')



# Feature Elimination & Fix
all_merged = select(all_merged, -c(COUNTRY_CODE.y, CUSTOMER.y, COUNTRY, MO_ID, SO_ID))

# Feature Renaming
all_merged = rename(all_merged, COUNTRY_CODE = COUNTRY_CODE.x)
all_merged = rename(all_merged, CUSTOMER = CUSTOMER.x)

merged_df = all_merged

df = data.frame(merged_df)
df = select (df,-c(MO_CREATED_DATE,SALES_BRANCH,REV_CURRENT_YEAR))

df = df %>%
  mutate(
    OFFER_STATUS = case_when(
      OFFER_STATUS %in% c("WIN","Win","Won","WON") ~ 1,
      OFFER_STATUS %in% c("LOsT","Lost","LOST","Lose") ~ 0,
    )
  )

# Manipulate "END_CUSTOMER" Feature: Convert to HAS_END_CUSTOMER
df = df %>%
  mutate(
    HAS_END_CUSTOMER = case_when(
      END_CUSTOMER %in% c(NA,"No") ~ 0,
      TRUE ~ 1 # Includes numbers and "Yes" values
      
    )
  )

# Delete column: END_CUSTOMER
df = select (df,-c(END_CUSTOMER))


# Manipulate "ISIC" Feature: Convert to HAS_ISIC
df = df %>%
  mutate(
    HAS_ISIC = case_when(
      ISIC %in% c(NA) ~ 0,
      TRUE ~ 1 # Includes numbers
    )
  )

# Delete column: ISIC
df = select (df,-c(ISIC))

# Manipulate "COUNTRY_CODE" Feature: Convert to IS_COUNTRY_CODE_CH
df = df %>%
  mutate(
    IS_COUNTRY_CODE_CH = case_when(
      COUNTRY_CODE %in% c("CH") ~ 1,
      COUNTRY_CODE %in% c("FR") ~ 0,
    )
  )

# Delete column: COUNTRY_CODE
df = select (df,-c(COUNTRY_CODE))

# Create "TOTAL_COSTS_PRODUCT" Feature: Sum of "COSTS_PRODUCT_*"
df = df %>%
  mutate(
    TOTAL_COSTS_PRODUCT = COSTS_PRODUCT_A
    + COSTS_PRODUCT_B + 
      COSTS_PRODUCT_C + COSTS_PRODUCT_D + COSTS_PRODUCT_E
  )

# Delete columns: COSTS_PRODUCT_A to COSTS_PRODUCT_E
df = select (df,-c(COSTS_PRODUCT_A,COSTS_PRODUCT_B,
                   COSTS_PRODUCT_C,COSTS_PRODUCT_D,
                   COSTS_PRODUCT_E))

# Manipulate "CREATION_YEAR" Feature: Extract year
df = df %>%
  mutate(CREATION_YEAR = case_when(
    is.character(CREATION_YEAR) ~ CREATION_YEAR %>%
      substr(nchar(CREATION_YEAR) - 3, nchar(CREATION_YEAR)) %>%
      as.numeric()
  ))

# Create "SINCE_CREATION_YEAR" Feature: 2021 - CREATION_YEAR
df = df %>%
  mutate(SINCE_CREATION_YEAR = case_when(!is.na(CREATION_YEAR) ~ as.double(2021 - CREATION_YEAR)))

# Convert negative values to zero
df$REV_CURRENT_YEAR.1 = ifelse(df$REV_CURRENT_YEAR.1 < 0,0,df$REV_CURRENT_YEAR.1)
df$REV_CURRENT_YEAR.2 = ifelse(df$REV_CURRENT_YEAR.2 < 0,0,df$REV_CURRENT_YEAR.2)

df = df %>% mutate( # If REV_CURRENT_YEAR.1 is 0 or null, fill it from REV_CURRENT_YEAR.2
  REV_CURRENT_YEAR.1 = ifelse(
    ((REV_CURRENT_YEAR.1 <= 0) |
       is.na(REV_CURRENT_YEAR.1)) &
      !is.na(REV_CURRENT_YEAR.2),
    REV_CURRENT_YEAR.2,
    REV_CURRENT_YEAR.1
  ) %>% as.double()
)

df = df %>% mutate( # If REV_CURRENT_YEAR.2 is 0 or null, fill it from REV_CURRENT_YEAR.1
  REV_CURRENT_YEAR.2 = ifelse(
    ((REV_CURRENT_YEAR.2 <= 0) |
       is.na(REV_CURRENT_YEAR.2)) &
      !is.na(REV_CURRENT_YEAR.1),
    REV_CURRENT_YEAR.1,
    REV_CURRENT_YEAR.2
  ) %>% as.double()
)

# Create "REV_PERCENTAGE_INCREASE" Feature: REV_CURRENT_YEAR.2 to REV_CURRENT_YEAR.1
calculate_percentage_increase = function(new, old) {
  100 * (new - old) / old
}
df = df %>%
  mutate(REV_PERCENTAGE_INCREASE = case_when(
    !is.na(REV_CURRENT_YEAR.1) & !is.na(REV_CURRENT_YEAR.2) ~
      calculate_percentage_increase(REV_CURRENT_YEAR.1, REV_CURRENT_YEAR.2)
  ))

# Note: 2020 and 2021 annual average exchange rates are used.
# Source: https://www.x-rates.com/average/?from=USD&to=EUR&amount=1&year=2021
df = df %>%
  mutate(
    REV_CURRENT_YEAR.1 = case_when(
      CURRENCY ==  "Pound Sterling" ~ REV_CURRENT_YEAR.1 * 1.1438161149110808,
      CURRENCY ==  "Chinese Yuan" ~ REV_CURRENT_YEAR.1 * 0.12906362243502054,
      CURRENCY ==  "US Dollar" ~ REV_CURRENT_YEAR.1 * 0.8614249616963066,
      CURRENCY ==  "Euro" ~ REV_CURRENT_YEAR.1
    )
  )


df = df %>%
  mutate(
    REV_CURRENT_YEAR.2 = case_when(
      CURRENCY ==  "Pound Sterling" ~ REV_CURRENT_YEAR.2 * 1.1438161149110808,
      CURRENCY ==  "Chinese Yuan" ~ REV_CURRENT_YEAR.2 * 0.12906362243502054,
      CURRENCY ==  "US Dollar" ~ REV_CURRENT_YEAR.2 * 0.8614249616963066,
      CURRENCY ==  "Euro" ~ REV_CURRENT_YEAR.1
    )
  )

# Create "OWNERSHIP_NO_INFO_AS_NA" Feature: Treat "No information" as NA value
df$OWNERSHIP_NO_INFO_AS_NA = ifelse(df$OWNERSHIP ==  "No information", NA, df$OWNERSHIP)

# Create "OWNERSHIP_NA_AS_NO_INFO" Feature: Treat NA values as "No information"
df$OWNERSHIP_NA_AS_NO_INFO = ifelse(is.na(df$OWNERSHIP), "No information", df$OWNERSHIP)


# Create "SO_CREATED_DATE_SCALED" Feature: Scale "SO_CREATED_DATE" x 100


df$SO_CREATED_DATE_SCALED = as_datetime(df$SO_CREATED_DATE, format = "%d.%m.%Y %H:%M") # Parse date-format 1
date_format_2 = as_datetime(df$SO_CREATED_DATE, format = "%Y-%m-%d %H:%M:%S") # Parse date-format 1
df$SO_CREATED_DATE_SCALED[is.na(df$SO_CREATED_DATE_SCALED)] = date_format_2[!is.na(date_format_2)]

df$SO_CREATED_DATE_SCALED = as.numeric(as.POSIXct(df$SO_CREATED_DATE_SCALED))# Convert to Unix Time Stamp
standart_scale_x100 = function (x) 100*(x - mean(x, na.rm = T)) / sd(x, na.rm = T)

df$SO_CREATED_DATE_SCALED = standart_scale_x100(df$SO_CREATED_DATE_SCALED ) # Scale x 100

# Delete column: "SO_CREATED_DATE"
df = select (df,-c(SO_CREATED_DATE))

df$TOTAL_COSTS_PRODUCT = ifelse(df$TOTAL_COSTS_PRODUCT < 0,-df$TOTAL_COSTS_PRODUCT,df$TOTAL_COSTS_PRODUCT)
df$TOTAL_COSTS_PRODUCT_LOG=log(df$TOTAL_COSTS_PRODUCT+1)

df$SERVICE_COST = ifelse(df$SERVICE_COST < 0,-df$SERVICE_COST,df$SERVICE_COST)
df$SERVICE_COST_LOG=log(df$SERVICE_COST+1)

df$OFFER_PRICE_LOG=log(df$OFFER_PRICE)

df$SERVICE_LIST_PRICE_LOG=log(df$SERVICE_LIST_PRICE+1)

df$MATERIAL_COST_LOG=log(df$MATERIAL_COST+1)

df$REV_CURRENT_YEAR_LOG.1=log(df$REV_CURRENT_YEAR.1+1)

df$REV_CURRENT_YEAR_LOG.2=log(df$REV_CURRENT_YEAR.2+1)

df$CREATION_YEAR_LOG=log(df$CREATION_YEAR)

df$SINCE_CREATION_YEAR_LOG=log(df$SINCE_CREATION_YEAR+1)

#For REV_PERCENTAGE_INCREASE
Q1 <- quantile(df$REV_PERCENTAGE_INCREASE, .25,na.rm=T)
Q3 <- quantile(df$REV_PERCENTAGE_INCREASE, .75,na.rm=T)
IQR <- IQR(df$REV_PERCENTAGE_INCREASE,na.rm=T)
df = df %>%  mutate(
  REV_PERCENTAGE_INCREASE_NO_OUTLIER = case_when(
    REV_PERCENTAGE_INCREASE < (Q1 - 3.0*IQR) ~ (Q1 - 3.0*IQR),
    REV_PERCENTAGE_INCREASE > (Q3 + 3.0*IQR) ~ (Q3 + 3.0*IQR),
    TRUE ~ REV_PERCENTAGE_INCREASE
  )
)
df$TECH_REDUCED_1 = ifelse(df$TECH %in% c("E", "EPS", "FP", "BP"), "E_EPS_FP_BP", df$TECH)
df$TECH_REDUCED_2_IS_F = ifelse(df$TECH == "F", 1, 0)

df$OFFER_TYPE_REDUCED_1 = ifelse(
  df$OFFER_TYPE %in% c(
    "FD",
    "EH",
    "FEI",
    "MSYS",
    "DCF",
    "GAM",
    "CP",
    "CS",
    "CI",
    "EN",
    "FIB",
    "PAT",
    "XCPS"
  ),
  "FD_EH_FEI_MSYS_DCF_GAM_CP_CS_CI_EN_FIB_PAT_XCPS",
  df$OFFER_TYPE
)
df$OFFER_TYPE_REDUCED_2 = ifelse(
  df$OFFER_TYPE %in% c(
    "FED",
    "CPP",
    "ED",
    "EV",
    "FD",
    "EH",
    "FEI",
    "MSYS",
    "DCF",
    "GAM",
    "CP",
    "CS",
    "CI",
    "EN",
    "FIB",
    "PAT",
    "XCPS"
  ),
  "FED_CPP_ED_EV_FD_EH_FEI_MSYS_DCF_GAM_CP_CS_CI_EN_FIB_PAT_XCPS",
  df$OFFER_TYPE
)

df$OWNERSHIP_NA_AS_NO_INFO_REDUCED = ifelse(
  df$OWNERSHIP_NA_AS_NO_INFO %in% c("Governmental", "Individual Person", "No information"),
  "Governmental_IndividualPerson_Noinformation",
  df$OWNERSHIP_NA_AS_NO_INFO
)

df$OWNERSHIP_NO_INFO_AS_NA_REDUCED = ifelse(
  df$OWNERSHIP_NO_INFO_AS_NA %in% c("Governmental", "Individual Person"),
  "Governmental_IndividualPerson",
  df$OWNERSHIP_NO_INFO_AS_NA
)

df$OWNERSHIP_REDUCED = ifelse(
  df$OWNERSHIP %in% c("Governmental", "Individual Person", "No information"),
  "Governmental_IndividualPerson_Noinformation",
  df$OWNERSHIP
)

df$SALES_OFFICE_REDUCED = ifelse(
  df$SALES_OFFICE %in% c(
    "Montpellier",
    "Monaco",
    "Limoges",
    "Vertical Market",
    "Others Functions"
  ),
  "Montpellier_Monaco_Limoges_Vertical Market_OthersFunctions",
  df$SALES_OFFICE
)


# Replace with mean

df$IS_NA_REV_CURRENT_YEAR.1 = ifelse(is.na(df$REV_CURRENT_YEAR.1) == T,1,0)
df$REV_CURRENT_YEAR.1[is.na(df$REV_CURRENT_YEAR.1)] = mean(df$REV_CURRENT_YEAR.1, na.rm=TRUE)

df$IS_NA_REV_CURRENT_YEAR.2 = ifelse(is.na(df$REV_CURRENT_YEAR.2) == T,1,0)
df$REV_CURRENT_YEAR.2[is.na(df$REV_CURRENT_YEAR.2)] = mean(df$REV_CURRENT_YEAR.2, na.rm=TRUE)

df$IS_NA_CREATION_YEAR = ifelse(is.na(df$CREATION_YEAR) == T,1,0)
df$CREATION_YEAR[is.na(df$CREATION_YEAR)] = mean(df$CREATION_YEAR, na.rm=TRUE)

df$IS_NA_SINCE_CREATION_YEAR = ifelse(is.na(df$SINCE_CREATION_YEAR) == T,1,0)
df$SINCE_CREATION_YEAR[is.na(df$SINCE_CREATION_YEAR)] = mean(df$SINCE_CREATION_YEAR, na.rm=TRUE)

df$IS_NA_REV_CURRENT_YEAR_LOG.1 = ifelse(is.na(df$REV_CURRENT_YEAR_LOG.1) == T,1,0)
df$REV_CURRENT_YEAR_LOG.1[is.na(df$REV_CURRENT_YEAR_LOG.1)] = mean(df$REV_CURRENT_YEAR_LOG.1, na.rm=TRUE)

df$IS_NA_REV_CURRENT_YEAR_LOG.2 = ifelse(is.na(df$REV_CURRENT_YEAR_LOG.2) == T,1,0)
df$REV_CURRENT_YEAR_LOG.2[is.na(df$REV_CURRENT_YEAR_LOG.2)] = mean(df$REV_CURRENT_YEAR_LOG.2, na.rm=TRUE)

df$IS_NA_CREATION_YEAR_LOG = ifelse(is.na(df$CREATION_YEAR_LOG) == T,1,0)
df$CREATION_YEAR_LOG[is.na(df$CREATION_YEAR_LOG)] = mean(df$CREATION_YEAR_LOG, na.rm=TRUE)

df$IS_NA_SINCE_CREATION_YEAR_LOG = ifelse(is.na(df$SINCE_CREATION_YEAR_LOG) == T,1,0)
df$SINCE_CREATION_YEAR_LOG[is.na(df$SINCE_CREATION_YEAR_LOG)] = mean(df$SINCE_CREATION_YEAR_LOG, na.rm=TRUE)

df$IS_NA_REV_PERCENTAGE_INCREASE_NO_OUTLIER = ifelse(is.na(df$REV_PERCENTAGE_INCREASE_NO_OUTLIER) == T,1,0)
df$REV_PERCENTAGE_INCREASE_NO_OUTLIER[is.na(df$REV_PERCENTAGE_INCREASE_NO_OUTLIER)] = mean(df$REV_PERCENTAGE_INCREASE_NO_OUTLIER, na.rm=TRUE)

df$IS_NA_REV_PERCENTAGE_INCREASE = ifelse(is.na(df$REV_PERCENTAGE_INCREASE) == T,1,0)
df$REV_PERCENTAGE_INCREASE[is.na(df$REV_PERCENTAGE_INCREASE)] = mean(df$REV_PERCENTAGE_INCREASE, na.rm=TRUE)

df$IS_NA_SALES_LOCATION = ifelse(is.na(df$SALES_LOCATION) == T,1,0)
df$SALES_LOCATION[is.na(df$SALES_LOCATION)] = "Geneva West"

df$IS_NA_SALES_OFFICE = ifelse(is.na(df$SALES_OFFICE) == T,1,0)
df$SALES_OFFICE[is.na(df$SALES_OFFICE)] = "Geneva"

df$IS_NA_SALES_OFFICE_REDUCED = ifelse(is.na(df$SALES_OFFICE_REDUCED) == T,1,0)
df$SALES_OFFICE_REDUCED[is.na(df$SALES_OFFICE_REDUCED)] = "Geneva"

df$IS_NA_CURRENCY = ifelse(is.na(df$CURRENCY) == T,1,0)
df$CURRENCY[is.na(df$CURRENCY)] = "NOT_GIVEN"

df$IS_NA_OWNERSHIP = ifelse(is.na(df$OWNERSHIP) == T,1,0)
df$OWNERSHIP[is.na(df$OWNERSHIP)] = "NOT_GIVEN"

df$IS_NA_OWNERSHIP_NO_INFO_AS_NA = ifelse(is.na(df$OWNERSHIP_NO_INFO_AS_NA) == T,1,0)
df$OWNERSHIP_NO_INFO_AS_NA[is.na(df$OWNERSHIP_NO_INFO_AS_NA)] = "NOT_GIVEN"

df$IS_NA_OWNERSHIP_NO_INFO_AS_NA_REDUCED = ifelse(is.na(df$OWNERSHIP_NO_INFO_AS_NA_REDUCED) == T,1,0)
df$OWNERSHIP_NO_INFO_AS_NA_REDUCED[is.na(df$OWNERSHIP_NO_INFO_AS_NA_REDUCED)] = "NOT_GIVEN"

df$IS_NA_OWNERSHIP_REDUCED = ifelse(is.na(df$OWNERSHIP_REDUCED) == T,1,0)
df$OWNERSHIP_REDUCED[is.na(df$OWNERSHIP_REDUCED)] = "NOT_GIVEN"

df = df %>%  select(
  -c(
    IS_NA_REV_CURRENT_YEAR.1,
    IS_NA_REV_CURRENT_YEAR.2,
    IS_NA_CREATION_YEAR,
    IS_NA_SINCE_CREATION_YEAR,
    IS_NA_REV_CURRENT_YEAR_LOG.1,
    IS_NA_REV_CURRENT_YEAR_LOG.2,
    IS_NA_CREATION_YEAR_LOG,
    IS_NA_SINCE_CREATION_YEAR_LOG,
    IS_NA_SALES_OFFICE_REDUCED,
    IS_NA_REV_PERCENTAGE_INCREASE_NO_OUTLIER,
    IS_NA_OWNERSHIP,
    IS_NA_OWNERSHIP_REDUCED,
    IS_NA_OWNERSHIP_NO_INFO_AS_NA_REDUCED
  )
)

print("[INFO] Number of NA values in each column:")
for (i in 1:ncol(df)) {
  print(paste0(names(df)[i], ": ", sum(is.na(df[, i])), "/", nrow(df)))
}


labeled_data = df[is.na(df$TEST_SET_ID),]
test_data = df[!is.na(df$TEST_SET_ID),]


unique_customers = unique(labeled_data$IDX_CUSTOMER)
#summary(trnsc_geo_df$IDX_CUSTOMER)
train_ids = sample(unique_customers, size= floor(0.8 * length(unique_customers)), replace=FALSE)

train_set = labeled_data %>% filter(IDX_CUSTOMER %in% train_ids)
validation_set = labeled_data %>%  filter(!IDX_CUSTOMER %in% train_ids)


summary(train_set)
train_set = select(train_set, -c(TEST_SET_ID, IS_NA_SALES_LOCATION, IS_NA_SALES_OFFICE,IDX_CUSTOMER, REV_CURRENT_YEAR.1, REV_CURRENT_YEAR.2, CURRENCY, CREATION_YEAR,
                                 IS_NA_REV_PERCENTAGE_INCREASE, IS_NA_CURRENCY, IS_NA_OWNERSHIP_NO_INFO_AS_NA,
                                 SERVICE_COST_LOG, OFFER_PRICE_LOG, SERVICE_LIST_PRICE_LOG, MATERIAL_COST_LOG, REV_CURRENT_YEAR_LOG.1, REV_PERCENTAGE_INCREASE_NO_OUTLIER,
                                 TECH_REDUCED_2_IS_F,   OFFER_TYPE_REDUCED_1, OFFER_TYPE_REDUCED_2, OWNERSHIP_NA_AS_NO_INFO_REDUCED, OWNERSHIP_NO_INFO_AS_NA_REDUCED))


#compute weight for negative and positive
wn = length(train_set$OFFER_STATUS[train_set$OFFER_STATUS== 0])/length(train_set$OFFER_STATUS)
wp = 1 

rf <- randomForest(
  as.factor(OFFER_STATUS) ~ .,
  data=as.matrix(train_set),
  ntree=1000,
  classwt = c("0" = wn, "1" = wp)
)

ground_truth = validation_set$OFFER_STATUS
pred = predict(rf, newdata=validation_set)
pred

#pred == ground_truth & pred == 0

TP = length(pred[pred == ground_truth & pred == 1])
FN = length(pred[pred != ground_truth & pred == 0])
TN = length(pred[pred == ground_truth & pred == 0])
FP = length(pred[pred != ground_truth & pred == 1])

sensitiv = TP/(TP + FN)
specific = TN/(FP + TN)
BAC=(sensitiv + specific)/2
BAC

#length(train_set$OFFER_STATUS[train_set$OFFER_STATUS== 1])/length(train_set$OFFER_STATUS)
#length(ground_truth[ground_truth== 1])/length(ground_truth)

#code for creating submission
#submission = test_data

#submission = submission %>% mutate(OFFER_STATUS = predict(rf, newdata=submission))
#submission = submission %>% select(c(TEST_SET_ID, OFFER_STATUS))

#get format to comply with sumbission
#submission = submission %>% rename(id = TEST_SET_ID, prediction = OFFER_STATUS)
#write.csv(submission,"", row.names = FALSE)

