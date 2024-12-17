################ data analysis
### including code for all the tables and figure 2A, 2B
library(ggplot2)
library(dplyr)
library(MASS)
library(rcompanion) 


long <- read.csv("data/watch_G22_G32.csv",header=T) 
short <- long %>% tidyr::pivot_wider(names_from = time, 
                                     values_from = start_time_av:wake_bouts_v,
                                     names_sep = ".") %>% as.data.frame()
bio <- read.csv("data/bio.csv", header = T) 
short_bio <- short %>% right_join(bio, by = "subject") %>% arrange(subject)

################# table 1
## all subjects with G22 information: same as from long data
sub_G22 <- sapply(readxl::excel_sheets("data/G22_Exports_Edit.xls"), function(xx) strsplit(xx, split = "_")[[1]][1])
## all subjects with G32 information: 2 more than from long(2 subjects with measure < 3 days)
sub_G32 <- sapply(readxl::excel_sheets("data/G32_Exports_Edit.xls"), function(xx) strsplit(xx, split = "_")[[1]][1])
length(sub_G22)
length(sub_G32)
length(intersect(sub_G32, sub_G22))
sub_common <- intersect(sub_G32, sub_G22)

bio_demo <- bio %>% filter(subject %in% sub_common)
tab <- tableone::CreateTableOne(data = bio_demo %>% select(Race, Ethnicity, highest_level_of_education, yearly_household_income,
                                                           BMI_index, pre_eclamsia, hypertension, Disease, gestational_diabetes,
                                                           glucose_tolerance_test))
summary(tab)
table(bio_demo$pre_eclamsia)
table(bio_demo[, "glucose_tolerance_test"] >= 130)
table(bio_demo$BMI_prepregnancy > 25)
table(bio_demo$Disease)

## function to calculate wilcox test
wilcox.test_z = function(a,b, paired = T){
  if(paired == T){
    tmp = cbind(a,b)
    wildata = tmp[complete.cases(tmp),]
    data = cbind(as.vector(wildata),rep(1:2,each = dim(wildata)[1]))
    return(wilcoxonPairedR(x = data[,1], g = data[,2]))
  }else{
    return(wilcoxonR(x = a, g = b))
  }
}

################### Table 2
############ sleep variable change from G22 to G32 with wilcoxon rank test
sleep_variables=c("start_time","end_time","sleep_time","sleep_eff","WASO","frag")
ps = matrix(0, nrow = length(sleep_variables), ncol = 2)
colnames(ps) = c("r","p")
rownames(ps) = sleep_variables
for(i in 1:length(sleep_variables)){
  index_names = c(paste(sleep_variables[i],"_av.G22",sep=''),paste(sleep_variables[i],"_av.G32",sep=''))
  ps[i,1] = wilcox.test_z(short[,index_names[1]], short[,index_names[2]],paired = T)
  ps[i,2] = wilcox.test(x = short[,index_names[1]], y = short[,index_names[2]],paired = T)$p.value
}
print("sleep variable G22 G32 comparison")
ps <- ps %>% as.data.frame() %>% tibble::rownames_to_column("var")
ps

########### sleep variable summary statistics
sleep_variables=c("start_time","end_time","sleep_time","sleep_eff","WASO","frag")
dat_sum <- vector("list", length(sleep_variables))
for(jj in 1:length(sleep_variables)){
  index_names = c(paste(sleep_variables[jj],"_av",sep=''))
  dat_sum[[jj]] = long %>%
    subset(!is.na(eval(parse(text = index_names))) & (time == "G22" | time == "G32")) %>%
    group_by(time)%>%
    summarize(n = length(eval(parse(text = index_names))),
              mean = mean((eval(parse(text = index_names)))),
              sd = sd(eval(parse(text = index_names))),
              median = median(eval(parse(text = index_names))),
              max = max(eval(parse(text = index_names))),
              min = min(eval(parse(text = index_names))),
              var = sleep_variables[jj])
}
dat_sum %>% bind_rows() 
dat_sum %>% bind_rows() %>% mutate(mean_sd = sprintf("%2.1f(%2.1f)", mean, sd)) %>%
  select(var, mean_sd, time) %>%
  tidyr::pivot_wider(names_from = time, values_from = mean_sd) %>%
  left_join(ps %>% as.data.frame(), by = "var")


#####################################################
################## Table 3:  blood glucose level versus sleep variables
sleep_variables_time <- c(paste0(sleep_variables, "_av.G22"),
                          paste0(sleep_variables, "_av.G32"))

cor_cal = function(i){
  test = cor.test(short_bio[,sleep_variables_time[i]],short_bio$glucose_tolerance_test)
  return(c(test$estimate, test$p.value))
}
correlation = sapply(1:length(sleep_variables_time),cor_cal)
colnames(correlation) = sleep_variables_time
rownames(correlation) = c("r","p")
correlation = t(round(correlation,digits=4))
correlation %>% as.data.frame() %>% tibble::rownames_to_column("var") %>%
  tidyr::separate(var, into = c("var", "time"), sep = "[.]") %>%
  tidyr::pivot_wider(names_from = time, values_from = c(r, p)) 

##################### Table 4
#################### variable(binary) and sleep variables p values
short_disease = short_bio %>% filter(!is.na(Disease))
var_test = "short_disease$Disease"
ps = matrix(0, nrow = length(sleep_variables), ncol = 4)
temp = c("average_G22","average_G32")
colnames(ps) = sapply(temp,function(x) paste0(x, c("_r", "_p")))
rownames(ps) = sleep_variables
for(i in 1:length(sleep_variables)){
  index_names = paste0(sleep_variables[i],"_av.", c("G22", "G32"))
  ps[i,2] = wilcox.test(short_disease[,index_names[1]]~eval(parse(text = var_test)))$p.value
  ps[i,4] = wilcox.test(short_disease[,index_names[2]]~eval(parse(text = var_test)))$p.value
  ps[i,1] = wilcox.test_z(short_disease[,index_names[1]],eval(parse(text = var_test)),paired = F)
  ps[i,3] = wilcox.test_z(short_disease[,index_names[2]],eval(parse(text = var_test)),paired = F)
}
ps <- as.data.frame(ps) %>% mutate(var = sleep_variables)
## get average on each group
all_vars <- as.vector(sapply(sleep_variables, function(xx) paste0(xx, "_av.", c("G22", "G32"))))
short_disease %>% select(all_of(c(all_vars, "Disease", "subject"))) %>%
  tidyr::pivot_longer(all_of(all_vars),
                      names_to = "var", values_to = "value") %>%
  tidyr::separate(var, into = c("var", "time"), sep = "[.]") %>% 
  group_by(var, Disease, time) %>% 
  summarise( n = sum(!is.na(value)), 
             mean = mean(value, na.rm = T),
             sd = sd(value, na.rm = T),
             mean_sd = sprintf("%2.2f(%.2f)", mean, sd)) %>% ungroup() %>%
  select(var, Disease, time, n, mean_sd) %>%
  tidyr::pivot_wider(names_from = c(time, Disease), values_from = c(n, mean_sd)) %>%
  left_join(ps %>% mutate(var = paste0(var, "_av")), by = "var") 

#################### table 5
######## delay 
short_delay = short_bio %>% subset(delay == 0 | delay == 1)
var_test = "short_delay$delay"
ps = matrix(0, nrow = length(sleep_variables), ncol = 8)
temp = c("av_G22","av_G32","v_G22","v_G32")
colnames(ps)=c(sapply(temp,function(x) c(paste(x,"_r",sep = ""),paste(x,"_p",sep=""))))
rownames(ps) = sleep_variables
for(i in 1:length(sleep_variables)){
  index_names = c(paste(sleep_variables[i],"_av.G22",sep=''),paste(sleep_variables[i],"_av.G32",sep=''),
                  paste(sleep_variables[i],"_v.G22",sep=''),paste(sleep_variables[i],"_v.G32",sep=''))
  ps[i,2] = wilcox.test(short_delay[,index_names[1]]~eval(parse(text = var_test)))$p.value
  ps[i,4] = wilcox.test(short_delay[,index_names[2]]~eval(parse(text = var_test)))$p.value
  ps[i,6] = wilcox.test(short_delay[,index_names[3]]~eval(parse(text = var_test)))$p.value 
  ps[i,8] = wilcox.test(short_delay[,index_names[4]]~eval(parse(text = var_test)))$p.value
  ps[i,1] = wilcox.test_z(short_delay[,index_names[1]],eval(parse(text = var_test)),paired = F)
  ps[i,3] = wilcox.test_z(short_delay[,index_names[2]],eval(parse(text = var_test)),paired = F)
  ps[i,5] = wilcox.test_z(short_delay[,index_names[3]],eval(parse(text = var_test)),paired = F)
  ps[i,7] = wilcox.test_z(short_delay[,index_names[4]],eval(parse(text = var_test)),paired = F)
}
ps

##### mean(sd) for each group
all_vars <- as.vector(sapply(sleep_variables, function(xx) 
  paste0(xx, c("_av.G22", "_av.G32", "_v.G22", "_v.G32"))))
short_delay %>% select(all_of(c(all_vars, "delay", "subject"))) %>%
  tidyr::pivot_longer(all_of(all_vars),
                      names_to = "var", values_to = "value") %>%
  tidyr::separate(var, into = c("var", "time"), sep = "[.]") %>% 
  group_by(var, delay, time) %>% 
  summarise( n = sum(!is.na(value)), 
             mean = mean(value, na.rm = T),
             sd = sd(value, na.rm = T),
             mean_sd = sprintf("%2.2f(%.2f)", mean, sd)) %>% 
  select(var, delay, time, n, mean_sd) %>%
  tidyr::pivot_wider(names_from = c(time, delay), values_from = c(n, mean_sd)) 


## Figure 2A and 2B
short_delay %>% 
  mutate(delay = case_when(delay == 0 ~ "no DLII",
                           delay == 1 ~ "DLII"),
         delay = factor(delay, levels = c("no DLII", "DLII"))) %>%
  group_by(delay) %>%
  summarise(
    mean = mean(WASO_av.G32, na.rm = T),
    sd = sd(WASO_av.G32, na.rm = T)) %>%
  ggplot(aes(delay, mean)) + 
  geom_bar(stat = "identity", fill = "white", color = "black", width = 0.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  labs(title = "Wake after sleep onset\n(WASO)", x = "", y = "min") +
  theme_bw()

## figure 2B
short_delay %>% 
  mutate(delay = case_when(delay == 0 ~ "no DLII",
                           delay == 1 ~ "DLII"),
         delay = factor(delay, levels = c("no DLII", "DLII"))) %>%
  group_by(delay) %>%
  summarise(
    mean = mean(sleep_time_v.G32, na.rm = T),
    sd = sd(sleep_time_v.G32, na.rm = T)) %>%
  ggplot(aes(delay, mean)) + 
  geom_bar(stat = "identity", fill = "white", color = "black", width = 0.5) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  labs(title = "Night-to-night variation in total sleep time", x = "", y = "min") +
  theme_bw()

  
### figure 1
sheets_name <- readxl::excel_sheets("data/Breast_fullness_PHI_data.xlsx")
dat_full <- readxl::read_excel("data/Breast_fullness_PHI_data.xlsx", sheet = "all_data_updated", col_names = T) 
dat_full %>% as.data.frame() %>% select(subject = `study_id...1`, PPD1, PPD2, PPD3, PPD4, PPD5) %>%
  tidyr::pivot_longer(PPD1:PPD5, names_to = "days", values_to = "score") %>%
  mutate(score = case_when(score %in% c(1, 2, 3, 4, 5) ~ score,
                           TRUE ~ NA)) %>%
  drop_na(score) %>%
  group_by(days, score) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  ggplot(aes(x = days, y = proportion, fill = factor(score))) +
  geom_bar(stat = "identity") +
  labs(title = "Paticipant reports on the change in breast fullness score\n on postpartum(PP) days 1-5", 
       x = " ", y = " ", fill = "Score") +
  theme_bw()

