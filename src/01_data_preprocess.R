library(readxl)    
library(psych)
library(dplyr)

sheets_name <- readxl::excel_sheets("data/G22_Exports_Edit.xls")
nsub=length(sheets_name)
read_one_sheet = function(filename,sheet_i){
  data = readxl::read_excel(filename, sheet = sheet_i, skip = 12,col_names = FALSE)
  data[,13]=strtoi(strsplit(sheets_name[sheet_i], "_")[[1]][1])
  ## removed value with < 3 days of information
  if(dim(data)[1]<3) data = NULL
  return(data)
}
############### edit rmssd for starting and ending time since it is recurrent
rmssd_edit = function(a){
  temp = a[1:(length(a)-1)]-a[2:length(a)]
  diff = ifelse(abs(temp)>12, 24-abs(temp),temp)
  result=sqrt(1/(length(a)-1)*sum(diff^2))
  return(result)
}

################## G22
data = lapply(1:nsub,read_one_sheet,filename="data/G22_Exports_Edit.xls")
df = do.call(rbind.data.frame, data)
names(df)=c("Interval_Type","Interval", "Start_Date","start_Time","End_Date","end_Time",
            "Onset_Latency","WASO",	"Wake_Bouts","Sleep_Time","percet_Sleep",	"Fragmentation","subject")
#head(df)
## number of patients 
nsub
## number of patients with >= 3 measure for G22
length(unique(df$subject))


################################################ use 5:00pm us baseline

base_time=ISOdate(1899,12,31,00,00,00,tz="UTC")
start_time_diff = as.numeric(df$start_Time- base_time,units ="hours")
df$start_time= ifelse(start_time_diff<0, start_time_diff+24,start_time_diff)
end_time_diff = as.numeric(df$`end_Time`- base_time,units ="hours")
df$end_time= ifelse(end_time_diff<0,end_time_diff+24,end_time_diff)
g22 = df
g22$time = "G22"
#write.csv(g22, file="G22_original_watch.csv",row.names=FALSE)
G22 = df %>%
      group_by(subject) %>%
      summarize(################## average times
              start_time_av = mean(ifelse(start_time<12, start_time + 24, start_time), na.rm = T),
              end_time_av = mean(ifelse(end_time<12, end_time + 24, end_time), na.rm = T),
              onsite_latency_av = mean(Onset_Latency, na.rm = T),
              sleep_time_av = mean(Sleep_Time, na.rm = T),
              sleep_eff_av = mean(percet_Sleep, na.rm = T),
              WASO_av = mean(WASO, na.rm = T),
              frag_av = mean(Fragmentation, na.rm = T),
              wake_bouts_av = mean(Wake_Bouts, na.rm = T),
              ############## rmssd of times
              start_time_v = rmssd_edit(start_time),
              end_time_v = rmssd_edit(end_time),
              onsite_latency_v = rmssd(Onset_Latency),
              sleep_time_v = rmssd(Sleep_Time),
              sleep_eff_v = rmssd(percet_Sleep),
              WASO_v = rmssd(WASO),
              frag_v = rmssd(Fragmentation),
              wake_bouts_v = rmssd(Wake_Bouts),
              time = "G22")
#write.csv(G22, file="G22_watch_var.csv",row.names=FALSE)
######################## G32
################## 
sheets_name <- readxl::excel_sheets("data/G32_Exports_Edit.xls")
nsub=length(sheets_name)
data = lapply(1:nsub,read_one_sheet,filename="data/G32_Exports_Edit.xls")
df = do.call(rbind.data.frame, data)
names(df)=c("Interval_Type","Interval", "Start_Date","start_Time","End_Date","end_Time",
            "Onset_Latency","WASO",	"Wake_Bouts","Sleep_Time","percet_Sleep",	"Fragmentation","subject")
#head(df)
start_time_diff = as.numeric(df$`start_Time`- base_time,units ="hours")
df$start_time= ifelse(start_time_diff<0, start_time_diff+24,start_time_diff)
end_time_diff = as.numeric(df$`end_Time`- base_time,units ="hours")
df$end_time= ifelse(end_time_diff<0,end_time_diff+24,end_time_diff)
g32 = df
g32$time = "G32"
## number of patients 
nsub
## number of patients with >= 3 measure for G32
length(unique(df$subject))
#write.csv(g32, file="G32_original_watch.csv",row.names=FALSE)

G32 = df %>%
  group_by(subject) %>%
  summarize(################## average times
    start_time_av = mean(ifelse(start_time<12, start_time + 24, start_time), na.rm = T),
    end_time_av = mean(ifelse(end_time<12, end_time + 24, end_time), na.rm = T),
    onsite_latency_av = mean(Onset_Latency, na.rm = T),
    sleep_time_av = mean(Sleep_Time, na.rm = T),
    sleep_eff_av = mean(percet_Sleep, na.rm = T),
    WASO_av = mean(WASO, na.rm = T),
    frag_av = mean(Fragmentation, na.rm = T),
    wake_bouts_av = mean(Wake_Bouts, na.rm = T),
    ############## rmssd of times
    start_time_v = rmssd_edit(start_time),
    end_time_v = rmssd_edit(end_time),
    onsite_latency_v = rmssd(Onset_Latency),
    sleep_time_v = rmssd(Sleep_Time),
    sleep_eff_v = rmssd(percet_Sleep),
    WASO_v = rmssd(WASO),
    frag_v = rmssd(Fragmentation),
    wake_bouts_v = rmssd(Wake_Bouts),
    time = "G32")
#write.csv(G32, file="G32_watch_var.csv",row.names=FALSE)

g22_32_long=data.frame(rbind(G22,G32))
write.csv(g22_32_long, file="data/watch_G22_G32.csv",row.names=FALSE,na="")

# library(tidyr)
# library(reshape2)
# g22_32_wide=reshape(g22_32_long, idvar = "subject", timevar = "time", direction = "wide")
# long=read.csv("long.csv",header=TRUE)
# short=read.csv("short.csv",header=TRUE)
# delay_data = data.frame(delay=short$delay,subject=short$subject)
# long_temp=merge(long,delay_data, by.x="subject", by.y="subject", all=TRUE)
# 
# long_watch = merge(long_temp,g22_32_long,by.x = c("subject","timepoint"), by.y = c("subject","time"), all = TRUE)
# short_watch = merge(short,g22_32_wide,by = "subject", all=TRUE)
# 
# g22_32_org = rbind(g22, g32)
# long_long_g22_32_org = merge(long_temp,g22_32_org,by.x = c("subject","timepoint"), by.y = c("subject","time"), all = TRUE)
# 
# write.csv(long_long_g22_32_org, file="long_long_g22_32_org.csv",row.names=FALSE,na="")
# write.csv(long_watch, file="long_watch.csv",row.names=FALSE,na="")
# write.csv(short_watch,file="short_watch.csv",row.names=FALSE,na="")
