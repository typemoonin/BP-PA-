BP <- read.csv("D:\\大学文件\\UKB数据\\血压and脉率.csv")#读取血压的文件
BP.clean <- na.omit(BP)#去除NA值
BP_high <- function(SBP1, SBP2, DBP1, DBP2) {
  # 如果任意一个收缩压或舒张压超过 140/90，则判定为高血压
  if (any(SBP1 >= 140 | DBP1 >= 90 | SBP2 >= 140 | DBP2 >= 90)) {
    return("高血压")
  } else {
    return("正常")
  }
}#定义判断高血压的函数
BP.clean$hyper <- mapply(BP_high,
                         +                          BP.clean$Systolic.blood.pressure..automated.reading...Instance.0...Array.0,
                         +                          BP.clean$Systolic.blood.pressure..automated.reading...Instance.0...Array.1,
                         +                          BP.clean$Diastolic.blood.pressure..automated.reading...Instance.0...Array.0,
                         +                          BP.clean$Diastolic.blood.pressure..automated.reading...Instance.0...Array.1)#判断
BP_low <- function(SBP1, SBP2, DBP1, DBP2) {
  # 如果任意一个收缩压或舒张压低于 90/60，则判定为低血压
  if (any(SBP1 <= 90 | DBP1 <= 60 | SBP2 <= 90 | DBP2 <= 60)) {
    return("低血压")
  } else {
    return("正常")
  }
}#定义判断低血压的函数
# 假设 BP.clean 是你的数据框
BP.clean$hypo <- mapply(BP_low,
                                             BP.clean$Systolic.blood.pressure..automated.reading...Instance.0...Array.0,
                                             BP.clean$Systolic.blood.pressure..automated.reading...Instance.0...Array.1,
                                             BP.clean$Diastolic.blood.pressure..automated.reading...Instance.0...Array.0,
                                             BP.clean$Diastolic.blood.pressure..automated.reading...Instance.0...Array.1)#判断低血压
abnormal_BP <- subset(BP.clean, hyper == "高血压" | hypo == "低血压")#筛选血压不正常的人群
#用正确的筛选方法
abnormal_bp <- MetS_try_1_1 %>% filter(BP_criteria != "0")
#合并血压和PA值的数据
exposure_data <- BP_abnormal %>%
  select(Participant.ID) %>%
  left_join(PA %>% select(Participant.ID, LPA_result, MPA_result,VPA_result), by = "Participant.ID")
#合并癌症数据
final_data <- exposure_data %>% left_join(cancer_filtered,by="Participant.ID")
#将死亡时间加入final_data
final_data <- final_data %>% left_join(death %>% select(Participant.ID,Date.of.death),by="Participant.ID")
#转换数据格式
final_data$Date.of.attending.assessment.centre...Instance.0 <- as.Date(final_data$Date.of.attending.assessment.centre...Instance.0, format = "%Y-%m-%d")
final_data$Date.of.cancer.diagnosis <- as.Date(final_data$Date.of.cancer.diagnosis,format = "%Y-%m-%d")
final_data$Date.of.death <- as.Date(final_data$Date.of.death,format = "%Y-%m-%d")
outcome_time <- "2023-10-1"
outcome_time <- as.Date(outcome_time,format = "%Y-%m-%d")
