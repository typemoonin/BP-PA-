#泛癌分析
#####################
####尝试进行一个泛癌的分析
library(survival)
names(cancer_filtered)

# 计算随访时间和pheno
# 设定随访结束时间
follow_up_end <- as.Date("2023-12-31")  # 修改为实际的随访截止日期

# 计算随访时间
final_data$all.cancer.time <- with(final_data, ifelse(
  # 1️⃣ 只有死亡，没有癌症确诊 → 计算死亡时间
  !is.na(Date.of.death) & is.na(Date.of.cancer.diagnosis),
  as.numeric(difftime(Date.of.death, Date.of.attending.assessment.centre...Instance.0, units = "days")),
  
  # 2️⃣ 只有癌症确诊，没有死亡 → 计算癌症确诊时间
  ifelse(is.na(Date.of.death) & !is.na(Date.of.cancer.diagnosis),
         as.numeric(difftime(Date.of.cancer.diagnosis, Date.of.attending.assessment.centre...Instance.0, units = "days")),
         
         # 3️⃣ 既有癌症确诊，又有死亡 → 仍然按癌症确诊时间计算（保持原代码逻辑）
         ifelse(!is.na(Date.of.death) & !is.na(Date.of.cancer.diagnosis),
                as.numeric(difftime(Date.of.cancer.diagnosis, Date.of.attending.assessment.centre...Instance.0, units = "days")),
                
                # 4️⃣ 没有死亡，也没有癌症确诊 → 计算随访结束时间
                as.numeric(difftime(outcome_time, Date.of.attending.assessment.centre...Instance.0, units = "days"))
         )))) 

summary(final_data$all_cancer_time )
# 生成pheno列：earliest_cancer不为NA的赋值为1，否则赋值为0
final_data$all.cancer.pheno <- ifelse(!is.na(final_data$Date.of.cancer.diagnosis), 1, 0)

#我还需要合并数据框
full_data <- merge(final_data, covariates_filtered, by = "Participant.ID")
#######cox风险回归模型
#LPA的模型
model_III_LPA <- coxph(Surv(all.cancer.time, all.cancer.pheno) ~
                     LPA_result          
                    +Education.score                          
                    +Smoking.status...Instance.0               
                    +Alcohol.drinker.status                   
                    +Overall.health.rating                     
                    +Townsend.deprivation.index.at.recruitment
                    +BMI                                       
                    +Physical_Frailty                         
                    +Age.at.recruitment
                    +Ethnic.background...Instance.0,  
                     data = full_data)
#MPA的模型
model_III_MPA <- coxph(Surv(all.cancer.time, all.cancer.pheno) ~
                         MPA_result          
                       +Education.score                          
                       +Smoking.status...Instance.0               
                       +Alcohol.drinker.status                   
                       +Overall.health.rating                     
                       +Townsend.deprivation.index.at.recruitment
                       +BMI                                       
                       +Physical_Frailty                         
                       +Age.at.recruitment
                       +Ethnic.background...Instance.0,  
                       data = full_data)
#VPA的模型
model_III_VPA <- coxph(Surv(all.cancer.time, all.cancer.pheno) ~
                         VPA_result          
                       +Education.score                          
                       +Smoking.status...Instance.0               
                       +Alcohol.drinker.status                   
                       +Overall.health.rating                     
                       +Townsend.deprivation.index.at.recruitment
                       +BMI                                       
                       +Physical_Frailty                         
                       +Age.at.recruitment
                       +Ethnic.background...Instance.0,  
                       data = full_data)
#使用IPAQ的数据
model_III_IPAQ  <- coxph(Surv(all.cancer.time, all.cancer.pheno) ~
                        IPAQ.activity.group...Instance.0           
                       +Education.score                          
                       +Smoking.status...Instance.0               
                       +Alcohol.drinker.status                   
                       +Overall.health.rating                     
                       +Townsend.deprivation.index.at.recruitment
                       +BMI                                       
                       +Physical_Frailty                         
                       +Age.at.recruitment
                       +Ethnic.background...Instance.0,  
                       data = full_data)
####泛癌尝试批量分析每一种癌的结果

names(cancer_part_1)
cancer_part_1 <- cancer_part_1[,-c(2:4)]
names(PRS_kidney_cancer)
pan_cancer_IR <- PRS_kidney_cancer[,c(1,2,8:10,16,17,19:32,39,40,62:67,121,122,114:119)]

###
pan_cancer_IR <- merge(pan_cancer_IR,cancer_part_1,all.x = T,by="Participant.ID")
names(pan_cancer_IR)
###
names(cancer_part_2)
cancer_part_2 <- cancer_part_2[,-c(2:4)]
pan_cancer_IR <- merge(pan_cancer_IR,cancer_part_2,all.x = T,by="Participant.ID")
names(pan_cancer_IR)
###
names(cancer_part_3)
cancer_part_3 <- cancer_part_3[,-c(2:4)]
pan_cancer_IR <- merge(pan_cancer_IR,cancer_part_3,all.x = T,by="Participant.ID")
names(pan_cancer_IR)
###
names(cancer_part_4)
cancer_part_4 <- cancer_part_4[,-c(2:4)]
pan_cancer_IR <- merge(pan_cancer_IR,cancer_part_4,all.x = T,by="Participant.ID")
names(pan_cancer_IR)


# 加载必要的包
library(survival)

names(pan_cancer_IR)
# 获取癌症列的索引范围（奇数列为癌症类型）
cancer_cols <- seq(8, ncol(full_data), by = 2)  # 偶数列为癌症类型
time_cols <- seq(9, ncol(full_data), by = 2)    # 奇数列为对应的时间列

pan_cancer_IR <- subset(pan_cancer_IR,pan_cancer_IR$Sex=="Female") 

cancer_cols <- "C54.1 Endometrium"
time_cols <- "Time_C54.1 Endometrium"

# 初始化一个列表用于存储每次回归结果
cox_results <- list()

# 批量回归
for (i in seq_along(cancer_cols)) {
  
  # 获取癌症列和对应的时间列
  cancer_col_name <- colnames(full_data)[cancer_cols[i]]
  time_col_name <- colnames(full_data)[time_cols[i]]
  
  # 计算cancer_pheno：该癌症列不为NA的赋值为1，其他为0
  full_data$cancer_pheno <- ifelse(!is.na(full_data[[time_col_name]]), 1, 0)
  #计算随访时间
  full_data$cancer_time <- with(full_data, {
    # 获取当前癌症的时间列，动态选择该列
    time_col <- full_data[[time_col_name]]
    # 使用 ifelse 逐元素地判断条件并计算随访时间
    cancer_time <- ifelse(
      # 条件 1: 如果 'Date.of.death' 不为 NA 且 'time_col' 为 NA
      !is.na(Date.of.death) & is.na(time_col), 
      # 如果条件成立，计算从 'Date.of.death' 到 'Date.of.attending.assessment.centre...Instance.0' 的时间差，单位为天
      as.numeric(difftime(Date.of.death, Date.of.attending.assessment.centre...Instance.0, units = "days")),
      
      # 条件 2: 如果 'Date.of.death' 为 NA 且 'time_col' 不为 NA
      ifelse(
        is.na(Date.of.death) & !is.na(time_col), 
        # 如果条件成立，计算从 'time_col' 到 'Date.of.attending.assessment.centre...Instance.0' 的时间差，单位为天
        as.numeric(difftime(time_col, Date.of.attending.assessment.centre...Instance.0, units = "days")),
        
        # 条件 3: 如果 'Date.of.death' 和 'time_col' 都不为 NA
        ifelse(
          !is.na(Date.of.death) & !is.na(time_col), 
          # 如果条件成立，计算从 'time_col' 到 'Date.of.attending.assessment.centre...Instance.0' 的时间差，单位为天
          as.numeric(difftime(time_col, Date.of.attending.assessment.centre...Instance.0, units = "days")),
          
          # 默认情况：如果上述条件都不满足，计算 'time' 到 'Date.of.attending.assessment.centre...Instance.0' 的时间差，单位为天
          as.numeric(difftime(outcome_time, Date.of.attending.assessment.centre...Instance.0, units = "days"))
        )
      )
    )
    
    # 返回计算的随访时间
    return(cancer_time)
  })
  
  
  
  # 定义Cox回归模型
  model_III_IPAQ  <- coxph(Surv(cancer_time, cancer_pheno) ~
                             IPAQ.activity.group...Instance.0           
                           +Education.score                          
                           +Smoking.status...Instance.0               
                           +Alcohol.drinker.status                   
                           +Overall.health.rating                     
                           +Townsend.deprivation.index.at.recruitment
                           +BMI                                       
                           +Physical_Frailty                         
                           +Age.at.recruitment
                           +Ethnic.background...Instance.0,  
                           data = full_data)
  
  # 保存回归结果到列表中，命名为相应的癌症名称
  cox_results[[cancer_col_name]] <- summary(model_III_IPAQ)
}



# 如果你需要保存结果，可以将它们写入文件
save(cox_results, file = "4_cox_results.RData")

# 初始化一个空的数据框来存储结果
result_df <- data.frame(
  Cancer_Type = character(),
  Coef = numeric(),
  HR = numeric(),
  lower_95_CI = numeric(),
  upper_95_CI = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# 遍历cox_results，提取eGDR_change的结果
for (cancer in names(cox_results)) {
  # 提取当前癌症的结果
  current_result <- cox_results[[cancer]]
  
  # 找到IPAQ对应的行
  IPAQ_index <- which(rownames(current_result$coefficients) == "IPAQ.activity.group...Instance.0")
  
  # 提取回归系数、HR、95%置信区间和p值
  coef <- current_result$coefficients[IPAQ_index, "coef"]
  HR <- current_result$coefficients[IPAQ_index, "exp(coef)"]
  lower_95_CI <- current_result$conf.int[IPAQ_index, "lower .95"]
  upper_95_CI <- current_result$conf.int[IPAQ_index, "upper .95"]
  p_value <- current_result$coefficients[IPAQ_index, "Pr(>|z|)"]
  
  # 将结果添加到数据框中
  result_df <- rbind(result_df, data.frame(
    Cancer_Type = cancer,
    Coef = coef,
    HR = HR,
    lower_95_CI = lower_95_CI,
    upper_95_CI = upper_95_CI,
    p_value = p_value,
    stringsAsFactors = FALSE
  ))
}

# 打印结果
print(result_df)

# 保存结果到csv文件
write.csv(result_df, file = "运动泛癌结果.csv", row.names = FALSE)

############
pancancer_res_all <- read.csv("泛癌分析总结果 画图.csv")
head(pancancer_res_all)

pancancer_res_all <- na.omit(pancancer_res_all)
# 加载dplyr包
library(dplyr)