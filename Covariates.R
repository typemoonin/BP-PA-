#处理性别
covariates_filtered <- data.frame(covariates$Participant.ID, covariates$Sex)
covariates_filtered$covariates.Sex <- ifelse(covariates_filtered$covariates.Sex == "Male", 1, 0)
#合并年龄列
covariates_filtered <- covariates_filtered %>%
  left_join(covariates %>% select(Participant.ID, Age.at.recruitment), by = "Participant.ID")
#处理种族数据
df <- read.csv("D:\\大学文件\\UKB数据\\包含种族.csv")
covariates_filtered <- covariates_filtered %>%
  left_join(df %>% select(Participant.ID, Ethnic.background...Instance.0), by = "Participant.ID")
covariates_filtered <- covariates_filtered %>%
  mutate(Ethnic.background...Instance.0 = ifelse(Ethnic.background...Instance.0 == "British", 1, 0))

#处理教育程度数据
covariates_filtered <- covariates_filtered %>%
  left_join(covariates %>% select(Participant.ID, Education.score), by = "Participant.ID")
#处理吸烟程度数据
covariates_filtered <- covariates_filtered %>%
  left_join(covariates %>% select(Participant.ID, Smoking.status...Instance.0), by = "Participant.ID")
covariates_filtered <- covariates_filtered %>%
  mutate(Smoking.status...Instance.0 = recode(Smoking.status...Instance.0, 
                               "Current" = 2, 
                               "Previous" = 1, 
                               "Never" = 0))
#处理酒精摄入，但是是饮酒状态
covariates_filtered <- covariates_filtered %>%
  left_join(covariates %>% select(Participant.ID, Alcohol.drinker.status), by = "Participant.ID")
covariates_filtered <- covariates_filtered %>%
  mutate(Alcohol.drinker.status = recode(Alcohol.drinker.status, 
                                              "Current" = 2, 
                                              "Previous" = 1, 
                                              "Never" = 0))
#自评健康状态
covariates_filtered <- covariates_filtered %>%
  left_join(covariates %>% select(Participant.ID, Overall.health.rating), by = "Participant.ID")
covariates_filtered <- covariates_filtered %>%
  mutate(Overall.health.rating = recode(Overall.health.rating, 
                                         "Excellent" = 4, 
                                         "Good" = 3, 
                                         "Fair" = 2,
                                         "Poor" = 1))
#汤森剥夺指数
covariates_filtered <- covariates_filtered %>%
  left_join(covariates %>% select(Participant.ID, Townsend.deprivation.index.at.recruitment), by = "Participant.ID")
#BMI
covariates_filtered <- covariates_filtered %>%
  left_join(covariates %>% select(Participant.ID, BMI), by = "Participant.ID")
#虚弱
covariates_filtered <- covariates_filtered %>%
  left_join(frailty %>% select(Participant.ID, Physical_Frailty), by = "Participant.ID")
#IPAQ数据
covariates_filtered <- covariates_filtered %>%
  left_join(covariates %>% select(Participant.ID, IPAQ.activity.group...Instance.0), by = "Participant.ID")
covariates_filtered$IPAQ.activity.group...Instance.0 <- recode(covariates_filtered$IPAQ.activity.group...Instance.0,
                          "low" = 1,
                          "moderate" = 2,
                          "high" = 3)
