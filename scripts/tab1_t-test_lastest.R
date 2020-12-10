source("test/main_pkgs.R", encoding = "utf-8")

df = read_xlsx("E:/Research/cuijj6/jpaper/data-raw/2020年硕士毕业论文最终数据-GDM史患者再发妊娠期糖代谢异常-最终分析数据.xlsx") %>% as.data.table()
d <- df[, .(days2 = pweek2*7+pday2, days1 = pweek1*7+pday1, mode2, weight1_1, weight2_1, mode1)]
varnames <- c("days", "weight", "model") %>% set_names(., .)

t.test2 <- function(df, second_type = 0) {
    df = df[second == second_type]
    df2 <- list(first = df[, ..varnames_1], second = df[, ..varnames_2]) %>%
        map(~ set_names(., varnames_new)) %>%
        melt_list("order")
    df2$OGTT0_d %<>% as.numeric()
    df2$age %<>% as.numeric()

    d1 = df2[order == "first", -1]
    d2 = df2[order == "second", -1]
    t_test2.data.frame(d1, d2, NULL)
}

## 2. previous version ------------------------------------------------------------
# list(second = d[, lapply(.SD, label_sd), .(mode2), .SDcols = c("days2", "weight1_1")],
#     first = d[, lapply(.SD, label_sd), .(mode1), .SDcols = c("days1", "weight2_1")])
df %<>% mutate(
  bmi_d2 = BMI(wght_d2, height2),
  bmi_d1 = BMI(wght_d1, height1),
  bmi_b2 = BMI(wght_b2, height2),
  bmi_b1 = BMI(wght_b1, height1),
  `wght_db2` = wght_d2 - wght_b2,
  `wght_db1` = wght_d1 - wght_b1)
df %<>% mutate(`pweek2` = pweek2 + pday2/7,
               `pweek1` = pweek1 + pday1/7)
varnames <-
  c("HBA2_d", "OGTT2_0_d", "OGTT2_1_d", "OGTT2_2_d", "CHOL2_d", "TG2_d", "HDL2_d", "LDL2_d", "pweek2", "weight2_1", "bmi_b2", "bmi_d2", "wght_b2","wght_d2", "wght_db2", "age2",
    "HBA1_d", "OGTT1_0_d", "OGTT1_1_d", "OGTT1_2_d", "CHOL1_d", "TG1_d", "HDL1_d", "LDL1_d", "pweek1", "weight1_1", "bmi_b1", "bmi_d1", "wght_b1", "wght_d1", "wght_db1", "age1")

n = length(varnames)/2
varnames_2 = varnames[1:n]
varnames_1 = varnames[-(1:n)]

varnames_new <- c("HBA_d", "OGTT0_d", "OGTT1_d", "OGTT2_d", "CHOL_d", "TG_d", "HDL_d", "LDL_d",
                  "pweek", "weight", "bmi_b", "bmi_d", "wght_b","wght_d", "wght_db", "age") %>% set_names(., .)
levels <- c("OGTT正常组", "GDM组", "PGDM")
df$second %<>% factor(0:2, levels)

## 1. 对连续型变量进行t检验-----------------------------------------------------
{
    res = foreach(second_type = levels) %do%
        {
            t.test2(df, second_type)
        } %>%
        set_names(levels) %>%
        melt_list("type")
    res = res[order(variable, type), ]
    write_list2xlsx(res, "Table1_cuijj_20201210")
}

## 2. 对离散型变量进行卡方检验，age >= 35---------------------------------------
# `correct=FALSE`为了和SPSS取得一致的结果
{
    d <- df[, .(second, first = age1 >= 35, second = age2 >= 35)] %>%
        melt("second", variable.name = "type", value.name = "effect")

    d[, chisq.test(type, effect, correct = FALSE) %$% {
        listk(statistic, p.value)
    }, .(second)]
    # 表格中数字过少时，需要采用fisher.test
    # d[, chisq.test(type, effect, correct = FALSE) %$% { listk(statistic, p.value) }, .(second)]
}
