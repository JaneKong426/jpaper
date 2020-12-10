source("test/main_pkgs.R", encoding = "utf-8")

# file <- "test/2019年11月GDM再发最终分析数据.xlsx"
file <- "data-raw/table1_rawData.xlsx"
d1   <- read_xlsx(file, 1) %>% as.data.table()
d2   <- read_xlsx(file, 2) %>% as.data.table()

# d <- df[, .(days2 = pweek2*7+pday2, days1 = pweek1*7+pday1, mode2, weight1_1, weight2_1, mode1)]
# varnames <- c("days", "weight", "model") %>% set_names(., .)

{
    d <- list( second = d1, first = d2) %>%
        map( ~ set_names(., colnames(d1))) %>%
        melt_list("order")
    # d[!(model %in% c("剖宫产")), model := "其他"] # "顺产",
    # d$model %<>% factor(c("剖宫产", "其他")) #%>% as.character(), "顺产",

    print("mode")
    tbl2 = table(d[, .(order, `mode1（1=cesarean，0=delivery）`)])
    print(tbl)
    t2 <- chisq.test(tbl2)
    print(t2)

    print("年龄")
    tbl1 = d[, .(order, age1 >= 35)] %>% table()
    t1 = chisq.test(tbl1)
    print(tbl1)
    print(t1)

    # N <- 451
    # tbl_perc = round(tbl*100/451, 1)

    # chisq.test
    xsq
}
