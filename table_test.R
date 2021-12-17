data.ok

install.packages('flextable')
library(flextable)



names(get("mc.reside", data.ok) %>% attr('labels'))

ft <- flextable(iris)
ft <- add_header_row(ft, colwidths = c(2,3),
                     values = c("Sepal","Petal"))
df <- data.frame(table(data.ok$mc.reside))
flextable(df)

