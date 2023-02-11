## Willingness to Pay

modelinter1$estimate[["b_Erreichbarkeit"]]/modelinter1$estimate[["b_Miete"]]

allwtp <-wtp(cost="b_Miete", attr = names(modelinter1$estimate), modelname = modelinter1)

allwtp

