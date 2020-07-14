# library(emayili)
# library(magrittr)
#
# # from = "991810576@qq.com"
# from = "kongdd@live.cn"
#
# email <- envelope() %>%
#     from(from) %>%
#     to(from) %>%
#     subject("This is a plain text message!") %>% text("Hello!")
#
# # smtp <- server(host = "smtp.gmail.com",
# #                port = 465,
# #                username = "bob@gmail.com",
# #                password = "bd40ef6d4a9413de9c1318a65cbae5d7")
# # smtp(email, verbose = TRUE)
#
# # qq_send_port: 465
# # smtp <- server(host = "smtp.qq.com",
# #                port = 587,
# #                username = from,
# #                password = "efkvkspmmdqybfab")
#
# smtp_live <- server(host = "smtp.office365.com",
#                port = 587,
#                username = from,
#                password = "genie156&")
# smtp(smtp_live, verbose = TRUE)
library(reticulate)


