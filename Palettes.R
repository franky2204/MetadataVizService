library(MoMAColors)

possible_palettes_gradient<-list("Ernst","Exter","Flash","Alkalay1","Alkalay2","Althoff")
possible_palettes_facets<-list("Picasso","VanGogh","Levine1","Levine2","Fritsch","Ohchi","OKeeffe")
possible_palettes_neutral<-list(c("#454743","#D7DDD6"),c("#43474D","#D2D5DA"),c("#615F56","#D0CCC0"),c("#363236","#D9D0D8"),class="palette")


# df_prova <- read_csv("~/df_prova.csv")
# colTypes<-c("character","character","factor","factor","logic","numeric","numeric")
# ord_factor<-c(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE)
# names(colTypes)<-colnames(df_prova)
# names(ord_factor)<-colnames(df_prova)
# ntype_ord<-list(V2=3)
# ntype_facets<-list(V1=5)
# 
# palettes_factord<-mapply(moma.colors,possible_palettes_gradient[1:sum(ord_factor)],n=ntype_ord,SIMPLIFY = FALSE, USE.NAMES = FALSE)
# names(palettes_factord)<-names(colTypes)[ord_factor]
# 
# palettes_factnonord<-mapply(moma.colors,possible_palettes_facets[1:sum(!ord_factor[colTypes=="factor"])],n=ntype_facets,SIMPLIFY = FALSE, USE.NAMES = FALSE)
# names(palettes_factnonord)<-names(colTypes)[!ord_factor&colTypes=="factor"]
# 
# palettes_logic<-possible_palettes_neutral[1:sum(colTypes=="logic")]
# names(palettes_logic)<-names(colTypes)[colTypes=="logic"]
# 
# palettes<-append(append(palettes_factnonord,palettes_factord),palettes_logic)
# 
# boxplot_factornonord_num(df_prova,"V1","V5",palettes)
# ggplot(df_prova)+
#   geom_boxplot(aes(x=V1,
#                    y=V5),
#                alpha=0.5
#             #   fill=palettes["V1"][[1]]
#             #   color=palettes[var1][[1]]
#             )+
#   my_theme+
#   theme(
#     panel.grid.major.x = element_line(color="grey",linewidth = 0.1),
#     panel.grid.minor.x = element_line(color="grey",linewidth = 0.1),
#     axis.text = element_text(family = "PT Sans",size=15)
#   )
