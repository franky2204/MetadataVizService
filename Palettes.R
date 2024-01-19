possible_palettes_gradient<-list("Ernst","Exter","Flash","Alkalay1","Alkalay2","Althoff")
possible_palettes_facets<-list("Picasso","VanGogh","Levine1","Levine2","Fritsch","Ohchi","OKeeffe")
possible_palettes_neutral<-list(c("#454743","#D7DDD6"),c("#43474D","#D2D5DA"),c("#615F56","#D0CCC0"),c("#363236","#D9D0D8"),class="palette")

palettes<-append(
  append(
    mapply(moma.colors,possible_palettes_gradient[1:sum(ord_factor)],n=ntype_ord,SIMPLIFY = FALSE, USE.NAMES = FALSE),
    mapply(moma.colors,possible_palettes_facets[1:sum(!ord_factor)],n=ntype_facets,SIMPLIFY = FALSE,USE.NAMES = FALSE)),
  possible_palettes_neutral[1:sum(classes=="logic")]
)
attr(palettes,"class") <- NULL

names(palettes)<-c(variables[which(classes=="factor")][ord_factor],
                   variables[which(classes=="factor")][!ord_factor],
                   variables[which(classes=="logic")])

rm(list=c("possible_palettes_gradient","possible_palettes_facets","possible_palettes_neutral"))
