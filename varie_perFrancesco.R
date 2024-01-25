library(tidyverse)

variables<-c("cohlesterol","ID","location","grade","recidivo","int","double")

classes<-c("numeric","character","factor","factor","logic","integer","numeric")

ord_factor<-c(FALSE,TRUE)

lev_factor<-list(c("I","II","III"))

df<-read.csv("df_prova.csv",
             sep=",",
             #colClasses=classes,
             col.names = variables
             )

nfact<-0
nordfact<-0
change_type<-function(i) {
  column_class <- classes[i]
  if (column_class == "character") {
    as.character(df[[i]])
  } else if (column_class == "numeric") {
    as.numeric(df[[i]])
  } else if (column_class == "factor") {
    nfact<-nfact+1
    order<-ord_factor[nfact]
    if(order){
      nordfact<-nordfact+1
      df[[i]]<-factor(df[[i]],ordered=TRUE,levels = lev_factor[[nordfact]])
    }
    else{
      as.factor(df[[i]])
    }
  }
  else if (column_class == "logic") {
    as.logical(df[[i]])
  }
  else if (column_class == "integer") {
    as.integer(df[[i]])
  }
}
df <- data.frame(lapply(1:ncol(df),change_type))
colnames(df)<-variables

typeof(df$ID)

write.table(nreads_repl_samplesMoia%>%
  group_by(ID,tissue)%>%
  mutate(mintbdisch=(seq_num==min(seq_num)),
         name=paste(paste(ID,tissue,sep = "-"),S,L,sep="_"))%>%
  filter(mintbdisch)%>%
  ungroup()%>%
  select(name),"Moia_tbdischarged.txt")

install.packages("MetBrewer")
library(plotwidgets)
library(MetBrewer)
#palette<-met.brewer("Veronese",3)
#met.brewer("Gauguin")
library(MoMAColors)

palette<-moma.colors("OKeeffe",5)

palette
col2hsl(palette)

summary(df$double)

desat_palette<-hsl2col(col2hsl(palette)-c(0,min(0.2,min(col2hsl(palette)[2,])),0))
dark_palette<-hsl2col(col2hsl(palette)-c(0,0,min(0.1,min(col2hsl(palette)[3,]))))
light_palette<-hsl2col(col2hsl(palette)+c(0,0,min(0.2,min(1-col2hsl(palette)[3,]))))

show_col(palette)
show_col(light_palette)
show_col(dark_palette)



library(scales)
library(plotwidgets)
desat_pink<-hsl2col(col2hsl(palette)-c(0,0.2,0))
light_pink<-hsl2col(col2hsl(palette)+c(0,0,0.05))
dark_pink<-hsl2col(col2hsl(palette)-c(0,0,0.05))
show_col(light_pink)
show_col(dark_pink)

show_col(c(palette,dark_pink))


quality_Pavia%>%
  select(c(ID,tissue,time,S,L,read,seq_num))%>%
  group_by(ID,tissue,time,S,L)%>%
  pivot_wider(names_from = read,
              values_from = seq_num)%>%
  mutate(R1_R2=R1-R2)%>%
  ungroup()%>%
  filter(R1_R2!=0)

plot<-ggplot(df)+
  geom_bar(aes(x=location))

ggsave("prova_grafico1.jpeg",
       plot,
       width = 8,
       height = 5)
