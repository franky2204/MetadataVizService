library(tidyverse)

df<-data.frame(
  ID_patient=sample(paste("HAL78_",abs(.Random.seed)[1:278],sep=""),500,replace = TRUE),
  tissue=sample(c("rectum","colon","left","right"),500,replace = TRUE,prob=c(1,2,3,4)),
  hospital=sample(c("Torino1","Torino2","Milano","Alessandria","Genova"),500,replace = TRUE),
  grade=sample(c("I","II","III"),500,replace = TRUE,prob=c(2,2,1)),
  recidive=sample(c("TRUE","FALSE"),500,replace = TRUE,prob=c(2,1)),
  sex=sample(c("F","M"),500,replace = TRUE),
  therapy=sample(c("chemotherapy", "immunotherapy", "endocrine", "targeted"),500,replace = TRUE),
  time=sample(c("DIAG","FU3","M1","M2"),500,replace = TRUE),
  WBCellCount=rnorm(500,5,2),
  Hematocrit=rnorm(500,45,5),
  PlateletCount=rnorm(500,250,30))


df<-df%>%
  mutate(WBCellCount=ifelse(recidive,WBCellCount+2,WBCellCount),
         Hematocrit=ifelse(sex=="M",Hematocrit+5,Hematocrit),
         PlateletCount=ifelse(sex=="M",PlateletCount,PlateletCount+20),
         Prova_corr=WBCellCount*Hematocrit)

write.table(df,file = "df_Prova.tsv",quote=TRUE,sep="_")

variables<-c("ID_patient",
             "tissue",
             "hospital",
             "grade",
             "recidive",
             "sex",
             "therapy",
             "time",
             "WBCellCount",
             "Hematocrit",
             "PlateletCount",
             "Prova_corr")
colTypes<-c("character",
           "factor",
           "factor",
           "factor",
           "logic",
           "logic",
           "factor",
           "factor",
           "numeric",
           "numeric",
           "numeric",
           "numeric")

df[cbind(sample(1:nrow(df),500),sample(1:ncol(df),500,replace = TRUE))]<-NA

colnames(df) <- variables
names(colTypes)<-variables
ord_factor<-rep(FALSE,length(variables))
ord_factor[c(4,8)]<-TRUE
levels<-vector("list", ncol(df))
names(levels) <- variables
names(ord_factor) <- variables
levels[colTypes == "factor"] <- lapply(
  df %>%
    dplyr::select(
      all_of(
        variables[
          which(colTypes == "factor")
        ]
      )
    ),
  function(x){unique(na.omit(x))}
)

colTypes[colTypes == "factor" & sapply(df, function(v) {
  length(unique(v))
}) == 2] <- "logic"

ntype_ord <- lapply(
  levels[colTypes == "factor" & ord_factor],
  length
)
ntype_facets <- lapply(
  levels[colTypes == "factor" & !ord_factor],
  length
)

ndata <- nrow(df)
df <- as_tibble(
  lapply(variables,
         change_type,
         df = df,
         levels = levels,
         colTypes = colTypes,
         ord_factor = ord_factor
  ),
  .name_repair = "universal"
)

colnames(df) <- variables
colTypes <- factor(colTypes,
                      levels = c("character", "logic", "factor", "numeric")
)


palettes_factord<- mapply(moma.colors,
                          possible_palettes_gradient[1:sum(ord_factor)],
                          n=ntype_ord,
                          SIMPLIFY = FALSE,
                          USE.NAMES = FALSE)
names(palettes_factord)<-names(colTypes)[ord_factor]

palettes_factnonord<-mapply(moma.colors,
                            possible_palettes_facets[1:sum(!ord_factor[colTypes=="factor"])],
                            n=ntype_facets,
                            SIMPLIFY = FALSE,
                            USE.NAMES = FALSE)
names(palettes_factnonord)<-names(colTypes)[!ord_factor&colTypes=="factor"]

palettes_logic<-possible_palettes_neutral[1:sum(colTypes=="logic")]
names(palettes_logic)<-names(colTypes)[colTypes=="logic"]


