server<-function(input,output, session){
  
  # definition of reactive variables
  rv <- reactiveValues(
    AllAlright =FALSE,
    fileUploaded=FALSE,
    df_pre = NULL,
    df_post = NULL,
    variables = NULL,
    ncol = NULL,
    colTypes = NULL,
    levels = NULL,
    ntype_ord = NULL,
    ntype_facets =NULL,
    ord_factor =NULL,
    palettes=NULL,
    plot_n=1,
    plot=NULL
  )
  
  # condition to show the table adjustments buttons
  output$fileUploaded <- reactive({
    return(!is.null(input$file))
  })
  outputOptions(output,
                "fileUploaded",
                suspendWhenHidden = FALSE
  )
  
  # load del dataframe
  observeEvent(list(input$file,input$header,input$sep,input$quote),{
    if(req(!rv$AllAlright)){
      
      req(input$file)
      df <- NULL
      ext <- tools::file_ext(input$file$name)
      if (ext == "csv" && input$manually%%2 == 0) {
        
        df <- read.csv(input$file$datapath,
                       header = TRUE,
                       fill = TRUE,
                       stringsAsFactors = FALSE
        )
        df[df==""]<-NA
        rv$df_pre <- df
        rv$variables <- colnames(df)
        rv$ncol <- ncol(df)
        rv$levels <- vector("list", ncol(df))
        rv$colTypes <- rep("", ncol(df))
      }
      else if (ext == "tsv" && input$manually%%2 == 0) {
        
        df <- read.table(input$file$datapath,
                         header = TRUE,
                         sep="\t",
                         fill = TRUE,
                         stringsAsFactors = FALSE
        )
        df[df==""]<-NA
        
        rv$df_pre <- df
        rv$variables <- colnames(df)
        rv$ncol <- ncol(df)
        rv$levels <- vector("list", ncol(df))
        rv$colTypes <- rep("", ncol(df))
      }
      else {
        tryCatch(
          {
            df<-read.delim(input$file$datapath,
                           header = input$header,
                           sep = input$sep,
                           quote = input$quote
            )
            df[df==""]<-NA
            
            rv$df_pre <- df
            rv$variables <- colnames(df)
            rv$ncol <- ncol(df)
            rv$levels <- vector("list", ncol(df))
            rv$colTypes <- rep("", ncol(df))
          },
          warning = function(cond) {
            message("Here's the original error message:")
            message(conditionMessage(cond))
            df<-read.table(input$file$datapath,
                           header = input$header,
                           sep = input$sep,
                           quote = input$quote
            )
          },
          error = function(cond) {
            message("There is something wrong:")
            message(conditionMessage(cond))
          }
        )
      }
    }
  })
  
  # show tabella
  output$df_table <- renderDataTable(DT::datatable(rv$df_pre,
                                                   callback = JS(callback),
                                                   rownames = FALSE,
                                                   options = list(dom = "t", ordering = F)
  ))
  
  
  # aggiornamento nomi variabili
  observeEvent(input$colnames, {
    if(req(!rv$AllAlright)){
      req(input$file)
      req(input$colnames)
      rv$variables <- input$colnames
      names(rv$colTypes) <- rv$variables
    }
  })
  
  # crea Picker
  output$colTypeInputs <- renderUI({
    req(input$file)
    columnNames <- rv$variables
    tagList(
      lapply(1:rv$ncol, function(i) {
        pickerInput(paste0("colType_", i),
                    width = "fit",
                    label = paste(columnNames[i],": ",sep=""),
                    choices = c("", "character", "numeric", "factor"),
                    selected = rv$colTypes[i]
                    
        )
      })
    )
  })
  
  # aggiornamento tipi colonne
  observeEvent(
    lapply(1:req(rv$ncol),
           function(i) {
             return(input[[paste0("colType_", i)]])
           }
    ),
    {
      if(req(!rv$AllAlright)){
        
        req(input$file)
        req(input$changed)
        last_input <- input$changed
        i<-which(last_input==sapply(1:rv$ncol,
                                    function(i) {
                                      return(paste0("colType_", i))
                                    }
        )
        )
        if(length(i)!=0){
          colTypeInput <-input[[last_input]]
          rv$colTypes[i]<-colTypeInput
          if (colTypeInput == "factor") {
            var<-rv$variables[i]
            n_fact<-rv$df_pre%>%pull(all_of(var))%>%unique()%>%length()
            if(n_fact<=10){
              levels_ord <- rv$levels[[i]]
              if (is.null(levels_ord)) {
                levels_non_ord <- unique(na.omit(rv$df_pre[, i]))
              }
              else {
                levels_non_ord <- NULL
              }
              showModal(
                modalDialog(
                  bucket_list(
                    header = "Do the factors have a preferential order? (i.e. they are time steps) \n
                 if so drag them in the right bucket in proper order.",
                    group_name = paste0("bucket_ord_", last_input),
                    orientation = "horizontal",
                    add_rank_list(
                      text = "Unordered factor detected",
                      labels = levels_non_ord,
                      input_id = paste0("factor_nonord_", i)
                    ),
                    add_rank_list(
                      text = "Correct order",
                      labels = levels_ord,
                      input_id = paste0("factor_ord_", i)
                    )
                  ),
                  footer = tagList(
                    shinyjs::useShinyjs(),
                    actionButton(paste0("submit_nonord_", i),"Order is not meaningful"),
                    actionButton(paste0("submit_ord_", i), "Save with this order"),
                  )
                )
              )
            }
            else{
              shinyalert(title = "Not quite the choice",
                         text = paste("It appears that your column contains ",
                                      n_fact,
                                      " different levels when considered as a factor, whereas we can only handle a maximum of 10. \n
                                    You may have inadvertently selected the wrong column type. However, if this isn't the case and the number of values doesn't align with your expectations, consider reviewing your table for any discrepancies and reloading it. \n
                                    Please bear in mind that the system distinguishes values even if they differ only by a capitalization, spacing, or a single letter.",
                                      collapse=""),
                         confirmButtonCol = "#19323C",
                         imageHeight = "200",
                         imageWidth = "300",
                         size = "m",
                         imageUrl = "https://i.kym-cdn.com/entries/icons/original/000/018/489/nick-young-confused-face-300x256-nqlyaa.jpg")
              rv$colTypes[i]<-""
            }
          }
        }
      }
    })
  
  # salva ordine factor
  observeEvent(lapply(1:req(rv$ncol),
                      function(i) {
                        return(input[[paste0("submit_ord_", i)]])
                      }
  ),
  {if(req(!rv$AllAlright)){
    
    req(input$file)
    last_input <- input$changed
    i<-which(last_input==sapply(1:rv$ncol,
                                function(i) {
                                  return(paste0("submit_ord_", i))
                                }
    )
    )
    if(length(i)!=0){
      rv$levels[[i]] <- req(input[[paste0("factor_ord_", i)]])
      removeModal()
    }
  }})
  
  # risalva factor non ord
  observeEvent(lapply(1:req(rv$ncol),
                      function(i) {
                        return(input[[paste0("submit_nonord_", i)]])
                      }
  ),
  {if(req(!rv$AllAlright)){
    
    req(input$file)
    last_input <- input$changed
    i<-which(last_input==sapply(1:rv$ncol,
                                function(i) {
                                  return(paste0("submit_nonord_", i))
                                }
    )
    )
    if(length(i)!=0){
      rv$levels[i]<-list(NULL)
      removeModal()
    }
  }})
  
  # attiva load se tutti campi fillati
  observe({
    toggleState(
      "load",
      !is.null(rv$df_pre) & all(rv$colTypes != "") & all(!is.null(rv$colTypes))
    )
    req(input$file)
    lapply(1:req(rv$ncol), function(col) {
      toggleState(
        paste0("submit_ord_", col),
        length(input[[paste0("factor_nonord_", col)]]) == 0
      )
    })
  })
  
  # Sistemazione df
  observeEvent(input$load, {
    colnames(rv$df_pre) <- rv$variables
    names(rv$levels) <- rv$variables
    rv$ord_factor <- !sapply(rv$levels, is.null)
    names(rv$ord_factor) <- rv$variables
    rv$levels[rv$colTypes == "factor" & !rv$ord_factor] <- lapply(
      rv$df_pre %>%
        dplyr::select(
          all_of(
            rv$variables[
              which(rv$colTypes == "factor" & rv$ord_factor == FALSE)
            ]
          )
        ),
      function(x){unique(na.omit(x))}
    )
    rv$colTypes[rv$colTypes == "factor" & sapply(rv$df_pre, function(v) {
      length(unique(v))
    }) == 2] <- "logic"
    
    rv$ntype_ord <- lapply(
      rv$levels[rv$colTypes == "factor" & rv$ord_factor],
      length
    )
    rv$ntype_facets <- lapply(
      rv$levels[rv$colTypes == "factor" & !rv$ord_factor],
      length
    )
    
    rv$ndata <- nrow(rv$df_pre)
    rv$df_post <- as_tibble(
      lapply(rv$variables,
             change_type,
             df = rv$df_pre,
             levels = rv$levels,
             colTypes = rv$colTypes,
             ord_factor = rv$ord_factor
      ),
      .name_repair = "universal"
    )
    
    colnames(rv$df_post) <- rv$variables
    
    if(any(sapply(rv$df_post,function(col){
      return(all(sapply(col,is.na)))
    }))){
      wrong_type<-which(sapply(rv$df_post,function(col){
        return(all(sapply(col,is.na)))
      }))
      shinyalert(title = "Are you sure?",
                 text = paste("You probably have incorrectly assigned the variable type of",names(wrong_type),". \n Please select another variable type."),
                 imageUrl = "https://media.tenor.com/wy2zHeWyf2gAAAAe/side-eye-dog-suspicious-look.png",
                 imageHeight = "200",
                 imageWidth = "300",
                 confirmButtonCol = "#19323C")
      rv$AllAlright<-FALSE
    }
    else{
      rv$AllAlright<-TRUE
    }
    
    rv$colTypes <- factor(rv$colTypes,
                          levels = c("character", "logic", "factor", "numeric")
    )
    
    
    palettes_factord<- mapply(moma.colors,
                              possible_palettes_gradient[1:sum(rv$ord_factor)],
                              n=rv$ntype_ord,
                              SIMPLIFY = FALSE,
                              USE.NAMES = FALSE)
    names(palettes_factord)<-names(rv$colTypes)[rv$ord_factor]
    
    palettes_factnonord<-mapply(moma.colors,
                                possible_palettes_facets[1:sum(!rv$ord_factor[rv$colTypes=="factor"])],
                                n=rv$ntype_facets,
                                SIMPLIFY = FALSE,
                                USE.NAMES = FALSE)
    names(palettes_factnonord)<-names(rv$colTypes)[!rv$ord_factor&rv$colTypes=="factor"]
    
    palettes_logic<-possible_palettes_neutral[1:sum(rv$colTypes=="logic")]
    names(palettes_logic)<-names(rv$colTypes)[rv$colTypes=="logic"]
    
    rv$palettes<-append(append(palettes_factnonord,palettes_factord),palettes_logic)
  })
  
  output$AllAlright<-reactive({
    return(rv$AllAlright)
  })
  
  outputOptions(output,
                "AllAlright",
                suspendWhenHidden = FALSE
  )
  
  # crea le variabili da mostrare per la scelta
  output$variables_input<-renderUI({
    if(input$load!=0){
      req(rv$variables,rv$colTypes)
      tagList(
        checkboxGroupInput(
          inputId = "var",
          label = "Chose the variables you want to explore",
          choices = rv$variables[rv$colTypes!="character"],
          inline = FALSE
        ))
    }
  })
  
  # aggiorna il numero del plot da mostrare
  observeEvent(input$refresh, {
    rv$plot_n<-rv$plot_n+1
  })
  
  # aggiorna le variabili da mostrare
  observeEvent(input$var,{
    var<-names(sort(rv$colTypes[input$var]))
    var[rv$colTypes[var]=="factor"]<-names(sort(rv$ord_factor[names(rv$colTypes[var][rv$colTypes[var]=="factor"])]))
    if (length(var)==1){
      var<-as.character(input$var[1])
      
      if(rv$colTypes[var]=="logic"){
        rv$plot<-list(barplot_logic(rv$df_post,var,rv$palettes),
                      pie_logic(rv$df_post,var,rv$palettes)
        )
        rv$table<-rv$df_post%>%count(.data[[var]])
        if(all(!is.na(rv$df_post[[var]]))){
          rv$table<-rv$table%>%column_to_rownames(var)%>%t()
          rownames(rv$table)<-"Number \n of samples"
        }
        else{
          rv$table<-rv$table%>%filter(n)%>%t()
          colnames(rv$table)<-c(rv$levels[[var]],"NA")
          rownames(rv$table)<-"Number \n of samples"
        }
      }
      else if(rv$colTypes[var]=="factor"&!rv$ord_factor[var]){
        rv$plot<-list(waffle_factor_nonord(rv$df_post,var,rv$palettes,rv$ndata,rv$levels),
                      barplot_factor_nonord(rv$df_post,var,rv$palettes,rv$levels)
        )
        rv$table<-rv$df_post%>%count(.data[[var]])
        if(all(!is.na(rv$df_post[[var]]))){
          rv$table<-rv$table%>%column_to_rownames(var)%>%t()
          rownames(rv$table)<-"Number \n of samples"
        }
        else{
          rv$table<-rv$table%>%select(n)%>%t()
          colnames(rv$table)<-c(rv$levels[[var]],"NA")
          rownames(rv$table)<-"Number \n of samples"
        }
        
      }
      else if(rv$colTypes[var]=="factor"&rv$ord_factor[var]){
        rv$plot<-list(barplot_factor_ord_x(rv$df_post,var,rv$palettes),
                      barplot_factor_ord_stack(rv$df_post,var,rv$palettes)
        )
        rv$table<-rv$df_post%>%count(.data[[var]])
        if(all(!is.na(rv$df_post[[var]]))){
          rv$table<-rv$table%>%column_to_rownames(var)%>%t()
          rownames(rv$table)<-"Number \n of samples"
        }
        else{
          rv$table<-rv$table%>%select(n)%>%t()
          colnames(rv$table)<-c(rv$levels[[var]],"NA")
          rownames(rv$table)<-"Number \n of samples"
        }
      }
      else if(rv$colTypes[var]=="numeric"){
        rv$plot<-list(hist_numeric(rv$df_post,var),
                      density_numeric(rv$df_post,var,rv$palettes),
                      boxplot_numeric(rv$df_post,var,rv$palettes)
        )
        rv$table<-data.frame(
          as.array(
            summary(rv$df_post%>%select(.data[[var]])%>%pull())))%>%
          column_to_rownames("Var1")%>%t()
        rownames(rv$table)<-"values"
      }
    }
    else if (length(var)==2){
      var1<-as.character(var[1])
      var2<-as.character(var[2])
      
      if(rv$colTypes[var1]=="logic"&rv$colTypes[var2]=="logic"){
        rv$plot<-list(barplot_logic_logic(rv$df_post,var1,var2,rv$palettes),
                      barplot_logic_logic(rv$df_post,var2,var1,rv$palettes))
        rv$table<-rv$df_post%>%
          count(.data[[var1]],.data[[var2]])%>%
          pivot_wider(names_from = var2,values_from = n)
        
        if(all(!is.na(rv$df_post[[var1]]))){
          rv$table<-rv$table%>%
            column_to_rownames(var1)
        }
        else{
          rv$table<-rv$table%>%select(-all_of(var1))%>%t()
          colnames(rv$table)<-c(rv$levels[[var1]],"NA")
        }
        
      }
      else if(rv$colTypes[var1]=="logic" &
              rv$colTypes[var2]=="factor" & !rv$ord_factor[var2]){
        rv$plot<-list(barplot_logic_factnonord_stack(rv$df_post,var1,var2,rv$palettes),
                      barplot_logic_factnonord_dodge(rv$df_post,var1,var2,rv$palettes),
                      barplot_logic_factnonord_stack(rv$df_post,var2,var1,rv$palettes),
                      barplot_logic_factnonord_dodge(rv$df_post,var2,var1,rv$palettes)
        )
        rv$table<-rv$df_post%>%
          count(.data[[var1]],.data[[var2]])%>%
          pivot_wider(names_from = var2,values_from = n)
        if(all(!is.na(rv$df_post[[var1]]))){
          rv$table<-rv$table%>%
            column_to_rownames(var1)
        }
        else{
          rv$table<-rv$table%>%select(-all_of(var1))%>%t()
          colnames(rv$table)<-c(rv$levels[[var1]],"NA")
        }
      }
      else if(rv$colTypes[var1]=="logic" &
              rv$colTypes[var2]=="factor" & rv$ord_factor[var2]){
        rv$plot<-list(barplot_logic_factord_stack(rv$df_post,var1,var2,rv$palettes),
                      barplot_logic_factord_dodge(rv$df_post,var1,var2,rv$palettes),
                      barplot_logic_factord_stack(rv$df_post,var2,var1,rv$palettes),
                      barplot_logic_factord_dodge(rv$df_post,var2,var1,rv$palettes)
        )
        rv$table<-rv$df_post%>%
          count(.data[[var1]],.data[[var2]])%>%
          pivot_wider(names_from = var2,values_from = n)
        if(all(!is.na(rv$df_post[[var1]]))){
          rv$table<-rv$table%>%
            column_to_rownames(var1)
        }
        else{
          rv$table<-rv$table%>%select(-all_of(var1))%>%t()
          colnames(rv$table)<-c(rv$levels[[var1]],"NA")
        }
      }
      else if(rv$colTypes[var1]=="logic" &
              rv$colTypes[var2]=="numeric"){
        rv$plot<-list(density_logic_num(rv$df_post,var1,var2,rv$palettes),
                      boxplot_logic_num(rv$df_post,var1,var2,rv$palettes),
                      histogram_logic_num_stack(rv$df_post,var1,var2,rv$palettes),
                      histogram_logic_num_identity(rv$df_post,var1,var2,rv$palettes)
        )
        rv$table<-data.frame(
          do.call(
            rbind,
            tapply(
              rv$df_post%>%select(.data[[var2]])%>%pull(),
              rv$df_post%>%select(.data[[var1]])%>%pull(),
              summary)
          )
        )
      }
      else if(rv$colTypes[var1]=="factor" & !rv$ord_factor[var1] &
              rv$colTypes[var2]=="factor" & !rv$ord_factor[var2]){
        rv$plot<-list(barplot_factornonord_factornonord(rv$df_post,var1,var2,rv$palettes),
                      barplot_factornonord_factornonord(rv$df_post,var2,var1,rv$palettes)
        )
        rv$table<-rv$df_post%>%
          count(.data[[var1]],.data[[var2]])%>%
          pivot_wider(names_from = var2,values_from = n)
        if(all(!is.na(rv$df_post[[var1]]))){
          rv$table<-rv$table%>%
            column_to_rownames(var1)
        }
        else{
          rv$table<-rv$table%>%select(-all_of(var1))%>%t()
          colnames(rv$table)<-c(rv$levels[[var1]],"NA")
        }
      }
      else if(rv$colTypes[var1]=="factor" & !rv$ord_factor[var1] &
              rv$colTypes[var2]=="factor"& rv$ord_factor[var2]){
        rv$plot<-list(barplot_factornonord_factorord(rv$df_post,var1,var2,rv$palettes),
                      barplot_factorord_factornonord(rv$df_post,var1,var2,rv$palettes)
        )
        rv$table<-rv$df_post%>%
          count(.data[[var1]],.data[[var2]])%>%
          pivot_wider(names_from = var2,values_from = n)
        if(all(!is.na(rv$df_post[[var1]]))){
          rv$table<-rv$table%>%
            column_to_rownames(var1)
        }
        else{
          rv$table<-rv$table%>%select(-all_of(var1))%>%t()
          colnames(rv$table)<-c(rv$levels[[var1]],"NA")
        }
      }
      else if(rv$colTypes[var1]=="factor" & !rv$ord_factor[var1] &
              rv$colTypes[var2]=="numeric"){
        rv$plot<-list(density_factornonord_num(rv$df_post,var1,var2,rv$palettes),
                      histogram_factornonord_num_identity(rv$df_post,var1,var2,rv$palettes),
                      histogram_factornonord_num_stack(rv$df_post,var1,var2,rv$palettes),
                      boxplot_factornonord_num(rv$df_post,var1,var2,rv$palettes)
        )
        rv$table<-data.frame(
          do.call(
            rbind,
            tapply(
              rv$df_post%>%select(.data[[var2]])%>%pull(),
              rv$df_post%>%select(.data[[var1]])%>%pull(),
              summary)
          )
        )
      }
      else if(rv$colTypes[var1]=="factor" & rv$ord_factor[var1] &
              rv$colTypes[var2]=="factor" & rv$ord_factor[var2]){
        rv$plot<-list(barplot_factord_factord_dodge(rv$df_post,var1,var2,rv$palettes),
                      barplot_factord_factord_dodge(rv$df_post,var2,var1,rv$palettes),
                      barplot_factord_factord_stack(rv$df_post,var1,var2,rv$palettes),
                      barplot_factord_factord_stack(rv$df_post,var2,var1,rv$palettes)
        )
        rv$table<-rv$df_post%>%
          count(.data[[var1]],.data[[var2]])%>%
          pivot_wider(names_from = var2,values_from = n)
        if(all(!is.na(rv$df_post[[var1]]))){
          rv$table<-rv$table%>%
            column_to_rownames(var1)
        }
        else{
          rv$table<-rv$table%>%select(-all_of(var1))%>%t()
          colnames(rv$table)<-c(rv$levels[[var1]],"NA")
        }
      }
      else if(rv$colTypes[var1]=="factor" & rv$ord_factor[var1] &
              rv$colTypes[var2]=="numeric"){
        rv$plot<-list(density_factorord_num(rv$df_post,var1,var2,rv$palettes),
                      boxplot_factorord_num(rv$df_post,var1,var2,rv$palettes)
        )
        rv$table<-data.frame(
          do.call(
            rbind,
            tapply(
              rv$df_post%>%select(.data[[var2]])%>%pull(),
              rv$df_post%>%select(.data[[var1]])%>%pull(),
              summary)
          )
        )
      }
      else if(rv$colTypes[var1]=="numeric"&rv$colTypes[var2]=="numeric"){
        rv$plot<-list(scatter_num_num(rv$df_post,var1,var2,rv$palettes),
                      scatter_num_num(rv$df_post,var2,var1,rv$palettes)
        )
        rv$table<-data.frame(
          do.call(
            rbind,
            tapply(
              rv$df_post%>%
                select(all_of(c(var1,var2)))%>%
                pivot_longer(c(var1,var2))%>%
                select(value)%>%
                pull(),
              rv$df_post%>%
                select(all_of(c(var1,var2)))%>%
                pivot_longer(c(var1,var2))%>%
                select(name)%>%
                pull(),
              summary)
          )
        )
      }
    }
    else{rv$plot<-NULL}
  })
  
  # plotta descrizione
  
  # plotta grafici
  output$distPlot <- renderPlot({
    return(rv$plot[[numbers::mod(rv$plot_n, length(rv$plot))+1]])
  },bg="transparent")
  
  # printa tabella
  output$Table <- renderTable({
    return(rv$table)
  }, rownames = TRUE)
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste('plot-',paste(input$var,collapse="_"), '.png', sep='')
    },
    content = function(con) {
      ggsave(
        con,
        plot = rv$plot[[numbers::mod(rv$plot_n, length(rv$plot))+1]],
      )
    }
  )
  
  session$onSessionEnded(function() {
    stopApp()
  })
}
