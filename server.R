server <- function(input, output, session) {
  # definition of reactive variables
  rv <- reactiveValues(
    AllAlright = FALSE,
    fileUploaded = FALSE,
    df_pre = NULL,
    df_post = NULL,
    variables = NULL,
    ncol = NULL,
    colTypes = NULL,
    levels = NULL,
    ntype_ord = NULL,
    ntype_facets = NULL,
    ord_factor = NULL,
    palettes = NULL,
    plot_n = 1,
    plot = NULL,
    title = NULL,
    description = NULL
  )

  # save in the reactive value the defauts it gives to the variables
  inferColTypes <- function(df) {
    vapply(seq_len(ncol(df)), function(i) {
      col_data <- df[[i]]
      col_data_no_na <- na.omit(col_data)
      n_unique <- length(unique(col_data_no_na))

      # Prova a convertire in numerico
      as_num <- suppressWarnings(as.numeric(col_data_no_na))
      all_numeric <- all(!is.na(as_num)) && length(as_num) > 0

      has_letters <- any(grepl("[A-Za-z]", col_data_no_na))

      if (all_numeric && !has_letters) {
        "numeric"
      } else if (n_unique < 10) {
        "factor"
      } else {
        "character"
      }
    }, character(1))
  }


  # condition to show the table adjustments buttons
  output$fileUploaded <- reactive({
    return(!is.null(input$file))
  })
  outputOptions(output,
    "fileUploaded",
    suspendWhenHidden = FALSE
  )

  # load del dataframe
  observeEvent(list(input$file, input$file1, input$header, input$sep, input$quote), {
    if (req(!rv$AllAlright)) {
      req(input$file)
      df <- NULL
      ext <- tools::file_ext(input$file$name)
      if (ext == "csv" && input$manually %% 2 == 0) {
        df <- read.csv(input$file$datapath,
          header = FALSE,
          fill = TRUE,
          stringsAsFactors = FALSE
        )
        df[df == ""] <- NA
        rv$df_pre <- df
        rv$variables <- colnames(df)
        rv$ncol <- ncol(df)
        rv$levels <- vector("list", ncol(df))
        rv$colTypes <- inferColTypes(df)
      } else if (ext == "tsv" && input$manually %% 2 == 0) {
        df <- read.table(input$file$datapath,
          header = FALSE,
          # delim="\t",
          fill = TRUE,
          stringsAsFactors = FALSE
        )
        df[df == ""] <- NA

        rv$df_pre <- df
        rv$variables <- colnames(df)
        rv$ncol <- ncol(df)
        rv$levels <- vector("list", ncol(df))
        rv$colTypes <- inferColTypes(df)
      } else {
        tryCatch(
          {
            df <- read.delim(input$file$datapath,
              header = input$header,
              sep = input$sep,
              quote = input$quote
            )
            df[df == ""] <- NA

            rv$df_pre <- df
            rv$variables <- colnames(df)
            rv$ncol <- ncol(df)
            rv$levels <- vector("list", ncol(df))
            rv$colTypes <- inferColTypes(df)
          },
          warning = function(cond) {
            message("Here's the original error message:")
            message(conditionMessage(cond))
            df <- read.table(input$file$datapath,
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
      req(input$file1)
      df <- NULL
      ext <- tools::file_ext(input$file$name)
      if (ext == "csv" && input$manually %% 2 == 0) {
        df <- read.csv(input$file1$datapath,
          header = FALSE,
          fill = TRUE,
          stringsAsFactors = FALSE
        )
        df[df == ""] <- NA
        rv$df_pre <- df
        rv$variables <- colnames(df)
        rv$ncol <- ncol(df)
        rv$levels <- vector("list", ncol(df))
        rv$colTypes <- inferColTypes(df)
      } else if (ext == "tsv" && input$manually %% 2 == 0) {
        df <- read.table(input$file1$datapath,
          header = FALSE,
          # delim="\t",
          fill = TRUE,
          stringsAsFactors = FALSE
        )
        df[df == ""] <- NA

        rv$df_pre <- df
        rv$variables <- colnames(df)
        rv$ncol <- ncol(df)
        rv$levels <- vector("list", ncol(df))
        rv$colTypes <- inferColTypes(df)
      } else {
        tryCatch(
          {
            df <- read.delim(input$file1$datapath,
              header = input$header,
              sep = input$sep,
              quote = input$quote
            )
            df[df == ""] <- NA

            rv$df_pre <- df
            rv$variables <- colnames(df)
            rv$ncol <- ncol(df)
            rv$levels <- vector("list", ncol(df))
            rv$colTypes <- inferColTypes(df)
          },
          warning = function(cond) {
            message("Here's the original error message:")
            message(conditionMessage(cond))
            df <- read.table(input$file$datapath,
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
  output$df_table <- renderDataTable({
    req(rv$df_pre)

    df_display <- rv$df_pre
    df_display[is.na(df_display)] <- "NA" # converti NA in stringa per visualizzazione

    DT::datatable(
      df_display,
      callback = JS(callback),
      rownames = FALSE,
      options = list(dom = "t", ordering = FALSE),
      escape = FALSE
    )
  })




  # aggiornamento nomi variabili
  observeEvent(input$colnames, {
    if (req(!rv$AllAlright)) {
      req(input$file)
      if (all(req(input$colnames) != "")) {
        rv$variables <- input$colnames
        names(rv$colTypes) <- rv$variables
      }
    }
  })

  # crea Picker
  output$colTypeInputs <- renderUI({
    req(input$file)
    columnNames <- rv$variables
    tagList(
      lapply(1:rv$ncol, function(i) {
        col_data <- rv$df_pre[[i]]
        n_unique <- length(unique(col_data))

        # Heuristic per determinare il tipo
        col_data_no_na <- col_data[!is.na(col_data)]

        if (
          length(col_data_no_na) > 0 &&
            all(!is.na(suppressWarnings(as.numeric(col_data_no_na)))) &&
            all(!grepl("[A-Za-z]", col_data_no_na))
        ) {
          defaultEl <- "numeric"
        } else if (n_unique <= 3) {
          defaultEl <- "factor"
        } else {
          defaultEl <- "character"
        }

        # Se il tipo è factor, lascialo vuoto ("")
        selectedValue <- if (!is.null(rv$colTypes) &&
          length(rv$colTypes) >= i &&
          rv$colTypes[i] != "") {
          rv$colTypes[i]
        } else if (defaultEl == "factor") {
          ""
        } else {
          defaultEl
        }

        pickerInput(
          inputId = paste0("colType_", i),
          label = paste0(columnNames[i], ": "),
          choices = c("", "character", "numeric", "factor"),
          selected = selectedValue,
          inline = FALSE
        )
      })
    )
  })


  # aggiornamento tipi colonne
  observeEvent(
    lapply(
      1:req(rv$ncol),
      function(i) {
        return(input[[paste0("colType_", i)]])
      }
    ),
    {
      if (req(!rv$AllAlright)) {
        req(input$file)
        req(input$changed)
        last_input <- input$changed
        i <- which(last_input == sapply(
          1:rv$ncol,
          function(i) {
            return(paste0("colType_", i))
          }
        ))
        if (length(i) != 0) {
          colTypeInput <- input[[last_input]]
          rv$colTypes[i] <- colTypeInput
          if (colTypeInput == "factor") {
            var <- rv$variables[i]
            n_fact <- rv$df_pre %>%
              pull(i) %>%
              unique() %>%
              length()
            if (n_fact <= 10) {
              levels_ord <- rv$levels[[i]]
              if (is.null(levels_ord)) {
                levels_non_ord <- unique(na.omit(rv$df_pre[, i]))
              } else {
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
                    actionButton(paste0("submit_nonord_", i), "Order is not meaningful"),
                    actionButton(paste0("submit_ord_", i), "Save with this order"),
                  )
                )
              )
            } else {
              shinyalert(
                title = "Not quite the choice",
                text = paste("It appears that your column contains ",
                  n_fact,
                  " different levels when considered as a factor, whereas we can only handle a maximum of 10. \n
                                    You may have inadvertently selected the wrong column type. However, if this isn't the case and the number of values doesn't align with your expectations, consider reviewing your table for any discrepancies and reloading it. \n
                                    Please bear in mind that the system distinguishes values even if they differ only by a capitalization, spacing, or a single letter.",
                  collapse = ""
                ),
                confirmButtonCol = "#19323C",
                imageHeight = "200",
                imageWidth = "300",
                size = "m",
                imageUrl = "https://i.kym-cdn.com/entries/icons/original/000/018/489/nick-young-confused-face-300x256-nqlyaa.jpg"
              )
              rv$colTypes[i] <- ""
            }
          }
        }
      }
    }
  )

  # salva ordine factor
  observeEvent(lapply(
    1:req(rv$ncol),
    function(i) {
      return(input[[paste0("submit_ord_", i)]])
    }
  ), {
    if (req(!rv$AllAlright)) {
      req(input$file)
      last_input <- input$changed
      i <- which(last_input == sapply(
        1:rv$ncol,
        function(i) {
          return(paste0("submit_ord_", i))
        }
      ))
      if (length(i) != 0 & req(input[[paste0("submit_ord_", i)]]) != 0) {
        rv$levels[[i]] <- req(input[[paste0("factor_ord_", i)]])
        removeModal()
      }
    }
  })

  # risalva factor non ord
  observeEvent(lapply(
    1:req(rv$ncol),
    function(i) {
      return(input[[paste0("submit_nonord_", i)]])
    }
  ), {
    if (req(!rv$AllAlright)) {
      req(input$file)
      last_input <- input$changed
      i <- which(last_input == sapply(
        1:rv$ncol,
        function(i) {
          return(paste0("submit_nonord_", i))
        }
      ))
      if (length(i) != 0) {
        rv$levels[i] <- list(NULL)
        removeModal()
      }
    }
  })

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
      function(x) {
        unique(na.omit(x))
      }
    )
    rv$colTypes[rv$colTypes == "factor" & rv$ord_factor & sapply(rv$df_pre, function(v) {
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

    if (any(sapply(rv$df_post, function(col) {
      return(all(sapply(col, is.na)))
    }))) {
      wrong_type <- which(sapply(rv$df_post, function(col) {
        return(all(sapply(col, is.na)))
      }))
      shinyalert(
        title = "Are you sure?",
        text = paste("You probably have incorrectly assigned the variable type of", names(wrong_type), ". \n Please select another variable type."),
        imageUrl = "https://media.tenor.com/wy2zHeWyf2gAAAAe/side-eye-dog-suspicious-look.png",
        imageHeight = "200",
        imageWidth = "300",
        confirmButtonCol = "#19323C"
      )
      rv$AllAlright <- FALSE
      rv$levels[!rv$ord_factor] <- list(NULL)
      rv$colTypes[rv$colTypes == "logic"] <- "factor"
      rv$colTypes[names(wrong_type)] <- ""
    } else {
      rv$AllAlright <- TRUE
      rv$colTypes <- factor(rv$colTypes,
        levels = c("character", "logic", "factor", "numeric")
      )

      palettes_factord <- mapply(moma.colors,
        possible_palettes_gradient[1:sum(rv$ord_factor)],
        n = rv$ntype_ord,
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )
      if (length(rv$colTypes[rv$ord_factor & rv$colTypes == "factor"]) > 0) {
        names(palettes_factord) <- names(rv$colTypes)[rv$ord_factor]
      }

      palettes_factnonord <- mapply(moma.colors,
        possible_palettes_facets[1:sum(!rv$ord_factor[rv$colTypes == "factor"])],
        n = rv$ntype_facets,
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
      )
      if (length(rv$colTypes[!rv$ord_factor & rv$colTypes == "factor"]) > 0) {
        names(palettes_factnonord) <- names(rv$colTypes)[!rv$ord_factor & rv$colTypes == "factor"]
      }

      palettes_logic <- possible_palettes_neutral[1:sum(rv$colTypes == "logic")]
      if (length(rv$colTypes[rv$colTypes == "logic"]) > 0) {
        names(palettes_logic) <- names(rv$colTypes)[rv$colTypes == "logic"]
      }

      rv$palettes <- append(append(palettes_factnonord, palettes_factord), palettes_logic)
    }
  })

  output$AllAlright <- reactive({
    return(rv$AllAlright)
  })

  outputOptions(output,
    "AllAlright",
    suspendWhenHidden = FALSE
  )

  # crea le variabili da mostrare per la scelta
  output$variables_input <- renderUI({
    if (input$load != 0) {
      req(rv$variables, rv$colTypes)
      tagList(
        checkboxGroupInput(
          inputId = "var",
          label = "Chose the variables you want to explore",
          choices = rv$variables[rv$colTypes != "character"],
          inline = FALSE
        )
      )
    }
  })

  # aggiorna il numero del plot da mostrare
  observeEvent(input$refresh, {
    rv$plot_n <- rv$plot_n + 1
  })

  # aggiorna le variabili da mostrare
  observeEvent(input$var, {
    var <- names(sort(rv$colTypes[input$var]))
    var[rv$colTypes[var] == "factor"] <- names(sort(rv$ord_factor[names(rv$colTypes[var][rv$colTypes[var] == "factor"])]))
    if (length(var) == 1) {
      var <- as.character(input$var[1])

      if (rv$colTypes[var] == "logic") {
        rv$plot <- list(
          barplot_logic(rv$df_post, var, rv$palettes),
          pie_logic(rv$df_post, var, rv$palettes)
        )
        rv$table <- rv$df_post %>% count(.data[[var]])
        if (all(!is.na(rv$df_post[[var]]))) {
          rv$table <- rv$table %>%
            column_to_rownames(var) %>%
            t()
          rownames(rv$table) <- "Number \n of samples"
        } else {
          rv$table <- rv$table %>%
            filter(n) %>%
            t()
          colnames(rv$table) <- c(rv$levels[[var]], "NA")
          rownames(rv$table) <- "Number \n of samples"
        }
        rv$title <- list(
          paste("Bar plot showing the distribution of data by", var),
          paste("Pie chart showing the distribution of data by", var)
        )
        rv$description <- list(
          "The height of each bar corresponds to the number of samples contained in the group. Choose barplots for precise comparisons and pie charts for illustrating proportions in a whole.",
          "Each slice represents a category and its size is proportionial to the number of samples contained in the group. Choose barplots for precise comparisons and pie charts for illustrating proportions in a whole."
        )
      } else if (rv$colTypes[var] == "factor" & !rv$ord_factor[var]) {
        rv$plot <- list(
          waffle_factor_nonord(rv$df_post, var, rv$palettes, rv$ndata, rv$levels),
          barplot_factor_nonord(rv$df_post, var, rv$palettes, rv$levels)
        )
        rv$table <- rv$df_post %>% count(.data[[var]])
        if (all(!is.na(rv$df_post[[var]]))) {
          rv$table <- rv$table %>%
            column_to_rownames(var) %>%
            t()
          rownames(rv$table) <- "Number \n of samples"
        } else {
          rv$table <- rv$table %>%
            dplyr::select(n) %>%
            t()
          colnames(rv$table) <- c(rv$levels[[var]], "NA")
          rownames(rv$table) <- "Number \n of samples"
        }
        rv$title <- list(
          paste("Waffle chart showing the distribution of data subdivided by", var),
          paste("Bar plot showing the distribution of data subdivided by", var)
        )
        rv$description <- list(
          "Each square represents a unit and the colors are assigned according to the belonging to groups of samples, making it easy to see proportions at a glance.",
          "The length of each bar corresponds to the number of samples contained in the group. Choose barplots for precise comparisons among groups."
        )
      } else if (rv$colTypes[var] == "factor" & rv$ord_factor[var]) {
        rv$plot <- list(
          barplot_factor_ord_x(rv$df_post, var, rv$palettes),
          barplot_factor_ord_stack(rv$df_post, var, rv$palettes)
        )
        rv$table <- rv$df_post %>% count(.data[[var]])
        if (all(!is.na(rv$df_post[[var]]))) {
          rv$table <- rv$table %>%
            column_to_rownames(var) %>%
            t()
          rownames(rv$table) <- "Number \n of samples"
        } else {
          rv$table <- rv$table %>%
            dplyr::select(n) %>%
            t()
          colnames(rv$table) <- c(rv$levels[[var]], "NA")
          rownames(rv$table) <- "Number \n of samples"
        }
        rv$title <- list(
          paste("Bar plot showing the distribution of data subdivided by", var),
          paste("Bar plot showing the distribution of data subdivided by", var, "stucked")
        )
        rv$description <- list(
          "The height of each bar corresponds to the number of samples contained in the group. Choose side-by-syde barplots for precise comparisons among groups.",
          "The height of each bar corresponds to the number of samples contained in the group. Choose stucked barplots to show proportions between groups."
        )
      } else if (rv$colTypes[var] == "numeric") {
        rv$plot <- list(
          hist_numeric(rv$df_post, var),
          density_numeric(rv$df_post, var, rv$palettes),
          boxplot_numeric(rv$df_post, var, rv$palettes)
        )
        rv$table <- data.frame(
          as.array(
            summary(rv$df_post %>% dplyr::select(.data[[var]]) %>% pull())
          )
        ) %>%
          column_to_rownames("Var1") %>%
          t()
        rownames(rv$table) <- "values"
        rv$title <- list(
          paste("Histogram of", var, "values"),
          paste("Density of", var, "values"),
          paste("Boxplot of", var, "values")
        )
        rv$description <- list(
          "Each bar's height reflects the number of samples with values lying in that bin. If values are discrete, histograms are ideal, expecially for smaller sample sizes, since they show a more accurate representation of the distribution.",
          "Density plots provide a smoothed estimate of the distribution of the values between samples, especially useful with larger datasets. If you need to see the nuances of the distribution, such as small peaks and valleys, density plots are preferable.",
          "Concise summary of a dataset's distribution, focusing on central tendency and variability. Median, quartiles, and potential outliers can be seen."
        )
      }
    } else if (length(var) == 2) {
      var1 <- as.character(var[1])
      var2 <- as.character(var[2])

      if (rv$colTypes[var1] == "logic" & rv$colTypes[var2] == "logic") {
        rv$plot <- list(
          barplot_logic_logic(rv$df_post, var1, var2, rv$palettes),
          barplot_logic_logic(rv$df_post, var2, var1, rv$palettes)
        )
        rv$table <- rv$df_post %>%
          count(.data[[var1]], .data[[var2]]) %>%
          pivot_wider(names_from = var2, values_from = n)

        if (all(!is.na(rv$df_post[[var1]]))) {
          rv$table <- rv$table %>%
            column_to_rownames(var1)
        } else {
          rv$table <- rv$table %>%
            dplyr::select(-all_of(var1)) %>%
            t()
          colnames(rv$table) <- c(rv$levels[[var1]], "NA")
        }
        rv$title <- list(
          paste("Barplot of", var1, "colored according to", var2),
          paste("Barplot of", var2, "colored according to", var1)
        )
        rv$description <- list(
          paste("The plot represents the number of samples in each factor of", var1, "divided in two colors according to", var2),
          paste("The plot represents the number of samples in each factor of", var2, "divided in two colors according to", var1)
        )
      } else if (rv$colTypes[var1] == "logic" &
        rv$colTypes[var2] == "factor" & !rv$ord_factor[var2]) {
        rv$plot <- list(
          barplot_logic_factnonord_stack(rv$df_post, var1, var2, rv$palettes),
          barplot_logic_factnonord_dodge(rv$df_post, var1, var2, rv$palettes),
          barplot_logic_factnonord_stack(rv$df_post, var2, var1, rv$palettes),
          barplot_logic_factnonord_dodge(rv$df_post, var2, var1, rv$palettes)
        )
        rv$table <- rv$df_post %>%
          count(.data[[var1]], .data[[var2]]) %>%
          pivot_wider(names_from = var2, values_from = n)
        if (all(!is.na(rv$df_post[[var1]]))) {
          rv$table <- rv$table %>%
            column_to_rownames(var1)
        } else {
          rv$table <- rv$table %>%
            dplyr::select(-all_of(var1)) %>%
            t()
          colnames(rv$table) <- c(rv$levels[[var1]], "NA")
        }
        rv$title <- list(
          paste("Barplot of", var1, "colored according to", var2, "stacked"),
          paste("Barplot of", var1, "colored according to", var2, "dodged"),
          paste("Barplot of", var2, "colored according to", var1, "stacked"),
          paste("Barplot of", var2, "colored according to", var1, "dodged")
        )
        rv$description <- list(
          paste("The plot represents the number of samples in each factor of", var1, "divided in two colors according to", var2),
          paste("The plot represents the number of samples in each factor of", var1, "divided in two colors according to", var2),
          paste("The plot represents the number of samples in each factor of", var2, "divided in two colors according to", var1),
          paste("The plot represents the number of samples in each factor of", var2, "divided in two colors according to", var1)
        )
      } else if (rv$colTypes[var1] == "logic" &
        rv$colTypes[var2] == "factor" & rv$ord_factor[var2]) {
        rv$plot <- list(
          barplot_logic_factord_stack(rv$df_post, var1, var2, rv$palettes),
          barplot_logic_factord_dodge(rv$df_post, var1, var2, rv$palettes),
          barplot_logic_factord_stack(rv$df_post, var2, var1, rv$palettes),
          barplot_logic_factord_dodge(rv$df_post, var2, var1, rv$palettes)
        )
        rv$table <- rv$df_post %>%
          count(.data[[var1]], .data[[var2]]) %>%
          pivot_wider(names_from = var2, values_from = n)
        if (all(!is.na(rv$df_post[[var1]]))) {
          rv$table <- rv$table %>%
            column_to_rownames(var1)
        } else {
          rv$table <- rv$table %>%
            dplyr::select(-all_of(var1)) %>%
            t()
          colnames(rv$table) <- c(rv$levels[[var1]], "NA")
        }
        rv$title <- list(
          paste("Barplot of", var1, "colored according to", var2, "stacked"),
          paste("Barplot of", var1, "colored according to", var2, "dodged"),
          paste("Barplot of", var2, "colored according to", var1, "stacked"),
          paste("Barplot of", var2, "colored according to", var1, "dodged")
        )
      } else if (rv$colTypes[var1] == "logic" &
        rv$colTypes[var2] == "numeric") {
        rv$plot <- list(
          density_logic_num(rv$df_post, var1, var2, rv$palettes),
          boxplot_logic_num(rv$df_post, var1, var2, rv$palettes),
          histogram_logic_num_stack(rv$df_post, var1, var2, rv$palettes),
          histogram_logic_num_identity(rv$df_post, var1, var2, rv$palettes)
        )
        rv$table <- data.frame(
          do.call(
            rbind,
            tapply(
              rv$df_post %>% dplyr::select(.data[[var2]]) %>% pull(),
              rv$df_post %>% dplyr::select(.data[[var1]]) %>% pull(),
              summary
            )
          )
        )
        rv$title <- list(
          paste("Ridgeline plot of", var2, "according to", var1),
          paste("Boxplots of", var2, "subdivided according to", var1),
          paste("Histograms of", var2, "colored according to", var1, "stucked"),
          paste("Histograms of", var2, "subdivided and colored according to", var1)
        )
      } else if (rv$colTypes[var1] == "factor" & !rv$ord_factor[var1] &
        rv$colTypes[var2] == "factor" & !rv$ord_factor[var2]) {
        rv$plot <- list(
          barplot_factornonord_factornonord(rv$df_post, var1, var2, rv$palettes),
          barplot_factornonord_factornonord(rv$df_post, var2, var1, rv$palettes)
        )
        rv$title <- list(
          paste("Barplot of", var1, "colored according to", var2),
          paste("Barplot of", var2, "colored according to", var1)
        )
        rv$table <- rv$df_post %>%
          count(.data[[var1]], .data[[var2]]) %>%
          pivot_wider(names_from = var2, values_from = n)
        if (all(!is.na(rv$df_post[[var1]]))) {
          rv$table <- rv$table %>%
            column_to_rownames(var1)
        } else {
          rv$table <- rv$table %>%
            dplyr::select(-all_of(var1)) %>%
            t()
          colnames(rv$table) <- c(rv$levels[[var1]], "NA")
        }
      } else if (rv$colTypes[var1] == "factor" & !rv$ord_factor[var1] &
        rv$colTypes[var2] == "factor" & rv$ord_factor[var2]) {
        rv$plot <- list(
          barplot_factornonord_factorord(rv$df_post, var1, var2, rv$palettes),
          barplot_factorord_factornonord(rv$df_post, var1, var2, rv$palettes)
        )
        rv$title <- list(
          paste("Barplot of", var1, "colored according to", var2, "stacked"),
          paste("Barplot of", var2, "colored according to", var1, "stacked")
        )
        rv$table <- rv$df_post %>%
          count(.data[[var1]], .data[[var2]]) %>%
          pivot_wider(names_from = var2, values_from = n)
        if (all(!is.na(rv$df_post[[var1]]))) {
          rv$table <- rv$table %>%
            column_to_rownames(var1)
        } else {
          rv$table <- rv$table %>%
            dplyr::select(-all_of(var1)) %>%
            t()
          colnames(rv$table) <- c(rv$levels[[var1]], "NA")
        }
      } else if (rv$colTypes[var1] == "factor" & !rv$ord_factor[var1] &
        rv$colTypes[var2] == "numeric") {
        rv$plot <- list(
          density_factornonord_num(rv$df_post, var1, var2, rv$palettes),
          histogram_factornonord_num_identity(rv$df_post, var1, var2, rv$palettes),
          histogram_factornonord_num_stack(rv$df_post, var1, var2, rv$palettes),
          boxplot_factornonord_num(rv$df_post, var1, var2, rv$palettes)
        )
        rv$title <- list(
          paste("Ridgeline plot of", var2, "according to", var1),
          paste("Histograms of", var2, "subdivided and colored according to", var1),
          paste("Histograms of", var2, "colored according to", var1, "stacked"),
          paste("Boxplots of", var2, "subdivided according to", var1)
        )
        rv$table <- data.frame(
          do.call(
            rbind,
            tapply(
              rv$df_post %>% dplyr::select(.data[[var2]]) %>% pull(),
              rv$df_post %>% dplyr::select(.data[[var1]]) %>% pull(),
              summary
            )
          )
        )
      } else if (rv$colTypes[var1] == "factor" & rv$ord_factor[var1] &
        rv$colTypes[var2] == "factor" & rv$ord_factor[var2]) {
        rv$plot <- list(
          barplot_factord_factord_dodge(rv$df_post, var1, var2, rv$palettes),
          barplot_factord_factord_dodge(rv$df_post, var2, var1, rv$palettes),
          barplot_factord_factord_stack(rv$df_post, var1, var2, rv$palettes),
          barplot_factord_factord_stack(rv$df_post, var2, var1, rv$palettes)
        )
        rv$title <- list(
          paste("Barplot of", var1, "colored according to", var2, "dodged"),
          paste("Barplot of", var2, "colored according to", var1, "dodged"),
          paste("Barplot of", var1, "colored according to", var2, "stacked"),
          paste("Barplot of", var2, "colored according to", var1, "stacked")
        )
        rv$table <- rv$df_post %>%
          count(.data[[var1]], .data[[var2]]) %>%
          pivot_wider(names_from = var2, values_from = n)
        if (all(!is.na(rv$df_post[[var1]]))) {
          rv$table <- rv$table %>%
            column_to_rownames(var1)
        } else {
          rv$table <- rv$table %>%
            dplyr::select(-all_of(var1)) %>%
            t()
          colnames(rv$table) <- c(rv$levels[[var1]], "NA")
        }
      } else if (rv$colTypes[var1] == "factor" & rv$ord_factor[var1] &
        rv$colTypes[var2] == "numeric") {
        rv$plot <- list(
          density_factorord_num(rv$df_post, var1, var2, rv$palettes),
          boxplot_factorord_num(rv$df_post, var1, var2, rv$palettes)
        )
        rv$title <- list(
          paste("Ridgeline plot of", var2, "according to", var1),
          paste("Boxplots of", var2, "subdivided according to", var1)
        )
        rv$table <- data.frame(
          do.call(
            rbind,
            tapply(
              rv$df_post %>% dplyr::select(.data[[var2]]) %>% pull(),
              rv$df_post %>% dplyr::select(.data[[var1]]) %>% pull(),
              summary
            )
          )
        )
      } else if (rv$colTypes[var1] == "numeric" & rv$colTypes[var2] == "numeric") {
        rv$plot <- list(
          scatter_num_num(rv$df_post, var1, var2, rv$palettes),
          scatter_num_num(rv$df_post, var2, var1, rv$palettes)
        )
        rv$title <- list(
          paste("Scatterplot of", var2, "vs", var1),
          paste("Scatterplot of", var1, "vs", var2)
        )
        rv$table <- data.frame(
          do.call(
            rbind,
            tapply(
              rv$df_post %>%
                dplyr::select(all_of(c(var1, var2))) %>%
                pivot_longer(c(var1, var2)) %>%
                dplyr::select(value) %>%
                pull(),
              rv$df_post %>%
                dplyr::select(all_of(c(var1, var2))) %>%
                pivot_longer(c(var1, var2)) %>%
                dplyr::select(name) %>%
                pull(),
              summary
            )
          )
        )
      }
    } else {
      rv$plot <- NULL
      rv$title <- NULL
      rv$table <- NULL
    }
  })

  # scrive titolo
  output$Title <- renderText({
    return(rv$title[[numbers::mod(rv$plot_n, length(rv$title)) + 1]])
  })

  # scrive descrizione
  output$Description <- renderText({
    return(rv$description[[numbers::mod(rv$plot_n, length(rv$plot)) + 1]])
  })

  # plotta grafici
  output$distPlot <- renderPlot(
    {
      return(rv$plot[[numbers::mod(rv$plot_n, length(rv$plot)) + 1]])
    },
    bg = "transparent"
  )

  # plotta grafico dopo
  output$distPlot_next <- renderPlot(
    {
      return(rv$plot[[numbers::mod((rv$plot_n + 1), length(rv$plot)) + 1]] +
        theme(
          legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank()
        ))
    },
    bg = "transparent"
  )

  # printa tabella
  output$Table <- renderTable(
    {
      return(rv$table)
    },
    rownames = TRUE
  )


  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot-", paste(input$var, collapse = "_"), ".png", sep = "")
    },
    content = function(con) {
      ggsave(
        con,
        plot = rv$plot[[numbers::mod(rv$plot_n, length(rv$plot)) + 1]],
      )
    }
  )

  session$onSessionEnded(function() {
    stopApp()
  })
}
