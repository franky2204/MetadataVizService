change_type<-function(variable,df,levels,colTypes,ord_factor) {
  column_class <- colTypes[variable]
  if (column_class == "character") {
    new_col <- as.character(df[[variable]])
  }
  else if (column_class == "numeric") {
    new_col <- as.numeric(df[[variable]])
  }
  else if (column_class == "factor") {
    if(ord_factor[variable]){
      new_col <- factor(df[[variable]],
                               ordered=TRUE,
                               levels = levels[[variable]])
    }
    else{
      new_col <- as.factor(df[[variable]])
    }
  }
  else if (column_class == "logic") {
    if(any(is.na(as.logical(levels[[variable]])))){
      new_col <- as.factor(df[[variable]])
    }
    else{
      new_col <- as.logical(df[[variable]])
    }
  }
  return(new_col)
}

callback <- c(
    "var colnames = table.columns().header().to$().map(function() {",
    "  return this.innerHTML;",
    "}).get();",
    "Shiny.onInputChange('colnames', colnames);",
    "table.on('dblclick.dt', 'thead th', function(e) {",
    "  var $th = $(this);",
    "  var index = $th.index();",
    "  var colname = $th.text(), newcolname = colname;",
    "  var $input = $('<input type=\"text\">')",
    "  $input.val(colname);",
    "  $th.empty().append($input);",
    "  $input.on('change', function() {",
    "    newcolname = $input.val();",
    "    if(newcolname != colname){",
    "      $(table.column(index).header()).text(newcolname);",
    "      colnames[index] = newcolname;",
    "      Shiny.onInputChange('colnames', colnames);",
    "    }",
    "    $input.remove();",
    "  }).on('blur', function() {",
    "    $(table.column(index).header()).text(newcolname);",
    "    $input.remove();",
    "  });",
    "});"
  )

golden<-(1+sqrt(5))/2