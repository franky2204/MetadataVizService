barplot_logic<-function(df,var){
  plot_ly(df%>%
            count(.data[[var]]),
          y = ~.data[[var]],
          x = ~n,
          type = 'bar',
          textposition = 'auto',
          insidetextfont = list(color = '#FFFFFF'),
          hovertext = ~paste(n/ndata*100, '%',sep=""),
          hoverinfo=~text,
          text = ~n,
          marker = list(colors = palettes[var][[1]]),
          showlegend = FALSE)%>%
    layout(xaxis = list(showgrid = FALSE,
                        zeroline = FALSE,
                        showticklabels = FALSE,
                        title = ""),
           yaxis = list(showgrid = FALSE,
                        zeroline = FALSE,
                        title = ""
                        #showticklabels = FALSE
                        ))
}

barplot_logic(df,"sex")

pie_logic<-function(df,var){
  plot_ly(df%>%
            count(.data[[var]]),
          labels = ~.data[[var]],
          values = ~n,
          type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          hoverinfo = 'text',
          text = ~paste(n, 'samples'),
          marker = list(colors = palettes[var][[1]],
                        line = list(color = '#FFFFFF', width = 1)),
          showlegend = FALSE)%>%
    layout(xaxis = list(showgrid = FALSE,
                        zeroline = FALSE,
                        showticklabels = FALSE),
           yaxis = list(showgrid = FALSE,
                        zeroline = FALSE,
                        showticklabels = FALSE))
}

ggplotly(pie_logic(df,"sex"))

waffle_factor_nonord<-function(df,var){
  ggplot(df%>%count(.data[[var]]))+
    geom_waffle(
      aes(values=n,
          fill=.data[[var]]),
      n_rows = round(sqrt(ndata/golden))) +
    scale_fill_manual(name = as.character(var),
                      values = palettes[var][[1]],
                      labels = type_facets[var]) +
    coord_equal() +
    theme_void()+
    theme(
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill='transparent'),
      legend.box.background = element_rect(fill='transparent')
    )
}

barplot_factor_nonord<-function(df,var){
  ggplot(df)+
    geom_bar(
      aes(y=.data[[var]],
          fill=.data[[var]])) +
    scale_fill_manual(name = as.character(var),
                      values = palettes[var][[1]],
                      labels = type_facets[var]) +
    theme_classic()+
    theme(
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill='transparent'),
      legend.box.background = element_rect(fill='transparent')
    )
}

barplot_factor_ord_x<-function(df,var){
  ggplot(df)+
    geom_bar(aes(x=.data[[var]],
                 fill=.data[[var]]))+
    scale_fill_manual(values = palettes[var][[1]])+
    theme_classic()+
    theme(
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill='transparent'),
      legend.box.background = element_rect(fill='transparent')
    )
}

barplot_factor_ord_stack<-function(df,var){
  ggplot(df)+
    geom_bar(
      aes(x=0,
          fill=.data[[var]]),
      position = "stack",
      width=1)+
    scale_fill_manual(values = palettes[var][[1]])+
    scale_x_continuous(limits = c(-1,1))+
    theme_classic()+
    theme(
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill='transparent'),
      legend.box.background = element_rect(fill='transparent'),
      axis.text.x = element_blank()
    )
}

hist_numeric<-function(df,var){
  ggplot(df)+
    geom_histogram(aes(x=.data[[var]]))+
    theme_classic()+
    theme(
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill='transparent'),
      legend.box.background = element_rect(fill='transparent'),
      axis.text.x = element_blank()
    )
}

density_numeric<-function(df,var){
  ggplot(df)+
    geom_density(aes(x=.data[[var]]))+
    theme_classic()+
    theme(
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill='transparent'),
      legend.box.background = element_rect(fill='transparent'),
      axis.text.x = element_blank()
    )
}

boxplot_numeric<-function(df,var){
  ggplot(df)+
    geom_boxplot(aes(x=.data[[var]]))+
    theme_classic()+
    theme(
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill='transparent'),
      legend.box.background = element_rect(fill='transparent'),
      axis.text.x = element_blank()
    )
}

