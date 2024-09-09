create_boxplot <- function(data, x_var, y_var, col, alpha, size, ylab_text, ggtitle_text) {
  p <- ggplot(data,aes_string(x = x_var, y = y_var, fill = x_var)) +
     geom_boxplot(color = "black", lwd = 1, alpha = alpha) +
    ggbeeswarm::geom_beeswarm(aes(text = name), dodge.width = 0.1,
                               cex = 0.1,
                               shape = 21,
                               size = size) +
    ylab(ylab_text) +
    ggtitle(paste(ggtitle_text, "", sep = "")) +
    scale_fill_manual(values = unlist(col))
  p

}

process_boxplot <- function(input,yval,dataa,col) {
  
  y <- yval %>% as.character() 
  p <- create_boxplot(dataa, input$w, y, col, input$alpha, input$size, input$yAxisLabel, y)
  if (input$q != "" && input$pvaluecheck == TRUE) {
    tukey_result <-  process_tukey_result(input,y,input$q,p,dataa,0.05,input$Fontsize)
    p <- p + add_pvalue(tukey_result, remove.bracket = TRUE, bracket.size = 0.000001, label = "{symbol}", tip.length = 0,
                        label.size = tukey_result$size, inherit.aes = FALSE,y.position = tukey_result$y.position_boxplot) + scale_y_continuous(
                          limits = c(min(tukey_result$y.min.position), (max(tukey_result$y.position_boxplot) + max(tukey_result$y.position_boxplot) / 30))
                        )
  }
  p  <- p + theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.x = element_blank(),
    aspect.ratio = 0.75,
    text = element_text(size = input$Fontsize)
  )
  ggplotly(p,tooltip = c("name","x"))
}

process_boxplot_diagram <- function(input,yval,dataa,col) {
  
    y <- yval %>% as.character()
    p <- create_boxplot(dataa, input$w, y, col, input$alpha, input$size, input$yAxisLabel, y)
    p <- p + theme_prism(
      base_fontface = "plain",
      base_line_size = 0.9,
      base_family = "Arial"
    ) +
      scale_x_discrete(
        guide = guide_prism_bracket(width = 0.1),
        labels = scales::wrap_format(5)
      ) +
      theme(
        legend.position = "none",
        axis.title.x = element_blank(),
        aspect.ratio = 1.0,
        text = element_text(size = input$Fontsize)
      )
    if (input$q != "" && input$pvaluecheck == TRUE) {
      tukey_result <-  process_tukey_result(input,y,input$q,p,dataa,0.05,input$Fontsize)
      p <- p + add_pvalue(tukey_result, remove.bracket = TRUE, bracket.size = 0.000001, label = "{symbol}", tip.length = 0,
                          label.size = tukey_result$size, inherit.aes = FALSE,y.position = tukey_result$y.position_boxplot) + scale_y_continuous(
                            limits = c(min(dataa[[y]]), (max(tukey_result$y.position_boxplot) + max(tukey_result$y.position_boxplot) / 30))
                          )
    }
    p
}

process_action_boxplot <- function(input,dataa,metainfocol,svg_path,col,output) {
  
  withProgress(message = 'Calculation in progress', detail = 'This may take a while...', value = 1, {
    startnum <- 1 + metainfocol
    for (i in startnum:ncol(dataa)) {
      yval <- colnames(dataa)[i]
      p <- create_boxplot(dataa, input$w, dataa[[yval]], col, input$alpha, input$size, input$yAxisLabel, "")
      p <- p + theme_prism(
        base_fontface = "plain",
        base_line_size = 0.9,
        base_family = "Arial"
      ) +
        scale_x_discrete(
          guide = guide_prism_bracket(width = 0.1),
          labels = scales::wrap_format(5)
        ) +
        theme(
          legend.position = "none",
          axis.title.x = element_blank(),
          aspect.ratio = 1.0,
          text = element_text(size = input$Fontsize)
        )
      if (input$q != "" && input$pvaluecheck == TRUE) {
        tukey_result <-  process_tukey_result(input,yval,input$q,p,dataa,0.05,input$Fontsize)
        p <- p + add_pvalue(tukey_result, remove.bracket = TRUE, bracket.size = 0.000001, label = "{symbol}", tip.length = 0,
                            label.size = tukey_result$size, inherit.aes = FALSE,y.position = tukey_result$y.position_boxplot) + scale_y_continuous(
                              limits = c(min(dataa[[yval]]), (max(tukey_result$y.position_boxplot) + max(tukey_result$y.position_boxplot) / 30))
                            )
      }
      svg(paste("./svg/",colnames(dataa)[i], ".svg", sep = ""))
      plot(p)
      dev.off()
      incProgress(1 / ncol(dataa))
      Sys.sleep(0.25)
    }
  })
  data.frame(name = colnames(dataa)[-c(1:metainfocol)], path = paste("file:/", svg_path, "/", colnames(dataa)[-c(1:metainfocol)], ".svg", sep = "")) %>% write.csv("./svg/path.csv")
  output$testtext1 <- renderText({
    "Finish"
  })
}

create_barplot <- function(data, x_var, y_var, col, alpha, size, ylab_text, ggtitle_text) {
  p <- ggplot(data, aes_string(x = x_var, y = y_var, fill = x_var)) +
    geom_bar(color = "black", stat = "summary",fun = "mean",lwd = 1.2,width = 0.6) +
    ylab(ylab_text) +
    ggtitle(paste(ggtitle_text, "", sep = "")) +
    scale_fill_manual(values = unlist(col))
  
  p
}

process_barplot <- function(input,yval,dataa,col) {
  y <- yval %>% as.character()
  p <- create_barplot(dataa, input$w, y, col, input$alpha, input$size, input$yAxisLabel, y)
  if (input$q != "" && input$pvaluecheck == TRUE) {
    tukey_result <-  process_tukey_result(input,y,input$q,p,dataa,0.05,input$Fontsize)
    p <- p + add_pvalue(tukey_result,remove.bracket = TRUE,
                        bracket.size = 0.000001,
                        label = "{symbol}",
                        tip.length = 0,
                        label.size = tukey_result$size,inherit.aes = FALSE,y.position = tukey_result$y.position_barplot)
  }
  p  <- p + theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.x = element_blank(),
    aspect.ratio = 0.75,
    text = element_text(size = input$Fontsize)
  )
  ggplotly(p)
}


process_barplot_diagram <- function(input,yval,dataa,col) {
  
  y <- yval %>% as.character()
  p <- create_barplot(dataa, input$w, y, col, input$alpha, input$size, input$yAxisLabel, y)
  p <- p + theme_prism(
    base_fontface = "plain",
    base_line_size = 0.9,
    base_family = "Arial") +
    scale_x_discrete(
      guide = guide_prism_bracket(width = 0.1),
      labels = scales::wrap_format(5)) +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      aspect.ratio = 1.0,
      text = element_text(size = input$Fontsize))
  if (input$q != "" && input$pvaluecheck == TRUE) {
    tukey_result <-  process_tukey_result(input,y,input$q,p,dataa,0.05,input$Fontsize)
    p <- p + add_pvalue(tukey_result,remove.bracket = TRUE,
                        bracket.size = 0.000001,
                        label = "{symbol}",
                        tip.length = 0,
                        label.size = tukey_result$size,inherit.aes = FALSE,y.position = tukey_result$y.position_barplot)
  }
  p
}



process_action_barplot <- function(input,dataa,metainfocol,svg_path,col,output) {
  withProgress(message = 'Calculation in progress', detail = 'This may take a while...', value = 1, {
    startnum <- 1 + metainfocol
    for (i in startnum:ncol(dataa)) {
      yval <- colnames(dataa)[i]
      p <- create_barplot(dataa, input$w, dataa[[yval]], col, input$alpha, input$size, input$yAxisLabel, "")
      p <- p + theme_prism(
        base_fontface = "plain",
        base_line_size = 0.9,
        base_family = "Arial") +
        scale_x_discrete(
          guide = guide_prism_bracket(width = 0.1),
          labels = scales::wrap_format(5)) +
        theme(
          legend.position = "none",
          axis.title.x = element_blank(),
          aspect.ratio = 1.0,
          text = element_text(size = input$Fontsize))
      if (input$q!= "" && input$pvaluecheck == TRUE) {
        tukey_result <-  process_tukey_result(input,yval,input$q,p,dataa,0.05,input$Fontsize)
        p <- p + add_pvalue(tukey_result,remove.bracket = TRUE,
                            bracket.size = 0.000001,
                            label = "{symbol}",
                            tip.length = 0,
                            label.size = tukey_result$size,inherit.aes = FALSE,y.position = tukey_result$y.position_barplot) 
      }
      svg(paste("./svg/",colnames(dataa)[i], ".svg", sep = ""))
      plot(p)
      dev.off()
    }
	      incProgress(1 / ncol(dataa))
      Sys.sleep(0.25)
  })
  data.frame(name = colnames(dataa)[-c(1:metainfocol)], path = paste("file:/", svg_path, "/", colnames(dataa)[-c(1:metainfocol)], ".svg", sep = "")) %>% write.csv("./svg/path.csv")
  output$testtext1 <- renderText({
    "Finish"
  })
}

process_violinplot <- function(input,yval,dataa,col) {
  
  y <- yval %>% as.character()
  p <- create_violinplot(dataa, input$w, y, col, input$alpha, input$size, input$yAxisLabel, y)
  if (input$q != "" &&input$pvaluecheck == TRUE) {
    tukey_result <-  process_tukey_result(input,y,input$q,p,dataa,0.05,input$Fontsize)
    p <- p + add_pvalue(tukey_result, remove.bracket = TRUE, bracket.size = 0.000001, label = "{symbol}", tip.length = 0,
                        label.size = tukey_result$size, inherit.aes = FALSE,y.position = tukey_result$y.position_boxplot) + scale_y_continuous(
                          limits = c(min(dataa[[y]]), (max(tukey_result$y.position_boxplot) + max(tukey_result$y.position_boxplot) / 30))
                        ) 
  }
  p  <- p + theme(
    #legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.title.x = element_blank(),
    aspect.ratio = 0.75,
    text = element_text(size = input$Fontsize)
  )
  ggplotly(p)
  # }
}


create_violinplot <- function(data, x_var, y_var, col, alpha, size, ylab_text, ggtitle_text) {
  p <- ggplot(data, aes_string(x = x_var, y = y_var, fill = x_var)) +
    geom_violin(adjust = 2,trim = FALSE) +
    ylab(ylab_text) +
    ggtitle(paste(ggtitle_text, "", sep = "")) +
    scale_fill_manual(values = unlist(col))+
    stat_summary(fun = "mean",geom = "crossbar",color = "black",linewidth = 0.5, width = 0.5)
  
  p
}

process_violinplot_diagram <- function(input,yval,dataa,col) {

  y <- yval %>% as.character()
  p <- create_violinplot(dataa, input$w, y, col, input$alpha, input$size, input$yAxisLabel, y)
  p <- p + theme_prism(
    base_fontface = "plain",
    base_line_size = 0.9,
    base_family = "Arial") +
    scale_x_discrete(
      guide = guide_prism_bracket(width = 0.1),
      labels = scales::wrap_format(5)) +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      aspect.ratio = 1.0,
      text = element_text(size = input$Fontsize))
  if (input$q != "" &&input$pvaluecheck == TRUE) {
    tukey_result <-  process_tukey_result(input,y,input$q,p,dataa,0.05,input$Fontsize)
    p <- p + add_pvalue(tukey_result, remove.bracket = TRUE, bracket.size = 0.000001, label = "{symbol}", tip.length = 0,
                        label.size = tukey_result$size, inherit.aes = FALSE,y.position = tukey_result$y.position_boxplot) + scale_y_continuous(
                          limits = c(min(dataa[[y]]), (max(tukey_result$y.position_boxplot) + max(tukey_result$y.position_boxplot) / 30))
                        ) 
  }
  p
}

process_action_violinplot <- function(input,dataa,metainfocol,svg_path,col,output) {
  withProgress(message = 'Calculation in progress', detail = 'This may take a while...', value = 1, {
    startnum <- 1 + metainfocol
    for (i in startnum:ncol(dataa)) {
      yval <- colnames(dataa)[i]
      p <- create_violinplot(dataa, input$w, dataa[[yval]], col, input$alpha, input$size, input$yAxisLabel, "")
      p <- p + theme_prism(
        base_fontface = "plain",
        base_line_size = 0.9,
        base_family = "Arial") +
        scale_x_discrete(
          guide = guide_prism_bracket(width = 0.1),
          labels = scales::wrap_format(5)) +
        theme(
          legend.position = "none",
          axis.title.x = element_blank(),
          aspect.ratio = 1.0,
          text = element_text(size = input$Fontsize))
      if (input$q!= "" && input$pvaluecheck == TRUE) {
        tukey_result <-  process_tukey_result(input,yval,input$q,p,dataa,0.05,input$Fontsize)
        p <- p + add_pvalue(tukey_result, remove.bracket = TRUE, bracket.size = 0.000001, label = "{symbol}", tip.length = 0,
                            label.size = tukey_result$size, inherit.aes = FALSE,y.position = tukey_result$y.position_boxplot) + scale_y_continuous(
                              limits = c(min(dataa[[yval]]), (max(tukey_result$y.position_boxplot) + max(tukey_result$y.position_boxplot) / 30))
                            )
      }
      svg(paste("./svg/",colnames(dataa)[i], ".svg", sep = ""))
      plot(p)
      dev.off()
    }
    incProgress(1 / ncol(dataa))
    Sys.sleep(0.25)
  })
  data.frame(name = colnames(dataa)[-c(1:metainfocol)], path = paste("file:/", svg_path, "/", colnames(dataa)[-c(1:metainfocol)], ".svg", sep = "")) %>% write.csv("./svg/path.csv")
  output$testtext1 <- renderText({
    "Finish"
  })
}
process_polarplot <- function(input,yval,dataa,col) {
  
  # if(is.na(unique(select(dataa,input$w))) != TRUE){
  y <- yval %>% as.character()
  p <- create_polarplot(dataa, input$w, y, col, input$alpha, input$size, input$yAxisLabel, y, input$Fontsize)
  if (input$q != "" && input$pvaluecheck == TRUE) {
    tukey_result <-  process_tukey_result(input,y,input$q,p,dataa,0.05,input$Fontsize)
    p <- p + add_pvalue(tukey_result,remove.bracket = TRUE,
                        bracket.size = 0.000001,
                        label = "{symbol}",
                        tip.length = 0,
                        label.size = tukey_result$size,inherit.aes = FALSE,y.position = tukey_result$y.position_barplot)
  }
  plot(p)
  # }
}

create_polarplot <- function(data, x_var, y_var, col, alpha, size, ylab_text, ggtitle_text, Fontsize) {
  p <- ggplot(data, aes_string(x = x_var, y = y_var, fill = x_var)) +
    geom_bar(color = "black", stat = "summary",fun = "mean",lwd = 1.2,width = 0.6) +
    coord_polar()+
    scale_fill_manual(values = unlist(col))+
    labs(
      x="", 
      y="")+
    theme_minimal(10) +
    theme(
      axis.text.x = element_text(size = Fontsize, angle = 360,hjust=0.8),
      axis.title.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.y = element_blank())+
    easy_remove_axes()
  
  return(p)
}



process_dotplot <- function(input,yval,dataa,col) {
  y <- yval %>% as.character()
  p <- create_dotplot(dataa, input$w, y, col, input$alpha, input$size, input$yAxisLabel, y)
  if (input$q != "" && input$pvaluecheck == TRUE) {
    tukey_result <-  process_tukey_result(input,y,input$q,p,dataa,0.05,input$Fontsize)
    p <- p + add_pvalue(tukey_result, remove.bracket = TRUE, bracket.size = 0.000001, label = "{symbol}", tip.length = 0,
                        label.size = tukey_result$size, inherit.aes = FALSE,y.position = tukey_result$y.position_boxplot) + scale_y_continuous(
                          limits = c(min(tukey_result$y.min.position), (max(tukey_result$y.position_boxplot) + max(tukey_result$y.position_boxplot) / 30))
                        ) 
  }
  p <- p + theme(
    axis.title.x = element_blank(),
    aspect.ratio = 0.2,
    axis.text.x = element_text(angle = 90, hjust = 1),
    text = element_text(size = input$Fontsize)
  )
  ggplotly(p,tooltip = c("name","x"))
}


create_dotplot <- function(data, x_var, y_var, col, alpha, size, ylab_text, ggtitle_text) {
  p <- ggplot(data,aes_string(x = x_var, y = y_var, fill = x_var)) +
    ggbeeswarm::geom_beeswarm(aes(text = name),
                              cex = 0.1,
                              shape = 21,
                              size = size)+stat_summary(fun = "mean",geom = "crossbar",color = "red",linewidth = 0.5, width = 0.5)+
    ylab(ylab_text) +
    ggtitle(paste(ggtitle_text, "", sep = "")) +
    scale_fill_manual(values = unlist(col)) 
  p
}

process_dotplot_diagram <- function(input,yval,dataa,col) {
  
  
  y <- yval %>% as.character()
  p <- create_dotplot(dataa, input$w, y, col, input$alpha, input$size, input$yAxisLabel, y)
  p <- p + theme_prism(
    base_fontface = "plain",
    base_line_size = 0.9,
    base_family = "Arial"
  ) +
    scale_x_discrete(
      guide = guide_prism_bracket(width = 0.1),
      labels = scales::wrap_format(5)
    ) +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      aspect.ratio = 1.0,
      text = element_text(size = input$Fontsize)
    )
  if (input$q != "" && input$pvaluecheck == TRUE) {
    tukey_result <-  process_tukey_result(input,y,input$q,p,dataa,0.05,input$Fontsize)
    p <- p + add_pvalue(tukey_result, remove.bracket = TRUE, bracket.size = 0.000001, label = "{symbol}", tip.length = 0,
                        label.size = tukey_result$size, inherit.aes = FALSE,y.position = tukey_result$y.position_boxplot) + scale_y_continuous(
                          limits = c(min(dataa[[y]]), (max(tukey_result$y.position_boxplot) + max(tukey_result$y.position_boxplot) / 30))
                        )
  }
  p
}

process_action_dotplot <- function(input,dataa,metainfocol,svg_path,col,output) {
  
  withProgress(message = 'Calculation in progress', detail = 'This may take a while...', value = 1, {
    startnum <- 1 + metainfocol
    for (i in startnum:ncol(dataa)) {
      yval <- colnames(dataa)[i]
      p <- create_dotplot(dataa, input$w, dataa[[yval]], col, input$alpha, input$size, input$yAxisLabel, "")
      p <- p + theme_prism(
        base_fontface = "plain",
        base_line_size = 0.9,
        base_family = "Arial"
      ) +
        scale_x_discrete(
          guide = guide_prism_bracket(width = 0.1),
          labels = scales::wrap_format(5)
        ) +
        theme(
          legend.position = "none",
          axis.title.x = element_blank(),
          aspect.ratio = 1.0,
          text = element_text(size = input$Fontsize)
        )
      if (input$q != "" && input$pvaluecheck == TRUE) {
        tukey_result <-  process_tukey_result(input,yval,input$q,p,dataa,0.05,input$Fontsize)
        p <- p + add_pvalue(tukey_result, remove.bracket = TRUE, bracket.size = 0.000001, label = "{symbol}", tip.length = 0,
                            label.size = tukey_result$size, inherit.aes = FALSE,y.position = tukey_result$y.position_boxplot) + scale_y_continuous(
                              limits = c(min(dataa[[yval]]), (max(tukey_result$y.position_boxplot) + max(tukey_result$y.position_boxplot) / 30))
                            )
      }
      svg(paste("./svg/",colnames(dataa)[i], ".svg", sep = ""))
      plot(p)
      dev.off()
      incProgress(1 / ncol(dataa))
      Sys.sleep(0.25)
    }
  })
  data.frame(name = colnames(dataa)[-c(1:metainfocol)], path = paste("file:/", svg_path, "/", colnames(dataa)[-c(1:metainfocol)], ".svg", sep = "")) %>% write.csv("./svg/path.csv")
  output$testtext1 <- renderText({
    "Finish"
  })
}

process_select_color_input <- function(input,data) {
  if (length(grep(paste0(unique(as.matrix(select(data, input$w)))[1], "_selectcolor"), names(input), value = TRUE)) != 0) {
    inputlist <- grep("selectcolor", names(input), value = TRUE)
    for (input_name in inputlist) {
      collist[str_remove_all(input_name, pattern = "_selectcolor")] <- input[[input_name]]
    }
    cola <<- unlist(collist)
    cola <- cola[c(unique(data[[input$w]]))]
    return(cola)
  }
}



process_tukey_result <- function(input,yval, q,plot_obj, dataa, p_adj_threshold, Fontsize) {
  yval <- gsub("`","",yval)
  y <- dataa %>% pull(as.symbol(yval))
  w <- dataa %>% pull(as.symbol(input$w))
  tukey_result <- TukeyHSD(aov(y ~ w, data = dataa))
  tukey_result <- data.frame(tukey_result$w)
  a <- str_split_fixed(rownames(tukey_result), "-", 2)
  colnames(a) <- c("Class1", "Class2")
  
  tukey_result <- cbind(a, tukey_result)
  maxvalue <- select(dataa,input$w,yval) %>%
    group_by_(input$w) %>%
    mutate(Mean = mean(!!as.symbol(yval))) %>%
    ungroup() %>%
    distinct(!!as.symbol(input$w),.keep_all = TRUE )
  
  maxvalue <- max(maxvalue$Mean)
  tukey_result <- filter(tukey_result, Class1 %in% q | Class2 %in% q) %>%
    mutate(group2 = ifelse(Class1 == q, Class2, Class1)) %>%
    mutate(group1 = paste0(q)) %>%
    mutate(symbol = ifelse(p.adj < p_adj_threshold, "*", "")) %>%
    select(group1, group2, p.adj, symbol) %>%
    mutate(y.position_boxplot = max(dataa[[yval]])) %>%
    mutate(y.position_barplot = maxvalue-maxvalue/15 ) %>%
    mutate(y.min.position = min(dataa[[yval]])) %>%
    mutate(size = ifelse(symbol != "*", 0, Fontsize))
  return(tukey_result)
}