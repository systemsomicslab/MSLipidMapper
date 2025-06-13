perform_statistical_test <- function(data, comparisons, test_method, y_var, w_var, p_adjust_method = "bonferroni") {
  # 入力チェック
  if (missing(data) || missing(comparisons) || missing(test_method) || 
      missing(y_var) || missing(w_var)) {
    stop("Required arguments are missing")
  }
  
  # 検定関数の取得
  test_func <- tryCatch({
    get(test_method, asNamespace("rstatix"))
  }, error = function(e) {
    stop(paste("Invalid test method:", test_method))
  })
  
  # 変数名を文字列からシンボルに変換
  #y_sym <- sym(y_var)
  w_sym <- sym(w_var)
  
  # 結果を格納するデータフレーム
  results <- data.frame()
  
  # 各比較対についてループ
  for(comp in comparisons) {
    # グループを分割
	print(comparisons)
	print(comp)
    groups <- strsplit(comp, "vs")[[1]]
    print(as.formula(paste0("`", y_var, "` ~ `", w_var, "`")))
	
    # データのサブセット作成
    subset_data <- data %>%
      dplyr::filter(!!w_sym %in% groups)
	  
    print((subset_data$Class) %>% unique())

    
    # 検定実行
    tryCatch({
      test_result <- subset_data %>%
        test_func(
          formula = as.formula(paste0("`", y_var, "` ~ `", w_var, "`"))
        ) %>%
        adjust_pvalue(method = p_adjust_method) %>%
        add_significance() 
      
      results <- bind_rows(results, test_result)
      
    }, error = function(e) {
      warning(sprintf(
        "Error in comparison %s: %s",
        comp,
        as.character(e)
      ))
    })
  }
  
  # 結果が空の場合の処理
  if (nrow(results) == 0) {
    warning("No valid comparisons could be performed")
    return(NULL)
  }
  
  return(results)
}





create_boxplot <- function(data, x_var, y_var, col, alpha, size, xlab_text, ylab_text, ggtitle_text) {
  p <- ggplot(data) +
    ggbeeswarm::geom_beeswarm(aes_string(x = x_var, y = y_var, fill = x_var), dodge.width = 0.1,
                               cex = 0.1,
                               shape = 21,
                               size = size) +
	geom_boxplot(aes_string(x = x_var, y = y_var, fill = x_var),color = "black", lwd = 1, alpha = alpha) +
	xlab(xlab_text) +
    ylab(ylab_text) +
    ggtitle(paste(ggtitle_text, "", sep = "")) +
    scale_fill_manual(values = unlist(col))
  p

}

process_boxplot <- function(input, yval, dataa, col,stat_test) {

  y <- yval %>% as.character() %>% gsub("`", "", .)
  p <- create_boxplot(dataa, input$w, yval, col, input$alpha, input$size, 
                       input$xAxisLabel, input$yAxisLabel, yval)

    if (input$pvaluecheck == TRUE && input$test_method != "anova_test") {
      stat_results <- stat_test

	    y_range <- diff(range(dplyr::select(dataa,y)))
		y_max <- max(dplyr::select(dataa,y), na.rm = TRUE)
		step_size <- y_range * 0.15
        y_positions <- y_max + 
        seq(step_size, by = step_size, length.out = nrow(stat_results))
        
        p <- p + stat_pvalue_manual(
          stat_results,
		  label.size = input$pvaluefontsize,
          label = "p.adj.signif",
          y.position = y_positions,
          bracket.size = 0.5,
          bracket.shorten = 0.1
        )
    }
    
    p <- p +
      theme_classic() + 
      theme(
        axis.text.x = element_text(
          angle = input$x_rotation,
          size = input$Fontsize
        ),
        text = element_text(size = input$Fontsize)
      ) 
    
    plot(p)
}


process_boxplot_diagram <- function(input,yval,dataa,col,stat_test) {
  
    y <- yval %>% as.character() %>% gsub("`", "", .)
    p <- create_boxplot(dataa, input$w, yval, col, input$alpha, input$size, 
                       input$xAxisLabel, input$yAxisLabel, yval)
    p <- p + theme_prism(
      base_fontface = "plain",
      base_line_size = 0.9,
      base_family = "Arial"
    ) +
      theme(
        legend.position = "none",
		axis.text.x = element_text(angle = input$x_rotation, 
                                 size = input$Fontsize),
        axis.title.x = element_blank(),
        aspect.ratio = 1.0,
        text = element_text(size = input$Fontsize)
      )
    #if (input$q != "" && input$pvaluecheck == TRUE) {
    #  tukey_result <-  process_tukey_result(input,y,input$q,p,dataa,0.05,input$Fontsize)
    #  p <- p + add_pvalue(tukey_result, remove.bracket = TRUE, bracket.size = 0.000001, label = "{symbol}", tip.length = 0,
    #                      label.size = tukey_result$size, inherit.aes = FALSE,y.position = tukey_result$y.position_boxplot) + scale_y_continuous(
    #                        limits = c(min(dataa[[y]]), (max(tukey_result$y.position_boxplot) + max(tukey_result$y.position_boxplot) / 30))
    #                      )
    #}
	
	 if (input$pvaluecheck == TRUE && input$test_method != "anova_test") {
      stat_results <- stat_test

	    y_range <- diff(range(dplyr::select(dataa,y)))
		y_max <- max(dplyr::select(dataa,y), na.rm = TRUE)
		step_size <- y_range * 0.15
        y_positions <- y_max + 
        seq(step_size, by = step_size, length.out = nrow(stat_results))
        
        p <- p + stat_pvalue_manual(
          stat_results,
		  label.size = input$pvaluefontsize,
          label = "p.adj.signif",
          y.position = y_positions,
          bracket.size = 0.5,
          bracket.shorten = 0.1
        )
    }
	
    p
}

process_action_boxplot <- function(input,dataa,metainfocol,svg_path,col,output,data) {
  
  withProgress(message = 'Calculation in progress', detail = 'This may take a while...', value = 1, {
    startnum <- 1 + metainfocol
    for (i in startnum:ncol(dataa)) {
      yval <- colnames(dataa)[i]
      p <- create_boxplot(dataa, input$w, dataa[[yval]], col, input$alpha, input$size,input$xAxisLabel, input$yAxisLabel, "")
      p <- p + theme_prism(
        base_fontface = "plain",
        base_line_size = 0.9,
        base_family = "Arial"
      ) +
        theme(
          legend.position = "none",
		  axis.text.x = element_text(angle = input$x_rotation,
                                 size = input$Fontsize),
          axis.title.x = element_blank(),
          aspect.ratio = 1.0,
          text = element_text(size = input$Fontsize)
        )
    #  if (input$q != "" && input$pvaluecheck == TRUE) {
    #    tukey_result <-  process_tukey_result(input,yval,input$q,p,dataa,0.05,input$Fontsize)
    #    p <- p + add_pvalue(tukey_result, remove.bracket = TRUE, bracket.size = 0.000001, label = "{symbol}", tip.length = 0,
    #                        label.size = tukey_result$size, inherit.aes = FALSE,y.position = tukey_result$y.position_boxplot) + scale_y_continuous(
    #                          limits = c(min(dataa[[yval]]), (max(tukey_result$y.position_boxplot) + max(tukey_result$y.position_boxplot) / 30))
    #                        )
    #  }
	  
	   if (input$pvaluecheck == TRUE && input$test_method != "anova_test") { 
    # 検定関数の選択
    test_func <- get(input$test_method, asNamespace("rstatix"))
    
    # 結果を格納するデータフレーム
    results <- data.frame()
    for(comp in input$comparisons) {
      # グループを分割
      groups <- strsplit(comp, "vs")[[1]]
      
      # データのサブセット作成
      subset_data <- data %>%
        dplyr::filter(!!sym(input$w) %in% groups)

      # 検定実行
      test_result <- test_func(subset_data, as.formula(paste(yval, "~", input$w)),paired = FALSE) %>%
        adjust_pvalue(method = input$p_adjust) %>%
        add_significance()
      results <- rbind(results, test_result)
    }
    
    stat_results <- results

	   #stat_results <- perform_statistical_test(
       #      data = dataa,
       #      comparisons = input$comparisons,
       #      test_method = input$test_method,  # または "t_test" など
       #      y_var = yval,
       #      w_var = input$w,
       #      p_adjust_method = input$p_adjust
       #)
	    y_range <- diff(range(dplyr::select(dataa,yval)))
		y_max <- max(dplyr::select(dataa,yval), na.rm = TRUE)
		step_size <- y_range * 0.15
        y_positions <- y_max + 
        seq(step_size, by = step_size, length.out = nrow(stat_results))
        
        p <- p + stat_pvalue_manual(
          stat_results,
		  label.size = input$pvaluefontsize,
          label = "p.adj.signif",
          y.position = y_positions,
          bracket.size = 0.5,
          bracket.shorten = 0.1
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

create_barplot <- function(data, x_var, y_var, col, alpha, size, xlab_text, ylab_text, ggtitle_text) {
  p <- ggplot(data) +
    geom_bar(aes_string(x = x_var, y = y_var, fill = x_var),color = "black", stat = "summary",fun = "mean",lwd = 1.2,width = 0.6) +
	xlab(xlab_text) +
    ylab(ylab_text) +
    ggtitle(paste(ggtitle_text, "", sep = "")) +
    scale_fill_manual(values = unlist(col))
  
  p
}


process_barplot <- function(input, yval, dataa, col,stat_test) {

  y <- yval %>% as.character() %>% gsub("`", "", .)
  p <- create_barplot(dataa, input$w, yval, col, input$alpha, input$size, 
                       input$xAxisLabel, input$yAxisLabel, yval)

    if (input$pvaluecheck == TRUE && input$test_method != "anova_test") {
      stat_results <- stat_test

	    y_range <- diff(range(dplyr::select(dataa,y)))
		y_max <- dataa %>%
		dplyr::select(1,!!sym(input$w),!!sym(y)) %>%
        group_by(!!sym(input$w)) %>%
        summarise(mean_value = mean(!!sym(y))) %>%
        summarise(max_mean = max(mean_value)) %>%
        pull(max_mean)
        #y_max <- max(select(dataa,y), na.rm = TRUE)
		print(y_max)
		step_size <- y_range * 0.15
        y_positions <- y_max + 
        seq(step_size, by = step_size, length.out = nrow(stat_results))
        
        p <- p + stat_pvalue_manual(
          stat_results,
		  label.size = input$pvaluefontsize,
          label = "p.adj.signif",
          y.position = y_positions,
          bracket.size = 0.5,
          bracket.shorten = 0.1
        )
    }
    
    p <- p +
      theme_classic() + 
      theme(
        axis.text.x = element_text(
          angle = input$x_rotation,
          size = input$Fontsize
        ),
        text = element_text(size = input$Fontsize)
      ) 
    
    plot(p)
}


process_barplot_diagram <- function(input,yval,dataa,col,stat_test) {
  
  y <- yval %>% as.character()
  p <- create_barplot(dataa, input$w, y, col, input$alpha, input$size, input$xAxisLabel, input$yAxisLabel, y)
  p <- p + theme_prism(
    base_fontface = "plain",
    base_line_size = 0.9,
    base_family = "Arial") +
    theme(
      legend.position = "none",
	  axis.text.x = element_text(angle = input$x_rotation,
                                 size = input$Fontsize),
      aspect.ratio = 1.0,
      text = element_text(size = input$Fontsize))
	 if (input$pvaluecheck == TRUE && input$test_method != "anova_test") {
      stat_results <- stat_test

	    y_range <- diff(range(dplyr::select(dataa,y)))
		y_max <- max(dplyr::select(dataa,y), na.rm = TRUE)
		step_size <- y_range * 0.15
        y_positions <- y_max + 
        seq(step_size, by = step_size, length.out = nrow(stat_results))
        
        p <- p + stat_pvalue_manual(
          stat_results,
		  label.size = input$pvaluefontsize,
          label = "p.adj.signif",
          y.position = y_positions,
          bracket.size = 0.5,
          bracket.shorten = 0.1
        )
    }
  p
}



process_action_barplot <- function(input,dataa,metainfocol,svg_path,col,output,data) {
  withProgress(message = 'Calculation in progress', detail = 'This may take a while...', value = 1, {
    startnum <- 1 + metainfocol
    for (i in startnum:ncol(dataa)) {
      yval <- colnames(dataa)[i]
      p <- create_barplot(dataa, input$w, dataa[[yval]], col, input$alpha, input$size, input$xAxisLabel, input$yAxisLabel, "")
      p <- p + theme_prism(
        base_fontface = "plain",
        base_line_size = 0.9,
        base_family = "Arial") +
        theme(
          legend.position = "none",
          axis.title.x = element_blank(),
          aspect.ratio = 1.0,
		  axis.text.x = element_text(angle = input$x_rotation,
                                 size = input$Fontsize),
          text = element_text(size = input$Fontsize))
     # if (input$q!= "" && input$pvaluecheck == TRUE) {
      #  tukey_result <-  process_tukey_result(input,yval,input$q,p,dataa,0.05,input$Fontsize)
      #  p <- p + add_pvalue(tukey_result,remove.bracket = TRUE,
      #                      bracket.size = 0.000001,
      #                      label = "{symbol}",
      #                      tip.length = 0,
      #                      label.size = tukey_result$size,inherit.aes = FALSE,y.position = tukey_result$y.position_barplot) 
     # }
if (input$pvaluecheck == TRUE && input$test_method != "anova_test") { 
    # 検定関数の選択
    test_func <- get(input$test_method, asNamespace("rstatix"))
    
    # 結果を格納するデータフレーム
    results <- data.frame()
    for(comp in input$comparisons) {
      # グループを分割
      groups <- strsplit(comp, "vs")[[1]]
      
      # データのサブセット作成
      subset_data <- data %>%
        dplyr::filter(!!sym(input$w) %in% groups)

      # 検定実行
      test_result <- test_func(subset_data, as.formula(paste(yval, "~", input$w)),paired = FALSE) %>%
        adjust_pvalue(method = input$p_adjust) %>%
        add_significance()
      results <- rbind(results, test_result)
    }
    
    stat_results <- results

	   #stat_results <- perform_statistical_test(
       #      data = dataa,
       #      comparisons = input$comparisons,
       #      test_method = input$test_method,  # または "t_test" など
       #      y_var = yval,
       #      w_var = input$w,
       #      p_adjust_method = input$p_adjust
       #)
	    y_range <- diff(range(dplyr::select(dataa,yval)))
		y_max <- max(dplyr::select(dataa,yval), na.rm = TRUE)
		step_size <- y_range * 0.15
        y_positions <- y_max + 
        seq(step_size, by = step_size, length.out = nrow(stat_results))
        
        p <- p + stat_pvalue_manual(
          stat_results,
		  label.size = input$pvaluefontsize,
          label = "p.adj.signif",
          y.position = y_positions,
          bracket.size = 0.5,
          bracket.shorten = 0.1
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

process_violinplot <- function(input, yval, dataa, col,stat_test) {

  y <- yval %>% as.character() %>% gsub("`", "", .)
  p <- create_violinplot(dataa, input$w, yval, col, input$alpha, input$size, 
                       input$xAxisLabel, input$yAxisLabel, yval)

    if (input$pvaluecheck == TRUE && input$test_method != "anova_test") {
      stat_results <- stat_test

	    y_range <- diff(range(dplyr::select(dataa,y)))
        y_max <- max(dplyr::select(dataa,y), na.rm = TRUE)
		step_size <- y_range * 0.15
        y_positions <- y_max + 
        seq(step_size, by = step_size, length.out = nrow(stat_results))
        
        p <- p + stat_pvalue_manual(
          stat_results,
		  label.size = input$pvaluefontsize,
          label = "p.adj.signif",
          y.position = y_positions,
          bracket.size = 0.5,
          bracket.shorten = 0.1
        )
    }
    
    p <- p +
      theme_classic() + 
      theme(
        axis.text.x = element_text(
          angle = input$x_rotation,
          size = input$Fontsize
        ),
        text = element_text(size = input$Fontsize)
      ) 
    
    plot(p)
}

create_violinplot <- function(data, x_var, y_var, col, alpha, size, xlab_text, ylab_text, ggtitle_text) {
  p <- ggplot(data,aes_string(y = y_var)) +
    geom_violin(aes_string(x = x_var,fill = x_var), adjust = 2,trim = FALSE) +
	xlab(xlab_text) +
    ylab(ylab_text) +
    ggtitle(paste(ggtitle_text, "", sep = "")) +
    scale_fill_manual(values = unlist(col)) +
    stat_summary(aes_string(x = x_var),fun = "mean",geom = "crossbar",color = "black",linewidth = 0.5, width = 0.5)
  p
}

process_violinplot_diagram <- function(input,yval,dataa,col,stat_test) {

  y <- yval %>% as.character()
  p <- create_violinplot(dataa, input$w, y, col, input$alpha, input$size, input$xAxisLabel, input$yAxisLabel, y)
  p <- p + theme_prism(
    base_fontface = "plain",
    base_line_size = 0.9,
    base_family = "Arial") +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      aspect.ratio = 1.0,
	  axis.text.x = element_text(angle = input$x_rotation, 
                                 size = input$Fontsize),
      text = element_text(size = input$Fontsize))
	 if (input$pvaluecheck == TRUE && input$test_method != "anova_test") {
      stat_results <- stat_test

	    y_range <- diff(range(dplyr::select(dataa,y)))
		y_max <- max(dplyr::select(dataa,y), na.rm = TRUE)
		step_size <- y_range * 0.15
        y_positions <- y_max + 
        seq(step_size, by = step_size, length.out = nrow(stat_results))
        
        p <- p + stat_pvalue_manual(
          stat_results,
		  label.size = input$pvaluefontsize,
          label = "p.adj.signif",
          y.position = y_positions,
          bracket.size = 0.5,
          bracket.shorten = 0.1
        )
    }
  p
}

process_action_violinplot <- function(input,dataa,metainfocol,svg_path,col,output,data) {
  withProgress(message = 'Calculation in progress', detail = 'This may take a while...', value = 1, {
    startnum <- 1 + metainfocol
    for (i in startnum:ncol(dataa)) {
      yval <- colnames(dataa)[i]
      p <- create_violinplot(dataa, input$w, dataa[[yval]], col, input$alpha, input$size, input$xAxisLabel, input$yAxisLabel, "")
      p <- p + theme_prism(
        base_fontface = "plain",
        base_line_size = 0.9,
        base_family = "Arial") +
        theme(
          legend.position = "none",
          axis.title.x = element_blank(),
          aspect.ratio = 1.0,
		  axis.text.x = element_text(angle = input$x_rotation,
                                 size = input$Fontsize),
          text = element_text(size = input$Fontsize))
      #if (input$q!= "" && input$pvaluecheck == TRUE) {
      #  tukey_result <-  process_tukey_result(input,yval,input$q,p,dataa,0.05,input$Fontsize)
       # p <- p + add_pvalue(tukey_result, remove.bracket = TRUE, bracket.size = 0.000001, label = "{symbol}", tip.length = 0,
       #                     label.size = tukey_result$size, inherit.aes = FALSE,y.position = tukey_result$y.position_boxplot) + scale_y_continuous(
       #                       limits = c(min(dataa[[yval]]), (max(tukey_result$y.position_boxplot) + max(tukey_result$y.position_boxplot) / 30))
       #                     )
      #}
if (input$pvaluecheck == TRUE && input$test_method != "anova_test") { 
    # 検定関数の選択
    test_func <- get(input$test_method, asNamespace("rstatix"))
    
    # 結果を格納するデータフレーム
    results <- data.frame()
    for(comp in input$comparisons) {
      # グループを分割
      groups <- strsplit(comp, "vs")[[1]]
      
      # データのサブセット作成
      subset_data <- data %>%
        dplyr::filter(!!sym(input$w) %in% groups)

      # 検定実行
      test_result <- test_func(subset_data, as.formula(paste(yval, "~", input$w)),paired = FALSE) %>%
        adjust_pvalue(method = input$p_adjust) %>%
        add_significance()
      results <- rbind(results, test_result)
    }
    
    stat_results <- results

	   #stat_results <- perform_statistical_test(
       #      data = dataa,
       #      comparisons = input$comparisons,
       #      test_method = input$test_method,  # または "t_test" など
       #      y_var = yval,
       #      w_var = input$w,
       #      p_adjust_method = input$p_adjust
       #)
	    y_range <- diff(range(dplyr::select(dataa,yval)))
		y_max <- max(dplyr::select(dataa,yval), na.rm = TRUE)
		step_size <- y_range * 0.15
        y_positions <- y_max + 
        seq(step_size, by = step_size, length.out = nrow(stat_results))
        
        p <- p + stat_pvalue_manual(
          stat_results,
		  label.size = input$pvaluefontsize,
          label = "p.adj.signif",
          y.position = y_positions,
          bracket.size = 0.5,
          bracket.shorten = 0.1
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


process_dotplot <- function(input, yval, dataa, col,stat_test) {

  y <- yval %>% as.character() %>% gsub("`", "", .)
  p <- create_dotplot(dataa, input$w, yval, col, input$alpha, input$size, 
                       input$xAxisLabel, input$yAxisLabel, yval)

    if (input$pvaluecheck == TRUE && input$test_method != "anova_test") {
      stat_results <- stat_test
	    y_range <- diff(range(dplyr::select(dataa,y)))
        y_max <- max(dplyr::select(dataa,y), na.rm = TRUE)
		step_size <- y_range * 0.15
        y_positions <- y_max + 
        seq(step_size, by = step_size, length.out = nrow(stat_results))
        
        p <- p + stat_pvalue_manual(
          stat_results,
		  label.size = input$pvaluefontsize,
          label = "p.adj.signif",
          y.position = y_positions,
          bracket.size = 0.5,
          bracket.shorten = 0.1
        )
    }
    
    p <- p +
      theme_classic() + 
      theme(
        axis.text.x = element_text(
          angle = input$x_rotation,
          size = input$Fontsize
        ),
        text = element_text(size = input$Fontsize)
      ) 
    
    plot(p)
}

create_dotplot <- function(data, x_var, y_var, col, alpha, size, xlab_text, ylab_text, ggtitle_text) {
  p <- ggplot(data,aes_string(y = y_var)) +
    ggbeeswarm::geom_beeswarm(aes_string(x = x_var,fill = x_var),
                              cex = 0.1,
                              shape = 21,
                              size = size)+
	xlab(xlab_text) +
    ylab(ylab_text) +
    ggtitle(paste(ggtitle_text, "", sep = "")) +
    scale_fill_manual(values = unlist(col)) +
    stat_summary(aes_string(x = x_var),fun = "mean",geom = "crossbar",color = "black",linewidth = 0.5, width = 0.5)
  p
}

process_dotplot_diagram <- function(input,yval,dataa,col,stat_test) {
  
  
  y <- yval %>% as.character()
  p <- create_dotplot(dataa, input$w, y, col, input$alpha, input$size, input$xAxisLabel, input$yAxisLabel, y)
  p <- p + theme_prism(
    base_fontface = "plain",
    base_line_size = 0.9,
    base_family = "Arial"
  ) +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      aspect.ratio = 1.0,
	  axis.text.x = element_text(angle = input$x_rotation,
                                 size = input$Fontsize),
      text = element_text(size = input$Fontsize)
    )
	 if (input$pvaluecheck == TRUE && input$test_method != "anova_test") {
      stat_results <- stat_test

	    y_range <- diff(range(dplyr::select(dataa,y)))
		y_max <- max(dplyr::select(dataa,y), na.rm = TRUE)
		step_size <- y_range * 0.15
        y_positions <- y_max + 
        seq(step_size, by = step_size, length.out = nrow(stat_results))
        
        p <- p + stat_pvalue_manual(
          stat_results,
		  label.size = input$pvaluefontsize,
          label = "p.adj.signif",
          y.position = y_positions,
          bracket.size = 0.5,
          bracket.shorten = 0.1
        )
    }
  p
}

process_action_dotplot <- function(input,dataa,metainfocol,svg_path,col,output,data) {
  
  withProgress(message = 'Calculation in progress', detail = 'This may take a while...', value = 1, {
    startnum <- 1 + metainfocol
    for (i in startnum:ncol(dataa)) {
      yval <- colnames(dataa)[i]
      p <- create_dotplot(dataa, input$w, dataa[[yval]], col, input$alpha, input$size, input$xAxisLabel, input$yAxisLabel, "")
      p <- p + theme_prism(
        base_fontface = "plain",
        base_line_size = 0.9,
        base_family = "Arial"
      ) +
        theme(
          legend.position = "none",
          axis.title.x = element_blank(),
          aspect.ratio = 1.0,
		  axis.text.x = element_text(angle = input$x_rotation,
                                 size = input$Fontsize),
          text = element_text(size = input$Fontsize)
        )
     # if (input$q != "" && input$pvaluecheck == TRUE) {
     #   tukey_result <-  process_tukey_result(input,yval,input$q,p,dataa,0.05,input$Fontsize)
     #   p <- p + add_pvalue(tukey_result, remove.bracket = TRUE, bracket.size = 0.000001, label = "{symbol}", tip.length = 0,
     #                       label.size = tukey_result$size, inherit.aes = FALSE,y.position = tukey_result$y.position_boxplot) + scale_y_continuous(
     #                         limits = c(min(dataa[[yval]]), (max(tukey_result$y.position_boxplot) + max(tukey_result$y.position_boxplot) / 30))
      #                      )
     # }
if (input$pvaluecheck == TRUE && input$test_method != "anova_test") { 
    # 検定関数の選択
    test_func <- get(input$test_method, asNamespace("rstatix"))
    
    # 結果を格納するデータフレーム
    results <- data.frame()
    for(comp in input$comparisons) {
      # グループを分割
      groups <- strsplit(comp, "vs")[[1]]
      
      # データのサブセット作成
      subset_data <- data %>%
        dplyr::filter(!!sym(input$w) %in% groups)

      # 検定実行
      test_result <- test_func(subset_data, as.formula(paste(yval, "~", input$w)),paired = FALSE) %>%
        adjust_pvalue(method = input$p_adjust) %>%
        add_significance()
      results <- rbind(results, test_result)
    }
    
    stat_results <- results

	   #stat_results <- perform_statistical_test(
       #      data = dataa,
       #      comparisons = input$comparisons,
       #      test_method = input$test_method,  # または "t_test" など
       #      y_var = yval,
       #      w_var = input$w,
       #      p_adjust_method = input$p_adjust
       #)
	    y_range <- diff(range(dplyr::select(dataa,yval)))
		y_max <- max(dplyr::select(dataa,yval), na.rm = TRUE)
		step_size <- y_range * 0.15
        y_positions <- y_max + 
        seq(step_size, by = step_size, length.out = nrow(stat_results))
        
        p <- p + stat_pvalue_manual(
          stat_results,
		  label.size = input$pvaluefontsize,
          label = "p.adj.signif",
          y.position = y_positions,
          bracket.size = 0.5,
          bracket.shorten = 0.1
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
  if (length(grep(paste0(unique(as.matrix(dplyr::select(data, input$w)))[1], "_selectcolor"), names(input), value = TRUE)) != 0) {
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
  maxvalue <- dplyr::select(dataa,input$w,yval) %>%
    group_by_(input$w) %>%
    mutate(Mean = mean(!!as.symbol(yval))) %>%
    ungroup() %>%
    distinct(!!as.symbol(input$w),.keep_all = TRUE )
  
  maxvalue <- max(maxvalue$Mean)
  tukey_result <- filter(tukey_result, Class1 %in% q | Class2 %in% q) %>%
    mutate(group2 = ifelse(Class1 == q, Class2, Class1)) %>%
    mutate(group1 = paste0(q)) %>%
    mutate(symbol = ifelse(p.adj < p_adj_threshold, "*", "")) %>%
    dplyr::select(group1, group2, p.adj, symbol) %>%
    mutate(y.position_boxplot = max(dataa[[yval]])) %>%
    mutate(y.position_barplot = maxvalue-maxvalue/15 ) %>%
    mutate(y.min.position = min(dataa[[yval]])) %>%
    mutate(size = ifelse(symbol != "*", 0, Fontsize))
  return(tukey_result)
}