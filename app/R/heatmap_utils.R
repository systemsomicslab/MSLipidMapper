groupnodeheatmap <- function(lipidclassproperties,data,metadata,class,levels,inputclass,input){
  # lipidclassproperties
  if(input$filetype == "MS-DIAL export"){
  lipid_data_classmean <<- right_join(lipidclassproperties,lipidmeancalforgroupnode(data,metadata,input$w) ,by = c("LbmClass"="Ontology"))
  }
  
  if(input$filetype == "Sample in rows"){
    colnames(data) <- data[1,]
    data <- data[-1,]
    data[, -c(1:2)] <- apply(data[, -c(1:2)], 2, as.numeric)
	colnames(data)[1] <- "name"
	colnames(metadata)[1] <- "name"
    lipid <- data %>% pivot_longer(cols = -c(1:2),names_to = "Metabolite name") %>%  
      inner_join(metadata,by = c("name","Class")) %>% 
      dplyr::select(c(paste(input$w,"",sep=""),`Metabolite name`,name,value)) %>%
      group_by_at(c("Metabolite name",input$w)) %>%
      mutate(mean = mean(value)) %>%
      ungroup() %>%
      distinct(`Metabolite name`,!!as.symbol(input$w),.keep_all = T) %>%
      dplyr::select(c(1,2,5)) %>%
      pivot_wider(names_from = paste(input$w,"",sep=""),values_from = mean)
    lipidont <- read.csv(input$ontfile$datapath,check.names = F)
    lipid <- inner_join(lipidont,lipid,by =c("lipid" = "Metabolite name"))
    lipid <- rename(lipid,`Metabolite name`  = lipid)
    lipid_data_classmean <- right_join(lipidclassproperties,lipid,by = c("LbmClass"="Ontology")) %>% filter(TotalChain != 0)# %>% filter(`Annotation tag (VS1.0)` == "410")
  }
  
  #lipid_data_classmean <- right_join(lipidclassproperties,lipidmeancalforgroupnode(data,metadata,paste(class,"",sep="")) ,by = c("LbmClass"="Ontology")) # %>% filter(`Annotation tag (VS1.0)` == "410")
  lipid_data_classmean <- filter(lipid_data_classmean,LbmClass == inputclass)
  if(unique(lipid_data_classmean$TotalChain) == "1"){
    targetmonoacylchain <- c("16:0","16:1","18:0","18:1","18:2","18:3","20:3","20:4","20:5","22:4","22:5","22:6")
    monoacyl <- lipid_data_classmean %>% filter(TotalChain == "1") %>% filter(!LbmClass %in% c("AHexCS","AHexSTS"))
    targetlipidclass <- monoacyl$LbmClass %>% unique()
    monoacyl <- monoacyl[,-c(2:7)]
    if(input$acylfilter == T){
      
      acyllist <- list()
      for (i in 1:nrow(monoacyl)) {
        a <- unlist(str_split(monoacyl$`Metabolite name`[i],pattern = " "))[2]
        
        if (str_detect(a,pattern = ";") == TRUE){
          acyl <- a %>% str_split(pattern = ";") %>% unlist()
          if(str_detect(acyl[1],pattern = "-")){
            acyl <- acyl[1] %>% str_split(pattern = "-") %>% unlist()
            acyllist[i] <- a[2]
          }else{
            acyllist[i] <- acyl[1]
          }
          
        }else if(str_detect(a,pattern = "/")==TRUE){
          acyl <- a %>% str_split(pattern = "/") %>% unlist()
          acyllist[i] <- acyl[2]
        }else{
          if(str_detect(a,pattern = "-")){
            acyl <- a %>% str_split(pattern = "-") %>% unlist()
            acyllist[i] <- acyl[2]
          }else{
            acyllist[i] <- a
          }
        }
      }
      
      
      
      monoacyl$acyl <- unlist(acyllist)
      #
      targetmonoacyllipid <- monoacyl %>% filter(acyl %in% targetmonoacylchain) %>% dplyr::select(-c(acyl))
    }
    else{
      targetmonoacyllipid <- monoacyl
    }
    targetlipidclass <- unique(targetmonoacyllipid$LbmClass)
    monoacyl <- targetmonoacyllipid
    
    Lipidclassdata<- filter(monoacyl , LbmClass == inputclass) %>% distinct(`Metabolite name`,.keep_all = TRUE)
    if(length(Lipidclassdata$LbmClass) != 0){
      d <- Lipidclassdata
      rowname <- as.matrix((d$`Metabolite name`))
      d <- dplyr::select(d,-c(LbmClass))
      n <- nrow(d)
      BreaksList <- seq(-2,2, by = 0.5)
      height <- n*0.9
      list <- c(-2,0,2)
      lipid <- d
      lipid <- lipid[,-1]
      rownames(lipid) <- rowname
      lipid <- scale_rows(lipid) %>% as.data.frame()
      lipid <- tibble::rownames_to_column(lipid, "Metabolite name")
      lipid <- pivot_longer(lipid,cols = -c(`Metabolite name`))
      lipid <- mutate(lipid, name = name %>% factor(levels = input$levels))
      
      ghm <- ggplot(lipid, aes(x = name, y =`Metabolite name` , fill = value))
      ghm <- ghm + geom_tile(color = "black") + scale_fill_gradientn(
        colors=c("navy","white","red"),limits=c(min(lipid$value), max(lipid$value))
      ) + theme(axis.text.x = element_text(angle = 90, hjust = 0),axis.text = element_text(size = input$Fontsize))
      print(ghm)
    }}
  if(unique(lipid_data_classmean$TotalChain) == "2"&& unique(lipid_data_classmean$Categories) != "Sphingolipids" && unique(lipid_data_classmean$Categories) != "Sterol Lipids"){
    targetdiacylchain <- c("16:0","16:1","18:0","18:1","18:2","18:3","20:3","20:4","20:5","22:4","22:5","22:6")
    diacyl <- lipid_data_classmean %>% filter(TotalChain == "2", Categories != "Sphingolipids")
    targetlipidclass <- diacyl$LbmClass %>% unique()
    diacyl <- diacyl[,-c(3:7)]
    if(input$acylfilter == T){
      acyl1list <- list()
      acyl2list <- list()
      for (i in 1:nrow(diacyl) ) {
        if(str_detect(diacyl$`Metabolite name`[i],pattern = "\\|") == TRUE){
          a <-  unlist(str_split(diacyl$`Metabolite name`[i],pattern = "\\|"))[2] %>% str_split(pattern = " ") %>% unlist()
          if(str_detect(a[2],pattern = "_") == TRUE){
            a <- a[2] %>% str_split(pattern = "_") %>% unlist()
            if(str_detect(a[1],pattern = "-") == TRUE){
              acyl1 <- a[1] %>% str_split(pattern = "-") %>% unlist()
              acyl1list[i] <- acyl1[2]
            }else{
              acyl1list[i] <- a[1]
            }
            if(str_detect(a[2],pattern = ";") == TRUE){
              acyl2 <- a[2] %>% str_split(pattern = ";") %>% unlist()
              acyl2list[i] <- acyl2[1]
            }else{
              acyl2list[i] <- a[2]
            }
          }else{
            a <- unlist(a)[2] %>% str_split(pattern = "\\)") %>% unlist()
            acyl1 <- a[1] %>%  str_split(pattern = " ") %>% unlist()
            acyl1list[i] <- acyl1[2]
            acyl2list[i] <- a[2]
          }
        }
        else{
          acyl1list[i] <- NA
          acyl2list[i] <- NA
        }
      }
      
      diacyl$acyl1 <- unlist(acyl1list)
      diacyl$acyl2 <- unlist(acyl2list)
      
      targetdiacyllipid <- diacyl %>% filter(acyl1 %in% targetdiacylchain, acyl2 %in% targetdiacylchain)
      
      # if(input$sn == T){
      #   targetdiacyllipid$acyl1 <- paste(targetdiacyllipid$LbmClass," ",targetdiacyllipid$acyl1," (sn-1)",sep ="")
      #   targetdiacyllipid$acyl2 <- paste(targetdiacyllipid$LbmClass," ",targetdiacyllipid$acyl2," (sn-2)",sep ="")
      #   targetdiacyllipid <- targetdiacyllipid %>% dplyr::select( `Metabolite name`,LbmClass,acyl1,acyl2,everything()) %>% pivot_longer(cols = -c(1:4)) %>% group_by(name,acyl1) %>% mutate(mean1 = mean(value)) %>% ungroup() %>% group_by(name,acyl2) %>% mutate(mean2 = mean(value)) %>% ungroup()
      #   sn1 <- dplyr::select(targetdiacyllipid,c(LbmClass,name,acyl1,mean1)) %>% distinct(name,acyl1,.keep_all = T)
      #   colnames(sn1) <- c("LbmClass","name","Metabolite name","value")
      #   sn2 <- dplyr::select(targetdiacyllipid,c(LbmClass,name,acyl2,mean2))%>% distinct(name,acyl2,.keep_all = T)
      #   colnames(sn2) <- c("LbmClass","name","Metabolite name","value")
      #   diacyl <- rbind(sn1,sn2) %>% pivot_wider(names_from = "name",values_from = "value") #%>% data.frame()
      # }
      
      
      diacyl <- targetdiacyllipid %>% dplyr::select(-c(acyl1,acyl2))
      
    }
    else{}
    Lipidclassdata<- filter(diacyl , LbmClass == inputclass) %>% distinct(`Metabolite name`,.keep_all = TRUE)
    if(length(Lipidclassdata$LbmClass) != 0){
      d <- Lipidclassdata
      rowname <- as.matrix((d$`Metabolite name`))
      d <- dplyr::select(d,-c(`Metabolite name`,LbmClass))
      BreaksList <- seq(-2,2, by = 0.5)
      # print(d)
      n <- nrow(d)
      height <- n/10*3
      list <- c(-2,0,2)
      lipid <- d
      lipid <- lipid[,-1]
      rownames(lipid) <- rowname
      lipid <- scale_rows(lipid) %>% as.data.frame()
      lipid <- tibble::rownames_to_column(lipid, "Metabolite name")
      lipid <- pivot_longer(lipid,cols = -c(`Metabolite name`))
      
      lipid <- mutate(lipid, name = name %>% factor(levels = input$levels))
      ghm <- ggplot(lipid, aes(x = name, y =`Metabolite name` , fill = value))
      ghm <- ghm + geom_tile(color = "black") + scale_fill_gradientn(
        colors=c("navy","white","red"),limits=c(min(lipid$value), max(lipid$value))
      ) + theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.text = element_text(size = input$Fontsize))
      print(ghm)
    }}
  
  if(unique(lipid_data_classmean$TotalChain) == "2"&& unique(lipid_data_classmean$Categories) == "Sterol Lipids" && unique(lipid_data_classmean$LbmClass) != "WE"){
    targetdiacylchain <- c("16:0","16:1","18:0","18:1","18:2","18:3","20:3","20:4","20:5","22:4","22:5","22:6")
    diacyl <- lipid_data_classmean %>% filter(TotalChain == "2", LbmClass != "WE")# %>%
    targetlipidclass <- diacyl$LbmClass %>% unique()
    diacyl <- diacyl[,-c(3:7)]
    if(input$acylfilter == T){
      acyl1list <- list()
      acyl2list <- list()
      for (i in 1:nrow(diacyl) ) {
        
        if(str_detect(diacyl$`Metabolite name`[i],pattern = "\\|") == TRUE){
          a <-  unlist(str_split(diacyl$`Metabolite name`[i],pattern = "\\|"))[2] %>% str_split(pattern = " ") %>% unlist()
          if(str_detect(a[2],pattern = "/") == TRUE){
            a <- a[2] %>% str_split(pattern = "/") %>% unlist()
            acyl1list[i] <- a[1]
            acyl2list[i] <- a[2]
          }
        }
        else{
          acyl1list[i] <- NA
          acyl2list[i] <- NA
        }
      }
      diacyl$acyl1 <- unlist(acyl1list)
      diacyl$acyl2 <- unlist(acyl2list)
      
      targetdiacyllipid <- diacyl %>% filter(acyl1 %in% targetdiacylchain, acyl2 %in% targetdiacylchain)
      
      # if(input$sn == T){
      #   targetdiacyllipid$acyl1 <- paste(targetdiacyllipid$LbmClass," ",targetdiacyllipid$acyl1," (sn-1)",sep ="")
      #   targetdiacyllipid$acyl2 <- paste(targetdiacyllipid$LbmClass," ",targetdiacyllipid$acyl2," (sn-2)",sep ="")
      #   targetdiacyllipid <- targetdiacyllipid %>% dplyr::select( `Metabolite name`,LbmClass,acyl1,acyl2,everything()) %>% pivot_longer(cols = -c(1:4)) %>% group_by(name,acyl1) %>% mutate(mean1 = mean(value)) %>% ungroup() %>% group_by(name,acyl2) %>% mutate(mean2 = mean(value)) %>% ungroup()
      #   sn1 <- dplyr::select(targetdiacyllipid,c(LbmClass,name,acyl1,mean1)) %>% distinct(name,acyl1,.keep_all = T)
      #   colnames(sn1) <- c("LbmClass","name","Metabolite name","value")
      #   sn2 <- dplyr::select(targetdiacyllipid,c(LbmClass,name,acyl2,mean2))%>% distinct(name,acyl2,.keep_all = T)
      #   colnames(sn2) <- c("LbmClass","name","Metabolite name","value")
      #   diacyl <- rbind(sn1,sn2) %>% pivot_wider(names_from = "name",values_from = "value") #%>% data.frame()
      # }
      
      
      diacyl <- targetdiacyllipid %>% dplyr::select(-c(acyl1,acyl2))
      
    }
    else{}
    
    
    Lipidclassdata<- filter(diacyl , LbmClass == inputclass) %>% distinct(`Metabolite name`,.keep_all = TRUE)
    if(length(Lipidclassdata$LbmClass) != 0){
      d <- Lipidclassdata
      rowname <- as.matrix((d$`Metabolite name`))
      d <- dplyr::select(d,-c(`Metabolite name`,LbmClass))
      
      BreaksList <- seq(-2,2, by = 0.5)
      n <- nrow(d)
      height <- n/10*3
      list <- c(-2,0,2)
      lipid <- d
      lipid <- lipid[,-1]
      rownames(lipid) <- rowname
      lipid <- scale_rows(lipid) %>% as.data.frame()
      lipid <- tibble::rownames_to_column(lipid, "Metabolite name")
      lipid <- pivot_longer(lipid,cols = -c(`Metabolite name`))
      lipid <- mutate(lipid, name = name %>% factor(levels = input$levels))
      ghm <- ggplot(lipid, aes(x = name, y =`Metabolite name` , fill = value))
      ghm <- ghm + geom_tile(color = "black") + scale_fill_gradientn(
        colors=c("navy","white","red"),limits=c(min(lipid$value), max(lipid$value))
      ) + theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.text = element_text(size = input$Fontsize))
      print(ghm)
    }}
  
  if(unique(lipid_data_classmean$TotalChain) == "2"&&unique(lipid_data_classmean$Categories) == "Sphingolipids"&&unique(lipid_data_classmean$LbmClass) != "ASM"){
    targetSphingoidBase <- c("18:0;O2","18:1;O2","18:2;O2","18:0;O3","18:0;2O","18:1;2O","18:2;2O","18:0;3O")
    targetnacylchain <- c("16:0","16:1","18:0", "20:0", "22:0", "24:0", "24:1", "26:0", "26:1")
    epidermalacylceramide <- c("Cer_EODS","Cer_EOS","Cer_EBDS","HexCer_EOS")
    epidermalacylceramidetargetacylchain <- c("18:2")
    diacyl <- lipid_data_classmean %>% filter(TotalChain == "2",Categories == "Sphingolipids",LbmClass != "ASM")
    targetlipidclass <- diacyl$LbmClass %>% unique()
    diacyl <- diacyl[,-c(3:7)]
    
    if(input$acylfilter == T){
      acyl1list <- list()
      acyl2list <- list()
      for (i in 1:nrow(diacyl)) {
        if(str_detect(diacyl$`Metabolite name`[i],pattern = "\\|") == TRUE){
          a <-  unlist(str_split(diacyl$`Metabolite name`[i],pattern = "\\|"))[2] %>% str_split(pattern = " ")
          if(str_detect(unlist(a)[2],pattern = "/") == TRUE){
            a <- unlist(a)[2] %>% str_split(pattern = "/") %>% unlist()
            acyl1list[i] <- a[1]
            if(str_detect(a[2],pattern = ";") == TRUE){
              
              acyl2 <- a[2] %>% str_split(pattern = ";") %>% unlist()
              acyl2list[i] <- acyl2[1]
            }
            else if(str_detect(a[2],pattern = "\\(") == TRUE){
              acyl2 <- a[2] %>% str_split(pattern = "\\(") %>% unlist()
              acyl2list[i] <- acyl2[1]
            }
            else{
              acyl2list[i] <- a[2]
            }
          }
          else{
            acyl1list[i] <- NA
            acyl2list[i] <- NA
          }
        }
        else{
          acyl1list[i] <- NA
          acyl2list[i] <- NA
        }
      }
      diacyl$acyl1 <- unlist(acyl1list)
      diacyl$acyl2 <- unlist(acyl2list)
      targetdiacyllipidnotEOS <- diacyl %>% filter(!Categories %in% epidermalacylceramide) %>% filter(acyl1 %in% targetSphingoidBase, acyl2 %in% targetnacylchain)
      targetdiacyllipidEOS  <- diacyl %>% filter(Categories %in% epidermalacylceramide) %>% filter(acyl1 %in% targetSphingoidBase, acyl2 %in% epidermalacylceramidetargetacylchain)
      targetdiacyllipid <- rbind(targetdiacyllipidEOS,targetdiacyllipidnotEOS)
      targetlipidclass <- unique(targetdiacyllipid$LbmClass)
      
      # if(input$sn == T){
      #   targetdiacyllipid$acyl1 <- paste(targetdiacyllipid$LbmClass," C",targetdiacyllipid$acyl1,sep ="")
      #   targetdiacyllipid$acyl2 <- paste(targetdiacyllipid$LbmClass," C",targetdiacyllipid$acyl2,sep ="")
      #   targetdiacyllipid <- targetdiacyllipid %>% dplyr::select( `Metabolite name`,LbmClass,acyl1,acyl2,everything()) %>% pivot_longer(cols = -c(1:4)) %>% group_by(name,acyl1) %>% mutate(mean1 = mean(value)) %>% ungroup() %>% group_by(name,acyl2) %>% mutate(mean2 = mean(value)) %>% ungroup()
      #   sn1 <- dplyr::select(targetdiacyllipid,c(LbmClass,name,acyl1,mean1)) %>% distinct(name,acyl1,.keep_all = T)
      #   colnames(sn1) <- c("LbmClass","name","Metabolite name","value")
      #   sn2 <- dplyr::select(targetdiacyllipid,c(LbmClass,name,acyl2,mean2))%>% distinct(name,acyl2,.keep_all = T)
      #   colnames(sn2) <- c("LbmClass","name","Metabolite name","value")
      #   diacyl <- sn2 %>% pivot_wider(names_from = "name",values_from = "value")
      # }
      
      
      diacyl <- targetdiacyllipid %>% dplyr::select(-c(acyl1,acyl2))
      
      
    }
    else{
    }
    
    
    Lipidclassdata<- filter(diacyl , LbmClass == inputclass) %>% distinct(`Metabolite name`,.keep_all = TRUE)
    if(length(Lipidclassdata$LbmClass) != 0){
      d <- Lipidclassdata
      rowname <- as.matrix((d$`Metabolite name`))
      d <- dplyr::select(d,-c(`Metabolite name`,LbmClass))
      n <- nrow(d)
      BreaksList <- seq(-2,2, by = 0.5)
      n <- nrow(d)
      height <- n/10*3
      list <- c(-2,0,2)
      lipid <- d
      lipid <- lipid[,-1]
      rownames(lipid) <- rowname
      lipid <- scale_rows(lipid) %>% as.data.frame()
      lipid <- tibble::rownames_to_column(lipid, "Metabolite name")
      lipid <- pivot_longer(lipid,cols = -c(`Metabolite name`))
      lipid <- mutate(lipid, name = name %>% factor(levels = input$levels))
      ghm <- ggplot(lipid, aes(x = name, y =`Metabolite name` , fill = value))
      ghm <- ghm + geom_tile(color = "black") + scale_fill_gradientn(
        colors=c("navy","white","red"),limits=c(min(lipid$value), max(lipid$value))
      ) + theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.text = element_text(size = input$Fontsize))
      print(ghm)
      
    }}
  
  if(unique(lipid_data_classmean$LbmClass) == "ASM"){
    targetnacylchain <- c("16:0","16:1","18:0", "20:0", "22:0", "24:0", "24:1", "26:0", "26:1")
    diacyl <- lipid_data_classmean %>% filter(LbmClass == "ASM")# %>%
    targetlipidclass <- diacyl$LbmClass %>% unique()
    diacyl <- diacyl[,-c(3:7)]
    
    if(input$acylfilter == T){
      
      acyl1list <- list()
      for (i in 1:nrow(diacyl)) {
        if(str_detect(diacyl$`Metabolite name`[i],pattern = "\\(") == TRUE){
          a <- str_split(diacyl$`Metabolite name`[i],pattern = "\\(") %>% unlist()
          a1 <- a[2] %>%  str_split(pattern = "\\)") %>% unlist()
          a2 <- a1[1] %>% str_split(pattern = " ") %>% unlist()
          acyl1list[i] <- a2[2]
        }
        else{
          acyl1list[i] <- NA
        }
      }
      
      diacyl$acyl1 <- unlist(acyl1list)
      targetdiacyllipid <- diacyl %>%  filter(acyl1 %in% targetnacylchain) %>% dplyr::select(-c(Categories))
      
      targetlipidclass <- unique(targetdiacyllipid$LbmClass)
      
      
      
      diacyl <- targetdiacyllipid %>% dplyr::select(-c(acyl1))
      
      
      
    }
    else{
      
    }
    
    
    Lipidclassdata<- filter(diacyl , LbmClass == inputclass) %>% distinct(`Metabolite name`,.keep_all = TRUE)
    if(length(Lipidclassdata$LbmClass) != 0){
      d <- Lipidclassdata
      rowname <- as.matrix((d$`Metabolite name`))
      d <- dplyr::select(d,-c(`Metabolite name`,LbmClass))
      n <- nrow(d)
      BreaksList <- seq(-2,2, by = 0.5)
      n <- nrow(d)
      height <- n/10*3
      list <- c(-2,0,2)
      lipid <- d
      lipid <- lipid[,-1]
      rownames(lipid) <- rowname
      lipid <- scale_rows(lipid) %>% as.data.frame()
      lipid <- tibble::rownames_to_column(lipid, "Metabolite name")
      lipid <- pivot_longer(lipid,cols = -c(`Metabolite name`))
      lipid <- mutate(lipid, name = name %>% factor(levels = input$levels))
      ghm <- ggplot(lipid, aes(x = name, y =`Metabolite name` , fill = value))
      ghm <- ghm + geom_tile(color = "black") + scale_fill_gradientn(
        colors=c("navy","white","red"),limits=c(min(lipid$value), max(lipid$value))
      ) + theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.text = element_text(size = input$Fontsize))
      print(ghm)
      
    }
  }
  if(unique(lipid_data_classmean$TotalChain) == "0"){
    targetnacylchain <- c("16:0","16:1","18:0", "20:0", "22:0", "24:0", "24:1", "26:0", "26:1")
    diacyl <- lipid_data_classmean %>% filter(TotalChain == "0")# %>%
    targetlipidclass <- diacyl$LbmClass %>% unique()
    diacyl <- diacyl[,-c(3:7)]
    if(input$acylfilter == T){
      
      acyl1list <- list()
      acyl2list <- list()
      
      for (i in 1:nrow(diacyl)) {
        acyl1list[i] <- NA
      }
      
      diacyl$acyl1 <- unlist(acyl1list)
      targetdiacyllipid <- diacyl %>%  filter(acyl1 %in% targetnacylchain) %>% dplyr::select(-c(Categories))
      targetlipidclass <- unique(targetdiacyllipid$LbmClass)
      
      namelist <- list()
      pathlist <- list()
      
      diacyl <- targetdiacyllipid %>% dplyr::select(-c(acyl1))
      
      
      
    }
    else{
      
    }
    
    Lipidclassdata<- filter(diacyl , LbmClass == inputclass) %>% distinct(`Metabolite name`,.keep_all = TRUE)
    if(length(Lipidclassdata$LbmClass) != 0){
      d <- Lipidclassdata
      rowname <- as.matrix((d$`Metabolite name`))
      d <- dplyr::select(d,-c(`Metabolite name`,LbmClass))
      n <- nrow(d)
      BreaksList <- seq(-2,2, by = 0.5)
      n <- nrow(d)
      height <- n/10*3
      list <- c(-2,0,2)
      lipid <- d
      lipid <- lipid[,-1]
      rownames(lipid) <- rowname
      lipid <- scale_rows(lipid) %>% as.data.frame()
      lipid <- tibble::rownames_to_column(lipid, "Metabolite name")
      lipid <- pivot_longer(lipid,cols = -c(`Metabolite name`))
      lipid <- mutate(lipid, name = name %>% factor(levels = input$levels))
      ghm <- ggplot(lipid, aes(x = name, y =`Metabolite name` , fill = value))
      ghm <- ghm + geom_tile(color = "black") + scale_fill_gradientn(
        colors=c("navy","white","red"),limits=c(min(lipid$value), max(lipid$value))
      ) + theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.text = element_text(size = input$Fontsize))
      print(ghm)
      
    }}
  if(unique(lipid_data_classmean$TotalChain) > 2 && unique(lipid_data_classmean$LbmClass) %in% c("TG","OxTG","HBMP","EtherTG")){
    targettriacylchain <- c("16:0","16:1","18:0","18:1","18:2","18:3","20:3","20:4","20:5","22:4","22:5","22:6")
    Triacyl <- lipid_data_classmean %>% filter(TotalChain > 2, LbmClass %in% c("TG","OxTG","HBMP","EtherTG"))
    targetlipidclass <- Triacyl$LbmClass %>% unique()
    Triacyl <- Triacyl[,-c(3:7)]
    if(input$acylfilter == T){
      acyllist1 <- list()
      acyllist2 <- list()
      acyllist3 <- list()
      for (i in 1:nrow(Triacyl)) {
        if(Triacyl$LbmClass[i] == "TG"){
          
          a <-  unlist(str_split(Triacyl$`Metabolite name`[i],pattern = "\\|"))[2] %>% str_split(pattern = " ")
          a <- unlist(a)[2] %>% str_split(pattern = "_") %>% unlist()
          acyllist1[i] <- a[1]
          acyllist2[i] <- a[2]
          acyllist3[i] <- a[3]
        }else if(Triacyl$LbmClass[i] =="OxTG"){
          
          a <-  unlist(str_split(Triacyl$`Metabolite name`[i],pattern = "\\|"))[2] %>% str_split(pattern = " ")
          a <- unlist(a)[2] %>% str_split(pattern = "_") %>% unlist()
          acyllist1[i] <- a[1]
          acyllist2[i] <- a[2]
          b <- a[3] %>% str_split(pattern = ";") %>% unlist()
          acyllist3[i] <- b[1]
        }else if(Triacyl$LbmClass[i] =="HBMP"){
          a <-  unlist(str_split(Triacyl$`Metabolite name`[i],pattern = "\\|"))[2] %>% str_split(pattern = " ")
          a <- unlist(a)[2] %>% str_split(pattern = "_") %>% unlist()
          b <- a[1] %>% str_split(pattern = "/") %>% unlist()
          acyllist1[i] <-  b[1]
          acyllist2[i] <-  b[2]
          acyllist3[i] <-  a[2]
        }else if(Triacyl$LbmClass[i] =="EtherTG"){
          a <-  unlist(str_split(Triacyl$`Metabolite name`[i],pattern = "\\|"))[2] %>% str_split(pattern = " ")
          a <- unlist(a)[2] %>% str_split(pattern = "_") %>% unlist()
          acyl <- a[1] %>% str_split(pattern = "-") %>% unlist()
          acyllist1[i] <- acyl[2]
          acyllist2[i] <- a[2]
          acyllist3[i]<- a[3]
        }
      }
      
      Triacyl$acyl1 <- unlist(acyllist1)
      Triacyl$acyl2 <- unlist(acyllist2)
      Triacyl$acyl3 <- unlist(acyllist3)
      
      targettriacyllipid <- Triacyl %>% filter(acyl1 %in% targettriacylchain, acyl2 %in% targettriacylchain,acyl3 %in% targettriacylchain) %>% dplyr::select(-c(acyl1,acyl2,acyl3,Categories))
    }
    else{
      targettriacyllipid <- Triacyl
    }
    Triacyl <- targettriacyllipid
    # i <-
    Lipidclassdata<- filter(Triacyl , LbmClass == inputclass) %>% distinct(`Metabolite name`,.keep_all = TRUE)
    if(length(Lipidclassdata$LbmClass) != 0){
      d <- Lipidclassdata
      rowname <- as.matrix((d$`Metabolite name`))
      d <- dplyr::select(d,-c(`Metabolite name`,LbmClass))
      BreaksList <- seq(-2,2, by = 0.5)
      n <- nrow(d)
      height <- n/10*3
      list <- c(-2,0,2)
      lipid <- d
      lipid <- lipid[,-1]
      rownames(lipid) <- rowname
      lipid <- scale_rows(lipid) %>% as.data.frame()
      lipid <- tibble::rownames_to_column(lipid, "Metabolite name")
      lipid <- pivot_longer(lipid,cols = -c(`Metabolite name`))
      lipid <- mutate(lipid, name = name %>% factor(levels = input$levels))
      ghm <- ggplot(lipid, aes(x = name, y =`Metabolite name` , fill = value))
      ghm <- ghm + geom_tile(color = "black") + scale_fill_gradientn(
        colors=c("navy","white","red"),limits=c(min(lipid$value), max(lipid$value))
      ) + theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.text = element_text(size = input$Fontsize))
      print(ghm)
      
    }}
  if(unique(lipid_data_classmean$TotalChain) > 2 && ! unique(lipid_data_classmean$LbmClass) %in% c("TG","OxTG","HBMP","EtherTG")){
    Triacyl <- lipid_data_classmean %>% filter(TotalChain > 2, ! LbmClass %in% c("TG","OxTG","HBMP","EtherTG"))
    targetlipidclass <- Triacyl$LbmClass %>% unique()
    Triacyl <- Triacyl[,-c(3:7)]
    diacyl <- Triacyl
    Lipidclassdata<- filter(diacyl , LbmClass == inputclass) %>% distinct(`Metabolite name`,.keep_all = TRUE)
    if(length(Lipidclassdata$LbmClass) != 0){
      d <- Lipidclassdata
      rowname <- as.matrix((d$`Metabolite name`))
      d <- d <- dplyr::select(d,-c(`Metabolite name`,LbmClass))
      n <- nrow(d)
      BreaksList <- seq(-2,2, by = 0.5)
      n <- nrow(d)
      height <- n/10*3
      list <- c(-2,0,2)
      lipid <- d
      lipid <- lipid[,-1]
      rownames(lipid) <- rowname
      lipid <- scale_rows(lipid) %>% as.data.frame()
      lipid <- tibble::rownames_to_column(lipid, "Metabolite name")
      lipid <- pivot_longer(lipid,cols = -c(`Metabolite name`))
      lipid <- mutate(lipid, name = name %>% factor(levels = input$levels))
      ghm <- ggplot(lipid, aes(x = name, y =`Metabolite name` , fill = value))
      ghm <- ghm + geom_tile(color = "black") + scale_fill_gradientn(
        colors=c("navy","white","red"),limits=c(min(lipid$value), max(lipid$value))
      ) + theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.text = element_text(size = input$Fontsize))
      print(ghm)
      
    }}
  
}