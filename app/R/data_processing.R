##' Checking duplicated metabolites from alignment table
##'
##' @title Checking duplicated metabolites from alignment table
##' @param data Lipidomics alignment file form MS-DIAL analysis
##' @return The alignment table with duplicated metabolites removed.
##' @examples
##' data <- read.csv("D:/100cells/20240222/lipid_metabolome.csv",header = F)
##' result <- Metabolite_duplicate_check(data)
##' @references 
##' @author Takaki Oka
##' @export
Metabolite_duplicate_check <- function(data){
  # convert rownames to column for row index
  
  ontology_column <- 12
  batch_id_row <- 2
  peakinfo_column <- 1:35
  header_row <- 5
  peaktable <- data[, !data[batch_id_row, ] %in% NA, drop = FALSE]
  originalcolnum <- colnames(peaktable)
  peaktable <- peaktable %>%
    setNames(peaktable[5,]) %>%
    rownames_to_column()
  
  # Find rows with max 'Fill %' for each 'Metabolite name'
  check_duplicates <- peaktable %>%
    group_by(`Metabolite name`) %>%
    slice(which.max(`Fill %`)) %>%
    ungroup()
  
  id <- check_duplicates$rowname
  datav2 <- peaktable %>%
    filter(rowname %in% c(id, 1:5)) %>%
    select(-rowname)
  colnames(datav2) <- originalcolnum
  return(datav2)
}

##' Converting alignment file
##'
##' @title Converting alignment file to dataframe
##' @param data alignment file form MS-DIAL analysis
##' @return dataframe of alignment file
##' @examples
##' process_alignment_file(data)
##' @references 
##' @author Takaki OKA
##' @export
process_alignment_file <- function(data) {
  ontology_column <- 12
  batch_id_row <- 2
  peakinfo_column <- 1:35
  header_row <- 5
  data <- Metabolite_duplicate_check(data)
  #data <- Median_normalize_to_alignment_format(data)
  peaktable <- data[, !data[batch_id_row, ] %in% NA, drop = FALSE]
  peaktable <- peaktable[, peaktable[batch_id_row, ] == "Sample", drop = FALSE]
  sample_info <- data.frame(name = unlist(peaktable[5, ]),
                            Class = unlist(peaktable[1, ]))
  
  peaktable <- cbind(data[, peakinfo_column], peaktable)
  colnames(peaktable) <- peaktable[header_row, ]
  peaktable <- peaktable[-c(1:header_row), ] 
  
  peaktable[, -peakinfo_column] <- data.frame(lapply(peaktable[, -peakinfo_column], as.numeric)) 
  peaktable <-  distinct(peaktable,`Metabolite name`,.keep_all = TRUE)
  lipid_info <- peaktable[, colnames(peaktable) %in% c("Metabolite name","Ontology",sample_info$name)]
  peak_info <- peaktable[, !colnames(peaktable) %in% c("Metabolite name", sample_info$name)]
  
  return(list(lipid_info,sample_info,peak_info))
}

##' Median normalizeing of alignment table wuth retaining alignment format
##'
##' @title Median normalizeing of alignment table
##' @param data Lipidomics alignment file form MS-DIAL analysis
##' @return Median normalized data with alignment format
##' @examples
##' data <- read.csv("D:/100cells/20240222/lipid_metabolome.csv",header = F)
##' result <- Median_normalize(data)
##' @references 
##' @author Nami Sakamoto, Takaki Oka
##' @export
Median_normalize_to_alignment_format <- function(data){
  
  # Extract lipidontinf
  peakinfo <- data[5:nrow(data), c(1:35)]
  lipidontinf <- data[5:nrow(data), c(4, 12)]
  colnames(lipidontinf) <- lipidontinf[1,]
  lipidontinf <- lipidontinf[-1,]
  rownames(lipidontinf) <- NULL
  
  # Extract sample information
  sampleinf <- t(data[1:4, 35:ncol(data)])
  sampledata <- t(data[5:nrow(data), 36:ncol(data)])
  Metabolitename <- t(data[-c(1:4), 4])
  
  # Combine sample information
  colnames(sampledata) <- colnames(Metabolitename)
  datav2_v2 <- cbind(data.frame(sampleinf, rbind(data.frame(Metabolitename), data.frame(sampledata))))
  colnames(datav2_v2) <- datav2_v2[1,]
  datav2_v2 <- datav2_v2[-1,]
  rownames(datav2_v2) <- NULL
  names(datav2_v2)[5] <- "name"
  
  # Filter out 'Batch ID' values and process numeric data
  lipidmetabolomedata_all <- datav2_v2 %>%
    filter(!`Batch ID` %in% c('Average', 'Stdev'))
  numericdata <- lipidmetabolomedata_all[, -c(1:4)]
  rownames(numericdata) <- NULL
  numericdatax <- as.data.frame(sapply(numericdata[, -1], as.numeric))
  rownames(numericdatax) <- lipidmetabolomedata_all$name
  numericdatax2 <- numericdatax %>%
    tibble::rownames_to_column() %>%
    dplyr::rename(sampleid = rowname) %>%
    pivot_longer(!sampleid, names_to = "lipidname", values_to = "value")
  
  log2value <- numericdatax2 %>%
    mutate(log2value = log2(value)) %>%
    dplyr::select(-value) %>%
    pivot_wider(names_from = "lipidname", values_from = "log2value") %>%
    column_to_rownames(var = "sampleid")
  
  samplemedian <- log2value %>% apply(1,median) %>% as.data.frame()
  colnames(samplemedian) <- "median"
  
  mean_forsamplemedian <- mean(samplemedian$median)
  
  log2mediannormalizedata <- rownames_to_column(log2value) %>%
    dplyr::rename(sampleid = rowname) %>%
    left_join(rownames_to_column(samplemedian), by = c("sampleid" = "rowname")) %>%
    pivot_longer(!c(sampleid, median), names_to = "lipidname", values_to = "log2value") %>%
    mutate(mediannormvalue = log2value - median + mean_forsamplemedian) %>%
    dplyr::select(-c(log2value, median)) %>%
    pivot_wider(names_from = "lipidname", values_from = "mediannormvalue") %>%
    column_to_rownames(var = "sampleid")
  
  log2_2xmediannormalizedata <- 2^log2mediannormalizedata
  log2_2xmediannormalizedata_0 <- lipidmetabolomedata_all[, c(1:5)] %>%
    left_join(rownames_to_column(log2_2xmediannormalizedata), by = c("name" = "rowname")) %>% t() %>% data.frame()%>% rownames_to_column("V0")
  aligntable <- right_join(peakinfo,log2_2xmediannormalizedata_0,by = c("V4" = "V0"))
  headerrow <- tail(aligntable,n = 5)
  aligntable <- rbind(headerrow,aligntable)
  aligntable[1:5,1:35] <- data[1:5,1:35]
  aligntable <- filter(aligntable,is.na(V1)==F)
  return(aligntable)
}
##' Converting alignment file to expression data of lipid class
##'
##' @title Converting alignment file to dataframe
##' @param data alignment file form MS-DIAL analysis
##' @return Expression data of lipid class and sample meta data 
##' @examples
##' convert_msdial_export_to_lipid_class_dataframe(data)
##' @references 
##' @author Takaki OKA
##' @export
convert_msdial_export_to_lipid_class_dataframe <- function(data) {
  data_frame <- process_alignment_file(data)
  lipid_info <- data_frame[[1]]
  sample_info <- data_frame[[2]]
  
  lipidtable <- lipid_info[,colnames(lipid_info) %in% c("Metabolite name","Ontology", sample_info$name)] %>% pivot_longer(cols = -(1:2)) 
  lipidtable <- lipidtable %>%
    group_by(name, Ontology) %>%
    mutate(mean = mean(value)) %>%
    ungroup() %>%
    distinct(name, Ontology, .keep_all = TRUE) %>%
    select(-c("Metabolite name", value)) %>%
    pivot_wider(values_from = mean, names_from = Ontology)
  return(list(lipidtable,sample_info))
}

##' Converting alignment file to expression data of lipid molecules
##'
##' @title Converting alignment file to dataframe
##' @param data alignment file form MS-DIAL analysis
##' @return Expression data of lipid molecules and sample meta data 
##' @examples
##' convert_msdial_export_to_lipid_molecules_dataframe(data)
##' @references 
##' @author Takaki OKA
##' @export
convert_msdial_export_to_lipid_molecules_dataframe <- function(data) {
  data_frame <- process_alignment_file(data)
  lipid_info <- data_frame[[1]]
  sample_info <- data_frame[[2]]
  
  lipidtable <- lipid_info[,colnames(lipid_info) %in% c("Metabolite name", sample_info$name)] %>% pivot_longer(cols = -(1))  %>%  pivot_wider(names_from = `Metabolite name`,values_from = value) 
  return(list(lipidtable,sample_info))
}

##' Updating select input with file upload
##'
##' @title Updating select input with file upload
##' @param data alignment file form MS-DIAL analysis
##' @return Expression data of lipid molecules and sample meta data 
##' @examples
##' convert_msdial_export_to_lipid_molecules_dataframe(data)
##' @references 
##' @author Takaki OKA
##' @export
processAndUpdateInputs <- function(data, session, metadata, metainfocol) {
  shiny::updateSelectInput(session, "y", selected = paste(colnames(data)[c(metainfocol + 1)]), choices = colnames(data)[-c(1:metainfocol)])
  shiny::updateSelectInput(session, "w", selected = "Class", choices = colnames(data)[c(2:metainfocol)])
  shiny::updateSelectInput(session, "z", selected = "Class", choices = colnames(data)[c(2:metainfocol)])
}

processAndUpdateInputs2 <- function(data, session, metadata, metainfocol) {
  #shiny::updateSelectInput(session, "y", selected = paste(colnames(data)[c(metainfocol + 1)]), choices = colnames(data)[-c(1:metainfocol)])
  shiny::updateSelectInput(session, "X1", selected = paste(colnames(data)[c(metainfocol + 1)]), choices = colnames(data)[-c(1:metainfocol)])
  shiny::updateSelectInput(session, "X2", selected = paste(colnames(data)[c(metainfocol + 1)]), choices = colnames(data)[-c(1:metainfocol)])
  #shiny::updateSelectInput(session, "z", selected = "Class", choices = colnames(data)[c(2:metainfocol)])
}
# update_select_input <- function(input,session) {
# 
#     
#   
# }

scale_rows <- function(x) {
  t(apply(x, 1, function(row) scales::rescale(row, to = c(-2, 2))))
}

lipidmeancalforgroupnode <- function(data, metadata, selectclass) {
  Ontology_column <- 12
  BatchID_row <- 2
  peakinfo_column <- 1:35
  header_row <- 5
  processed_data <- process_alignment_file(data)
  lipid_data_classmean <- processed_data[[1]]
  sampleinfo <- processed_data[[2]]
  lipid_data_lipidclassmean <- pivot_longer(lipid_data_classmean, cols = -c(1:2)) %>%
    select(`Metabolite name`, Ontology, name, value) %>%
    inner_join(metadata, by = "name") %>%
    select(`Metabolite name`,Ontology ,selectclass, name, value) %>%
    group_by(`Metabolite name`, across(all_of(selectclass))) %>%
    mutate(mean = mean(value)) %>%
    ungroup() %>%
    distinct(`Metabolite name`,across(all_of(selectclass)), .keep_all = TRUE) %>%
    select(`Metabolite name`, Ontology, selectclass, mean) %>%
    pivot_wider(names_from = selectclass, values_from = mean)
  return(lipid_data_lipidclassmean)
}

processSampleInRows <- function(originaldata, session, input) {
  colnames(originaldata) <- originaldata[1,]
  originaldata <- originaldata[-1,]
  originaldata[, -c(1:2)] <- apply(originaldata[, -c(1:2)], 2, as.numeric)
  lipidont <- read.csv(input$ontfile$datapath, check.names = FALSE)
  colnames(lipidont)[1] <- "lipid"
  colnames(originaldata)[1] <- "name"
  if (length(input$file2) != 0) {
    metadata <- read.csv(input$file2$datapath)
    colnames(metadata)[1] <- "name"
    data <- originaldata %>%
      pivot_longer(cols = -c(1:2), names_to = "lipid") %>%
      inner_join(lipidont, by = c("lipid")) %>%
      group_by(name, Ontology) %>%
      mutate(mean = mean(value)) %>%
      ungroup() %>%
      distinct(name, Ontology, .keep_all = TRUE) %>%
      select(1, 2, 5, 6) %>%
      pivot_wider(names_from = "Ontology", values_from = "mean")
    data <- inner_join(metadata, data, by = c("name" = "name"))
  } else {
    metadata <- data.frame(name = originaldata[,1],Class = originaldata[,2])
    data <- originaldata %>%
      pivot_longer(cols = -c(1:2), names_to = "lipid") %>%
      inner_join(lipidont, by = c("lipid")) %>%
      group_by(name, Ontology) %>%
      mutate(mean = mean(value)) %>%
      ungroup() %>%
      distinct(name, Ontology, .keep_all = TRUE) %>%
      select(1, 5, 6) %>%
      pivot_wider(names_from = "Ontology", values_from = "mean")
    data <- inner_join(metadata, data, by = c("name" = "name"))
  }
  return(list(data,metadata))
}

processMSDIALExport <- function(originaldata, session, input) {
  if (length(input$file2) != 0) {
  tablelist <- originaldata %>% convert_msdial_export_to_lipid_class_dataframe()
  data <- tablelist[[1]]
  metadata <- read.csv(input$file2$datapath)
  colnames(metadata)[1] <- "name"
  metadata <- inner_join(tablelist[[2]], metadata, by = c("name"))
  data <- inner_join(metadata, data, by = c("name" = "name"))
  metainfocol <- ncol(metadata)
  print(data)
  } else {
  tablelist <- originaldata %>% convert_msdial_export_to_lipid_class_dataframe()
  data <- tablelist[[1]]
  metadata <- tablelist[[2]]
  metainfocol <- ncol(metadata)
  data <- inner_join(metadata, data, by = c("name" = "name"))
  }
  return(list(data,metadata))
}

processMSDIALExporttomoldata <- function(originaldata, session, input) {
  if (length(input$file2) != 0) {
  tablelist <- originaldata %>% convert_msdial_export_to_lipid_molecules_dataframe()
  data <- tablelist[[1]]
  metadata <- read.csv(input$file2$datapath)
  colnames(metadata)[1] <- "name"
  metadata <- inner_join(tablelist[[2]], metadata, by = c("name"))
  data <- inner_join(metadata, data, by = c("name" = "name"))
  metainfocol <- ncol(metadata)
  } else {
  tablelist <- originaldata %>% convert_msdial_export_to_lipid_molecules_dataframe()
  data <- tablelist[[1]]
  metadata <- tablelist[[2]]
  metainfocol <- ncol(metadata)
  data <- inner_join(metadata, data, by = c("name" = "name"))
  }
  return(list(data,metadata))
}

processSampleInRowstomoldata <- function(originaldata, session, input) {
  moldata <- originaldata 
  colnames(moldata) <- moldata[1,]
  moldata <- moldata[, !duplicated(colnames(moldata))]
  moldata <- moldata[-1,]
  moldata[,-c(1,2)] <- apply(moldata[,-c(1,2)],2,as.numeric) %>% data.frame()
  colnames(moldata)[1] <- "name"
  if (length(input$file2) != 0) {
    metadata <- read.csv(input$file2$datapath)
    colnames(metadata)[1] <- "name"
    data <- inner_join(metadata, moldata, by = c("name" = "name"))
  } else {
    metadata <- data.frame(name = originaldata[,1],Class = originaldata[,2])
    data <- moldata
  }
  return(list(data,metadata))
}

read_graph_json <- function(file_path) {
  tryCatch({
    paste(readLines(file_path), collapse = "")
  }, error = function(e) {
    message("Error reading graph JSON file: ", e$message)
    return(NULL)
  })
}


pvaluecheckbox =reactiveVal()
pvaluecheckbox <<- ""