source("./modules/librarys.R")
source("./modules/data_processing.R")
source("./modules/plot_utils.R")
source("./modules/heatmap_utils.R")
source("./modules/modaldialog_utils.R")
source("./modules/selectcolor_utils.R")
source("./modules/ui_modules.R")

shinyUI(
  fluidPage(
    useShinyjs(),
    tags$head(
      tags$style(HTML(
        "
        .well { background-color: initial; border: initial; }
        .navbar { margin-bottom: 5px; }
        .container-fluid { padding-right: 10px; padding-left: 10px; }
        #sidebar {
          position: fixed;
          left: 0;
          width: 250px;
          height: calc(100% - 160px);  
          top: 160px;  
          background: #f8f9fa;
          transition: all 0.3s;
          padding: 15px;
          z-index: 1000;
        }
        #sidebar.inactive {
          left: -250px;
        }
		#content {
        margin-left: 60px;
        transition: all 0.3s;
        height: calc(100vh - 50px);
      }
	  #content.active {
        margin-left: 310px;
      }
        #main-content {
          margin-left: 250px;
          transition: all 0.3s;
          padding-top: 80px;  
        }
        #main-content.inactive {
          margin-left: 0;
        }
        #toggle_sidebar {
          position: fixed;
          left: 260px;
          top: 160px;  
          z-index: 1001;
          width: 40px;
          height: 40px;
          background: #343a40;
          color: #fff;
          border: none;
          border-radius: 5px;
          transition: all 0.3s;
        }
        #toggle_sidebar.inactive {
          left: 10px;
        }
        #graphContainer {
          height: 100%;
          width: 100%;
        }
        #tab3 {
          margin-top: 70px;
        }
        "
      ))
    ),
    navbarPage(
      "MSDIAL2Cytoscape",
      theme = shinytheme("yeti"),
      tabPanel(
        waiter::useWaiter(),
        uiOutput("dynamicSidebar"),
        uiOutput("dynamicSidebarToggle"),
        tabsetPanel(
          id = "mainTabs",
          uploadpanel,
          tabPanel(
            value = "tab2",
              tagList(
    div(
      style = "display: flex; align-items: center; justify-content: center; margin-bottom: 15px;",
      img(src = "./barplot.png", style = "width: 50px; height: 50px; margin-right: 10px;"),
      div(style = "font-size: 18px; color: #000000; font-weight: bold;", "Plot")
    )
  ),
            fluidRow(
              column(
                width = 12,
				createCustomSidebar("y", "w", "z", "alpha", "size", "Fontsize", "levels", "pvaluecheck", "q"),
                plotTabAppearanceUI(
                  "", "./bar.png", "50px", "50px", "plottest", "plottest2", "testtext"
                )
              )
            )
          ),
          tabPanel(
  value = "tab3",   

    tagList(
    div(
      style = "display: flex; align-items: center; justify-content: center; margin-bottom: 15px;",
      img(src = "./network2.png", style = "width: 50px; height: 50px; margin-right: 10px;"),
      div(style = "font-size: 18px; color: #000000; font-weight: bold;", "Pathway analysis")
    )
  ),

 actionButton("toggle_sidebar", icon("bars"), class = "btn-info"), 

    div(
      id = "sidebar",
      wellPanel(
        id = "pathway-controls",
        selectInput("pathwaytype", "Pathway type", 
                    choices = c("Global pathway", "Ceramide pathway", "Remodeling pathway"), 
                    selected = "Global pathway"),
        actionButton("pathway", "Pathway mapping", class = "btn-primary btn-block"),
        actionButton("save_pdf", "Save as PNG", class = "btn-success btn-block"),
        downloadButton("exportCYJS", "Export CYJS", class = "btn-block"),
        downloadButton("exportStyles", "Export styles.xml", class = "btn-block"),
        checkboxInput("viewacyllevel", "Close-up nodes", value = TRUE)
      )
    ),
        div(
      id = "main-content",
      fluidRow(
        column(
          width = 9,
          div(id = "content", 
              uiOutput("graphContainer")
          )
        ),
        column(
  width = 3,

  actionButton("getSelectedNodes", "Click nodes in the Network and Confirm Selection", class = "btn-primary btn-block"),
  br(), 
  tabsetPanel(
    id = "sideTabset",
    tabPanel("Lipid Species Visualization",
      div(id = "tabContent",
	  wellPanel(
        selectInput(
          inputId = "selectmol",
          label = "Select molecule to display",
          choices = c(" "),
          selected = " "
        ),
        plotOutput(outputId = "corselect2")),
		wellPanel(
		plotOutput(outputId = "heatmap",height = "600px" ,width = "100%"),
        checkboxInput(inputId = "acylfilter", label = "Filtering common acylchains (16:0, 16:1, 18:0, 18:1, 18:2, 18:3, 20:3, 20:4, 20:5, 22:4, 22:5, 22:6)", value = TRUE),
        checkboxInput(inputId = "sn", label = "sn", value = FALSE))
      )
    ),
    tabPanel("Correlation Plot",
      div(id = "tabContent",
	  wellPanel(
	  textOutput("textOutput"), 
	  br(),
	  plotlyOutput(outputId = "corselect")),
        checkboxInput(inputId = "ClassorMol", label = "Correlation of lipid species", value = FALSE),
        selectInput(
          inputId = "X1",
          label = "X",
          choices = c(" "),
          selected = " "
        ),
        selectInput(
          inputId = "X2",
          label = "Y",
          choices = c(" "),
          selected = " "
        )
      )
    )
  )
)
      )
    ),
    tags$script(HTML(
      "
      function toggleSidebar() {
        var sidebar = $('#sidebar');
        var mainContent = $('#main-content');
        var toggleBtn = $('#toggle_sidebar');
        
        sidebar.toggleClass('inactive');
        mainContent.toggleClass('inactive');
        toggleBtn.toggleClass('inactive');
        
        if (sidebar.hasClass('inactive')) {
          toggleBtn.find('i').removeClass('fa-times').addClass('fa-bars');
        } else {
          toggleBtn.find('i').removeClass('fa-bars').addClass('fa-times');
        }
      }
      "
    ))
)
        )
      )
    )
  )
)



