# Plasma QC tracking UI

ui <- fluidPage(shinytheme("journal"),
                useShinyjs(),
                titlePanel("DFI Metabolomics Plasma QC tracking"),
                
                tabsetPanel(
                  tabPanel("TMS-MOX",
                           br(),
                           fluidRow(plotlyOutput("plot_tms1")),
                           br(), hr(), br(),
                           fluidRow(plotlyOutput("plot_tms2")),
                           br(), hr(), br(),
                           fluidRow(plotlyOutput("plot_tms3")),
                           br(), br()
                  ),
                  tabPanel("PFBBr",
                           fluidRow(plotlyOutput("plot_pfbbr"))
                  ),
                  tabPanel("Tryptophan",
                           fluidRow(plotlyOutput("plot_tryptophan"))
                  ),
                  tabPanel("Bile-acids",
                           fluidRow(plotlyOutput("plot_bile"))
                  )
                  )
)


