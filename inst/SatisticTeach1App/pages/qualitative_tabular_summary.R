tabular_summary <- tabPanel("Tabular summary",
                    br(),
                    conditionalPanel(condition = "input.GrupoQuali == 0",
                      strong(h2("Frequency table")),
                      br(),
                      DT::DTOutput("TfreqQuali")
                    ),
                    conditionalPanel(condition = "input.GrupoQuali == 1",
                      strong(h2("Two-way table")),
                      br(),
                      DT::DTOutput("TwofreqQuali") 
                  )
)