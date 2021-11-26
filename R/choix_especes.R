choix_especes_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    actionButton(ns("especes_bttn"), "Espèces")
    # , actionButton(ns("dev"), "DEV")
  )
}

choix_especes <- function(id, listeEspeces) {
  moduleServer(id, function(input, output, session) {
    # dev
    # observeEvent(input$dev, {
    #   browser()
    # })
    
    # Get species list
    SpeciesList <- fread("SpeciesList.csv", encoding = "Latin-1")
    SpeciesList$color=factor(SpeciesList$Esp)
    #groupes=unique(SpeciesList$GroupFR)
    especes=unique(SpeciesList$Esp)
    # List, not reactive !
    listeEspeces <- SpeciesList %>% select(c("Esp", "Group"))
    
    # Display UI
    observeEvent(input$especes_bttn, {
      x <- try(showModal(
        modalDialog(
          title = "Choix des espèces",
          tags$div(
            style = "columns: 4",
            lapply(
              unique(listeEspeces$Group),
              function(group) {
                # check if it is first time selecting species
                if(length(selectedSpecies()) != 0) { # loads saved
                  items <- selectedSpecies()[[group]]
                } else # loads all
                  items <- listeEspeces %>% 
                    filter(Group == group) %>% 
                    select(Esp) %>%
                    unlist() %>%
                    unname()
                
                tagList(
                  checkboxGroupInput(
                    session$ns(group),
                    actionLink(
                      sprintf("%s-all", session$ns(group)),
                      group
                    ),
                    selected = items,
                    choices = listeEspeces %>% 
                      filter(Group == group) %>% 
                      select(Esp) %>%
                      unlist() %>%
                      unname()
                  )
                )
              }
            ) # end of lapply
          ) # end of div
        )
      )) # end of showmodal
      if(class(x) == "try-error") browser()
    })
    
    # select all species
    # one observer created per group
    sapply(
      unique(listeEspeces$Group),  
      function(group) {
      observeEvent(
        input[[paste0(group, "-all")]], 
        {
          all_items <- listeEspeces %>%
            filter(Group == group) %>%
            select(Esp) %>% 
            unlist() %>%
            unname()
          # browser()
          updateCheckboxGroupInput(
            session = session,
            group,
            selected = if(length(input[[group]]) == length(all_items))
              character(0) else
                all_items
          )
        }
      )
    })
    
    # get selected species
    selectedSpecies <- eventReactive({
      sapply(unique(listeEspeces$Group), function(g) input[[g]])
    }, {
      .selected <- sapply(unique(listeEspeces$Group), function(g) input[[g]])
      .selected[lengths(.selected) != 0] # lengthS with a S ! 
    })
    
    return(selectedSpecies)
  })
}

# TEST =====
library(shiny)

ui <- fluidPage(
  choix_especes_UI("especes"),
  textOutput("dims")
)

server <- function(input, output, session) {
  selectedSpecies <- choix_especes("especes")
  output$dims <- renderText({lengths(selectedSpecies())})
}

shinyApp(ui, server)