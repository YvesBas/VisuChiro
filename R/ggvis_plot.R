#' Create a GGVIS scatterplot
#'
#' This function creates a GGVIS scatterplot and returns...
#'
#' @param data a `data.frame` with ...
#' @param AlleYoupi5 a `data.frame` with ...
#' @param AlleYoupi7 a `data.frame` with ...
#' @param AlleYoupi8 a `data.frame` with ...
#' @param submit the value of the submit button (e.g. `input$"input"`)
#' @param sp_corr ...
#' @param prb_corr ...
#' @param submit0 ...
#'
#' @return A list with the following elements:
#' - AlleYoupi5 ...,
#' - AlleYoupi7 ...,
#' - AlleYoupi8 ...,
#' - input
#' 
#' @export
#' @importFrom data.table as.data.table
#'
#' @examples
#' ## ...

ggvis_plot <- function(data = sp(), AlleYoupi5, 
                       AlleYoupi7, AlleYoupi8, submit,
                       sp_corr, prb_corr, submit0) {
  
  
  
  gplot <- data %>%
    
    ggvis::ggvis(~DateHeure, ~parametre, key:= ~Affiche) %>%
    
    ggvis::layer_points(size   = ~tadarida_probabilite * 20, 
                        fill   = ~factor(tadarida_taxon),
                        stroke = 1, 
                        shape  = ~factor(tadarida_taxon)) %>%
    
    ggvis::set_options(width   = 820, 
                       height  = 540, 
                       padding = padding(5, 90, 40, 120)) %>%
    
    ggvis::hide_legend("stroke") %>%
    ggvis::hide_legend("size") %>%
    
    ggvis::add_legend(c("shape", "fill"), title = "Especes") %>%
    
    ggvis::hide_legend("size") %>%
    
    ggvis::add_tooltip(function(data) {
      
      soundexe <- paste0(unlist(strsplit(data$"Affiche", " ")[1])[1], ".wav")
      
      #soundexe <- paste(wavdir, "\\", unlist(strsplit(data$Affiche, " ")[1])[1], ".wav", sep="");
      #ConfigSyrinx[7,1]=paste0("Sound file name=",soundexe);
      #ConfigSyrinx[8,1]=paste0("Sound file title=",basename(soundexe));
      #fwrite(ConfigSyrinx,"temp.dsp");
      #shell.exec("temp.dsp")}, "click") %>%
      #shell.exec(soundexe)}, "click") %>%
      
      clipr::write_clip(soundexe) }, "click") %>%

    add_tooltip(function(data) {
      
      qui <- which(AlleYoupi5$Affiche == data$Affiche) #affichage d'?tiquette en fonction de la position du curseur
      output$"table2" <- shiny::renderTable(AlleYoupi5[qui, ])
      
      shiny::reactiveValues()
      
      if (submit > submit0) { #si on a cliqu? sur "valider"
        
        AlleYoupi5[qui, 8:9] <- shiny::isolate(c(sp_corr, prb_corr))
        
        if (!exists("AlleYoupi8")) {
          AlleYoupi8 <- AlleYoupi5[0, ]
        } #tableau qui s'affiche dans le dernier onglet de l'appli (validations faites)
        
        AlleYoupi8 <- shiny::isolate(unique(rbind(AlleYoupi5[qui, ], AlleYoupi8))) #incr?mente les validations dans AlleYoupi8
        #AlleYoupi7 <- shiny::isolate(AlleYoupi5) #tableau avec validations ? sauver
        AlleYoupi7 <- shiny::isolate(unique((rbind(AlleYoupi8, AlleYoupi5)))) #tableau avec validations ? sauver
        AlleYoupi7 <- unique(as.data.table(AlleYoupi7), by = c("nom du fichier","tadarida_taxon"))
        AlleYoupi7 <- AlleYoupi7[order(AlleYoupi7$`nom du fichier`), ]
        submit0 <- submit
      }
      
      output$table3 <- shiny::renderDataTable({ AlleYoupi8 }) #affiche AlleYoupi8 dans le dernier onglet
      output$table4 <- shiny::renderDataTable({ AlleYoupi7 }) #affiche AlleYoupi8 dans le dernier onglet
      
      #  Sauver imm?diatement cette table modifi?e.
    }, "click") %>%
    
    ggvis::add_tooltip(function(data) { paste0(data$Affiche) }, "hover") #%>% #d?finir l'affichage quand on "survole" des points dans le graphe
    
    # bind_shiny("plot", "plot_ui")
    list("gplot"      = gplot,
         "AlleYoupi5" = AlleYoupi5, 
         "AlleYoupi7" = AlleYoupi7, 
         "AlleYoupi8" = AlleYoupi8, 
         "submit0"    = submit0)
}


## Function example ----

# mysp <- readr::read_rds(here::here("sp_shiny.rds"))
# 
# AlleYoupi5 <- fread(here::here("participation-6176abef454c9f0d79509239-observations.csv"))
# AlleYoupi7 <- AlleYoupi5
# AlleYoupi8 <- AlleYoupi5
# 
# submit   <- 1#input$"submit"
# submit0  <- 0
# 
# sp_corr  <- "Pippip"#input$"espececorrige"
# prb_corr <- "SUR"#input$"probacorrige"
# 
# xxx <- mysp %>% ggvis_plot(AlleYoupi5, AlleYoupi7, AlleYoupi8, 
#                            submit, sp_corr, prb_corr, submit0)
# 
