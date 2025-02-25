library(bslib)
library(emojifont)
library(emoGG)
library(ggimage)
library(ggiraph)
library(ggthemes)
library(ggpubr)
library(gridlayout)
library(patchwork)
library(plotly)
library(png)
library(raster)
library(Rfast)
library(shiny)
library(shinyChatR)
library(sf)
library(tidyverse)

#chatdata <- read_rds("data/chatdata.rds")
mapdata <- st_cast(read_rds("data/gamedata.rds"), "MULTIPOLYGON")
# palette (this palette is from the Dark2 colorBrewer palette)
map_palette <- c(FRA = "#a6cee3", GBR = "#1f78b4", RUS = "#33a02c", ITA = "#b2df8a", AHE = "#ff7f00", GER = "#b15928", OTT = "#e31a1c")

ui <- grid_page(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  layout = c(
    "niceTogglesArea map",
    "sidebar         map",
    "sidebar         map"
  ),
  row_sizes = c(
    "0.44fr",
    "1.56fr",
    "1fr"
  ),
  col_sizes = c(
    "0.4fr",
    "1fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "map",
    card_body(
      girafeOutput(
        outputId = "map",
        height = "100%",
        width = "100%"
      )
    )
  ),
  grid_card(
    area = "sidebar",
    card_body(
      tabsetPanel(
        selected = "Game",
        nav_panel(
          title = "Game",
          grid_container(
            layout = c(
              "countryDetails",
              "actionsMainButtons"
            ),
            row_sizes = c(
              "1fr",
              "1fr"
            ),
            col_sizes = c(
              "1fr"
            ),
            gap_size = "20px",
            grid_card(
              area = "countryDetails",
              card_body(
                gap = "0px",
                grid_container(
                  layout = c(
                    "nationalFlag nationalDetails"
                  ),
                  gap_size = "10px",
                  col_sizes = c(
                    "100px",
                    "1fr"
                  ),
                  row_sizes = c(
                    "130px"
                  ),
                  grid_card(
                    area = "nationalFlag",
                    card_body(plotOutput(outputId = "nationalFlagOutput"))
                  ),
                  grid_card(
                    area = "nationalDetails",
                    card_body(uiOutput(outputId = "nationalDetailsOutput"))
                  )
                )
              )
            ),
            grid_card(
              area = "actionsMainButtons",
              card_body(
                actionButton(
                  inputId = "researchModal",
                  label = "Research"
                ),
                actionButton(inputId = "messageModal", label = "Message"),
                actionButton(inputId = "orderModal", label = "Check")
              )
            )
          )
        ),
        nav_panel(
          title = "Statistics",
          grid_container(
            layout = c(
              "pointsDisplay",
              "mapModeButtons",
              "statsPlot    "
            ),
            row_sizes = c(
              "0.31fr",
              "0.19fr",
              "1fr"
            ),
            col_sizes = c(
              "1fr"
            ),
            gap_size = "20px",
            grid_card(
              area = "statsPlot",
              card_body(plotOutput(outputId = "plot"))
            ),
            grid_card(
              area = "pointsDisplay",
              card_body(
                grid_container(
                  layout = c(
                    "apOutput incomeOutput"
                  ),
                  row_sizes = c(
                    "1fr"
                  ),
                  col_sizes = c(
                    "1fr",
                    "1fr"
                  ),
                  gap_size = "10px",
                  grid_card(
                    area = "apOutput",
                    card_header("Action Points"),
                    card_body(textOutput(outputId = "apOutput"))
                  ),
                  grid_card(
                    area = "incomeOutput",
                    card_header("Income"),
                    card_body(textOutput(outputId = "incomeOutput"))
                  )
                )
              )
            ),
            grid_card(
              area = "mapModeButtons",
              card_body(
                popover(
                  actionButton(
                    inputId = "mapModeSelect",
                    label = "Select Map Mode"
                  ),
                  title = "Map Mode",
                  id = "mapModePopover",
                  placement = "top",
                  radioButtons(
                    inputId = "mapModeSelectPopover",
                    label = "",
                    choices = list(
                      "Tiles" = "tilemap",
                      "Provinces" = "provincemap",
                      "Regions" = "regionmap"
                    ),
                    width = "200%"
                  )
                )
              )
            )
          )
        ),
        nav_panel(
          title = "About",
          grid_container(
            layout = c(
              "menuDetails"
            ),
            gap_size = "0px",
            col_sizes = c(
              "1fr"
            ),
            row_sizes = c(
              "1fr"
            ),
            grid_card(
              area = "menuDetails",
              card_body(
                markdown(
                  mds = c(
                    "Create or join a game! Click **Create** to generate a lobby code to share with friends, or click **Join** to enter a lobby code from your enemies!"
                  )
                ),
                actionButton(
                  inputId = "createGameButton",
                  label = "Create"
                ),
                actionButton(inputId = "joinGameButton", label = "Join"),
                card(
                  full_screen = TRUE,
                  card_header("About"),
                  card_body(
                    markdown(
                      mds = c(
                        "Chicanery is a multiplayer strategy game with simultaneous turns following the turn/phase format of Diplomacy and using traditional Diplomacy unit combat mechanics. I made this game partly as a fun expansion on Diplomacy, and partly as a test of the limits of RShiny. I took inspiration to do this with Shiny after finding another project by [Francesco Bellelli](https://github.com/fbellelli/TileMaster)---who demonstrated that the necessary map interactions were even possible---while some adding mechanical depth, chat, and multiplayer functionality, and packaged it into an Electron app. Your fundamental objective remains to dominate the map by outwitting your friends.",
                        "",
                        "**Links**: \\\\\\\\\\\\\\\\ ",
                        "[Game Wiki](https://chicanerygame.netlify.app/) \\\\\\\\\\\\\\\\ ",
                        "[Github (Game Repo)](https://github.com/tinskoric/chicanery) \\\\\\\\\\\\\\\\ ",
                        "[Tin Skoric (Personal Site)](https://tins.page/)"
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  ),
  grid_card(
    area = "niceTogglesArea",
    card_body(
      grid_container(
        layout = c(
          "lightToggle timeRemaining"
        ),
        row_sizes = c(
          "63px"
        ),
        col_sizes = c(
          "63px",
          "1fr"
        ),
        gap_size = "10px",
        grid_card(
          area = "timeRemaining",
          card_body(textOutput(outputId = "timeOutput"))
        ),
        grid_card(
          area = "lightToggle",
          input_dark_mode(
            mode = "dark"
          )
        )
      )
    )
  )
)


server <- function(input, output, server) {
  
  map_pieces <- mapdata %>% 
    filter(unit != "none") %>% 
    mutate(x = st_coordinates(centroid)[,1],
           y = st_coordinates(centroid)[,2])
  
  output$nationalFlagOutput <- renderPlot({
    plotRGB(stack("data/flags/GBR.png"))
  })
  
  output$map <- renderGirafe({
    bg_gg <- ggplot() + background_image(readPNG(paste("data/map/", input$mapModeSelectPopover, ".png", sep=""))) + theme(aspect.ratio = 1)
    map_gg <- bg_gg + 
    geom_sf_interactive(data = (mapdata %>% filter(occupied_by != "unoccupied")), aes(fill = occupied_by, tooltip = unit), color = "transparent", alpha = 0.5) +
    # geom_emoji(data = (map_pieces %>% filter(unit == "NAVY")), aes(x, y), emoji = "26f5", size = 0.02) +
    # geom_emoji(data = (map_pieces %>% filter(unit == "ARMY")), aes(x, y), emoji = "1f482", size = 0.02) +
    geom_image(data = (map_pieces %>% filter(unit == "NAVY") %>% filter(occupied_by == "GBR")), aes(x, y), image = paste("data/icons/", "GBR", "/NAVY.png", sep = ""), size = 0.015, alpha = 0.7) +
    geom_image(data = (map_pieces %>% filter(unit == "ARMY") %>% filter(occupied_by == "GBR")), aes(x, y), image = paste("data/icons/", "GBR", "/ARMY.png", sep = ""), size = 0.015, alpha = 0.7) +
    geom_image(data = (map_pieces %>% filter(unit == "NAVY") %>% filter(occupied_by == "FRA")), aes(x, y), image = paste("data/icons/", "FRA", "/NAVY.png", sep = ""), size = 0.015, alpha = 0.7) +
    geom_image(data = (map_pieces %>% filter(unit == "ARMY") %>% filter(occupied_by == "FRA")), aes(x, y), image = paste("data/icons/", "FRA", "/ARMY.png", sep = ""), size = 0.015, alpha = 0.7) +
    geom_image(data = (map_pieces %>% filter(unit == "NAVY") %>% filter(occupied_by == "RUS")), aes(x, y), image = paste("data/icons/", "RUS", "/NAVY.png", sep = ""), size = 0.015, alpha = 0.7) +
    geom_image(data = (map_pieces %>% filter(unit == "ARMY") %>% filter(occupied_by == "RUS")), aes(x, y), image = paste("data/icons/", "RUS", "/ARMY.png", sep = ""), size = 0.015, alpha = 0.7) +
    geom_image(data = (map_pieces %>% filter(unit == "NAVY") %>% filter(occupied_by == "ITA")), aes(x, y), image = paste("data/icons/", "ITA", "/NAVY.png", sep = ""), size = 0.015, alpha = 0.7) +
    geom_image(data = (map_pieces %>% filter(unit == "ARMY") %>% filter(occupied_by == "ITA")), aes(x, y), image = paste("data/icons/", "ITA", "/ARMY.png", sep = ""), size = 0.015, alpha = 0.7) +
    geom_image(data = (map_pieces %>% filter(unit == "NAVY") %>% filter(occupied_by == "AHE")), aes(x, y), image = paste("data/icons/", "AHE", "/NAVY.png", sep = ""), size = 0.015, alpha = 0.7) +
    geom_image(data = (map_pieces %>% filter(unit == "ARMY") %>% filter(occupied_by == "AHE")), aes(x, y), image = paste("data/icons/", "AHE", "/ARMY.png", sep = ""), size = 0.015, alpha = 0.7) +
    geom_image(data = (map_pieces %>% filter(unit == "NAVY") %>% filter(occupied_by == "GER")), aes(x, y), image = paste("data/icons/", "GER", "/NAVY.png", sep = ""), size = 0.015, alpha = 0.7) +
    geom_image(data = (map_pieces %>% filter(unit == "ARMY") %>% filter(occupied_by == "GER")), aes(x, y), image = paste("data/icons/", "GER", "/ARMY.png", sep = ""), size = 0.015, alpha = 0.7) +
    geom_image(data = (map_pieces %>% filter(unit == "NAVY") %>% filter(occupied_by == "OTT")), aes(x, y), image = paste("data/icons/", "OTT", "/NAVY.png", sep = ""), size = 0.015, alpha = 0.7) +
    geom_image(data = (map_pieces %>% filter(unit == "ARMY") %>% filter(occupied_by == "OTT")), aes(x, y), image = paste("data/icons/", "OTT", "/ARMY.png", sep = ""), size = 0.015, alpha = 0.7) +
    # scale_fill_brewer(palette = "Dark2", direction = 1) +
    scale_fill_manual_interactive(values = map_palette) +
    coord_sf(xlim = c(0, 2147), ylim = c(0, 2160)) +
    theme_void() +
    theme(
      legend.position = "none",
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      aspect.ratio = 1
    )
    girafe(ggobj = map_gg, fonts = list(sans = "Space Grotesk")) %>% 
      girafe_options(
        opts_tooltip(
          use_fill = TRUE
        ),
        opts_hover(css = "fill:orange;"),
        opts_zoom(min = 1.3, max = 5))
  })
  
  #chat_server("test",
  #            rds_path = chatdata#,
  #            chat_user = "user1"
  #            )
  
  observe({ 
    showModal( 
      modalDialog( 
        title = "Reserch", 
        easy_close = TRUE, 
        "This is your important message." 
      ) 
    ) 
  }) |> 
    bindEvent(input$researchModal) 
  observe({ 
    showModal( 
      modalDialog( 
        title = "Messages", 
        easy_close = TRUE, 
        chat_ui("test")
      ) 
    ) 
  }) |> 
    bindEvent(input$messageModal) 
  observe({ 
    showModal( 
      modalDialog( 
        title = "Orders", 
        easy_close = TRUE, 
        "This is your important message." 
      ) 
    ) 
  }) |> 
    bindEvent(input$orderModal) 
  
}

shinyApp(ui, server)
