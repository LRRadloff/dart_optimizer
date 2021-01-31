library(shiny)

source('Dart_Optimizer_support_2.R')
sim_means <- read_csv("sim_means.csv")

ui <- fluidPage(

    # Application title
    titlePanel("What point to target when playing Dart"),
    p(
        "If you're a very precise thrower you'll obviously target T20 all the time. 
            But what if you're no Michael van Gerwen? Is it still worth the risk of hitting a 1 or 5? 
            This app can tell you - provided a few assumptions (see below) and a good self-assessment of your precision."
    ),
    h3("How it works"),
    p(
        "First you have to calibrate the variation in your throws, i.e. how far off-target you usually hit. 
            Follow the instructions in the panel on the left."
    ),
    p(
        "In advance for each point of a grid of 3841 points distributed across the board, 
            and all combinations of standard deviations, 2000 throws targeted at that point with 
            that precision have been simulated. The results of these 2000 throws are computed and averaged
            in order to obtain a good estimate of how many points to expect when aiming at that point.
        When hitting the \"Find best targets!\" button a plot of a dart board is generated where the 
            possible target points are colored according to the estimated average result for your choice of standard deviations.
            In addition you have the option to mark the best target points."
    ),
    sidebarLayout(
        sidebarPanel(
            "Choose the vertical and horizontal standard deviations such that you think that, 
            when targeting the center of the board,", 
            tags$li(
                "two out of three of your throws hit inside the dashed lines"
            ), 
            tags$li(
                "nine out of ten of your throws hit inside the solid lines."
            ),
            sliderInput("sd_vert",
                        "Vertical standard deviation",
                        min = 0.01,
                        max = 0.5,
                        step = 0.01,
                        value = 0.3),
            sliderInput("sd_horiz",
                        "Horizontal standard deviation",
                        min = 0.01,
                        max = 0.5,
                        step = 0.01,
                        value = 0.3),
            actionButton("go_sim", label = "Find best targets!")
        ),
        mainPanel(
           plotOutput("sd_visualizer")
        )
    ),
    sidebarLayout(
        sidebarPanel(
            radioButtons("best_targets_type", label = "What type of \"best target points\" would you like to be displayed?", choices = c("Best n target points." = 0, "All points less than x% worse than best point." = 1)),
            selectInput("no_best_targets", label = "If \"Best n target points.\", choose n.", 
                        choices = 0:5, 
                        selected = 0),
            selectInput("max_perc_diff", 
                        label = "If \"All points less than x% worse than best point.\", choose x.", 
                        choices = c("1%" = 0.01,"2%" = 0.02,"3%" = 0.03,"4%" = 0.04,"5%" = 0.05,"6%" = 0.06,"7%" = 0.07,"8%" = 0.08,"9%" = 0.09,"10%" = 0.1),
                        selected = 0.05)
        ),
        mainPanel(
            plotOutput("target_optimizer_plot")
        )
    ),
    fluidRow(
        column(width = 6,
            h3("Assumptions"),
            p(
                "In the (unlikely :P) case that you're taking these results very seriously be aware of all the assumptions going into it:"
            ),
            tags$li(
                "The errors, i.e. the deviations between target and hitting point, follow a normal distribution. 
                    Its variance can be calibrated by the user."
            ),
            tags$li(
                "Horizontal and vertical deviations are uncorrelated."
            ),
            tags$li(
                "The throws are unbiased, i.e. in average you're hitting where you're aiming."
            ),
            tags$li(
                "The precision of your throws is independent of your target."
            )
        ),
        column(width = 4,
            h3("Further notes"),
            tags$li(
                "All the assumptions could be tested for if you were ready to collect some data on your throws.
                Optimally you would record both the exact target and the precise location of the hitting point on the board. 
                But even only noting the field the dart hit should be sufficient given enough throws.
                With such data you could also consistently estimate the input parameters."
            ),
            tags$li(
                "Since the results are based on simulations there is some randomness present in the results. 
                Therefore the \"best target\" displayed is not necessarily the very best 
                and in another run of the simulations another \"best target\" might have come up. 
                But all very good target points accoding to that simulation are indeed very good." 
            )
        )
    )
)

server <- function(input, output) {
    output$sd_visualizer <- renderPlot({
        
        sd_horiz <- input$sd_horiz
        sd_vert <- input$sd_vert
        
        corr_M <- matrix(c(sd_horiz^2, 0, 0, sd_vert^2), ncol = 2)
        inner_ellipse_M <- ellipse(corr_M, level = 2/3)
        inner_ellipse_df <- tibble(x = inner_ellipse_M[,1], y = inner_ellipse_M[,2])
        outer_ellipse_M <- ellipse(corr_M, level = 0.9)
        outer_ellipse_df <- tibble(x = outer_ellipse_M[,1], y = outer_ellipse_M[,2])
        
        ell_plt <- dart_board_plt +
            geom_path(data = inner_ellipse_df, mapping = aes(x,y), color = 'red', linetype = 'dashed') +
            geom_path(data = outer_ellipse_df, mapping = aes(x,y), color = 'red')
        
        hoz_plt <-dart_board_plt +
            geom_vline(xintercept = c(qnorm(1/6, sd = sd_horiz), qnorm(5/6, sd = sd_horiz)), color = 'red', linetype = 'dashed') + 
            geom_vline(xintercept = c(qnorm(1/20, sd = sd_horiz), qnorm(19/20, sd = sd_horiz)), color = 'red')
        
        vert_plt <- dart_board_plt + 
            geom_hline(yintercept = c(qnorm(1/6, sd = sd_vert), qnorm(5/6, sd = sd_vert)), color = 'red', linetype = 'dashed') + 
            geom_hline(yintercept = c(qnorm(1/20, sd = sd_vert), qnorm(19/20, sd = sd_vert)), color = 'red')
        
        grid.arrange(vert_plt, hoz_plt, ell_plt, ncol = 3)
    })
    sim_means_df <-  eventReactive(input$go_sim, {
        sim_means %>% filter(abs(sd_vert - input$sd_vert) < 0.00001, 
                             abs(sd_horiz - input$sd_horiz) < 0.00001)
    })
    opt_target_df <- reactive({
        if (input$best_targets_type == 0) {
            sim_means_df() %>% slice_max(mean_res, n=as.integer(input$no_best_targets))
        }else if (input$best_targets_type == 1) {
            max_res <- max(sim_means_df()$mean_res)
            sim_means_df() %>% filter(mean_res > (1 - as.numeric(input$max_perc_diff)) * max_res)
        }
    })
    output$target_optimizer_plot <- renderPlot({
        sd_vert <- isolate({
            input$sd_vert
        })
        sd_horiz <- isolate({
            input$sd_horiz
        })
        ggplot() + 
            geom_point(data = sim_means_df(), mapping = aes(x = x_target, y = y_target, color = mean_res), alpha = 0.8, size = 4, shape = 18) + 
            geom_point(data = opt_target_df(), mapping = aes(x = x_target, y = y_target), color = 'red', shape = 4, size = 3, stroke = 2) +
            geom_circle(data = circle_df, mapping = aes(x0 = x, y0 = y, r = r)) +
            geom_segment(data = border_df, mapping = aes(x = x, y = y, xend = xend, yend = yend)) +
            theme_void() +
            coord_fixed(xlim = c(-1, 1), ylim = c(-1,1)) +
            theme(aspect.ratio=1) + 
            scale_color_viridis_c(option = 'magma') +
            geom_text(data = board_numbers_df, mapping = aes(x = x, y = y, label = numbers)) +
            labs(color = 'Average points', caption = paste("Vertical SD: ", sd_vert, "\n Horizontal SD: ", sd_horiz))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
