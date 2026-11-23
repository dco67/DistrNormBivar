library(shiny)
library(plotly)
library(mvtnorm)
library(bslib)
library(colourpicker)

`%||%` <- function(x, y) if (is.null(x)) y else x

ui <- tagList(
  # Configuration MathJax
  withMathJax(),
  tags$div(HTML("<script type='text/x-mathjax-config'>
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$'], ['\\\\(','\\\\)']]}
                });
                </script>")),
  tags$style(HTML("
    .bivariate-main-layout {
      display: grid;
      grid-template-columns: minmax(0, 7fr) minmax(0, 5fr);
      gap: 0.25rem;
      position: relative;
    }
    .bivariate-main-layout > .card { min-width: 0; }
    .density-legend-overlay {
      position: absolute;
      top: 3.2rem;
      left: calc(58.333% + 0.125rem);
      width: 58px;
      height: 520px;
      transform: translateX(-50%);
      z-index: 2;
      pointer-events: none;
    }
    .density-legend {
      height: 100%;
      display: flex;
      align-items: center;
      justify-content: center;
      gap: 4px;
      color: #343a40;
      font-size: 0.72rem;
      line-height: 1;
    }
    .density-legend__title {
      writing-mode: vertical-rl;
      transform: rotate(180deg);
    }
    .density-legend__bar {
      width: 14px;
      height: 78%;
      border: 1px solid rgba(0, 0, 0, .25);
    }
    .density-legend__ticks {
      height: 78%;
      display: flex;
      flex-direction: column;
      justify-content: space-between;
    }
  ")),
  navset_card_tab(
    title = h4("Distribution normale bivariée"),

    # Onglet Introduction
    nav_panel(
      title = "Introduction théorique",
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Introduction"),
          p("La distribution normale bivariée décrit la loi conjointe de deux variables continues. Ses marginales sont normales et sa forme dépend des deux moyennes, des deux écarts-types et de leur corrélation."),
          p("Deux variables dont les distributions marginales sont normales ne suivent pas nécessairement une loi normale bivariée : c'est bien la loi conjointe qui constitue ici l'hypothèse."),

          card(
            class = "border-primary",
            card_header("Commencer ici : observer avant de calculer"),
            tags$ol(
              tags$li("Dans l'onglet ", tags$strong("Distribution Bivariée"), ", placez d'abord la corrélation à 0. La projection en courbes de niveau devient presque circulaire : connaître X ne renseigne alors pas sur Y."),
              tags$li("Comparez ensuite une corrélation de 0,80 puis de -0,80. L'intensité resserre l'ellipse; le signe en change l'orientation."),
              tags$li("Enfin, modifiez un seul écart-type. La corrélation reste la même, mais l'échelle des axes et la pente de la relation changent."),
              tags$li("La hauteur de la surface représente une ", tags$strong("densité"), ", non une probabilité ponctuelle. Une probabilité correspond à une aire sous la surface.")
            )
          ),

          # Équation de densité
          card(
            card_header("Fonction de densité"),
            withMathJax(
              "La fonction de densité de probabilité est donnée par :",
              "$$f(x,y) = \\frac{1}{2\\pi\\color{red}{\\sigma_x}\\color{red}{\\sigma_y}\\sqrt{1-\\color{red}{\\rho}^2}}
    \\exp\\left(-\\frac{1}{2(1-\\color{red}{\\rho}^2)}\\left[
    \\frac{(x-\\color{red}{\\mu_x})^2}{\\color{red}{\\sigma_x}^2} +
    \\frac{(y-\\color{red}{\\mu_y})^2}{\\color{red}{\\sigma_y}^2} -
    \\frac{2\\color{red}{\\rho}(x-\\color{red}{\\mu_x})(y-\\color{red}{\\mu_y})}{\\color{red}{\\sigma_x}\\color{red}{\\sigma_y}}
    \\right]\\right)$$
    où les paramètres de la fonction (surlignés en rouge), sont:",
              p(span("$\\color{red}{\\mu_x}, \\color{red}{\\mu_y}$ : les moyennes")),
              p(span("$\\color{red}{\\sigma_x}, \\color{red}{\\sigma_y}$ : les écarts-types")),
              p(span("$\\color{red}{\\rho}$ : le coefficient de corrélation"))
            )
          ),

          # Distribution conditionnelle
          card(
            card_header("Théorie de la distribution conditionnelle"),
            withMathJax(
              p("Pour une distribution normale bivariée, la distribution conditionnelle Y|X=x suit également une loi normale avec :"),
              p(span("$\\mu_{Y|X=x} = \\mu_y + \\rho\\frac{\\sigma_y}{\\sigma_x}(x-\\mu_x)$ (moyenne conditionnelle)")),
              p(span("$\\sigma_{Y|X=x} = \\sigma_y\\sqrt{1-\\rho^2}$ (écart-type conditionnel)")),
              p("Cette propriété remarquable montre que :"),
              tags$ul(
                tags$li("La moyenne conditionnelle est une fonction linéaire de x"),
                tags$li("L'écart-type conditionnel est constant (ne dépend pas de x)"),
                tags$li("Plus la corrélation est forte, plus la distribution conditionnelle est concentrée")
              )
            )
          )
        ),
        card(
          card_header(h4("Distribution normale bivariée (ρ = 0.75)")),
          plotlyOutput("intro_mesh_plot", height = "520px")
        )
      )
    ),
    nav_panel(
      title = "Distribution Bivariée",
      page_sidebar(
        sidebar = sidebar(
          width = 450,
          # Type de contrôle
          radioButtons("input_type", "Type de contrôle",
            choices = c(
              "Numériques" = "numeric",
              "Curseurs" = "slider"
            ),
            inline = TRUE
          ),

          # Paramètres pour X et Y
          card(
            card_header("Paramètres de la distribution"),

            # Paramètres numériques
            conditionalPanel(
              condition = "input.input_type == 'numeric'",
              layout_columns(
                col_widths = c(6, 6),
                numericInput("mean_x", "Moyenne X", value = 0, step = 0.1),
                numericInput("mean_y", "Moyenne Y", value = 0, step = 0.1)
              ),
              layout_columns(
                col_widths = c(6, 6),
                numericInput("sd_x", "Écart-type X", value = 1, min = 0.1, step = 0.1),
                numericInput("sd_y", "Écart-type Y", value = 1, min = 0.1, step = 0.1)
              )
            ),

            # Paramètres avec curseurs
            conditionalPanel(
              condition = "input.input_type == 'slider'",
              layout_columns(
                col_widths = c(6, 6),
                sliderInput("mean_x_sld", "Moyenne X",
                  min = -3, max = 3, value = 0, step = 0.1
                ),
                sliderInput("mean_y_sld", "Moyenne Y",
                  min = -3, max = 3, value = 0, step = 0.1
                )
              ),
              layout_columns(
                col_widths = c(6, 6),
                sliderInput("sd_x_sld", "Écart-type X",
                  min = 0.1, max = 3, value = 1, step = 0.1
                ),
                sliderInput("sd_y_sld", "Écart-type Y",
                  min = 0.1, max = 3, value = 1, step = 0.1
                )
              )
            )
          ),

          # Corrélation
          card(
            card_header("Corrélation"),
            sliderInput("correlation", "Coefficient de corrélation",
              min = -0.99, max = 0.99, value = 0.70, step = 0.01
            )
          ),

          # Options d'affichage
          card(
            card_header("Options d'affichage"),
            radioButtons("color_scheme", "Palette de couleurs",
              choices = c(
                "Personnalisée" = "custom",
                "Arc-en-ciel" = "rainbow"
              ),
              selected = "rainbow",
              inline = TRUE
            ),
            # Couleurs personnalisées conditionnelles
            conditionalPanel(
              condition = "input.color_scheme == 'custom'",
              layout_columns(
                col_widths = c(6, 6),
                colourInput("color_min", "Couleur min",
                  value = "#0247FE",
                  showColour = "background",
                  allowTransparent = TRUE,
                  palette = "square"
                ),
                colourInput("color_max", "Couleur max",
                  value = "#FE2712",
                  showColour = "background",
                  allowTransparent = TRUE,
                  palette = "square"
                )
              )
            ),
            checkboxInput("show_contours", "Afficher les isolignes", value = TRUE),
            checkboxInput("show_sample", "Afficher un échantillon simulé", value = FALSE),
            conditionalPanel(
              condition = "input.show_sample",
              layout_columns(
                col_widths = c(6, 6),
                selectInput(
                  "sample_n", "Taille de l'échantillon",
                  choices = c("50 observations" = 50, "200 observations" = 200, "500 observations" = 500),
                  selected = 200
                ),
                div(
                  style = "padding-top: 1.9rem;",
                  actionButton("resample", "Nouvel échantillon", class = "btn-outline-primary")
                )
              )
            )
          )
        ),

        # Main panel content
        tagList(
          div(
            class = "bivariate-main-layout",
            card(
              height = "100%", # S'adapte au contenu
              fill = FALSE, # Empêche le remplissage automatique
              card_header(
                "Distribution normale bivariée en 3D",
                actionButton("btn_modal_3d", "Agrandir", class = "btn-sm"),
                downloadButton("download_3d_html", "HTML", class = "btn-sm")
              ),
              plotlyOutput("plot3d", height = "520px")
            ),
            div(
              class = "density-legend-overlay",
              uiOutput("density_legend")
            ),
            card(
              height = "100%",
              fill = FALSE,
              card_header(
                "Courbes de niveau",
                actionButton("btn_modal_contour", "Agrandir", class = "btn-sm"),
                downloadButton("download_contour_html", "HTML", class = "btn-sm")
              ),
              plotlyOutput("contour_plot", height = "520px")
            )
          ),
          card(
            class = "mt-2 bg-light",
            card_header("Lire les graphiques"),
            uiOutput("bivariate_interpretation")
          )
        )
      )
    ),
    nav_panel(
      title = "Distribution Conditionnelle",
      page_sidebar(
        sidebar = sidebar(
          width = 450,
          # Paramètres de la distribution
          card(
            card_header("Paramètres de la distribution"),
            tags$div(
              style = "padding: 5px;",
              uiOutput("current_params_summary")
            )
          ),
          card(
            class = "bg-info",
            card_header("Paramètres conditionnels"),
            # Radio buttons pour choisir le type de contrôle
            radioButtons("input_type_cond", "Type de contrôle",
              choices = c(
                "Numériques" = "numeric",
                "Curseurs" = "slider"
              ),
              selected = "slider",
              inline = TRUE
            ),

            # Contrôles numériques
            conditionalPanel(
              condition = "input.input_type_cond == 'numeric'",
              layout_columns(
                col_widths = c(6, 6),
                numericInput("cond_x", "Valeur de X fixée", value = 0, step = 0.1),
                numericInput("threshold_y", "Seuil de Y", value = 0, step = 0.1)
              )
            ),

            # Contrôles avec curseurs
            # Dans la partie "Contrôles avec curseurs", remplacer par :
            conditionalPanel(
              condition = "input.input_type_cond == 'slider'",
              layout_columns(
                col_widths = c(6, 6),
                sliderInput("cond_x_sld", "Valeur de X fixée",
                  min = -3, max = 3, value = 0, step = 0.1,
                  animate = animationOptions(
                    interval = 1000,
                    loop = TRUE
                  )
                ),
                sliderInput("threshold_y_sld", "Seuil de Y",
                  min = -3, max = 3, value = 0, step = 0.1,
                  animate = animationOptions(
                    interval = 1000,
                    loop = TRUE
                  )
                )
              )
            ),
            style = "padding: 5px;",
            div(
              style = "margin-bottom: 10px;",
              withMathJax(
                uiOutput("conditional_params_display")
              )
            ),
            div(
              style = "margin-bottom: 0px;",
              withMathJax(
                uiOutput("conditional_prob_display")
              )
            )
          ) # Paramètres conditionnels
        ),
        # Contenu principal
        tagList(
          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header(
                "Distribution bivariée",
                layout_columns(
                  col_widths = c(6, 6),
                  checkboxInput("show_regression_cond", "Régression", value = TRUE),
                  radioButtons("plot_type_cond", "Graphique",
                    choices = c("Courbes de niveau" = "contour", "Surface 3D" = "3d"),
                    selected = "3d",
                    inline = TRUE
                  )
                )
              ),
              plotlyOutput("bivariate_conditional_plot", height = "400px")
            ),
            card(
              card_header("Distribution conditionnelle Y|X=x"),
              plotlyOutput("conditional_plot", height = "400px")
            )
          ),
          card(
            class = "mt-2 bg-light",
            card_header("Interpréter la coupe conditionnelle"),
            uiOutput("conditional_interpretation")
          )
        )
      )
    ),
    nav_panel(
      title = "Laboratoire guidé",
      card(
        class = "border-primary mb-3",
        card_header("Un parcours en quatre expériences"),
        p("Pour chaque expérience : formulez d'abord votre prédiction, appliquez les réglages, puis ouvrez l'onglet ", tags$strong("Distribution Bivariée"), " pour la vérifier. Les réglages appliqués demeurent en place."),
        p(tags$strong("Fil directeur : "), "la corrélation règle l'orientation et le resserrement de l'ellipse; les écarts-types règlent son échelle sur les deux axes.")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("1. Indépendance (modèle normal bivarié)"),
          p("Prédiction : quelle forme prendront les courbes de niveau lorsque la corrélation est nulle?"),
          tags$ul(
            tags$li("Réglages : μX = μY = 0, σX = σY = 1, ρ = 0."),
            tags$li("À observer : l'ellipse devient un cercle; fixer X ne modifie ni la moyenne ni l'incertitude de Y.")
          ),
          actionButton("lab_independence", "Appliquer les réglages", class = "btn-primary")
        ),
        card(
          card_header("2. Association positive forte"),
          p("Prédiction : dans quel sens l'ellipse sera-t-elle inclinée lorsque X et Y tendent à augmenter ensemble?"),
          tags$ul(
            tags$li("Réglages : μX = μY = 0, σX = σY = 1, ρ = 0,80."),
            tags$li("À observer : l'ellipse est resserrée et orientée du bas-gauche vers le haut-droite.")
          ),
          actionButton("lab_positive", "Appliquer les réglages", class = "btn-primary")
        ),
        card(
          card_header("3. Association négative forte"),
          p("Prédiction : qu'est-ce qui change par rapport à l'expérience précédente, et qu'est-ce qui reste inchangé?"),
          tags$ul(
            tags$li("Réglages : μX = μY = 0, σX = σY = 1, ρ = -0,80."),
            tags$li("À observer : le resserrement est le même, mais l'ellipse est maintenant orientée du haut-gauche vers le bas-droite.")
          ),
          actionButton("lab_negative", "Appliquer les réglages", class = "btn-primary")
        ),
        card(
          card_header("4. Même corrélation, autre échelle"),
          p("Prédiction : si ρ demeure égal à 0,70, la relation reste-t-elle identique lorsque les écarts-types changent?"),
          tags$ul(
            tags$li("Réglages : μX = μY = 0, σX = 0,50, σY = 2,00, ρ = 0,70."),
            tags$li("À observer : la corrélation ne change pas, mais l'ellipse s'étire et la pente de la régression de Y sur X devient beaucoup plus forte.")
          ),
          actionButton("lab_scale", "Appliquer les réglages", class = "btn-primary")
        )
      ),
      card(
        class = "mt-3 bg-light",
        card_header("Réglages appliqués"),
        uiOutput("lab_status"),
        tags$hr(),
        tags$ol(
          tags$li("Passez à l'onglet Distribution Bivariée et choisissez les courbes de niveau."),
          tags$li("Décrivez la forme et l'orientation de l'ellipse avant de consulter l'encadré « Lire les graphiques »."),
          tags$li("Pour les expériences 2 et 3, ouvrez aussi l'onglet Distribution Conditionnelle et comparez une coupe à X = 0 et une coupe éloignée de zéro.")
        )
      )
    )
  )
)



server <- function(input, output, session) {
  camera_position <- reactiveVal(list(x = 1.5, y = 1.5, z = 1.5))
  lab_experiment <- reactiveVal(NULL)
  sample_draw <- reactiveVal(1)

  observeEvent(input$resample, {
    sample_draw(sample_draw() + 1)
  }, ignoreInit = TRUE)

  apply_lab_settings <- function(label, rho, sd_x_value, sd_y_value) {
    # Les deux jeux de contrôles sont synchronisés afin que le laboratoire
    # fonctionne autant en mode « Numériques » qu'en mode « Curseurs ».
    updateNumericInput(session, "mean_x", value = 0)
    updateNumericInput(session, "mean_y", value = 0)
    updateNumericInput(session, "sd_x", value = sd_x_value)
    updateNumericInput(session, "sd_y", value = sd_y_value)
    updateSliderInput(session, "mean_x_sld", value = 0)
    updateSliderInput(session, "mean_y_sld", value = 0)
    updateSliderInput(session, "sd_x_sld", value = sd_x_value)
    updateSliderInput(session, "sd_y_sld", value = sd_y_value)
    updateSliderInput(session, "correlation", value = rho)
    updateCheckboxInput(session, "show_contours", value = TRUE)
    lab_experiment(label)
  }

  observeEvent(input$lab_independence, {
    apply_lab_settings("Expérience 1 — indépendance (modèle normal bivarié)", 0, 1, 1)
  })
  observeEvent(input$lab_positive, {
    apply_lab_settings("Expérience 2 — association positive forte", 0.80, 1, 1)
  })
  observeEvent(input$lab_negative, {
    apply_lab_settings("Expérience 3 — association négative forte", -0.80, 1, 1)
  })
  observeEvent(input$lab_scale, {
    apply_lab_settings("Expérience 4 — même corrélation, autre échelle", 0.70, 0.50, 2)
  })

  output$lab_status <- renderUI({
    selected <- lab_experiment()
    if (is.null(selected)) {
      return(div(
        class = "p-2",
        "Choisissez une expérience ci-dessus pour appliquer ses réglages."
      ))
    }

    div(
      class = "p-2",
      p(tags$strong(selected), " est prête à explorer."),
      p(sprintf(
        "Réglages actuels : μX = %.2f, μY = %.2f, σX = %.2f, σY = %.2f, ρ = %.2f.",
        mean_x(), mean_y(), sd_x(), sd_y(), input$correlation
      )),
      p("Ouvrez maintenant l'onglet Distribution Bivariée : les courbes de niveau sont le meilleur point de départ pour vérifier votre prédiction.")
    )
  })

  # Valeurs réactives pour les paramètres
  mean_x <- reactive({
    if (input$input_type == "numeric") input$mean_x else input$mean_x_sld
  })
  mean_y <- reactive({
    if (input$input_type == "numeric") input$mean_y else input$mean_y_sld
  })
  sd_x <- reactive({
    if (input$input_type == "numeric") input$sd_x else input$sd_x_sld
  })
  sd_y <- reactive({
    if (input$input_type == "numeric") input$sd_y else input$sd_y_sld
  })

  get_axis_limits <- reactive({
    # On prend 4 écarts-types de chaque côté de la moyenne
    x_min <- mean_x() - 4 * sd_x()
    x_max <- mean_x() + 4 * sd_x()
    y_min <- mean_y() - 4 * sd_y()
    y_max <- mean_y() + 4 * sd_y()

    list(
      x_min = x_min,
      x_max = x_max,
      y_min = y_min,
      y_max = y_max
    )
  })

  clamp_to_range <- function(value, lower, upper, fallback) {
    if (is.null(value) || !is.finite(value)) value <- fallback
    min(max(value, lower), upper)
  }

  observe({
    limits <- get_axis_limits()
    updateSliderInput(
      session, "cond_x_sld",
      min = limits$x_min,
      max = limits$x_max,
      value = clamp_to_range(input$cond_x_sld, limits$x_min, limits$x_max, mean_x()),
      step = (limits$x_max - limits$x_min) / 80
    )
    updateSliderInput(
      session, "threshold_y_sld",
      min = limits$y_min,
      max = limits$y_max,
      value = clamp_to_range(input$threshold_y_sld, limits$y_min, limits$y_max, mean_y()),
      step = (limits$y_max - limits$y_min) / 80
    )
  })


  # Fonction pour générer les données de la distribution
  generate_data <- reactive({
    # Obtenir les limites des axes
    limits <- get_axis_limits()

    # Créer une grille de points adaptée aux limites
    x <- seq(limits$x_min, limits$x_max, length.out = 100)
    y <- seq(limits$y_min, limits$y_max, length.out = 100)
    grid <- expand.grid(x = x, y = y)

    # Matrice de variance-covariance
    sigma <- matrix(c(
      sd_x()^2,
      sd_x() * sd_y() * input$correlation,
      sd_x() * sd_y() * input$correlation,
      sd_y()^2
    ), 2, 2)

    # Calculer les densités
    z <- matrix(
      dmvnorm(grid,
        mean = c(mean_x(), mean_y()),
        sigma = sigma
      ),
      nrow = length(x)
    )

    list(x = x, y = y, z = z)
  })

  simulated_sample <- reactive({
    n <- as.integer(input$sample_n %||% 200)
    # Le même tirage demeure visible pendant l'exploration des paramètres;
    # le bouton « Nouvel échantillon » déclenche une nouvelle réalisation.
    set.seed(1100 + sample_draw())
    sigma <- matrix(c(
      sd_x()^2,
      sd_x() * sd_y() * input$correlation,
      sd_x() * sd_y() * input$correlation,
      sd_y()^2
    ), 2, 2)
    points <- rmvnorm(n, mean = c(mean_x(), mean_y()), sigma = sigma)
    colnames(points) <- c("X", "Y")
    points
  })

  observeEvent(event_data("plotly_relayout"), {
    ed <- event_data("plotly_relayout")
    if (!is.null(ed[["scene.camera"]])) {
      camera_position(ed[["scene.camera"]])
    }
  })

  # Créer l'échelle de couleurs personnalisée
  get_colorscale <- reactive({
    if (input$color_scheme == "custom") {
      list(
        c(0, input$color_min),
        c(1, input$color_max)
      )
    } else {
      colors <- rev(rainbow(10))
      colorscale <- lapply(seq_along(colors), function(i) {
        c((i - 1) / (length(colors) - 1), colors[i])
      })
      colorscale
    }
  })

  output$density_legend <- renderUI({
    colors <- if (input$color_scheme == "custom") {
      c(input$color_min, input$color_max)
    } else {
      rev(rainbow(10))
    }
    positions <- seq(0, 100, length.out = length(colors))
    gradient <- paste(
      sprintf("%s %.1f%%", colors, positions),
      collapse = ", "
    )
    max_density <- max(generate_data()$z)
    tick_values <- rev(seq(0, max_density, length.out = 6))
    tick_labels <- vapply(tick_values, function(value) {
      sub("\\.?0+$", "", formatC(value, format = "f", digits = 3))
    }, character(1))

    div(
      class = "density-legend",
      div(class = "density-legend__title", "Densité"),
      div(
        class = "density-legend__bar",
        style = paste0("background: linear-gradient(to top, ", gradient, ");")
      ),
      div(
        class = "density-legend__ticks",
        lapply(tick_labels, tags$span)
      )
    )
  })

  # Fonction pour créer le plot 3D
  # Dans la fonction create_3d_plot, modifiez la partie add_surface et layout comme suit:
  create_3d_plot <- function(height = NULL) {
    data <- generate_data()

    plot_ly(height = height %||% 800) %>%
      add_surface(
        x = data$x,
        y = data$y,
        z = data$z,
        colorscale = get_colorscale(),
        showscale = FALSE,
        showlegend = FALSE,
        colorbar = list(
          title = "Densité",
          len = 0.8,
          thickness = 15,
          x = 0.85 # Rapproché de 1.15 à 0.95
        )
      ) %>%
      layout(
        scene = list(
          camera = camera_position(),
          xaxis = list(
            title = "X",
            range = c(get_axis_limits()$x_min, get_axis_limits()$x_max)
          ),
          yaxis = list(
            title = "Y",
            range = c(get_axis_limits()$y_min, get_axis_limits()$y_max)
          ),
          zaxis = list(
            title = "Densité",
            range = c(0, max(data$z) * 1.2)
          ),
          aspectratio = list(x = 1, y = 1, z = 0.7)
        ),
        margin = list(l = 15, r = 15, t = 10, b = 15),
        showlegend = FALSE
      )
  }

  # Fonction pour créer le plot contour
  create_contour_plot <- function(height = NULL, show_colorbar = FALSE) {
    data <- generate_data()
    density_colorbar <- list(
      title = list(text = "Densité", side = "right"),
      len = 0.78,
      thickness = 14,
      x = 1.02,
      xanchor = "left",
      y = 0.5,
      yanchor = "middle"
    )

    base_plot <- if (input$show_contours) {
      plot_ly(height = height %||% 800) %>%
        add_contour(
          x = data$x,
          y = data$y,
          z = data$z,
          colorscale = get_colorscale(),
          showscale = show_colorbar,
          showlegend = FALSE,
          contours = list(
            showlines = TRUE,
            coloring = "heatmap"
          ),
          colorbar = density_colorbar
        )
    } else {
      plot_ly(height = height %||% 800) %>%
        add_heatmap(
          x = data$x,
          y = data$y,
          z = data$z,
          colorscale = get_colorscale(),
          showscale = show_colorbar,
          showlegend = FALSE,
          colorbar = density_colorbar
        )
    }

    if (isTRUE(input$show_sample)) {
      points <- simulated_sample()
      base_plot <- base_plot %>%
        add_markers(
          x = points[, "X"],
          y = points[, "Y"],
          marker = list(
            color = "rgba(25, 25, 25, 0.55)",
            size = 5,
            line = list(color = "rgba(255, 255, 255, 0.65)", width = 0.5)
          ),
          name = sprintf("Échantillon simulé (n = %d)", nrow(points)),
          hovertemplate = "X = %{x:.2f}<br>Y = %{y:.2f}<extra></extra>",
          inherit = FALSE,
          showlegend = FALSE
        )
    }

    base_plot %>%
      layout(
        xaxis = list(
          title = "X",
          range = c(get_axis_limits()$x_min, get_axis_limits()$x_max),
          scaleanchor = "y",
          scaleratio = 1,
          constrain = "domain"
        ),
        yaxis = list(
          title = "Y",
          range = c(get_axis_limits()$y_min, get_axis_limits()$y_max),
          constrain = "domain"
        ),
        margin = list(
          l = 35,
          r = if (show_colorbar) 70 else 20,
          t = 15,
          b = 35
        ),
        autosize = TRUE,
        showlegend = FALSE
      )
  }

  # Plots principaux
  output$plot3d <- renderPlotly({
    create_3d_plot(height = 520)
  })
  output$contour_plot <- renderPlotly({
    create_contour_plot(height = 520)
  })

  # Gérer les boutons modaux
  observeEvent(input$btn_modal_3d, {
    showModal(modalDialog(
      title = "Distribution normale bivariée en 3D",
      size = "l",
      plotlyOutput("plot3d_modal", height = "800px"),
      footer = modalButton("Fermer")
    ))

    output$plot3d_modal <- renderPlotly({
      create_3d_plot(height = 800)
    })
  })

  observeEvent(input$btn_modal_contour, {
    showModal(modalDialog(
      title = "Courbes de niveau",
      size = "l",
      plotlyOutput("contour_plot_modal", height = "800px"),
      footer = modalButton("Fermer")
    ))

    output$contour_plot_modal <- renderPlotly({
      create_contour_plot(height = 800, show_colorbar = TRUE)
    })
  })

  # Les graphiques Plotly sont des widgets HTML; cet export préserve leur interactivité.
  output$download_3d_html <- downloadHandler(
    filename = function() {
      paste0("distribution_bivariee_3d_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      htmlwidgets::saveWidget(create_3d_plot(), file = file, selfcontained = TRUE)
    }
  )

  output$download_contour_html <- downloadHandler(
    filename = function() {
      paste0("distribution_bivariee_contours_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      htmlwidgets::saveWidget(
        create_contour_plot(show_colorbar = TRUE),
        file = file,
        selfcontained = TRUE
      )
    }
  )

  # Fonctions réactives pour les paramètres conditionnels
  cond_x <- reactive({
    if (input$input_type_cond == "numeric") input$cond_x else input$cond_x_sld
  })

  threshold_y <- reactive({
    if (input$input_type_cond == "numeric") input$threshold_y else input$threshold_y_sld
  })


  get_conditional_params <- reactive({
    mu_cond <- mean_y() + input$correlation * (sd_y() / sd_x()) * (cond_x() - mean_x())
    sd_cond <- sd_y() * sqrt(1 - input$correlation^2)
    list(mean = mu_cond, sd = sd_cond)
  })

  calculate_conditional_prob <- reactive({
    params <- get_conditional_params()
    # Probabilité que Y soit plus grand que la valeur seuil
    prob <- 1 - pnorm(threshold_y(), mean = params$mean, sd = params$sd)
    prob
  })

  output$bivariate_interpretation <- renderUI({
    rho <- input$correlation
    abs_rho <- abs(rho)
    intensity <- if (abs_rho < 0.10) {
      "quasi nulle"
    } else if (abs_rho < 0.30) {
      "faible"
    } else if (abs_rho < 0.50) {
      "modérée"
    } else if (abs_rho < 0.70) {
      "assez forte"
    } else {
      "forte"
    }
    direction <- if (rho > 0.001) {
      "Quand X augmente, les valeurs attendues de Y tendent aussi à augmenter."
    } else if (rho < -0.001) {
      "Quand X augmente, les valeurs attendues de Y tendent à diminuer."
    } else {
      "Aucune tendance linéaire systématique ne relie X et Y."
    }
    slope <- rho * sd_y() / sd_x()

    div(
      class = "p-2",
      p(
        tags$strong("Association observée : "),
        sprintf("la corrélation est %.2f; son intensité est %s.", rho, intensity)
      ),
      p(direction),
      if (isTRUE(input$show_sample)) {
        p(
          class = "text-primary",
          "Les points superposés sont une seule réalisation aléatoire de cette distribution. Utilisez « Nouvel échantillon » : le nuage change, mais la densité théorique ne change pas."
        )
      },
      tags$ul(
        tags$li(
          sprintf(
            "La pente de la droite de régression de Y sur X est %.2f. Elle dépend de la corrélation, mais aussi du rapport entre les écarts-types de Y et de X.",
            slope
          )
        ),
        tags$li("Une ellipse plus resserrée traduit une association linéaire plus forte; elle ne démontre toutefois ni causalité ni absence de dispersion individuelle."),
        tags$li("La couleur et la hauteur indiquent où les couples (X, Y) sont les plus concentrés. Une probabilité se lit comme une aire, non comme la hauteur d'un point isolé.")
      )
    )
  })

  output$conditional_interpretation <- renderUI({
    params <- get_conditional_params()
    x_gap <- cond_x() - mean_x()
    y_shift <- params$mean - mean_y()
    uncertainty_reduction <- 100 * (1 - params$sd / sd_y())
    prob <- calculate_conditional_prob()

    expected_y <- if (abs(y_shift) < 0.005) {
      "correspond pratiquement à la moyenne générale de Y"
    } else if (y_shift > 0) {
      sprintf("se situe %.2f au-dessus de la moyenne générale de Y", y_shift)
    } else {
      sprintf("se situe %.2f au-dessous de la moyenne générale de Y", abs(y_shift))
    }
    x_position <- if (abs(x_gap) < 0.005) {
      "à sa moyenne"
    } else if (x_gap > 0) {
      sprintf("%.2f au-dessus de sa moyenne", x_gap)
    } else {
      sprintf("%.2f au-dessous de sa moyenne", abs(x_gap))
    }

    div(
      class = "p-2",
      p(
        "Fixer X à ", tags$strong(sprintf("%.2f", cond_x())),
        " revient à prendre une coupe verticale de la distribution bivariée. Cette valeur est ", x_position, "."
      ),
      tags$ul(
        tags$li(
          sprintf(
            "Pour cette coupe, la valeur attendue de Y est %.2f : elle %s.",
            params$mean, expected_y
          )
        ),
        tags$li(
          sprintf(
            "L'écart-type conditionnel est %.2f plutôt que %.2f. Connaître X réduit ici l'incertitude sur Y de %.1f %%.",
            params$sd, sd_y(), uncertainty_reduction
          )
        ),
        tags$li(
          sprintf(
            "La région rouge représente P(Y > %.2f | X = %.2f) = %.1f %%. Parmi les cas ayant cette valeur de X, c'est la part attendue au-dessus du seuil de Y.",
            threshold_y(), cond_x(), 100 * prob
          )
        )
      )
    )
  })

  output$conditional_plot <- renderPlotly({
    params <- get_conditional_params()

    # Générer les points pour la courbe
    y <- seq(get_axis_limits()$y_min, get_axis_limits()$y_max, length.out = 100)
    density <- dnorm(y, mean = params$mean, sd = params$sd)

    # Vérifier si les paramètres sont valides
    if (all(is.finite(density)) && all(is.finite(y))) {
      # Créer le graphique
      plot_ly() %>%
        # Courbe de densité
        add_lines(x = y, y = density, name = "Distribution conditionnelle") %>%
        # Ligne verticale pour la moyenne conditionnelle
        add_segments(
          x = params$mean, xend = params$mean,
          y = 0, yend = max(density),
          line = list(dash = "dash"),
          name = "Moyenne conditionnelle"
        ) %>%
        # Zone de probabilité
        add_lines(
          x = y[y >= threshold_y()],
          y = density[y >= threshold_y()],
          fill = "tozeroy",
          name = "P(Y > seuil)",
          fillcolor = "rgba(255, 0, 0, 0.2)"
        ) %>%
        # Ligne verticale pour le seuil
        add_segments(
          x = threshold_y(), xend = threshold_y(),
          y = 0, yend = dnorm(threshold_y(), params$mean, params$sd),
          line = list(color = "red"),
          name = "Seuil"
        ) %>%
        layout(
          title = list(
            text = sprintf(
              "Y|X=%.2f ~ N(%.2f, %.2f²)",
              cond_x(), params$mean, params$sd
            ),
            font = list(size = 14)
          ),
          xaxis = list(
            title = "Y",
            range = c(get_axis_limits()$y_min, get_axis_limits()$y_max)
          ),
          yaxis = list(
            title = "Densité",
            range = c(0, max(density) * 1.1)
          ),
          showlegend = TRUE
        )
    } else {
      # Retourner un graphique vide avec un message d'erreur
      plot_ly() %>%
        layout(
          title = "Erreur : Valeurs hors limites",
          xaxis = list(title = "Y"),
          yaxis = list(title = "Densité"),
          annotations = list(
            x = 0.5,
            y = 0.5,
            text = "Impossible d'afficher la distribution pour ces paramètres",
            showarrow = FALSE
          )
        )
    }
  })

  create_intro_mesh <- function() {
    # Créer une grille de points
    x <- seq(-3, 3, length.out = 120)
    y <- seq(-3, 3, length.out = 120)
    grid <- expand.grid(x = x, y = y)

    # Matrice de variance-covariance avec rho = 0.75
    sigma <- matrix(c(
      1, 0.75,
      0.75, 1
    ), 2, 2)

    # Calculer les densités
    z <- matrix(
      dmvnorm(grid,
        mean = c(0, 0),
        sigma = sigma
      ),
      nrow = length(x)
    )

    plot_ly() %>%
      add_surface(
        x = x,
        y = y,
        z = z,
        type = "surface",
        showscale = FALSE,
        contours = list(
          x = list(show = TRUE, width = 1.5, color = "black"),
          y = list(show = TRUE, width = 1.5, color = "black")
        ),
        colorscale = list(
          c(0, "#440154"), # Couleur de base (violet foncé)
          c(0.5, "#21908C"), # Couleur intermédiaire (turquoise)
          c(1, "#FDE725") # Couleur maximale (jaune)
        ),
        opacity = 0.95,
        lighting = list(
          ambient = 0.6,
          diffuse = 0.8,
          fresnel = 0.2,
          specular = 0.5,
          roughness = 0.5
        )
      ) %>%
      layout(
        scene = list(
          camera = list(
            eye = list(x = 1.5, y = 0.5, z = 0.25)
          ),
          xaxis = list(title = "X", range = c(-3, 3)),
          yaxis = list(title = "Y", range = c(-3, 3)),
          zaxis = list(title = "Densité"),
          aspectratio = list(x = 1, y = 1, z = 0.7)
        ),
        showlegend = FALSE,
        margin = list(l = 0, r = 0, t = 0, b = 0) # Maximiser l'espace du graphique
      )
  }

  # Rendu du graphique mesh d'introduction
  output$intro_mesh_plot <- renderPlotly({
    create_intro_mesh()
  })

  output$bivariate_conditional_plot <- renderPlotly({
    data <- generate_data()
    params <- get_conditional_params()

    if (input$plot_type_cond == "contour") {
      # Version contour
      p <- plot_ly() %>%
        add_contour(
          x = data$x,
          y = data$y,
          z = data$z,
          colorscale = get_colorscale(),
          showscale = TRUE,
          contours = list(
            showlines = TRUE,
            coloring = "heatmap"
          )
        ) %>%
        # Ligne verticale à X=x
        add_segments(
          x = cond_x(),
          xend = cond_x(),
          y = get_axis_limits()$y_min,
          yend = get_axis_limits()$y_max,
          line = list(color = "darkgreen", width = 2),
          name = "X = x"
        ) %>%
        add_segments(
          x = get_axis_limits()$x_min,
          xend = get_axis_limits()$x_max,
          y = threshold_y(),
          yend = threshold_y(),
          line = list(color = "green", width = 2, dash = "dot"),
          name = "Y = seuil"
        )

      if (input$show_regression_cond) {
        x_seq <- seq(get_axis_limits()$x_min, get_axis_limits()$x_max, length.out = 100)
        y_reg <- mean_y() + input$correlation * (sd_y() / sd_x()) * (x_seq - mean_x())
        p <- p %>% add_lines(
          x = x_seq,
          y = y_reg,
          line = list(color = "yellow", dash = "dash"),
          name = "Droite de régression"
        )
      }

      p %>% layout(
        xaxis = list(title = "X", range = c(get_axis_limits()$x_min, get_axis_limits()$x_max)),
        yaxis = list(title = "Y", range = c(get_axis_limits()$y_min, get_axis_limits()$y_max)),
        showlegend = TRUE
      )
    } else {
      # Version 3D
      # Trouver la hauteur de la distribution bivariée à x=cond_x()
      x_idx <- which.min(abs(data$x - cond_x()))
      z_slice <- data$z[x_idx, ]
      max_z_at_x <- max(z_slice)

      # Calculer la distribution conditionnelle
      y_seq <- seq(get_axis_limits()$y_min, get_axis_limits()$y_max, length.out = 100)
      conditional_density <- dnorm(y_seq, mean = params$mean, sd = params$sd)
      normalized_density <- conditional_density * (max_z_at_x / max(conditional_density))

      plot_ly() %>%
        # Surface principale
        add_surface(
          x = data$x,
          y = data$y,
          z = data$z,
          colorscale = get_colorscale(),
          showscale = TRUE
        ) %>%
        # Plan vertical à X=x
        add_surface(
          x = matrix(rep(cond_x(), 2), nrow = 2),
          y = matrix(c(
            get_axis_limits()$y_min, get_axis_limits()$y_max,
            get_axis_limits()$y_min, get_axis_limits()$y_max
          ), nrow = 2),
          z = matrix(c(0, 0, max(data$z), max(data$z)), nrow = 2),
          colorscale = list(c(0, "rgba(255,0,0,0.3)"), c(1, "rgba(255,0,0,0.3)")),
          opacity = 0.8,
          showscale = FALSE,
          name = "Plan X = x"
        ) %>%
        # Plan vertical à Y=threshold_y
        add_surface(
          x = matrix(c(
            get_axis_limits()$x_min, get_axis_limits()$x_max,
            get_axis_limits()$x_min, get_axis_limits()$x_max
          ), nrow = 2),
          y = matrix(rep(threshold_y(), 4), nrow = 2),
          z = matrix(c(0, 0, max(data$z), max(data$z)), nrow = 2),
          colorscale = list(c(0, "rgba(0,255,0,0.3)"), c(1, "rgba(0,255,0,0.3)")),
          opacity = 0.8,
          showscale = FALSE,
          name = "Plan Y = seuil"
        ) %>%
        # Tranche de la densité conjointe à X = x, proportionnelle à Y | X = x
        add_trace(
          type = "scatter3d",
          x = rep(cond_x(), length(y_seq)),
          y = y_seq,
          z = normalized_density,
          mode = "lines",
          line = list(color = "black", width = 6),
          name = "Tranche de densité à X = x"
        ) %>%
        layout(
          scene = list(
            camera = camera_position(),
            xaxis = list(title = "X", range = c(get_axis_limits()$x_min, get_axis_limits()$x_max)),
            yaxis = list(title = "Y", range = c(get_axis_limits()$y_min, get_axis_limits()$y_max)),
            zaxis = list(title = "Densité"),
            aspectratio = list(x = 1, y = 1, z = 0.7)
          )
        )
    }
  })

  # Mise à jour pour le résumé des paramètres actuels
  output$current_params_summary <- renderUI({
    withMathJax(
      HTML(sprintf(
        "<b>Distribution marginale de X :</b><br>
        $X \\sim N(\\mu_x = %.2f, \\sigma_x = %.2f)$<br><br>
        <b>Distribution marginale de Y :</b><br>
        $Y \\sim N(\\mu_y = %.2f, \\sigma_y = %.2f)$<br><br>
        <b>Corrélation :</b><br>
        $\\rho = %.2f$",
        mean_x(), sd_x(),
        mean_y(), sd_y(),
        input$correlation
      ))
    )
  })

  # Affichage des paramètres conditionnels avec LaTeX
  output$conditional_params_display <- renderUI({
    params <- get_conditional_params()
    withMathJax(
      HTML(sprintf(
        "<b>Moyenne conditionnelle :</b><br>
       $\\mu_{Y|X=%g} = %g + %g \\times \\frac{%g}{%g}(%g-%g) = %.3f$
       <br><br>
       <b>Écart-type conditionnel :</b><br>
       $\\sigma_{Y|X=%g} = %g\\sqrt{1-%g^2} = %.3f$",
        cond_x(), mean_y(), input$correlation, sd_y(), sd_x(), cond_x(), mean_x(), params$mean,
        cond_x(), sd_y(), input$correlation, params$sd
      ))
    )
  })

  output$conditional_prob_display <- renderUI({
    prob <- calculate_conditional_prob()
    withMathJax(
      HTML(sprintf(
        "<b>Probabilité conditionnelle :</b><br>\n
     $P(Y > %.2f \\, | \\, X = %.2f) = %.4f$",
        threshold_y(),
        cond_x(),
        prob
      ))
    )
  })
}

shinyApp(ui = ui, server = server)
