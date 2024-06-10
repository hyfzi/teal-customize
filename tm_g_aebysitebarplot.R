#'Create a Bar plot Related to AE
#'
#'This module is used to display a bar plot related to the adverse event incidence rate. You
#'can obtain the adverse event incidence rate under different categories by selecting the appropriate X variables.
#'
#'The algorithm for numerator and denominator depends on the selection of different X variables as follows:
#'
#'When the X variable is chosen as SITEID
#'  - Numerator:Number of subjects experiencing adverse events in each SITEID
#'  - Denominator:Number of subjects in each SITEID within ADSL.
#'
#'When the X variable is chosen as AESOC/AEDECOD
#'  - Numerator:Number of adverse events classified under each AESOC/AEDECOD
#'  - Denominator:Number of subjects in the ADSL.
#'
#'Note:Since this module is based on the 'tm_g_scatterplot' module, there are some restrictions
#'on the input for the Y variable. We recommend, or rather enforce, that the Y variable selection
#'be the SITEID from the ADSL dataset.
#'
#'Create a plot with the \code{\link{ggplot2}[geom_point]} function
#'
#' @param label (character(1)) Label shown in the navigation item for the module.
#' @param x (`data_extract_spec` or `list` of multiple `data_extract_spec`) Variable
#'   names selected to plot along the x-axis by default.
#' @param y (`data_extract_spec` or `list` of multiple `data_extract_spec`) Variable
#'   names selected to plot along the y-axis by default.
#' @param plot_height optional, (numeric) A vector of length three with ⁠c(value, min and max)
#' ⁠ for a slider encoding the plot height.
#' @param plot_width optional, (numeric) A vector of length three with ⁠c(value, min and max)
#' ⁠ for a slider encoding the plot width.
#' @param rotate_xaxis_labels optional, (logical) Whether to rotate plot
#' X axis labels. Does not rotate by default (FALSE).
#' @param pre_output (shiny.tag, optional)  with text placed before the output to put
#' the output into context. For example a title.
#' @param post_output (shiny.tag, optional) with text placed after the output to put the output into context.
#' For example the shiny::helpText() elements are useful.
#' @param table_dec optional, (`integer`) Number of decimal places used to round numeric values in the table.
#' @param ggplot2_args 	(ggplot2_args) object created by teal.widgets::ggplot2_args() with settings for
#'  the module plot. The argument is merged with options variable teal.ggplot2_args and default module setup.
#'
#' @export
#'
#' @examples
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADAE <- synthetic_cdisc_data("latest")$adae
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"),
#'     cdisc_dataset("ADAE", ADAE, code = "ADAE <- synthetic_cdisc_data(\"latest\")$adae"),
#'     check = TRUE
#'   ),
#'
#'   modules = modules(tm_g_aebysitebarplot(
#'     label = "Bar Plot",
#'     x = data_extract_spec(
#'       dataname = "ADAE",
#'       select = select_spec(
#'         label = "Select variable:",
#'         choices = variable_choices(ADAE, c("SITEID", "AEDECOD", "AESOC")),
#'         selected = "SITEID",
#'         multiple = FALSE,
#'         fixed = FALSE
#'       )
#'     ),
#'    y = data_extract_spec(
#'      dataname = "ADSL",
#'      select = select_spec(
#'        label = "Select variable:",
#'        choices = variable_choices(ADSL, c("SITEID")),
#'        selected = "SITEID",
#'        multiple = FALSE,
#'        fixed = FALSE)),
#'     ggplot2_args = teal.widgets::ggplot2_args(
#'       labs = list(subtitle = "Bar Plot")
#'     )
#'   )
#'
#'   )
#' )
#' \dontrun{
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_g_aebysitebarplot <- function(label = "Barplot",
                                 x,
                                 y,
                                 plot_height = c(600, 200, 2000),
                                 plot_width = NULL,
                                 rotate_xaxis_labels = FALSE,
                                 pre_output = NULL,
                                 post_output = NULL,
                                 table_dec = 2,
                                 ggplot2_args = teal.widgets::ggplot2_args()) {
  logger::log_info("Initializing tm_g_aebysitebarplot")
  if (!requireNamespace("ggpmisc", quietly = TRUE)) {
    stop("Cannot load ggpmisc - please install the package or restart your session.")
  }
  if (!requireNamespace("ggExtra", quietly = TRUE)) {
    stop("Cannot load ggExtra - please install the package or restart your session.")
  }
  if (!requireNamespace("colourpicker", quietly = TRUE)) {
    stop("Cannot load colourpicker - please install the package or restart your session.")
  }
  if (inherits(x, "data_extract_spec")) x <- list(x)
  if (inherits(y, "data_extract_spec")) y <- list(y)
  
  checkmate::assert_string(label)
  checkmate::assert_list(x, types = "data_extract_spec")
  checkmate::assert_list(y, types = "data_extract_spec")
  
  checkmate::assert_scalar(table_dec)
  checkmate::assert_flag(rotate_xaxis_labels)
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )
  
  checkmate::assert_class(ggplot2_args, "ggplot2_args")
  
  args <- as.list(environment())
  
  data_extract_list <- list(
    x = x,
    y = y
  )
  
  module(
    label = label,
    server = srv_g_aebysitebarplot,
    ui = ui_g_aebysitebarplot,
    ui_args = args,
    server_args = c(
      data_extract_list,
      list(plot_height = plot_height, plot_width = plot_width, table_dec = table_dec, ggplot2_args = ggplot2_args)
    ),
    filters = teal.transform::get_extract_datanames(data_extract_list)
  )
}

ui_g_aebysitebarplot <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    args$x,
    args$y
  )
  
  
  shiny::tagList(
    include_css_files("custom"),
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        teal.widgets::plot_with_settings_ui(id = ns("scatter_plot"))
      ),
      encoding = div(
        ### Reporter
        teal.reporter::simple_reporter_ui(ns("simple_reporter")),
        tags$label("Encodings", class = "text-primary"),
        teal.transform::datanames_input(args[c("x")]),
        teal.transform::datanames_input(args[c("y")]),
        
        teal.transform::data_extract_ui(
          id = ns("x"),
          label = "Y variable",
          data_extract_spec = args$x,
          is_single_dataset = is_single_dataset_value
        ),
        shinyjs::hidden(
          teal.transform::data_extract_ui(
            id = ns("y"),
            label = "catogray",
            data_extract_spec = args$y,
            is_single_dataset = is_single_dataset_value
          )
        ),
        numericInput(ns('num'),
                     "Select Top n SOCs/PTs by Incidence Rate",
                     min=1,
                     value = 10),
        
        sliderInput(
          ns('point_size'),
          "Select Bar Width",
          min=0,
          max=1,
          value=0.5
        ),
        checkboxInput(ns("rotate_xaxis_labels"), "Rotate Y axis labels", value = args$rotate_xaxis_labels),
      ),
      forms = teal::get_rcode_ui(ns("rcode")),
      pre_output = args$pre_output,
      post_output = args$post_output
    )
  )
}

srv_g_aebysitebarplot <- function(id,
                                  datasets,
                                  reporter,
                                  x,
                                  y,
                                  plot_height,
                                  plot_width,
                                  table_dec,
                                  ggplot2_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  moduleServer(id, function(input, output, session) {
    teal.code::init_chunks()
    
    data_extract <- list(
      x = x,
      y = y
    )
    
    data_extract <- data_extract[!vapply(data_extract, is.null, logical(1))]
    selector_list <- teal.transform::data_extract_multiple_srv(data_extract, datasets)
    
    
    reactive_select_input <- reactive({
      selectors <- selector_list()
      extract_names <- names(selectors)
      for (extract in extract_names) {
        if (is.null(selectors[[extract]]) || length(selectors[[extract]]()$select) == 0) {
          selectors <- selectors[-which(names(selectors) == extract)]
        }
      }
      selectors
    })
    
    merged_data <- teal.transform::data_merge_srv(
      selector_list = reactive_select_input,
      datasets = datasets,
      merge_function = "dplyr::right_join"
    )
    
    
    output$num_na_removed <- renderUI({
      if (add_trend_line()) {
        ANL <- merged_data()$data() # nolint
        x_var <- as.vector(merged_data()$columns_source$x)
        if ((num_total_na <- nrow(ANL) - nrow(stats::na.omit(ANL[, c(x_var)]))) > 0) {
          shiny::tags$div(paste(num_total_na, "row(s) with missing values were removed"), shiny::tags$hr())
        }
      }
    })
    
    observeEvent(
      eventExpr = {
        req(length(reactive_select_input()) > 0)
      }, handlerExpr = {
        shinyjs::hide("free_scales")
      }
    )
    
    plot_r <- reactive({
      validate({
        need(all(c("x") %in% names(reactive_select_input())), "Please select X")
      })
      
      teal.code::chunks_reset()
      teal.code::chunks_push_data_merge(merged_data())
      
      ANL <- merged_data()$data() # nolint
      
      teal::validate_has_data(ANL, 1)
      
      x_var <- as.vector(merged_data()$columns_source$x)
      
      # Change the Y-axis Length
      if(x_var=='x.SITEID'){
        
        ylim1 <- ANL %>% mutate(aesub = case_when(!is.na(x_var) ~ USUBJID))  %>%
          group_by(!!sym(x_var)) %>% summarise(aesubct = (n_distinct(aesub,na.rm = TRUE)), bign = n_distinct(USUBJID)) %>%
          mutate(y_var = max(aesubct/bign)) %>%
          filter(!is.na(!!sym(x_var))) %>%
          slice(which.max(y_var)) %>% select(y_var)
        
        ylim <- ylim1$y_var + 0.15 %>% as.numeric()
        
      }
      else{
        
        ylim1 <- ANL %>% mutate(aesub = case_when(!is.na(x_var) ~ USUBJID)) %>%
          group_by(!!sym(x_var)) %>% summarise(aesubct = (n_distinct(aesub,na.rm = TRUE))) %>%
          filter(!is.na(!!sym(x_var))) %>%
          mutate(y_var = aesubct/n_distinct(ANL$USUBJID)) %>% slice(which.max(y_var)) %>% select(y_var)
        ylim <- ylim1$y_var + 0.08  %>% as.numeric()
      }
      
      # Define Object Name for Subsequent Calculations and Label Display
      y_var <- as.vector('PCT')
      # Numerator
      aesubct <- as.vector('AESUBCT')
      # Denominator
      bign <- as.vector('BIGN')
      # Number of Subjects with AE
      aesub <- as.vector('AESUB')
      
      aeterm <- as.vector('AETERM')
      size <- input$size # nolint
      point_size <- input$point_size
      num <- input$num
      rotate_xaxis_labels <- input$rotate_xaxis_labels # nolint
      color <- input$color # nolint
      validate(need(length(x_var) == 1, "There must be exactly one x var."))
      
      teal::validate_has_data(ANL[, c(x_var)], 1, complete = TRUE, allow_inf = FALSE)
      
      point_sizes <- {point_size}
      num <- {num}
      ysiteid <- as.vector("y.SITEID")
      pre_pro_anl <- "ANL"
      ord <- "ord"
      
      
      if (x_var == "x.SITEID"){
        # SITEID Calculation
        plot_call <- substitute(expr = pre_pro_anl %>%
                                  # aesub Number of Subjects with AE
                                  mutate(!!aesub := case_when(!is.na(!!sym(x_var)) ~ USUBJID)) %>%
                                  # ysiteid:Number of People in Each Site -ADSL
                                  group_by(!!sym(ysiteid)) %>%
                                  # aesubct:Numerator, totsubct:Denominator
                                  summarise(!!aesubct := (n_distinct(!!sym(aesub),na.rm = TRUE)), !!bign := n_distinct(USUBJID)) %>%
                                  # rename: for plot
                                  rename(x.SITEID = y.SITEID) %>%
                                  # PCT
                                  mutate(!!y_var := !!sym(aesubct)/!!sym(bign)) %>%
                                  filter(!is.na(!!sym(x_var)) & !!sym(y_var) != 0 ) %>%
                                  # ord: change the display order
                                  mutate(!!ord := !!sym(y_var) * 10000 + !!sym(bign)) %>%
                                  ggplot() ,
                                env = list(pre_pro_anl = str2lang(pre_pro_anl), x_var = x_var, y_var = y_var,
                                           aesubct = aesubct, bign = bign, aesub = aesub, ysiteid = ysiteid, ord = ord))
      }
      else{
        # Select Top n SOC/PT Counts by Frequency
        plot_call <- substitute(expr = pre_pro_anl %>%
                                  mutate(!!aesub := case_when(!is.na(!!sym(x_var)) ~ USUBJID)) %>%
                                  group_by(!!sym(x_var)) %>%
                                  
                                  summarise(!!aesubct := (n_distinct(!!sym(aesub), na.rm = TRUE))) %>%
                                  mutate(!!y_var := !!sym(aesubct) / n_distinct(pre_pro_anl$USUBJID)) %>%
                                  mutate(!!ord := !!sym(y_var) * 10000 + n_distinct(pre_pro_anl$USUBJID)) %>%
                                  mutate(!!bign := n_distinct(pre_pro_anl$USUBJID)) %>%
                                  filter(!is.na(!!sym(x_var))) %>%
                                  slice_max(order_by = !!sym(y_var), n = num) %>%
                                  ggplot(), env = list(pre_pro_anl = str2lang(pre_pro_anl), x_var = x_var, y_var = y_var,
                                                       aesubct = aesubct, bign = bign, aesub = aesub,num = num, ord = ord))
      }
      
      plot_call <- {
        substitute(
          expr = plot_call +
            aes(x = fct_reorder(x_name, ord), y = m) +
            geom_col(alpha = 1, width = point_sizes, fill = "lightblue", colour = "black") +
            # add label
            # geom_text(aes(label = paste(n,'/',bign,'(', sprintf(table_dec1, round(m*100, digits = table_dec)),'%)')), hjust = -.1) +
            geom_text(aes(label = paste(n,'/',bign,' (',  round(m*100, digits = table_dec),'%)')), hjust = -0.1) +
            # rotate plot
            coord_flip() +
            scale_y_continuous(labels = scales::percent, limits = c(0, ylim))
          ,
          env = list(
            plot_call = plot_call,
            x_name = as.name(x_var),
            point_sizes = point_sizes,
            m = as.name(y_var),
            n = as.name(aesubct),
            bign = as.name(bign),
            ylim = ylim,
            table_dec=table_dec,
            ord = as.name(ord)
          )
        )
      }
      
      plot_label_generator <- function(rhs_formula = quote(x ~ 1),
                                       show_form = input$show_form,
                                       show_r2 = input$show_r2,
                                       pos = input$pos,
                                       label_size = input$label_size) {
        stopifnot(sum(show_form, show_r2) >= 1)
        aes_label <- paste0(
          "aes(",
          "label = ",
          if (sum(show_form, show_r2) > 1) "paste(",
          paste(
            c(
              if (show_form) "stat(eq.label)",
              if (show_r2) "stat(adj.rr.label)",
            ),
            collapse = ", "
          ),
          if (sum(show_form, show_r2) > 1) ", sep = '*\", \"*'))" else ")"
        )
        label_geom <- substitute(
          expr = ggpmisc::stat_poly_eq(
            mapping = aes_label,
            formula = rhs_formula,
            parse = TRUE,
            label.x = pos,
            size = label_size
          ),
          env = list(
            rhs_formula = rhs_formula,
            pos = pos,
            aes_label = str2lang(aes_label),
            label_size = label_size
          )
        )
        substitute(
          expr = plot_call + label_geom,
          env = list(
            plot_call = plot_call,
            label_geom = label_geom
          )
        )
      }
      
      x_label <-
        varname_w_label(
          x_var,
          ANL,
          prefix =  NULL,
          suffix =  NULL
        )
      
      dev_ggplot2_args <- teal.widgets::ggplot2_args(
        labs = list(x = x_label),
        theme = list(legend.position = "bottom")
      )
      
      # rotate y label
      if (rotate_xaxis_labels) {
        dev_ggplot2_args$theme[["axis.text.y"]] <- quote(element_text(angle = -25, vjust = 1)) # nolint
      }
      
      all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args,
        module_plot = dev_ggplot2_args
      )
      
      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(all_ggplot2_args, ggtheme = "gray")
      
      plot_call <- substitute(
        expr = plot_call +
          labs +
          ggthemes +
          themes,
        env = list(
          plot_call = plot_call,
          labs = parsed_ggplot2_args$labs,
          ggthemes = parsed_ggplot2_args$ggtheme,
          themes = parsed_ggplot2_args$theme
        )
      )
      
      plot_call <- substitute(expr = p <- plot_call, env = list(plot_call = plot_call))
      teal.code::chunks_push(id = "plot_call", expression = plot_call)
      
      # explicitly calling print on the plot inside the chunk evaluates
      # the ggplot call and therefore catches errors
      plot_print_call <- quote(p)
      teal.code::chunks_push(id = "print_plot_call", expression = plot_print_call)
      teal.code::chunks_safe_eval()
      teal.code::chunks_get_var(var = "p")
    })
    
    # Insert the plot into a plot_with_settings module from teal.widgets
    teal.widgets::plot_with_settings_srv(
      id = "scatter_plot",
      plot_r = plot_r,
      height = plot_height,
      # width = plot_width,
      clicking = FALSE)
    
    teal::get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      datanames = teal.transform::get_extract_datanames(list(x)),
      modal_title = "R Code for a Barplot",
      code_header = "Barplot"
    )
    
    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal.reporter::TealReportCard$new()
        card$set_name("Bar Plot")
        card$append_text("Bar Plot", "header2")
        card$append_fs(datasets$get_filter_state())
        card$append_text("Plot", "header3")
        card$append_plot(plot_r())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    
  })
}

