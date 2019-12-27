# LIBRARIES ======================================================================

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(DT)
library(tidyverse)
library(lubridate)
library(quantmod)



# CONSTANTS =============================================================================

df_prices <- read_csv(file = "prices.csv") 

symbols <- unique(read_csv(file = "pos.csv")$symbol)

format_accounting <- function(x, cents = TRUE, currency = "$") {
      m <- as.double(x)
      if (any(is.na(m))) stop("x is not numeric")
      if (! is.logical(cents)) stop("cents should be TRUE or FALSE")
      nsmall <- if (cents) 2 else 0
      if (m < 0) {
            mn <- abs(m)
            mn <- round(x = mn, digits = 2)
            mn <- format(x = mn, big.mark = ",", nsmall = nsmall)
            paste0(currency, "(", mn, ")")
      } else {
            mp <- round(x = m, digits = 2)
            mp <- format(x = mp, big.mark = ",", nsmall = nsmall)
            paste0(currency, mp)
      }
}

inputID_loop <- function(id, loop) {
      paste(id, loop, sep = "_")
}



# ============================================================================================================================
# UI =========================================================================================================================
# ============================================================================================================================

ui <- fluidPage(
      title = "Portfolio Manager",
      theme = shinytheme(theme = "lumen"),
      useShinyjs(),
      includeCSS("portfolio.css"),
      
      div(
            class = "b",
            
            tabsetPanel(
                  id = "tab", 
                  type = "pills",
                  
                  # DASHBOARD
                  
                  tabPanel(
                        title = "", 
                        value = "dash", 
                        icon = icon("dashboard"),
                        
                        div(
                              div(
                                    uiOutput(outputId = "ui_dash_inputs"),
                                    uiOutput(outputId = "ui_b_close"),
                                    class = "i mr shadow glow",
                                    style = "width: 15%;"
                              ),
                              
                              uiOutput(outputId = "ui_dash_cards"),
                              
                              div(
                                    DT::dataTableOutput(outputId = "t_position"),
                                    class = "i mb shadow",
                                    style = "width: 100%;"
                              ),
                              
                              div(
                                    plotOutput(outputId = "p_portfolio"),
                                    class = "i mr shadow",
                                    style = "width: 48%;"
                              ),
                              
                              class = "row mt"
                        )
                  ),
                  
                  # NEW TRADES
                  
                  tabPanel(
                        title = "", 
                        value = "new", 
                        icon = icon("plus"),
                        
                        div(
                              div(
                                    uiOutput(outputId = "ui_newTrades"),
                                    uiOutput(outputId = "ui_newTrades2"),
                                    uiOutput(outputId = "ui_newTrades3"),
                                    class = "i mr shadow glow",
                                    style = "width: 100%;"
                              ),
                              
                              div(
                                    DT::dataTableOutput(outputId = "t_newTrades"),
                                    class = "i mt shadow",
                                    style = "width: 100%;"
                              ),
                              
                              div(
                                    DT::dataTableOutput(outputId = "t_currentPrice"),
                                    uiOutput(outputId = "currentPrice_timestamp"),
                                    class = "i mt shadow",
                                    style = "width: 100%;"
                              ),
                              class = "row mt"
                        )
                  ),
                  
                  # SETTINGS
                  
                  tabPanel(
                        title = "", 
                        value = "settings", 
                        icon = icon("gears"),
                        
                        div(
                              div(
                                    numericInput(inputId = "balance", label = "Initial balance", value = 500, width = "100%"),
                                    uiOutput(outputId = "ui_b_edit"),
                                    class = "i mr shadow glow",
                                    style = "width: 15%;"
                              ),
                              
                              div(
                                    DT::dataTableOutput(outputId = "t_position_edit"),
                                    class = "i shadow",
                                    style = "width: 83%;"
                              ),
                              
                              div(
                                    uiOutput(outputId = "ui_inputs_edit"),
                                    class = "mt",
                                    style = "width: 100%;"
                              ),
                              
                              class = "row mt"
                        )
                  )
                  
            ),
            
            div(
                  verbatimTextOutput(outputId = "test"), 
                  class = "i mt wobble",
                  style = "width: 100%;"
            )
      )
      
)



# ============================================================================================================================
# SERVER =====================================================================================================================
# ============================================================================================================================

server <- function(input, output, session) {
      
      # TESTING --------------------------------------------------------------------------------------------------------------
      
      output$test <- renderPrint({
            req(NULL)
            
            # tryCatch(
            #       expr = {
            #             
            #       },
            #       error = function(e) return(e)
            # )
            
            
            
      })
      
      rv <- reactiveValues(
            prices = {
                  if (as_date(max(df_prices$Trade.Time[1])) != today()) { 
                        df_prices <- symbols %>% map_df(function(x) {
                              getQuote(Symbols = x) %>%
                                    data.frame(symbol = x, stringsAsFactors = FALSE)
                        })
                        write_csv(x = df_prices, path = "run.csv") 
                        read_csv(file = "run.csv")
                  } else {
                        df_prices
                  }
            },
            pos = read_csv(file = "pos.csv")
      )
      
      
      
      # DASHBOARD ==================================================================================================================================
      
      # Dashboard inputs
      
      output$ui_dash_inputs <- renderUI({
            req(rv$prices)
            list(
                  # dateInput(inputId = "date", label = strong("Date"), value = max(rv$prices$date), min = min(df_prices$date), max = today(), format = "M d yyyy", width = "100%"),
                  # selectInput(inputId = "symbol", label = strong("Symbol"), choices = symbols, selected = "MSFT", width = "100%"),
                  # selectInput(inputId = "index", label = strong("Index"), choices =  unique(rv$prices$index), selected = "Close", width = "100%"),
                  actionButton(inputId = "b_refreshPrices", label = "Refresh prices", width = "100%", class = "btn-info"),
                  div("Prices as of ", max(rv$prices$Trade.Time[1]), style = "text-align: center; color: #a0a0a0; font-size: 12px;")
            )
            
      })
      
      
      # Refresh prices
      
      observeEvent(input$b_refreshPrices, {
            withProgress(
                  expr = {
                        df_prices <- symbols %>% map_df(function(x) {
                              getQuote(Symbols = x) %>%
                                    data.frame(symbol = x, stringsAsFactors = FALSE)
                        })
                        write_csv(df_prices, "prices.csv") 
                        rv$prices <- read_csv(file = "prices.csv")
                  }, 
                  message = "Updating prices"
            )
      })
      
      
      # Positions
      
      df_position <- reactive({
            rv$prices %>%
                  select(valueDate = Trade.Time, symbol, close = Last) %>%
                  inner_join(y = rv$pos, by = "symbol") %>%
                  mutate(
                        return = (close - execution - commission) * quantity, 
                        notional = close * quantity, 
                        investment = execution * quantity, 
                        roi = return / abs(investment)
                  )
      })
      
      output$t_position <- DT::renderDataTable({
            df_position() %>%
                  filter(status == "Open") %>%
                  select(Symbol = symbol, `Bought at` = execution, `Current` = close, Shares = quantity, Type = type, `P&L` = return, Return = roi, Investment = investment, Notional = notional, Commission = commission) %>%
                  datatable(options = list(dom = "t"), style = "bootstrap", rownames = FALSE, selection = "multiple") %>%
                  formatStyle(columns = 1:10, backgroundColor = "#282828") %>%
                  formatStyle(columns = 6:7, color = styleInterval(cuts = 0, values = c("#dd4e1a", "#39c900"))) %>%
                  formatCurrency(columns = c(2:3, 6, 8:10), currency = "$", interval = 3, mark = ",", digits = 2, before = TRUE) %>%
                  formatPercentage(columns = 7, digits = 2)
      })
      
      
      # Dashboard cards
      
      dash_cards <- reactive({
            df <- df_position() %>% filter(status == "Open")
            dfClosed <- df_position() %>% filter(status == "Closed")
            list(
                  pl = df$return %>% sum %>% format_accounting,
                  return = round(sum(df$return) / input$balance * 100, 2) %>% paste0("%"),
                  equity = format_accounting(input$balance + sum(df$return)),
                  shares = df$quantity %>% sum %>% format_accounting(currency = ""),
                  rpl = dfClosed$return %>% sum %>% format_accounting,
                  bp = format_accounting(input$balance - sum(df$notional))
            )
      })
      
      output$ui_dash_cards <- renderUI({
            m <- dash_cards()
            list(
                  div(
                        div("P&L", style = "font-size: 14px; text-align: left; padding: 1%;"), 
                        div(m$pl, style = "font-weight: bold; font-size: 1.33em;"), 
                        class = "i mr mb info"
                  ),
                  div(
                        div("Return", style = "font-size: 14px; text-align: left; padding: 1%;"), 
                        div(m$return, style = "font-weight: bold; font-size: 1.33em;"), 
                        class = "i mr mb info"
                  ),
                  div(
                        div("Equity", style = "font-size: 14px; text-align: left; padding: 1%;"), 
                        div(m$equity, style = "font-weight: bold; font-size: 1.33em;"), 
                        class = "i mr mb info"
                  ),
                  div(
                        div("Realized P&L", style = "font-size: 14px; text-align: left; padding: 1%;"), 
                        div(m$rpl, style = "font-weight: bold; font-size: 1.33em;"), 
                        class = "i mr mb info"
                  ),
                  div(
                        div("Buying Power", style = "font-size: 14px; text-align: left; padding: 1%;"), 
                        div(m$bp, style = "font-weight: bold; font-size: 1.33em;"), 
                        class = "i mr mb info"
                  )
            )
      })
      
      
      # Dashboard plots
      
      output$p_portfolio <- renderPlot({
            df <- df_position() %>%
                  filter(status == "Open") %>%
                  add_row(symbol = "CASH", notional = (input$balance + sum(df_position()$return)) - sum(df_position()$notional))
            
            df %>%
                  mutate(percent = round(notional / sum(df$notional) * 100, 1)) %>%
                  arrange(notional) %>%
                  mutate(symbol = factor(symbol, levels = symbol %>% unique)) %>%
                  ggplot +
                  geom_bar(aes(x = symbol, y = notional), stat = "identity", fill = "#7fabd1", alpha = 0.75, width = 0.5) +
                  geom_text(aes(x = symbol, y = notional, label = percent %>% paste0("%")), hjust = 1.25, color = "white", size = 7) +
                  scale_y_continuous(name = "", labels = scales::dollar) +
                  xlab("") +
                  ggtitle(label = NULL, subtitle = "Portfolio breakdown by notional value") +
                  coord_flip() +
                  theme_dark(base_size = 20) +
                  theme(
                        text = element_text(colour = "white"),
                        plot.background = element_rect(fill = "#282828", color = "#282828"),
                        panel.background = element_rect(fill = "#282828"),
                        panel.border = element_blank(),
                        panel.grid.major = element_line(color = "#4c4c4c"),
                        panel.grid.minor = element_line(color = "#4c4c4c"),
                        axis.text = element_text(colour = "white")
                  ) 
      })
      
      
      
      
      
      
      # CLOSE TRADES ==========================================================================================================
      
      df_close <- reactive({
            req(length(input$t_position_rows_selected) > 0)
            df_position()[input$t_position_rows_selected, ]
      })
      
      check_close <- reactive({
            df_close() %>% 
                  filter(status == "Open" & order == "Filled") %>% 
                  group_by(symbol) %>% 
                  summarise(quantity = sum(quantity)) %>%
                  mutate(flat = quantity != 0) %>%
                  "$"("flat") %>%
                  any
      })
      
      output$ui_b_close <- renderUI({
            req(! check_close())
            list(
                  tags$br(),
                  actionButton(inputId = "b_close", label = "Close trades", class = "btn-info", width = "100%")
            )
      })
      
      output$t_close_shorts <- renderTable({
            df_close() %>%
                  filter(type == "SELL") %>%
                  arrange(symbol, execution) %>%
                  mutate(execution = format_accounting(execution), quantity = format_accounting(quantity, cents = FALSE, currency = "")) %>%
                  select(`Trade ID` = id, Symbol = symbol, Shares = quantity, Price = execution)
      }, striped = TRUE, hover = TRUE, rownames = FALSE)
      
      output$t_close_longs <- renderTable({
            df_close() %>%
                  filter(type == "BUY") %>%
                  arrange(symbol, execution) %>%
                  mutate(execution = format_accounting(execution), quantity = format_accounting(quantity, cents = FALSE, currency = "")) %>%
                  select(`Trade ID` = id, Symbol = symbol, Shares = quantity, Price = execution)
      }, striped = TRUE, hover = TRUE, rownames = FALSE)
      
      observeEvent(input$b_close, {
            showModal(
                  modalDialog(
                        title = "You are about to match the following trades", 
                        div(
                              div(tableOutput(outputId = "t_close_longs"), class = "i mb", style = "width: 50%;"),
                              div(tableOutput(outputId = "t_close_shorts"), class = "i mb", style = "width: 50%;"),
                              tags$p("P&L: ", df_close()$return %>% sum %>% format_accounting, style = "font-size: 2em;")
                        ),
                        footer = list(
                              actionButton(inputId = "b_close_cancel", label = "Cancel", width = "48%", class = "btn-danger i mr"),
                              actionButton(inputId = "b_close_confirm", label = "Match", width = "48%", class = "btn-success i ml")
                        ), 
                        easyClose = TRUE,
                        size = "l"
                  )
            )            
      })
      
      observeEvent(input$b_close_cancel, removeModal())
      
      observeEvent(input$b_close_confirm, {
            update <- rv$pos
            update$status[df_close()$id] <- "Closed"
            update$match[df_close()$id] <- if (is.na(max(update$match))) 1 else max(update$match) + 1
            write_csv(x = update, path = "pos.csv")
            rv$pos <- read_csv(file = "pos.csv")
            removeModal()
      })
      
      
      
      # NEW TRADES =============================================================================================================================
      
      # Add inputs
      
      output$ui_newTrades <- renderUI({
            div(selectInput(inputId = "new_symbol", label = strong("Symbol"), choices = c(rv$prices$symbol %>% unique, "New symbol"), width = "100%"), class = "i mr", style = "width: 18%;")
      })
      
      output$ui_newTrades2 <- renderUI({
            req(input$new_symbol)
            newSymbol <- if (input$new_symbol == "New symbol") textInput(inputId = "new_symbol_text", label = strong("New symbol"), placeholder = "MSFT", width = "100%") else NULL
            div(
                  div(newSymbol, class = "i mr", style = "width: 18%;"),
                  div(numericInput(inputId = "new_execution", label = strong("Price"), value = NULL, min = 0, step = 0.01, width = "100%"), class = "i mr", style = "width: 18%;"),
                  div(numericInput(inputId = "new_quantity", label = strong("Quantity"), value = NULL, min = 1, step = 1, width = "100%"), class = "i mr", style = "width: 18%;"),
                  div(selectInput(inputId = "new_type", label = strong("Buy/Sell"), choices = c("BUY", "SELL"), width = "100%"), class = "i", style = "width: 18%;"),
                  class = "row",
                  style = "width: 100%;"
            )
      })
      
      output$ui_newTrades3 <- renderUI({
            div(
                  div(numericInput(inputId = "new_commission", label = strong("Commission"), value = 0, min = 0, step = 1, width = "100%"), class = "i mr", style = "width: 23.5%;"),
                  div(dateInput(inputId = "new_date", label = strong("Trade date"), value = today(), max = today(), format = "M d yyyy", width = "100%"), class = "i mr", style = "width: 23.5%;"),
                  div(selectInput(inputId = "new_order", label = strong("Order type"), choices = c("Filled", "Limit", "Stop"), width = "100%"), class = "i mr", style = "width: 23.5%;"),
                  div(actionButton(inputId = "b_new", label = strong("Add trade"), width = "100%", class = "btn-success"), class = "i", style = "width: 23.5%; padding-top: 30px;"),
                  class = "row",
                  style = "width: 100%; padding-left: 15px;"
            )
      })
      
      df_newTrades <- reactive({
            validate(need(expr = ! is.null(input$new_quantity), message = ""))
            
            shares <- if (is.na(input$new_quantity)) {
                  ""
            } else {
                  if (input$new_type == "SELL") -input$new_quantity else input$new_quantity
            }
            data.frame(
                  id = max(rv$pos$id) + 1,
                  symbol = if (input$new_symbol == "New symbol") toupper(input$new_symbol_text) else input$new_symbol,
                  execution = if (is.na(input$new_execution)) "" else input$new_execution,
                  quantity = shares,
                  type = input$new_type,
                  commission = input$new_commission,
                  date = input$new_date,
                  order = input$new_order,
                  status = "Open",
                  match = NA,
                  stringsAsFactors = FALSE
            )
      })
      
      output$t_newTrades <- DT::renderDataTable({
            df_newTrades() %>%
                  select(`Trade ID` = id, Symbol = symbol, Price = execution, Shares = quantity, `Buy/Sell` = type, Commission = commission, `Trade Date` = date, `Order Type` = order) %>%
                  datatable(options = list(dom = "t"), rownames = FALSE, style = "bootstrap") %>%
                  formatStyle(columns = 1:8, backgroundColor = "#282828") %>%
                  formatCurrency(columns = c("Price", "Commission")) %>%
                  formatCurrency(columns = "Shares", currency = "", digits = 0)
      })
      
      output$t_newTrades_modal <- renderTable({
            df_newTrades() %>%
                  mutate(
                        id = id %>% as.character, 
                        execution = execution %>% format_accounting, 
                        quantity = quantity %>% format_accounting(currency = "", cents = FALSE),
                        commission = commission %>% format_accounting,
                        date = date %>% as.character
                  ) %>%
                  select(
                        `Trade ID` = id, 
                        Symbol = symbol, 
                        Price = execution, 
                        Shares = quantity, 
                        `Buy/Sell` = type, 
                        Commission = commission, 
                        `Trade Date` = date, 
                        `Order Type` = order
                  ) 
      })
      
      observeEvent(input$b_new, {
            showModal(
                  modalDialog(
                        title = "Add new trade", 
                        tableOutput(outputId = "t_newTrades_modal"),
                        footer = list(
                              actionButton(inputId = "b_new_cancel", label = "Cancel", width = "48%", class = "btn-danger i mr"),
                              actionButton(inputId = "b_new_confirm", label = "Save", width = "48%", class = "btn-success i ml")
                        ), 
                        easyClose = TRUE,
                        size = "l"
                  )
            )
      })
      
      observeEvent(input$b_new_cancel, removeModal())
      
      observeEvent(input$b_new_confirm, {
            withProgress(
                  expr = {
                        update <- bind_rows(
                              df_newTrades() %>% map_df(as.character),
                              rv$pos %>% map_df(as.character)
                        )
                        write_csv(x = update, path = "pos.csv")
                        rv$pos <- read_csv(file = "pos.csv")
                  }, 
                  message = "Saving trade"
            )
            removeModal()
      })
      
      
      # Current prices
      
      output$t_currentPrice <- DT::renderDataTable({
            rv$prices %>%
                  select(Symbol = symbol, Last, Change, Open, High, Low, Volume) %>%
                  datatable(options = list(dom = "t"), rownames = FALSE, style = "bootstrap") %>%
                  formatStyle(columns = 1:7, backgroundColor = "#282828") %>%
                  formatStyle(columns = "Change", color = styleInterval(cuts = 0, values = c("#dd4e1a", "#39c900"))) %>%
                  formatCurrency(columns = c("Last", "Change", "Open", "High", "Low"), currency = "$", interval = 3, mark = ",", digits = 2, before = TRUE) %>%
                  formatCurrency(columns = "Volume", currency = "", interval = 3, mark = ",", digits = 0)
      })
      
      output$currentPrice_timestamp <- renderUI({
            rv$prices$Trade.Time %>% 
                  format("%d-%B-%Y %H:%M:%S") %>%
                  span(style = "font-size: 14px; color: #a0a0a0;")
      })
      
      
      # EDIT TRADES ====================================================================================================================
      
      # Positions
      
      output$t_position_edit <- DT::renderDataTable({
            rv$pos %>%
                  mutate(match = ifelse(is.na(match), "", match)) %>%
                  datatable(
                        options = list(dom = "t"), 
                        colnames = c("Trade ID", "Symbol", "Price", "Shares", "Buy/Sell", "Commission", "Trade Date", "Order Type", "Status", "Match ID"), 
                        style = "bootstrap", 
                        selection = "multiple",
                        rownames = FALSE
                  ) %>%
                  formatStyle(columns = colnames(rv$pos), backgroundColor = "#282828") %>%
                  formatCurrency(columns = c("execution", "commission")) %>%
                  formatCurrency(columns = "quantity", currency = "", interval = 3, mark = ",", digits = 0)
      })
      
      output$ui_b_edit <- renderUI({
            req(length(input$t_position_edit_rows_selected) > 0)
            list(
                  p(actionButton(inputId = "b_edit", label = "Edit trades", width = "100%", class = "btn-info")),
                  actionButton(inputId = "b_delete", label = "Delete trades", width = "100%", class = "btn-danger")
            )
      })
      
      df_edit <- reactive({
            rv$pos[input$t_position_edit_rows_selected, ]
      })
      
      output$ui_inputs_edit <- renderUI({
            req(input$t_position_edit_rows_selected)
            input$t_position_edit_rows_selected %>%
                  map(function(x) {
                        m <- rv$pos[x, ]
                        div(
                              div("Trade ID: ", m$id, style = "font-size: 1.5em;"),
                              div(strong(m$symbol), style = "font-size: 1.5em;"),
                              hr(),
                              numericInput(inputId = inputID_loop("edit_execution", x), label = "Price", value = m$execution, min = 0, step = 0.01, width = "100%"),
                              numericInput(inputId = inputID_loop("edit_quantity", x), label = "Quantity", value = abs(m$quantity), min = 1, step = 1, width = "100%"),
                              selectInput(inputId = inputID_loop("edit_type", x), label = "Buy/Sell", choices = c("BUY", "SELL"), selected = m$type, width = "100%"),
                              numericInput(inputId = inputID_loop("edit_commission", x), label = "Commission", value = m$commission, min = 0, step = 1, width = "100%"),
                              dateInput(inputId = inputID_loop("edit_date", x), label = "Trade date", value = m$date, max = today(), format = "M d yyyy", width = "100%"),
                              selectInput(inputId = inputID_loop("edit_order", x), label = "Order type", choices = c("Filled", "Limit", "Stop"), selected = m$order, width = "100%"),
                              class = "i mr mt shadow glow"
                        )
                  })
      })
      
      df_editedTrades <- reactive({
            req(input$t_position_edit_rows_selected)
            input$t_position_edit_rows_selected %>%
                  map_df(function(x) {
                        m_type <- eval(parse(text = paste0("input$", "edit_type", "_", x)))
                        m_quantity_pre <- eval(parse(text = paste0("input$", "edit_quantity", "_", x)))
                        m_quantity <- if (m_type == "BUY") abs(m_quantity_pre) else -m_quantity_pre
                        data.frame(
                              id = rv$pos$id[x],
                              symbol = rv$pos$symbol[x],
                              execution = eval(parse(text = paste0("input$", "edit_execution", "_", x))),
                              quantity = m_quantity,
                              type = m_type,
                              commission = eval(parse(text = paste0("input$", "edit_commission", "_", x))),
                              date = eval(parse(text = paste0("input$", "edit_date", "_", x))),
                              order = eval(parse(text = paste0("input$", "edit_order", "_", x))),
                              status = rv$pos$status[x],
                              match = rv$pos$match[x],
                              stringsAsFactors = FALSE
                        )
                  })
      })
      
      df_edit_bind <- reactive({
            bind_rows(
                  rv$pos[-input$t_position_edit_rows_selected, ] %>% map_df(as.character),
                  df_editedTrades() %>% map_df(as.character)
            )
      })
      
      check_edit <- reactive({
            bind_rows(
                  df_edit_bind(),
                  rv$pos %>% map_df(as.character)
            ) %>% 
                  duplicated %>%
                  sum %>%
                  "=="(nrow(rv$pos))
      })
      
      output$t_editedTrades <- renderTable({
            df_editedTrades() %>%
                  mutate(
                        id = id %>% as.character, 
                        execution = execution %>% format_accounting, 
                        quantity = quantity %>% format_accounting(currency = "", cents = FALSE),
                        commission = commission %>% format_accounting,
                        date = date %>% as.character
                  ) %>%
                  select(
                        `Trade ID` = id, 
                        Symbol = symbol, 
                        Price = execution, 
                        Shares = quantity, 
                        `Buy/Sell` = type, 
                        Commission = commission, 
                        `Trade Date` = date, 
                        `Order Type` = order
                  ) 
      })
      
      observeEvent(input$b_edit, {
            if (check_edit()) {
                  showModal(
                        modalDialog(
                              title = "No changes were made",
                              easyClose = TRUE
                        )
                  )
            } else {
                  showModal(
                        modalDialog(
                              title = "Updating the following trades", 
                              tableOutput(outputId = "t_editedTrades"),
                              footer = list(
                                    actionButton(inputId = "b_edit_cancel", label = "Cancel", width = "48%", class = "btn-danger i mr"),
                                    actionButton(inputId = "b_edit_confirm", label = "Save", width = "48%", class = "btn-success i ml")
                              ), 
                              easyClose = TRUE,
                              size = "l"
                        )
                  )
            }
      })
      
      observeEvent(input$b_edit_cancel, removeModal())
      
      observeEvent(input$b_edit_confirm, {
            withProgress(
                  expr = {
                        write_csv(x = df_edit_bind(), path = "pos.csv")
                        rv$pos <- read_csv(file = "pos.csv")
                  }, 
                  message = "Saving trade edits"
            )
            removeModal()
      })
      
      
      # DELETE TRADES ========================================================================================================================
      
      observeEvent(input$b_delete, {
            showModal(
                  modalDialog(
                        title = "Are you sure you want to delete the following trades?", 
                        tableOutput(outputId = "t_editedTrades"),
                        footer = list(
                              actionButton(inputId = "b_delete_cancel", label = "Cancel", width = "48%", class = "i mr"),
                              actionButton(inputId = "b_delete_confirm", label = "Delete", width = "48%", class = "btn-danger i ml")
                        ), 
                        easyClose = TRUE,
                        size = "l"
                  )
            )
      })
      
      observeEvent(input$b_delete_cancel, removeModal())
      
      observeEvent(input$b_delete_confirm, {
            withProgress(
                  expr = {
                        write_csv(x = rv$pos[-input$t_position_edit_rows_selected, ], path = "pos.csv")
                        rv$pos <- read_csv(file = "pos.csv")
                  }, 
                  message = "Saving trade edits"
            )
            removeModal()
      })
      
      
      
      
      
      
      
}

shinyApp(ui = ui, server = server) # run the app
