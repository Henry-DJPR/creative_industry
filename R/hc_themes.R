

#' @title Highcharts Theme for Creative Victoria
#'
#' @param hc
#' @param backgroundColor
#' @param textColour
#'
#' @return
#' @export
#'
#' @examples
hc_creative_theme <- function(hc, backgroundColor = NULL, textColour = cv_blue1, ...){

  highcharter::hc_add_theme(
    hc,
    highcharter::hc_theme(
      chart = list(
        backgroundColor = backgroundColor,
        style = list(
          fontFamily = "VIC-Regular",
          `font-size` = "var(--bs-body-font-size)",
          color = textColour
        )
      ),
      title = list(
        align = "left",
        style = list(
          color = textColour,
          fontFamily = "VIC-Regular",
          `font-weight` =  "bold",
          `font-size` = "2rem",
          `background-color` = textColour
        )
      ),
      subtitle = list(
        align = "left",
        style = list(
          color = textColour,
          fontFamily = "VIC-Regular",
          `font-size` = "var(--bs-body-font-size)"
        )
      ),
      legend = list(
        itemStyle = list(
          fontFamily = "VIC-Regular",
          color = textColour
        ),
        itemHoverStyle = list(
          color = "gray"
        )
      ),
      xAxis = list(
        labels = list(
          style = list(
            color = textColour,
            fontFamily = "VIC-Light",
            `font-size` = "11px"
          )
        ),
        title = list(align = 'high',
                     style = list(color = textColour,
                                  `font-size` = "11px")),
        lineWidth = 1,
        lineColor = textColour,
        tickPosition = "outside",
        tickColor = textColour,
        tickWidth = 1,
        tickLength = 4
      ),
      yAxis = list(
        labels = list(
          useHTML = TRUE,
          style = list(
            color = textColour,
            fontFamily = "VIC-Light",
            `font-size` = "11px"
          ),
          y = 5
        ),
        title = list(align = 'high',
                     style = list(color = textColour,
                                  `font-size` = "11px")),
        minorGridLineWidth = 0,
        majorGridLineWidth = 1,
        lineWidth = 0,
        lineColor = "transparent",
        # gridLineColor = "transparent",
        opposite = FALSE
      ),
      caption = list(
        style = list(
          color = textColour
        )
      ),
      series = list(
        line = list(lineWidth = 4),
        spline = list(lineWidth = 4),
        area = list(lineWidth = 4,
                    fillOpacity = 0.4,
                    marker = list(
                      enabled = FALSE,
                      symbol = 'circle',
                      radius = 2,
                      states = list(
                        hover = list(
                          enabled = TRUE)
                      )
                      )
                    )
        )
      )
  )
}





#' @title Y label in Billions
#'
#' @param hc
#'
#' @return
#' @export
#'
#' @examples
hc_y_billions <- function(hc){
  hc_yAxis(hc, labels = list(
    formatter = JS(
      "function(){
          billions = this.value / 1e9
          return('$' + billions + 'B')
        }"
    )
  ))
}




#' @title Creative Victoria Tooltip
#'
#' @param hc
#' @param axis_type
#' @param prefix
#'
#' @return
#' @export
#'
#' @examples
hc_creative_tooltip <- function(hc,
                               axis_type = c('date', 'text', 'numeric'),
                               prefix = ''){

  axis_type <- match.arg(axis_type)

  value <- switch(axis_type,
                  date = "Highcharts.dateFormat('%B %Y', this.point.x)",
                  text = "this.key", # needs to return text label, currently returning axis position
                  numeric = "round(this.point.x)")


  formatter_text <- glue(.open = '{{', .close = '}}',
                         "function () {\n",
                         "  var ret = '',\n",
                         "      multi,\n",
                         "      axis = this.series.yAxis,\n",
                         "      numericSymbols = ['k', 'M', 'B', 'T', 'P', 'E'],\n",
                         "      i = numericSymbols.length;\n",
                         "  while (i-- && ret === '') {\n",
                         "      multi = Math.pow(1000, i + 1);\n",
                         "      if (axis.tickInterval >= multi && numericSymbols[i] !== null) {\n",
                         "          ret = parseFloat(Highcharts.numberFormat(this.y / multi, -1)).toFixed(1) + numericSymbols[i];\n",
                         "  }\n}\n",
                         "  outHTML = '<span style=\"display:inline-block;border-radius:50%;width:8px;height:8px;background-color:' + this.series.color + ';\"></span>&nbsp;<small>' + {{value}} + '</small><br>' + this.point.series.name + ': <b>{{prefix}}' + ret + '</b>'\n",
                         "  return outHTML;\n}\n")

  hc |>
    hc_tooltip(useHTML = TRUE,
               formatter = JS(formatter_text))
}




#' @title Define Chart Export Options
#'
#' @param hc
#' @param buttons
#' @param caption
#'
#' @return
#' @export
#'
#' @examples
hc_creative_export <- function(hc,
                              buttons,
                              caption = "Source: Tourism Research Australia IVS/NVS surveys"){
  hc |>
    hc_exporting(
      #filename = "Food and Fibre Exports",
      enabled = TRUE,
      allowHTML = TRUE,
      #showTable = TRUE,
      accessibility = list(
        enabled = TRUE,
        keyboardNavigation = list(enabled = TRUE)),
      # pdfFont = list(
      #   normal = "fonts/VIC-Regular.ttf"
      # ),
      buttons = list(
        contextButton = list(
          menuItems = buttons
        )),
      csv = list(
        dateFormat = "%B %Y"
      ),
      tableCaption = FALSE,
      chartOptions = list(
        title = list(
          style = list(
            fontFamily = "Arial"
          )
        ),
        subtitle = list(
          style = list(
            fontFamily = "Arial"
          )
        ),
        xAxis = list(labels = list(style = list(fontFamily = "Arial"))),
        yAxis = list(labels = list(style = list(fontFamily = "Arial"))),
        caption = list(
          text = caption
        ),
        chart = list(
          style = list(
            fontFamily = "Arial"
          )
        )
      )
    )
}




#' @title Highcharts Theme with Fill Gradient
#'
#' @param line_colour 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
hc_sparkline_fill <- function(line_colour, ...) {
  
  theme <- list(
    chart = list(
      backgroundColor = NULL,
      margins = c(0, 0, 0, 0),
      spacingTop = 0,
      spacingRight = 0,
      spacingBottom = 0,
      spacingLeft = 0,
      plotBorderWidth = 0,
      borderWidth = 0,
      style = list(overflow = "visible")
    ),
    xAxis = list(
      visible = FALSE,
      endOnTick = FALSE,
      startOnTick = FALSE
    ),
    yAxis = list(
      visible = FALSE,
      endOnTick = FALSE,
      startOnTick = FALSE
    ),
    tooltip = list(
      outside = FALSE,
      shadow = FALSE,
      borderColor = "transparent",
      borderWidth = 0,
      backgroundColor = "transparent",
      style = list(textOutline = "3px #f5ecdf")
    ),
    plotOptions = list(
      series = list(
        marker = list(enabled = FALSE),
        lineWidth = 4,
        shadow = FALSE,
        fillOpacity = 0.25,
        color = line_colour,
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
          stops = list(
            list(0.00, "#FFFFFF00"),
            list(0.50, "#FFFFFF7F"),
            list(1.00, "#FFFFFFFF")
          )
        )
      )
    ),
    credits = list(
      enabled = FALSE,
      text = ""
    )
  )
  
  theme <- structure(theme, class = "hc_theme")
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }
  
  theme
}




