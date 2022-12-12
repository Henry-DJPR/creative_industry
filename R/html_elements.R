


#' @title Large Number Banner
#'
#' @param text 
#' @param description 
#' @param sparkline 
#' @param colour 
#' @param colour_text 
#' @param text_length 
#'
#' @return
#' @export
#'
#' @examples
number_banner <- function(id, 
                          text, 
                          text_length,
                          caption, 
                          sparkline = NULL, 
                          colour = 'white', 
                          colour_text = 'black'){
  
  #id = encode(as.integer(Sys.time()), hashid_settings(salt = 'creative'))
  
  style_background <- glue::glue('background: {colour};',
                                 'padding:1em;',
                                 'border-radius: 20px;',
                                 'color:{colour_text};')
  
  style_banner <- glue::glue("font-size:{100/text_length}vmin;", # TODO: screens of diff res have slightly different results
                             "font-family:VIC-Bold;",
                             "text-align:center;",
                             "white-space: nowrap;",
                             "width:100%;",
                             "height:{100/text_length + 3}vmin;")
  
  style_caption <- paste0('font-size:2vmin;',
                          "font-family:VIC-Regular;",
                          "text-align:left;",
                          'min-height:30px;',
                          'padding-left:10%;',
                          'padding-right:10%;')
  
  if (!is.null(sparkline)) {
    style_sparkline <- paste0('min-height:30px;')
  }
  
  tags$figure(#style = 'position:relative;',
    tags$figcaption(hidden = 'hidden',
                    tagList(
                      tags$strong(id=glue::glue("{id}-caption"),
                                  id),
                      tags$br(),
                      tags$span(id=glue::glue("{id}-summary"),
                                caption)
                    )
                   ),
    div(id = id,
        `aria-labelledby` = glue::glue("{id}-caption"),
        `aria-describedby` = glue::glue("{id}-summary"),
        class="container",
        style = style_background,
        div(class = "row",
            style = style_banner,
            div(class = 'col-12',
                text)
        ),
        if (!is.null(sparkline)) div(class = 'row', style = style_sparkline, sparkline),
        div(class = "row",
            style = style_caption,
            caption
        )
    )
  )
  
}


#' @title Build a Formatted Number 
#'
#' @param value 
#' @param transform 
#' @param highlight_colour 
#' @param highlight_size 
#' @param post 
#' @param unit 
#'
#' @return
#' @export
#'
#' @examples
number_build <- function(value, 
                         unit = NULL, 
                         transform = c('none','thousand','million','billion'), 
                         highlight_colour = 'red', 
                         highlight_size = 60,
                         post = NULL
                         ){
  
  transform <- match.arg(transform)
  
  unit_style <- glue::glue('font-size:{highlight_size}%;',
                           'color:{highlight_colour};')
  post_style <- glue::glue('font-size:{highlight_size / 2}%;',
                           'color:{highlight_colour};',
                           'text-align:right;',
                           'position:relative;',
                           'top:-80%;',
                           'left:0;')
  
  
  if (transform == 'none') {
    banner_value <- format(value, big.mark   = ",")
  } else if (transform == 'thousand') {
    banner_value <- (value / 1e3) |>
      round(2) 
  } else if (transform == 'million') {
    banner_value <- (value / 1e6) |>
      round(2) 
  } else if (transform == 'billion') {
    banner_value <- (value / 1e9) |>
      round(2) 
  }
  
  if (is.null(post) & transform != 'none') post <- transform
    
  withTags(
    span(
      if (!is.null(unit)) span(style = unit_style, unit),
      banner_value, 
      if (!is.null(post)) div(style = post_style, post)
      ), 
    .noWS = c("outside", "inside")
  )
  
  
}





#' @title Number of Digits in Number
#'
#' @param num 
#'
#' @return
#' @export
#'
#' @examples
nnum <- function(num){
  nchar(as.character(num))
}

