#' Create a sidebar navigation panel
#'
#' @param title Text in the panel
#' @param ref reference to the id that the panel navs to and the class used to identify and indicate an active panel
#' @param active Logical - should this panel launch as active?
#'
#' @return Returns a <li> list item for a sidebar navigation panel
sidebarNavPanel <- function(ref, active = FALSE) {
  title <- paste0(str_sub(ref, 12, -5), ' [', str_sub(ref, 1, 10), ']')
  value <- restoreInput(id = ref, default = NULL)
  tags$li(
    id = paste0('li-', ref),
    tags$a(
      id = ref,
      href = '#',
      class = 'action-button',
      `data-val` = value,
      title
    )
  )
}

#' Creates a tooltip element
#' 
#' @param direction the direction that the tooltip should appear in. Accepts "right", "left", "top", "bottom"
#' @param content the text content of the tooltip
#'
#' @return Returns an icon element that will provide a tooltip containing
#'  `content` on mouse hover. Default icon is the fa info circle and default
#'  placement is to the right of the icon.
#'
makeTooltip <- function(content, icon_name = "info-circle", direction = "right", class = NULL, lib = "font-awesome", ...) {
  icon(
    name = icon_name,
    class = class,
    lil = lib,
    `data-toggle` = "tooltip",
    `data-placement` = direction,
    title = content,
    `data-html` = "true"
  )
}