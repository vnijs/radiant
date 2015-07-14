                               # onclick = "var path = location.pathname.replace(/\\/.+/, '');
                               # history.replaceState(null, null, path);"))
                               # setTimeout(100000);
#                                onclick = "window.location.reload();
#                                var path = location.pathname.replace(/\\/.+/, '');
#                                history.replaceState(null, null, path);"))
#                tabPanel(tags$a(id = "new_session", href = "", target = "_blank",
#                                list(icon("plus"), "New session"),
#                                onclick = "window.location.reload();
#                                var path = location.pathname.replace(/\\/.+/, '');
#                                history.replaceState(null, null, path);"))
               # window.location.reload();"))


# output$new_session <- renderUI({
#   tabPanel(tags$a(id = "new_session", href = url_current(), target = "_blank",
#                 class = "action-button", list(icon("plus"), "New session"),
#                 onclick = "var path = location.pathname.replace(/\\/.+/, '');
#                 history.replaceState(null, null, path);
#                 window.location.reload();"))
# })

