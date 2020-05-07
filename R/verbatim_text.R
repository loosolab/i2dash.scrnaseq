#' @rdname text_output
#' @return A string containing markdown code for the rendered component
setMethod("text_output",
          signature = signature(dashboard = "i2dashboard", object = "missing"),
          function(dashboard, transmitter = NULL, event = "plotly_selected", title = NULL) {
            # Create random env id
            env_id <- paste0("env_", stringi::stri_rand_strings(1, 6, pattern = "[A-Za-z0-9]"))

            # Validate input
            assertive.types::assert_is_character(transmitter)
            assertive.types::assert_is_character(event)

            transmitter %>% gsub(x = ., pattern = " ", replacement = "_") %>% make.names -> transmitter

            # Create component environment
            env <- new.env()

            env$event <- event
            env$transmitter <- transmitter

            saveRDS(env, file = file.path(dashboard@datadir, paste0(env_id, ".rds")))

            # Expand component
            timestamp <- Sys.time()
            expanded_component <- knitr::knit_expand(file = system.file("templates", "text_output.Rmd", package = "i2dash.scrnaseq"), title = title, env_id = env_id, date = timestamp)
            return(expanded_component)
          })
