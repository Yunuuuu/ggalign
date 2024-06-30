#' @importFrom ggplot2 aes
#' @export
ggheat <- function(.data, .mapping = aes(), ...) {
    .data <- build_matrix(.data)
    structure(list(
        matrix = .data,
        params = rlang::list2(...),
        heatmap = ggplot2::ggplot(mapping = .mapping),
        top = NULL, left = NULL, bottom = NULL, right = NULL
    ), active = NULL, class = c("ggheatmap", "ggheat"))
}

#' Add components to ggheat
#'
#' @name ggheat-add
#' @export
`+.ggheatmap` <- function(e1, e2) {
    if (missing(e2)) {
        cli::cli_abort(c(
            "Cannot use {.code +} with a single argument.",
            "i" = "Did you accidentally put {.code +} on a new line?"
        ))
    }
    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    ggheatmap_add(e2, e1, e2name)
}

ggheatmap_add <- function(object, plot, object_name) UseMethod("ggheatmap_add")

#' @export
ggheatmap_add.default <- function(object, plot, object_name) {
    cli::cli_abort(
        "Can't add {.var {object_name}} to a {.cls ggheatmap} object."
    )
}

#' @export
ggheatmap_add.NULL <- function(object, plot, object_name) {
    plot
}

#' @export
ggheatmap_add.gg <- function(object, plot, object_name) {
    # add elements to the active context
    if (is.null(pos <- active(plot))) {
        plot$heatmap <- ggplot2::ggplot_add(
            object, .subset2(plot, "heatmap"), object_name
        )
    } else {
        if (!is.null(annos <- .subset2(plot, pos))) {
            cli::cli_abort(c(
                "Cannot add {object_name} to {pos} annotation",
                i = "Did you forget to initialize it with {.fn anno_{pos}}?"
            ))
        }
        annos[[active(annos)]] <- ggplot2::ggplot_add(
            object, .subset2(annos, active(annos)), object_name
        )
        plot[[pos]] <- annos
    }
    plot
}
