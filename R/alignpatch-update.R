#' @importFrom rlang caller_env
#' @importFrom ggplot2 update_ggplot
local(
    S7::method(`+`, list(alignpatches, S7::class_any)) <-
        function(e1, e2) {
            # Get the name of what was passed in as e2, and pass along so that
            # it can be displayed in error messages
            if (missing(e2)) {
                cli_abort(c(
                    "Cannot use {.code +} with a single argument.",
                    "i" = "Did you accidentally put {.code +} on a new line?"
                ))
            }
            e2name <- deparse(substitute(e2, env = caller_env(2L)))
            update_ggplot(e2, e1, e2name)
        }
)

#' @importFrom ggplot2 update_ggplot
S7::method(update_ggplot, list(S7::class_any, alignpatches)) <-
    function(object, plot, objectname) {
        if (is.null(object)) return(plot) # styler: off
        cli_abort(c(
            sprintf("Cannot add %s to {.code alignpatches}.", objectname),
            "x" = "Only layout elements or other compatible objects can be added."
        ))
    }

#' @importFrom S7 prop<- prop
#' @importFrom ggplot2 update_ggplot
S7::method(update_ggplot, list(ggplot2::class_ggplot, alignpatches)) <-
    S7::method(
        update_ggplot,
        list(S7::new_S3_class("formula"), alignpatches)
    ) <-
    S7::method(
        update_ggplot,
        list(S7::new_S3_class("function"), alignpatches)
    ) <-
    S7::method(
        update_ggplot,
        list(S7::new_S3_class("recordedplot"), alignpatches)
    ) <-
    S7::method(
        update_ggplot,
        list(S7::new_S3_class("trellis"), alignpatches)
    ) <-
    S7::method(
        update_ggplot,
        list(S7::new_S3_class("Heatmap"), alignpatches)
    ) <-
    S7::method(
        update_ggplot,
        list(S7::new_S3_class("HeatmapList"), alignpatches)
    ) <-
    S7::method(
        update_ggplot,
        list(S7::new_S3_class("HeatmapAnnotation"), alignpatches)
    ) <-
    S7::method(
        update_ggplot,
        list(S7::new_S3_class("pheatmap"), alignpatches)
    ) <-
    function(object, plot, objectname) {
        prop(plot, "plots") <- c(prop(plot, "plots"), list(object))
        plot
    }

#' @importFrom S7 prop<- prop
#' @importFrom ggplot2 update_ggplot
S7::method(update_ggplot, list(layout_design, alignpatches)) <-
    function(object, plot, objectname) {
        prop(plot, "layout", check = FALSE) <- prop(plot, "layout") + object
        plot
    }

# plot_layout is from `patchwork` package
#' @importFrom ggplot2 update_ggplot is_waiver
#' @importFrom rlang inject
S7::method(
    update_ggplot,
    list(S7::new_S3_class("plot_layout"), alignpatches)
) <-
    function(object, plot, objectname) {
        object["area"] <- list(object$design) # pathwork use `design`
        object <- .subset(object, names(layout_design@properties))
        if (is_waiver(object$guides)) {
            object$guides <- NA
        } else if (identical(object$guides, "auto")) {
            object$guides <- waiver()
        } else if (identical(object$guides, "collect")) {
            object$guides <- "tlbr"
        } else if (identical(object$guides, "keep")) {
            object["guides"] <- list(NULL)
        }
        update_ggplot(inject(layout_design(!!!object)), plot, objectname)
    }

##############################################################
# Bypass S7 setter validation: update internal property via attr() directly
#' @importFrom S7 prop<- prop
#' @importFrom ggplot2 update_ggplot
S7::method(update_ggplot, list(layout_title, alignpatches)) <-
    function(object, plot, objectname) {
        prop(plot, "titles") <- prop(plot, "titles") + object
        plot
    }

##############################################################
#' @importFrom S7 prop<- prop
#' @importFrom ggplot2 update_ggplot
S7::method(update_ggplot, list(S3_layout_theme, alignpatches)) <-
    function(object, plot, objectname) {
        prop(plot, "theme") <- layout_theme_update(prop(plot, "theme"), object)
        plot
    }

#' @importFrom S7 prop<- prop
#' @importFrom ggplot2 update_ggplot
#' @importFrom rlang inject
S7::method(
    update_ggplot,
    list(S7::new_S3_class("plot_annotation"), alignpatches)
) <-
    function(object, plot, objectname) {
        titles <- .subset(object, names(layout_title@properties))
        titles <- inject(layout_title(!!!titles))
        prop(plot, "titles") <- prop(plot, "titles") + titles
        prop(plot, "theme") <- layout_theme_update(
            prop(plot, "theme"), .subset2(object, "theme")
        )
        # Transform patchwork tag into ggalign style
        tags <- .subset2(object, "tag_levels") %|w|% NA
        if (length(tags) == 0L) tags <- NA
        if (is.list(tags)) tags <- .subset2(tags, length(tags))
        prop(plot, "tags") <- prop(plot, "tags") + layout_tags(
            tags = tags,
            sep = .subset2(object, "tag_sep"),
            prefix = .subset2(object, "tag_prefix"),
            suffix = .subset2(object, "tag_suffix")
        )
        plot
    }

##############################################################
# Bypass S7 setter validation: update internal property via `attr()` directly
#' @importFrom S7 prop<- prop
#' @importFrom ggplot2 update_ggplot
S7::method(update_ggplot, list(layout_tags, alignpatches)) <-
    function(object, plot, objectname) {
        prop(plot, "tags") <- prop(plot, "tags") + object
        plot
    }

##############################################################
#' @importFrom rlang caller_env
local(S7::method(`&`, list(alignpatches, S7::class_any)) <-
    function(e1, e2) {
        if (missing(e2)) {
            cli_abort(c(
                "Cannot use {.code &} with only one argument.",
                "i" = "Did you accidentally put {.code &} at the end of a line?"
            ))
        }
        if (is.null(e2)) return(e1) # styler: off

        # Get the name of what was passed in as e2, and pass along so that it
        # can be displayed in error messages
        e2name <- deparse(substitute(e2, env = caller_env(2L)))
        if (is_theme(e2)) {
            prop(e1, "theme") <- ggfun("add_theme")(
                prop(e1, "theme"), e2, e2name
            )
        }
        alignpatches_propagate(e2, e1, e2name)
    })

#' @importFrom S7 S7_dispatch
alignpatches_propagate <- S7::new_generic(
    "alignpatches_propagate", "object",
    fun = function(object, patches, objectname) S7_dispatch()
)

#' @importFrom ggplot2 is_ggplot update_ggplot
#' @importFrom S7 prop S7_inherits
#' @importFrom rlang try_fetch
S7::method(alignpatches_propagate, S7::class_any) <-
    function(object, patches, objectname) {
        plots <- prop(patches, "plots")
        for (i in seq_along(plots)) {
            plot <- .subset2(plots, i)
            if (is_ggplot(plot)) {
                plots[[i]] <- update_ggplot(object, plot, objectname)
            } else if (S7_inherits(plot, alignpatches)) {
                plots[[i]] <- alignpatches_propagate(object, plot, objectname)
            } else if (S7_inherits(plot, LayoutProto)) {
                plots[[i]] <- layout_propagate(plot, object, objectname)
            } else {
                # For other object types, attempt to combine them with `object`
                # using the `&` operator. This re-dispatches the `&` method so
                # that any custom patch combination logic defined for the
                # object's class can be applied. If the operation fails,
                # silently ignore the error.
                try_fetch(
                    plots[[i]] <- .subset2(plots, i) & object,
                    error = function(cnd) NULL
                )
            }
        }
        prop(patches, "plots", check = FALSE) <- plots
        patches
    }

S7::method(alignpatches_propagate, ggplot2::class_ggplot) <-
    S7::method(alignpatches_propagate, layout_title) <-
    S7::method(alignpatches_propagate, S3_layout_theme) <-
    S7::method(alignpatches_propagate, layout_tags) <-
    S7::method(alignpatches_propagate, layout_design) <-
    function(object, patches, objectname) {
        cli_abort(c(
            sprintf(
                "Cannot use {.code &} to combine {.cls alignpatches} with {.code %s}.",
                objectname
            ),
            "i" = "If your intention was to add or update elements, use {.code +} instead."
        ))
    }
