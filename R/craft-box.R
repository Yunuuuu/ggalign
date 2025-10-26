#' @importFrom S7 new_object S7_object S7_inherits
#' @importFrom ggplot2 ggproto is_ggplot
#' @importFrom rlang list2 is_named2
#' @include graph.R
#' @include utils-grid.R
#' @include utils-assert.R
CraftBox <- S7::new_class("CraftBox",
    properties = list(
        designer = S7::new_S3_class("ggalign::CraftDesigner"),
        # params = S7::new_property(
        #     S7::class_list,
        #     validator = function(value) {
        #         if (!is_named2(value)) {
        #             return("must be named")
        #         }
        #     }
        # ),
        graph = S7::new_property(
            S7::new_union(NULL, Graph),
            validator = function(value) {
                if (S7_inherits(value, Graph) &&
                    length(prop(prop(value, "control"), "size")) != 1L) {
                    return("'size' in 'plot_control()' must be of length 1 for CraftBox")
                }
            },
            setter = function(self, value) {
                if (is.null(value)) {
                    prop(self, "graph", check = FALSE) <- value
                } else if (S7_inherits(value, Graph)) {
                    # we need check control size, don't set `check = FALSE`
                    prop(self, "graph") <- value
                } else {
                    if (S7_inherits(graph <- prop(self, "graph"), Graph)) {
                        prop(graph, "plot", check = FALSE) <- value
                        prop(self, "graph", check = FALSE) <- graph
                    } else {
                        prop(self, "graph", check = FALSE) <- Graph(plot)
                    }
                }
                self
            }
        ),
        init_hooks = S7::new_property(
            S7::class_list,
            validator = function(value) {
                for (fn in value) {
                    if (!is.function(fn)) {
                        return("must be a list of functions")
                    }
                    if (length(formals(fn)) < 2L) {
                        return("each function must accept at least two arguments: 'plot' and 'orientation'")
                    }
                }
            }
        ),
        name = S7::new_property(
            S7::class_character,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single character string")
                }
            },
            setter = function(self, value) {
                if (identical(value, NA)) value <- NA_character_
                prop(self, "name") <- value
                self
            },
            default = NA_character_
        )
    ),
    constructor = function(designer = NULL, ..., graph = NULL, name = NULL,
                           call = caller_call()) {
        new_object(
            S7_object(),
            designer = ggproto(
                NULL, designer %||% CraftDesigner,
                ...,
                call = call
            ),
            # params = list2(...),
            graph = graph,
            init_hooks = list(),
            name = name %||% NA_character_
        )
    }
)

#' @importFrom utils str
local(S7::method(str, CraftBox) <- function(object, ...) {
    cat(prop(object, "designer")$summary(prop(object, "plot")), sep = "\n")
    invisible(object)
})

#' @importFrom grid grid.draw
local(S7::method(plot, CraftBox) <- S7::method(grid.draw, CraftBox) <-
    function(x, ...) {
        cli_abort("Direct plotting of {.obj_type_friendly {x}} is not supported")
    })

local(S7::method(`+`, list(CraftBox, S7::class_missing)) <-
    function(e1, e2) {
        cli_abort(c(
            "The {.code +} operator cannot be used with a single argument.",
            "i" = "Did you accidentally place {.code +} on a new line without a second argument?"
        ))
    })

local(S7::method(`+`, list(CraftBox, S7::class_any)) <- function(e1, e2) {
    e2name <- deparse(substitute(e2, env = caller_env(2L)))
    ggalign_update(e1, e2, e2name)
})

#' @importFrom ggplot2 update_ggplot
S7::method(ggalign_update, list(CraftBox, S7::class_any)) <-
    function(x, object, objectname, ...) {
        if (is.null(object)) return(x) # styler: off
        if (is.null(prop(x, "graph"))) {
            cli_abort(c(
                sprintf("Cannot add {.var {objectname}} to %s", object_name(x)),
                i = sprintf("no plot found for %s", object_name(x))
            ))
        }
        # we need check the plot size, don't set `check = FALSE`
        prop(x, "graph") <- ggalign_update(
            prop(x, "graph"), object, objectname, ...
        )
        x
    }

#' @importFrom S7 prop prop<-
S7::method(ggalign_update, list(CraftBox, S7::class_function)) <-
    function(x, object, objectname, ...) {
        if (is.null(prop(x, "graph"))) {
            cli_abort(c(
                sprintf("Cannot add {.var {objectname}} to %s", object_name(x)),
                i = sprintf("no plot found for %s", object_name(x))
            ))
        }
        if (length(formals(object)) < 2L) {
            cli_abort(c(
                sprintf(
                    "Cannot add {.var {objectname}} to the %s",
                    obj_type_friendly(x)
                ),
                i = "function must accept at least two arguments"
            ))
        }
        prop(x, "init_hooks", check = FALSE) <- c(prop(x, "init_hooks"), object)
        x
    }

###############################################################
#' @importFrom S7 S7_inherits
is_craftbox <- function(x) S7_inherits(x, CraftBox)

#' @importFrom S7 prop
is_cross_craftbox <- function(x) {
    is_craftbox(x) && is_cross_craftsman(prop(x, "craftsman"))
}

#' @importFrom S7 prop
is_align_craftbox <- function(x) {
    is_craftbox(x) && is_align_craftsman(prop(x, "craftsman"))
}

is_cross_craftsman <- function(x) inherits(x, "CraftCross")

is_align_craftsman <- function(x) inherits(x, "CraftAlign")
