#' @export
ggalign_build.StackLayout <- function(x) {
    x <- default_layout(x)
    stack_build(x) %||% (
        align_plots(theme = x@theme) + layout_title(
            title = .subset2(x@titles, "title"),
            subtitle = .subset2(x@titles, "subtitle"),
            caption = .subset2(x@titles, "caption")
        ))
}

#' @param extra_coords layout parameters of the axis vertically with the stack.
#' @noRd
stack_build <- function(stack, controls = stack@controls, extra_coords = NULL) {
    if (is_empty(stack@plot_list)) {
        return(NULL)
    }
    composer <- stack_build_composer(stack, controls, extra_coords)
    if (is_empty(plots <- .subset2(composer, "plots"))) {
        return(NULL)
    }
    # arrange plots
    titles <- stack@titles
    sizes <- stack@sizes
    direction <- stack@direction

    # recycle the sizes when necessary
    if (length(sizes) == 1L) sizes <- rep(sizes, length.out = 3L)
    sizes <- sizes[
        c(
            .subset2(composer, "left_or_top"),
            TRUE,
            .subset2(composer, "right_or_bottom")
        )
    ]
    align_plots(
        !!!plots,
        design = area(
            .subset2(composer, "t"),
            .subset2(composer, "l"),
            .subset2(composer, "b"),
            .subset2(composer, "r")
        ),
        widths = switch_direction(
            direction,
            do.call(unit.c, .subset2(composer, "sizes")),
            sizes
        ),
        heights = switch_direction(
            direction,
            sizes,
            do.call(unit.c, .subset2(composer, "sizes"))
        ),
        guides = .subset2(.subset2(controls, "plot_align"), "guides"),
        theme = stack@theme
    ) + layout_title(
        title = .subset2(titles, "title"),
        subtitle = .subset2(titles, "subtitle"),
        caption = .subset2(titles, "caption")
    )
}

stack_build_composer <- function(stack, controls, extra_coords) {
    UseMethod("stack_build_composer")
}

#' @export
stack_build_composer.StackLayout <- function(stack, controls, extra_coords) {
    plot_list <- stack@plot_list
    direction <- stack@direction
    position <- .subset2(stack@heatmap, "position")

    # we remove the plot without actual plot area
    keep <- vapply(plot_list, function(plot) {
        # we remove align objects without plot area
        # Now, only `ggalign_align` will contain `NULL`
        !is_align(plot) || !is.null(plot@plot)
    }, logical(1L), USE.NAMES = FALSE)
    plot_list <- .subset(plot_list, keep)
    if (is_empty(plot_list)) return(NULL) # styler: off

    # we reorder the plots based on the `order` slot
    plot_order <- vapply(plot_list, function(plot) {
        if (is_layout(plot)) {
            .subset2(plot@plot_active, "order")
        } else {
            .subset2(plot@active, "order")
        }
    }, integer(1L), USE.NAMES = FALSE)
    plot_list <- .subset(plot_list, make_order(plot_order))

    # build the stack
    composer <- stack_composer(stack@direction)

    # for `free_spaces`, if we have applied it in the whole stack layout
    # we shouln't use it for a single plot. Otherwise, the guide legends
    # collected by the layout will overlap with the axis of the plot in the
    # layout.
    #
    # this occurs in the annotation stack (`position` is not `NULL`).
    stack_spaces <- .subset2(.subset2(controls, "plot_align"), "free_spaces")
    if (is_string(stack_spaces) && !is.null(position)) {
        released_spaces <- stack_spaces
    } else {
        released_spaces <- NULL
    }
    coords <- setup_layout_coords(stack@layout)
    stack_composer_add(
        plot_list,
        composer,
        controls = controls,
        coords = coords,
        extra_coords = extra_coords,
        direction = direction,
        position = position,
        released_spaces = released_spaces
    )
}

make_order <- function(order) {
    l <- length(order)
    index <- seq_len(l)

    # for order not set by user, we use heuristic algorithm to define the order
    need_action <- is.na(order)
    if (all(need_action)) { # shorthand for the usual way, we don't set any
        return(index)
    } else if (all(!need_action)) { # we won't need do something special
        return(order(order))
    }

    # 1. for outliers, we always put them in the two tail
    # 2. for order has been set and is not the outliers,
    #    we always follow the order
    # 3. non-outliers were always regarded as the integer index
    used <- as.integer(order[!need_action & order >= 1L & order <= l])

    # we flatten user index to continuous integer sequence
    sequence <- vec_unrep(used) # key is the sequence start
    start <- .subset2(sequence, "key")
    end <- pmin(
        start + .subset2(sequence, "times") - 1L,
        vec_c(start[-1L] - 1L, l) # the next start - 1L
    )
    used <- .mapply(function(s, e) s:e, list(s = start, e = end), NULL)

    # following index can be used
    unused <- vec_set_difference(index, unlist(used, FALSE, FALSE))

    # we assign the candidate index to the order user not set.
    order[need_action] <- unused[seq_len(sum(need_action))]

    # make_order(c(NA, 1, NA)): c(2, 1, 3)
    # make_order(c(NA, 1, 3)): c(2, 1, 3)
    # make_order(c(NA, 1, 3, 1)): c(2, 4, 3, 1)
    order(order)
}
