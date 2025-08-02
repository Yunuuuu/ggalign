#' @export
`ggalign_build.ggalign::StackLayout` <- function(x) {
    x <- default_layout(x)
    (stack_build(x) %||% align_plots(theme = x@theme)) +
        layout_title(
            title = .subset2(x@titles, "title"),
            subtitle = .subset2(x@titles, "subtitle"),
            caption = .subset2(x@titles, "caption")
        )
}

#' @param schemes,theme Parameters from parent layout
#' @param extra_domain layout parameters of the axis vertically with the stack.
#' @noRd
stack_build <- function(stack, schemes = NULL, theme = NULL,
                        extra_domain = NULL) {
    if (is_empty(stack@plot_list)) {
        return(NULL)
    }
    direction <- stack@direction
    position <- .subset2(stack@heatmap, "position")
    schemes <- inherit_parent_layout_schemes(stack, schemes)

    if (is_horizontal(direction)) {
        spacing <- "y"
    } else {
        spacing <- "x"
    }
    theme <- inherit_parent_layout_theme(stack, theme, spacing = spacing)
    composer <- resolve_stack_layout(stack, schemes, theme, extra_domain)
    if (is_empty(plots <- .subset2(composer, "plots"))) {
        return(NULL)
    }

    # arrange plots
    if (is.null(position)) { # for stack layout
        # sizes should be of length 3
        sizes <- convert(stack@sizes, S3_unit)
        # recycle the sizes when necessary
        if (length(sizes) == 1L) sizes <- rep(sizes, length.out = 3L)
        sizes <- sizes[
            c(
                .subset2(composer, "left_or_top"),
                TRUE,
                .subset2(composer, "right_or_bottom")
            )
        ]
    } else { # for the heatmap annotation
        sizes <- NA
    }
    plot <- align_plots(
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
        guides = prop(schemes_get(schemes, "scheme_align"), "guides"),
        theme = stack@theme
    )

    # for annotation, we should always make it next to the main body
    if (is.null(position)) {
        return(plot)
    }
    plot <- free_vp(
        plot,
        x = switch(position,
            left = 1L,
            right = 0L,
            0.5
        ),
        y = switch(position,
            top = 0L,
            bottom = 1L,
            0.5
        ),
        just = switch(position,
            top = "bottom",
            left = "right",
            bottom = "top",
            right = "left"
        )
    )

    # whether we should override the `guides` collection for the whole
    # annotation stack
    free_guides <- .subset2(stack@heatmap, "free_guides")
    if (!is.waive(free_guides)) plot <- free_guide(plot, free_guides)
    # we also apply the `free_spaces` for the whole annotation stack
    free_spaces <- prop(
        schemes_get(schemes, "scheme_align"), "free_spaces"
    ) %|w|% NULL
    if (!is.null(free_spaces)) {
        plot <- free_space(free_border(plot, free_spaces), free_spaces)
    }
    plot
}

#' @param schemes,theme Parameters for current stack, which have inherited
#' parameters from the parent.
#' @noRd
resolve_stack_layout <- function(stack, schemes, theme, extra_domain) {
    UseMethod("resolve_stack_layout")
}

#' @export
`resolve_stack_layout.ggalign::StackLayout` <- function(stack, schemes, theme,
                                                        extra_domain) {
    plot_list <- stack@plot_list
    direction <- stack@direction
    position <- .subset2(stack@heatmap, "position")

    # we remove the plot without actual plot area
    keep <- vapply(plot_list, function(plot) {
        # we remove objects without plot area
        # Now, only `CraftBox` will contain `NULL`
        !is_craftbox(plot) || !is.null(plot@plot)
    }, logical(1L), USE.NAMES = FALSE)
    plot_list <- .subset(plot_list, keep)
    if (is_empty(plot_list)) return(NULL) # styler: off

    # we reorder the plots based on the `order` slot
    plot_order <- vapply(plot_list, function(plot) {
        if (is_layout(plot)) {
            prop(plot@plot_active, "order")
        } else {
            prop(plot@active, "order")
        }
    }, integer(1L), USE.NAMES = FALSE)
    plot_list <- .subset(plot_list, make_order(plot_order))

    # build the stack
    composer <- stack_composer(direction)

    # for `free_spaces`, if we have applied it in the whole stack layout
    # we shouln't use it for a single plot. Otherwise, the guide legends
    # collected by the layout will overlap with the axis of the plot in the
    # layout.
    #
    # this occurs in the annotation stack (`position` is not `NULL`).
    #
    # here is the example:
    # p1 <- ggplot(mtcars) +
    #     geom_point(aes(mpg, disp))
    # p2 <- ggplot(mtcars) +
    #     geom_boxplot(aes(gear, disp, group = gear, fill = gear))
    # p3 <- ggplot(mtcars) +
    #     geom_bar(aes(gear)) +
    #     facet_wrap(~cyl)
    # align_plots(
    #     free_space(free_border(
    #         align_plots(
    #             # we shouldn't add free_space for the internal plot
    #             free_space(
    #                 free_border(
    #                     p1 + scale_y_continuous(
    #                         expand = expansion(),
    #                         labels = ~ paste("very very long labels", .x)
    #                     ),
    #                     "l"
    #                 ),
    #                 "l"
    #             ),
    #             p2 + theme(legend.position = "left"),
    #             guides = "l"
    #         ),
    #         "l"
    #     ), "l"),
    #     p3 + theme(plot.margin = margin(l = 5, unit = "cm")),
    #     ncol = 1
    # )
    stack_spaces <- prop(
        schemes_get(schemes, "scheme_align"), "free_spaces"
    )
    if (is_string(stack_spaces) && !is.null(position)) {
        released_spaces <- stack_spaces
    } else {
        released_spaces <- NULL
    }

    domain <- domain_init(stack@domain)
    stack_composer_add(
        plot_list,
        stack = stack,
        composer,
        schemes = schemes,
        theme = theme,
        domain = domain,
        extra_domain = extra_domain,
        direction = direction,
        position = position,
        released_spaces = released_spaces,
        previous_domain = NULL
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
