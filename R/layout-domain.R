#' @importFrom S7 S7_dispatch
layout_update_domain <- S7::new_generic(
    "layout_update_domain", "layout",
    function(layout, ..., domain, objectname) {
        S7_dispatch()
    }
)

#' @include layout-.R
S7::method(layout_update_domain, StackLayout) <-
    S7::method(layout_update_domain, CircleLayout) <-
    function(layout, ..., domain, objectname) {
        layout@domain <- domain
        layout@plot_list <- lapply(layout@plot_list, function(plot) {
            if (is_craftbox(plot)) return(plot) # styler: off
            # StackLayout will contain `QuadLayout`
            layout_update_domain(plot,
                direction = layout@direction,
                domain = domain,
                objectname = objectname
            )
        })
        layout
    }

#' @include layout-.R
S7::method(layout_update_domain, QuadLayout) <-
    function(layout, ..., direction, domain, objectname) {
        prop(layout, direction) <- domain
        if (is_horizontal(direction)) {
            if (!is.null(left <- layout@left)) {
                layout@left <- layout_update_domain(left,
                    domain = domain, objectname = objectname
                )
            }
            if (!is.null(right <- layout@right)) {
                layout@right <- layout_update_domain(right,
                    domain = domain, objectname = objectname,
                    from_head = TRUE
                )
            }
        } else {
            if (!is.null(top <- layout@top)) {
                layout@top <- layout_update_domain(top,
                    domain = domain, objectname = objectname
                )
            }
            if (!is.null(bottom <- layout@bottom)) {
                layout@bottom <- layout_update_domain(bottom,
                    domain = domain, objectname = objectname,
                    from_head = TRUE
                )
            }
        }
        layout
    }

#' @include layout-.R
S7::method(layout_update_domain, StackCross) <-
    function(layout, ..., domain, objectname, from_head = FALSE) {
        # `domain` must be a discrete_domain()
        domain_list <- c(layout@odomain, list(layout@domain))

        # for cross_points, the updating will span it, but only update the panel
        # information
        cross_points <- layout@cross_points

        # the break_points set breaks, updating won't span the break points
        break_points <- layout@break_points

        plot_list <- layout@plot_list
        n <- length(plot_list)
        points <- c(cross_points, n)
        point_index <- seq_along(points)
        if (!from_head) point_index <- rev(point_index)
        for (i in point_index) {
            cross_point <- .subset(points, i)

            # we first update the domain in the updated tail
            # it means the first domain when `from_head` is `TRUE`
            # the last domain when `from_head` is `FALSE`
            if ((from_head && i == 1L) || (!from_head && cross_point == n)) {
                new_domain <- domain
            } else if (!from_head && any(cross_point == break_points)) {
                break
            } else {
                # for domain not in updated tail, we'll only update `panel` and
                # `nobs`, we check the new panel doesn't break the original
                # index
                new_nobs <- domain@nobs
                new_panel <- domain@panel
                new_domain <- .subset2(domain_list, i)
                # we check the new panel don't disrupt the ordering index
                if (!is.null(new_panel) &&
                    !is.null(old_index <- new_domain@index)) {
                    # we always prevent from reordering twice.
                    new_index <- reorder_index(new_panel, old_index)
                    if (!all(old_index == new_index)) {
                        cli_abort(sprintf(
                            "%s disrupt the previously established ordering index of %s (%d)",
                            objectname, object_name(layout), i
                        ))
                    }
                    prop(new_domain, "index", check = FALSE) <- new_index
                }
                prop(new_domain, "nobs", check = FALSE) <- new_nobs
                prop(new_domain, "panel", check = FALSE) <- new_panel
            }
            domain_list[i] <- list(new_domain)

            # we then update the domain for each plot
            if (i == 1L) {
                subset <- seq_len(cross_point)
            } else {
                subset <- (.subset(points, i - 1L) + 1L):cross_point
            }

            layout@plot_list[subset] <- lapply(
                plot_list[subset], function(plot) {
                    if (is_craftbox(plot)) {
                        return(plot)
                    }
                    layout_update_domain(plot,
                        direction = layout@direction,
                        domain = new_domain
                    )
                }
            )
            if (from_head && any(cross_point == break_points)) break
        }
        layout@odomain <- vec_slice(
            domain_list,
            seq_len(length(domain_list) - 1L)
        )
        layout@domain <- .subset2(domain_list, length(domain_list))
        layout
    }

#' Layout can align ordinal variable or continuous variable
#'
#' @param x A `LayoutProto` object.
#' @noRd
is_layout_discrete <- S7::new_generic("is_layout_discrete", "x")

is_layout_continuous <- S7::new_generic("is_layout_continuous", "x")

#' @include layout-.R
S7::method(is_layout_discrete, ChainLayout) <- function(x, ...) {
    is_discrete_domain(x@domain)
}

#' @include layout-.R
S7::method(is_layout_continuous, ChainLayout) <- function(x, ...) {
    # `NULL` is a un-defined `ContinuousDomain`
    is.null(x@domain) || is_continuous_domain(x@domain)
}

#' @include layout-.R
S7::method(is_layout_discrete, QuadLayout) <- function(x, direction, ...) {
    is_discrete_domain(prop(x, direction))
}

#' @include layout-.R
S7::method(is_layout_continuous, QuadLayout) <- function(x, direction, ...) {
    # `NULL` is a un-defined `ContinuousDomain`
    is.null(prop(x, direction)) || is_continuous_domain(prop(x, direction))
}
