setup_domain <- S7::new_generic("setup_domain", "domain")

#' @include domain.R
S7::method(setup_domain, ContinuousDomain) <- function(domain) domain

#' @include domain.R
S7::method(setup_domain, DiscreteDomain) <- function(domain) {
    # if `nobs` is not initialized, it means no `Align` object exist
    # it's not necessary to initialize the `panel` and `index`
    # this is for `stack_layout` which may have no data
    if (is.null(nobs <- domain@nobs)) {
        return(domain)
    }
    if (is.null(domain@panel)) {
        attr(domain, "panel") <- factor(rep_len(1L, nobs))
    }
    if (is.null(domain@index)) {
        attr(domain, "index") <- reorder_index(domain@panel)
    }
    domain
}

############################################################
#' @importFrom S7 S7_dispatch
update_domain <- S7::new_generic(
    "update_domain", "layout",
    function(layout, ..., domain, object_name) {
        S7_dispatch()
    }
)

S7::method(update_domain, QuadLayout) <-
    function(layout, ..., direction, domain, object_name) {
        slot(layout, direction) <- domain
        if (is_horizontal(direction)) {
            if (!is.null(left <- layout@left)) {
                layout@left <- update_domain(left,
                    domain = domain, object_name = object_name
                )
            }
            if (!is.null(right <- layout@right)) {
                layout@right <- update_domain(right,
                    domain = domain, object_name = object_name,
                    from_head = TRUE
                )
            }
        } else {
            if (!is.null(top <- layout@top)) {
                layout@top <- update_domain(top,
                    domain = domain, object_name = object_name
                )
            }
            if (!is.null(bottom <- layout@bottom)) {
                layout@bottom <- update_domain(bottom,
                    domain = domain, object_name = object_name,
                    from_head = TRUE
                )
            }
        }
        layout
    }

S7::method(update_domain, StackLayout) <-
    S7::method(update_domain, CircleLayout) <-
    function(layout, ..., domain, object_name) {
        layout@domain <- domain
        layout@plot_list <- lapply(layout@plot_list, function(plot) {
            if (is_craftbox(plot)) return(plot) # styler: off
            # StackLayout will contain `QuadLayout`
            update_domain(plot,
                direction = layout@direction,
                domain = domain,
                object_name = object_name
            )
        })
        layout
    }

S7::method(update_domain, StackCross) <-
    function(layout, ..., domain, object_name, from_head = FALSE) {
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
                # `nobs`, we check the new panel doesn't break the original index
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
                            object_name, object_name(layout), i
                        ))
                    }
                    attr(new_domain, "index") <- new_index
                }
                attr(new_domain, "nobs") <- new_nobs
                attr(new_domain, "panel") <- new_panel
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
                    update_domain(plot,
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
        layout@domain <- domain_list[[length(domain_list)]]
        layout
    }
