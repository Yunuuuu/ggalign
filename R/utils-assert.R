assert_gp <- function(gp, arg = rlang::caller_arg(gp),
                      call = rlang::caller_env()) {
    assert_s3_class(gp, "gpar", arg = arg, call = call)
}

assert_mapping <- function(mapping, arg = rlang::caller_arg(mapping),
                           call = rlang::caller_call()) {
    if (!inherits(mapping, "uneval")) {
        cli::cli_abort(c("{.arg {arg}} must be created with {.fn aes}.",
            x = "You've supplied {.obj_type_friendly {mapping}}."
        ), call = call)
    }
}
