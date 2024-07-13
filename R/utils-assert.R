check_gp <- function(gp, arg = rlang::caller_arg(gp),
                     call = rlang::caller_env()) {
    assert_s3_class(gp, "gpar", arg = arg, call = call)
}
