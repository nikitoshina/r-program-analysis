library(lobstr)
library(dplyr)
library(tibble)
library(purrr)
library(rlang)
library(readr)
library(stringr)


if (interactive()) {
    file_address <- "./tests/source_file.r"
    file_contents <- read_file(file_address)
} else {
    f <- file("stdin", blocking = FALSE)
    open(f)
    file_contents <- paste(readLines(f), collapse = "\n")
    close(f)
}

file_expressions <- parse_exprs(file_contents)

# Convenience Function to get type of the expression
expr_type <- function(x) {
    if (rlang::is_syntactic_literal(x)) {
        "constant"
    } else if (is.symbol(x)) {
        "symbol"
    } else if (is.call(x)) {
        "call"
    } else if (is.pairlist(x)) {
        "pairlist"
    } else {
        typeof(x)
    }
}
# Convenience Function to set up switch statement
switch_expr <- function(x, ...) {
    switch(expr_type(x),
        ...,
        stop("Don't know how to handle type ", typeof(x), call. = FALSE)
    )
}
# Special function to handle symbols
check_symbol <- function(x, path) {
    # If it is a dataframe add all of its columns into the store
    if (tryCatch(is.data.frame(eval(x)), error = \(e) FALSE) &&
        as_string(x) %in% store) {
        store <<- c(store, names(eval(x)))
    }
    # If variable not in store record an error
    if (!(as_string(x) %in% store)) {
        error_store <<- bind_rows(
            error_store,
            tibble(
                path = path,
                ast_list = list(NULL),
                vars = list(as_string(x))
            )
        )
    }
}
#
#
# Main function handling base cases
find_assign_rec <- function(ast_list, path = "") {
    # a lot of functions fail when given a symbol, but work perfectly fine with a list
    if (is_symbol(ast_list)) {
        ast_list <- list(ast_list)
    }
    map2(ast_list, seq_along(ast_list), \(node, idx) {
        # Keep track of the path
        new_path <- paste(path, idx, sep = ifelse(nzchar(path), ",", ""))
        switch_expr(node,
            # Base cases
            constant = NULL,
            symbol = check_symbol(node, new_path),
            # Recursive cases
            pairlist = find_assign_rec(node, new_path),
            # Function calls are special as they include `<-` and access variables
            call = find_assign_call(node, new_path),
            list = find_assign_rec(node, new_path)
        )
    })
    return(NULL)
}
# How to handle different functions
find_assign_call <- function(ast_list, path = "") {
    # if it is assignment and there is a symbol on the left
    if (is_call(ast_list, "<-") && is_symbol(ast_list[[2]])) {
        # Store defined variable
        # Note `x <- x` won't lead to an error
        store <<- c(store, as_string(ast_list[[2]]))
        # Function definition
        if (is_call(ast_list[[3]], "function")) {
            func_definitions <<- bind_rows(
                func_definitions,
                tibble(
                    func_name = as_string(ast_list[[2]]),
                    func_call = list(ast_list[[3]]),
                    # Get all globals used in the function
                    func_globals = list(codetools::findGlobals(eval(ast_list))),
                    func_path = path
                )
            )
            # if it is function defition we don't want to evaluate its arguments nor what ever inside
            return(NULL)
        }
    }
    #
    if (is_call(ast_list, c("library", "require"))) {
        # I don't support passing anything but a single name to the library().
        # won't get packages that get attached .onAttach, so library(tidyverse) won't attach other packages
        # Stores name space
        store <<- c(store, ls(paste0("package:", ast_list[[2]])))
        return(NULL)
    }
    if (is_call(ast_list, "source")) {
        # Run the analysis on the source(file)
        read_file(ast_list[[2]]) |>
            parse_exprs() |>
            find_assign_rec(path = paste0(path, ",'", ast_list[[2]], "'"))
        return(NULL)
    }
    # Handle package::function calls
    if (is_call(ast_list, "::")) {
        # Check if this package::function exists
        defined <- tryCatch(
            base::exists(ast_list[[3]], envir = base::asNamespace(ast_list[[2]])),
            error = function(e) FALSE
        )
        function_name <- as_string(ast_list[[3]])
        if (!defined) {
            error_store <<- bind_rows(
                error_store,
                tibble(
                    path = path,
                    ast_list = list(ast_list),
                    vars = list(function_name)
                )
            )
        }
        return(NULL)
    }
    # Deals with using passing anonymous function and prevents errors where function defines an argument.
    # For example imap() uses both `x` and `idx`.
    if (is_call(ast_list, "function")) {
        length_before <- length(store)
        store <<- c(store, names(ast_list[[2]]))
        length_after <- length(store)
        which_to_remove <- seq(length_before, length_after)[-1]
        path <- paste(path, "3", sep = ",")
        find_assign_rec(ast_list[[3]], path = path)
        store <<- store[-which_to_remove]
        return(NULL)
    }
    # handles function calls, note it will look at the name of the function
    if ( #
        # Make sure we get a name of the function
        !is_call(ast_list[[1]]) &&
            # Check whether the function is defined
            as_string(ast_list[[1]]) %in% func_definitions$func_name) {
        # keep track of visited functions
        func_names_covered <- c()
        # store undefined variables
        undefined <- c()
        # Recursively look for globals in functions used inside of the function
        r_find_func_globals <- function(func_names) {
            # if we get empty string: return
            if (is_empty(func_names) && all(func_names %in% func_names_covered)) {
                return(NULL)
            }
            new_func_names <- func_names[!func_names %in% func_names_covered]
            func_names_covered <<- c(func_names_covered, new_func_names)

            globals_used <- func_definitions |>
                filter(func_name %in% new_func_names) |>
                pull(func_globals) |>
                unlist()

            func_names_out <- globals_used[globals_used %in% func_definitions$func_name]
            undefined <<- c(undefined, globals_used[!globals_used %in% store])

            r_find_func_globals(func_names = func_names_out)
            return(NULL)
        }
        # Start the function with the function we received
        func_name_2 <- as_string(ast_list[[1]])
        r_find_func_globals(func_name_2)

        # Get path to function definition
        func_path <- func_definitions |>
            filter(func_name %in% func_name_2) |>
            pull(func_path)

        # if everything is defined: return
        if (is_empty(undefined)) {
            return(NULL)
        }

        # store the path to function's definition along with undefined variables
        error_store <<- bind_rows(
            error_store,
            tibble(
                path = func_path,
                ast_list = list(NULL),
                vars = list(undefined)
            )
        )
    }
    # Handle `for` loop
    if (is_call(ast_list, "for")) {
        # store location of local variable, e.g. `i` in `for (i in 1:5)`
        store <<- c(store, ast_list[[2]])
        which_to_remove <- length(store)
        find_assign_rec(ast_list, path = path)
        store <<- store[-which_to_remove]
        return(NULL)
    }
    #
    find_assign_rec(ast_list, path = path)
    return(NULL)
}
# add all the packages to load by default
base_packages <- paste0(
    "package:",
    c(
        "stats", "graphics", "grDevices",
        "utils", "datasets",
        "methods", "base"
    )
)
store <- map(base_packages, ls) |> reduce(c)
# period is often used to pass arguments in pipes, or refer to previous entry
# also include empty strings
store <- c(store, ".", "")
func_definitions <- tibble()
error_store <- tibble()
invisible(find_assign_rec(file_expressions))


if (nrow(error_store) == 0) {
    write("No Error", stdout())
    stop("No Errors")
}

solution <- c()

# Map across all of the errors, and format them for print
purrr::pwalk(error_store, \(path, ast_list, vars) {
    p2e <- path
    var_with_error <- vars
    # Dealing with `source`
    # get all source file names
    all_source_files_names <- str_extract_all(p2e, "'([^']*)'")[[1]]
    if (!is_empty(all_source_files_names)) {
        last_sourced_file <- as.character(
            str_remove_all(
                # Get last sourced file
                all_source_files_names[length(all_source_files_names)],
                "'"
            )
        )
        # find location of single quotes enclosing source files
        locations <- str_locate_all(p2e, "'[^']*'")[[1]]
        last_position <- max(locations) # the largest number is last '
        # path to the expression in the last sourced file
        path <- substring(p2e, last_position + 2) |>
            str_split_1(",") |>
            as.numeric()
        #
        source_file_ast <- parse_exprs(read_file(last_sourced_file))
        # Highlight the error
        source_file_ast[[path]] <- as.symbol(paste0("%", deparse(source_file_ast[[path]]), "%"))
        # Extract the path to the expressions with the error
        expression_with_error <- path[1]
        text_entry <- paste(
            paste0(p2e, " : ", var_with_error),
            str_flatten(rep("-", 60)),
            deparse(source_file_ast[[expression_with_error]]),
            sep = "\\n"
        )
        solution <<- c(solution, text_entry)
        return(NULL)
    }
    # Handle errors in the main file
    expression_with_error <- str_extract(p2e, "^[^,]*") |> as.numeric()
    error_location <- str_split_1(p2e, ",") |> as.numeric()
    # Highlight the error
    file_expressions[[error_location]] <- as.symbol(paste0("%", deparse(file_expressions[[error_location]]), "%"))
    text_entry <- paste(
        paste0(p2e, " : ", var_with_error),
        str_flatten(rep("-", 60)),
        paste(deparse(file_expressions[[expression_with_error]]), collapse = "\\n"),
        sep = "\\n"
    )
    solution <<- c(solution, text_entry)
})

ready_for_stdout <- solution |>
    str_flatten(collapse = "\\n \\n")

write(ready_for_stdout, stdout())
