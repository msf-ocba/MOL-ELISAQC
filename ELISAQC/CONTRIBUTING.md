Thank you for your interest in this repository.
Please see below for guidelines for contributing to the repository. 

# Contributing new functions
We encourage you to contribute your own functions. Eligible functions should meet the following criteria:

- The function should fit the **remit** of this package, i.e. processing ELISA results
- New functions should complement existing functions
- Alternatively, suggest enhancements to existing functions
- Please provide as much detail as possible about your function including:
    + name and contact details of the author 
    + references for any credits (e.g. StackOverflow, other contributors)
    + a concise description of the function's purpose (avoid jargon)
    + details and caveates
    + a testable example
- Please provide these details by using structured `Roxygen2` style annotation 
  (see  [here](http://kbroman.org/pkg_primer/pages/docs.html) for examples)
- Please **do not** include any sensitive data (usernames, passwords, personal identifying information) in your functions.

To contribute new functions or edits to existing code, please do the following: 

 1. Fork the repository
 2. Add your function to the `ELISAQC` development branch and commit to your repository
 3. When satisfied that your function works as you wish, submit a merge request describing the function. 
 4. Include your user name in the request title and if the merge request addresses a specific issue in the main repository, please reference that with issue: #the_issue_number in your commit message.

For guides on setting up git to work with both a fork and the original repository, see the github guide [Configuring a remote for a fork](https://help.github.com/articles/configuring-a-remote-for-a-fork/) and see [syncing a fork](https://help.github.com/articles/syncing-a-fork/) for information on how to keep your forked version up to date with the original repository.

## Style guide

 * snake_case for function names, if possible (not CamelCase unless names are very long)
 * please don't use dot separators for function names, this causes confusion when methods are used for classes. 
 * please include whitespace between operators; e.g. `z + x`, not `z+x` and `this_var = numerator / denom`, not `this_var=numerator/denom`
 * please refer to the [Advanced R style guide](http://adv-r.had.co.nz/Style.html) for further information on style

# Issues

We very much welcome issues if you have some feedback on the current functions, or ideas for new functions. 

 * Please include reproducible examples with reproducible data where possible, e.g. using `dput()` for data and consider the [reprex](http://reprex.tidyverse.org/) package. 
 * Please consider including information from `devtools::session_info()` if you have any reason to suspect that the bug you are reporting might be due to an different version of R or an operating system problem.
 * Please use labels, where you can find an appropriate label, e.g. feature request, bug
