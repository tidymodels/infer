# Contributing

Contributions to the `infer` whether in the form of bug fixes, issue reports, new code or documentation improvement
are encouraged and welcome. We welcome novices who may have never contributed to a package before as well as friendly
veterans looking to help us improve the package for users.

Please use the GitHub issues. For any pull request, please link to or open a corresponding issue 
in GitHub issues. Please ensure that you have notifications turned on and respond to questions, comments or 
needed changes promptly.

##  Tests

`infer` uses `testthat` for testing. Please try to provide 100% test coverage for any submitted code and always check 
that existing tests continue to pass. If you are a beginner and need help with writing a test, mention this
in the issue and we will try to help.

It's also helpful to run `goodpractice::gp()` to ensure that lines of code are not over 80 columns and that all lines of code have tests written. Please do so prior to submitting any pull request and fix any suggestions from there.
Reach out to us if you need any assistance there too.

## Pull requests

Pull requests should be against the `develop` branch NOT the `master` branch.  You can set this when creating 
your pull request. Please make a separately named branch to submit.  Keep each branch for a complete specific
issue.

## Code style

Please use snake case (such as `rep_sample_n`) for function names.  Besides that, in general follow the 
[tidyverse style](http://style.tidyverse.org/) style for R. 

## Code of Conduct

When contributing to the `infer` package you must follow the code of 
conduct defined in [CONDUCT.md](CONDUCT.md)