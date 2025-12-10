## Test environments

* Local Windows (mingw32), R version 4.4.1 (2024-06-14 ucrt) [1 NOTE generated]
* macos-15 on GitHub, ASAN + UBSAN on macOS (R-hub action at https://github.com/ready4-dev/ready4/actions/runs/20090837951/job/57637811313)
* macos-latest on GitHub (R-hub action at https://github.com/ready4-dev/ready4/actions/runs/20090837951/job/57637811305)
* Ubuntu 22.04.5 LTS (R-hub action at https://github.com/ready4-dev/ready4/actions/runs/20090837951/job/57637811306) 
* Windows Server 2022 x64 (build 20348) (on winbuilder at https://win-builder.r-project.org/aOI1Z96vUUef/) 

## R CMD check results

There was 1 NOTE (on Local Windows):

  unable to verify current timestamps

---

This version addresses the following CRAN policy violation:

- 'Packages which use Internet resources should fail gracefully with an informative message if the resource is not available or has changed (and not give a check warning nor error)'.

In particular, this version updates the get_gracefully() function and its use in functions such as get_libraries_tb(). The affected functions have been updated so that get_gracefully() is now called with either TRUE, FALSE or NA values supplied to the not_chr_1L_lgl argument. This should ensure a NULL return when internet resources have not been retrieved and more appropriate message delivery.




