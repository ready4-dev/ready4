## Test environments

* Local Windows (mingw32), R 4.3.1 [1 NOTE generated]
* Mac OS macos-latest (R-hub action at https://github.com/ready4-dev/ready4/actions/runs/8684726421/job/23812843262)
* Windows windows-latest (R-hub action at https://github.com/ready4-dev/ready4/actions/runs/8684726421/job/23812843409)
* Fedora Linux 38 R Under development (unstable) gcc13 (R-hub action at https://github.com/ready4-dev/ready4/actions/runs/8684726421/job/23812843048) 
* Windows Server 2022 x64 (build 20348) (on winbuilder) [1 NOTE generated] 

## R CMD check results

There were no ERRORs or WARNINGs. 

There was 1 NOTE (on Local Windows and winbuilder) relating to this being a new submission.



---

This version addresses the following CRAN policy violation:

- 'Packages which use Internet resources should fail gracefully with an informative message if the resource is not available or has changed (and not give a check warning nor error)'.

To address this issue, this version introduces the get_gracefully() function and deploys it within functions that seek to retrieve internet resources.




