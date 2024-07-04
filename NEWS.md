# ready4 0.1.14
This patch modifies the print_packages() function so that it can filter output by model "section".

# ready4 0.1.13
This patch updates one of the examples files so that its execution does not breach the CRAN recommended run time limit.

# ready4 0.1.12
This patch updates functions for printing HTML tables to return NULL values if supplied with NULL values instead of tibble objects.

# ready4 0.1.11
This patch introduces the get_gracefully() function to address a CRAN policy violation and to ensure that unsuccessful attempts at retrieving internet resources fail gracefully.

# ready4 0.1.10
This patch fixes a documentation error under which the description of many functions referred to the wrong function.

# ready4 0.1.9
This patch fixes a typo in a reference to an arXiv preprint contained in the DESCRIPTION file.

# ready4 0.1.8
This patch addresses CRAN reviewer feedback by ensuring that prompt messages written to console can now be suppressed. This patch also updates the documentation of functions imported from other packages and the way an arXiv pre-print is referenced in the DESCRIPTION file to comply with CRAN reviewer recommendations.

# ready4 0.1.7
This patch modifies framework syntax descriptions as well as updating function documentation and function argument defaults to comply with CRAN reviewer recommendations.

# ready4 0.1.6
This patch modifies code style, documentation and argument defaults to comply with CRAN reviewer recommendations.

# ready4 0.1.5
This patch removed an extraneous LICENSE file.

# ready4 0.1.4
This patch made minor modification to function write_conditional_tags.

# ready4 0.1.3

Patch with fixes of bugs in write_blog_entries and write_to_render_post functions.

# ready4 0.1.2

This is a patch for the release being prepared for submission to CRAN.

## New features

This adds the helper function write_conditional_tags for updating documentation once selected package dependencies have been moved from 'Imports' to 'Suggests'.

# ready4 0.1.1
This is a patch for the release being prepared for submission to CRAN.

## New features

This release addresses a bug with printing the tabular summary of framework libraries.

# ready4 0.1.0

This is the development release that is being prepared for submission to CRAN.

## New features

This release introduces the Ready4Module template model module, the ready4 framework syntax and functions for maintaining a documentation website.
