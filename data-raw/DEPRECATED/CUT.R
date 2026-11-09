# `ready4` includes two module templates that inherit from `Ready4Module`. These are `Ready4Public` and `Ready4Private` and both are almost as minimally informative as their parent (the only difference being that their instances have the values "Public" or "Private" assigned to the `dissemination_1L_chr` slot).
#
# ```{r}
# Y <- Ready4Public()
# str(Y)
# ```
#
# ```{r}
# Z <- Ready4Private()
# str(Z)
# ```
#
# Like the `Ready4Module` template they inherit from, the purpose of `Ready4Public` and `Ready4Private` is to be used as parent classes of other templates. Using either of `Ready4Public` and `Ready4Private` can be a potentially efficient way of partially automating access policies for model data. If **all** the data contained in a module can **always** be shared publicly, it may be convenient to note this by using a module that has been created from the `Ready4Public` template. Similarly, if at least some of the data contained in a module will always be unsuitable for public dissemination, it can be useful to use a module created from `Ready4Private`. When the dissemination policy for data contained in a module will vary depending on user or context, it is more appropriate to use a module template that inherits from `Ready4Module` but not from either of `Ready4Public` and `Ready4Private`. In this latest case, users may choose to add descriptive information about the data access policy themselves using the `renewSlot` method.  The dissemination policy can be inspected with the `procureSlot` method.
#
# ```{r}
# X <- renewSlot(X,
#                "dissemination_1L_chr",
#                "Staff and students of research institutes")
# procureSlot(X,
#             "dissemination_1L_chr")
# ```
