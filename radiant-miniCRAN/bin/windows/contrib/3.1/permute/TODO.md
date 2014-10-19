# TODO List

A TODO list for **permute** - or things I know are broken or needed.

 * `summary.allPerms` - is not printing the permutation scheme.
   *Done in 0.7-5*

 * `print.permControl` - this needs to be updated to new API, and I don't
   like the `*** Foo ***` headings I used before. *Done in 0.7-3*

 * Need a way to update the permutation scheme, e.g. when a control
   object already exists and want to tweak it. Example is in `?allPerms`
   where I turn mirroring on via

        ctrl$within$mirror <- TRUE

    But how? Best idea currently is an `update.permControl` method. The
    generic is in namespace `stats`. Or a `modify()` function, perhaps
    coupled with specific replacement functions for certain components.
   
    *DONE, in part, in 0.7-5* The matched call is now returned by `how()`
    and this allows `update()` to do its work with no further effort from
    me. What isn't so neat is that currently this means you need to type
    out in full any specification of `within` and `plots` as these take
    the results of function calls. Hence we have, from `./man/how.Rd`
    
        plts <- gl(4,10)
        blks <- gl(2,20)
        h1 <- how(within = Within(type = "series", mirror = TRUE),
                  plots = Plots(strata = plts, type = "series"),
                  blocks = blks)

        ## The design can be updated...
        ## ... remove the blocking:
        update(h1, blocks = NULL)
        ## ... or switch the type of shuffling at a level:
        update(h1, plots = Plots(strata = plts, type = "none"))
        
    Where in the second `update()` the entire `Plots()` call needs to
    repeated to change just one part, the `type`.

    This has been tweaked a bit. The second example can now be done via:
    
        update(h1, plots = update(getPlots(h1), type = "none"))
    
    Here `getPlots(h1)` returns the `plots` component, which too has 
    a `call` component and hence can be `update()`ed, hence the nested 
    calls to `update()`.
         
 * `permControl` - deprecate this in favour of `how` as in "how to 
   permute"? *DONE Completed in 0.7-4*

 * `permuplot` - this may be fundamentally bust - it only worked in the
   original API and never got updated. Now the API is 2 versions further
   on! It is also more complex than the original - not sure it'll be
   easy without a lot of work to visualise all possible schemes.

 * `check` insists on returning all permutations *without* the observed
   one.

   *DONE Completed somewhere, probably when I made observed only work via
   how()*

 * The following example from `shuffleSet()` gives same number of 
   permutations via `numPerms()` for both designs; should be fewer if 
   same permutation within each plot.
   
        ## 10 random permutations in presence of Plot-level strata
        plotStrata <- Plots(strata = gl(4,5))
        CTRL <- how(plots = plotStrata,
                    within = Within(type = "free"))
        shuffleSet(20, 10, control = CTRL)
        ## as above but same random permutation within Plot-level strata
        CTRL <- how(plots = plotStrata,
                    within = Within(type = "free", constant = TRUE))
        shuffleSet(20, 10, CTRL)

    *DONE in 0.8-0*

 * Write an Rd page for the `"permutationMatrix"` S3 class where I can 
   describe the object returned by `shuffleSet()` and the methods 
   available for it.

    *DONE in 0.7-8*