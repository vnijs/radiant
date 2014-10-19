\name{NEWS}
\title{permute News}
\encoding{UTF-8}

\section{Changes in version 0.8-0}{

  \subsection{GENERAL}{

    \itemize{
      \item Version 0.8-0 represents a major update of \pkg{permute},
      with some backwards-incompatible changes to the main
      functions. The main addition is the availability of block-level
      restrictions on the permutations, which are required for whole- and
      split-plot designs. 
    }

  } % general

  \subsection{NEW FEATURES}{

    \itemize{
      \item \code{how()}, a new function to create permutation
      designs. This replaces the deprecated function \code{permControl}.

      \item \pkg{permute} gains the addition of true blocking structures
      with which to restrict the permutations. Blocks sit as the
      outermost layer of the permutations, and can contain plots which
      in turn contain samples. In contrasts to plots, blocks are never
      permuted and samples are never shuffled between
      blocks. Permutation only ever happens within blocks.

      To facilitate this, plot-level strata are now specified via
      \code{Plots()} instead of via the old strata argument of
      \code{how()}. Blocks are specified via a new argument \code{blocks},
      which takes a factor variable.

      \item A new suite of extractor and replacement functions is
      provided with which to interact with permutation designs created
      by \code{how()}. Extractor functions have names \code{getFoo()},
      where \code{Foo()} is a component of the design. Replacement
      functions have names \code{setFoo}. The replacement function are
      especially for use by package authors wishing to alter permutation
      within their functions. The extractor functions are recommened for
      all users wishing to extract features of the permutation design. 

      \item As a convenience for users, the \code{update()} function
      will now work with objects of classes \code{"how"}, \code{"Plots"}
      or \code{"Within"} to allow quick updating of features of the
      permutation design. This approach is intended for interactive use
      at the top-level and not within functions, where the new
      \code{setFoo} replacement functions should be used.

      \item \code{shuffleSet()} is enhanced in this version. Firstly, the
      function now returns a classed object which has a \code{print()}
      method to allow for compact printing of the design elements used
      to generate the set of permutations. Second, \code{shuffleSet()}
      will sample \code{nset} permutations from the entire set of
      permutations should a small number of possible permutations
      trigger generation of the entire set. This avoids the generation
      of a set of non-unique permutations. Finally the random seed that
      generated the set is stored as an attribute.

      \item \code{allPerms()} no longer assumes that samples are in
      block and/or plot ordering.

      \item The package vignette is much expanded in this version with
      new sections on using \pkg{permute} within functions that will be
      of interest to package authors wishing to use \pkg{permute} in
      their packages.
    }

  } % new features

  \subsection{DEPRECATED}{

    \itemize{
      \item \code{permControl} is deprecated in favour of \code{how}.

      \item \code{permuplot} is broken and effectively defunct given the
      changes to the way permutation are defined and the addition of
      blocks. \code{permuplot} is no longer expoerted from the package
      namespace.
    }
    
  } % deprecated
}