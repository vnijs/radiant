// Line below was in ui.R. No longer needed
// tags$head(HTML("<script type='text/x-mathjax-config'>MathJax.Hub.Config({ TeX: { equationNumbers: {autoNumber: 'all'} } });</script>")),
// Use $\setCounter{-10}$ at the top of a file to reset the equation counter for
// each markdown file
// If you do not use this feature, equation number will start with (1) on the
// case that is opened first - probably not what you want

// from https://groups.google.com/d/msg/mathjax-users/uA-l1L9yVTA/-adeXpevMIkJ
window.MathJax = {
  jax: ["input/TeX", "output/HTML-CSS"], //just some defaults
  extensions: ["tex2jax.js", "MathMenu.js", "MathZoom.js"],
  TeX: {
    extensions: ["AMSmath.js", "AMSsymbols.js"],
    equationNumbers: {
      autoNumber: "all"
    }
  },
  AuthorInit: function() {
    MathJax.Hub.Register.StartupHook("TeX AMSmath Ready", function() {
      MathJax.InputJax.TeX.Definitions.Add({
        macros: {
          setCounter: "setCounter"
        }
      }, null, true);
      MathJax.InputJax.TeX.Parse.Augment({
        setCounter: function(name) {
          var num =  parseInt(this.GetArgument(name));
          MathJax.Extension["TeX/AMSmath"].number = num;
        }
      });
    });
  }
};
