# Example from BiocStyle html function
#' @export html_document3
html_document3 <- function(toc = TRUE,
                           number_sections = TRUE,
                           fig_width = NA,
                           fig_height = NA,
                           fig_retina = NULL,
                           css = NULL,
                           pandoc_args = NULL,
                           ...) {

  require("BiocStyle", quietly = TRUE)

  base_format <- function(toc = TRUE,
                          number_sections = TRUE,
                          fig_width = NA,
                          fig_height = NA,
                          fig_retina = NULL,
                          css = NULL,
                          ...) {

  ## customize the default rmarkdown template
  template <- create_html_template()

  # append any user-provided CSS files
  css <- c(soundboard2.css, css)

  post_processor <- function(metadata, input, output, clean, verbose) {
    x = BiocStyle:::readUTF8(output)

    ## insert author affiliations
    x <- BiocStyle:::modifyLines(x, from='<!-- AUTH AFFIL -->',
        insert=BiocStyle:::auth_affil_html(metadata))

    ## format caption titles
    # x = caption_titles(x)

    ## replace footnotes with sidenotes
    # x = process_footnotes(x)

    BiocStyle:::writeUTF8(x, output)
    output
  }

  # knitr options
  knitr <- merge_lists(.knitr_options(), list(
    knit_hooks = list(
      plot = function(x, options = list()) {
        out.extra = switch(options$fig.env, NULL,
                           "smallfigure" = 'class="smallfigure"',
                           "figure*" = 'class="widefigure"',
                           "figure" = NULL)
        options$out.extra = paste(options$out.extra, out.extra)
        hook_plot_md(x, options)
      }
    )
  ))

  # Call the rmarkdown::html_document function with `template="default"` and
  # substitute the template only afterwards in order to retain some of the
  # original functionality such as mathjax, floating toc, and code folding.

  rmarkdown_html_document <- rmarkdown::html_document(
    toc = toc,
    number_sections = TRUE,
    fig_width = fig_width,
    fig_height = fig_height,
    css = css,
    ...)

  ## override some default pandoc args; we use this low-level approach rather
  ## than passing them in 'pandoc_args' to 'rmarkdown::html_document' because
  ## rmarkdown just concatenates base and overlay argument lists which does not
  ## allow for substitution
  pandoc_args <- c(pandoc_args, c("--template", template))

  arg_names <- c("--email-obfuscation", "--template")
  arg_names <- arg_names[arg_names %in% pandoc_args]

  idx <- match(arg_names, pandoc_args)

  ## substitute arguments
  rmarkdown_html_document$pandoc$args [
    match(arg_names, rmarkdown_html_document$pandoc$args) + 1L
  ] <- pandoc_args [idx + 1L]

  ## append the rest
  rmarkdown_html_document$pandoc$args <-
    c(rmarkdown_html_document$pandoc$args, pandoc_args[-c(idx, idx+1L)])


  rmarkdown::output_format(
    knitr = knitr,
    pandoc = NULL,
    pre_processor = pre_processor,
    post_processor = post_processor,
    base_format = rmarkdown_html_document)
  }

    bookdown::html_document2(
                             toc = toc,
                             number_sections = FALSE,
                             fig_width = fig_width,
                             fig_height = fig_height,
                             fig_retina = fig_retina,
                             css = css,
                             ...
                             )
}

create_html_template <- function() {
  lines <- BiocStyle:::readUTF8(system.file("rmd", "h", "default.html", package = "rmarkdown"))

  template <- BiocStyle:::biocTempfile("template.html")

  ## placeholder for author affiliation block which is inserted during postprocessing
  lines <- BiocStyle:::modifyLines(lines, from='$for(author)$', to='$endfor$', insert='<!-- AUTH AFFIL -->')

  lines <- BiocStyle:::modifyLines(lines, from='<div class="abstract">', to='</div>', insert=c(
    '<h4 class="abstract">Abstract</h4>',
    '$abstract$',
    '$endif$',
    '$if(package)$',
    '<h4 class="package">Package</h4>',
    '<p>$package$</p>'))

  lines <- BiocStyle:::modifyLines(lines, from='<div id="$idprefix$TOC">', replace=FALSE, before=TRUE, insert="<h1>Contents</h1>")

  ## modify some inline CSS
  lines <- BiocStyle:::modifyLines(lines, from='^\\.toc-content \\{', to = '\\}', fixed=FALSE)

  lines <- BiocStyle:::modifyLines(lines, from='^\\code \\{', to = '\\}', fixed=FALSE)

  for (i in 1:2)
    lines <- BiocStyle:::modifyLines(lines, from='^  pre:not\\(\\[class\\]\\) \\{', to = '\\}', fixed=FALSE)

  lines <- BiocStyle:::modifyLines(lines=lines, from='^\\.main-container \\{', to = '\\}', fixed=FALSE, offset=c(1L, -1L), insert=c(
    '  max-width: 828px;',
    '  margin-left: auto;',
    '  margin-right: auto;'))

  lines <- BiocStyle:::modifyLines(lines, from='^div\\.tocify \\{', to = '\\}', fixed=FALSE, offset=c(1L, -1L), insert=c(
    '  width: 20%;',
    '  max-width: 246px;',
    '  max-height: 85%;'))

  ## use the modified code folding script
  lines <- BiocStyle:::modifyLines(lines=lines, from='<script src="$navigationjs$/codefolding.js"></script>', insert=
                         sprintf('<script src="%s"></script>', file.path(resources, "html", "codefolding.js")))

  ## Automatic equation numbering
  lines <- BiocStyle:::modifyLines(lines=lines, from='$if(mathjax-url)$', replace=FALSE, before=TRUE, insert=c(
    '<script type="text/x-mathjax-config">',
    '  MathJax.Hub.Config({',
    # '    TeX: {',
    # '      TagSide: "right",',
    # '      equationNumbers: {',
    # '        autoNumber: "AMS"',
    # '      }',
    # '    },',
    '    "HTML-CSS": {',
    '      styles: {',
    '        ".MathJax_Display": {',
    '           "text-align": "center",',
    '           padding: "0px 150px 0px 65px",',
    '           margin: "0px 0px 0.5em"',
    '        },',
    '      }',
    '    }',
    '  });',
    '</script>'))

  BiocStyle:::writeUTF8(lines, template)

  template
}
