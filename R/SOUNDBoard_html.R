#' @importFrom knitr opts_chunk hook_pdfcrop
#' @importFrom bookdown html_document2
NULL

# Use BiocStyle defaults
.width = function(fontsize = "10pt", knitr = FALSE, default = 80L) {
    w = if (is.null(fontsize))
        default
    else
        switch(fontsize, default,
               "12pt" = 66L,
               "11pt" = 73L,
               "10pt" = 80L,
               "9pt"  = 89L,
               "8pt"  = 100L)

    # knitr output is usually commented out
    if (knitr) {
        com = knitr::opts_chunk$get("comment")
        if (!is.null(com) && !is.na(com) && nzchar(com))
            w = w - (nchar(com) + 1L)
    }
    w
}

.opts_chunk <- list(
  error = FALSE,
  fig.scap = NA, # disable default short caption extraction
  crop = TRUE
)

# a list of knitr options as returned by rmarkdown::knitr_options
.knitr_options <- function() {
  list(
    opts_knit = list(
      width = .width()
    ),
    opts_chunk = .opts_chunk,
    knit_hooks = list(
      # remove figure margins
      crop = knitr::hook_pdfcrop
    ),
    opts_hooks = .opts_hooks,
    opts_template = NULL
  )
}

.opts_hooks = list(
  # options fig.small and fig.wide have precedance over fig.env
  fig.small = function(options) {
    if (isTRUE(options$fig.small)) {
      options$fig.env = "smallfigure"
    }
    options
  },
  fig.wide = function(options) {
    if (isTRUE(options$fig.wide)) {
      options$fig.env = "figure*"
    }
    options
  },
  # Set default plot dimensions if user provided no values
  # Important: the hooks are processed in the order they are defined here
  # regardless of the order of options in the code chunk header
  fig.width = function(options) {
    if (is.na(options$fig.width)) {
      options$fig.width = switch(options$fig.env, 7,# fallback to knitr default
                                 "smallfigure" = 5,
                                 "figure*" = 10,
                                 "figure" = 8)
      # override 'fig.height' if 'fig.asp' is set (as plain knitr does)
      if (is.numeric(options$fig.asp))
        options$fig.height = options$fig.width * options$fig.asp
    }

    # re-evaluate code from knitr:::fix_options which is called before the hook
    if ( is.na(options$out.width.px) )
      options$out.width.px = options$fig.width * options$dpi/options$fig.retina
    if ( !is.null(options$out.width) && is.na(options$out.width) )
      options$out.width = "100%"

    options
  },
  fig.height = function(options) {
    if ( is.na(options$fig.height) ){
      options$fig.height = 5
    }

    # re-evaluate code from knitr:::fix_options which is called before the hook
    if ( is.na(options$out.height.px) )
      options$out.height.px = options$fig.height * options$dpi/options$fig.retina

    options
  }
)

SOUNDBoardTempFile <- function (fileName) {
    biocDir = file.path(tempdir(), "SOUNDBoard")
    if (!dir.exists(biocDir))
        dir.create(biocDir)
    file.path(biocDir, fileName)
}

create_html_template <- function() {
  lines <- BiocStyle:::readUTF8(system.file("rmd", "h", "default.html", package = "rmarkdown"))

  template <- SOUNDBoardTempFile("template.html")

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
                         sprintf('<script src="%s"></script>', file.path(.options$get("resources"), "html", "codefolding.js")))

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

#' @export html_document3
html_document3 <- function(toc = TRUE, number_sections = TRUE, fig_width = NA,
                           fig_height = NA, fig_retina = NULL, css = NULL,
                           pandoc_args = NULL, ...) {

  requireNamespace("BiocStyle", quietly = TRUE)
  requireNamespace("SOUNDBoard", quietly = TRUE)

    bookdown::html_document2(base_format = base_format, toc = toc,
        number_sections = FALSE, fig_width = fig_width, fig_height = fig_height,
        fig_retina = fig_retina, css = css, ...)
}

base_format <- function(toc = TRUE, number_sections = TRUE, fig_width = NA,
                          fig_height = NA, fig_retina = NULL, css = NULL, ...) {

  ## customize the default rmarkdown template
  template <- create_html_template()

  # append any user-provided CSS files
  css <- c(.options$get("css"), css)


  # knitr options
  knitr <- rmarkdown:::merge_lists(.knitr_options(), list(
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
  pandoc_args <- NULL
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
    pre_processor = BiocStyle:::pre_processor,
    post_processor = post_processor,
    base_format = rmarkdown_html_document)
  }

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
