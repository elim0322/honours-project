snap <- function(infile = NULL, outfile = NULL) {
    if (is.null(infile)) {
        infile <- load.dir()
    }
    if (!grepl("post.html$", infile))
        stop("infile is not a post.html file")
    
    src <- readLines(infile)
    
    # 1st: matches for <p blah blah...> (like <p class =...>)
    # 2nd: matches for <p>
    # Does the boolean operator OR work for grep??
    modLines <- c(grep("(^\\s*<p\\s+.*>$)", src),
                  grep("^\\s*<p>\\s*$", src))
    # Assuming below lines all end with ">"
    src[modLines] <- gsub(">", ' contenteditable="true">', src[modLines])
    
    # Load jQuery, ckeditor.js, annotator.js
    loader <- c('<script src="http://code.jquery.com/jquery-1.11.0.min.js"></script>',
           '<script src="/ckeditor/ckeditor.js"></script>',
           '<script src="http://assets.annotateit.org/annotator/v1.2.7/annotator-full.min.js"></script>',
           '<link rel="stylesheet" href="http://assets.annotateit.org/annotator/v1.2.7/annotator.min.css">')
    
    antr <- 
    "<script>
    jQuery(function ($) {
        $('div.chunk').annotator().annotator('setupPlugins', {}, {
          Tags: false,
          Filter: false
        });
    });
    </script>"
    
    saveChgs <-
    "<script>
    savechanges = function() {
      var output = '';
      var editorblocks = CKEDITOR.instances;
      for (i in editorblocks) {
        if (editorblocks[i].checkDirty()) {
          output = output + 'editor ' + i + ':\\n' + editorblocks[i].getData() +
            '\\n\\n';
        } else {
          output = output + 'editor ' + i + ' NOT MODIFIED\\n\\n';
          }
      }
      jQuery.ajax({
        type: \"POST\",
        url: \"save-changes.php\",
        data: { changes: output }
      });
      var annotations = jQuery.ajax({
        type: \"GET\",
        url: \"http://annotateit.org/api/search?uri=http://stat220.stat.auckland.ac.nz/cke/test.html\",
        async: false
      });
      jQuery.ajax({
        type: \"POST\",
        url: \"save-annotations.php\",
        data: { annotations: annotations.responseText }
      });
    }
    </script>"
    
    saver <-
    '<p id="savebutton" style="background-color:grey; text-align:center"><button onclick="savechanges()">save</button>
    </p>'
    
    # Protection: find the smallest grep = topmost <head>
    headLines <- min(grep("<head>", src))
    # Only one body tag per html document
    bodyLines <- grep("<body>", src)
    
    # 1st component: start to "<head>"
    # 2nd component: <head>+1 line to "<body>"
    # 3rd component: "<body>"+1 line to the end
    srcPieces <- list(src[1:headLines], src[(headLines + 1):bodyLines], src[(bodyLines + 1):length(src)])
    
    ####################################### Writing edit.html #######################################
    if (is.null(outfile)) {
        outfile <- gsub("post.html","edit.html", infile)
    }
    f <- file(outfile, open = "w")
    writeLines(c(srcPieces[[1]], loader, antr, saveChgs, srcPieces[[2]], saver, srcPieces[[3]]), f)
    close(f)
}

