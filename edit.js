
    <!-- loading jQuery, CKEditor, and annotator.js -->
    <script src="http://code.jquery.com/jquery-1.11.0.min.js"></script>
    <script src="/ckeditor/ckeditor.js"></script>
    <script src="http://assets.annotateit.org/annotator/v1.2.7/annotator-full.min.js"></script>
    <link rel="stylesheet" href="http://assets.annotateit.org/annotator/v1.2.7/annotator.min.css">
    
    <!-- activate annotator.js on <div class="chunk"> elements -->
    <script>
    jQuery(function ($) {
        $('div.chunk').annotator().annotator('setupPlugins', {}, {
          Tags: false,
          Filter: false
        });
    });
    </script>
    
    <!-- action for "save" button 
         to save CKEditor changes to test-changes.txt
         and to save annotator.js annotations to test-annotations.txt -->
    <script>
    savechanges = function() {
      var output = '';
      var editorblocks = CKEDITOR.instances;
      for (i in editorblocks) {
        if (editorblocks[i].checkDirty()) {
          output = output + 'editor ' + i + ':\n' + editorblocks[i].getData() +
            '\n\n';
        } else {
          output = output + 'editor ' + i + ' NOT MODIFIED\n\n';
          }
      }
      jQuery.ajax({
        type: "POST",
        url: "save-changes.php",
        data: { changes: output }
      });
      var annotations = jQuery.ajax({
        type: "GET",
        url: "http://annotateit.org/api/search?uri=http://stat220.stat.auckland.ac.nz/cke/test.html",
        async: false
      });
      jQuery.ajax({
        type: "POST",
        url: "save-annotations.php",
        data: { annotations: annotations.responseText }
      });
    }
    </script>
