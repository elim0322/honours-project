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
         data: { changes: output },
         async: false
      });
      var annotations = jQuery.ajax({
         type: "GET",
         url: "http://annotateit.org/api/search?uri=http://stat220.stat.auckland.ac.nz/cke/test.html",
         async: false
      });
      jQuery.ajax({
         type: "POST",
         url: "save-annotations.php",
         data: { annotations: annotations.responseText },
         async: false
      });
    }
    
    /* action for "submit" button
       to get rid of old annotations and changes */
    submit = function() {
      savechanges();
      var annotations = $('div.chunk').data('annotator').plugins['Store'].annotations;
      var numAnn = annotations.length;
      for (i=0; i<numAnn; i++) {
        var ann = annotations[i];
        var id = ann.id;
        var response = jQuery.ajax({
          type: "OPTIONS",
          url: "http://annotateit.org/api/annotations/" + id,
          origin: "http://stat220.stat.auckland.ac.nz",
          host: "http://anotateit.org",
          headers: {
            'access-control-request-method': "DELETE",
            'Access-Control-Request-Headers': "content-type,x-annotator-auth-token"
          },
          async: false
        })
        var token = $('div.chunk').data('annotator:headers')['x-annotator-auth-token'];
        var result = jQuery.ajax({
          type: "DELETE",
          url: "http://annotateit.org/api/annotations/" + id,
          headers: {
            'x-annotator-auth-token': token
          },
          async: false
        })
      }
      /* Navigate back to upload.html */
      window.location.href = "upload.html";
    }
    </script>
