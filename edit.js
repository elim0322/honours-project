    <!-- loading jQuery, CKEditor, and annotator.js -->
    <script src="http://code.jquery.com/jquery-1.11.0.min.js"></script>
    <script src="/ckeditor/ckeditor.js"></script>
    <script src="http://assets.annotateit.org/annotator/v1.2.7/annotator-full.min.js"></script>
    <script src="./annotator.offline.min.js"></script>
    <link rel="stylesheet" href="http://assets.annotateit.org/annotator/v1.2.7/annotator.min.css">
    
    <!-- activate annotator.js on <div class="chunk"> elements -->
    <script>
    jQuery(function ($) {
        $('div.chunk').annotator().annotator("addPlugin", "Offline");
    });
    jQuery(function ($) {
        $('div.chunk').data('annotator').plugins.Offline.store.clear();
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
          output = output + 'EDITOR ' + editorblocks[i].name + ':\n' + 
                   editorblocks[i].getData() +
                   '\n\n';
        } else {
          output = output + 'EDITOR ' + editorblocks[i].name + 
                   ' NOT MODIFIED\n\n';
          }
      }
      jQuery.ajax({
         type: "POST",
         url: "save-changes.php",
         data: { changes: output },
         async: false
      });
      var annotations = $("div.chunk").data('annotator').plugins.Offline.store.all();
      jQuery.ajax({
         type: "POST",
         url: "save-annotations.php",
         data: { annotations: JSON.stringify(annotations) },
         async: false
      });
    }
    
    </script>
