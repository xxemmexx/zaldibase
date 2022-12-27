function externes_table_module_js(ns_prefix) {
  $("#" + ns_prefix + "externes_table").on("click", ".edit_btn", function() {
    Shiny.setInputValue(ns_prefix + "externe_id_to_edit", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
}