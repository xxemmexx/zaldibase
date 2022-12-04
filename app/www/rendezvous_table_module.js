function rendezvous_table_module_js(ns_prefix) {

    $("#" + ns_prefix + "rendezvous_table").on("click", ".edit_btn", function() {
    Shiny.setInputValue(ns_prefix + "rendezvous_patient_id", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
  
}