function rapatriement_table_module_js(ns_prefix) {

    $("#" + ns_prefix + "rapatriement_table").on("click", ".edit_btn", function() {
    Shiny.setInputValue(ns_prefix + "rapatriement_patient_id", this.id, { priority: "event"});
    $(this).tooltip('hide');
  });
  
}