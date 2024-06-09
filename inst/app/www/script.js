$(document).ready(() => {
  // Enable tooltips.
  $('[data-bs-toggle="tooltip"]').tooltip();

  // Toggle the text in the bulk data help button.
  $('#data-format-toggle').on('click', event => {
    btn = $(event.target);
    show_format = 'Show required format';
    btn.text(btn.text() === show_format ? 'Hide required format' : show_format);
  });

  // Trigger the file selector via a custom button.
  $('#data-upload-select').on('click', () => {
    $('#data_upload').trigger('click');
  });

  // Display the name of the uploaded file.
  $('#data_upload').on('change', event => {
    $('#data-upload-name').attr('placeholder', event.target.files[0].name);
  });
});
