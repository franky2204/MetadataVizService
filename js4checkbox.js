$(document).ready(function(){
  $('input[name=var]').on('click', function(event){
    if($('input[name=var]:checked').length > 2){
      $(this).prop('checked', false);
    }
  });
  $('input[name=var]').on('click', function(event){
    if($('input[name=var]:checked').length == 0){
      $(this).prop('checked', true);
    }
  });
});