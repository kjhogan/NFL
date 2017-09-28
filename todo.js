//Check off to-dos
$("li").click(function(){
    $(this).toggleClass("completed");
});

$("span").click(function(e){
    e.stopPropagation();
    $(this).parent().fadeOut(500,function(){
        $(this).remove();
    });
});