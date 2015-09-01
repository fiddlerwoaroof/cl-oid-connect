$(document).ready(function () {
    return $('.link-header').click(function () {
        $(this).siblings('.link-content').each(function () {
            return $(this).css('max-height') === '0px' ? $(this).css('max-height', this.scrollHeight) : $(this).css('max-height', '0px');
        });
        return $(this).parent().toggleClass('closed');
    });
});

