$(document).ready(function () {
    $('.link-header').click(function () {
        $(this).siblings('.link-content').each(function () {
            if ($(this).css('max-height') === '0px') {
                var addedHeight = $(this).children().outerHeight();
                var parentHeight = $(this).parents('.post-list').css('max-height');
                $(this).css('max-height', addedHeight);
                return $(this).parents('.post-list').css('max-height', addedHeight + parentHeight);
            } else {
                return $(this).css('max-height', '0px');
            };
        });
        return $(this).parent().toggleClass('closed');
    });
    $('.feed-header').click(function () {
        $(this).siblings('.post-list').each(function () {
            return $(this).css('max-height') === '0px' ? $(this).css('max-height', this.scrollHeight) : $(this).css('max-height', '0px');
        });
        return $(this).parent().toggleClass('closed');
    });
    invertPalette = function () {
        var styleSheet = $('link[href^="/theme"]');
        var styleSheetName = styleSheet.attr('href');
        return styleSheet.attr('href', styleSheetName.match(/dark/) ? styleSheetName.replace(/dark/, 'light') : styleSheetName.replace(/light/, 'dark'));
    };
    $('.flip-button').click(invertPalette);
    return null;
});

