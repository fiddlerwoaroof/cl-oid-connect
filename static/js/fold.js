'';
jQuery(document).ready(function () {
    jQuery('.link-header, .feed-header').click(function () {
        jQuery('.menu').addClass('open');
        return true;
    });
    jQuery('.link-header').click(function () {
        jQuery(this).siblings('.link-content').each(function () {
            if (jQuery(this).css('max-height') === '0px') {
                var addedHeight = jQuery(this).children().outerHeight();
                var parentHeight = jQuery(this).parents('.post-list').css('max-height');
                jQuery(this).css('max-height', addedHeight);
                return jQuery(this).parents('.post-list').css('max-height', addedHeight + parentHeight);
            } else {
                return jQuery(this).css('max-height', '0px');
            };
        });
        return jQuery(this).parent().toggleClass('closed');
    });
    jQuery('.feed-header').click(function () {
        jQuery(this).siblings('.post-list').each(function () {
            return jQuery(this).css('max-height') === '0px' ? jQuery(this).css('max-height', this.scrollHeight) : jQuery(this).css('max-height', '0px');
        });
        return jQuery(this).parent().toggleClass('closed');
    });
    jQuery('.flip-button').click(function () {
        var styleSheet = jQuery('link[href^="/theme"]');
        var styleSheetName = styleSheet.attr('href');
        return styleSheet.attr('href', styleSheetName.match(/dark/) ? styleSheetName.replace(/dark/, 'light') : styleSheetName.replace(/light/, 'dark'));
    });
    return null;
});

