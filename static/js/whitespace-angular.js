var whitespace = angular.module('whitespace', ['ngResource', 'ngSanitize']);
whitespace.controller('MainCtrl', ['$scope', '$http', '$resource', '$sce', function ($scope, $http, $resource, $sce) {
    var feeds = $resource('feeds/json', {  });
    $scope.feeds = feeds.get();
    $scope.data = 'hello world!';
    $scope.renderHtml = function (htmlCode) {
        return $sce.trustAsHtml(htmlCode);
    };
    $scope.toggleClosed = function (ent) {
        return ent.closed = !ent.closed;
    };
    return null;
}]);

