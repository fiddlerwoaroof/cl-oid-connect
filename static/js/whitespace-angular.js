var whitespace = angular.module('whitespace', ['ngResource', 'ngSanitize']);
whitespace.controller('MainCtrl', ['$scope', '$http', '$resource', '$sce', function ($scope, $http, $resource, $sce) {
    var feeds = $resource('/feeds', {  }, { json : { method : 'GET', url : '/feeds/json' }, add : { method : 'POST', url : '/feeds/add' } });
    $scope.feeds = feeds.json();
    $scope.data = 'hello world!';
    $scope.addForm = { url : '' };
    $scope.renderHtml = function (htmlCode) {
        return $sce.trustAsHtml(htmlCode);
    };
    $scope.toggleClosed = function (ent) {
        return ent.closed = !ent.closed;
    };
    return $scope.addFeed = function () {
        return feeds.add({ 'url' : $scope.addForm.url, 'api' : 'yes' }).$promise.then(function (feed) {
            return $scope.feeds.result.unshift(feed.result);
        });
    };
}]);

