'use strict';

angular.module('gigaApp')
	.directive('oauthButtons', function () {
		return {
			templateUrl: 'components/oauth-buttons/oauth-buttons.html',
			restrict: 'EA',
			controller: 'OauthButtonsCtrl',
			controllerAs: 'OauthButtons',
			scope: {
				classes: '@'
			}
		};
	});
