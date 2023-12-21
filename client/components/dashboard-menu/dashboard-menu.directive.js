'use strict';

angular.module('gigaApp.dashboard-menu', [])
	.directive('dashboardMenu', function (Auth, $state) {
		return {
			scope: {},
			templateUrl: 'components/dashboard-menu/dashboard-menu.html',
			restrict: 'EA',
			link: function (scope, element, attrs) {
				scope.hasRole = Auth.hasRole;
				scope.stateIs = $state.is;
			}
		};
	});
