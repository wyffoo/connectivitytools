'use strict';

angular.module('gigaApp.simple-alert', [])
	.directive('simpleAlert', function () {
		return {
			templateUrl: 'components/simple-alert/simple-alert.html',
			restrict: 'EA',
			link: function (scope) {
				scope.alerts = [];

				scope.closeAlert = (index) => {
					scope.alerts.splice(index, 1);
				};

				scope.addAlert = (type, msg) => {
					scope.alerts.push({type: type, msg: msg});
				};

			}
		};
	});
