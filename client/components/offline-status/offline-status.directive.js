'use strict';

(function () {

	function errorsInterceptor($rootScope, $q) {
		return {
			request(config) {
				return config;
			},

			// Intercept 401s and redirect you to login
			responseError(response) {
				var status = response.status;
				if (status === -1) {
					$rootScope.$broadcast('ConnectionError', status);
				} else if ((status >= 500) && (status < 600)) {
					$rootScope.$broadcast('ServerError', status);
				}
				return $q.reject(response);
			}
		};
	}

	angular.module('gigaApp.offline-status', [])
		.directive('offlineStatus', function () {
			return {
				scope: {},
				templateUrl: 'components/offline-status/offline-status.html',
				restrict: 'EA',
				link: function (scope) {
					scope.ServerError = false;
					scope.ConnectionError = false;

					scope.$root.$on('ConnectionError', () => {
						scope.ConnectionError = true;
						setTimeout(() => {
							scope.ConnectionError = false;
							scope.$apply('ConnectionError');
						}, 5000);
					});

					scope.$root.$on('ServerError', () => {
						scope.ServerError = true;
						setTimeout(() => {
							scope.ServerError = false;
							scope.$apply('ServerError');
						}, 5000);
					});
				}
			};
		})
		.factory('errorsInterceptor', errorsInterceptor)
		.config(function ($httpProvider) {
			$httpProvider.interceptors.push('errorsInterceptor');
		});

})();
