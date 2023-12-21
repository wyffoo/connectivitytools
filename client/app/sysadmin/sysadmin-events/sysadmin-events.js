'use strict';

angular.module('gigaApp.sysadmin')
	.config(function ($stateProvider) {
		$stateProvider
			.state('sysadmin-events', {
				url: '/sysadmin/events',
				template: '<sysadmin-events></sysadmin-events>',
				authenticate: 'sysadmin',
				resolve: {
					Events: function ($http) {
						// $http returns a promise for the url data
						return $http({method: 'GET', url: 'assets/mocks/sysadmin-events.json'});
					},
					EventTypes: function ($http) {
						// $http returns a promise for the url data
						return $http({method: 'GET', url: 'assets/mocks/sysadmin-events-types.json'});
					}
				}
			});
	});
