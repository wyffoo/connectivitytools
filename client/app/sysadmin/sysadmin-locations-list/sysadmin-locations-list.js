'use strict';

angular.module('gigaApp.sysadmin')
	.config(function ($stateProvider) {
		$stateProvider
			.state('sysadmin-locations-list', {
				url: '/sysadmin/locations/list',
				template: '<sysadmin-locations-list></sysadmin-locations-list>',
				authenticate: 'sysadmin',
				resolve: {
					loadCountries: function ($http) {
						// $http returns a promise for the url data
						return $http({method: 'GET', url: '/api/locations/countries'});
					}
				}
			});
	});
