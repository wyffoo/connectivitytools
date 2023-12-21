'use strict';

angular.module('gigaApp.sysadmin')
	.config(function ($stateProvider) {
		$stateProvider
			.state('sysadmin-users', {
				url: '/sysadmin/users',
				template: '<sysadmin-users></sysadmin-users>',
				authenticate: 'sysadmin',
				resolve: {
					loadUsers: function ($http) {
						// $http returns a promise for the url data
						return $http({method: 'GET', url: '/api/users'});
					},
					loadCountries: function ($http) {
						return $http({method: 'GET', url: '/api/projects/verified_countries'});
					}
				}
			});
	});
