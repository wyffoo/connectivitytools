'use strict';

angular.module('gigaApp.sysadmin')
	.config(function ($stateProvider) {
		$stateProvider
			.state('sysadmin-projects-settings', {
				url: '/sysadmin/projects-settings',
				template: '<sysadmin-projects-settings></sysadmin-projects-settings>',
				authenticate: 'sysadmin',
				resolve: {
					Technologies: function ($http) {
						// $http returns a promise for the url data
						return $http({method: 'GET', url: 'assets/mocks/sysadmin-technologies.json'});
					}
				}
			});
	});
