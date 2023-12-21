'use strict';

angular.module('gigaApp.sysadmin')
	.config(function ($stateProvider) {
		$stateProvider
			.state('sysadmin-projects-list', {
				url: '/admin/projects/list',
				template: '<sysadmin-projects-list></sysadmin-projects-list>',
				authenticate: 'sysadmin',
				resolve: {
					loadProjects: function ($http) {
						// $http returns a promise for the url data
						return $http({method: 'GET', url: '/api/projects'});
					}
				}
			});
	});
