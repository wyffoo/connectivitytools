'use strict';

angular.module('gigaApp.admin')
	.config(function ($stateProvider) {
		$stateProvider
			.state('admin-projects-list', {
				url: '/admin/projects/list',
				template: '<admin-projects-list></admin-projects-list>',
				authenticate: ['admin','sysadmin'],
				resolve: {
					loadProjects: function ($http, appConfig) {
						// $http returns a promise for the url data
						return $http({method: 'GET', url: '/api/projects/list'});
					}
				}
			});
	});
