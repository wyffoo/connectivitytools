'use strict';

angular.module('gigaApp.admin')
	.config(function ($stateProvider) {
		$stateProvider
			.state('admin-blueprint-project-form', {
				url: '/admin/blueprint-project/form/:project',
				template: '<admin-blueprint-project-form></admin-blueprint-project-form>',
				authenticate: 'admin',
				resolve: {
					loadCountries: function ($http, $stateParams) {
						// $http returns a promise for the url data
						return $http({method: 'GET', url: '/api/projects/my_countries'});
					}
				}
			});
	});
