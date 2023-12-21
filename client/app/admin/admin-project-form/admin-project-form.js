'use strict';

angular.module('gigaApp.admin')
	.config(function ($stateProvider) {
		$stateProvider
			.state('admin-project-form', {
				url: '/admin/project/form/:project',
				template: '<admin-project-form></admin-project-form>',
				authenticate: 'admin',
				resolve: {
					loadCountries: function ($http, $stateParams) {
						// $http returns a promise for the url data
						return $http({method: 'GET', url: '/api/projects/countries'});
					}
				}
			});
	});
