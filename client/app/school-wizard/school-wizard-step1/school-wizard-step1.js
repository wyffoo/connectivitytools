'use strict';

angular.module('gigaApp.school-wizard')
	.config(function ($stateProvider) {
		$stateProvider
			.state('school-wizard-step1', {
				url: '/school-wizard/step1',
				template: '<school-wizard-step1></school-wizard-step1>',
				resolve: {
					loadCountries: function ($http, $stateParams) {
						// $http returns a promise for the url data
						return $http({method: 'GET', url: '/api/projects/countries'});
					}
				}
			});
	});
