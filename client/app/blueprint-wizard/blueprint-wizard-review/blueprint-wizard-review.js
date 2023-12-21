'use strict';

angular.module('gigaApp.blueprint-wizard')
	.config(function ($stateProvider) {
		$stateProvider
			.state('blueprint-wizard-review', {
				url: '/blueprint-wizard/review',
				template: '<blueprint-wizard-review></blueprint-wizard-review>',
				authenticate: 'admin',
				resolve: {
					loadCountries: function ($http, $stateParams) {
						// $http returns a promise for the url data
						return $http({method: 'GET', url: '/api/projects/countries'});
					}
				}
			});
	});
