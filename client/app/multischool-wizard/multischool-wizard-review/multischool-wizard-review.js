'use strict';

angular.module('gigaApp.multischool-wizard')
	.config(function ($stateProvider) {
		$stateProvider
			.state('multischool-wizard-review', {
				url: '/multischool-wizard/review',
				template: '<multischool-wizard-review></multischool-wizard-review>',
				authenticate: 'admin',
				resolve: {
					loadCountries: function ($http, $stateParams) {
						// $http returns a promise for the url data
						return $http({method: 'GET', url: '/api/projects/countries'});
					},
					loadTemplateInfo: function ($http, $localStorage) {
						let model = $localStorage.SchoolWizardModel;
						let template_flags = (model.is_broadband_required?'1':'0') +','+(model.is_bandwidth_calc_required && model.is_bandwidth_calc_required!=='provided'?'1':'0')
							+','+(model.is_lan_required?'1':'0');
						return $http({method: 'GET', url: '/api/projects/input_templates/'+template_flags})
					}
				}
			});
	});
