'use strict';

angular.module('gigaApp.multischool-wizard')
	.config(function ($stateProvider) {
		$stateProvider
			.state('multischool-wizard-broadband-initial', {
				url: '/multischool-wizard/broadband-initial',
				template: '<multischool-wizard-broadband-initial></multischool-wizard-broadband-initial>',
				authenticate: 'admin',
				resolve: {},
			})
			.state('multischool-wizard-broadband-bandwidth-selector', {
				url: '/multischool-wizard/broadband-bandwidth-selector',
				template: '<multischool-wizard-broadband-bandwidth-selector></multischool-wizard-broadband-bandwidth-selector>',
				authenticate: 'admin',
				resolve: {}
			})
			.state('multischool-wizard-broadband-ct', {
				url: '/multischool-wizard/broadband-ct',
				template: '<multischool-wizard-broadband-ct></multischool-wizard-broadband-ct>',
				authenticate: 'admin',
				resolve: {}
			})
			.state('multischool-wizard-broadband-tp', {
				url: '/multischool-wizard/broadband-tp',
				template: '<multischool-wizard-broadband-tp></multischool-wizard-broadband-tp>',
				authenticate: 'admin',
				resolve: {
					loadDefaults: function ($http, $stateParams) {
						return $http({method: 'GET', url: '/api/tp/default'});
					},
					loadCustom: function ($http, $stateParams) {
						return $http({method: 'GET', url: '/api/tp/custom'});
					}
				}
			})

	});
