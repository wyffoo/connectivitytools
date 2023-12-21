'use strict';

angular.module('gigaApp.blueprint-wizard')
	.config(function ($stateProvider) {
		$stateProvider
			.state('blueprint-wizard-settings', {
				url: '/blueprint-wizard/settings',
				template: '<blueprint-wizard-settings></blueprint-wizard-settings>',
				authenticate: 'admin',
				resolve: {},
			})
	});
