'use strict';

angular.module('gigaApp.multischool-wizard')
	.config(function ($stateProvider) {
		$stateProvider
			.state('multischool-wizard-lan-initial', {
				url: '/multischool-wizard/lan-initial',
				template: '<multischool-wizard-lan-initial></multischool-wizard-lan-initial>',
				authenticate: 'admin',
				resolve: {}
			})
			.state('multischool-wizard-lan-configuration', {
				url: '/multischool-wizard/lan-configuration',
				template: '<multischool-wizard-lan-configuration></multischool-wizard-lan-configuration>',
				authenticate: 'admin',
				resolve: {}
			})
			.state('multischool-wizard-lan-users-simple', {
				url: '/multischool-wizard/multischool-wizard-lan-users-simple',
				template: '<multischool-wizard-lan-users-simple></multischool-wizard-lan-users-simple>',
				authenticate: 'admin',
				resolve: {}
			})
			// .state('multischool-wizard-lan-extended-users', {
			// 	url: '/multischool-wizard/lan-extended-users',
			// 	template: '<multischool-wizard-lan-extended-users></multischool-wizard-lan-extended-users>',
			// 	resolve: {}
			// })
			// .state('multischool-wizard-lan-buildings', {
			// 	url: '/multischool-wizard/lan-buildings',
			// 	template: '<multischool-wizard-lan-buildings></multischool-wizard-lan-buildings>',
			// 	resolve: {}
			// })
			// .state('multischool-wizard-lan-lp', {
			// 	url: '/multischool-wizard/lan-learning-process',
			// 	template: '<multischool-wizard-lan-lp></multischool-wizard-lan-lp>',
			// 	resolve: {}
			// });
	});
