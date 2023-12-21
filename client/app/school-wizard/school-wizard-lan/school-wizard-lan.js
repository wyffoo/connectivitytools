'use strict';

angular.module('gigaApp.school-wizard')
	.config(function ($stateProvider) {
		$stateProvider
			.state('school-wizard-lan-initial', {
				url: '/school-wizard/lan-initial',
				template: '<school-wizard-lan-initial></school-wizard-lan-initial>',
				resolve: {}
			})
			.state('school-wizard-lan-configuration', {
				url: '/school-wizard/lan-configuration',
				template: '<school-wizard-lan-configuration></school-wizard-lan-configuration>',
				resolve: {}
			})
			.state('school-wizard-lan-users-simple', {
				url: '/school-wizard/school-wizard-lan-users-simple',
				template: '<school-wizard-lan-users-simple></school-wizard-lan-users-simple>',
				resolve: {}
			})
			.state('school-wizard-lan-extended-users', {
				url: '/school-wizard/lan-extended-users',
				template: '<school-wizard-lan-extended-users></school-wizard-lan-extended-users>',
				resolve: {}
			})
			.state('school-wizard-lan-buildings', {
				url: '/school-wizard/lan-buildings',
				template: '<school-wizard-lan-buildings></school-wizard-lan-buildings>',
				resolve: {}
			})
			.state('school-wizard-lan-lp', {
				url: '/school-wizard/lan-learning-process',
				template: '<school-wizard-lan-lp></school-wizard-lan-lp>',
				resolve: {}
			});
	});
