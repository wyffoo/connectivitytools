'use strict';

angular.module('gigaApp.school-wizard')
	.config(function ($stateProvider) {
		$stateProvider
			.state('school-wizard-broadband-initial', {
				url: '/school-wizard/broadband-initial',
				template: '<school-wizard-broadband-initial></school-wizard-broadband-initial>',
				resolve: {}
			})
			.state('school-wizard-broadband-bandwidth-selector', {
				url: '/school-wizard/broadband-bandwidth-selector',
				template: '<school-wizard-broadband-bandwidth-selector></school-wizard-broadband-bandwidth-selector>',
				resolve: {}
			})
			.state('school-wizard-broadband-extended-computers', {
				url: '/school-wizard/broadband-extended-computers',
				template: '<school-wizard-broadband-extended-computers></school-wizard-broadband-extended-computers>',
				resolve: {}
			})
			.state('school-wizard-broadband-extended-users', {
				url: '/school-wizard/broadband-extended-users',
				template: '<school-wizard-broadband-extended-users></school-wizard-broadband-extended-users>',
				resolve: {}
			})
			.state('school-wizard-broadband-location', {
				url: '/school-wizard/broadband-location',
				template: '<school-wizard-broadband-location></school-wizard-broadband-location>',
				resolve: {}
			})
			.state('school-wizard-broadband-required-bandwidth', {
				url: '/school-wizard/broadband-required-bandwidth',
				template: '<school-wizard-broadband-required-bandwidth></school-wizard-broadband-required-bandwidth>',
				resolve: {}
			})
			.state('school-wizard-broadband-tp', {
				url: '/school-wizard/broadband-tp',
				template: '<school-wizard-broadband-tp></school-wizard-broadband-tp>',
				resolve: {}
			})
			.state('school-wizard-broadband-users-simple', {
				url: '/school-wizard/broadband-users-simple',
				template: '<school-wizard-broadband-users-simple></school-wizard-broadband-users-simple>',
				resolve: {}
			});
	});
