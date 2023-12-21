'use strict';

angular.module('gigaApp')
	.config(function ($stateProvider) {
		$stateProvider
			.state('main', {
				url: '/',
				template: '<main></main>'
			})
			.state('request-blueprint', {
				url: '/request-blueprint',
				template: '<request-blueprint></request-blueprint>'
			});
	});
