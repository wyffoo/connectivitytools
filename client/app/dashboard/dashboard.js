'use strict';

angular.module('gigaApp')
	.config(function ($stateProvider) {
		$stateProvider
			.state('dashboard', {
				url: '/dashboard',
				template: '<dashboard></dashboard>',
				authenticate: true
			});
	});
