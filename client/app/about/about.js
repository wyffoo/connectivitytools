'use strict';

angular.module('gigaApp')
	.config(function ($stateProvider) {
		$stateProvider
			.state('about', {
				url: '/about',
				template: '<about></about>'
			});
	});
