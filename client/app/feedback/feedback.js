'use strict';

angular.module('gigaApp')
	.config(function ($stateProvider) {
		$stateProvider
			.state('feedback', {
				url: '/feedback',
				template: '<feedback></feedback>'
			});
	});
