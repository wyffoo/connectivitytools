'use strict';

angular.module('gigaApp')
	.controller('OauthButtonsCtrl', function ($window) {
		this.loginOauth = function (provider) {
			$window.location.href = '/api/auth/' + provider;
		};
	});
