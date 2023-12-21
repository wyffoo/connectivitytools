'use strict';

angular.module('gigaApp')
	.directive('navbar', () => ({
		templateUrl: 'components/navbar/navbar.html',
		restrict: 'E',
		controller: 'NavbarController',
		controllerAs: 'nav'
	}));
