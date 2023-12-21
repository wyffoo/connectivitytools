'use strict';

angular.module('gigaApp')
	.directive('wizardProgressbar', () => ({
		scope: {
			state: '=',
		},
		templateUrl: 'components/wizard-progressbar/wizard-progressbar.html',
		restrict: 'E',
		controller: 'WizardProgressbarController',
		controllerAs: 'nav'
	}));
