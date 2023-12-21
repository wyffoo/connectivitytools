'use strict';

angular.module('gigaApp.wizard-header', [])
	.directive('wizardHeader', function ($translate, appConfig) {
		return {
			scope: {
				step: '=',
				hidesub: '=',
				progressbar: '='
			},
			templateUrl: 'components/wizard-header/wizard-header.html',
			restrict: 'EA',
			link: function (scope, element, attrs) {
				scope.isCountriesMode = appConfig.isCountriesMode;
				scope.isSchoolsMode = appConfig.isSchoolsMode;
				scope.isGlobalMode = appConfig.isGlobalMode;
			}
		};
	});
