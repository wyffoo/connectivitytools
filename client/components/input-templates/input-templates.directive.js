'use strict';

angular.module('gigaApp.input-templates', [])
	.directive('inputTemplates', function (appConfig) {
		return {
			scope: {
				template: '=',
				project: '=',
				variant: '=',
				type: '<'
			},
			templateUrl: 'components/input-templates/input-templates.html',
			restrict: 'EA',
			link: function (scope, element, attrs) {
				scope.isCountriesMode = appConfig.isCountriesMode;
				scope.isSchoolsMode = appConfig.isSchoolsMode;
				scope.isGlobalMode = appConfig.isGlobalMode;
				scope.locations_type = 'city';
			}
		};
	});
