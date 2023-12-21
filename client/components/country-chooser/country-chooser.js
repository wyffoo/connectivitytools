'use strict';

angular.module('gigaApp.country-chooser', [])
	.directive('countryChooser', function ($translate) {
		return {
			scope: {
				step: '=',
				model: '='
			},
			templateUrl: 'components/country-chooser/country-chooser.html',
			restrict: 'EA',
			link: function (scope, element, attrs) {
			}
		};
	});
