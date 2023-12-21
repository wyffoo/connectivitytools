'use strict';

angular.module('gigaApp')
	.directive('countryPicker', function () {
		var list = [
			{id: '', name: ''}
		];
		return {
			templateUrl: 'components/country-picker/country-picker.html',
			restrict: 'EA',
			link: function () {
			}
		};
	});
