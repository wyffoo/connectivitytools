'use strict';

angular.module('gigaApp')
	.directive('objectParamsForm', function () {
		return {
			scope: {
				items: '=',
				model: '='
			},
			templateUrl: 'app/object-params/components/object-params-form/object-params-form.html',
			restrict: 'EA',
			link: function (scope, element, attrs) {
				scope.title = attrs.title;
			}
		};
	});
