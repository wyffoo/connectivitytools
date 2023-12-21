'use strict';

angular.module('gigaApp.bottom-buttons', [])
	.directive('bottomButtons', function ($translate) {
		return {
			scope: {
				btnLeft:'&',
				btnRight:'&'
			},
			templateUrl: 'components/bottom-buttons/bottom-buttons.html',
			restrict: 'EA',
			link: function (scope, element, attrs) {
				scope.hideLeft = typeof attrs.hideLeft !== 'undefined' && attrs.hideLeft==='yes';
				scope.hideRight = typeof attrs.hideRight !== 'undefined' && attrs.hideRight==='yes';
			}
		};

	});
