'use strict';

angular.module('gigaApp')
	.directive('footer', function () {
		return {
			templateUrl: 'components/footer/footer.html',
			restrict: 'E',
			link: function (scope, element) {
				element.addClass('footer');
			}
		};
	});
