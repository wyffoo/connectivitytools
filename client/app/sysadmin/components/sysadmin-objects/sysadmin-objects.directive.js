'use strict';

angular.module('gigaApp')
	.directive('sysadminObjects', function () {
		return {
			scope: true,
			templateUrl: 'app/sysadmin/components/sysadmin-objects/sysadmin-objects.html',
			restrict: 'EA',
			link: function (scope, element, attrs) {
				scope.grp = attrs.group;
			}
		};
	});
