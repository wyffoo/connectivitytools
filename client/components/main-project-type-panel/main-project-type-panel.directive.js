'use strict';

angular.module('gigaApp.main-project-type-panel', [])
	.directive('mainProjectTypePanel', function () {
		let types = {
			1: {
				name: 'CONNECT ONE SCHOOL',
				desc: '',
				imgSrc: '/assets/images/n4.gif'
			},
			2: {
				name: 'CONNECT MULTIPLE SCHOOLS',
				desc: '',
				imgSrc: '/assets/images/n3.gif'
			},
			3: {
				name: 'CALCULATE BLUEPRINT',
				desc: '',
				imgSrc: '/assets/images/11.png'
			}
		};

		return {
			scope: {},
			templateUrl: 'components/main-project-type-panel/main-project-type-panel.html',
			restrict: 'EA',
			link: function (scope, element, attrs) {
				if (attrs.size === 'medium') {
					scope.width = '300px';
					scope.height = '300px';
				}
				scope.additionalText = attrs.additionalText;
				scope.level = attrs.type > 0 && attrs.type <= 2 ? types[attrs.type] : 0;
			}
		};
	});
