'use strict';

angular.module('gigaApp.project-review', [])
	.directive('projectReview', function ($translate) {
		return {
			scope: {
				project: '=',
				settings: '=',
				countries: '='
			},
			templateUrl: 'components/project-review/project-review.html',
			restrict: 'EA',
			link: function (scope, element, attrs) {
				scope.countries_index = {};
				scope.countries.forEach((item)=>{
					scope.countries_index[item.id] = item.country_name;
				});
			}
		};
	});
