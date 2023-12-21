'use strict';

angular.module('gigaApp.lan-configuration-visualization', [])
	.directive('lanConfigurationVisualization', function ($translate) {
		return {
			scope: {
				segments: '='
			},
			templateUrl: 'components/lan-configuration-visualization/lan-configuration-visualization.html',
			restrict: 'EA',
			link: function (scope, element, attrs) {
				scope.$watch('segments', (state)=>{
					let lang = $translate.use();
					console.log(state);
					if (state.wired && !state.wireless && !state.public_hotspot) {
						scope.state_class = 'lnsv-1-'+lang;
					} else if (!state.wired && state.wireless && !state.public_hotspot) {
						scope.state_class = 'lnsv-2-'+lang;
					} else if (!state.wired && !state.wireless && state.public_hotspot) {
						scope.state_class = 'lnsv-3-'+lang;
					} else if (state.wired && state.wireless && !state.public_hotspot) {
						scope.state_class = 'lnsv-1-2-'+lang;
					} else if (state.wired && !state.wireless && state.public_hotspot) {
						scope.state_class = 'lnsv-1-3-'+lang;
					} else if (!state.wired && state.wireless && state.public_hotspot) {
						scope.state_class = 'lnsv-2-3-'+lang;
					} else if (state.wired && state.wireless && state.public_hotspot) {
						scope.state_class = 'lnsv-1-2-3-'+lang;
					} else {
						scope.state_class = 'lnsv-clear-'+lang;
					}

				}, true);
			}
		};
	});
