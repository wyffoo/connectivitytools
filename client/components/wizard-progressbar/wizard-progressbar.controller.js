'use strict';

class WizardProgressbarController {
	//start-non-standard
	//end-non-standard

	constructor($scope, $translate) {
		let t = {};
		$translate([
			'progress-bar.s1',
			'progress-bar.s2',
			'progress-bar.s3',
			'progress-bar.s4',
			'progress-bar.s5',
			'progress-bar.s6',
			'progress-bar.s7',
			'progress-bar.s8'
		]).then((result) => {
			t = result;
			let w = 100/8;
			//Types: 'active','pristine', 'partial', 'completed'
			$scope.sections = [
				{
					value: w, type: 'pristine', popover: t['progress-bar.s1']
				},
				{
					value: w, type: 'pristine', popover: t['progress-bar.s2']
				},
				{
					value: w, type: 'pristine', popover: t['progress-bar.s3']
				},
				{
					value: w, type: 'pristine', popover: t['progress-bar.s4']
				},
				{
					value: w, type: 'pristine', popover: t['progress-bar.s5']
				},
				{
					value: w, type: 'pristine', popover: t['progress-bar.s6']
				},
				{
					value: w, type: 'pristine', popover: t['progress-bar.s7']
				},
				{
					value: w, type: 'pristine', popover: t['progress-bar.s8']
				},
			];
			$scope.$watch('state.status',()=>{
				//console.log($scope.state);
				for (let i=0,size=$scope.state.status.length;i<size;i++) {
					$scope.sections[i].type = $scope.state.status[i];
				}
				$scope.sections[$scope.state.active].type = 'active';
			});
		});
	}

}

angular.module('gigaApp')
	.controller('WizardProgressbarController', WizardProgressbarController);
