'use strict';

(function () {

	class SchoolWizardLanBuildings {
		model = {}

		constructor($scope, $stateParams, $state, $localStorage) {
			this.scope = $scope;
			this.state = $state;
			this.model = $localStorage.SchoolWizardModel;
		}

		previous() {
			this.state.go('');
		}

		submit() {
			this.state.go('');
		}

		gotoNext() {
			this.state.go('school-wizard-lan-lp')
		}
	}

	angular.module('gigaApp.school-wizard')
		.component('schoolWizardLanBuildings', {
			templateUrl: 'app/school-wizard/school-wizard-lan/school-wizard-lan-buildings.html',
			controller: SchoolWizardLanBuildings
		})

})();
