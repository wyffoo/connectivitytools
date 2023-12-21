'use strict';

(function () {

	class SchoolWizardLanLp {
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
	}

	angular.module('gigaApp.school-wizard')
		.component('schoolWizardLanLp', {
			templateUrl: 'app/school-wizard/school-wizard-lan/school-wizard-lan-lp.html',
			controller: SchoolWizardLanLp
		})

})();
