'use strict';

(function () {

	class SchoolWizardBroadbandExtendedComputers {
		model = {}

		constructor($scope, $stateParams, $state, $localStorage) {
			this.scope = $scope;
			this.state = $state;
			this.model = $localStorage.SchoolWizardModel;
		}

		previous() {
			this.state.go('school-wizard-broadband-location');
		}

		submit() {
			this.state.go('');
		}

		gotoNext(){
			this.state.go('school-wizard-broadband-location');
		};
	}

	angular.module('gigaApp.school-wizard')
		.component('schoolWizardBroadbandExtendedComputers', {
			templateUrl: 'app/school-wizard/school-wizard-broadband/school-wizard-broadband-extended-computers.html',
			controller: SchoolWizardBroadbandExtendedComputers
		});

})();
