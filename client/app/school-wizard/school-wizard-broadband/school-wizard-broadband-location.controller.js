'use strict';

(function () {

	class SchoolWizardBroadbandLocation {
		model = {}

		constructor($scope, $stateParams, $state, $localStorage) {
			this.scope = $scope;
			this.state = $state;
			this.model = $localStorage.SchoolWizardModel;
		}

		previous() {
			this.state.go('');
		}

		gotoNext() {
			this.state.go('school-wizard-broadband-required-bandwidth');
		}
	}

	angular.module('gigaApp.school-wizard')
		.component('schoolWizardBroadbandLocation', {
			templateUrl: 'app/school-wizard/school-wizard-broadband/school-wizard-broadband-location.html',
			controller: SchoolWizardBroadbandLocation
		});

})();
