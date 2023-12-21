'use strict';

(function () {

	class SchoolWizardBroadbandRequiredBandwidthController {
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
		.component('schoolWizardBroadbandRequiredBandwidth', {
			templateUrl: 'app/school-wizard/school-wizard-broadband/school-wizard-broadband-required-bandwidth.html',
			controller: SchoolWizardBroadbandRequiredBandwidthController
		});

})();
