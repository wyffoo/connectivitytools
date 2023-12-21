'use strict';

(function () {

	class SchoolWizardBroadbandInitialController {
		model = {}

		constructor($scope, $stateParams, $state, Modal, $localStorage) {
			this.scope = $scope;
			this.state = $state;
			this.model = $localStorage.SchoolWizardModel;
			this.infoModal = Modal.info.simple();
		}

		modalTest() {
			this.infoModal('Title','<strong>Description</strong>');
		}

		open() {

		}

		calculate() {
			this.state.go('school-wizard-broadband-bandwidth-selector');
		}

		skip() {
			this.state.go('school-wizard-lan-initial');
		}

		// nav buttons
		continue() {
			this.state.go('multischool-wizard-broadband-bandwidth-selector');
		}

		back() {
			this.state.go('multischool-wizard-broadband-initial');
		}
	}

	angular.module('gigaApp.school-wizard')
		.component('schoolWizardBroadbandInitial', {
			templateUrl: 'app/school-wizard/school-wizard-broadband/school-wizard-broadband-initial.html',
			controller: SchoolWizardBroadbandInitialController
		});

})();
