'use strict';

(function () {

	class SchoolWizardBroadbandUsersSimple {
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
		gotoUsrExt(){
			this.state.go('school-wizard-broadband-extended-users');
		}

		gotoNext(){
			this.state.go('school-wizard-broadband-extended-computers');
		};
	}

	angular.module('gigaApp.school-wizard')
		.component('schoolWizardBroadbandUsersSimple', {
			templateUrl: 'app/school-wizard/school-wizard-broadband/school-wizard-broadband-users-simple.html',
			controller: SchoolWizardBroadbandUsersSimple
		});

})();
