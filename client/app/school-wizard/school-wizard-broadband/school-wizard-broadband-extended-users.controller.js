'use strict';

(function () {

	class SchoolWizardBroadbandExtendedUsers {
		model = {}

		constructor($scope, $stateParams, $state, $localStorage) {
			this.scope = $scope;
			this.state = $state;
			this.model = $localStorage.SchoolWizardModel;
		}

		previous() {
			this.state.go('school-wizard-broadband-users-simple');
		}

		submit() {
			this.state.go('');
		}

		gotoComp(){
			this.state.go('school-wizard-broadband-extended-computers');
		};
	}

	angular.module('gigaApp.school-wizard')
		.component('schoolWizardBroadbandExtendedUsers', {
			templateUrl: 'app/school-wizard/school-wizard-broadband/school-wizard-broadband-extended-users.html',
			controller: SchoolWizardBroadbandExtendedUsers
		})

})();
