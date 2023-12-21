'use strict';

(function () {

	class SchoolWizardLanExtendedUsers {
		model = {

		}

		constructor($scope, $stateParams, $state, $localStorage) {
			this.scope = $scope;
			this.state = $state;
			this.model = $localStorage.SchoolWizardModel;
		}

		previous() {
			this.state.go('school-wizard-lan-users-simple');
		}

		submit() {
			this.state.go('');
		}

		gotoBuild(){
			this.state.go('school-wizard-lan-buildings');
		};
	}

	angular.module('gigaApp.school-wizard')
		.component('schoolWizardLanExtendedUsers', {
			templateUrl: 'app/school-wizard/school-wizard-lan/school-wizard-lan-extended-users.html',
			controller: SchoolWizardLanExtendedUsers
		})

})();
