'use strict';

(function () {

	class MultischoolWizardBroadbandTpController {
		model = {
			tp_id: ""
		}

		profiles = [
		]

		constructor($scope, $stateParams, $state, Modal, $localStorage, $translate, $http) {
			this.scope = $scope;
			this.state = $state;
			this.http = $http;
			this.infoModal = Modal.info.simple();
			this.profiles = $scope.$parent.$resolve.loadDefaults.data;
			this.custom_profiles = $scope.$parent.$resolve.loadCustom.data;
			this.model = $localStorage.SchoolWizardModel;
			this.model.progress.active = 3;
			this.scope.show_custom = this.model.tp_id > 9;
			this.confirmCustomProfileDelete = Modal.confirm.delete((index) => {
				this.deleteCustomProfile(index);
			});
		}

		switchDefaultProfile(id) {
			this.model.tp_id = id;
			this.model.tp_ql = this.profiles[id][2];
			this.scope.show_custom = false;
		}

		switchCustomProfile(id) {
			this.model.tp_id = id;
			this.model.tp_ql = this.custom_profiles[id]['ql'];
			this.model.tp_name = this.custom_profiles[id]['name'];
			this.scope.show_custom = true;
		}

		editCustomProfile(id) {
			this.state.go('traffic-profile-form', {'profile_id':id});
		}

		deleteCustomProfile(id) {
			let tp = this.custom_profiles[id];
			this.http.delete('/api/tp/profile/'+id).then((response) => {
				if (response.data.status === 'success') {
					delete this.custom_profiles[id];
					delete this.model.tp_id;
					delete this.model.tp_ql;
					delete this.model.tp_name;
				}
			});
		}

		showCustomProfiles() {
			this.model.tp_id = null;
			this.model.tp_ql = null;
			this.scope.show_custom = true;
		}

		back() {
			this.state.go('multischool-wizard-broadband-bandwidth-selector');
		}

		continue() {
			if (!this.model.tp_id) {
				return;
			}
			this.model.progress.status[3] = 'completed';
			if (this.model.calculate_only_required_bandwidth === true) {
				this.model.is_lan_required = false;
				this.state.go('multischool-wizard-review');
			} else if (this.model.is_bandwidth_calc_required === 'provided') {
				this.state.go('multischool-wizard-lan-initial');
			} else {
				this.state.go('multischool-wizard-broadband-ct');
			}
		}
	}

	angular.module('gigaApp.multischool-wizard')
		.component('multischoolWizardBroadbandTp', {
			templateUrl: 'app/multischool-wizard/multischool-wizard-broadband/multischool-wizard-broadband-tp.html',
			controller: MultischoolWizardBroadbandTpController
		});

})();
