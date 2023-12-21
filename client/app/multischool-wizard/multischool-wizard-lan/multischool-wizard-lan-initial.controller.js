'use strict';

(function () {

	class MultischoolWizardLanInitialController {
		constructor($scope, $stateParams, $state, $localStorage, $translate) {
			this.model = $localStorage.SchoolWizardModel;
			this.model.progress.active = 5;
			this.scope = $scope;
			this.state = $state;
			this.model.progress[5] = 'active';
			this.translations = {};
			$translate([
				'multi-lan-initial.modal1_title',
				'multi-lan-initial.modal1_body',
			]).then((result)=>{
				this.translations = result;
			});
		}

		modal(num) {
			this.infoModal(this.translations['multi-lan-initial.modal'+num+'_title'],
				this.translations['multi-lan-initial.modal'+num+'_body']);
		}

		calculate() {
			this.model.progress.status[5] = 'completed';
			this.model.progress.status[6] = 'pristine';
			this.model.is_lan_required = true;
			this.state.go('multischool-wizard-lan-configuration');
		}

		skip() {
			this.model.progress.status[5] = 'partial';
			this.model.progress.status[6] = 'completed';
			this.model.is_lan_required = false;
			this.model.lan_config = null;
			this.state.go('multischool-wizard-review');
		}

		// nav buttons
		back() {
			if (this.model.is_broadband_required === false) {
				this.state.go('multischool-wizard-broadband-initial');
			} else if (this.model.is_bandwidth_calc_required === 'provided' || this.model.is_bandwidth_calc_required === 'calculate_with_tech') {
				if (this.model.is_net_in_output === true) {
					this.state.go('multischool-wizard-broadband-ct');
				} else {
					this.state.go('multischool-wizard-broadband-initial');
				}
			} else {
				this.state.go('multischool-wizard-broadband-tp');
			}
		}
	}

	angular.module('gigaApp.multischool-wizard')
		.component('multischoolWizardLanInitial', {
			templateUrl: 'app/multischool-wizard/multischool-wizard-lan/multischool-wizard-lan-initial.html',
			controller: MultischoolWizardLanInitialController
		});

})();
