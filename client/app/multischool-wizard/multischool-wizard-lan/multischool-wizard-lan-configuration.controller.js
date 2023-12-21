'use strict';

(function () {

	class MultischoolWizardLanConfigurationController {
		model = {
			lan_conf: {wired: false, wireless: false, public_hotspot: false}
		}

		constructor($scope, $stateParams, $state, $localStorage) {
			this.scope = $scope;
			this.state = $state;
			this.model = $localStorage.SchoolWizardModel;
			this.model.progress.active = 6;
			if (typeof this.model.lan_config  === 'undefined') {
				this.model.lan_config = {wired: false, wireless: false, public_hotspot: false};
			}
		}

		back() {
			this.state.go('multischool-wizard-lan-initial');
		}

		next() {
			if (this.model.lan_config.wired === false && this.model.lan_config.wireless === false && this.model.lan_config.public_hotspot === false) {
				return;
			}
			this.model.progress.status[6] = 'completed';
			this.model.is_lan_required = true;
			this.state.go('multischool-wizard-review');
		}
	}

	angular.module('gigaApp.multischool-wizard')
		.component('multischoolWizardLanConfiguration', {
			templateUrl: 'app/multischool-wizard/multischool-wizard-lan/multischool-wizard-lan-configuration.html',
			controller: MultischoolWizardLanConfigurationController
		});

})();
