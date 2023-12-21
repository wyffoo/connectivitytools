'use strict';

(function () {

	class MultischoolWizardBroadbandBandwidthSelectorController {
		model = {

		}

		constructor($scope, $stateParams, $state, Modal, $localStorage, $translate) {
			this.scope = $scope;
			this.state = $state;
			this.infoModal = Modal.info.simple();
			this.model = $localStorage.SchoolWizardModel;
			this.model.progress.active = 2;
			this.model.net_in_output_techs = [];
			this.translations = {};
			$translate([
				'multi-bandwidth-selector.modal1_title',
				'multi-bandwidth-selector.modal1_body',
				'multi-bandwidth-selector.modal2_title',
				'multi-bandwidth-selector.modal2_body',
				'multi-bandwidth-selector.modal3_title',
				'multi-bandwidth-selector.modal3_body'
			]).then((result)=>{
				this.translations = result;
			});
		}

		modal(num) {
			this.infoModal(this.translations['multi-bandwidth-selector.modal'+num+'_title'],
				this.translations['multi-bandwidth-selector.modal'+num+'_body']);
		}

		back() {
			this.state.go('multischool-wizard-broadband-initial');
		}

		knowBandwidth() {
			delete this.model.tp_id;
			delete this.model.tp_ql;
			delete this.model.tp_name;
			this.model.is_bandwidth_calc_required = 'provided';
			this.model.progress.status[2] = 'completed';
			this.model.progress.status[4] = 'pristine';
			this.state.go('multischool-wizard-broadband-ct');
		}

		calculateBandwidth() {
			this.model.progress.status[2] = 'completed';
			this.model.progress.status[4] = 'pristine';
			this.model.is_bandwidth_calc_required = 'calculate_with_tech';
			this.model.calculate_only_required_bandwidth = false;
			this.state.go('multischool-wizard-broadband-tp');
		}

		calculateOnlyBandwidth() {
			this.model.progress.status[2] = 'completed';
			this.model.progress.status[4] = 'completed';
			//Skip technologies and optimization
			this.model.is_bandwidth_calc_required = 'calculate';
			this.model.calculate_only_required_bandwidth = true;
			this.model.tp_id = null;
			this.state.go('multischool-wizard-broadband-tp');
		}

	}

	angular.module('gigaApp.multischool-wizard')
		.component('multischoolWizardBroadbandBandwidthSelector', {
			templateUrl: 'app/multischool-wizard/multischool-wizard-broadband/multischool-wizard-broadband-bandwidth-selector.html',
			controller: MultischoolWizardBroadbandBandwidthSelectorController
		});

})();
