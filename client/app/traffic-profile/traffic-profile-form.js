'use strict';

(function () {

	class TrafficProfileFormController {
		constructor($scope, $stateParams, $state, $localStorage, $http, Modal) {
			this.scope = $scope;
			this.state = $state;
			this.http = $http;
			this.traffic_sources = $scope.$parent.$resolve.loadTrafficSources.data;
			this.services = $scope.$parent.$resolve.loadServices.data;
			let existingRecords = $scope.$parent.$resolve.loadRecords.data;

			this.profile_records = {};
			if ($stateParams.profile_id !== null) {
				let record = $scope.$parent.$resolve.loadProfile.data;
				//Existing profile
				this.model = {
					id: record.id,
					name: record.profile_name,
					quality_level: record.quality_level.toLowerCase()
				};
				//Prefill with not_used
				this.traffic_sources.forEach((ts)=>{
					this.services.forEach((s)=>{
						this.profile_records[ts[0]+'.'+s.id] = [ts[0],s.id,'notused',s.defaultset];
					})
				});
				existingRecords.forEach((r)=>{
					this.profile_records[r.ts_id+'.'+r.s_id] = [r.ts_id,r.s_id,r.level.toLowerCase(),r.defaultset];
				});
				console.log(this.profile_records);
			} else {
				//New profile
				this.model = {
					quality_level: "medium"
				};
				//Prefill with not_used
				this.traffic_sources.forEach((ts)=>{
					this.services.forEach((s)=>{
						this.profile_records[ts[0]+'.'+s.id] = [ts[0],s.id,'notused',s.defaultset];
					})
				});
			}

			this.scope.showForm = true;
			this.scope.ts_accordion_status = {};
			this.scope.custom_services_extended = false;
			this.scope.include_default_services = true;
			this.scope.new_service = {};
			this.confirmServiceDelete = Modal.confirm.delete((index) => {
				this.deleteService(index);
			});
		}

		setServiceUsingLevel(traffic_source_id, service_id, level) {
			if (service_id === null) {//Set for every service
				this.services.forEach((s)=>{
					this.profile_records[traffic_source_id+'.'+s.id][2] = level;
				})
			} else {
				this.profile_records[traffic_source_id + '.' + service_id][2] = level;
			}
		}

		deleteService(index) {
			let service = this.services[index];
			this.http.delete('/api/tp/service/'+service.id).then((response) => {
				if (response.data.status === 'success') {
					this.traffic_sources.forEach((ts)=> {
						delete this.profile_records[ts[0] + '.' + service.id];
					});
					this.services.splice(index,1);
				}
			});
		}

		serviceSync(index) {
			let service = this.services[index];
			if (!this.scope.custom_services_extended) {
				//Populate medium into low and high
				service.latency_low = service.latency_high = service.latency_medium;
				service.bitrate_low = service.bitrate_high = service.bitrate_medium;
				service.intensity_low = service.intensity_high = service.intensity_medium;
				service.datavolume_low = service.datavolume_high = service.datavolume_medium;
			}
			this.http.put('/api/tp/service', service).then((response) => {
				if (response.data.status === 'success') {

				}
			});
		}

		saveNewService() {
			let new_service = this.scope.new_service;
			new_service.defaultset = 0;
			if (!this.scope.custom_services_extended) {
				//Populate medium into low and high
				new_service.latency_low = new_service.latency_high = new_service.latency_medium;
				new_service.bitrate_low = new_service.bitrate_high = new_service.bitrate_medium;
				new_service.intensity_low = new_service.intensity_high = new_service.intensity_medium;
				new_service.datavolume_low = new_service.datavolume_high = new_service.datavolume_medium;
			}
			//Save on server, get ID in return
			this.http.post('/api/tp/service', new_service).then((response) => {
				if (response.data.status === 'success' && response.data.service_id) {
					new_service.id = response.data.service_id;
					this.services.push(new_service);
					this.scope.new_service = {};
					this.traffic_sources.forEach((ts)=> {
						this.profile_records[ts[0] + '.' + response.data.service_id] = [ts[0], response.data.service_id, 'notused', new_service.defaultset];
					});
				}
			});
		}

		saveProfile() {
			console.log(this.model.id);
			if (this.model.id) {
				//Existing profile
				this.http.put('/api/tp/profile/'+this.model.id, {
					'name': this.model.name,
					'quality_level': this.model.quality_level,
					'records': this.profile_records,
					'include_default_services': this.scope.include_default_services
				}).then((response) => {
					if (response.data.status === 'success' && response.data.profile_id) {
						this.state.go('multischool-wizard-broadband-tp', {profile_id: response.data.profile_id});
					}
				});
			} else {
				//New profile
				this.http.post('/api/tp/profile', {
					'name': this.model.name,
					'quality_level': this.model.quality_level,
					'records': this.profile_records,
					'include_default_services': this.scope.include_default_services
				}).then((response) => {
					if (response.data.status === 'success' && response.data.profile_id) {
						this.state.go('multischool-wizard-broadband-tp', {profile_id: response.data.profile_id});
					}
				});
			}
		}

		// nav buttons
		back() {
			this.state.go('multischool-wizard-broadband-tp');
		}
	}

	angular.module('gigaApp.traffic-profile')
		.component('trafficProfileForm', {
			templateUrl: 'app/traffic-profile/traffic-profile-form.html',
			controller: TrafficProfileFormController
		});

})();
