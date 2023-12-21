'use strict';

(function () {

	class SysadminLocationsListComponent {
		//start-non-standard
		locations = [];
		//end-non-standard

		constructor(Modal, $scope, $state, LocationsResource, $translate, $http, Upload) {
			this.translations = {};
			this.state = $state;
			this.scope = $scope;
			this.upload = Upload;
			$translate([]).then((result)=>{
				this.translations = result;
			});
			$scope.location_type = 'city';
			this.countries = $scope.$parent.$resolve.loadCountries.data;
			$scope.params = {};
			$scope.limit = 15;
			this.locations = [];
			let ctrl = this;
			let getData = () => {
				$http.get('/api/locations', {
					params: {
						'type': $scope.location_type,
						'offset': $scope.params.offset || 0,
						'limit': $scope.limit,
						'order_by': $scope.params.sortBy,
						'country_id': $scope.country_id,
						'search_by': $scope.params.searchBy
					}
				}).then(function(ret) {
					$scope.params.total = ret.data.meta.total;
					ctrl.locations = ret.data.data;
				});
			};

			// Watch for changes, then load data
			//getData();
			$scope.$watch('params.offset', getData);
			$scope.$watch('params.sortBy', getData, true);
			$scope.$watch('location_type', getData, true);
			$scope.$watch('country_id', getData, true);
			this.confirmDuplicate = Modal.confirm.simple((project) => {
				this.duplicateProject(project);
			});
			this.confirmDelete = Modal.confirm.delete((project) => {
				this.delete(project);
			});
		}

		uploadDatasetFile(file, errFiles) {
			this.file = file;
			this.errFile = errFiles && errFiles[0];
			if (file) {
				file.upload = this.upload.upload({
					url: '/api/locations/dataset/'+this.scope.country_id+'/'+this.scope.location_type,
					data: {file: file},
					method: 'POST'
				});

				file.upload.then((response) => {
					setTimeout(() => {
						file.result = response.data;
					});
				}, (response) => {
					if (response.status > 0)
						this.errorMsg = response.status + ': ' + response.data;
				}, (evt) => {
					file.progress = Math.min(100, parseInt(100.0 *
						evt.loaded / evt.total));
					if (file.progress === 100) {
						if (this.uploadDatasetFile.timeout) {
							clearTimeout(this.uploadDatasetFile.timeout);
						}
						this.uploadDatasetFile.timeout = setTimeout(() => {
							this.uploadDatasetFile.timeout = null;
							this.state.reload();
						}, 1000);
					}
				});
			}
		}

		duplicateProject(project) {
			this.locationsResource.duplicate({id:project.id}, ()=>{
				this.state.reload();
			});
		}

		calculate(project) {
			this.locationsResource.calculate({id:project.id}, ()=>{
				this.state.reload();
			});
		}

		downloadXLSX(project) {
			this.locationsResource.downloadXLSX({id:project.id});
		}

		delete(project) {
			this.locationsResource.delete({id:project.id}, ()=>{
				this.locations.splice(this.locations.indexOf(project), 1);
			});
		}
	}

	angular.module('gigaApp.sysadmin')
		.component('sysadminLocationsList', {
			templateUrl: 'app/sysadmin/sysadmin-locations-list/sysadmin-locations-list.html',
			controller: SysadminLocationsListComponent
		});

})();
