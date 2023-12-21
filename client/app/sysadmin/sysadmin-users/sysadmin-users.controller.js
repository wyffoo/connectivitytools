'use strict';

(function () {

	class SysadminUsersComponent {
		//start-non-standard
		users = [];
		totalItems = 0;
		currentPage = 0;
		//end-non-standard

		constructor($scope, Modal, $http, $state, appConfig) {
			this.modal = Modal;
			this.http = $http;
			this.state = $state;
			this.users = $scope.$parent.$resolve.loadUsers.data;
			this.countries = $scope.$parent.$resolve.loadCountries.data;
			this.confirmDelete = Modal.confirm.delete((user) => {
				this.delete(user);
			});
			this.confirmBlock = Modal.confirm.simple((user) => {
				this.block(user);
			});
			this.confirmUnblock = Modal.confirm.simple((user) => {
				this.unblock(user);
			});
			$scope.isCountriesMode = appConfig.isCountriesMode;
			if (appConfig.isCountriesMode) {
				this.users.forEach((item)=>{
					$scope.$on('ucc'+item.id, (event, data) => {
						this.updateAssignedCountries(item.id, data.val);
					});
				});
			}
		}

		updateAssignedCountries(user_id, countries) {
			this.http({
				url: '/api/users/countries',
				method: 'POST',
				data: { user_id: user_id, countries: countries }
			}).then((resp) => {
				console.log(resp.data.status);
				if (resp.data && resp.data.status !== 'success') {
					this.state.reload();
				}
			}).catch(function(error) {
			})
		}

		saveCountries(technology, params) {
			//TODO: send params to server
			if (angular.isArray(params)) {
				//Multi-technology params
				console.log('save params', params);
			} else {
				console.log('save params', params);
				return this.http({method: 'GET', url: '/'});
			}
		}

		block(user) {
			this.http({
				url: '/api/users/status',
				method: 'POST',
				data: { user_id: user.id, status: 'suspended' }
			}).then((resp) => {
				if (resp.data && resp.data.status !== 'success') {
					this.state.reload();
				} else {
					user.status = 'suspended';
				}
			}).catch(function(error) {
			});
		}

		unblock(user) {
			this.http({
				url: '/api/users/status',
				method: 'POST',
				data: { user_id: user.id, status: 'active' }
			}).then((resp) => {
				if (resp.data && resp.data.status !== 'success') {
					this.state.reload();
				} else {
					user.status = 'active';
				}
			}).catch(function(error) {
			});
		}

	}

	angular.module('gigaApp.sysadmin')
		.component('sysadminUsers', {
			templateUrl: 'app/sysadmin/sysadmin-users/sysadmin-users.html',
			controller: SysadminUsersComponent
		});

})();
