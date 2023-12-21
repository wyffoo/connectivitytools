'use strict';

(function () {

	class MainController {

		constructor(Auth, User, $state, $localStorage, appConfig) {
			this.auth = Auth;
			this.isLoggedIn = Auth.isLoggedIn;
			this.state = $state;
			this.isCountriesMode = appConfig.isCountriesMode;
			this.isSchoolsMode = appConfig.isSchoolsMode;
			this.isGlobalMode = appConfig.isGlobalMode;
			var page = 'countries';
			if (this.isCountriesMode) {
				page = 'countries';
			} else if (this.isSchoolsMode) {
				page = 'schools';
			} else {
				page = 'global';
			}
		}

		$onInit() {
			this.isLoggedIn((is)=>{
				if (is) {
					if (this.auth.hasRole('admin') || this.auth.hasRole('sysadmin')) {
							this.state.go('admin-projects-list');
					} else {
						this.state.go('dashboard');
					}
				}
			});
		}
	}

	class RBComponent {
		constructor() {
			this.message = 'Hello';
		}
	}
	
	angular.module('gigaApp')
		.component('main', {
			templateUrl: function($translate,appConfig){
				if (appConfig.isCountriesMode) {
					return 'app/main/main.countries.html';
				} else if (appConfig.isSchoolsMode) {
					return 'app/main/main.schools.html';
				} else {
					return 'app/main/main.global.html';
				}
			},
			controller: MainController
		})
		.component('requestBlueprint', {
			templateUrl: function($translate,appConfig){
				return 'app/main/main.request_blueprint.en.html';
			},
			controller: RBComponent
		});

})();
