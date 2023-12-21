'use strict';

(function () {

	class DashboardComponent {
		constructor(Auth) {
			this.auth = Auth;
		}
	}

	angular.module('gigaApp')
		.component('dashboard', {
			templateUrl: function($translate){
				return `app/dashboard/dashboard.${$translate.use()||$translate.fallbackLanguage()}.html`;
			},
			controller: DashboardComponent
		});

})();
