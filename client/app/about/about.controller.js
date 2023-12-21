'use strict';
(function () {

	class AboutComponent {
		constructor() {
			this.message = 'Hello';
		}
	}

	angular.module('gigaApp')
		.component('about', {
			templateUrl: function($translate, appConfig){
				if (appConfig.isCountriesMode) {
					return `app/about/about.countries.${$translate.use() || $translate.fallbackLanguage()}.html`;
				} else if (appConfig.isGlobalMode) {
					return `app/about/about.global.${$translate.use() || $translate.fallbackLanguage()}.html`;
				} else {
					return `app/about/about.schools.${$translate.use() || $translate.fallbackLanguage()}.html`;
				}
			},
			controller: AboutComponent
		});

})();
