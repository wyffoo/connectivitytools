'use strict';

window.L.Control.Fullscreen = window.L.Control.FullScreen;

angular.module('gigaApp', [
	'gigaApp.constants',
	'gigaApp.offline-status',
	'gigaApp.auth',
	'gigaApp.simple-alert',
	'gigaApp.main-project-type-panel',
	'gigaApp.dashboard-menu',
	'gigaApp.language-picker',
	'gigaApp.lan-configuration-visualization',
	'gigaApp.project-review',
	'gigaApp.input-templates',
	'gigaApp.wizard-header',
	'gigaApp.bottom-buttons',
	'gigaApp.country-chooser',
	'ngCookies',
	'ngResource',
	'ngStorage',
	'ngSanitize',
	'compareField',
	'vcRecaptcha',
	'ui.router',
	'ui.bootstrap',
	'validation.match',
	// 'checklist-model',
	'ui.checkbox',
	'ngLoadingSpinner',
	// 'chart.js',
	// 'cb.x2js'
	'mp.autoFocus',
	'pascalprecht.translate',
	'gigaApp.admin',
	'gigaApp.sysadmin',
	'gigaApp.school-wizard',
	'gigaApp.multischool-wizard',
	'gigaApp.blueprint-wizard',
	'gigaApp.traffic-profile',
	'ngFileUpload',
	'ui-leaflet',
	'ui.toggle',
	'zingchart-angularjs',
	'siTable',
	'jackrabbitsgroup.angular-multiselect'
])
	.config(function ($urlRouterProvider, $locationProvider, $translatePartialLoaderProvider, $translateProvider, $localStorageProvider) {
		$urlRouterProvider
			.otherwise('/');

		$translateProvider.useSanitizeValueStrategy('escape');

		$translatePartialLoaderProvider.addPart('global');

		$translateProvider.useLoader('$translatePartialLoader', {
			urlTemplate: '/assets/i18n/{lang}/{part}.json'
		});

		//$translateProvider.fallbackLanguage('ua');
		let lang = $localStorageProvider.get('preferredLanguage');
		if (lang === null) {
			lang = 'en';
		}
		$translateProvider.preferredLanguage(lang);
		$translateProvider.use(lang);

		$locationProvider.html5Mode(false);
	})
	.config(['$qProvider', function ($qProvider) {
		$qProvider.errorOnUnhandledRejections(false);
	}])
	.run(function ($rootScope, $translate, $state) {
		// $rootScope.$on('$translateChangeSuccess', function(){
		// 	$state.reload();
		// });
		$rootScope.$on('$translatePartialLoaderStructureChanged', function () {
			$translate.refresh();
		});
	})
	.filter('isEmpty', function () {
		return function (object) {
			return angular.isUndefined(object) || angular.equals({}, object);
		};
	})
	.filter('newlines', function () {
		return function (text) {
			return text.replace(/\n/g, '<br/>');
		};
	})
	.filter('newlines', function () {
		return function (text) {
			return text.split(/\n/g);
		};
	});

angular.module('gigaApp').filter('cut', function () {
	return function (value, wordwise, max, tail) {
		if (!value) return '';

		max = parseInt(max, 10);
		if (!max) return value;
		if (value.length <= max) return value;

		value = value.substr(0, max);
		if (wordwise) {
			var lastspace = value.lastIndexOf(' ');
			if (lastspace !== -1) {
				//Also remove . and , so its gives a cleaner result.
				if (value.charAt(lastspace-1) === '.' || value.charAt(lastspace-1) === ',') {
					lastspace = lastspace - 1;
				}
				value = value.substr(0, lastspace);
			}
		}

		return value + (tail || ' …');
	};
});
