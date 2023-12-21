'use strict';

class NavbarController {
	//start-non-standard
	menu = [{
		'title': 'Домой',
		'state': 'main'
	}];

	isCollapsed = true;
	//end-non-standard

	constructor(Auth, $translate, appConfig, $state) {
		this.isLoggedIn = Auth.isLoggedIn;
		this.hasRole = Auth.hasRole;
		this.getCurrentUser = Auth.getCurrentUser;
		this.currentLanguage = $translate.use();
		this.isCountriesMode = appConfig.isCountriesMode;
		this.isSchoolsMode = appConfig.isSchoolsMode;
		this.isGlobalMode = appConfig.isGlobalMode;
		this.stateIs = $state.is;
	}
}

angular.module('gigaApp')
	.controller('NavbarController', NavbarController);
