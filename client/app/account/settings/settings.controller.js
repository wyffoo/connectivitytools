'use strict';

class SettingsController {
	constructor(Auth, $translate) {
		this.translations = {};
		$translate([
            'settings.passchanged',
            'settings.incorpass'
		]).then((result)=>{
			this.translations = result;
		});
		this.errors = {};
		this.submitted = false;

		this.Auth = Auth;
	}

	changePassword(form) {
		this.submitted = true;

		if (form.$valid) {
			this.Auth.changePassword(this.user.oldPassword, this.user.newPassword)
				.then(() => {
					this.message = this.translations['settings.passchanged'];
				})
				.catch(() => {
					this.errors.other = this.translations['settings.incorpass'];
					this.message = '';
				});
		}
	}
}

angular.module('gigaApp')
	.controller('SettingsController', SettingsController);
