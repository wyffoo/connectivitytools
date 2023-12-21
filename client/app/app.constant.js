(function (angular, undefined) {
	var $cookies;
	angular.injector(['ngCookies']).invoke(['$cookies', function(_$cookies_) {
		$cookies = _$cookies_;
	}]);
	angular.module("gigaApp.constants", ['ngCookies'])
		.constant("appConfig", {
			"userRoles": [
				"guest",
				"admin",
				"sysadmin"
			],
			'isSchoolsMode': window.location.hostname.startsWith('schools'),
			'isCountriesMode': window.location.hostname.startsWith('countries'),
			'isGlobalMode': window.location.hostname.startsWith('global')
		});

})(angular);
